# ============================================
# Perturb Query Module for PerturbSigDB
# Gene Perturbation Signature Alignment
# 输入: gene + logFC pairs (方向感知签名)
# 功能: 使用 Spearman 秩相关性从库中寻找最相似的扰动签名
# ============================================

# ===== GPSA Configuration =====
GSEA_DIR <- "GSEA"                         # 复用 GSQuery 的索引目录
FILTER_CANDIDATES <- 10000                 # Phase 1 后保留的候选签名数
N_TOP_EXTRACT <- 200                        # 取 top up/down 基因用于倒排索引查询


# ===== Global GPSA Core Function =====
# user_dt: data.table with columns "gene" and "logFC"
# 仿照 GSQuery: Phase1 倒排索引预筛选 → Phase2 对候选逐个读.fst + Spearman
run_gpsa_global <- function(user_dt, top_n = 500, progress_file = NULL, type_filter = "both") {
  
  # Timing log file (written alongside progress_file, visible via tail -f)
  log_file <- if (!is.null(progress_file)) {
    paste0(dirname(progress_file), "/gpsa_timing.log")
  } else {
    tempfile("gpsa_timing_")
  }
  t0 <- proc.time()
  
  gpsa_log <- function(...) {
    msg <- paste0("[GPSA ", format(Sys.time(), "%H:%M:%S"), "] ",
                  paste(..., sep = " "))
    cat(msg, "\n")
    write(msg, file = log_file, append = TRUE)
  }
  
  gpsa_log("Starting with", nrow(user_dt), "gene-logFC pairs")
  
  update_progress <- function(pct, msg = "") {
    if (!is.null(progress_file)) {
      writeLines(sprintf("%.1f|%s", pct, msg), progress_file)
    }
  }
  
  # ===== Preprocess user data =====
  user_dt <- copy(user_dt)
  user_dt[, gene := toupper(gene)]
  user_dt <- user_dt[!duplicated(gene)]
  user_dt <- user_dt[!is.na(logFC) & is.finite(logFC)]
  n_user <- nrow(user_dt)
  
  if (n_user < 5) {
    gpsa_log("Too few valid gene-logFC pairs:", n_user)
    return(NULL)
  }
  
  # Cap to most informative genes by |logFC| for Spearman (avoids O(n_genes × n_candidates) blowup)
  N_SPEARMAN_GENES <- 10000L
  if (n_user > N_SPEARMAN_GENES) {
    user_dt[, abs_logFC := abs(logFC)]
    setorder(user_dt, -abs_logFC)
    user_dt <- user_dt[1:N_SPEARMAN_GENES]
    user_dt[, abs_logFC := NULL]
    n_user <- N_SPEARMAN_GENES
    gpsa_log("Capped to top", n_user, "genes by |logFC| for Spearman scoring")
  }
  
  # Pre-compute user ranks (used for ALL Spearman computations)
  user_genes <- user_dt$gene
  user_logfc <- user_dt$logFC
  names(user_logfc) <- user_genes
  user_ranks <- rank(user_logfc, ties.method = "average")
  names(user_ranks) <- user_genes
  
  # Extract top up + top down genes for inverted index lookup
  user_dt_sorted <- user_dt[order(-logFC)]
  n_extract <- min(N_TOP_EXTRACT, n_user %/% 2)
  if (n_extract < 5) n_extract <- min(5, n_user)
  
  top_up_genes   <- user_dt_sorted[1:n_extract, gene]
  top_down_genes <- user_dt_sorted[(n_user - n_extract + 1):n_user, gene]
  query_genes_for_idx <- unique(c(top_up_genes, top_down_genes))
  
  # ===== Load index =====
  update_progress(2, "Loading signature lookup...")
  lookup_file <- file.path(GSEA_DIR, "signatures", "signature_lookup.rds")
  if (!file.exists(lookup_file)) {
    stop("[GPSA] signature_lookup.rds not found at ", lookup_file)
  }
  lookup <- readRDS(lookup_file)
  gpsa_log(nrow(lookup), "signatures in lookup")
  update_progress(5, paste0("Lookup loaded: ", nrow(lookup), " signatures"))
  
  # ===== Phase 1: 倒排索引快速过滤 =====
  t_phase1 <- proc.time()
  update_progress(7, "Loading demonstration inverted index...")
  gpsa_log("Loading inverted index...")
  inverted_file <- file.path(GSEA_DIR, "signatures", "inverted_index.rds")
  if (!file.exists(inverted_file)) {
    stop("[GPSA] inverted_index.rds not found at ", inverted_file)
  }
  inverted <- readRDS(inverted_file)
  gpsa_log("Inverted index loaded in", round((proc.time() - t_phase1)[3], 1), "s")
  
  update_progress(15, "Searching inverted index for candidates...")
  matched_genes <- inverted[gene %in% query_genes_for_idx]
  
  if (nrow(matched_genes) == 0) {
    gpsa_log("No query genes found in inverted index")
    update_progress(100, "No results found")
    return(NULL)
  }
  
  pairs_list <- matched_genes[, .(sig_idx = unlist(sig_indices)), by = gene]
  hit_counts <- pairs_list[, .N, by = sig_idx][order(-N)]
  
  n_candidates <- min(FILTER_CANDIDATES, nrow(hit_counts))
  candidate_idx <- hit_counts$sig_idx[1:n_candidates]
  gpsa_log("Phase 1 done in", round((proc.time() - t_phase1)[3], 1),
           "s, top", n_candidates, "candidates from", nrow(hit_counts), "total")
  
  # ===== Type filter (Chemical / Genetic) before Phase 2 =====
  if (!is.null(type_filter) && type_filter != "both") {
    meta_base_dir <- "signature_by_gene_fst"
    chem_files <- file.path(meta_base_dir, c(
      "CIGSMetadata.fst", "HerbDataMetadata.fst",
      "LINCS_2020_Metadata.fst", "LINCS_L1000_Metadata.fst"
    ))
    gen_files <- file.path(meta_base_dir, c(
      "GuoziDataMetadata.fst", "PerturbAtlasMetadata.fst",
      "LINCS_2020_KD_Metadata.fst", "LINCS_2020_KO_Metadata.fst",
      "LINCS_L1000_KD_Metadata.fst", "LINCS_L1000_KO_Metadata.fst"
    ))
    target_files <- if (type_filter == "chemical") chem_files else gen_files
    
    type_datasets <- character(0)
    for (mf in target_files) {
      if (!file.exists(mf)) next
      meta_dt <- tryCatch(
        read_fst(mf, columns = "Dataset", as.data.table = TRUE),
        error = function(e) NULL
      )
      if (!is.null(meta_dt) && "Dataset" %in% names(meta_dt)) {
        type_datasets <- c(type_datasets, meta_dt$Dataset)
      }
    }
    type_datasets <- unique(type_datasets)
    
    if (length(type_datasets) > 0) {
      valid_mask <- lookup$name %in% type_datasets
      n_before <- nrow(hit_counts)
      hit_counts <- hit_counts[hit_counts$sig_idx %in% which(valid_mask)]
      n_after <- nrow(hit_counts)
      gpsa_log(sprintf("Type filter (%s): %d → %d candidates", type_filter, n_before, n_after))
      
      n_candidates <- min(FILTER_CANDIDATES, nrow(hit_counts))
      if (n_candidates == 0) {
        gpsa_log("No candidates after type filter")
        update_progress(100, "No results after type filter")
        return(NULL)
      }
      candidate_idx <- hit_counts$sig_idx[1:n_candidates]
      gpsa_log("Top", n_candidates, "candidates after type filter")
    }
  }
  
  # ===== Phase 2: Spearman Rank Correlation (parallel) =====
  # Use a laptop-safe number of parallel workers in the demonstration
  n_cores <- if (.Platform$OS.type == "windows") 1L else {
    workers <- suppressWarnings(as.integer(Sys.getenv("PSDB_QUERY_WORKERS", "2")))
    if (is.na(workers) || workers < 1L) workers <- 2L
    min(workers, 4L)
  }
  gpsa_log("Phase 2: Spearman scoring on", n_candidates, "candidates with", n_cores, "cores")
  update_progress(20, sprintf("Spearman on %d candidates (%d cores)...", n_candidates, n_cores))
  t_phase2 <- proc.time()
  
  process_spearman <- function(row_idx) {
    tryCatch({
      row <- lookup[row_idx]
      dt <- read_fst(row$file, as.data.table = TRUE)
      
      db_genes <- toupper(dt$gene)
      db_logfc <- dt$logFC
      names(db_logfc) <- db_genes
      db_logfc <- db_logfc[!duplicated(names(db_logfc))]
      
      n_total <- length(db_logfc)
      if (n_total < 10) return(NULL)
      
      shared <- intersect(user_genes, db_genes)
      n_shared <- length(shared)
      if (n_shared < 5) return(NULL)
      
      u_ranks <- user_ranks[shared]
      db_ranks <- rank(db_logfc, ties.method = "average")
      names(db_ranks) <- names(db_logfc)
      d_ranks <- db_ranks[shared]
      
      rho <- cor(u_ranks, d_ranks, method = "pearson")
      if (is.na(rho)) return(NULL)
      
      list(
        Dataset     = row$name,
        N_Genes_DB  = row$n_genes,
        Overlap     = n_shared,
        Overlap_Pct = round(n_shared / n_user * 100, 2),
        Spearman_Rho = round(rho, 4)
      )
    }, error = function(e) NULL)
  }
  
  # Split candidates into smaller chunks for parallel processing with progress
  N_BATCHES <- 5L
  nchunks    <- n_cores * N_BATCHES
  chunk_size <- max(1L, ceiling(n_candidates / nchunks))
  chunks <- split(candidate_idx, ceiling(seq_along(candidate_idx) / chunk_size))
  n_chunks <- length(chunks)
  gpsa_log("Phase 2: split into", n_chunks, "chunks,", N_BATCHES, "batches")
  
  results_list <- list()
  batch_size <- ceiling(n_chunks / N_BATCHES)
  
  for (b in seq_len(N_BATCHES)) {
    bidx <- ((b - 1) * batch_size + 1):min(b * batch_size, n_chunks)
    if (length(bidx) == 0) break
    
    batch_results <- parallel::mclapply(bidx, function(ci) {
      chunk <- chunks[[ci]]
      lapply(chunk, process_spearman)
    }, mc.cores = min(n_cores, length(bidx)), mc.preschedule = TRUE)
    
    results_list <- c(results_list, batch_results)
    
    pct <- 20 + (b / N_BATCHES) * 75
    update_progress(pct, sprintf("Spearman: batch %d/%d done (%d chunks, ~%d sigs scored)",
      b, N_BATCHES, max(bidx), length(results_list)))
    
    gpsa_log(sprintf("  Batch %d/%d done in %.1fs (chunks %d-%d)",
      b, N_BATCHES, (proc.time() - t_phase2)[3], min(bidx), max(bidx)))
  }
  
  results_list <- unlist(results_list, recursive = FALSE)
  
  gpsa_log("Phase 2 done in", round((proc.time() - t_phase2)[3], 1), "s")
  
  update_progress(95, "Merging and ranking results...")
  t_post <- proc.time()
  
  result <- rbindlist(results_list)
  gpsa_log("rbindlist in", round((proc.time() - t_post)[3], 2), "s")
  
  if (is.null(result) || nrow(result) == 0) {
    gpsa_log("No results")
    update_progress(100, "No results found")
    return(NULL)
  }
  
  gpsa_log("Raw results:", nrow(result), "signatures")
  
  # Multiple testing correction, deduplicate, rank
  result <- result[!is.na(Spearman_Rho)]
  result <- result[!duplicated(Dataset)]
  
  result[, Pvalue := {
    n <- Overlap
    denom <- pmax(1 - Spearman_Rho^2, 1e-15)
    t_stat <- Spearman_Rho * sqrt(pmax(1, n - 2) / denom)
    t_stat[!is.finite(t_stat)] <- 0
    pv <- 2 * pt(-abs(t_stat), df = pmax(1, n - 2))
    # Prevent floating-point underflow to exactly 0 (pt() returns 0 for very large t)
    # This preserves distinguishability in downsteam adjusted p-values
    pmax(pv, 1e-300)
  }]
  
  # FDR correction options:
  # 1. BH over the candidate set (practical, used as primary metric)
  result[, Padj := p.adjust(Pvalue, method = "BH")]
  
  # 2. Bonferroni over full database (conservative upper bound, for transparency)
  #    Accounts for all signatures in the database, not just tested candidates
  n_total <- nrow(lookup)
  result[, Padj_Conservative := pmin(1.0, Pvalue * n_total)]
  # NOTE: Do NOT round — small values like 1e-12 would become 0, loss of information
  gpsa_log("Pvalue computation in", round((proc.time() - t_post)[3], 2), "s")
  
  setorder(result, -Spearman_Rho)
  
  if (nrow(result) > top_n) result <- head(result, top_n)
  
  update_progress(100, "Done!")
  gpsa_log("Total compute time:", round((proc.time() - t_phase1)[3], 1), "s")
  gpsa_log("Final results:", nrow(result), "signatures")
  return(result)
}


# ===================== UI Function =====================
GPSAUI <- function(id, title = "Perturb Query") {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .gpsa-box {
          background-color: #f0f6ff;
          border-radius: 12px;
          padding: 20px;
          margin-bottom: 20px;
        }
        .dataTables_scrollHeadInner { width: 100% !important; }
        .dataTables_scrollHeadInner table { width: 100% !important; }
        .gpsa-title {
          color: #2c5f8a;
          font-weight: 600;
          margin-bottom: 15px;
        }
        .result-section {
          background-color: #f8f9fa;
          border-radius: 12px;
          padding: 20px;
          margin-top: 20px;
        }
        .progress-container {
          margin: 20px 0;
          padding: 15px;
          background: #f8f9fa;
          border-radius: 8px;
        }
        .progress-info {
          text-align: center;
          margin-top: 10px;
          color: #666;
          font-size: 14px;
        }
        .status-msg {
          padding: 10px 15px;
          border-radius: 6px;
          margin: 10px 0;
          font-weight: 500;
        }
        .status-running {
          background-color: #fff3cd;
          color: #856404;
          border: 1px solid #ffc107;
        }
        .status-done {
          background-color: #d4edda;
          color: #155724;
          border: 1px solid #28a745;
        }
        table.dataTable thead th {
          background-color: #3a7bd5 !important;
          color: #ffffff !important;
          font-weight: 600;
          text-align: center !important;
        }
        table.dataTable tbody td { text-align: center !important; max-width: 200px; white-space: normal; word-break: break-word; overflow-wrap: break-word; }
        table.dataTable tbody tr:hover {
          background-color: #f7faff !important;
        }
        .dataset-link {
          color: #3a7bd5;
          font-weight: 500;
          text-decoration: underline;
          cursor: pointer;
        }
        .dataset-link:hover {
          color: #2d6acd;
        }
        /* Modal */
        .dataset-wide-modal { width: 96vw !important; max-width: 1400px !important; margin: 20px auto !important; }
        .dataset-wide-modal .modal-content { border-radius: 14px; overflow: hidden; }
        .dataset-wide-modal .modal-body { max-height: 88vh; overflow-y: auto; padding: 18px 22px; }
        table.dataTable tbody tr.selected > *,
        table.dataTable.stripe tbody tr.odd.selected > *,
        table.dataTable.display tbody tr.odd.selected > *,
        table.dataTable.hover tbody tr.selected:hover > *,
        table.dataTable.display tbody tr.selected:hover > * {
          box-shadow: inset 0 0 0 9999px rgba(208, 208, 208, 0.9) !important;
          color: #333 !important;
        }
        .modal-image-row { margin-top: 10px; margin-bottom: 14px; }
        .modal-image-card { background: #ffffff; border-radius: 12px; padding: 12px; border: 1px solid #e9eef5; box-shadow: 0 2px 6px rgba(0,0,0,0.04); }
        .modal-img-box { height: 240px; display: flex; align-items: center; justify-content: center; overflow: hidden; }
        .modal-img-box-large { height: 340px; display: flex; align-items: center; justify-content: center; overflow: hidden; }
        .modal-structure-img { max-width: 100%; max-height: 100%; width: auto !important; height: auto !important; object-fit: contain; }
        .modal-img-caption { text-align: center; color: #555; font-size: 13px; margin-top: 6px; font-weight: 500; }
        .detail-card { background: #ffffff; border-radius: 14px; padding: 20px; }
        .detail-title { font-size: 18px; font-weight: 700; color: #2c5f8a; margin-bottom: 12px; }
        .detail-item { padding: 6px 0; border-bottom: 1px solid #f0f0f0; }
        .detail-item:last-child { border-bottom: none; }
        .detail-label { font-weight: 700; color: #555; }
        .detail-value { color: #333; }
        .detail-effect { padding: 8px 12px; border-radius: 6px; font-weight: 600; margin-bottom: 12px; display: inline-block; }
        .effect-suppressor { background: #e3f2fd; color: #1565c0; }
        .effect-activator { background: #fff3e0; color: #e65100; }
        .plot-download-btn { margin-top: 10px; margin-bottom: 5px; }
      ")),
      tags$script(HTML(sprintf("
        $(document).on('click', '.dl-btn', function(e) {
          e.stopPropagation();
          var ds = this.getAttribute('data-ds');
          var gem = this.getAttribute('data-gem');
          Shiny.setInputValue('%s', {
            ds: ds,
            gem: gem,
            ts: Date.now()
          }, {priority: 'event'});
        });
        $(document).on('click', '.le-toggle-btn', function(e) {
          e.stopPropagation();
          var $wrap = $(this).closest('.le-wrap');
          var $short = $wrap.find('.le-short');
          var $full = $wrap.find('.le-full');
          $short.toggle();
          $full.toggle();
          $(this).toggleClass('fa-caret-down fa-caret-up');
        });
      ", ns("download_single_ds"))))
    ),
    
    # Hidden input for dataset click trigger
    hidden(tags$input(id = ns("click_dataset"), type = "text", style = "display:none;")),
    fluidRow(
      shinydashboard::box(
        title = title,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 12,
        
        # ===== Input Section =====
        fluidRow(
          column(12,
                 tags$div(class = "gpsa-box",
                          tags$h4(icon("align-center"), " Perturbation Similarity Query", class = "gpsa-title"),
                          tags$hr(),
                          p(HTML("Perturb Query computes Spearman rank correlation on intersecting genes to rank transcriptionally similar perturbations. For a drug-induced signature, concordant genetic perturbations may suggest candidate targets, or mechanism-of-action hypotheses. For a gene knockdown or knockout signature, concordant chemical perturbations may indicate candidate inhibitors or pathway modulators. For a disease-associated signature, strongly negatively correlated perturbations may indicate candidate reversal agents that may counteract disease-associated transcriptional programs.")),
                          p("Provide a two-column table with gene names and logFC values:", style = "margin-bottom: 5px;"),
                          
                          fluidRow(
                            column(5,
                                   fileInput(ns("sig_file"), "Upload Signature File (CSV/TSV)",
                                             accept = c(".csv", ".txt", ".tsv"),
                                             buttonLabel = "Browse...",
                                             placeholder = "gene,logFC file")
                            ),
                            column(7,
                                   div(style = "margin-bottom: 8px;",
                                     downloadButton(ns("download_demo_gpsa"), "Download Demo File",
                                       class = "btn-sm btn-outline-primary",
                                       style = "margin-right: 8px;"),
                                     actionButton(ns("fill_demo_gpsa"), "Demo",
                                       class = "btn btn-sm btn-outline-success",
                                       style = "border-radius: 4px; margin-right: 8px;"),
                                     actionButton(ns("clear_upload_gpsa"), "Clear",
                                       class = "btn btn-sm btn-outline-danger",
                                       style = "border-radius: 4px;")
                                   ),
                                   textAreaInput(ns("sig_text"), NULL,
                                                 placeholder = "Paste gene and logFC here (tab/comma/space separated)\nExample:\nTP53\t-1.5\nEGFR\t2.3\nMYC\t1.8\nBRCA1\t-0.9\nCDKN2A\t1.2\n...",
                                                 rows = 12)
                            )
                          )),
                          
                          # ===== File vs Text Conflict Warning =====
                          uiOutput(ns("file_text_conflict_warning")),
                          
                          fluidRow(
                            column(12,
                            column(3,
                                   numericInput(ns("top_n"), "Top N Results",
                                                value = 500, min = 10, max = 5000, step = 10)
                            ),
                            column(3,
                                   tags$div(style = "display: flex; align-items: center; gap: 24px;",
                                     radioButtons(ns("type_filter"), "Type",
                                       choices = c("Both" = "both", "Chemical" = "chemical", "Genetic" = "genetic"),
                                       selected = "both", inline = TRUE)
                                   )
                            ),
                            conditionalPanel(
                              condition = sprintf("input['%s'] == 'chemical' || input['%s'] == 'both'", ns("type_filter"), ns("type_filter")),
                              column(2,
                                     tags$div(style = "display: flex; align-items: center; gap: 0px; margin-top: 15px;",
                                              checkboxInput(ns("fda_only"), "FDA Approved Only", value = FALSE)
                                     )
                              )
                            ),
                            column(4),
                          )
                 )
        )
        ),
        
        # ===== Run Button =====
        fluidRow(
          column(12,
                 actionButton(ns("run_gpsa"),
                              label = "Run Perturb Query",
                              icon = icon("play"),
                              class = "btn-primary btn-lg",
                              style = "border-radius: 8px; padding: 12px 40px; width: 100%;")
          )
        ),
        
        # ===== Progress Section =====
        fluidRow(
          column(12,
                 div(id = ns("progress_container"),
                     class = "progress-container",
                     style = "display: none;",
                     
                     div(id = ns("status_msg"), class = "status-msg status-running",
                         icon("hourglass-half"), " Analysis in progress...Usually completes within 2 min. Please do not close !"),
                     
                     div(class = "progress", style = "height: 22px; margin: 12px 0; background-color: #e9ecef; border-radius: 10px; overflow: hidden;",
                         div(id = ns("progress_bar"), class = "progress-bar progress-bar-striped progress-bar-animated",
                             role = "progressbar",
                             style = "width: 0%; background-color: #87CEEB;",
                             "0%")
                     )
                 )
          )
        ),
        
        # ===== Results Section =====
        fluidRow(
          column(12,
                 div(id = ns("results_container"),
                     class = "result-section",
                     style = "display: none;",
                     
                     tags$h4(icon("list"), " Results: Most Similar Perturbation Signatures"),
                     
                     fluidRow(
                       column(3,
                              downloadButton(ns("download_results"), "Download CSV",
                                            class = "btn-success", style = "margin-bottom: 10px;")
                       )
                     ),
                     
                     fluidRow(
                       column(12,
                              tags$h4(icon("table"), " Perturbation Signature Results"),
                              tags$div(style = "margin-bottom:15px;",
                                       uiOutput(ns("results_tabs_ui"))
                              )
                       ),
                       
                       # ===== Dataset Selection for Visualization (below tables) =====
                      column(12,
                             fluidRow(
                                uiOutput(ns("rank_plots_ui"))
                              ),
                              
                              tags$hr(),
                              tags$h5("Top Perturbations × Key Query Genes Heatmap", style = "color:#2c5f8a;font-weight:bold;"),
                              tags$p("LogFC of user's top up/down genes across the best-matching perturbation signatures. Clipped to [-5, 5]."),
                              shinycssloaders::withSpinner(
                                plotOutput(ns("heatmap_plot"), height = "550px"),
                                type = 4, color = "#3a7bd5"
                              ),
                              div(style = "text-align:center; margin-top:8px;",
                                  downloadButton(ns("download_heatmap_plot"), "Download as PDF", class = "btn-sm btn-info plot-download-btn")
                              )
                       )
                     )
                 )
          )
        )
      )
    )
  )
}


# ===================== Server Function =====================
GPSAServer <- function(id, analysis_type_OV = NULL, help_id = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Register static file path for dataset CSV downloads (same as GQuery)
    if (!"dataset_csv" %in% names(shiny::resourcePaths())) {
      shiny::addResourcePath("dataset_csv", normalizePath("rdata_columns_csv"))
    }
    
    cat("[GPSA] Server initialized (Spearman rank correlation on ALL signatures via by-gene fst)\n")
    
    # ===== Load valid gene symbols for input validation =====
    valid_gene_symbols <- if (file.exists("c2.cp.kegg_legacy.v2024.1.Hs.symbols.gmt")) {
      unique(unlist(fgsea::gmtPathways("c2.cp.kegg_legacy.v2024.1.Hs.symbols.gmt")))
    } else character(0)
    
    # ===== Reactive Values =====
    rv <- reactiveValues(
      results = NULL,
      query_genes = NULL,
      running = FALSE,
      progress_pct = 0,
      progress_file = NULL,
      polling_id = 0,
      user_dt = NULL  # store parsed input
    )
    
    # Store modal plots for download
    modal_plot_store <- reactiveVal(NULL)
    
    # Metadata文件路径 (.fst)
    meta_base_dir <- "signature_by_gene_fst"
    chem_meta_files <- file.path(meta_base_dir, c(
      "CIGSMetadata.fst",
      "HerbDataMetadata.fst",
      "LINCS_2020_Metadata.fst",
      "LINCS_L1000_Metadata.fst"
    ))
    gen_meta_files <- file.path(meta_base_dir, c(
      "GuoziDataMetadata.fst",
      "PerturbAtlasMetadata.fst",
      "LINCS_2020_KD_Metadata.fst",
      "LINCS_2020_KO_Metadata.fst",
      "LINCS_L1000_KD_Metadata.fst",
      "LINCS_L1000_KO_Metadata.fst"
    ))
    # 注释文件路径
    fda_anno_file <- file.path(meta_base_dir, "FDA_annotation.fst")
    drug_target_anno_file <- file.path(meta_base_dir, "drug_target_annotation.fst")
    gene_anno_file <- file.path(meta_base_dir, "gene_annotation.fst")
    
    # Helper: Get DEG data with caching
    get_deg_dt_cached <- function(dataset, data_dir) {
      key <- paste0(normalizePath(data_dir, mustWork = FALSE), "::", dataset)
      if (!exists(key, envir = .GlobalEnv)) {
        fst_file <- file.path(data_dir, paste0(dataset, ".fst"))
        csv_file <- file.path(data_dir, paste0(dataset, ".csv"))
        if (file.exists(fst_file)) {
          dt <- read_fst(fst_file, as.data.table = TRUE)
        } else if (file.exists(csv_file)) {
          dt <- fread(csv_file)
        } else {
          chem_fst <- file.path(data_dir, "Chemical", paste0(dataset, ".fst"))
          gen_fst  <- file.path(data_dir, "Genetic", paste0(dataset, ".fst"))
          if (file.exists(chem_fst)) {
            dt <- read_fst(chem_fst, as.data.table = TRUE)
          } else if (file.exists(gen_fst)) {
            dt <- read_fst(gen_fst, as.data.table = TRUE)
          } else {
            dt <- NULL
          }
        }
        assign(key, dt, envir = .GlobalEnv)
      }
      get(key, envir = .GlobalEnv)
    }
    
    # Demo file download
    output$download_demo_gpsa <- downloadHandler(
      filename = function() { "Perturb Query Demo.txt" },
      content = function(file) {
        file.copy("Perturb Query Demo.txt", file)
      }
    )
    
    # Auto-fill demo into text area
    observeEvent(input$fill_demo_gpsa, {
      demo_lines <- readLines("Perturb Query Demo.txt")
      demo_lines <- demo_lines[demo_lines != ""]
      updateTextAreaInput(session, "sig_text",
        value = paste(demo_lines, collapse = "\n"))
    })
    
    # Clear uploaded signature file and text area
    observeEvent(input$clear_upload_gpsa, {
      shinyjs::reset("sig_file")
      updateTextAreaInput(session, "sig_text", value = "")
    })
    
    # ===== File vs Text Conflict Warning =====
    # When both a file is uploaded AND text is pasted, warn that file takes priority.
    has_file_input <- reactive({
      !is.null(input$sig_file) && file.exists(input$sig_file$datapath)
    })
    
    has_text_input <- reactive({
      nzchar(trimws(input$sig_text)) && length(nzchar(strsplit(trimws(input$sig_text), "\n")[[1]])) >= 2
    })
    
    output$file_text_conflict_warning <- renderUI({
      if (has_file_input() && has_text_input()) {
        tags$div(
          style = "background: #fff3cd; border: 1px solid #ffc107; border-radius: 6px; padding: 10px 14px; margin: 8px 0;",
          tags$div(
            style = "display: flex; align-items: center; gap: 8px;",
            icon("exclamation-triangle", style = "color: #e6a400; font-size: 16px;"),
            tags$span(
              style = "color: #856404; font-size: 13px;",
              tags$b("Both file and text input detected."),
              " The uploaded file will be used; ",
              tags$u("pasted text will be ignored."),
              " Clear the file upload to use the pasted text instead."
            )
          )
        )
      }
    })
    
    safe_filename <- function(x, max_len = 120) {
      x <- gsub("[^a-zA-Z0-9_\\-\\.]", "_", as.character(x))
      if (nchar(x) > max_len) x <- substr(x, 1, max_len)
      x
    }
    
    # Download combined plots PDF (modal)
    output$download_combined_pdf <- downloadHandler(
      filename = function() {
        st <- modal_plot_store()
        name <- if (!is.null(st$gem_id) && nzchar(as.character(st$gem_id))) as.character(st$gem_id) else "dataset"
        paste0(safe_filename(name), "_combined_plots.pdf")
      },
      content = function(file) {
        st <- modal_plot_store()
        req(st)
        req(st$combined_plot)
        
        pdf_width <- switch(as.character(length(st$ggplots)),
                            "1" = 6, "2" = 10, "3" = 14, "4" = 12,
                            "5" = 14, "6" = 14, 12
        )
        pdf_height <- switch(as.character(length(st$ggplots)),
                             "1" = 5, "2" = 5, "3" = 5, "4" = 10,
                             "5" = 10, "6" = 12, 10
        )
        
        ggplot2::ggsave(
          filename = file,
          plot = st$combined_plot,
          width = pdf_width,
          height = pdf_height,
          device = "pdf"
        )
      },
      contentType = "application/pdf"
    )
    
    # ===== Parse User Input Signature =====
    parse_signature <- function() {
      dt <- NULL
      
      # From file upload
      if (!is.null(input$sig_file) && file.exists(input$sig_file$datapath)) {
        tryCatch({
          raw <- fread(input$sig_file$datapath, header = TRUE, fill = TRUE)
          dt <- raw
        }, error = function(e) {
          cat("[GPSA] File read error:", e$message, "\n")
        })
      }
      
      # From text input
      if (is.null(dt) && nzchar(input$sig_text)) {
        lines <- strsplit(trimws(input$sig_text), "\n")[[1]]
        lines <- lines[nzchar(lines)]
        if (length(lines) < 2) return(NULL)
        
        first_line <- lines[1]
        has_header <- grepl("^[a-zA-Z]", strsplit(trimws(first_line), "[\t, ]+")[[1]][2])
        data_lines <- if (has_header) lines[-1] else lines
        
        parsed <- strsplit(data_lines, "[\t, ]+")
        parsed <- Filter(function(x) length(x) >= 2, parsed)
        if (length(parsed) == 0) return(NULL)
        
        genes_vec <- sapply(parsed, `[`, 1)
        logfc_vec <- suppressWarnings(as.numeric(sapply(parsed, `[`, 2)))
        
        dt <- data.table(gene = genes_vec, logFC = logfc_vec)
      }
      
      if (is.null(dt) || nrow(dt) < 5) return(NULL)
      
      if (ncol(dt) > 2) {
        gene_candidates <- c("gene", "Gene", "GENE", "symbol", "Symbol", "SYMBOL", "gene_name", "GeneSymbol")
        fc_candidates <- c("logFC", "LogFC", "log2FC", "Log2FC", "log2FoldChange", "fc", "FC", "value")
        
        gene_col <- gene_candidates[gene_candidates %in% names(dt)][1]
        fc_col <- fc_candidates[fc_candidates %in% names(dt)][1]
        
        if (!is.na(gene_col)) dt[[gene_col]] <- as.character(dt[[gene_col]])
        if (!is.na(gene_col) && !is.na(fc_col)) {
          dt <- dt[, .(gene = get(gene_col), logFC = suppressWarnings(as.numeric(get(fc_col))))]
        } else {
          dt <- dt[, .(gene = as.character(.SD[[1]]), logFC = suppressWarnings(as.numeric(.SD[[2]])))]
        }
      } else {
        setnames(dt, c("gene", "logFC"))
      }
      
      dt[, gene := toupper(trimws(as.character(gene)))]
      dt <- dt[nzchar(gene) & grepl("[A-Za-z0-9]", gene)]
      dt <- dt[!is.na(logFC) & is.finite(logFC)]
      dt <- dt[!duplicated(gene)]
      
      if (nrow(dt) < 5) return(NULL)
      
      cat("[GPSA] Parsed", nrow(dt), "gene-logFC pairs\n")
      return(dt)
    }
    
    # ===== Run analysis =====
    run_analysis <- function() {
      if (rv$running) {
        showNotification("Analysis already running...", type = "warning")
        return()
      }
      
      user_dt <- parse_signature()
      if (is.null(user_dt) || nrow(user_dt) < 5) {
        showNotification(
          "Please provide at least 5 valid gene-logFC pairs. Format: gene(logFC per line.",
          type = "error"
        )
        return()
      }
      
      rv$running <- TRUE
      rv$results <- NULL
      rv$user_dt <- copy(user_dt)
      
      top_n_val <- input$top_n
      
      pfile <- tempfile("gpsa_progress_", fileext = ".txt")
      rv$progress_file <- pfile
      writeLines("0.0|Initializing...", pfile)  # Pre-create so polling sees it immediately
      rv$polling_id <- rv$polling_id + 1
      
      shinyjs::show("progress_container")
      shinyjs::hide("results_container")
      shinyjs::html("status_msg",
                    paste0(icon("hourglass-half"), " Analysis in progress... Due to the large dataset size, this process may take approximately 1 minutes. Please do not close this page or navigate away."))
      shinyjs::addClass("status_msg", "status-running")
      shinyjs::removeClass("status_msg", "status-done")
      shinyjs::html("progress_text", "Starting...")
      shinyjs::runjs(sprintf("$('#%s').css('width', '0%%').text('0%%');", ns("progress_bar")))
      
      # Run in external R process via callr (same as Synergy, avoids promise race conditions)
      # Metadata merge moved inside callr to avoid blocking the Shiny main thread.
      rv$callr_process <- callr::r_bg(function(user_dt, top_n, progress_file, type_filter, fda_only, gpsa_source, chem_meta_files, gen_meta_files, fda_anno_file, drug_target_anno_file, gene_anno_file) {
        library(data.table)
        library(fst)
        library(parallel)
        source(gpsa_source)
        results <- run_gpsa_global(user_dt, top_n, progress_file, type_filter)
        if (is.null(results) || nrow(results) == 0) return(NULL)
        # Merge metadata and filter inside callr (avoids blocking Shiny main thread)
        meta_list <- list()
        for (meta_file in chem_meta_files) {
          if (!file.exists(meta_file)) next
          meta_dt <- tryCatch(read_fst(meta_file, as.data.table = TRUE), error = function(e) NULL)
          if (is.null(meta_dt) || !"Dataset" %in% names(meta_dt) || !"GEM_ID" %in% names(meta_dt)) next
          meta_dt[, Type := "Chemical"]
          meta_list[[length(meta_list) + 1]] <- meta_dt
        }
        for (meta_file in gen_meta_files) {
          if (!file.exists(meta_file)) next
          meta_dt <- tryCatch(read_fst(meta_file, as.data.table = TRUE), error = function(e) NULL)
          if (is.null(meta_dt) || !"Dataset" %in% names(meta_dt) || !"GEM_ID" %in% names(meta_dt)) next
          meta_dt[, Type := "Genetic"]
          meta_list[[length(meta_list) + 1]] <- meta_dt
        }
        if (length(meta_list) > 0) {
          meta_merged <- unique(rbindlist(meta_list, fill = TRUE), by = "Dataset")
          results <- merge(results, meta_merged, by = "Dataset", all.x = TRUE)
        }
        # Merge annotations: FDA + drug_target for Chemical, gene info for Genetic
        if ("Perturbagen" %in% names(results)) {
          if (file.exists(fda_anno_file)) {
            fda_dt <- tryCatch(read_fst(fda_anno_file, as.data.table = TRUE), error = function(e) NULL)
            if (!is.null(fda_dt)) results <- merge(results, fda_dt, by = "Perturbagen", all.x = TRUE)
          }
          if (file.exists(drug_target_anno_file)) {
            dt_dt <- tryCatch(read_fst(drug_target_anno_file, as.data.table = TRUE), error = function(e) NULL)
            if (!is.null(dt_dt)) results <- merge(results, dt_dt, by = "Perturbagen", all.x = TRUE)
          }
        }
        if ("pbgene" %in% names(results) && file.exists(gene_anno_file)) {
          gene_dt <- tryCatch(read_fst(gene_anno_file, as.data.table = TRUE), error = function(e) NULL)
          if (!is.null(gene_dt)) {
            results <- merge(results, gene_dt, by = "pbgene", all.x = TRUE)
            if ("entrez_id" %in% names(results)) results[, entrez_id := NULL]
          }
        }
        keep_cols <- intersect(c("Dataset","GEM_ID","Type","Spearman_Rho",
                                 "Overlap","Overlap_Pct","N_Genes_DB",
                                 "Perturbagen","pbgene",
                                 "clinical_phase","moa","target","known_targets",
                                 "uniprot_id","entrez_id","protein_class"), names(results))
        extra <- setdiff(names(results), keep_cols)
        if (length(extra) > 0) results[, (extra) := NULL]
        # Remove rows where Perturbagen contains Chinese characters
        if ("Perturbagen" %in% names(results)) {
          results <- results[!grepl("[\u4e00-\u9fff]", Perturbagen)]
        }
        type_f <- type_filter
        if (!is.null(type_f) && type_f == "chemical") results <- results[Type == "Chemical"]
        else if (!is.null(type_f) && type_f == "genetic") results <- results[Type == "Genetic"]
        if (isTRUE(fda_only)) results <- results[clinical_phase == "Launched"]
        if (nrow(results) > 0) {
          if (!is.null(type_f) && type_f == "chemical") {
            cols_drop <- intersect(c("known_targets", "protein_class"), names(results))
          } else if (!is.null(type_f) && type_f == "genetic") {
            cols_drop <- intersect(c("target", "known_targets", "uniprot_id", "entrez_id",
                                     "clinical_phase", "moa"), names(results))
          } else {
            cols_drop <- character(0)
          }
          if (length(cols_drop) > 0) results[, (cols_drop) := NULL]
          results <- results[order(-abs(Spearman_Rho))]
          if (nrow(results) > 100000) results <- head(results, 100000)
        }
        results
      }, args = list(
        user_dt = user_dt,
        top_n = 100000L,
        progress_file = pfile,
        type_filter = isolate(input$type_filter),
        fda_only = isolate(input$fda_only),
        gpsa_source = "R/GPSA.R",
        chem_meta_files = chem_meta_files,
        gen_meta_files = gen_meta_files,
        fda_anno_file = fda_anno_file,
        drug_target_anno_file = drug_target_anno_file,
        gene_anno_file = gene_anno_file
      ), stdout = "|", stderr = "|")
    }
    
    # ===== Run GPSA =====
    observeEvent(input$run_gpsa, {
      run_analysis()
    })
    
    # ===== Callr Result Polling =====
    # Metadata merge + filtering now runs inside callr — results are ready immediately.
    observe({
      req(rv$running)
      req(rv$callr_process)
      
      invalidateLater(1000)
      
      rx <- rv$callr_process
      if (rx$is_alive()) return()
      
      cat("[GPSA] Callr process completed\n")
      
      results <- tryCatch(rx$get_result(), error = function(e) {
        cat("[GPSA] Callr result error:", conditionMessage(e), "\n")
        NULL
      })
      
      # Clean up progress
      shinyjs::hide("progress_container")
      rv$polling_id <- rv$polling_id + 1
      if (!is.null(rv$progress_file) && file.exists(rv$progress_file)) {
        file.remove(rv$progress_file)
        rv$progress_file <- NULL
      }
      
      if (is.null(results) || nrow(results) == 0) {
        rv$running <- FALSE
        showNotification(
          "No similar signatures found",
          type = "warning", duration = 5
        )
        return()
      }
      
      rv$results <- results
      rv$running <- FALSE
      
      shinyjs::show("results_container")
      showNotification(
        paste("Analysis completed!"),
        type = "message", duration = 5
      )
    })
    
    # ===== Progress Polling =====
    observe({
      req(rv$running)
      req(rv$progress_file)
      
      invalidateLater(300)
      
      pfile <- rv$progress_file
      if (!file.exists(pfile)) return()
      
      line <- tryCatch(readLines(pfile, warn = FALSE), error = function(e) NULL)
      if (is.null(line) || length(line) == 0 || !nzchar(line[1])) return()
      
      parts <- strsplit(line[1], "|", fixed = TRUE)[[1]]
      msg <- if (length(parts) > 1) parts[2] else ""
      if (nzchar(msg)) shinyjs::html("progress_text", msg)
      # Update progress bar
      pct <- suppressWarnings(as.numeric(parts[1]))
      if (!is.na(pct) && is.finite(pct)) {
        pct <- max(0, min(100, pct))
        shinyjs::runjs(sprintf(
          "$('#%s').css('width', '%s%%').text('%s%%');",
          ns("progress_bar"), round(pct, 1), round(pct, 1)
        ))
      }
    })
    
    # ===== Dynamic Rank Plot Layout (Chemical / Genetic based on type filter) =====
    output$rank_plots_ui <- renderUI({
      type_val <- input$type_filter
      if (is.null(type_val)) type_val <- "both"
      
      chem_plot <- div(style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px;",
        tags$h5("Chemical Rank", style = "text-align:center;margin-top:5px;color:#3a7bd5;font-weight:bold;"),
        shinycssloaders::withSpinner(
          plotOutput(ns("results_rank_chem"), height = "320px"),
          type = 4, color = "#3a7bd5"
        ),
        div(style = "text-align:center; margin-top:8px;",
          downloadButton(ns("download_rank_chem"), "Download as PDF", class = "btn-sm btn-info plot-download-btn")
        )
      )
      
      gen_plot <- div(style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px;",
        tags$h5("Genetic Rank", style = "text-align:center;margin-top:5px;color:#28a745;font-weight:bold;"),
        shinycssloaders::withSpinner(
          plotOutput(ns("results_rank_gen"), height = "320px"),
          type = 4, color = "#28a745"
        ),
        div(style = "text-align:center; margin-top:8px;",
          downloadButton(ns("download_rank_gen"), "Download as PDF", class = "btn-sm btn-info plot-download-btn")
        )
      )
      
      if (type_val == "both") {
        tagList(column(6, gen_plot), column(6, chem_plot))
      } else if (type_val == "chemical") {
        column(12, chem_plot)
      } else {
        column(12, gen_plot)
      }
    })
    
    # ===== Results Rank Plots (Chemical / Genetic, simple scatter) =====
    make_rank_plot <- function(type_val) {
      reactive({
        req(rv$results)
        dt <- copy(rv$results)
        if ("Type" %in% names(dt)) dt <- dt[is.na(Type) | Type == type_val]
        if (nrow(dt) < 5) return(NULL)
        # Rank plot: only show PSG/PSC IDs as label
        if ("GEM_ID" %in% names(dt)) {
          valid <- grepl("^(PSG|PSC)\\d+$", dt$GEM_ID)
          dt <- dt[valid]
          dt[, plot_label := GEM_ID]
        } else {
          dt[, plot_label := Dataset]
        }
        if (type_val == "Chemical" && "Perturbagen" %in% names(dt))
          dt[!is.na(Perturbagen) & Perturbagen != "", plot_label := Perturbagen]
        if (type_val == "Genetic" && "pbgene" %in% names(dt))
          dt[!is.na(pbgene) & pbgene != "", plot_label := pbgene]
        dt <- dt[order(-Spearman_Rho)]
        dt[, rank := .I]
        dt <- head(dt, 10000)
        max_abs <- max(abs(dt$Spearman_Rho), na.rm = TRUE)
        y_lim <- max_abs * 1.15
        label_data <- head(dt[order(-abs(Spearman_Rho))], 10)
        label_data <- label_data[!duplicated(plot_label)]
        p <- ggplot() + theme_minimal(base_size = 11)
        p <- p + geom_hline(yintercept = 0, color = "#E74C3C", linetype = "dashed", linewidth = 0.4)
        p <- p + geom_point(data = dt, aes(x = rank, y = Spearman_Rho, color = Spearman_Rho), size = 1.8, alpha = 0.7)
        p <- p + scale_color_gradient2(low = "#3498DB", mid = "gray85", high = "#E74C3C",
                                       midpoint = 0, limits = c(-max_abs, max_abs),
                                       name = "ρ  (Blue=Suppressed, Red=Activated)")
        p <- p + guides(color = guide_colorbar(barwidth = 8, barheight = 0.4,
                                               title.position = "top", title.hjust = 0.5))
        if (nrow(label_data) > 0)
          p <- p + ggrepel::geom_text_repel(data = label_data,
                                            aes(x = rank, y = Spearman_Rho, label = plot_label), color = "grey20",
                                            size = 3, max.overlaps = 20, box.padding = 0.3, point.padding = 0.2,
                                            min.segment.length = 0.2, segment.color = "grey50", segment.size = 0.3,
                                            show.legend = FALSE, fontface = "bold", force = 1)
        p <- p + labs(x = "Rank", y = "Spearman ρ", title = type_val) +
          coord_cartesian(ylim = c(-y_lim, y_lim)) +
          theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
                plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
                legend.position = "bottom", legend.title.align = 0.5)
        p
      })
    }
    chem_rank_plot_obj <- make_rank_plot("Chemical")
    gen_rank_plot_obj  <- make_rank_plot("Genetic")

    output$results_rank_chem <- renderPlot({ chem_rank_plot_obj() })
    output$results_rank_gen  <- renderPlot({ gen_rank_plot_obj()  })

    output$download_rank_chem <- downloadHandler(
      filename = function() { paste0("Chemical_Rank_", Sys.Date(), ".pdf") },
      content = function(file) {
        p <- chem_rank_plot_obj()
        ggsave(file, plot = p, device = "pdf", width = 8, height = 6)
      }
    )
    output$download_rank_gen <- downloadHandler(
      filename = function() { paste0("Genetic_Rank_", Sys.Date(), ".pdf") },
      content = function(file) {
        p <- gen_rank_plot_obj()
        ggsave(file, plot = p, device = "pdf", width = 8, height = 6)
      }
    )
    
    # ===== Dynamic Results Tables (show based on type filter) =====
    output$results_tabs_ui <- renderUI({
      type_val <- input$type_filter
      if (is.null(type_val)) type_val <- "both"
      
      chem_tab <- tabPanel("Chemical",
        shinycssloaders::withSpinner(
          DT::DTOutput(ns("results_chemical")),
          type = 4, color = "#3a7bd5"
        )
      )
      gen_tab <- tabPanel("Genetic",
        shinycssloaders::withSpinner(
          DT::DTOutput(ns("results_genetic")),
          type = 4, color = "#28a745"
        )
      )
      
      if (type_val == "both") {
        tabsetPanel(id = ns("results_tabset"), gen_tab, chem_tab)
      } else if (type_val == "chemical") {
        tabsetPanel(id = ns("results_tabset"), chem_tab)
      } else {
        tabsetPanel(id = ns("results_tabset"), gen_tab)
      }
    })
    
    # ===== Render Results Tables (split by Chemical/Genetic) =====
    render_type_table <- function(type_val) DT::renderDT({
      req(rv$results)
      dt <- copy(rv$results)
      
      # Filter by type
      dt <- dt[is.na(Type) | Type == type_val]
      dt <- head(dt, isolate(input$top_n))
      # Remove Type column
      dt[, Type := NULL]
      # Drop irrelevant annotation columns based on table type
      if (type_val == "Chemical") {
        cols_drop <- intersect(c("known_targets", "protein_class"), names(dt))
      } else {
        cols_drop <- intersect(c("target", "known_targets", "uniprot_id",
                                 "clinical_phase", "moa"), names(dt))
      }
      if (length(cols_drop) > 0) dt[, (cols_drop) := NULL]
      # Remove any remaining empty columns
      empty_cols <- names(dt)[sapply(dt, function(col) all(is.na(col) | col == "" | col == "NA"))]
      if (length(empty_cols) > 0) dt[, (empty_cols) := NULL]
      if (nrow(dt) == 0) {
        empty_dt <- data.table(Message = paste("No", tolower(type_val), "results found"))
        return(DT::datatable(empty_dt, rownames = FALSE,
                             options = list(dom = 't', pageLength = 1, searching = FALSE, ordering = FALSE)))
      }
      
      numeric_cols <- intersect(names(dt), c("Pvalue", "Padj", "Padj_Conservative", "Spearman_Rho", "Overlap_Pct", "Overlap", "N_Genes_DB"))
      for (col in numeric_cols) {
        dt[, (col) := signif(get(col), 5)]
      }
      
      # Truncate target (FDA) with click-to-expand toggle
      if ("target" %in% names(dt)) {
        dt[, target := {
          targets <- strsplit(target, "|", fixed = TRUE)
          sapply(targets, function(g) {
            g <- trimws(g)
            g <- g[nzchar(g)]
            if (length(g) == 0) return("")
            n <- length(g)
            short <- paste(head(g, 5), collapse = " | ")
            if (n > 5) short <- paste0(short, " +", n - 5, " more")
            full <- paste(g, collapse = " | ")
            sprintf(
              '<span class="le-wrap"><span class="le-short">%s</span><span class="le-full" style="display:none">%s</span> <i class="fa fa-caret-down le-toggle-btn" style="cursor:pointer;color:#3a7bd5"></i></span>',
              short, full
            )
          })
        }]
      }
      
      # Truncate known_targets (drug-target map) with click-to-expand toggle
      if ("known_targets" %in% names(dt)) {
        dt[, known_targets := {
          targets <- strsplit(known_targets, "|", fixed = TRUE)
          sapply(targets, function(g) {
            g <- trimws(g)
            g <- g[nzchar(g)]
            if (length(g) == 0) return("")
            n <- length(g)
            short <- paste(head(g, 5), collapse = " | ")
            if (n > 5) short <- paste0(short, " +", n - 5, " more")
            full <- paste(g, collapse = " | ")
            sprintf(
              '<span class="le-wrap"><span class="le-short">%s</span><span class="le-full" style="display:none">%s</span> <i class="fa fa-caret-down le-toggle-btn" style="cursor:pointer;color:#3a7bd5"></i></span>',
              short, full
            )
          })
        }]
      }
      
      if ("protein_class" %in% names(dt)) {
        dt[, protein_class := {
          classes <- strsplit(protein_class, "|", fixed = TRUE)
          sapply(classes, function(g) {
            g <- trimws(g)
            g <- g[nzchar(g)]
            if (length(g) == 0) return("")
            n <- length(g)
            short <- paste(head(g, 5), collapse = " | ")
            if (n > 5) short <- paste0(short, " +", n - 5, " more")
            full <- paste(g, collapse = " | ")
            sprintf(
              '<span class="le-wrap"><span class="le-short">%s</span><span class="le-full" style="display:none">%s</span> <i class="fa fa-caret-down le-toggle-btn" style="cursor:pointer;color:#3a7bd5"></i></span>',
              short, full
            )
          })
        }]
        setnames(dt, "protein_class", "Class")
      }
      
        # disease_area and indication: removed — not in keep_cols
      
      if ("GEM_ID" %in% names(dt) && "Dataset" %in% names(dt)) {
        missing <- is.na(dt$GEM_ID) | dt$GEM_ID == ""
        dt[, .gem_id_raw := fifelse(missing, Dataset, GEM_ID)]
        # Only keep rows with valid PSG/PSC GEM_ID
        valid <- grepl("^(PSG|PSC)\\d+$", dt$GEM_ID)
        dt <- dt[valid]
      } else {
        dt[, .gem_id_raw := Dataset]
      }
      
      if ("Dataset" %in% names(dt)) {
        dt[, Download := sprintf(
          '<button class="btn btn-sm btn-success dl-btn" data-ds="%s" data-gem="%s"><i class="fa fa-download"></i> Download</button>',
          gsub('"', '&quot;', as.character(Dataset), fixed = TRUE),
          gsub('"', '&quot;', as.character(.gem_id_raw), fixed = TRUE)
        )]
      }
      
      if ("GEM_ID" %in% names(dt) && "Dataset" %in% names(dt)) {
        dt$GEM_ID <- sprintf(
          '<span class="dataset-link" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority: \'event\'})">%s</span>',
          ns("click_dataset"),
          dt$Dataset,
          dt$GEM_ID
        )
        setcolorder(dt, c("GEM_ID", setdiff(names(dt), "GEM_ID")))
      }
      
      # Rename merge key "Dataset" to avoid conflict and "GEM_ID" to "Dataset" for display
      if ("Dataset" %in% names(dt)) setnames(dt, "Dataset", ".Dataset")
      if ("GEM_ID" %in% names(dt))  setnames(dt, "GEM_ID", "Dataset")
      # Rename Perturbagen to PbChem for display
      if ("Perturbagen" %in% names(dt)) setnames(dt, "Perturbagen", "PbChem")
      # Capitalize first letter of displayed column names
      cap_cols <- intersect(c("pbgene", "method"), names(dt))
      for (cc in cap_cols) setnames(dt, cc, paste0(toupper(substr(cc, 1, 1)), substr(cc, 2, nchar(cc))))
      
      # ---- Keep Dataset in data, hide via columnDefs (Download.R pattern) ----
      
      dataset_col_idx <- match(".Dataset", names(dt)) - 1
      gem_id_col_idx <- match(".gem_id_raw", names(dt)) - 1
      
      hidden_targets <- c(
        which(names(dt) == ".Dataset") - 1,
        which(names(dt) == ".gem_id_raw") - 1
      )
      
      dt <- replace_empty_with_dash(dt)
        DT::datatable(
        dt,
        rownames = FALSE,
        escape = FALSE,
        selection = list(mode = "single", target = "row"),
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          scrollCollapse = TRUE,
          autoWidth = FALSE,
          dom = 'frtip',
          order = list(list(which(names(dt) == "Spearman_Rho") - 1, "desc")),
          columnDefs = list(list(visible = FALSE, targets = hidden_targets), dt_dash_render())
        ),
        callback = DT::JS(sprintf(
          "new MutationObserver(function(m){m.forEach(function(r){$(r.addedNodes).filter('.selected,.selected td').css({boxShadow:'inset 0 0 0 9999px rgba(208,208,208,0.9)',color:'#333'});});}).observe(document.querySelector('#'+table.table().node().id+' tbody'),{childList:true,subtree:true});
          table.on('dblclick', 'tbody tr', function() {
            var data = table.row(this).data();
            if (!data) return;
            var ds = data[%d];
            var gem = data[%d];
            Shiny.setInputValue('%s', {
              ds: ds,
              gem: gem,
              ts: Date.now()
            }, {priority: 'event'});
          });",
          dataset_col_idx, gem_id_col_idx,
          ns("download_single_ds")
        ))
      ) %>%
        DT::formatSignif(columns = intersect(numeric_cols, names(dt)), digits = 5)
    })
    
    output$results_chemical <- render_type_table("Chemical")
    output$results_genetic <- render_type_table("Genetic")
    

    
    # ===== Populate Dataset Dropdown =====
    observe({
      req(rv$results)
      dt <- rv$results
      if ("GEM_ID" %in% names(dt)) {
        dt[is.na(GEM_ID) | GEM_ID == "", GEM_ID := Dataset]
      }
      # Match the same truncation as tables: top_n from each type
      top_n_val <- isolate(input$top_n)
      dt_chem <- if ("Type" %in% names(dt)) head(dt[Type == "Chemical"], top_n_val) else dt[0]
      dt_gen  <- if ("Type" %in% names(dt)) head(dt[Type == "Genetic"],  top_n_val) else dt[0]
      dt <- rbindlist(list(dt_chem, dt_gen), fill = TRUE)
      if (nrow(dt) == 0) dt <- head(rv$results, top_n_val)  # fallback
      choices <- setNames(dt$Dataset, dt$GEM_ID)
      choices <- choices[!is.na(names(choices)) & names(choices) != "NA"]
      updateSelectInput(session, "visual_dataset", choices = choices,
                        selected = dt$Dataset[1])
    })
    
    # ===== Cache Hallmark gene sets =====
    hallmark_pathways <- reactive({
      gmt_file <- "h.all.v2022.1.Hs.symbols.gmt"
      if (!file.exists(gmt_file)) return(NULL)
      fgsea::gmtPathways(gmt_file)
    })
    
    # ===== Composite reactive: fetch selected dataset DEG data =====
    selected_dataset_data <- reactive({
      req(input$visual_dataset)
      req(rv$user_dt)
      ds <- input$visual_dataset
      if (is.null(ds) || !nzchar(ds)) return(NULL)
      
      gem_id <- ds
      if (!is.null(rv$results) && nrow(rv$results) > 0) {
        row <- rv$results[Dataset == ds]
        if (nrow(row) > 0) {
          gid <- as.character(row$GEM_ID[1])
          if (nzchar(gid) && !is.na(gid)) gem_id <- gid
        }
      }
      
      ref_deg <- tryCatch(
        get_deg_dt_cached(ds, GSEA_DIR),
        error = function(e) NULL
      )
      
      if (is.null(ref_deg) || nrow(ref_deg) == 0) return(NULL)
      if (!"gene" %in% names(ref_deg) || !"logFC" %in% names(ref_deg)) return(NULL)
      
      ref_deg <- ref_deg[!is.na(logFC) & is.finite(logFC)]
      ref_deg <- ref_deg[!duplicated(gene)]
      
      user_dt <- rv$user_dt
      
      list(ref_deg = ref_deg, user_dt = user_dt, ds = ds, gem_id = gem_id)
    })
    
    # ===== Reactive plot objects for download =====
    # FuncPlot object (reactive)
    func_plot_obj <- reactive({
      sdata <- selected_dataset_data()
      req(sdata)
      
      user_dt <- sdata$user_dt
      ref_deg <- sdata$ref_deg
      gem_id <- sdata$gem_id
      
      pathways <- hallmark_pathways()
      if (is.null(pathways) || length(pathways) == 0) {
        return(ggplot() + 
                 annotate("text", x = 1, y = 1, label = "Hallmark gene sets not found",
                          size = 5, color = "grey50") + theme_void())
      }
      
      user_dt <- user_dt[order(-logFC)]
      user_ranked <- setNames(user_dt$logFC, user_dt$gene)
      user_ranked <- user_ranked[!duplicated(names(user_ranked))]
      
      ref_deg <- ref_deg[order(-logFC)]
      ref_ranked <- setNames(ref_deg$logFC, ref_deg$gene)
      ref_ranked <- ref_ranked[!duplicated(names(ref_ranked))]
      
      user_gsea <- tryCatch({
        fgsea::fgseaMultilevel(pathways = pathways, stats = user_ranked, nPermSimple = 1000)
      }, error = function(e) NULL)
      
      ref_gsea <- tryCatch({
        fgsea::fgseaMultilevel(pathways = pathways, stats = ref_ranked, nPermSimple = 1000)
      }, error = function(e) NULL)
      
      if (is.null(user_gsea) || is.null(ref_gsea) || nrow(user_gsea) == 0 || nrow(ref_gsea) == 0) {
        return(ggplot() + 
                 annotate("text", x = 1, y = 1, label = "GSEA against Hallmark pathways failed",
                          size = 5, color = "grey50") + theme_void())
      }
      
      your_nes  <- setNames(user_gsea$NES, user_gsea$pathway)
      your_padj <- setNames(user_gsea$padj, user_gsea$pathway)
      ref_nes   <- setNames(ref_gsea$NES, ref_gsea$pathway)
      ref_padj  <- setNames(ref_gsea$padj, ref_gsea$pathway)
      
      common_pathways <- intersect(names(your_nes), names(ref_nes))
      if (length(common_pathways) == 0) {
        return(ggplot() + 
                 annotate("text", x = 1, y = 1, label = "No shared pathways between user and reference",
                          size = 5, color = "grey50") + theme_void())
      }
      
      df <- data.frame(
        your_nes  = your_nes[common_pathways],
        ref_nes   = ref_nes[common_pathways],
        your_padj = your_padj[common_pathways],
        ref_padj  = ref_padj[common_pathways]
      )
      df <- df[complete.cases(df), ]
      
      if (nrow(df) == 0) {
        return(ggplot() + 
                 annotate("text", x = 1, y = 1, label = "No complete data for pathway comparison",
                          size = 5, color = "grey50") + theme_void())
      }
      
      df$sig <- (df$your_padj < 0.05) & (df$ref_padj < 0.05)
      df_sig <- subset(df, sig)
      
      cor_all <- tryCatch(
        cor.test(df$your_nes, df$ref_nes, method = "spearman"),
        error = function(e) NULL
      )
      if (!is.null(cor_all)) {
        r_all <- round(cor_all$estimate, 3)
        p_all <- format(cor_all$p.value, scientific = TRUE, digits = 2)
      } else {
        r_all <- NA; p_all <- NA
      }
      
      cor_sig <- NULL
      if (nrow(df_sig) >= 3) {
        cor_sig <- tryCatch(
          cor.test(df_sig$your_nes, df_sig$ref_nes, method = "spearman"),
          error = function(e) NULL
        )
      }
      if (!is.null(cor_sig)) {
        r_sig <- round(cor_sig$estimate, 3)
        p_sig <- format(cor_sig$p.value, scientific = TRUE, digits = 2)
      } else {
        r_sig <- NA; p_sig <- NA
      }
      
      label_all <- if (!is.na(r_all))
        paste0("All: R = ", r_all, ", p = ", p_all) else "All: NA"
      label_sig <- if (!is.na(r_sig))
        paste0("FDR<0.05: R = ", r_sig, ", p = ", p_sig) else "FDR<0.05: NA"
      label_text <- paste0(label_all, "\n", label_sig)
      
      ggplot(df, aes(x = ref_nes, y = your_nes)) +
        geom_point(color = "gray40", size = 2) +
        geom_point(data = df_sig, color = "red", size = 2.5) +
        geom_smooth(aes(color = "All Hallmark pathways"), method = "lm",
                    se = FALSE, linewidth = 1) +
        {if (nrow(df_sig) >= 3) geom_smooth(data = df_sig,
                                            aes(color = "FDR < 0.05"), method = "lm",
                                            se = FALSE, linewidth = 1.2)} +
        scale_color_manual(
          name = NULL,
          values = c("All Hallmark pathways" = "gray50",
                     "FDR < 0.05" = "red")
        ) +
        geom_hline(yintercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
        geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
        labs(
          title = paste("Dataset:", gem_id),
          x = "NES (Reference)",
          y = "NES (Your Data)"
        ) +
        theme_bw() +
        theme(
          panel.border   = element_rect(color = "black", fill = NA, linewidth = 0.8),
          plot.title    = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.title    = element_text(size = 12, face = "bold"),
          axis.text     = element_text(size = 10, face = "bold"),
          legend.position = "right",
          legend.text   = element_text(size = 9, face = "bold")
        ) +
        annotate("text", x = -Inf, y = Inf,
                 label = label_text,
                 hjust = 0, vjust = 2, color = "red", size = 4, fontface = "bold")
    })
    
    # Render plots
    output$func_plot <- renderPlot({
      print(func_plot_obj())
    })
    
    # ===== Heatmap: Top Perturbations × Key Query Genes =====
    heatmap_obj <- reactive({
      req(rv$results)
      req(rv$user_dt)
      
      # --- Genes: top 20 up + top 20 down from user query ---
      user_dt <- copy(rv$user_dt)
      user_dt <- user_dt[order(-logFC)]
      n_gene_side <- 20L
      top_up_genes <- head(user_dt$gene, n_gene_side)
      top_dn_genes <- tail(user_dt$gene, n_gene_side)
      heat_genes <- unique(c(top_up_genes, top_dn_genes))
      heat_genes <- toupper(heat_genes)
      
      # --- Perturbations: top 10 positive + top 10 negative by Spearman Rho ---
      results_dt <- copy(rv$results)
      pos_pert <- head(results_dt[order(-Spearman_Rho)], 10L)
      neg_pert <- head(results_dt[order(Spearman_Rho)], 10L)
      top_pert <- rbind(pos_pert, neg_pert)
      top_pert <- unique(top_pert, by = "Dataset")
      
      # Build matrix: rows = perturbations, cols = genes
      mat <- matrix(NA_real_, nrow = nrow(top_pert), ncol = length(heat_genes))
      rownames(mat) <- if ("GEM_ID" %in% names(top_pert)) {
        ids <- top_pert$GEM_ID
        ids[is.na(ids) | ids == ""] <- top_pert$Dataset[is.na(ids) | ids == ""]
        ids
      } else {
        top_pert$Dataset
      }
      colnames(mat) <- heat_genes
      
      for (i in seq_len(nrow(top_pert))) {
        ds <- top_pert$Dataset[i]
        deg <- tryCatch(get_deg_dt_cached(ds, GSEA_DIR), error = function(e) NULL)
        if (is.null(deg) || !"gene" %in% names(deg) || !"logFC" %in% names(deg)) next
        deg[, gene := toupper(gene)]
        deg <- deg[!duplicated(gene) & !is.na(logFC) & is.finite(logFC)]
        common <- intersect(heat_genes, deg$gene)
        if (length(common) == 0) next
        idx <- match(common, deg$gene)
        mat[i, match(common, heat_genes)] <- deg$logFC[idx]
      }
      
      # Remove all-NA rows and cols
      mat <- mat[rowSums(!is.na(mat)) > 0, , drop = FALSE]
      mat <- mat[, colSums(!is.na(mat)) > 0, drop = FALSE]
      
      if (nrow(mat) < 2 || ncol(mat) < 3) {
        return(ggplot() +
                 annotate("text", x = 1, y = 1, label = "Not enough overlapping data for heatmap",
                          size = 5, color = "grey50") + theme_void())
      }
      
      # Clip logFC to [-5, 5]
      mat[mat > 5] <- 5
      mat[mat < -5] <- -5
      
      # Melt for ggplot
      df <- as.data.table(mat, keep.rownames = "Perturbation")
      df <- melt(df, id.vars = "Perturbation", variable.name = "Gene", value.name = "logFC")
      df <- df[!is.na(logFC)]
      
      # Order perturbations by Spearman Rho (already sorted)
      pert_order <- rownames(mat)
      df[, Perturbation := factor(Perturbation, levels = rev(pert_order))]
      
      # Order genes: up first then down
      gene_order <- intersect(toupper(c(top_up_genes, rev(top_dn_genes))), colnames(mat))
      df[, Gene := factor(Gene, levels = gene_order)]
      
      n_pert <- nrow(mat)
      n_up   <- sum(gene_order %in% toupper(top_up_genes))
      n_down <- length(gene_order) - n_up
      
      p <- ggplot(df, aes(x = Gene, y = Perturbation, fill = logFC)) +
        geom_tile(color = "grey90", linewidth = 0.3) +
        scale_fill_gradient2(
          low = "#4285F4", mid = "white", high = "#EA4335",
          midpoint = 0, limits = c(-5, 5),
          name = "logFC\n(in DB)"
        ) +
        labs(
          title = "Top Perturbations × Key Query Genes",
          subtitle = paste("Top 10 positive + 10 negative Spearman Rho | Top", n_gene_side, "up +", n_gene_side, "down genes | Clipped to [-5, 5]"),
          x = "Query Gene", y = "Matching Perturbation"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.border   = element_rect(color = "black", fill = NA, linewidth = 0.8),
          plot.title    = element_text(hjust = 0.5, face = "bold", size = 13),
          plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40"),
          axis.text.x   = element_text(angle = 45, hjust = 1, size = 8, face = "bold"),
          axis.text.y   = element_text(size = 9, face = "bold"),
          axis.title    = element_text(size = 11, face = "bold"),
          panel.grid    = element_blank(),
          legend.position = "right",
          plot.margin    = margin(t = 5, r = 10, b = 5, l = 10)
        )
      
      # Bracket annotation below x-axis
      if (n_up > 0 && n_down > 0) {
        bracket_y  <- -1.2
        text_y     <- -0.7
        tick_bottom <- bracket_y - 0.25
        
        p <- p +
          # Up-regulated bracket
          annotate("segment", x = 1, xend = n_up, 
                   y = bracket_y, yend = bracket_y, 
                   color = "#EA4335", linewidth = 0.6, lineend = "round") +
          annotate("segment", x = 1, xend = 1, 
                   y = bracket_y, yend = tick_bottom, 
                   color = "#EA4335", linewidth = 0.6, lineend = "round") +
          annotate("segment", x = n_up, xend = n_up, 
                   y = bracket_y, yend = tick_bottom, 
                   color = "#EA4335", linewidth = 0.6, lineend = "round") +
          annotate("text", x = (1 + n_up) / 2, y = text_y,
                   label = paste0("Up-regulated (Top ", n_up, ")"),
                   size = 3.2, color = "#EA4335", fontface = "bold", hjust = 0.5) +
          # Down-regulated bracket
          annotate("segment", x = n_up + 1, xend = n_up + n_down,
                   y = bracket_y, yend = bracket_y,
                   color = "#4285F4", linewidth = 0.6, lineend = "round") +
          annotate("segment", x = n_up + 1, xend = n_up + 1,
                   y = bracket_y, yend = tick_bottom,
                   color = "#4285F4", linewidth = 0.6, lineend = "round") +
          annotate("segment", x = n_up + n_down, xend = n_up + n_down,
                   y = bracket_y, yend = tick_bottom,
                   color = "#4285F4", linewidth = 0.6, lineend = "round") +
          annotate("text", x = n_up + (1 + n_down) / 2, y = text_y,
                   label = paste0("Down-regulated (Top ", n_down, ")"),
                   size = 3.2, color = "#4285F4", fontface = "bold", hjust = 0.5) +
          coord_cartesian(clip = "off")
      }
      p
    })
    
    output$heatmap_plot <- renderPlot({
      print(heatmap_obj())
    })
    
    output$download_heatmap_plot <- downloadHandler(
      filename = function() { paste0("PerturbQuery_Heatmap_", Sys.Date(), ".pdf") },
      content = function(file) {
        p <- heatmap_obj()
        ggsave(file, plot = p, device = "pdf", width = 14, height = 8)
      }
    )
    
    # Download handlers for plots
    output$download_func_plot <- downloadHandler(
      filename = function() {
        sdata <- selected_dataset_data()
        gem_id <- if (!is.null(sdata$gem_id)) sdata$gem_id else "FuncPlot"
        paste0("FuncPlot_", safe_filename(gem_id), ".pdf")
      },
      content = function(file) {
        p <- func_plot_obj()
        ggsave(file, plot = p, device = "pdf", width = 8, height = 6)
      }
    )
    
    # ===== Download CSV results =====
    output$download_results <- downloadHandler(
      filename = function() {
        type_f <- input$type_filter
        if (is.null(type_f) || type_f == "both") {
          active_tab <- input$results_tabset
          type_suffix <- if (!is.null(active_tab) && active_tab %in% c("Chemical", "Genetic")) active_tab else "All"
        } else if (type_f == "chemical") {
          type_suffix <- "Chemical"
        } else {
          type_suffix <- "Genetic"
        }
        paste0("perturb_query_results_", type_suffix, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv$results)
        dt <- copy(rv$results)
        # Filter by active tab
        type_f <- input$type_filter
        if (is.null(type_f) || type_f == "both") {
          active_tab <- input$results_tabset
          if (!is.null(active_tab) && active_tab %in% c("Chemical", "Genetic")) {
            dt <- dt[Type == active_tab]
            effective_type <- active_tab
          } else {
            effective_type <- NULL
          }
        } else if (type_f == "chemical") {
          dt <- dt[Type == "Chemical"]
          effective_type <- "Chemical"
        } else {
          dt <- dt[Type == "Genetic"]
          effective_type <- "Genetic"
        }
        # Drop type-irrelevant columns
        if (!is.null(effective_type)) {
          if (effective_type == "Chemical") {
            drop_type <- intersect(c("pbgene", "pb_ENSEMBL", "pb_ENSEMB", "gene_biotype",
                                     "method", "known_targets", "protein_class",
                                     "disease_area", "indication",
                                     "Type", "row_key", "query_genes",
                                     "Time", "Dose", "uniprot_id", "entrez_id"),
                                   names(dt))
          } else {
            drop_type <- intersect(c("Perturbagen", "Time", "Dose", "target",
                                     "known_targets", "uniprot_id", "entrez_id",
                                     "disease_area", "indication",
                                     "pb_ENSEMBL", "gene_biotype", "Type",
                                     "row_key", "query_genes",
                                     "pb_ENSEMB", "method", "clinical_phase", "moa"),
                                   names(dt))
          }
          if (length(drop_type) > 0) dt[, (drop_type) := NULL]
        }
        # Fill missing GEM_ID from Dataset first
        if ("GEM_ID" %in% names(dt) && "Dataset" %in% names(dt)) {
          dt[is.na(GEM_ID) | GEM_ID == "", GEM_ID := Dataset]
        }
        # Ensure GEM_ID exists (rename Dataset if needed)
        if (!"GEM_ID" %in% names(dt) && "Dataset" %in% names(dt)) {
          setnames(dt, "Dataset", "GEM_ID")
        }
        # ---- UNCONDITIONALLY strip Dataset and all internal columns ----
        drop <- intersect(c("Dataset", ".gem_id_raw", ".source_data_file", "perturb_id"), names(dt))
        if (length(drop) > 0) dt[, (drop) := NULL]
        # Move GEM_ID to first column, rename to Dataset for export
        if ("GEM_ID" %in% names(dt)) {
          setcolorder(dt, c("GEM_ID", setdiff(names(dt), "GEM_ID")))
          setnames(dt, "GEM_ID", "Dataset")
        }
        # Rename Perturbagen to PbChem for CSV output
        if ("Perturbagen" %in% names(dt)) setnames(dt, "Perturbagen", "PbChem")
        # Capitalize first letter of column names for CSV output
        cap_cols <- intersect(c("pbgene", "method"), names(dt))
        for (cc in cap_cols) setnames(dt, cc, paste0(toupper(substr(cc, 1, 1)), substr(cc, 2, nchar(cc))))
        # Rename protein_class to Class for CSV (match table display)
        if ("protein_class" %in% names(dt)) setnames(dt, "protein_class", "Class")
        strip_html(dt)
        fwrite(dt, file)
      }
    )
    
    # ===== Dataset Click Modal (unchanged) =====
    observeEvent(input$click_dataset, {
      ds <- input$click_dataset
      if (is.null(ds) || !nzchar(ds)) return(NULL)
      
      dt <- isolate(rv$results)
      if (is.null(dt) || nrow(dt) == 0) return(NULL)
      
      row <- dt[Dataset == ds]
      if (is.null(row) || nrow(row) == 0) return(NULL)
      row <- row[1]
      
      rho_val <- as.numeric(row$Spearman_Rho)
      pval_val <- as.numeric(row$Pvalue)
      overlap_val <- as.integer(row$Overlap)
      
      # Load metadata (.fst)
      all_meta_files <- c(chem_meta_files, gen_meta_files)
      meta_list <- list()
      for (meta_file in all_meta_files) {
        if (!file.exists(meta_file)) next
        meta_dt <- tryCatch(
          read_fst(meta_file, as.data.table = TRUE),
          error = function(err) NULL
        )
        if (is.null(meta_dt) || !"Dataset" %in% names(meta_dt)) next
        meta_list[[length(meta_list) + 1]] <- meta_dt
      }
      
      if (length(meta_list) > 0) {
        meta_merged <- rbindlist(meta_list, fill = TRUE, use.names = TRUE)
        meta_for_merge <- meta_merged[, c("Dataset", setdiff(names(meta_merged), "Dataset")), with = FALSE]
        row <- merge(data.table(Dataset = ds), meta_for_merge, by = "Dataset", all.x = TRUE)
        if (nrow(row) == 0) row <- as.list(dt[1])
      }
      
      # Detect type
      type <- "chemical"
      compound <- ""
      gene <- ""
      if (!is.na(row$Perturbagen) && nzchar(as.character(row$Perturbagen))) {
        type <- "chemical"
        compound <- as.character(row$Perturbagen)
      } else if (!is.na(row$pbgene) && nzchar(as.character(row$pbgene))) {
        type <- "genetic"
        gene <- as.character(row$pbgene)
      } else if (!is.na(row$Gene) && nzchar(as.character(row$Gene))) {
        type <- "genetic"
        gene <- as.character(row$Gene)
      }
      
      link_html <- NULL
      mol_img <- NULL
      
      if (type == "chemical" && nzchar(compound)) {
        cid_url <- URLencode(compound, reserved = TRUE)
        img_2d <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", cid_url, "/PNG")
        img_3d <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", cid_url, "/PNG?record_type=3d")
        mol_img <- div(
          class = "modal-image-row",
          fluidRow(
            column(6,
                   div(class = "modal-image-card",
                       div(class = "modal-img-box",
                           tags$img(src = img_2d, class = "modal-structure-img", onerror = "this.parentElement.parentElement.style.display='none'")
                       ),
                       div(class = "modal-img-caption", "2D Structure")
                   )
            ),
            column(6,
                   div(class = "modal-image-card",
                       div(class = "modal-img-box",
                           tags$img(src = img_3d, class = "modal-structure-img", onerror = "this.parentElement.parentElement.style.display='none'")
                       ),
                       div(class = "modal-img-caption", "3D Ball-Stick Model")
                   )
            )
          ),
          br()
        )
        link_html <- tags$a(href=paste0("https://pubchem.ncbi.nlm.nih.gov/compound/", cid_url), target="_blank", class="btn btn-sm btn-primary", icon("external-link-alt"), " Open in PubChem")
      }
      
      if (type == "genetic" && nzchar(gene)) {
        g <- URLencode(gene, reserved = TRUE)
        string_img <- paste0("https://string-db.org/api/image/network?identifiers=", g, "&species=9606")
        mol_img <- div(
          class = "modal-image-row",
          div(class = "modal-image-card",
              div(class = "modal-img-box-large",
                  tags$img(src = string_img, class = "modal-structure-img", onerror = "this.parentElement.parentElement.style.display='none'")
              ),
              div(class = "modal-img-caption", "Protein-Protein Interaction Network (STRING)")
          ),
          br()
        )
        link_html <- tags$a(href=paste0("https://www.uniprot.org/uniprotkb?query=", g), target="_blank", class="btn btn-sm btn-warning", icon("external-link-alt"), " Open in UniProt")
      }
      
      # Get DEG data and create plots (same as GSQuery/Synergy)
      deg_dt <- get_deg_dt_cached(ds, GSEA_DIR)
      
      plot_lollipop <- NULL; plot_density <- NULL; plot_main_1 <- NULL
      plot_main_2 <- NULL; plot_count_bar <- NULL; plot_cdf <- NULL
      valid_data <- FALSE; deg <- NULL; has_pval <- FALSE; has_fdr <- FALSE
      
      if (!is.null(deg_dt) && nrow(deg_dt) > 0) {
        gene_candidates <- c("Gene","gene","symbol","SYMBOL","gene_name","GeneSymbol","gene_id")
        fc_candidates   <- c("logFC","LogFC","log2FC","Log2FC","log2FoldChange","fc","value","Log2 (FC_avg)")
        pval_candidates <- c("pvalue","p_value","p.val","Pvalue","P_value","P.val","p","P.Value","p-value","")
        fdr_candidates  <- c("fdr","FDR","padj","p.adj","p_adj","Padj","P.adj","P_adj","FDR-BH","adj.P.Val")
        
        gene_col <- gene_candidates[gene_candidates %in% names(deg_dt)][1]
        fc_col   <- fc_candidates[fc_candidates %in% names(deg_dt)][1]
        pval_col <- pval_candidates[pval_candidates %in% names(deg_dt)][1]
        fdr_col  <- fdr_candidates[fdr_candidates %in% names(deg_dt)][1]
        
        if (!is.na(gene_col) && !is.na(fc_col)) {
          deg_copy <- copy(deg_dt)
          deg <- data.table(gene = as.character(deg_copy[[gene_col]]), logFC = suppressWarnings(as.numeric(deg_copy[[fc_col]])))
          if (!is.na(pval_col) && !is.na(fdr_col)) {
            deg$pval <- suppressWarnings(as.numeric(deg_copy[[pval_col]]))
            deg$fdr  <- suppressWarnings(as.numeric(deg_copy[[fdr_col]]))
            if (sum(!is.na(deg$pval)) > 0 && sum(!is.na(deg$fdr)) > 0) {
              has_pval <- TRUE; has_fdr <- TRUE
            }
          }
          deg <- deg[!is.na(logFC) & !is.na(gene) & nzchar(gene) & grepl("[A-Za-z]", gene)]
          if (nrow(deg) > 0) valid_data <- TRUE
        }
      }
      
      if (valid_data && !is.null(deg)) {
        deg$group <- ifelse(deg$logFC > 1, "Up", ifelse(deg$logFC < -1, "Down", "NS"))
        group_colors <- c("Down"="#4285F4", "NS"="gray", "Up"="#EA4335")
        
        # 1. Count bar chart
        count_stats <- deg[, .(gene_count = .N), by = group]
        count_stats$group <- factor(count_stats$group, levels = c("Down", "NS", "Up"))
        plot_count_bar <- ggplot(count_stats, aes(x=group, y=gene_count, fill=group)) +
          geom_col(width=0.7, show.legend = FALSE) + scale_fill_manual(values=group_colors) +
          geom_text(aes(label=gene_count), vjust=-0.3, fontface="bold") +
          labs(title="Differential Gene Count", x="", y="Gene Number") + theme_bw()
        
        # 2. CDF curve
        deg_sorted <- deg[order(logFC)]
        deg_sorted$cdf <- seq(0, 1, length.out = nrow(deg_sorted))
        deg_sorted$group <- "NS"
        deg_sorted$group[deg_sorted$logFC < -1] <- "Down"
        deg_sorted$group[deg_sorted$logFC > 1] <- "Up"
        deg_sorted$group <- factor(deg_sorted$group, levels = c("Down", "NS", "Up"))
        deg_sorted$gene <- as.character(deg_sorted$gene)
        deg_sorted$gene[is.na(deg_sorted$gene) | deg_sorted$gene == ""] <- "unknown"
        
        top10_genes <- deg[order(-abs(logFC))][1:min(10, .N)]
        top10_genes <- top10_genes[!is.na(gene) & nzchar(as.character(gene))]
        top10_genes$gene <- as.character(top10_genes$gene)
        setkey(deg_sorted, gene)
        top10_cdf <- merge(copy(top10_genes)[, .(gene, logFC)], deg_sorted[, .(gene, cdf)], by = "gene", all.x = TRUE)
        
        plot_cdf <- ggplot(deg_sorted, aes(x=logFC, y=cdf, color=group, text=gene)) +
          geom_line(linewidth=1.1, alpha=0.9) + geom_point(size=0.7, alpha=0.8) +
          scale_color_manual(values=group_colors) + geom_vline(xintercept=c(-1,0,1), color="black", linewidth=0.35, linetype="dashed") +
          labs(title="logFC Cumulative Distribution (CDF)", x="logFC", y="Cumulative Proportion") +
          ggrepel::geom_text_repel(data = top10_cdf, aes(x=logFC, y=cdf, label=gene),
                                   size=3, max.overlaps=15, box.padding=0.3, color="black") +
          theme_bw() + theme(legend.position="bottom", plot.title=element_text(face="bold", size=13))
        
        # 3/4. Main plot: Ranked logFC
        {
          deg_sorted2 <- deg[order(logFC)]
          deg_sorted2$rank <- 1:nrow(deg_sorted2)
          top10_rank <- merge(copy(top10_genes)[, .(gene, logFC)], deg_sorted2[, .(gene, rank, group)], by="gene", all.x=TRUE)
          top10_rank <- top10_rank[!is.na(rank)]
          plot_main_1 <- ggplot(deg_sorted2, aes(x=rank, y=logFC, color=group, text=gene)) +
            geom_point(size=0.8, alpha=0.6) + scale_color_manual(values=group_colors) +
            geom_hline(yintercept=c(-1,0,1), linetype="dashed", linewidth=0.3) +
            labs(title="Gene Ranked by logFC", x="Gene Rank", y="logFC") +
            ggrepel::geom_text_repel(data=top10_rank, aes(x=rank, y=logFC, label=gene),
                                     size=3, max.overlaps=15, box.padding=0.3) +
            theme_bw() + theme(legend.position="bottom")
          
          # logFC Distribution histogram
          deg$bin <- cut(deg$logFC, breaks=seq(-10,10,1), include.lowest=TRUE)
          bin_stats <- deg[, .(count=.N), by=.(bin, group)]
          plot_main_2 <- ggplot(bin_stats, aes(x=bin, y=count, fill=group)) +
            geom_col(position="stack", width=0.8) + scale_fill_manual(values=group_colors) +
            geom_vline(xintercept=which(levels(bin_stats$bin)=="(-0.5,0]"), linetype="dashed", linewidth=0.3) +
            labs(title="logFC Distribution", x="logFC Bins", y="Gene Count") +
            theme_bw() + theme(legend.position="bottom", axis.text.x=element_text(angle=45, hjust=1, size=8))
        }
        
        # 5. Lollipop plot
        top_up <- head(deg[logFC>0][order(-logFC)], 20)
        top_down <- head(deg[logFC<0][order(logFC)], 20)
        top_genes_lolli <- rbindlist(list(top_up, top_down), fill=TRUE)
        if (nrow(top_genes_lolli) > 0) {
          top_genes_lolli$direction <- ifelse(top_genes_lolli$logFC > 0, "Up", "Down")
          top_genes_lolli$gene <- as.character(top_genes_lolli$gene)
          top_genes_lolli <- top_genes_lolli[!is.na(gene) & nzchar(gene)]
          if (nrow(top_genes_lolli) > 0) {
            plot_lollipop <- ggplot(top_genes_lolli, aes(x=reorder(gene, logFC), y=logFC, fill=direction, text=gene)) +
              geom_col(width=0.2) + geom_point(size=2, shape=21) + coord_flip() +
              labs(title="Top 20 Genes", x="", y="logFC", fill="Direction") +
              scale_fill_manual(values=c("Up"="#EA4335","Down"="#4285F4")) +
              theme_bw() + theme(axis.text.y=element_text(size=8), legend.position="bottom")
          }
        }
        
        # 6. Density plot
        plot_density <- ggplot(deg, aes(x=logFC)) +
          geom_density(fill="#76A7FA", alpha=0.4) + geom_vline(xintercept=0, linetype="dashed") +
          labs(title="Density", x="logFC", y="Density") + theme_bw()
      }
      
      # Combine plots for display and PDF download
      modal_ggplots <- list()
      combined_plot <- NULL
      
      if (valid_data) {
        modal_ggplots <- list(count_bar=plot_count_bar, cdf=plot_cdf, main_1=plot_main_1,
                              main_2=plot_main_2, density=plot_density, lollipop=plot_lollipop)
        modal_ggplots <- Filter(function(x) !is.null(x) && inherits(x, "ggplot"), modal_ggplots)
        
        if (length(modal_ggplots) > 0) {
          theme_small <- theme_bw() + theme(
            plot.title = element_text(size=11, face="bold", hjust=0.5),
            axis.title = element_text(size=9), axis.text = element_text(size=8),
            legend.title = element_text(size=9), legend.text = element_text(size=8),
            legend.position = "bottom", plot.margin = margin(8,8,8,8), legend.margin = margin(5,5,5,5)
          )
          modal_ggplots <- lapply(modal_ggplots, function(p) p + theme_small)
          
          n_plots <- length(modal_ggplots)
          combined_plot <- switch(as.character(n_plots),
                                  "1" = modal_ggplots[[1]],
                                  "2" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], ncol=2),
                                  "3" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], modal_ggplots[[3]], ncol=3),
                                  "4" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], modal_ggplots[[3]], modal_ggplots[[4]], ncol=2),
                                  "5" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], modal_ggplots[[3]], modal_ggplots[[4]], modal_ggplots[[5]], ncol=3),
                                  { p1 <- wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], modal_ggplots[[3]], ncol=3)
                                  p2 <- wrap_plots(modal_ggplots[[4]], modal_ggplots[[5]], modal_ggplots[[6]], ncol=3)
                                  wrap_plots(p1, p2, ncol=1) }
          )
        }
        
        modal_plot_store(list(
          dataset=as.character(ds),
          gem_id=if (!is.null(row$GEM_ID) && nzchar(as.character(row$GEM_ID))) as.character(row$GEM_ID) else as.character(ds),
          ggplots=modal_ggplots,
          combined_plot=combined_plot
        ))
      } else {
        modal_plot_store(NULL)
      }
      
      # Effect tag
      effect_class <- ifelse(!is.na(rho_val) && rho_val > 0, "effect-activator", "effect-suppressor")
      effect_tag <- div(class = paste("detail-effect", effect_class),
                        icon("chart-line"),
                        paste("Spearman Rho:", round(rho_val, 4), "| Overlap:", overlap_val, "| P:", format.pval(pval_val, 3)))
      
      # Build metadata items
      skip_cols <- c("dataset", "Dataset", "Download", "download", "Type")
      display_map <- c("GEM_ID" = "Dataset", "Perturbagen" = "PbChem")
      items <- list()
      for (col in names(row)) {
        if (col %in% skip_cols) next
        val <- row[[col]]
        if (is.null(val) || length(val) == 0 || identical(as.character(val), "") || identical(as.character(val), "NA") || (length(val) == 1 && is.na(val))) next
        if (is.numeric(val) && !is.infinite(val)) val <- round(as.numeric(val), 4)
        if (is.numeric(val) && is.infinite(val)) val <- ifelse(val > 0, "Inf", "-Inf")
        dcol <- if (col %in% names(display_map)) display_map[col] else col
        items[[length(items)+1]] <- div(class="detail-item", span(class="detail-label", paste0(dcol, ": ")), span(class="detail-value", gsub("<[^>]+>", "", as.character(val))))
      }
      
      showModal(modalDialog(
        tags$div(
          id = ns("dataset_detail_modal"),
          tags$script(HTML(sprintf("
            setTimeout(function() {
              var el = document.getElementById('%s');
              if (el) { $(el).closest('.modal-dialog').addClass('dataset-wide-modal'); }
            }, 50);
          ", ns("dataset_detail_modal")))),
          
          div(class = "detail-card",
              effect_tag, br(), link_html, mol_img,
              items,
              
              if(valid_data) {
                tagList(
                  br(), hr(),
                  tags$div(style="margin:10px 0 16px 0; text-align:center;",
                           downloadButton(ns("download_combined_pdf"), "Download Plot", class="btn btn-default btn-lg")
                  ),
                  withSpinner(plotOutput(ns("modal_combined"), height="650px"), type=4, color="#3a7bd5")
                )
              }
          )
        ),
        size = "l", easyClose = TRUE, footer = modalButton("Close")
      ))
      
      # Render combined plot
      if(valid_data) {
        output$modal_combined <- renderPlot({
          st <- modal_plot_store()
          req(st$combined_plot)
          print(st$combined_plot)
        })
      }
    })
    
    # ================== 单数据集动态下载（Blob方式，绕过downloadLink程序化点击失效） ==================
    observeEvent(input$download_single_ds, {
      req(input$download_single_ds)
      ds  <- input$download_single_ds$ds
      gem <- input$download_single_ds$gem
      ts  <- input$download_single_ds$ts

      # 防重复下载：同一 gem+ds 组合 1 秒内不重复执行
      key <- paste0("_dl_ts_", ds, "_", gem)
      last <- session$userData[[key]]
      if (!is.null(last) && (ts - last) < 1000) return()
      session$userData[[key]] <- ts

      dt <- resolve_dataset_data(ds)
      content <- if (!is.null(dt) && nrow(dt) > 0) {
        # Keep only gene & logfc columns
        gene_candidates <- c("Gene","gene","symbol","SYMBOL","gene_name","GeneSymbol","gene_id")
        fc_candidates   <- c("logFC","LogFC","log2FC","Log2FC","log2FoldChange","fc","value","Log2 (FC_avg)")
        gene_col <- intersect(gene_candidates, names(dt))
        fc_col   <- intersect(fc_candidates, names(dt))
        keep_cols <- c(if(length(gene_col) > 0) gene_col[1] else NULL,
                       if(length(fc_col) > 0) fc_col[1] else NULL)
        if (length(keep_cols) > 0) {
          dt <- dt[, ..keep_cols]
          setnames(dt, old = keep_cols, new = c("Gene", "LogFC")[1:length(keep_cols)])
        }
        tmp <- tempfile(fileext = ".csv")
        fwrite(dt, file = tmp, sep = ",", quote = TRUE, na = "NA")
        txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
        unlink(tmp)
        txt
      } else {
        paste0('"error","No data found for dataset: ', ds, '"')
      }

      safe_name <- gsub("[^A-Za-z0-9_.-]", "_", gem)
      session$sendCustomMessage("trigger_csv_download", list(
        filename = paste0(safe_name, ".csv"),
        content = content
      ))
    })

  })
}