# ============================================
# GSQuery Module for PerturbSigDB
# 基于 GSEA 的扰动查询功能
# 输入：基因列表（无logFC）
# 功能：找出哪些扰动会影响该基因集
# ============================================

# ===== Signature Index Configuration =====
SIGNATURE_INDEX_DIR <- "GSEA/signatures"
SIGNATURE_LOOKUP_FILE <- "GSEA/signatures/signature_lookup.rds"
FILTER_CANDIDATES <- 10000   # 快速过滤后保留的候选签名数

# ===== 极简索引生成（只存文件路径和基因数）=====
generate_signature_index <- function(gsea_dir, n_cores = parallel::detectCores() - 1) {
  cat("[GSQuery] Generating minimal signature index...\n")
  
  fst_files <- list.files(gsea_dir, pattern = "\\.fst$", full.names = TRUE, recursive = TRUE)
  total_files <- length(fst_files)
  cat("[GSQuery] Total files:", total_files, "\n")
  
  # Create index directory
  index_dir <- file.path(gsea_dir, "signatures")
  if (!dir.exists(index_dir)) dir.create(index_dir, recursive = TRUE)
  
  # Extract name and gene count only (super fast via metadata)
  extract_basic <- function(f) {
    tryCatch({
      sig_name <- sub("\\.fst$", "", basename(f))
      dt <- read_fst(f, as.data.table = TRUE)
      list(name = sig_name, file = f, n_genes = nrow(dt))
    }, error = function(e) NULL)
  }
  
  batch_size <- 20000
  n_batches <- ceiling(total_files / batch_size)
  all_meta <- vector("list", total_files)
  idx <- 1
  
  for (batch in 1:n_batches) {
    start_idx <- (batch-1)*batch_size + 1
    end_idx <- min(batch*batch_size, total_files)
    batch_files <- fst_files[start_idx:end_idx]
    cat(sprintf("[GSQuery] Batch %d/%d...\n", batch, n_batches))
    
    results <- parallel::mclapply(batch_files, extract_basic,
                                  mc.cores = n_cores, mc.preschedule = TRUE)
    for (r in results) {
      if (!is.null(r)) { all_meta[[idx]] <- r; idx <- idx + 1 }
    }
    if (batch %% 10 == 0) gc()
  }
  all_meta <- all_meta[1:(idx-1)]
  
  # Save compact data.table (just strings + ints)
  lookup_dt <- data.table(
    name   = sapply(all_meta, `[[`, "name"),
    file   = sapply(all_meta, `[[`, "file"),
    n_genes = sapply(all_meta, `[[`, "n_genes"),
    stringsAsFactors = FALSE
  )
  
  lookup_file <- file.path(index_dir, "signature_lookup.rds")
  saveRDS(lookup_dt, lookup_file)
  cat("[GSQuery] Index saved:", lookup_file,
      "(", format(object.size(lookup_dt), "auto"), ")\n")
  cat("[GSQuery] Total indexed:", nrow(lookup_dt), "\n")
  
  return(invisible(lookup_file))
}

# ===== 倒排索引：gene → signature_row_indices =====
# 每个签名提取 top N up genes + bottom N down genes（默认各200）
# 索引大小：~3 GB（109万签名 × 400基因/签名）
INVERTED_INDEX_FILE <- "GSEA/signatures/inverted_index.rds"
N_TOP_BOTTOM <- 200

build_inverted_index <- function(gsea_dir, n_top = N_TOP_BOTTOM, 
                                  n_cores = parallel::detectCores() - 1) {
  cat("[GSQuery] Building inverted index (gene → signatures)...\n")
  
  # Find all fst files
  fst_files <- list.files(gsea_dir, pattern = "\\.fst$", full.names = TRUE, recursive = TRUE)
  total_files <- length(fst_files)
  cat("[GSQuery] Total files:", total_files, "\n")
  
  # Process each file: extract top up + bottom down genes
  extract_top_bottom <- function(f) {
    tryCatch({
      dt <- read_fst(f, as.data.table = TRUE)
      dt <- dt[order(-logFC)]
      n <- nrow(dt)
      n_extract <- min(n_top, n %/% 2)
      top_up <- toupper(dt[1:n_extract, gene])
      top_down <- toupper(dt[(n - n_extract + 1):n, gene])
      unique(c(top_up, top_down))
    }, error = function(e) character(0))
  }
  
  # Batch processing
  batch_size <- 20000
  n_batches <- ceiling(total_files / batch_size)
  all_genes <- vector("list", total_files)
  idx <- 1
  
  for (batch in 1:n_batches) {
    start_i <- (batch - 1) * batch_size + 1
    end_i <- min(batch * batch_size, total_files)
    batch_files <- fst_files[start_i:end_i]
    cat(sprintf("[GSQuery] Inverted index batch %d/%d...\n", batch, n_batches))
    
    batch_genes <- parallel::mclapply(batch_files, extract_top_bottom,
                                       mc.cores = n_cores, mc.preschedule = TRUE)
    for (genes in batch_genes) {
      if (length(genes) > 0) { all_genes[[idx]] <- genes; idx <- idx + 1 }
    }
    if (batch %% 5 == 0) gc()
  }
  all_genes <- all_genes[1:(idx - 1)]
  n_sigs <- length(all_genes)
  
  # Build inverted index: gene → integer vector of signature indices
  cat("[GSQuery] Building gene→signature mapping...\n")
  
  # Collect all (gene, sig_idx) pairs
  gene_sig_pairs <- vector("list", n_sigs)
  for (i in 1:n_sigs) {
    gene_sig_pairs[[i]] <- data.table(gene = all_genes[[i]], sig_idx = i)
  }
  pair_dt <- rbindlist(gene_sig_pairs)
  rm(gene_sig_pairs, all_genes); gc()
  
  # Group by gene → list of sig_idx
  setkey(pair_dt, gene)
  inverted <- pair_dt[, .(sig_indices = list(sig_idx)), by = gene]
  
  # Save
  index_file <- file.path(gsea_dir, "signatures", "inverted_index.rds")
  saveRDS(inverted, index_file, compress = TRUE)
  cat("[GSQuery] Inverted index saved:", index_file,
      "(", format(object.size(inverted), "auto"), ")\n")
  cat("[GSQuery] Unique genes:", nrow(inverted), "\n")
  
  return(invisible(index_file))
}

# ===== Global GSQuery Core Function =====
run_gsquery_global <- function(query_genes, gsea_dir, top_n = 100, progress_file = NULL, type_filter = "both", n_cores = if (.Platform$OS.type == "windows") 1L else 2L,
                                lookup_cache = NULL, inverted_cache = NULL) {
  cat("[GSQuery] Starting with", length(query_genes), "genes\n")
  
  # Helper: write progress to file for polling from Shiny
  update_progress <- function(pct, msg = "") {
    if (!is.null(progress_file)) {
      writeLines(sprintf("%.1f|%s", pct, msg), progress_file)
    }
  }
  
  # Step 0: Load index (or use cache)
  update_progress(5, "Loading signature index...")
  
  if (!is.null(lookup_cache)) {
    lookup <- lookup_cache
    total_files <- nrow(lookup)
    cat("[GSQuery]", total_files, "signatures (cached)\n")
  } else {
    lookup_file <- file.path(gsea_dir, "signatures", "signature_lookup.rds")
    if (file.exists(lookup_file)) {
      cat("[GSQuery] Loading index...\n")
      lookup <- readRDS(lookup_file)
      total_files <- nrow(lookup)
      cat("[GSQuery]", total_files, "signatures loaded\n")
    } else {
      cat("[GSQuery] Building index first...\n")
      generate_signature_index(gsea_dir, n_cores = 4)
      lookup <- readRDS(lookup_file)
      total_files <- nrow(lookup)
    }
  }
  
  query_genes_upper <- toupper(query_genes)
  
  # ============ Phase 1: 倒排索引快速过滤 ============
  update_progress(10, "Loading inverted index...")
  
  if (!is.null(inverted_cache)) {
    cat("[GSQuery] Phase 1: Inverted index lookup (cached)...\n")
    inverted <- inverted_cache
  } else {
    inverted_file <- file.path(gsea_dir, "signatures", "inverted_index.rds")
    if (!file.exists(inverted_file)) {
      cat("[GSQuery] Inverted index not found, building...\n")
      build_inverted_index(gsea_dir, n_cores = 4)
    }
    cat("[GSQuery] Phase 1: Inverted index lookup...\n")
    inverted <- readRDS(inverted_file)
  }
  
  update_progress(15, "Searching inverted index...")
  matched_genes <- inverted[gene %in% query_genes_upper]
  
  if (nrow(matched_genes) == 0) {
    cat("[GSQuery] No query genes found in inverted index\n")
    update_progress(100, "No results found")
    return(NULL)
  }
  
  pairs_list <- matched_genes[, .(sig_idx = unlist(sig_indices)), by = gene]
  hit_counts <- pairs_list[, .N, by = sig_idx][order(-N)]
  
  n_candidates <- min(FILTER_CANDIDATES, nrow(hit_counts))
  candidate_idx <- hit_counts$sig_idx[1:n_candidates]
  cat("[GSQuery] Top", n_candidates, "candidates from inverted index\n")
  
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
      cat(sprintf("[GSQuery] Type filter (%s): %d → %d candidates\n", type_filter, n_before, n_after))
      
      n_candidates <- min(FILTER_CANDIDATES, nrow(hit_counts))
      if (n_candidates == 0) {
        cat("[GSQuery] No candidates after type filter\n")
        update_progress(100, "No results after type filter")
        return(NULL)
      }
      candidate_idx <- hit_counts$sig_idx[1:n_candidates]
      cat("[GSQuery] Top", n_candidates, "candidates after type filter\n")
    }
  }
  
  # ============ Phase 2: MWU 快速打分（无置换，精确公式）============
  cat("[GSQuery] Phase 2: MWU scoring on candidates...\n")
  update_progress(20, "MWU scoring candidates...")
  
  n_query <- length(query_genes_upper)
  
  process_mwu <- function(row_idx) {
    tryCatch({
      row <- lookup[row_idx]
      dt <- read_fst(row$file, as.data.table = TRUE)
      genes_all <- toupper(dt$gene)
      logfc <- dt$logFC
      names(logfc) <- genes_all
      logfc <- logfc[!duplicated(names(logfc))]
      
      n_total <- length(logfc)
      if (n_total < 10) return(NULL)
      
      is_query <- names(logfc) %in% query_genes_upper
      n_matched <- sum(is_query)
      if (n_matched < 3) return(NULL)
      
      n_bg <- n_total - n_matched
      n_overlap <- length(intersect(query_genes_upper, genes_all))
      
      rank_pos <- rank(-logfc, ties.method = "average")
      q_ranks <- rank_pos[is_query]
      
      sum_ranks <- sum(q_ranks)
      U <- sum_ranks - n_matched * (n_matched + 1) / 2
      E_U <- n_matched * n_bg / 2
      
      es <- (E_U - U) / (n_matched * n_bg / 2)
      var_U <- n_matched * n_bg * (n_matched + n_bg + 1) / 12
      z <- -(U - E_U) / sqrt(var_U)
      # log-space p-value to avoid underflow when z > 37
      log_p <- pnorm(-abs(z), log.p = TRUE) + log(2)
      p <- if (log_p > log(.Machine$double.xmin)) exp(log_p) else .Machine$double.xmin
      
      rank_deviation <- abs(q_ranks - (n_total / 2))
      names(rank_deviation) <- names(logfc)[is_query]
      leading_edge <- names(sort(rank_deviation, decreasing = TRUE))
      leading_edge <- head(leading_edge, 20)
      
      data.table(
        Dataset = row$name, N_Genes = row$n_genes,
        Overlap = n_overlap,
        Overlap_Pct = round(n_overlap / n_query * 100, 2),
        ES = round(es, 4), NES = round(z, 4),
        Pvalue = p,
        Leading_Edge = paste(leading_edge, collapse = ",")
      )
    }, error = function(e) NULL)
  }
  
  # Parallel MWU scoring
  available_cores <- suppressWarnings(parallel::detectCores())
  if (length(available_cores) != 1L || is.na(available_cores)) available_cores <- 2L
  mwu_cores <- max(1L, min(as.integer(n_cores), max(1L, available_cores - 1L)))
  cat("[GSQuery] MWU parallel on", n_candidates, "candidates with", mwu_cores, "cores\n")
  
  N_BATCHES <- 5L
  nchunks    <- mwu_cores * N_BATCHES
  chunk_size <- max(1L, ceiling(n_candidates / nchunks))
  chunks <- split(candidate_idx, ceiling(seq_along(candidate_idx) / chunk_size))
  n_chunks <- length(chunks)
  cat("[GSQuery] MWU split into", n_chunks, "chunks,", N_BATCHES, "batches\n")
  
  results_list <- list()
  batch_size <- ceiling(n_chunks / N_BATCHES)
  t_mwu <- proc.time()
  
  for (b in seq_len(N_BATCHES)) {
    bidx <- ((b - 1) * batch_size + 1):min(b * batch_size, n_chunks)
    if (length(bidx) == 0) break
    
    batch_results <- parallel::mclapply(bidx, function(ci) {
      chunk <- chunks[[ci]]
      lapply(chunk, process_mwu)
    }, mc.cores = min(mwu_cores, length(bidx)), mc.preschedule = TRUE)
    
    results_list <- c(results_list, batch_results)
    
    pct <- 20 + (b / N_BATCHES) * 80
    update_progress(pct, sprintf("MWU: batch %d/%d done (%d chunks, ~%d sigs scored)",
      b, N_BATCHES, max(bidx), length(results_list)))
    
    cat(sprintf("[GSQuery]   Batch %d/%d done in %.1fs\n",
      b, N_BATCHES, (proc.time() - t_mwu)[3]))
  }
  
  results_list <- unlist(results_list, recursive = FALSE)
  
  update_progress(100, "Finalizing results...")
  
  result <- rbindlist(results_list)
  
  if (nrow(result) == 0) {
    cat("[GSQuery] No results\n")
    return(NULL)
  }
  
  # FDR correction across all results
  result[, Padj := p.adjust(Pvalue, method = "BH")]
  
  result <- result[!is.na(NES)][order(-abs(NES))]
  result <- result[!duplicated(Dataset)]
  if (nrow(result) > top_n) result <- head(result, top_n)
  
  cat("[GSQuery]", nrow(result), "final results\n")
  return(result)
}

GSQueryUI <- function(id, title = "GSQuery") {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .gsquery-box {
          background-color: #f0f6ff;
          border-radius: 12px;
          padding: 20px;
          margin-bottom: 20px;
        }
        /* Fix DT scrollX header-body misalignment */
        .dataTables_scrollHeadInner { width: 100% !important; padding-right: 0 !important; }
        .dataTables_scrollHeadInner table { width: 100% !important; table-layout: fixed !important; }
        .dataTables_scrollBody table { table-layout: fixed !important; }
        .gsquery-title {
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
        table.dataTable tbody td { text-align: center !important; }
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
        .rank-plot-container {
          background: white;
          border-radius: 8px;
          padding: 15px;
          margin-top: 15px;
        }
        /* Modal styles from OneGene/Synergy */
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
        .detail-label { font-weight: 600; color: #555; }
        .detail-value { color: #333; }
        .detail-effect { padding: 8px 12px; border-radius: 6px; font-weight: 600; margin-bottom: 12px; display: inline-block; }
        .effect-suppressor { background: #e3f2fd; color: #1565c0; }
        .effect-activator { background: #fff3e0; color: #e65100; }
        .detail-btn-group { margin: 15px 0; }
      ")),
      tags$script(HTML(sprintf("
        $(document).on('click', '.le-toggle-btn', function(e) {
          e.stopPropagation();
          var $wrap = $(this).closest('.le-wrap');
          var $short = $wrap.find('.le-short');
          var $full = $wrap.find('.le-full');
          $short.toggle();
          $full.toggle();
          $(this).toggleClass('fa-caret-down fa-caret-up');
        });
      ")))
    ),

    # Hidden input for dataset click
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
            tags$div(class = "gsquery-box",
              tags$h4(icon("dna"), " Input Gene Set", class = "gsquery-title"),
              tags$hr(),
              p(HTML("<b>GSQuery</b> will find which perturbations in the database modulate your gene set of interest.")),
              p("Upload a single-column file or paste a gene list below:"),
              
              fluidRow(
                column(6,
                  fileInput(ns("gene_file"), "Upload Gene File (one gene per line)",
                    accept = c(".csv", ".txt", ".tsv"),
                    buttonLabel = "Browse...",
                    placeholder = "No file selected")
                ),
                column(6,
                  textAreaInput(ns("gene_list"), NULL,
                    placeholder = "Paste genes here (one per line)\nExample:\nTP53\nEGFR\nMYC\nBRCA1",
                    rows = 10)
                )
              ),
              
              fluidRow(
                column(4,
                  numericInput(ns("top_n"), "Top N Results",
                    value = 500, min = 10, max = 5000, step = 10)
                ),
                column(4,
                  radioButtons(ns("mode"), "Direction",
                    choices = c("Both" = "both", "Activate" = "activate", "Suppress" = "suppress"),
                    selected = "both", inline = TRUE)
                )
              )
            )
          )
        ),
        
        # ===== Step 2: Run Button =====
        fluidRow(
          column(12,
            actionButton(ns("run_gsquery"),
              label = "Run GSQuery",
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
                icon("hourglass-half"), " Analysis in progress... Due to the large dataset size, this process may take approximately 3 minutes. Please do not close this page or navigate away."),
              
              div(id = ns("progress_text"), class = "progress-info",
                "Initializing... Please wait.")
            )
          )
        ),
        
        # ===== Step 3: Results =====
        fluidRow(
          column(12,
            div(id = ns("results_container"),
              class = "result-section",
              style = "display: none;",
              
              tags$h4(icon("list"), " Results: Top Perturbations Affecting Your Gene Set"),
              
              fluidRow(
                column(3,
                  downloadButton(ns("download_results"), "Download CSV",
                    class = "btn-success", style = "margin-bottom: 10px;")
                ),
                column(12,
                  fluidRow(
                    column(6,
                      tags$h5("Chemical", style = "text-align:center;margin-bottom:3px;color:#3a7bd5;font-weight:bold;"),
                      shinycssloaders::withSpinner(
                        plotOutput(ns("results_rank_chem"), height = "400px"),
                        type = 4, color = "#3a7bd5"
                      )
                    ),
                    column(6,
                      tags$h5("Genetic", style = "text-align:center;margin-bottom:3px;color:#28a745;font-weight:bold;"),
                      shinycssloaders::withSpinner(
                        plotOutput(ns("results_rank_gen"), height = "400px"),
                        type = 4, color = "#28a745"
                      )
                    )
                  )
                ),
                column(12,
                  tags$div(style = "margin-top:10px;",
                    tabsetPanel(
                      tabPanel("Chemical",
                        shinycssloaders::withSpinner(
                          DT::DTOutput(ns("results_chemical")),
                          type = 4, color = "#3a7bd5"
                        )
                      ),
                      tabPanel("Genetic",
                        shinycssloaders::withSpinner(
                          DT::DTOutput(ns("results_genetic")),
                          type = 4, color = "#28a745"
                        )
                      )
                    )
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


GSQueryServer <- function(id, analysis_type_OV = NULL, help_id = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Register static file path for dataset CSV downloads (same as GQuery)
    if (!"dataset_csv" %in% names(shiny::resourcePaths())) {
      shiny::addResourcePath("dataset_csv", normalizePath("rdata_columns_csv"))
    }
    
    # ===== Constants =====
    GSEA_DIR <- "GSEA"
    
    cat("[GSQuery] Server initialized (Phase1: inverted index; Phase2: sequential MWU)\n")
    
    # ===== Reactive Values =====
    rv <- reactiveValues(
      results = NULL,
      query_genes = NULL,
      running = FALSE,
      progress_pct = 0,
      progress_file = NULL,
      polling_id = 0  # increment to invalidate polling
    )
    
    # Store modal plots for download
    modal_plot_store <- reactiveVal(NULL)
    gsea_table_store <- reactiveVal(NULL)
    gsea_plot_store  <- reactiveVal(NULL)
    
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
    
    # Helper: Get DEG data with caching (searches subdirs)
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
          # Search in Chemical/ and Genetic/ subdirectories
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
    
    # Helper: safe filename
    safe_filename <- function(x, max_len = 120) {
      x <- gsub("[^a-zA-Z0-9_\\-\\.]", "_", as.character(x))
      if (nchar(x) > max_len) x <- substr(x, 1, max_len)
      x
    }
    
    # Download combined plots PDF
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
    
    # GSEA stats table
    output$gsea_stats_table <- renderTable({
      req(gsea_table_store())
      gsea_table_store()
    }, striped = TRUE, bordered = TRUE, hover = TRUE, align = "c",
    width = "100%", rownames = FALSE)
    
    # GSEA plot download
    output$download_gsea_plot <- downloadHandler(
      filename = function() {
        st <- modal_plot_store()
        name <- if (!is.null(st$gem_id) && nzchar(as.character(st$gem_id))) as.character(st$gem_id) else "dataset"
        paste0(safe_filename(name), "_GSEA_plot.pdf")
      },
      content = function(file) {
        req(gsea_plot_store())
        ggplot2::ggsave(filename = file, plot = gsea_plot_store(),
                        width = 8, height = 6, device = "pdf")
      },
      contentType = "application/pdf"
    )
    
    # ===== Parse Gene List Input =====
    parse_gene_list <- function() {
      genes <- character(0)
      
      # From file upload
      if (!is.null(input$gene_file) && file.exists(input$gene_file$datapath)) {
        tryCatch({
          dt <- fread(input$gene_file$datapath, header = FALSE)
          genes <- toupper(as.character(dt[[1]]))
        }, error = function(e) {
          cat("[GSQuery] File read error:", e$message, "\n")
        })
      }
      
      # From text input
      if (length(genes) == 0 && nzchar(input$gene_list)) {
        lines <- strsplit(trimws(input$gene_list), "\n")[[1]]
        lines <- lines[nzchar(lines)]
        genes <- toupper(trimws(lines))
      }
      
      # Clean up
      genes <- genes[nzchar(genes)]
      genes <- unique(genes)
      
      if (length(genes) < 3) {
        return(NULL)
      }
      
      cat("[GSQuery] Parsed", length(genes), "unique genes\n")
      genes
    }
    
    # ===== Internal function to run analysis =====
    run_analysis <- function() {
      if (rv$running) {
        showNotification("Analysis already running...", type = "warning")
        return()
      }
      
      # Parse input genes
      query_genes <- parse_gene_list()
      if (is.null(query_genes) || length(query_genes) < 3) {
        showNotification("Please provide at least 3 genes", type = "error")
        return()
      }
      
      rv$query_genes <- query_genes
      rv$running <- TRUE
      rv$results <- NULL
      
      top_n_val <- input$top_n
      
      # Create progress file for cross-process communication
      pfile <- tempfile("gsquery_progress_", fileext = ".txt")
      rv$progress_file <- pfile
      rv$polling_id <- rv$polling_id + 1  # trigger polling observer
      
      # Show progress UI
      shinyjs::show("progress_container")
      shinyjs::hide("results_container")
      shinyjs::html("status_msg", 
        paste0(icon("hourglass-half"), " Analysis in progress... Due to the large dataset size, this process may take approximately 3 minutes. Please do not close this page or navigate away."))
      shinyjs::addClass("status_msg", "status-running")
      shinyjs::removeClass("status_msg", "status-done")
      shinyjs::html("progress_text", "Starting...")
      
      # Run in future_promise for non-blocking execution
      future_promise({
        run_gsquery_global(
          query_genes = query_genes,
          gsea_dir = GSEA_DIR,
          top_n = top_n_val,
          progress_file = pfile
        )
      }, seed = NULL) -> promise
      
      promise$then(function(results) {
        cat("[GSQuery] Analysis completed!\n")
        
        # Hide progress and clean up
        shinyjs::hide("progress_container")
        rv$polling_id <- rv$polling_id + 1  # stop polling
        if (!is.null(rv$progress_file) && file.exists(rv$progress_file)) {
          file.remove(rv$progress_file)
          rv$progress_file <- NULL
        }
        
        if (!is.null(results) && nrow(results) > 0) {
          # Merge GEM_ID from metadata (.fst) with type tracking
          meta_list <- list()
          for (meta_file in chem_meta_files) {
            if (!file.exists(meta_file)) next
            meta_dt <- tryCatch(
              read_fst(meta_file, as.data.table = TRUE),
              error = function(err) NULL
            )
            if (is.null(meta_dt)) next
            if (!"Dataset" %in% names(meta_dt) || !"GEM_ID" %in% names(meta_dt)) next
            meta_dt[, Type := "Chemical"]
            meta_list[[length(meta_list) + 1]] <- meta_dt
          }
          for (meta_file in gen_meta_files) {
            if (!file.exists(meta_file)) next
            meta_dt <- tryCatch(
              read_fst(meta_file, as.data.table = TRUE),
              error = function(err) NULL
            )
            if (is.null(meta_dt)) next
            if (!"Dataset" %in% names(meta_dt) || !"GEM_ID" %in% names(meta_dt)) next
            meta_dt[, Type := "Genetic"]
            meta_list[[length(meta_list) + 1]] <- meta_dt
          }
          if (length(meta_list) > 0) {
            meta_merged <- unique(rbindlist(meta_list, fill = TRUE), by = "Dataset")
            results <- merge(results, meta_merged, by = "Dataset", all.x = TRUE)
          }
          # Keep only core result columns; drop extra metadata columns
          keep_cols <- intersect(c("Dataset", "GEM_ID", "Type", "ES", "NES", "Overlap",
                                    "Overlap_Pct", "Pvalue", "Padj", "Leading_Edge", "N_Genes",
                                    "Perturbagen", "pbgene"), names(results))
          extra <- setdiff(names(results), keep_cols)
          if (length(extra) > 0) results[, (extra) := NULL]
          
          # Filter by activation/suppression direction
          mode_val <- isolate(input$mode)
          if (mode_val == "activate") {
            results <- results[ES > 0]
          } else if (mode_val == "suppress") {
            results <- results[ES < 0]
          }
          if (nrow(results) > 0) {
            results <- results[order(-abs(NES))]
            if (nrow(results) > 100000) results <- head(results, 100000)
          }
          
          rv$results <- results
          rv$running <- FALSE
          
          shinyjs::show("results_container")
          showNotification(
            paste("Found", nrow(results), "datasets affecting your gene set!"),
            type = "message",
            duration = 5
          )
        } else {
          rv$running <- FALSE
          showNotification(
            "No significant results found with current criteria",
            type = "warning",
            duration = 5
          )
        }
      })
      
      promise$catch(function(error) {
        cat("[GSQuery] Error:", conditionMessage(error), "\n")
        shinyjs::hide("progress_container")
        rv$polling_id <- rv$polling_id + 1  # stop polling
        if (!is.null(rv$progress_file) && file.exists(rv$progress_file)) {
          file.remove(rv$progress_file)
          rv$progress_file <- NULL
        }
        rv$running <- FALSE
        showNotification(
          paste("Analysis failed:", conditionMessage(error)),
          type = "error"
        )
      })
    }
    
    # ===== Run GSQuery =====
    observeEvent(input$run_gsquery, {
      run_analysis()
    })
    
    # ===== Progress Polling =====
    # Poll the progress file written by the worker process every 500ms
    observe({
      # Only poll while analysis is running; reactive dependency on rv$running auto start/stop
      req(rv$running)
      req(rv$progress_file)
      
      invalidateLater(500)
      
      pfile <- rv$progress_file
      if (file.exists(pfile)) {
        line <- readLines(pfile, warn = FALSE)
        if (length(line) > 0 && nzchar(line[1])) {
          parts <- strsplit(line[1], "|", fixed = TRUE)[[1]]
          msg <- if (length(parts) > 1) parts[2] else ""
          shinyjs::html("progress_text", msg)
        }
      }
    })
    
    # ===== Results Rank Plots (Chemical / Genetic, simple scatter) =====
    render_results_rank <- function(type_val, output_id) {
      output[[output_id]] <- renderPlot({
        req(rv$results)
        dt <- copy(rv$results)
        if ("Type" %in% names(dt)) dt <- dt[is.na(Type) | Type == type_val]
        if (nrow(dt) < 5) return(NULL)
        if ("GEM_ID" %in% names(dt)) dt[is.na(GEM_ID) | GEM_ID == "", GEM_ID := Dataset]
        dt[, plot_label := GEM_ID]
        if (type_val == "Chemical" && "Perturbagen" %in% names(dt))
          dt[!is.na(Perturbagen) & Perturbagen != "", plot_label := Perturbagen]
        if (type_val == "Genetic" && "pbgene" %in% names(dt))
          dt[!is.na(pbgene) & pbgene != "", plot_label := pbgene]
        dt <- dt[order(-ES)]
        dt[, rank := .I]
        dt <- head(dt, 100000)
        max_abs <- max(abs(dt$ES), na.rm = TRUE)
        y_lim <- max_abs * 1.15
        label_data <- head(dt[order(-abs(ES))], 10)
        label_data <- label_data[!duplicated(plot_label)]
        p <- ggplot() + theme_minimal(base_size = 11)
        p <- p + geom_hline(yintercept = 0, color = "#E74C3C", linetype = "dashed", linewidth = 0.4)
        p <- p + geom_point(data = dt, aes(x = rank, y = ES, color = ES, size = abs(ES)), alpha = 0.7)
        p <- p + scale_size_continuous(range = c(0.3, 3), guide = "none")
        p <- p + scale_color_gradient2(low = "#3498DB", mid = "gray85", high = "#E74C3C",
                                       midpoint = 0, limits = c(-max_abs, max_abs),
                                       name = "ES  (Blue=Suppressed, Red=Activated)")
        p <- p + guides(color = guide_colorbar(barwidth = 8, barheight = 0.4,
                                                title.position = "top", title.hjust = 0.5))
        if (nrow(label_data) > 0)
          p <- p + ggrepel::geom_text_repel(data = label_data,
            aes(x = rank, y = ES, label = plot_label), color = "grey20",
            size = 3, max.overlaps = 20, box.padding = 0.3, point.padding = 0.2,
            min.segment.length = 0.2, segment.color = "grey50", segment.size = 0.3,
            show.legend = FALSE, fontface = "bold", force = 1)
        p <- p + labs(x = "Rank", y = "ES") +
          coord_cartesian(ylim = c(-y_lim, y_lim)) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
                legend.position = "bottom", legend.title.align = 0.5)
        p
      })
    }
    render_results_rank("Chemical", "results_rank_chem")
    render_results_rank("Genetic", "results_rank_gen")
    
    # ===== Render Results Tables (split by Chemical/Genetic) =====
    render_type_table <- function(type_val) DT::renderDT({
      req(rv$results)
      dt <- copy(rv$results)
      
      # Filter by type
      dt <- dt[is.na(Type) | Type == type_val]
      dt <- head(dt, isolate(input$top_n))
      if (nrow(dt) == 0) {
        empty_dt <- data.table(Message = paste("No", tolower(type_val), "results found"))
        return(DT::datatable(empty_dt, rownames = FALSE,
          options = list(dom = 't', pageLength = 1, searching = FALSE, ordering = FALSE)))
      }
      
      numeric_cols <- intersect(names(dt), c("ES", "NES", "Overlap_Pct", "Pvalue", "Padj"))
      for (col in numeric_cols) {
        dt[, (col) := signif(get(col), 5)]
      }
      
      # Truncate Leading_Edge with click-to-expand toggle
      if ("Leading_Edge" %in% names(dt)) {
        dt[, Leading_Edge := {
          genes <- strsplit(Leading_Edge, ",")
          sapply(genes, function(g) {
            n <- length(g)
            short <- paste(head(g, 3), collapse = ", ")
            if (n > 3) short <- paste0(short, " +", n - 3, " more")
            full <- paste(g, collapse = ", ")
            sprintf(
              '<span class="le-wrap"><span class="le-short">%s</span><span class="le-full" style="display:none">%s</span> <i class="fa fa-caret-down le-toggle-btn" style="cursor:pointer;color:#3a7bd5"></i></span>',
              short, full
            )
          })
        }]
      }
      
      # Prepare GEM_ID for download filename (fill NA, save raw before HTML transform)
      if ("GEM_ID" %in% names(dt) && "Dataset" %in% names(dt)) {
        missing <- is.na(dt$GEM_ID) | dt$GEM_ID == ""
        dt$GEM_ID[missing] <- dt$Dataset[missing]
        dt[, .gem_id_raw := GEM_ID]
      } else {
        dt[, .gem_id_raw := Dataset]
      }
      
      # Add Download button column
      if ("Dataset" %in% names(dt)) {
        dt[, Download := sprintf(
          '<button class="btn btn-sm btn-success" onclick="Shiny.setInputValue(\'%s\',{ds:\'%s\',gem:\'%s\',ts:Date.now()},{priority:\'event\'})"><i class="fa fa-download"></i> Download</button>',
          ns("download_single_ds"),
          gsub("'", "\\'", gsub('"', '&quot;', as.character(Dataset), fixed = TRUE), fixed = TRUE),
          gsub("'", "\\'", gsub('"', '&quot;', as.character(.gem_id_raw), fixed = TRUE), fixed = TRUE)
        )]
      }
      
      # Make GEM_ID column clickable
      if ("GEM_ID" %in% names(dt) && "Dataset" %in% names(dt)) {
        dt$GEM_ID <- sprintf(
          '<span class="dataset-link" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority: \'event\'})">%s</span>',
          ns("click_dataset"),
          dt$Dataset,
          dt$GEM_ID
        )
        setcolorder(dt, c("GEM_ID", setdiff(names(dt), "GEM_ID")))
      }
      
      # Hidden column: absolute NES for sorting
      dt[, abs_NES := abs(NES)]
      
      dataset_col_idx <- match("Dataset", names(dt)) - 1
      gem_id_col_idx <- match(".gem_id_raw", names(dt)) - 1
      
      hidden_targets <- c(
        which(names(dt) == "Dataset") - 1,
        which(names(dt) == ".gem_id_raw") - 1,
        which(names(dt) == "abs_NES") - 1
      )
      
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
          order = list(list(which(names(dt) == "abs_NES") - 1, "desc")),
          columnDefs = list(list(visible = FALSE, targets = hidden_targets))
        ),
        callback = DT::JS(sprintf(
          "new MutationObserver(function(m){m.forEach(function(r){$(r.addedNodes).filter('.selected,.selected td').css({boxShadow:'inset 0 0 0 9999px rgba(208,208,208,0.9)',color:'#333'});});}).observe(document.querySelector('#'+table.table().node().id+' tbody'),{childList:true,subtree:true});
          var $bodyTable = $(this).find('.dataTables_scrollBody table');
          var $headInner = $(this).find('.dataTables_scrollHeadInner');
          if ($bodyTable.length && $headInner.length) {
            $headInner.css('width', $bodyTable.outerWidth());
          }
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
    
    # ===== Download Handler =====
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("gsquery_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv$results)
        dt <- copy(rv$results)
        # Fill missing GEM_ID from Dataset first
        if ("GEM_ID" %in% names(dt) && "Dataset" %in% names(dt)) {
          dt[is.na(GEM_ID) | GEM_ID == "", GEM_ID := Dataset]
        }
        # Ensure GEM_ID exists (rename Dataset if needed)
        if (!"GEM_ID" %in% names(dt) && "Dataset" %in% names(dt)) {
          setnames(dt, "Dataset", "GEM_ID")
        }
        # ---- UNCONDITIONALLY strip Dataset and all internal columns ----
        drop <- intersect(c("Dataset", ".gem_id_raw", ".source_meta_file", ".source_data_file"), names(dt))
        if (length(drop) > 0) dt[, (drop) := NULL]
        # Move GEM_ID to first column
        if ("GEM_ID" %in% names(dt)) {
          setcolorder(dt, c("GEM_ID", setdiff(names(dt), "GEM_ID")))
        }
        fwrite(dt, file)
      }
    )
    
    # ===== Dataset Click Modal =====
    observeEvent(input$click_dataset, {
      ds <- input$click_dataset
      if (is.null(ds) || !nzchar(ds)) return(NULL)
      
      dt <- isolate(rv$results)
      if (is.null(dt) || nrow(dt) == 0) return(NULL)
      
      # Find the row
      row <- dt[grep(ds, Dataset, fixed = TRUE)]
      if (is.null(row) || nrow(row) == 0) return(NULL)
      row <- row[1]
      
      # Get GSEA results info
      nes_val <- as.numeric(row$NES)
      es_val <- as.numeric(row$ES)
      pval_val <- as.numeric(row$Pvalue)
      padj_val <- as.numeric(row$Padj)
      overlap_val <- as.integer(row$Overlap)
      n_genes <- as.integer(row$N_Genes)
      leading_edge <- as.character(row$Leading_Edge)
      
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
      
      # Merge metadata to row
      if (length(meta_list) > 0) {
        meta_merged <- rbindlist(meta_list, fill = TRUE, use.names = TRUE)
        meta_for_merge <- meta_merged[, c("Dataset", setdiff(names(meta_merged), "Dataset")), with = FALSE]
        row <- merge(data.table(Dataset = ds), meta_for_merge, by = "Dataset", all.x = TRUE)
        if (nrow(row) == 0) row <- as.list(dt[1])
      }
      
      # Detect type (chemical or genetic)
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
      
      link_html <- NULL; mol_img <- NULL
      
      # 2D/3D chemical structure
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
      
      # Genetic STRING network
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
      
      # Build items from row metadata
      skip_cols <- c("dataset", "Dataset", "download_btn", "source_id")
      items <- list()
      for (col in names(row)) {
        if (col %in% skip_cols) next
        val <- row[[col]]
        if (is.null(val) || length(val) == 0 || identical(as.character(val), "") || identical(as.character(val), "NA") || (length(val) == 1 && is.na(val))) next
        if (is.numeric(val) && !is.infinite(val)) val <- round(as.numeric(val), 4)
        if (is.numeric(val) && is.infinite(val)) val <- ifelse(val > 0, "Inf", "-Inf")
        items[[length(items)+1]] <- div(class="detail-item", span(class="detail-label", paste0(col, ": ")), span(class="detail-value", as.character(val)))
      }
      
      # Get DEG data and create plots (same as Synergy module)
      deg_dt <- get_deg_dt_cached(ds, GSEA_DIR)
      
      plot_lollipop <- NULL; plot_density <- NULL; plot_main_1 <- NULL
      plot_main_2 <- NULL; plot_count_bar <- NULL; plot_cdf <- NULL; plot_manhattan <- NULL
      valid_data <- FALSE; deg <- NULL; has_pval <- FALSE; has_fdr <- FALSE
      main_plot_title <- ""
      
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
        
        # Count bar chart
        count_stats <- deg[, .(gene_count = .N), by = group]
        count_stats$group <- factor(count_stats$group, levels = c("Down", "NS", "Up"))
        plot_count_bar <- ggplot(count_stats, aes(x=group, y=gene_count, fill=group)) +
          geom_col(width=0.7, show.legend = FALSE) + scale_fill_manual(values=group_colors) +
          geom_text(aes(label=gene_count), vjust=-0.3, fontface="bold") +
          labs(title="Differential Gene Count", x="", y="Gene Number") + theme_bw()
        
        # CDF curve
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
          ggrepel::geom_text_repel(
            data = top10_cdf,
            aes(x = logFC, y = cdf, label = gene),
            size = 3, max.overlaps = 15, box.padding = 0.3, color = "black"
          ) +
          theme_bw() + theme(legend.position="bottom", plot.title=element_text(face="bold", size=13))
        
        if (has_pval && has_fdr) {
          main_plot_title <- "Volcano & Manhattan Plot"
          deg_plot <- deg[!is.na(pval)]
          if (nrow(deg_plot) == 0) deg_plot <- deg
          deg_plot$pval_capped <- pmax(deg_plot$pval, 1e-300)

          top10_volcano <- merge(copy(top10_genes)[, .(gene, logFC)], deg_plot[, .(gene, pval_capped, group)], by = "gene", all.x = TRUE)
          top10_volcano <- top10_volcano[!is.na(pval_capped)]

          plot_main_1 <- ggplot(deg_plot, aes(x=logFC, y=-log10(pval_capped), color=group, text=gene)) +
            geom_point(size=0.8, alpha=0.7) + scale_color_manual(values=group_colors) +
            geom_vline(xintercept=c(-1,1), linetype="dashed", linewidth=0.3) +
            geom_hline(yintercept=-log10(0.05), linetype="dashed", linewidth=0.3) +
            labs(title="Standard Volcano Plot", x="log2(Fold Change)", y="-log10(P-value)") +
            ggrepel::geom_text_repel(
              data = top10_volcano,
              aes(x = logFC, y = -log10(pval_capped), label = gene),
              size = 3, max.overlaps = 15, box.padding = 0.3
            ) +
            theme_bw() + theme(legend.position="bottom")

          # Manhattan plot
          deg_manhattan <- deg[!is.na(pval)]
          if (nrow(deg_manhattan) > 0) {
            deg_manhattan$rank <- 1:nrow(deg_manhattan)
            deg_manhattan$pval_capped <- pmax(deg_manhattan$pval, 1e-300)
            top10_manhattan <- merge(copy(top10_genes)[, .(gene, logFC)], deg_manhattan[, .(gene, rank, pval_capped, group)], by = "gene", all.x = TRUE)
            top10_manhattan <- top10_manhattan[!is.na(pval_capped)]

            plot_manhattan <- ggplot(deg_manhattan, aes(x=rank, y=-log10(pval_capped), color=group, text=gene)) +
              geom_point(size=0.6, alpha=0.6) + scale_color_manual(values=group_colors) +
              geom_hline(yintercept=-log10(0.05), linetype="dashed", linewidth=0.3, color="red") +
              labs(title="Manhattan Plot", x="Gene Rank", y="-log10(P-value)") +
              ggrepel::geom_text_repel(
                data = top10_manhattan,
                aes(x = rank, y = -log10(pval_capped), label = gene),
                size = 3, max.overlaps = 15, box.padding = 0.3
              ) +
              theme_bw() + theme(legend.position="bottom")
          }
        } else {
          main_plot_title <- "logFC Signature Pattern"
          deg_sorted2 <- deg[order(logFC)]
          deg_sorted2$rank <- 1:nrow(deg_sorted2)
          
          top10_rank <- merge(copy(top10_genes)[, .(gene, logFC)], deg_sorted2[, .(gene, rank, group)], by = "gene", all.x = TRUE)
          top10_rank <- top10_rank[!is.na(rank)]
          
          plot_main_1 <- ggplot(deg_sorted2, aes(x=rank, y=logFC, color=group, text=gene)) +
            geom_point(size=0.8, alpha=0.6) + scale_color_manual(values=group_colors) +
            geom_hline(yintercept=c(-1,0,1), linetype="dashed", linewidth=0.3) +
            labs(title="Gene Ranked by logFC", x="Gene Rank", y="logFC") +
            ggrepel::geom_text_repel(
              data = top10_rank,
              aes(x = rank, y = logFC, label = gene),
              size = 3, max.overlaps = 15, box.padding = 0.3
            ) +
            theme_bw() + theme(legend.position="bottom")
          
          deg$bin <- cut(deg$logFC, breaks=seq(-10,10,1), include.lowest=TRUE)
          bin_stats <- deg[, .(count=.N), by=.(bin, group)]
          plot_main_2 <- ggplot(bin_stats, aes(x=bin, y=count, fill=group)) +
            geom_col(position="stack", width=0.8) + scale_fill_manual(values=group_colors) +
            geom_vline(xintercept=which(levels(bin_stats$bin)=="(-0.5,0]"), linetype="dashed", linewidth=0.3) +
            labs(title="logFC Distribution", x="logFC Bins", y="Gene Count") +
            theme_bw() + theme(legend.position="bottom", axis.text.x=element_text(angle=45, hjust=1, size=8))
        }
        
        # Lollipop plot
        top_up <- head(deg[logFC>0][order(-logFC)], 20)
        top_down <- head(deg[logFC<0][order(logFC)], 20)
        top_genes_lolli <- rbindlist(list(top_up, top_down), fill=TRUE)
        if (nrow(top_genes_lolli) > 0) {
          top_genes_lolli$direction <- ifelse(top_genes_lolli$logFC > 0, "Up", "Down")
          top_genes_lolli$gene <- as.character(top_genes_lolli$gene)
          top_genes_lolli <- top_genes_lolli[!is.na(gene) & nzchar(gene)]
          if (nrow(top_genes_lolli) > 0) {
            plot_lollipop <- ggplot(top_genes_lolli, aes(x=reorder(gene, logFC), y=logFC, fill=direction, text=gene)) +
              geom_col(width=0.2) +
              geom_point(size=2, shape=21) +
              coord_flip() +
              labs(title="Top 20 Genes", x="", y="logFC", fill="Direction") +
              scale_fill_manual(values=c("Up"="#EA4335","Down"="#4285F4")) +
              theme_bw() +
              theme(axis.text.y=element_text(size=8), legend.position="bottom")
          }
        }
        
        plot_density <- ggplot(deg, aes(x=logFC)) +
          geom_density(fill="#76A7FA", alpha=0.4) + geom_vline(xintercept=0, linetype="dashed") +
          labs(title="Density", x="logFC", y="Density") + theme_bw()
      }
      
      # GSEA enrichment data (for GSQuery: perturbation DEG vs. user's query gene set)
      # ALL values (ES, NES, P, Leading Edge) are freshly computed by standard fgsea algorithm
      gsea_data <- NULL
      if (valid_data) {
        query_genes <- isolate(rv$query_genes)
        if (!is.null(query_genes) && length(query_genes) > 0) {
          ranked_stats <- deg$logFC
          names(ranked_stats) <- toupper(deg$gene)
          ranked_stats <- sort(ranked_stats, decreasing = TRUE)
          ranked_stats <- ranked_stats[!duplicated(names(ranked_stats))]
          matched_pathway <- intersect(names(ranked_stats), toupper(query_genes))
          if (length(matched_pathway) >= 3) {
            # ---- Run standard GSEA via fgsea (permutation test) ----
            fgsea_res <- tryCatch({
              fgsea::fgseaMultilevel(
                pathways = list(query = matched_pathway),
                stats    = ranked_stats,
                nPermSimple = 10000,
                eps      = 0
              )
            }, error = function(e) NULL)

            if (!is.null(fgsea_res) && nrow(fgsea_res) > 0) {
              # ---- Extract ES/NES/P/LeadingEdge from fgsea result ----
              es_val  <- fgsea_res$ES[1]
              nes_val <- fgsea_res$NES[1]
              pval_val <- fgsea_res$pval[1]
              padj_val <- fgsea_res$padj[1]
              leading_edge_genes <- fgsea_res$leadingEdge[[1]]

              # ---- Compute running score & hit for the plot ----
              N <- length(ranked_stats)
              Nh <- length(matched_pathway)
              hit <- names(ranked_stats) %in% matched_pathway
              abs_rank <- abs(ranked_stats)
              hit_weight <- ifelse(hit, abs_rank / sum(abs_rank[hit]), 0)
              miss_weight <- ifelse(hit, 0, 1 / (N - Nh))
              running_score <- cumsum(hit_weight - miss_weight)
              es_idx <- which.max(running_score)

              gsea_data <- list(
                matched_pathway   = matched_pathway,
                ranked_stats      = ranked_stats,
                running_score     = running_score,
                hit               = hit,
                es                = es_val,
                es_idx            = es_idx,
                leading_edge_genes = leading_edge_genes,
                n_matched         = Nh,
                n_query           = length(query_genes),
                nes               = nes_val,
                pval              = pval_val
              )
            }
          }
        }
      }
      
      # Combine plots for display and PDF download (6 plots only, GSEA is separate)
      modal_ggplots <- list()
      combined_plot <- NULL
      
      if (valid_data) {
        modal_ggplots <- list(count_bar=plot_count_bar, cdf=plot_cdf, main_1=plot_main_1,
                             manhattan=plot_manhattan, main_2=plot_main_2, density=plot_density, lollipop=plot_lollipop)
        modal_ggplots <- Filter(function(x) !is.null(x) && inherits(x, "ggplot"), modal_ggplots)
        
        # Build combined plot
        if (length(modal_ggplots) > 0) {
          theme_small <- theme_bw() + theme(
            plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8),
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 8),
            legend.position = "bottom",
            plot.margin = margin(8, 8, 8, 8),
            legend.margin = margin(5, 5, 5, 5)
          )
          
          modal_ggplots <- lapply(modal_ggplots, function(p) p + theme_small)
          
          n_plots <- length(modal_ggplots)
          combined_plot <- switch(as.character(n_plots),
            "1" = modal_ggplots[[1]],
            "2" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], ncol = 2),
            "3" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], modal_ggplots[[3]], ncol = 3),
            "4" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], modal_ggplots[[3]], modal_ggplots[[4]], ncol = 2),
            "5" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], modal_ggplots[[3]], modal_ggplots[[4]], modal_ggplots[[5]], ncol = 3),
            wrap_plots(modal_ggplots[[1]],modal_ggplots[[2]],modal_ggplots[[3]],modal_ggplots[[4]],modal_ggplots[[5]],modal_ggplots[[6]],
              design=c(area(1,1),area(1,2),area(1,3),area(2,1,2,2),area(3,1,3,2),area(2,3,3,3)))
          )
        }
        
        modal_plot_store(list(
          dataset=as.character(ds),
          gem_id=if (!is.null(row$GEM_ID) && nzchar(as.character(row$GEM_ID))) as.character(row$GEM_ID) else as.character(ds),
          ggplots=modal_ggplots,
          combined_plot=combined_plot,
          gsea_data=gsea_data
        ))
      } else {
        modal_plot_store(NULL)
      }
      
      # Effect tag based on ES (match OneGene/Synergy style)
      effect_class <- ifelse(!is.na(es_val) && es_val > 0, "effect-activator", "effect-suppressor")
      effect_tag <- div(class=paste("detail-effect", effect_class),
                        icon("chart-line"),
                        paste("NES:", round(nes_val, 4), "| ES:", round(es_val, 4), "| P:", format.pval(pval_val, 3)))
      
      # Show modal
      showModal(modalDialog(
        tags$div(
          id = ns("dataset_detail_modal"),
          tags$script(HTML(sprintf("
            setTimeout(function() {
              var el = document.getElementById('%s');
              if (el) { $(el).closest('.modal-dialog').addClass('dataset-wide-modal'); }
            }, 50);
          ", ns("dataset_detail_modal")))),
          
          div(class="detail-card",
            
            effect_tag, br(), link_html, br(), mol_img,
            items,
            
            if(valid_data) {
              tagList(
                br(), hr(),
                
                tags$div(
                  style = "margin: 10px 0 16px 0; text-align: center;",
                  downloadButton(ns("download_combined_pdf"), "Download Plot", class = "btn btn-default btn-lg")
                ),
                
                withSpinner(plotOutput(ns("modal_combined"), height="650px"), type=4, color="#3a7bd5"),
                
                if (!is.null(gsea_data)) {
                  tagList(
                    tags$hr(style = "border-color: #1a5276; margin: 24px 0 12px 0;"),
                    tags$h4(style = "text-align: center; color: #1a5276; margin-bottom: 16px;",
                            icon("chart-line"), " GSEA Analysis: Query Gene Set vs. Perturbation DEG"),
                    
                    # GSEA stats table
                    div(style = "overflow-x: auto; margin-bottom: 12px;",
                      tableOutput(ns("gsea_stats_table"))
                    ),
                    
                    tags$div(
                      style = "margin: 8px 0 12px 0; text-align: center;",
                      downloadButton(ns("download_gsea_plot"), "Download GSEA Plot", class = "btn btn-default btn-sm")
                    ),
                    
                    withSpinner(plotOutput(ns("modal_gsea"), height="520px"), type=4, color="#228B22")
                  )
                }
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
        
        output$modal_gsea <- renderPlot({
          st <- modal_plot_store()
          req(st$gsea_data)
          gd <- st$gsea_data

          N <- length(gd$ranked_stats)
          rs <- gd$running_score
          es_idx <- gd$es_idx
          es_val <- gd$es
          le_genes <- gd$leading_edge_genes

          # ---- GSEA table data ----
          gsea_df <- data.frame(
            row.names = NULL,
            ES  = round(es_val, 4),
            NES = round(gd$nes, 4),
            P   = format.pval(gd$pval, 3),
            Matched = paste0(gd$n_matched, "/", gd$n_query),
            Leading_Edge_Genes = paste(le_genes, collapse = ", ")
          )
          gsea_table_store(gsea_df)

          # ---- Data for 3 panels ----
          plot_data <- data.frame(
            rank = seq_len(N),
            rs   = rs,
            lfc  = gd$ranked_stats,
            hit  = gd$hit,
            stringsAsFactors = FALSE
          )

          # ---- Panel 1: Running Enrichment Score ----
          yr <- range(rs, na.rm = TRUE)
          y_pad <- diff(yr) * 0.12
          ylim1 <- c(yr[1] - y_pad, yr[2] + y_pad)

          p1 <- ggplot(plot_data, aes(x = rank, y = rs)) +
            geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
            geom_ribbon(aes(ymin = 0, ymax = pmax(rs, 0)),
                        fill = "#E74C3C", alpha = 0.08) +
            geom_ribbon(aes(ymin = pmin(rs, 0), ymax = 0),
                        fill = "#3498DB", alpha = 0.08) +
            geom_line(color = "#27AE60", linewidth = 0.9) +
            geom_vline(xintercept = es_idx, color = "#C0392B",
                       linetype = "dashed", linewidth = 0.5) +
            annotate("point", x = es_idx, y = es_val,
                     color = "#C0392B", size = 3, shape = 18) +
            coord_cartesian(ylim = ylim1) +
            labs(x = NULL, y = "Running Enrichment Score",
                 title = paste0(gd$n_matched, "/", gd$n_query, " query genes matched"),
                 subtitle = paste0("NES = ", round(gd$nes, 4),
                                   "  |  P = ", format.pval(gd$pval, 3),
                                   "  |  ES = ", round(es_val, 3))) +
            theme_minimal(base_size = 11) +
            theme(
              plot.title       = element_text(face = "bold", size = 13, color = "#1a5276"),
              plot.subtitle    = element_text(size = 10, color = "gray40"),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.ticks.x     = element_blank(),
              axis.text.x      = element_blank()
            )

          # ---- Panel 2: Barcode ----
          hit_pos <- which(gd$hit)
          bc_df <- data.frame(x = hit_pos, y = 0, stringsAsFactors = FALSE)

          p2 <- ggplot(bc_df, aes(x = x, xend = x, y = 0, yend = 1)) +
            geom_segment(color = ifelse(gd$ranked_stats[hit_pos] > 0,
                                        "#E74C3C80", "#3498DB80"),
                         linewidth = 0.4) +
            scale_y_continuous(expand = c(0, 0)) +
            labs(x = NULL, y = NULL) +
            theme_void() +
            theme(
              plot.margin = margin(2, 10, 0, 10),
              panel.background = element_rect(fill = "white", color = NA)
            )

          # ---- Panel 3: Ranked list metric (logFC gradient) ----
          bin_n <- 200
          bin_w <- max(1, floor(N / bin_n))
          bin_idx <- seq(1, N, by = bin_w)
          bin_end <- c(bin_idx[-1] - 1, N)
          bin_mean <- mapply(function(s, e) mean(gd$ranked_stats[s:e]),
                             bin_idx, bin_end)
          bin_mid <- (bin_idx + bin_end) / 2
          lfc_df <- data.frame(
            x   = bin_mid,
            lfc = bin_mean,
            stringsAsFactors = FALSE
          )
          lfc_df$pos <- lfc_df$lfc > 0
          lfc_y <- range(lfc_df$lfc, na.rm = TRUE)

          p3 <- ggplot(lfc_df, aes(x = x, y = 0, fill = lfc)) +
            geom_tile(aes(width = bin_w, height = 0.8)) +
            scale_fill_gradient2(low = "#3498DB", mid = "gray90", high = "#E74C3C",
                                 midpoint = 0, name = "logFC") +
            labs(x = "Gene Rank (logFC: high → low)", y = NULL) +
            theme_minimal(base_size = 9) +
            theme(
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.text.y      = element_blank(),
              axis.ticks.y     = element_blank(),
              legend.position  = "bottom",
              legend.key.width = unit(1.5, "cm"),
              legend.key.height = unit(0.3, "cm"),
              legend.title     = element_text(size = 8),
              legend.text      = element_text(size = 7),
              legend.margin    = margin(0, 0, 0, 0)
            )

          # ---- Combine 3 panels ----
          combined_gsea <- wrap_plots(p1, p2, p3, ncol = 1,
                                       heights = c(4, 0.8, 1.5))
          gsea_plot_store(combined_gsea)
          print(combined_gsea)
        }, res = 120)
      }
    })

    # ================== 单数据集动态下载（Blob方式，绕过downloadLink程序化点击失效） ==================
    observeEvent(input$download_single_ds, {
      req(input$download_single_ds)
      ds  <- input$download_single_ds$ds
      gem <- input$download_single_ds$gem

      dt <- resolve_dataset_data(ds)
      content <- if (!is.null(dt) && nrow(dt) > 0) {
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

# ===== Alias for backwards compatibility =====
GenelistUI <- GSQueryUI
GenelistServer <- GSQueryServer
