# =========================
# GQueryUI - UI Module
# =========================
GQueryUI <- function(id, title = "Gene Query") {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .genequery-intro-box {
          background-color: #f0f6ff;
          border-radius: 12px;
          padding: 20px;
          margin-bottom: 10px;
        }
        .genequery-intro-title {
          color: #2c5f8a;
          font-weight: 600;
          margin-bottom: 15px;
        }
        /* 现有样式保持 */
        .dt-bottom-left { float: left; margin-top: 10px; }
        .dt-bottom-right { float: right; margin-top: 10px; }
        table.dataTable tbody tr { cursor: pointer; transition: all 0.2s ease; }
        table.dataTable tbody tr:hover { background-color: #f7faff !important; transform: scale(1.01); }
        table.dataTable thead th { background-color: #3a7bd5 !important; color: #fff !important; font-weight: 600; padding: 10px 8px !important; text-align: center !important; }
        table.dataTable tbody td { vertical-align: middle !important; padding: 8px !important; border-bottom: 1px solid #eee !important; text-align: center !important; max-width: 200px; white-space: normal; word-break: break-word; overflow-wrap: break-word; }
        .dataTables_wrapper .dataTables_filter input, .dataTables_wrapper .dataTables_length select { border-radius: 6px; border: 1px solid #d0d7de; padding: 4px 8px; }
        .dt-buttons .dt-button { padding: 5px 10px; border-radius: 6px; border: 1px solid #3a7bd5 !important; background: #fff !important; color: #3a7bd5 !important; font-weight: 500; }
        .dt-buttons .dt-button:hover { background: #eaf3ff !important; }
        .btn-search { margin-top: 0; border-radius: 8px; font-weight: 600; padding: 6px 14px; }
        .btn-demo-og { margin-top: 0; border-radius: 8px; font-weight: 600; padding: 6px 14px; }
        .btn-row-wrapper { display: flex; gap: 8px; align-items: flex-end; margin-top: 25px; }
        .dataset-link { color: #3a7bd5; font-weight: 500; text-decoration: underline; cursor: pointer; transition: color 0.2s; }
        .dataset-link:hover { color: #2d6acd; }
        /* Fix DT scrollX header-body misalignment */
        .dataTables_scrollHeadInner { width: 100% !important; }
        .dataTables_scrollHeadInner table { width: 100% !important; }
        /* dataset 详情弹窗：加宽 */
.dataset-wide-modal {
  width: 96vw !important;
  max-width: 1400px !important;
  margin: 20px auto !important;
}

table.dataTable tbody tr.selected > *,
table.dataTable.stripe tbody tr.odd.selected > *,
table.dataTable.display tbody tr.odd.selected > *,
table.dataTable.hover tbody tr.selected:hover > *,
table.dataTable.display tbody tr.selected:hover > * {
  box-shadow: inset 0 0 0 9999px rgba(208, 208, 208, 0.9) !important;
  color: #333 !important;
}
.dataset-wide-modal .modal-content {
  border-radius: 14px;
  overflow: hidden;
}

.dataset-wide-modal .modal-body {
  max-height: 88vh;
  overflow-y: auto;
  padding: 18px 22px;
}

@media (max-width: 768px) {
  .dataset-wide-modal {
    width: 98vw !important;
    max-width: 98vw !important;
    margin: 10px auto !important;
  }
}
        .detail-card { background: #f8f9fa; border-radius: 12px; padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); }
        .detail-title { color: #2c3e50; font-weight: 700; font-size: 18px; margin-bottom: 15px; border-left: 4px solid #3a7bd5; padding-left: 10px; }
        .detail-item { background: white; border-radius: 8px; padding: 10px 15px; margin-bottom: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); }
        .detail-label { font-weight: 600; color: #3a7bd5; min-width: 120px; display: inline-block; }
        .detail-value { color: #2c3e50; }
        .detail-effect { font-size: 16px; font-weight: bold; padding: 8px 12px; border-radius: 6px; display: inline-block; }
        .effect-suppressor { background: #e3f2fd; color: #0d6efd; }
        .effect-activator { background: #fff3cd; color: #fd7e14; }
        .section-title { font-size: 17px; font-weight: 700; margin-bottom: 10px; padding: 6px 10px; border-radius: 6px; display: inline-block; }
        .suppressor-title { color: #0d6efd; background-color: #e3f2fd; border-left: 4px solid #0d6efd; }
        .activator-title { color: #fd7e14; background-color: #fff3cd; border-left: 4px solid #fd7e14; }
        /* 弹窗里的结构图 / 网络图：固定展示区域，图片按比例显示，不拉伸 */
.modal-image-row {
  margin-top: 10px;
  margin-bottom: 14px;
}

.modal-image-card {
  background: #ffffff;
  border-radius: 12px;
  padding: 12px;
  border: 1px solid #e9eef5;
  box-shadow: 0 2px 6px rgba(0,0,0,0.04);
}

.modal-img-box {
  height: 240px;
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
}

.modal-img-box-large {
  height: 340px;
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
}

.modal-structure-img {
  max-width: 100%;
  max-height: 100%;
  width: auto !important;
  height: auto !important;
  object-fit: contain;
  display: block;
  border-radius: 8px;
}

.modal-img-caption {
  text-align: center;
  font-size: 12px;
  color: #555;
  margin-top: 8px;
}
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
    
    fluidRow(
      box(
        title = title,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 12,
        
        tags$div(class = "genequery-intro-box",
          tags$h4(icon("fingerprint"), " Gene Perturbator Query", class = "genequery-intro-title"),
          tags$hr(),
          p(HTML("<b>Gene Query</b> identifies perturbations that modulate the expression of a given gene of interest. Users can select a gene and optionally filter by perturbation type and |logFC| threshold, then click Search to view the top suppressors and activators.")),
          p("Results are separated into genetic (KD/KO/OE/Mut) and chemical (drug/compound) perturbations, ranked by |logFC|. This module helps prioritize candidate upstream regulators and potential chemical or genetic modulators of the selected gene.", style = "margin-bottom: 0;")
        ),
        br(),
        
        fluidRow(
          column(3,
                 shinyWidgets::virtualSelectInput(
                   inputId = ns("gene"),
                   label = "Gene",
                   choices = character(0),
                   multiple = F,
                   search = TRUE
                 )),
          column(3,
                 shinyWidgets::virtualSelectInput(
                   inputId = ns("ptype"),
                   label = "Perturbagen type",
                   choices = c("All" = "all", "Chemical only" = "chemical", "Genetic only" = "genetic"),
                   multiple = FALSE,
                   search = TRUE
                 )),
          column(3,
                 shinyWidgets::virtualSelectInput(
                   inputId = ns("logfc_filter"),
                   label = "|logFC| cutoff",
                   choices = character(0),
                   multiple = FALSE,
                   search = TRUE,
                   allowNewOption = TRUE
                 )),
          column(3,
                 div(class = "btn-row-wrapper",
                   actionButton(
                     ns("search"),
                     label = "Search",
                     icon = icon("magnifying-glass"),
                     class = 'btn-search'
                   ),
                   actionButton(
                     ns("demo_btn"),
                     label = "Demo",
                     icon = icon("play-circle"),
                     class = "btn-warning btn-demo-og"
                   )
                 ))
        ),
        
        br(),
        
        fluidRow(
          column(
            12,
            tags$div(class = "section-title suppressor-title",
                     icon("arrow-down"), " Top suppressors"),
            br(),
            tags$h5(icon("dna"), " Genetic", style = "color: #3a7bd5; font-weight: 600; margin-left: 10px;"),
            uiOutput(ns("download_suppressors_gen_csv_btn")),
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("suppressors_gen")),
              type = 4, color = "#3a7bd5"
            ),
            br(),
            tags$h5(icon("flask"), " Chemical", style = "color: #3a7bd5; font-weight: 600; margin-left: 10px;"),
            uiOutput(ns("download_suppressors_chem_csv_btn")),
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("suppressors_chem")),
              type = 4, color = "#3a7bd5"
            )
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            12,
            tags$div(class = "section-title activator-title",
                     icon("arrow-up"), " Top activators"),
            br(),
            tags$h5(icon("dna"), " Genetic", style = "color: #fd7e14; font-weight: 600; margin-left: 10px;"),
            uiOutput(ns("download_activators_gen_csv_btn")),
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("activators_gen")),
              type = 4, color = "#fd7e14"
            ),
            br(),
            tags$h5(icon("flask"), " Chemical", style = "color: #fd7e14; font-weight: 600; margin-left: 10px;"),
            uiOutput(ns("download_activators_chem_csv_btn")),
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("activators_chem")),
              type = 4, color = "#fd7e14"
            )
          )
        ),
        
        div(
          style = "display:none;",
          downloadLink(ns("download_suppressors_chem"), "download_suppressors_chem"),
          downloadLink(ns("download_suppressors_gen"), "download_suppressors_gen"),
          downloadLink(ns("download_activators_chem"), "download_activators_chem"),
          downloadLink(ns("download_activators_gen"), "download_activators_gen")
        )
      )
    )
  )
}


# =========================
# GQueryServer 完整优化版（含加载动画 + 棒棒糖图修复）
# =========================

# =========================
# PDF / ZIP 下载辅助函数
# =========================

safe_filename <- function(x, max_len = 120) {
  x <- as.character(x %||% "plot")
  x <- gsub("[\\\\/:*?\"<>|]+", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("_+", "_", x)
  x <- trimws(x)
  if (!nzchar(x)) x <- "plot"
  substr(x, 1, max_len)
}

make_plotly_from_gg <- function(p, tooltip = "all", webgl = FALSE, legend_bottom = TRUE) {
  q <- plotly::ggplotly(p, tooltip = tooltip)
  
  if (legend_bottom) {
    q <- q %>%
      plotly::layout(
        legend = list(
          orientation = "h",
          y = -0.2
        )
      )
  }
  
  if (webgl) {
    q <- plotly::toWebGL(q)
  }
  
  q
}

save_same_plot_pdf <- function(plotly_obj, ggplot_obj, file,
                               width_px = 900,
                               height_px = 650,
                               scale = 2) {
  ok <- FALSE
  
  # 优先保存 plotly 渲染结果：最接近页面展示图
  if (!is.null(plotly_obj)) {
    ok <- tryCatch({
      plotly::save_image(
        p = plotly_obj,
        file = file,
        width = width_px,
        height = height_px,
        scale = scale
      )
      file.exists(file) && isTRUE(file.info(file)$size > 0)
    }, error = function(e) {
      message("plotly::save_image failed, fallback to ggsave: ", conditionMessage(e))
      FALSE
    })
  }
  
  # 如果 Kaleido / save_image 没配置好，则用 ggplot2 兜底
  if (!ok) {
    ok2 <- tryCatch({
      ggplot2::ggsave(
        filename = file,
        plot = ggplot_obj,
        device = grDevices::cairo_pdf,
        width = width_px / 96,
        height = height_px / 96,
        units = "in",
        limitsize = FALSE
      )
      TRUE
    }, error = function(e) {
      message("ggsave cairo_pdf failed, fallback to default pdf: ", conditionMessage(e))
      FALSE
    })
    
    if (!ok2) {
      ggplot2::ggsave(
        filename = file,
        plot = ggplot_obj,
        device = "pdf",
        width = width_px / 96,
        height = height_px / 96,
        units = "in",
        limitsize = FALSE
      )
    }
  }
}


# ---------- 全局缓存（整个 Shiny 进程只加载一次） ----------
.ONEGENE_CACHE <- new.env(parent = emptyenv())
.ONEGENE_DEG_CACHE <- new.env(parent = emptyenv())

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

load_one_object <- function(path) {
  e <- new.env(parent = emptyenv())
  nm <- load(path, envir = e)
  e[[nm[1]]]
}

read_table_fast <- function(path_rdata) {
  base <- sub("\\.[Rr][Dd]ata$", "", path_rdata)
  
  # 优先在 signature_by_gene_fst/ 中查找 .fst / .qs 版本（metadata 用）
  fst_sigdir <- file.path("signature_by_gene_fst", paste0(basename(base), ".fst"))
  qs_sigdir  <- file.path("signature_by_gene_fst", paste0(basename(base), ".qs"))
  
  if (file.exists(fst_sigdir) && requireNamespace("fst", quietly = TRUE)) {
    return(as.data.table(fst::read_fst(fst_sigdir)))
  }
  if (file.exists(qs_sigdir) && requireNamespace("qs", quietly = TRUE)) {
    return(as.data.table(qs::qread(qs_sigdir)))
  }
  
  # 同目录下的 .qs / .fst / .rds
  path_qs  <- paste0(base, ".qs")
  path_fst <- paste0(base, ".fst")
  path_rds <- paste0(base, ".rds")
  
  if (file.exists(path_qs) && requireNamespace("qs", quietly = TRUE)) {
    return(as.data.table(qs::qread(path_qs)))
  }
  
  if (file.exists(path_fst) && requireNamespace("fst", quietly = TRUE)) {
    return(as.data.table(fst::read_fst(path_fst)))
  }
  
  if (file.exists(path_rds)) {
    return(as.data.table(readRDS(path_rds)))
  }
  
  as.data.table(load_one_object(path_rdata))
}

detect_gene_col <- function(dt) {
  cand <- c("gene_name", "Gene", "symbol", "SYMBOL", "GeneSymbol", "gene_symbol", "gene")
  hit <- cand[cand %in% names(dt)]
  if (length(hit) > 0) return(hit[1])
  names(dt)[1]
}

build_signature_index <- function(sig_dt, meta_dt, dataset_col = "Dataset", type_value = NA_character_) {
  gene_col <- detect_gene_col(sig_dt)
  
  genes <- toupper(trimws(as.character(sig_dt[[gene_col]])))
  genes[is.na(genes)] <- ""
  
  dataset_names <- setdiff(names(sig_dt), gene_col)
  
  meta_idx <- match(dataset_names, meta_dt[[dataset_col]])
  keep <- !is.na(meta_idx)
  
  dataset_names <- dataset_names[keep]
  meta_small <- copy(meta_dt[meta_idx[keep]])
  
  if (dataset_col %in% names(meta_small)) {
    meta_small[, (dataset_col) := NULL]
  }
  
  # 实际使用时 mat 由 sig_dt 构建，这里保留占位，实际在 get_onegene_cache 中会正确构建
  mat <- as.matrix(sig_dt[, ..dataset_names])
  storage.mode(mat) <- "double"
  
  gene_to_row <- match(unique(genes), genes)
  names(gene_to_row) <- unique(genes)
  
  list(
    type = type_value,
    gene_col = gene_col,
    genes = genes,
    gene_to_row = gene_to_row,
    datasets = dataset_names,
    mat = mat,
    meta = meta_small
  )
}

get_onegene_cache <- function(
    chem_sig_files  = c("CIGSData.rdata"),
    chem_meta_files = c("CIGSMetadata.rdata"),
    gen_sig_files   = c("GuoziData.rdata"),
    gen_meta_files  = c("GuoziDataMetadata.rdata"),
    data_dir        = "rdata_columns_csv",
    dataset_col     = "Dataset"
) {
  stopifnot(length(chem_sig_files) == length(chem_meta_files))
  stopifnot(length(gen_sig_files) == length(gen_meta_files))
  
  cache_key <- paste(
    paste(normalizePath(chem_sig_files, mustWork = FALSE), collapse = "|"),
    paste(normalizePath(chem_meta_files, mustWork = FALSE), collapse = "|"),
    paste(normalizePath(gen_sig_files, mustWork = FALSE), collapse = "|"),
    paste(normalizePath(gen_meta_files, mustWork = FALSE), collapse = "|"),
    normalizePath(data_dir, mustWork = FALSE),
    sep = "||"
  )
  
  if (exists(cache_key, envir = .ONEGENE_CACHE, inherits = FALSE)) {
    return(get(cache_key, envir = .ONEGENE_CACHE, inherits = FALSE))
  }
  
  if (!dir.exists(data_dir)) {
    stop("data_dir does not exist: ", data_dir)
  }
  
  # chemical
  chem_idx_list <- Map(function(sig_file, meta_file) {
    sig_dt  <- read_table_fast(sig_file)
    meta_dt <- read_table_fast(meta_file)
    build_signature_index(sig_dt, meta_dt, dataset_col, "chemical")
  }, chem_sig_files, chem_meta_files)
  names(chem_idx_list) <- tools::file_path_sans_ext(basename(chem_sig_files))
  
  # genetic
  gen_idx_list <- Map(function(sig_file, meta_file) {
    sig_dt  <- read_table_fast(sig_file)
    meta_dt <- read_table_fast(meta_file)
    build_signature_index(sig_dt, meta_dt, dataset_col, "genetic")
  }, gen_sig_files, gen_meta_files)
  names(gen_idx_list) <- tools::file_path_sans_ext(basename(gen_sig_files))
  
  all_genes <- sort(unique(c(
    unlist(lapply(chem_idx_list, function(x) names(x$gene_to_row)), use.names = FALSE),
    unlist(lapply(gen_idx_list, function(x) names(x$gene_to_row)), use.names = FALSE)
  )))
  all_genes <- all_genes[nzchar(all_genes)]
  
  cache_obj <- list(
    chem_idx_list = chem_idx_list,
    gen_idx_list  = gen_idx_list,
    all_genes     = all_genes,
    data_dir      = data_dir,
    dataset_col   = dataset_col
  )
  
  assign(cache_key, cache_obj, envir = .ONEGENE_CACHE)
  cache_obj
}

query_signature_index <- function(idx, genes) {
  genes <- unique(toupper(trimws(as.character(genes))))
  genes <- genes[nzchar(genes)]
  
  if (length(genes) == 0) return(NULL)
  
  hit_rows <- idx$gene_to_row[genes]
  keep_hit <- !is.na(hit_rows)
  if (!any(keep_hit)) return(NULL)
  
  genes <- genes[keep_hit]
  hit_rows <- as.integer(hit_rows[keep_hit])
  
  out <- vector("list", length(hit_rows))
  
  for (i in seq_along(hit_rows)) {
    eff <- idx$mat[hit_rows[i], ]
    keep_eff <- !is.na(eff)
    if (!any(keep_eff)) next
    
    res <- cbind(
      data.table(
        dataset = idx$datasets[keep_eff],
        effect = as.numeric(eff[keep_eff])
      ),
      idx$meta[keep_eff]
    )
    
    res[, type := idx$type]
    res[, query_gene := genes[i]]
    
    out[[i]] <- res
  }
  
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(NULL)
  
  rbindlist(out, fill = TRUE)
}

get_deg_dt_cached <- function(dataset, data_dir) {
  key <- paste0(normalizePath(data_dir, mustWork = FALSE), "::", dataset)
  
  if (exists(key, envir = .ONEGENE_DEG_CACHE, inherits = FALSE)) {
    return(get(key, envir = .ONEGENE_DEG_CACHE, inherits = FALSE))
  }
  
  fst_file <- file.path(data_dir, paste0(dataset, ".fst"))
  csv_file <- file.path(data_dir, paste0(dataset, ".csv"))
  
  dt <- NULL
  
  if (file.exists(fst_file) && requireNamespace("fst", quietly = TRUE)) {
    dt <- as.data.table(fst::read_fst(fst_file))
  } else if (file.exists(csv_file)) {
    dt <- fread(csv_file, showProgress = FALSE)
  } else {
    # Search in Chemical/ and Genetic/ subdirectories (same as other modules)
    chem_fst <- file.path(data_dir, "Chemical", paste0(dataset, ".fst"))
    gen_fst  <- file.path(data_dir, "Genetic", paste0(dataset, ".fst"))
    if (file.exists(chem_fst)) {
      dt <- as.data.table(fst::read_fst(chem_fst))
    } else if (file.exists(gen_fst)) {
      dt <- as.data.table(fst::read_fst(gen_fst))
    }
  }
  
  assign(key, dt, envir = .ONEGENE_DEG_CACHE)
  dt
}

render_gem_id_link <- function(df, ns) {
  if (!nrow(df)) return(df)
  if (!("GEM_ID" %in% names(df)) || !("dataset" %in% names(df))) return(df)
  
  # Only keep rows with valid PSG/PSC GEM_ID
  valid <- grepl("^(PSG|PSC)\\d+$", df$GEM_ID)
  df <- df[valid]
  if (!nrow(df)) return(df)
  
  df$GEM_ID <- sprintf(
    '<span class="dataset-link" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority: \'event\'})">%s</span>',
    ns("click_dataset"),
    df$dataset,
    df$GEM_ID
  )
  df
}

zip_current_table_files <- function(datasets, file, data_dir) {
  datasets <- unique(trimws(as.character(datasets)))
  datasets <- datasets[!is.na(datasets) & nzchar(datasets)]
  
  rel_files <- paste0(datasets, ".csv")
  abs_files <- file.path(data_dir, rel_files)
  keep <- file.exists(abs_files)
  rel_files <- rel_files[keep]
  
  if (length(rel_files) == 0) {
    note_file <- tempfile(fileext = ".txt")
    writeLines("No matching CSV files found.", con = note_file)
    zip::zipr(zipfile = file, files = basename(note_file), root = dirname(note_file))
  } else {
    zip::zipr(zipfile = file, files = rel_files, root = data_dir)
  }
}

make_download_dt <- function(tbl, ns, download_id, datasets_input_id, extra_drop_cols = character(0), type = "chemical") {
  tbl2 <- copy(tbl)
  
  # Rename effect column to LogFC
  if ("effect" %in% names(tbl2)) setnames(tbl2, "effect", "LogFC")
  
  # Remove unwanted columns (always)
  drop_cols <- intersect(c("type", "source_id"), names(tbl2))
  if (length(drop_cols) > 0) tbl2[, (drop_cols) := NULL]
  
  # Remove extra specified columns
  if (length(extra_drop_cols) > 0) {
    extra_drop_cols <- intersect(extra_drop_cols, names(tbl2))
    if (length(extra_drop_cols) > 0) tbl2[, (extra_drop_cols) := NULL]
  }
  
  # Remove columns that are all zeros (or NA+0)
  zero_cols <- names(tbl2)[sapply(tbl2, function(x) {
    if (is.numeric(x)) {
      all(is.na(x) | x == 0)
    } else {
      FALSE
    }
  })]
  protected <- c("LogFC", "dataset", "Dataset", "GEM_ID", "query_gene",
                 "Accession", "logFC", "Gene", "gene")
  zero_cols <- setdiff(zero_cols, protected)
  if (length(zero_cols) > 0) tbl2[, (zero_cols) := NULL]
  
  # Truncate target (FDA) with click-to-expand toggle
  if ("target" %in% names(tbl2)) {
    tbl2[, target := {
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
  if ("known_targets" %in% names(tbl2)) {
    tbl2[, known_targets := {
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
  
  if ("protein_class" %in% names(tbl2)) {
    tbl2[, protein_class := {
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
  }
  
  # disease_area and indication: removed
  
  # Save raw GEM_ID for download filename (before HTML transform)
  if ("GEM_ID" %in% names(tbl2) && "dataset" %in% names(tbl2)) {
    missing <- is.na(tbl2$GEM_ID) | tbl2$GEM_ID == ""
    tbl2$GEM_ID[missing] <- tbl2$dataset[missing]
    tbl2[, .gem_id_raw := GEM_ID]
  } else {
    tbl2[, .gem_id_raw := as.character(dataset)]
  }
  
  tbl2[, Download := sprintf(
    '<button class="btn btn-sm btn-success dl-btn" data-ds="%s" data-gem="%s"><i class="fa fa-download"></i> Download</button>',
    gsub('"', '&quot;', as.character(dataset), fixed = TRUE),
    gsub('"', '&quot;', as.character(.gem_id_raw), fixed = TRUE)
  )]
  
  tbl2 <- render_gem_id_link(tbl2, ns)
  
  # Move GEM_ID to first column
  if ("GEM_ID" %in% names(tbl2)) {
    setcolorder(tbl2, c("GEM_ID", setdiff(names(tbl2), "GEM_ID")))
  }
  
  # Rename GEM_ID to Dataset for display (after all internal GEM_ID-dependent processing)
  # Use setnames instead of DT::datatable colnames to avoid DT internal "escape" validation errors
  if ("GEM_ID" %in% names(tbl2)) {
    setnames(tbl2, "GEM_ID", "Dataset")
  }
  # Rename Perturbagen to PbChem for display
  if ("Perturbagen" %in% names(tbl2)) setnames(tbl2, "Perturbagen", "PbChem")
  # Capitalize first letter of displayed column names
  cap_cols <- intersect(c("pbgene", "method"), names(tbl2))
  for (cc in cap_cols) setnames(tbl2, cc, paste0(toupper(substr(cc, 1, 1)), substr(cc, 2, nchar(cc))))
  
  # ---- Keep dataset/Dataset in data, hide via columnDefs (Download.R pattern) ----
  
  # Determine which dataset col name exists
  dataset_col_name <- intersect(c("dataset", "Dataset"), names(tbl2))[1]
  if (is.na(dataset_col_name)) dataset_col_name <- "dataset"
  
  dataset_col_idx <- match(dataset_col_name, names(tbl2)) - 1
  gem_id_col_idx <- match(".gem_id_raw", names(tbl2)) - 1
  num_cols <- names(tbl2)[sapply(tbl2, is.numeric)]
  
  # Build columns to hide (always hide these)
  # Note: "Dataset" removed from hide_cols because GEM_ID is now renamed to Dataset
  hide_cols <- c("row_key", "query_genes", "dataset", ".gem_id_raw")
  
  # Type-specific hidden columns
  if (type == "genetic") {
    hide_cols <- c(hide_cols, "pb_ENSEMBL", "gene_biotype", "Type")
  } else {
    hide_cols <- c(hide_cols, "Type")
  }
  
  tbl2 <- replace_empty_with_dash(tbl2, exclude_cols = "GEM_ID")
  dt <- DT::datatable(
    tbl2,
    rownames = FALSE,
    selection = list(mode = "single", target = "row"),
    escape = FALSE,
    extensions = c("Select"),
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      scrollCollapse = TRUE,
      autoWidth = FALSE,
      dom = 'frtip',
      serverSide = TRUE,
      select = list(style = "multi"),
      columnDefs = list(
        list(targets = which(names(tbl2) %in% hide_cols) - 1, visible = FALSE),
        dt_dash_render()
      )
    ),
    callback = DT::JS(sprintf(
      "new MutationObserver(function(m){m.forEach(function(r){$(r.addedNodes).filter('.selected,.selected td').css({boxShadow:'inset 0 0 0 9999px rgba(208,208,208,0.9)',color:'#333'});});}).observe(document.querySelector('#'+table.table().node().id+' tbody'),{childList:true,subtree:true});
      table.on('dblclick', 'tbody tr', function() {
        var data = table.row(this).data();
        if (!data) return;
        var dataset = data[%d];
        var gem = data[%d];
        Shiny.setInputValue('%s', {
          ds: dataset,
          gem: gem,
          ts: Date.now()
        }, {priority: 'event'});
      });",
      dataset_col_idx, gem_id_col_idx,
      ns("download_single_ds")
    ))
  )
  if (length(num_cols) > 0) {
    dt %>% DT::formatSignif(columns = num_cols, digits = 4)
  } else {
    dt
  }
}

# =========================
# 优化后的 GQueryServer（全部包含加载动画 + 棒棒糖图修复）
# =========================
GQueryServer <- function(
    id,
    analysis_type_OV = "Query",
    help_id = NULL
) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # =========================
      # 图对象缓存：用于 PDF / ZIP 下载
      # =========================
      modal_plot_store <- reactiveVal(NULL)
      
      # 组合图PDF下载
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
          
          # 使用patchwork保存拼图
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
      
      
      chem_sig_files  <- c(
        "CIGSData.rdata",
        "HerbData.rdata",
        "LINCS_2020_Chemical.rdata",
        "LINCS_L1000_Chemical.rdata"
      )
      chem_meta_files <- c(
        "CIGSMetadata.rdata",
        "HerbDataMetadata.rdata",
        "LINCS_2020_Metadata.rdata",
        "LINCS_L1000_Metadata.rdata"
      )
      gen_sig_files  <- c(
        "GuoziData.rdata",
        "PerturbAtlasData.rdata",
        "LINCS_2020_KD.rdata",
        "LINCS_2020_KO.rdata",
        "LINCS_L1000_KD.rdata",
        "LINCS_L1000_KO.rdata"
      )
      gen_meta_files <- c(
        "GuoziDataMetadata.rdata",
        "PerturbAtlasMetadata.rdata",
        "LINCS_2020_KD_Metadata.rdata",
        "LINCS_2020_KO_Metadata.rdata",
        "LINCS_L1000_KD_Metadata.rdata",
        "LINCS_L1000_KO_Metadata.rdata"
      )
      
      data_dir    <- "rdata_columns_csv"
      dataset_col <- "Dataset"
      
      if (!"dataset_csv" %in% names(shiny::resourcePaths())) {
        shiny::addResourcePath("dataset_csv", normalizePath(data_dir))
      }
      
      # ----------------------------
      # 1️⃣ 安全加载 VirtualSelect
      # ----------------------------
      safe_qs_genes <- function(file) {
        obj <- qs::qread(file)
        if (is.list(obj) || is.data.frame(obj)) {
          genes <- obj$gene
          genes <- genes[!is.na(genes) & nzchar(genes) & grepl("[A-Za-z]", genes)]
          as.character(genes)
        } else {
          character(0)
        }
      }
      
      all_genes <- unique(unlist(lapply(
        list.files("signature_by_gene_fst", pattern = "_gene_map\\.qs$", full.names = TRUE),
        safe_qs_genes
      ), use.names = FALSE))
      
      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "gene",
        choices = all_genes,
        selected = NULL
      )
      
      # ----------------------------
      # 2️⃣ 查询结果
      # ----------------------------
      query_res <- eventReactive(input$search, {
        req(input$gene)
        genes <- unique(toupper(trimws(as.character(input$gene))))
        genes <- genes[nzchar(genes)]
        req(length(genes) > 0)
        
        out <- list()
        
        if (!is.null(input$ptype) && input$ptype %in% c("all", "chemical")) {
          x1_list <- lapply(seq_along(chem_sig_files), function(i) {
            query_onegene_by_gene_fst_one_source(
              sig_file = chem_sig_files[i],
              meta_file = chem_meta_files[i],
              genes = genes,
              type_value = "chemical",
              by_gene_dir = "signature_by_gene_fst",
              signature_dir = data_dir,
              dataset_col = dataset_col
            )
          })
          x1_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, x1_list)
          if (length(x1_list) > 0) out <- c(out, x1_list)
        }
        
        if (!is.null(input$ptype) && input$ptype %in% c("all", "genetic")) {
          x2_list <- lapply(seq_along(gen_sig_files), function(i) {
            query_onegene_by_gene_fst_one_source(
              sig_file = gen_sig_files[i],
              meta_file = gen_meta_files[i],
              genes = genes,
              type_value = "genetic",
              by_gene_dir = "signature_by_gene_fst",
              signature_dir = data_dir,
              dataset_col = dataset_col
            )
          })
          x2_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, x2_list)
          if (length(x2_list) > 0) out <- c(out, x2_list)
        }
        
        if (length(out) == 0) return(NULL)
        res <- rbindlist(out, fill = TRUE)
        
        cutoff_raw <- input$logfc_filter
        if (!is.null(cutoff_raw) && nzchar(trimws(cutoff_raw))) {
          cutoff <- suppressWarnings(as.numeric(trimws(cutoff_raw)))
          validate(need(!is.na(cutoff), "Invalid |logFC| cutoff"))
          res <- res[!is.na(effect) & abs(effect) > cutoff]
        }
        # Remove rows where Perturbagen contains Chinese characters
        if ("Perturbagen" %in% names(res)) {
          res <- res[!grepl("[\u4e00-\u9fff]", Perturbagen)]
        }
        if (nrow(res) == 0) return(NULL)
        res
      }, ignoreInit = TRUE)
      
      suppressors_tbl <- reactive({
        res <- query_res_final()
        if (is.null(res) || nrow(res) == 0) return(NULL)
        suppressors <- res[!is.na(effect) & effect < 0]
        if (nrow(suppressors) == 0) return(NULL)
        suppressors[order(effect)]
      })
      
      activators_tbl <- reactive({
        res <- query_res_final()
        if (is.null(res) || nrow(res) == 0) return(NULL)
        activators <- res[!is.na(effect) & effect > 0]
        if (nrow(activators) == 0) return(NULL)
        activators[order(-effect)]
      })
      
      # ===== Demo lazy-load: compute only on button click =====
      demo_data <- reactiveVal(NULL)
      demo_active <- reactiveVal(FALSE)
      
      # Normal search resets demo mode
      observeEvent(input$search, {
        demo_active(FALSE)
      }, ignoreInit = TRUE)
      
      # Unified query result: demo cache takes priority
      query_res_final <- reactive({
        if (demo_active() && !is.null(demo_data())) {
          return(demo_data())
        }
        query_res()
      })
      
      # ===== Demo button: compute on click =====
      observeEvent(input$demo_btn, {
        shinyjs::disable("demo_btn")
        on.exit(shinyjs::enable("demo_btn"))
        
        shinyalert(
          title = "Loading",
          text = "Please wait...",
          type = "info",
          closeOnEsc = FALSE,
          closeOnClickOutside = FALSE,
          showConfirmButton = FALSE
        )
        
        demo_genes <- "PCNA"
        out <- list()
        
        x1_list <- lapply(seq_along(chem_sig_files), function(i) {
          query_onegene_by_gene_fst_one_source(
            sig_file = chem_sig_files[i],
            meta_file = chem_meta_files[i],
            genes = demo_genes,
            type_value = "chemical",
            by_gene_dir = "signature_by_gene_fst",
            signature_dir = data_dir,
            dataset_col = dataset_col
          )
        })
        x1_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, x1_list)
        if (length(x1_list) > 0) out <- c(out, x1_list)
        
        x2_list <- lapply(seq_along(gen_sig_files), function(i) {
          query_onegene_by_gene_fst_one_source(
            sig_file = gen_sig_files[i],
            meta_file = gen_meta_files[i],
            genes = demo_genes,
            type_value = "genetic",
            by_gene_dir = "signature_by_gene_fst",
            signature_dir = data_dir,
            dataset_col = dataset_col
          )
        })
        x2_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, x2_list)
        if (length(x2_list) > 0) out <- c(out, x2_list)
        
        if (length(out) > 0) {
          res <- rbindlist(out, fill = TRUE)
          res <- res[!is.na(effect) & abs(effect) > 2]
          if ("Perturbagen" %in% names(res)) {
            res <- res[!grepl("[\u4e00-\u9fff]", Perturbagen)]
          }
          if (nrow(res) > 0) {
            demo_data(res)
            shinyalert::closeAlert()
            shinyWidgets::updateVirtualSelect("gene", selected = "PCNA", session = session)
            shinyWidgets::updateVirtualSelect("ptype", selected = "all", session = session)
            shinyWidgets::updateVirtualSelect("logfc_filter", selected = "2", session = session)
            demo_active(TRUE)
          } else {
            shinyalert::closeAlert()
            shinyalert("No Data", "Demo query returned no results.", type = "warning")
          }
        } else {
          shinyalert::closeAlert()
          shinyalert("No Data", "Demo query returned no results.", type = "warning")
        }
      })
      
      # ----------------------------
      # 3️⃣ 点击 dataset 弹窗 + 全量绘图（含棒棒糖图修复）
      # ----------------------------
      observeEvent(input$click_dataset, {
        ds <- input$click_dataset
        if (is.null(ds) || !nzchar(ds)) return(NULL)
        
        dt <- isolate(query_res_final())
        if (is.null(dt) || nrow(dt) == 0) return(NULL)
        
        row <- dt[dataset == ds]
        if (is.null(row) || nrow(row) == 0) return(NULL)
        row <- row[1]
        
        type <- as.character(row$type %||% "")
        compound <- as.character(row$Perturbagen %||% "")
        gene <- as.character(row$pbgene %||% "")
        query_gene <- as.character(row$query_gene %||% "")
        effect_val <- row$effect %||% 0
        
        link_html <- NULL; mol_img <- NULL; deg_gene_info <- NULL
        
        # 2D/3D chemical
        if (!is.na(type) && type == "chemical" && nzchar(compound)) {
          cid_url <- URLencode(compound, reserved = TRUE)
          img_2d <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", cid_url, "/PNG")
          img_3d <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", cid_url, "/PNG?record_type=3d")
          mol_img <- div(
            class = "modal-image-row",
            fluidRow(
              column(
                6,
                div(
                  class = "modal-image-card",
                  div(
                    class = "modal-img-box",
                    tags$img(
                      src = img_2d,
                      class = "modal-structure-img",
                      onerror = "this.parentElement.parentElement.style.display='none'"
                    )
                  ),
                  div(class = "modal-img-caption", "2D Structure")
                )
              ),
              column(
                6,
                div(
                  class = "modal-image-card",
                  div(
                    class = "modal-img-box",
                    tags$img(
                      src = img_3d,
                      class = "modal-structure-img",
                      onerror = "this.parentElement.parentElement.style.display='none'"
                    )
                  ),
                  div(class = "modal-img-caption", "3D Ball-Stick Model")
                )
              )
            ),
            br()
          )
          link_html <- tags$a(href=paste0("https://pubchem.ncbi.nlm.nih.gov/compound/", cid_url), target="_blank", class="btn btn-sm btn-primary", icon("external-link-alt"), " Open in PubChem")
        }
        
        # genetic STRING
        if (!is.na(type) && type == "genetic" && nzchar(gene)) {
          g <- URLencode(gene, reserved = TRUE)
          string_img <- paste0("https://string-db.org/api/image/network?identifiers=", g, "&species=9606")
          mol_img <- div(
            class = "modal-image-row",
            div(
              class = "modal-image-card",
              div(
                class = "modal-img-box-large",
                tags$img(
                  src = string_img,
                  class = "modal-structure-img",
                  onerror = "this.parentElement.parentElement.style.display='none'"
                )
              ),
              div(
                class = "modal-img-caption",
                "Protein-Protein Interaction Network (STRING)"
              )
            ),
            br()
          )
          link_html <- tags$a(href=paste0("https://www.uniprot.org/uniprotkb?query=", g), target="_blank", class="btn btn-sm btn-warning", icon("external-link-alt"), " Open in UniProt")
        }
        
        effect_class <- ifelse(!is.na(effect_val) && effect_val < 0, "effect-suppressor", "effect-activator")
        effect_tag <- div(class=paste("detail-effect", effect_class), icon("chart-line"), paste("LogFC:", round(effect_val,4)))
        
        # 展示 row 内容
        skip_cols <- c("dataset", "Dataset", "Download", "source_id", "type")
        display_map <- c("GEM_ID" = "Dataset", "Perturbagen" = "PbChem")
        items <- list()
        for (col in names(row)) {
          if (col %in% skip_cols) next
          val <- row[[col]]
          if (is.null(val) || length(val) == 0 ||
              identical(val, "") || identical(val, "NA") ||
              (length(val) == 1 && is.na(val))) next
          if (is.numeric(val)) val <- round(val, 4)
          dcol <- if (col %in% names(display_map)) display_map[col] else col
          items[[length(items)+1]] <- div(class="detail-item", span(class="detail-label", paste0(dcol, ": ")), span(class="detail-value", gsub("<[^>]+>", "", as.character(val))))
        }
        
        # ======================
        # 核心：安全读取数据 + 自动检测P值/FDR + 全量绘图（优化版）
        # ======================
        deg_dt <- get_deg_dt_cached(row$dataset, data_dir)
        
        plot_lollipop <- NULL
        plot_density <- NULL
        plot_main_1 <- NULL
        plot_main_2 <- NULL
        plot_count_bar <- NULL
        plot_cdf <- NULL
        
        valid_data <- FALSE
        deg <- NULL
        has_pval <- FALSE
        has_fdr <- FALSE
        
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
            deg <- data.table(
              gene  = as.character(deg_copy[[gene_col]]),
              logFC = suppressWarnings(as.numeric(deg_copy[[fc_col]]))
            )
            if (!is.na(pval_col) && !is.na(fdr_col)) {
              deg$pval <- suppressWarnings(as.numeric(deg_copy[[pval_col]]))
              deg$fdr  <- suppressWarnings(as.numeric(deg_copy[[fdr_col]]))
              if (sum(!is.na(deg$pval)) > 0 && sum(!is.na(deg$fdr)) > 0) {
                has_pval <- TRUE
                has_fdr  <- TRUE
              }
            }
            # 过滤：logFC非NA，gene非NA非空且包含至少一个字母（排除纯数字）
            deg <- deg[!is.na(logFC) & !is.na(gene) & nzchar(gene) & grepl("[A-Za-z]", gene)]
            # 去重：同一基因可能多次出现，取绝对logFC最大的一条
            if (nrow(deg) > 0) {
              deg <- deg[order(-abs(logFC))]
              deg <- deg[!duplicated(toupper(gene)), ]
            }
            if (nrow(deg) > 0) valid_data <- TRUE
          }
        }
        
        if (valid_data && !is.null(deg)) {
          deg$group <- ifelse(deg$logFC > 1, "Up", ifelse(deg$logFC < -1, "Down", "NS"))
          group_colors <- c("Down"="#4285F4", "NS"="gray", "Up"="#EA4335")
          
          # 计数条形图
          count_stats <- deg[, .(gene_count = .N), by = group]
          count_stats$group <- factor(count_stats$group, levels = c("Down", "NS", "Up"))
          plot_count_bar <- ggplot(count_stats, aes(x=group, y=gene_count, fill=group)) +
            geom_col(width=0.7, show.legend = FALSE) +
            scale_fill_manual(values=group_colors) +
            geom_text(aes(label=gene_count), vjust=-0.3, fontface="bold") +
            labs(title="Differential Gene Count", x="", y="Gene Number") +
            theme_bw()
          
          # CDF曲线
          deg_sorted <- deg[order(logFC)]
          deg_sorted$cdf <- seq(0, 1, length.out = nrow(deg_sorted))
          deg_sorted$group <- "NS"
          deg_sorted$group[deg_sorted$logFC < -1] <- "Down"
          deg_sorted$group[deg_sorted$logFC > 1] <- "Up"
          deg_sorted$group <- factor(deg_sorted$group, levels = c("Down", "NS", "Up"))
          deg_sorted$gene <- as.character(deg_sorted$gene)
          deg_sorted$gene[is.na(deg_sorted$gene) | deg_sorted$gene == ""] <- "unknown"
          
          # 计算logFC绝对值top10基因
          top10_genes <- deg[order(-abs(logFC))][1:min(10, .N)]
          top10_genes <- top10_genes[!is.na(gene) & nzchar(as.character(gene))]
          top10_genes$gene <- as.character(top10_genes$gene)
          # 为top10计算cdf值
          setkey(deg_sorted, gene)
          top10_genes <- merge(top10_genes, deg_sorted[, .(gene, cdf)], by = "gene", all.x = TRUE)
          
          plot_cdf <- ggplot(deg_sorted, aes(x = logFC, y = cdf, color = group, text = gene)) +
            geom_line(linewidth = 1.1, alpha = 0.9) +
            geom_point(size = 0.7, alpha = 0.8) +
            scale_color_manual(values = group_colors) +
            geom_vline(xintercept = c(-1, 0, 1), color = "black", linewidth = 0.35, linetype = "dashed") +
            labs(title = "logFC Cumulative Distribution (CDF)",
                 x = "logFC", y = "Cumulative Proportion") +
            # 添加top10标注
            ggrepel::geom_text_repel(
              data = top10_genes,
              aes(x = logFC, y = cdf, label = gene),
              size = 3, max.overlaps = 15, box.padding = 0.3, color = "black"
            ) +
            theme_bw() +
            theme(legend.position = "bottom",
                  plot.title = element_text(face = "bold", size = 13))
          
          {
            main_plot_title <- "logFC Signature Pattern"
            deg_sorted <- deg[order(logFC)]
            deg_sorted$rank <- 1:nrow(deg_sorted)
            plot_main_1 <- ggplot(deg_sorted, aes(x=rank, y=logFC, color=group, text=gene)) +
              geom_point(size=0.8, alpha=0.6) +
              scale_color_manual(values=group_colors) +
              geom_hline(yintercept = c(-1,0,1), linetype="dashed", linewidth=0.3) +
              labs(title="Gene Ranked by logFC", x="Gene Rank", y="logFC") +
              theme_bw() + theme(legend.position="bottom")
            
            deg$bin <- cut(deg$logFC, breaks = seq(-10,10,1), include.lowest = TRUE)
            bin_stats <- deg[, .(count = .N), by = .(bin, group)]
            if (nrow(bin_stats) > 0) {
              plot_main_2 <- ggplot(bin_stats, aes(x=bin, y=count, fill=group)) +
                geom_col(position="stack", width=0.8) +
                scale_fill_manual(values=group_colors) +
                geom_vline(xintercept = which(levels(bin_stats$bin) == "(-0.5,0]"), linetype="dashed", linewidth=0.3) +
                labs(title="logFC Distribution", x="logFC Bins", y="Gene Count") +
                theme_bw() + theme(legend.position="bottom", axis.text.x = element_text(angle=45, hjust=1, size=8))
            }
          }
          
          # ========== 棒棒糖图（修复版） ==========
          top_up <- head(deg[logFC>0][order(-logFC)], 20)
          top_down <- head(deg[logFC<0][order(logFC)], 20)
          top_genes <- rbindlist(list(top_up, top_down), fill=TRUE)
          if (nrow(top_genes) > 0) {
            top_genes$direction <- ifelse(top_genes$logFC > 0, "Up", "Down")
            # 强制gene为字符型，避免factor levels问题
            top_genes$gene <- as.character(top_genes$gene)
            top_genes <- top_genes[!is.na(gene) & nzchar(gene)]
            if (nrow(top_genes) > 0) {
              plot_lollipop <- ggplot(top_genes, aes(x=reorder(gene, logFC), y=logFC, fill=direction, text=gene)) +
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
            labs(title="Density", x="logFC", y="Density") +
            theme_bw()
        }
        
        # ======================
        # 将弹窗中的 ggplot / plotly 图缓存起来，用于 PDF / ZIP 下载
        # ======================
        modal_ggplots <- list()
        modal_plotlys <- list()
        combined_plot <- NULL
        
        if (valid_data) {
          modal_ggplots <- list(
            count_bar = plot_count_bar,
            cdf = plot_cdf,
            main_1 = plot_main_1,
            main_2 = plot_main_2,
            density = plot_density,
            lollipop = plot_lollipop
          )
          
          modal_ggplots <- Filter(function(x) !is.null(x) && inherits(x, "ggplot"), modal_ggplots)
          
          # 构建组合拼图
          if (length(modal_ggplots) > 0) {
            # 设置统一主题（保留标题，只调整大小和位置）
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
            
            # 为每个图添加统一主题（保留标题）
            modal_ggplots <- lapply(modal_ggplots, function(p) {
              p + theme_small
            })
            
            # 组合布局：2行3列
            n_plots <- length(modal_ggplots)
            
            # 根据图的数量决定布局
            combined_plot <- switch(as.character(n_plots),
              "1" = modal_ggplots[[1]],
              "2" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], ncol = 2),
              "3" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], modal_ggplots[[3]], ncol = 3),
              "4" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], 
                             modal_ggplots[[3]], modal_ggplots[[4]], ncol = 2),
              "5" = wrap_plots(modal_ggplots[[1]], modal_ggplots[[2]], modal_ggplots[[3]], 
                             modal_ggplots[[4]], modal_ggplots[[5]], ncol = 3),
              wrap_plots(modal_ggplots[[1]],modal_ggplots[[2]],modal_ggplots[[3]],modal_ggplots[[4]],modal_ggplots[[5]],modal_ggplots[[6]],
                design=c(area(1,1),area(1,2),area(1,3),area(2,1,2,2),area(3,1,3,2),area(2,3,3,3)))
            )
            
            # 添加数据集标题
            if (n_plots != 6) {
              combined_plot <- combined_plot
            }
          }
          
          modal_plot_store(list(
            dataset = as.character(row$dataset),
            gem_id = if (!is.null(row$GEM_ID) && nzchar(as.character(row$GEM_ID))) as.character(row$GEM_ID) else as.character(row$dataset),
            ggplots = modal_ggplots,
            combined_plot = combined_plot
          ))
        } else {
          modal_plot_store(NULL)
        }
        
        # ======================
        # 弹窗布局（所有 plotlyOutput 添加 withSpinner）
        # ======================
        showModal(modalDialog(
          tags$div(
            id = ns("dataset_detail_modal"),
            tags$script(HTML(sprintf("
              setTimeout(function() {
                var el = document.getElementById('%s');
                if (el) {
                  $(el).closest('.modal-dialog').addClass('dataset-wide-modal');
                }
              }, 50);
            ", ns("dataset_detail_modal")))),
            
            div(class="detail-card", 
                effect_tag, br(), link_html, br(), mol_img,
                items, deg_gene_info,
                
                if(valid_data) {
                  tagList(
                    br(), hr(),
                    
                    # 下载按钮
                    tags$div(
                      style = "margin: 10px 0 16px 0; text-align: center;",
                      downloadButton(
                        ns("download_combined_pdf"),
                        "Download Plot",
                        class = "btn btn-default btn-lg"
                      )
                    ),
                    
                    # 组合拼图展示（使用plotOutput渲染ggplot拼图）
                    withSpinner(plotOutput(ns("modal_combined"), height="650px"), type=4, color="#3a7bd5")
                  )
                }
            )
          ),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        
        # 渲染组合图（使用renderPlot直接渲染ggplot拼图）
        if(valid_data) {
          output$modal_combined <- renderPlot({
            st <- modal_plot_store()
            req(st$combined_plot)
            print(st$combined_plot)
          })
        }
      })
      # ----------------------------
      # 4️⃣ 按 Type 列拆分 Chemical / Genetic
      # ----------------------------
      subset_type <- function(tbl, target_type) {
        if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
        if (target_type == "genetic") {
          tbl[type == "genetic"]
        } else {
          tbl[type != "genetic"]
        }
      }
      
      suppressors_chem_tbl <- reactive({
        subset_type(suppressors_tbl(), "chemical")
      })
      suppressors_gen_tbl <- reactive({
        subset_type(suppressors_tbl(), "genetic")
      })
      activators_chem_tbl <- reactive({
        subset_type(activators_tbl(), "chemical")
      })
      activators_gen_tbl <- reactive({
        subset_type(activators_tbl(), "genetic")
      })
      
      # ----------------------------
      # 5️⃣ 表格输出
      # ----------------------------
      output$suppressors_chem <- DT::renderDT({
        tbl <- suppressors_chem_tbl()
        if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
        make_download_dt(tbl, ns, "download_suppressors_chem", "suppressors_chem_datasets",
                         extra_drop_cols = c("pbgene", "pb_ENSEMBL", "pb_ENSEMB", "gene_biotype", "method", "known_targets", "protein_class", "disease_area", "indication"),
                         type = "chemical")
      })
      
      output$suppressors_gen <- DT::renderDT({
        tbl <- suppressors_gen_tbl()
        if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
        make_download_dt(tbl, ns, "download_suppressors_gen", "suppressors_gen_datasets",
                         extra_drop_cols = c("Perturbagen", "Time", "Dose", "target", "known_targets", "uniprot_id", "entrez_id", "disease_area", "indication"),
                         type = "genetic")
      })
      
      output$activators_chem <- DT::renderDT({
        tbl <- activators_chem_tbl()
        if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
        make_download_dt(tbl, ns, "download_activators_chem", "activators_chem_datasets",
                         extra_drop_cols = c("pbgene", "pb_ENSEMBL", "pb_ENSEMB", "gene_biotype", "method", "known_targets", "protein_class", "disease_area", "indication"),
                         type = "chemical")
      })
      
      output$activators_gen <- DT::renderDT({
        tbl <- activators_gen_tbl()
        if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
        make_download_dt(tbl, ns, "download_activators_gen", "activators_gen_datasets",
                         extra_drop_cols = c("Perturbagen", "Time", "Dose", "target", "known_targets", "uniprot_id", "entrez_id", "disease_area", "indication"),
                         type = "genetic")
      })
      
      # ===== Conditional download button UIs (show only when table data is ready) =====
      output$download_suppressors_gen_csv_btn <- renderUI({
        req(suppressors_gen_tbl())
        req(nrow(suppressors_gen_tbl()) > 0)
        div(style = "text-align: left; margin-bottom: 3px;",
            downloadButton(ns("download_suppressors_gen_csv"), "CSV",
                           class = "btn-sm btn-outline-secondary"))
      })
      output$download_suppressors_chem_csv_btn <- renderUI({
        req(suppressors_chem_tbl())
        req(nrow(suppressors_chem_tbl()) > 0)
        div(style = "text-align: left; margin-bottom: 3px;",
            downloadButton(ns("download_suppressors_chem_csv"), "CSV",
                           class = "btn-sm btn-outline-secondary"))
      })
      output$download_activators_gen_csv_btn <- renderUI({
        req(activators_gen_tbl())
        req(nrow(activators_gen_tbl()) > 0)
        div(style = "text-align: left; margin-bottom: 3px;",
            downloadButton(ns("download_activators_gen_csv"), "CSV",
                           class = "btn-sm btn-outline-secondary"))
      })
      output$download_activators_chem_csv_btn <- renderUI({
        req(activators_chem_tbl())
        req(nrow(activators_chem_tbl()) > 0)
        div(style = "text-align: left; margin-bottom: 3px;",
            downloadButton(ns("download_activators_chem_csv"), "CSV",
                           class = "btn-sm btn-outline-secondary"))
      })
      
      # ===== Table CSV downloads (1:1 export) =====
      make_onegene_csv <- function(tbl) {
        if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
        dt <- copy(tbl)
        if ("effect" %in% names(dt)) setnames(dt, "effect", "LogFC")
        drop <- intersect(c("type", "source_id"), names(dt))
        if (length(drop) > 0) dt[, (drop) := NULL]
        # Drop internal/display columns
        drop2 <- intersect(c(".gem_id_raw", "Download", "Dataset", "dataset"), names(dt))
        if (length(drop2) > 0) dt[, (drop2) := NULL]
        dots <- grep("^\\.", names(dt), value = TRUE)
        if (length(dots) > 0) dt[, (dots) := NULL]
        strip_html(dt)
        dt
      }
      
      output$download_suppressors_chem_csv <- downloadHandler(
        filename = function() paste0("suppressors_chemical_", Sys.Date(), ".csv"),
        content = function(file) {
          dt <- make_onegene_csv(suppressors_chem_tbl())
          if (is.null(dt)) dt <- data.table(Message = "No data")
          fwrite(dt, file)
        }
      )
      output$download_suppressors_gen_csv <- downloadHandler(
        filename = function() paste0("suppressors_genetic_", Sys.Date(), ".csv"),
        content = function(file) {
          dt <- make_onegene_csv(suppressors_gen_tbl())
          if (is.null(dt)) dt <- data.table(Message = "No data")
          fwrite(dt, file)
        }
      )
      output$download_activators_chem_csv <- downloadHandler(
        filename = function() paste0("activators_chemical_", Sys.Date(), ".csv"),
        content = function(file) {
          dt <- make_onegene_csv(activators_chem_tbl())
          if (is.null(dt)) dt <- data.table(Message = "No data")
          fwrite(dt, file)
        }
      )
      output$download_activators_gen_csv <- downloadHandler(
        filename = function() paste0("activators_genetic_", Sys.Date(), ".csv"),
        content = function(file) {
          dt <- make_onegene_csv(activators_gen_tbl())
          if (is.null(dt)) dt <- data.table(Message = "No data")
          fwrite(dt, file)
        }
      )
      
    # ================== 单数据集动态下载（Blob方式，绕过downloadLink程序化点击失效） ==================
    observeEvent(input$download_single_ds, {
      req(input$download_single_ds)
      ds  <- input$download_single_ds$ds
      gem <- input$download_single_ds$gem

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
  }
)}

# ===== Alias for backwards compatibility =====
OneGeneMUI <- GQueryUI
OneGeneMServer <- GQueryServer