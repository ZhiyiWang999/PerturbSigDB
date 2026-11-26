# ============================================================
# Download Module - Metadata browsing & perturbation data download
# + Filter & ZIP batch download
# ============================================================

DownloadUI <- function(id, title = "Download") {
  ns <- NS(id)

  tagList(
    tags$head(
      # ========== Custom JS handlers ==========
      tags$script(HTML(paste0(
        # Single dataset download button click (off before on to prevent duplicate bindings)
        "$(document).off('click', '.dl-btn').on('click', '.dl-btn', function(e) {
          e.stopPropagation();
          var ds = this.getAttribute('data-ds');
          var gem = this.getAttribute('data-gem');
          Shiny.setInputValue('", ns("download_single_ds"), "', {
            ds: ds,
            gem: gem,
            ts: Date.now()
          }, {priority: 'event'});
        });",
        # Filter remove button (off before on to prevent duplicate bindings)
        "$(document).off('click', '.remove-filter-btn').on('click', '.remove-filter-btn', function(e) {
          var idx = this.getAttribute('data-idx');
          Shiny.setInputValue('", ns("remove_filter"), "', {idx: idx, ts: Date.now()}, {priority: 'event'});
        });",
        # Preview button: show spinner on click
        "Shiny.addCustomMessageHandler('preview_loading', function(msg) {
          var btn = $('#", ns("preview_matches"), "');
          if (msg.loading) {
            btn.prop('disabled', true).html('<i class=\"fa fa-spinner fa-spin\"></i> Loading...');
          } else {
            btn.prop('disabled', false).html('<i class=\"fa fa-search\"></i> Preview');
          }
        });"
      ))),
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
      ),
      tags$style(HTML("
        .dl-section-header {
          color: #2c5f8a;
          border-left: 4px solid #3a7bd5;
          padding-left: 10px;
          margin-top: 5px;
          margin-bottom: 5px;
        }
        /* Bulk download area styles */
        .bulk-download-box {
          background: linear-gradient(135deg, #e8f4fd 0%, #d4e9f7 100%);
          border: 2px solid #3a7bd5;
          border-radius: 12px;
          padding: 20px 25px;
          margin-bottom: 20px;
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 15px;
        }
        .bulk-download-box .bulk-info {
          flex: 1;
          min-width: 250px;
        }
        .bulk-download-box .bulk-info h4 {
          color: #1a5276;
          margin: 0 0 5px 0;
          font-size: 16px;
        }
        .bulk-download-box .bulk-info p {
          color: #555;
          margin: 0;
          font-size: 13px;
        }
        .bulk-download-btn {
          display: inline-block;
          background: linear-gradient(135deg, #3a7bd5 0%, #2c5f8a 100%);
          color: white !important;
          padding: 12px 28px;
          border-radius: 8px;
          font-size: 15px;
          font-weight: 600;
          text-decoration: none !important;
          white-space: nowrap;
          transition: all 0.25s ease;
          box-shadow: 0 2px 8px rgba(58, 123, 213, 0.35);
        }
        .bulk-download-btn:hover {
          background: linear-gradient(135deg, #2c5f8a 0%, #1a5276 100%);
          box-shadow: 0 4px 14px rgba(58, 123, 213, 0.5);
          transform: translateY(-1px);
          color: white !important;
          text-decoration: none !important;
        }
        .bulk-download-btn i {
          margin-right: 8px;
        }

        /* ===== Filter section styles ===== */
        .filter-row {
          display: flex;
          align-items: center;
          gap: 8px;
          margin-bottom: 8px;
          padding: 10px 12px;
          background: #f8f9fa;
          border-radius: 8px;
          border: 1px solid #dee2e6;
          flex-wrap: wrap;
        }
        .filter-row select, .filter-row input {
          height: 34px;
          border-radius: 6px;
          border: 1px solid #ced4da;
          padding: 4px 8px;
        }
        .filter-row .field-select { min-width: 140px; }
        .filter-row .op-select   { min-width: 120px; }
        .filter-row .val-input   { min-width: 200px; flex: 1; }
        .remove-filter-btn {
          background: #dc3545;
          color: white;
          border: none;
          border-radius: 50%;
          width: 28px;
          height: 28px;
          cursor: pointer;
          font-size: 14px;
          line-height: 28px;
          text-align: center;
          padding: 0;
          flex-shrink: 0;
        }
        .remove-filter-btn:hover { background: #c82333; }
        .add-filter-btn {
          background: #3a7bd5;
          color: white;
          border: none;
          border-radius: 6px;
          padding: 8px 18px;
          cursor: pointer;
          font-size: 13px;
        }
        .add-filter-btn:hover { background: #2c5f8a; }
        .filter-summary {
          margin-top: 15px;
          padding: 15px;
          background: linear-gradient(135deg, #e8f4fd 0%, #d4e9f7 100%);
          border-radius: 10px;
          border: 2px solid #3a7bd5;
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 12px;
        }
        .filter-summary .match-count {
          font-size: 18px;
          font-weight: bold;
          color: #1a5276;
        }
        .filter-summary .btn-group {
          display: flex;
          gap: 10px;
        }
        .preview-btn {
          background: #17a2b8;
          color: white;
          border: none;
          border-radius: 6px;
          padding: 10px 22px;
          font-size: 14px;
          font-weight: 600;
          cursor: pointer;
        }
        .preview-btn:hover { background: #138496; }
        .download-zip-btn {
          background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%);
          color: white;
          border: none;
          border-radius: 6px;
          padding: 10px 22px;
          font-size: 14px;
          font-weight: 600;
          cursor: pointer;
          box-shadow: 0 2px 8px rgba(40, 167, 69, 0.35);
        }
        .download-zip-btn:hover {
          box-shadow: 0 4px 14px rgba(40, 167, 69, 0.5);
          transform: translateY(-1px);
        }
        .download-zip-btn:disabled {
          background: #6c757d;
          cursor: not-allowed;
          box-shadow: none;
          transform: none;
        }
      "))
    ),

    tabsetPanel(
      id = ns("tabs"),

      # ==================== Genetic Perturbagens ====================
      tabPanel("Genetic Perturbagens",
        br(),
        tags$h4(icon("table"), " Metadata Information", class = "dl-section-header"),
        fluidRow(
          column(12,
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("gen_meta_table")),
              type = 4, color = "#3a7bd5"
            )
          )
        ),
        br(),
        div(style = "text-align: left; margin-bottom: 5px;",
          downloadButton(ns("download_gen_meta"), " Download Full Metadata CSV",
            class = "btn-success", icon = icon("download"))
        ),
        tags$hr(),
        tags$h4(icon("download"), " Perturbation Data Download", class = "dl-section-header"),
        fluidRow(
          column(12,
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("gen_data_table")),
              type = 4, color = "#3a7bd5"
            )
          )
        )
      ),

      # ==================== Chemical Perturbagens ====================
      tabPanel("Chemical Perturbagens",
        br(),
        tags$h4(icon("table"), " Metadata Information Download", class = "dl-section-header"),
        fluidRow(
          column(12,
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("chem_meta_table")),
              type = 4, color = "#3a7bd5"
            )
          )
        ),
        br(),
        div(style = "text-align: left; margin-bottom: 5px;",
          downloadButton(ns("download_chem_meta"), " Download Full Metadata CSV",
            class = "btn-success", icon = icon("download"))
        ),
        tags$hr(),
        tags$h4(icon("download"), " Perturbation Data Download", class = "dl-section-header"),
        fluidRow(
          column(12,
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("chem_data_table")),
              type = 4, color = "#3a7bd5"
            )
          )
        )
      )
    ),

    # ================================================================
    # Filter & Batch ZIP Download Section
    # ================================================================
    br(),
    box(width = 12, title = "Filter & Download Perturbation Data", status = "primary", solidHeader = TRUE,
      tags$div(style = "margin-bottom: 12px; line-height: 1.8;",
        tags$p(tags$b("How to use filters:")),
        tags$ol(
          tags$li(tags$b("Select a field"), " — Choose from Data Type (Chem/Genetic), Cell Line, Tissue, Perturbagen, Target Gene, or Accession."),
          tags$li(tags$b("Enter a keyword"), " — Type any keyword and the system will find all records whose selected field contains that keyword. For example, entering ", tags$code("MCF7"), " in Cell Line will match 'MCF7', 'MCF-7', 'MCF7/ADR', etc."),
          tags$li(tags$b("Add more filters"), " — Click ", tags$b("Add Filter"), " to add another condition. Use ", tags$b("AND"), " (all conditions must match) or ", tags$b("OR"), " (any condition matches) to combine multiple rows."),
          tags$li(tags$b("Preview & Download"), " — Click ", tags$b("Preview"), " to inspect matching results in a table, or ", tags$b("Download ZIP"), " to package all matching perturbation datasets into a ZIP archive.")
        ),
        tags$p(tags$b("Matching rules:"), " All searches are ", tags$b("case-insensitive"), " and ", tags$b("contain"), "-based. Special characters are automatically ignored during matching (e.g. ", tags$code("MCF-7"), " matches ", tags$code("MCF7"), ", ", tags$code("GSE-92742"), " matches ", tags$code("GSE92742"), "). You do not need to enter an exact or complete value."),
        tags$p(
          style = "font-size: 0.9em; color: #666; margin-top: 8px;",
          tags$b("Tip:"), " Leave all filter values empty to download all datasets (unfiltered). Use multiple AND filters to narrow results by intersecting conditions (e.g. Cell Line = ", tags$code("MCF7"), " AND Data Type = ", tags$code("Chem"), "). Use OR filters for alternative searches (e.g. Perturbagen = ", tags$code("Imatinib"), " OR Perturbagen = ", tags$code("Dasatinib"), ")."
        )
      ),

      # 筛选条件区域
      uiOutput(ns("filter_rows")),

      # 添加按钮
      div(style = "margin-top: 8px;",
        actionButton(ns("add_filter"), " Add Filter", icon = icon("plus"),
          class = "add-filter-btn",
          style = "background: #3a7bd5; color: white; border: none; border-radius: 6px; padding: 8px 18px;")
      ),

      # 结果摘要 & 操作按钮
      div(class = "filter-summary",
        div(class = "match-count",
          span("Matching datasets: "),
          span(id = ns("match_count_text"), textOutput(ns("match_count"), inline = TRUE))
        ),
        div(class = "btn-group",
          actionButton(ns("preview_matches"), " Preview", icon = icon("search"),
            class = "preview-btn",
            style = "background: #17a2b8; color: white; border: none; border-radius: 6px; padding: 10px 22px; font-size: 14px; font-weight: 600;"),
          actionButton(ns("trigger_download_zip"), " Download ZIP", icon = icon("file-archive"),
            class = "download-zip-btn",
            style = "background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%); color: white; border: none; border-radius: 6px; padding: 10px 22px; font-size: 14px; font-weight: 600; box-shadow: 0 2px 8px rgba(40,167,69,0.35);")
        )
      )
    )
  )
}


DownloadServer <- function(
    id,
    analysis_type_OV = "Download",
    help_id = "help-download.html"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Register static file path for dataset CSV downloads
    if (!"dataset_csv" %in% names(shiny::resourcePaths())) {
      shiny::addResourcePath("dataset_csv", normalizePath("rdata_columns_csv"))
    }

    # ================== Path config ==================
    data_dir <- "rdata_columns_csv"
    csv_dir  <- "dataset_csv"
    meta_fst_dir <- "signature_by_gene_fst"
    accession_map_file <- "Accession映射/Accession_merged_all最终.txt"

    # ================== Lazy-load Accession mapping table ==================
    load_accession_map <- function() {
      if (!file.exists(accession_map_file)) return(NULL)
      dt <- fread(accession_map_file, sep = "\t", header = TRUE, 
                  na.strings = c("NA", "\"\""), quote = "\"",
                  colClasses = list(character = "PMID"))
      dt <- dt[, .(Accession, PMID, Journal)]
      dt <- dt[!is.na(Accession) & Accession != ""]
      dt <- unique(dt, by = "Accession")
      dt
    }

    search_dirs <- unique(c(".", data_dir, csv_dir, meta_fst_dir))

    # ================== Metadata files (always use pre-standardized fst from meta_fst_dir) ==================
    chem_meta_files <- file.path(meta_fst_dir, c(
      "CIGSMetadata.fst",
      "CIGSData2Metadata.fst",
      "HerbDataMetadata.fst",
      "LINCS_2020_Metadata.fst",
      "LINCS_L1000_Metadata.fst"
    ))

    gen_meta_files <- file.path(meta_fst_dir, c(
      "GuoziDataMetadata.fst",
      "PerturbAtlasMetadata.fst",
      "LINCS_2020_KD_Metadata.fst",
      "LINCS_2020_KO_Metadata.fst",
      "LINCS_L1000_KD_Metadata.fst",
      "LINCS_L1000_KO_Metadata.fst"
    ))

    # ================== Utility functions ==================
    resolve_file <- function(filename) {
      if (is.na(filename) || filename == "") return(NA_character_)
      candidates <- unique(c(filename, file.path(search_dirs, filename)))
      candidates <- candidates[file.exists(candidates)]
      if (length(candidates) == 0) return(NA_character_)
      candidates[1]
    }

    load_metadata_files <- function(files) {
      dt_list <- list()
      for (f in files) {
        real_f <- resolve_file(f)
        if (is.na(real_f)) next
        dt <- tryCatch(
          read_fst(real_f, as.data.table = TRUE),
          error = function(err) NULL
        )
        if (is.null(dt) || nrow(dt) == 0) next
        
        meta_file <- basename(real_f)
        dt[, `.source_meta_file` := meta_file]
        
        dt_list[[length(dt_list) + 1]] <- dt
      }
      if (length(dt_list) == 0) return(NULL)
      rbindlist(dt_list, fill = TRUE, use.names = TRUE)
    }

    # ================== Lazy load container ==================
    data_env <- new.env()
    data_env$chem_loaded <- FALSE
    data_env$gen_loaded  <- FALSE
    data_env$chem_df <- NULL
    data_env$gen_df  <- NULL

    # Tissue standardization uses the global standardize_tissue() from global.R

    # ================== Load Chemical Metadata ==================
    load_chem_data <- function() {
      if (data_env$chem_loaded) return()
      df <- load_metadata_files(chem_meta_files)
      if (!is.null(df)) {
        # Merge PMID/Journal from accession mapping table
        acc_map <- load_accession_map()
        if (!is.null(acc_map) && "Accession" %in% names(df)) {
          df[, PMID := NULL]
          df <- merge(df, acc_map, by = "Accession", all.x = TRUE, sort = FALSE)
          pmid_direct <- grepl("^PMID:", df$Accession) & (is.na(df$PMID) | df$PMID == "")
          df[pmid_direct, PMID := sub("^PMID: ?", "", Accession)]
          # Fill known Journal for PMIDs missing journal info
          df[PMID == "29195078" & (is.na(Journal) | Journal == ""), Journal := "Cell"]
        }
        # Standardize Tissue (Cell is pre-standardized in fst)
        df <- standardize_tissue(df)

        if (!("Dataset" %in% names(df))) {
          df[, Dataset := paste0("dataset_", .I)]
        }
        if (!("GEM_ID" %in% names(df))) {
          df[, GEM_ID := Dataset]
        } else {
          missing <- is.na(df$GEM_ID) | df$GEM_ID == ""
          df$GEM_ID[missing] <- df$Dataset[missing]
        }
        # Filter out rows with empty Time first
        if ("Time" %in% names(df)) {
          df <- df[!is.na(Time) & as.character(Time) != ""]
        }
        # Use pre-computed PSC IDs from FST files (same as Browse.R — authoritative across all modules)
        # No dynamic renumbering — GEM_ID is authoritative
        df[, `.gem_id_raw` := GEM_ID]
        # Sort by PSC ID (natural order reflects completeness + Accession round-robin)
        setorderv(df, ".gem_id_raw")
        df[, ".source_meta_file" := NULL]
        data_env$chem_df <- df
      }
      data_env$chem_loaded <- TRUE
    }

    # ================== Load Genetic Metadata ==================
    load_gen_data <- function() {
      if (data_env$gen_loaded) return()
      df <- load_metadata_files(gen_meta_files)
      if (!is.null(df)) {
        # Remove GuoziData metadata entries whose signature data file does not exist in GSEA/Genetic/
        guozi_idx <- which(df$.source_meta_file == "GuoziDataMetadata.fst")
        if (length(guozi_idx) > 0) {
          existing_datasets <- sub("\\.fst$", "", list.files(file.path("GSEA", "Genetic"), pattern = "\\.fst$"))
          guozi_datasets <- df$Dataset[guozi_idx]
          keep_vec <- guozi_datasets %in% existing_datasets
          remove_rows <- guozi_idx[!keep_vec]
          if (length(remove_rows) > 0) {
            message(sprintf("[Download] Filtered out %d GuoziData entries with missing signature data", length(remove_rows)))
            df <- df[-remove_rows]
          }
        }
        
        # Remove "inhibit" method entries from global table
        if ("method" %in% names(df)) {
          inhibit_count <- sum(df$method == "inhibit", na.rm = TRUE)
          if (inhibit_count > 0) {
            message(sprintf("[Download] Filtered out %d 'inhibit' method entries", inhibit_count))
            df <- df[!(method == "inhibit" & !is.na(method))]
          }
        }
        df[, ".source_meta_file" := NULL]
        
        # Merge PMID/Journal from accession mapping table
        acc_map <- load_accession_map()
        if (!is.null(acc_map) && "Accession" %in% names(df)) {
          df[, PMID := NULL]
          df <- merge(df, acc_map, by = "Accession", all.x = TRUE, sort = FALSE)
          pmid_direct <- grepl("^PMID:", df$Accession) & (is.na(df$PMID) | df$PMID == "")
          df[pmid_direct, PMID := sub("^PMID: ?", "", Accession)]
          # Fill known Journal for PMIDs missing journal info
          df[PMID == "29195078" & (is.na(Journal) | Journal == ""), Journal := "Cell"]
        }
        # Standardize Tissue (Cell is pre-standardized in fst)
        df <- standardize_tissue(df)

        if (!("Dataset" %in% names(df))) {
          df[, Dataset := paste0("dataset_", .I)]
        }
        if (!("GEM_ID" %in% names(df))) {
          df[, GEM_ID := Dataset]
        } else {
          missing <- is.na(df$GEM_ID) | df$GEM_ID == ""
          df$GEM_ID[missing] <- df$Dataset[missing]
        }
        df[, `.gem_id_raw` := GEM_ID]
        # Sort by GEM_ID (PSG00001, PSG00002, ...)
        setorderv(df, ".gem_id_raw")
        data_env$gen_df <- df
      }
      data_env$gen_loaded <- TRUE
    }

    # ================== Default load on startup ==================
    # 改为懒加载：仅在用户切换到 Download 对应子 tab 时才加载数据
    # 避免 app 启动预加载时同步读取全部 11 个 metadata fst 文件（109 万行）

    # ================== Load on tab switch ==================
    observeEvent(input$tabs, {
      if (input$tabs == "Chemical Perturbagens") load_chem_data()
      if (input$tabs == "Genetic Perturbagens")    load_gen_data()
    }, ignoreInit = FALSE)

    # ================== Helper: Sort (completeness priority + first-page Accession diversity) ==================
    # Identical to Browse.R's reorder_for_display
    reorder_for_display <- function(df, gse_first = FALSE) {
      if (is.null(df) || nrow(df) == 0) return(df)
      # Exclude internal/hidden columns from completeness check
      internal_cols <- c(".source_data_file", "Download", ".gem_id_raw", "Dataset")
      chk_cols <- setdiff(names(df), internal_cols)
      if (length(chk_cols) == 0) return(df)
      # Remove rows where Perturbagen contains Chinese characters
      if ("Perturbagen" %in% names(df)) {
        df <- df[!grepl("[\u4e00-\u9fff]", Perturbagen)]
      }
      # Count NA / empty / "NA" per row → fewer = more complete
      df[, `.__na_count` := Reduce(`+`, lapply(.SD, function(x)
        is.na(x) | as.character(x) == "" | as.character(x) == "NA")),
        .SDcols = chk_cols]
      # GSE priority flag (TRUE = not GSE, pushed to end)
      if (gse_first && "Accession" %in% names(df)) {
        df[, `.__not_gse` := !grepl("^GSE", Accession)]
      }
      # Sort: within each Accession keep most-complete first; then global round-robin by Accession
      if ("Accession" %in% names(df) && nrow(df) > 0) {
        setorderv(df, c("Accession", ".__na_count"))
        df[, `.__acc_rank` := seq_len(.N), by = Accession]
        sort_cols <- if (gse_first) c(".__not_gse", ".__acc_rank", ".__na_count") else c(".__acc_rank", ".__na_count")
        setorderv(df, sort_cols)
        df[, `.__acc_rank` := NULL]
        if (gse_first) df[, `.__not_gse` := NULL]
      } else {
        if (gse_first) setorderv(df, c(".__not_gse", ".__na_count"))
        else setorderv(df, ".__na_count")
        if (gse_first) df[, `.__not_gse` := NULL]
      }
      df[, `.__na_count` := NULL]
      df
    }

    # ================== Helper: Add download button column ==================
    add_download_btn <- function(df) {
      if (!nrow(df) || !("Dataset" %in% names(df))) return(df)
      gem_col <- if ("GEM_ID" %in% names(df)) "GEM_ID" else "Dataset"
      df[, Download := sprintf(
        '<button class="btn btn-sm btn-success dl-btn" data-ds="%s" data-gem="%s"><i class="fa fa-download"></i> Download</button>',
        gsub('"', '&quot;', as.character(Dataset), fixed = TRUE),
        gsub('"', '&quot;', as.character(.gem_id_raw), fixed = TRUE)
      )]
      df
    }

    # ================== DT options ==================
    # Sanitize encoding: replace invalid byte sequences with hex, avoiding tolower() errors
    sanitize_encoding <- function(df) {
      for (col in names(df)) {
        if (is.character(df[[col]])) {
          set(df, j = col, value = iconv(as.character(df[[col]]), from = "UTF-8", to = "UTF-8", sub = "byte"))
        }
      }
      df
    }
    
    dt_meta_options <- function(df, type = "chemical") {
      if (type == "genetic") {
        hidden_cols <- which(names(df) %in% c(".Dataset", ".gem_id_raw", "Type",
                                               "pb_ENSEMBL", "Perturbagen", "Dose", "Time")) - 1
      } else {
        hidden_cols <- which(names(df) %in% c(".Dataset", ".gem_id_raw", "Type",
                                               "pb_ENSEMBL", "pbgene", "gene_biotype", "method")) - 1
      }
      list(
        pageLength = 15,
        scrollX = TRUE,
        search = list(regex = FALSE),  # plain-text search to avoid regex errors on HTML columns
        columnDefs = list(
          list(visible = FALSE, targets = hidden_cols),
          dt_dash_render()
        )
      )
    }

    dt_data_options <- function(df, type = "chemical") {
      if (type == "genetic") {
        hidden_cols <- which(names(df) %in% c(".Dataset", ".gem_id_raw", "Type",
                                               "pb_ENSEMBL", "Perturbagen", "Dose", "Time")) - 1
      } else {
        hidden_cols <- which(names(df) %in% c(".Dataset", ".gem_id_raw", "Type",
                                               "pb_ENSEMBL", "pbgene", "gene_biotype", "method")) - 1
      }
      list(
        pageLength = 15,
        scrollX = TRUE,
        search = list(regex = FALSE),  # plain-text search to avoid regex errors on HTML columns
        columnDefs = list(
          list(visible = FALSE, targets = hidden_cols),
          dt_dash_render()
        )
      )
    }

    # ================== Helper: HTML-escape all string columns except specified ==================
    escape_html_except <- function(df, keep_cols = c("Download")) {
      str_cols <- names(df)[sapply(df, is.character)]
      str_cols <- setdiff(str_cols, keep_cols)
      if (length(str_cols) == 0) return(df)
      for (col in str_cols) {
        vals <- as.character(df[[col]])
        # Fix encoding to avoid locale errors, then HTML-escape
        vals <- iconv(vals, to = "UTF-8", sub = "")
        vals <- gsub("&", "&amp;", vals, fixed = TRUE, useBytes = TRUE)
        vals <- gsub("<", "&lt;", vals, fixed = TRUE, useBytes = TRUE)
        vals <- gsub(">", "&gt;", vals, fixed = TRUE, useBytes = TRUE)
        set(df, j = col, value = vals)
      }
      df
    }

    # ================== Chemical Metadata table (top) ==================
    output$chem_meta_table <- DT::renderDT(server = TRUE, {
      req(data_env$chem_df)
      df <- sanitize_encoding(copy(data_env$chem_df))
      if ("GEM_ID" %in% names(df)) setcolorder(df, c("GEM_ID", setdiff(names(df), "GEM_ID")))
      # Rename merge key "Dataset" to avoid conflict and "GEM_ID" to "Dataset" for display
      if ("Dataset" %in% names(df)) setnames(df, "Dataset", ".Dataset")
      if ("GEM_ID" %in% names(df))  setnames(df, "GEM_ID", "Dataset")
      # Rename Perturbagen to PbChem for display
      if ("Perturbagen" %in% names(df)) setnames(df, "Perturbagen", "PbChem")
      # Dataset now contains stored PSCxxxxx/PSGxxxxx GEM_ID
      num_cols <- names(df)[sapply(df, is.numeric)]
      df <- replace_empty_with_dash(df)
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        escape = FALSE,
        options = dt_meta_options(df, "chemical")
      )
      if (length(num_cols) > 0) dt <- dt %>% DT::formatSignif(columns = num_cols, digits = 4)
      dt
    })

    # ================== Chemical Data table (bottom, with download buttons) ==================
    output$chem_data_table <- DT::renderDT(server = TRUE, {
      req(data_env$chem_df)
      df <- sanitize_encoding(copy(data_env$chem_df))
      df <- add_download_btn(df)
      df <- escape_html_except(df)
      if ("GEM_ID" %in% names(df)) setcolorder(df, c("GEM_ID", "Download", setdiff(names(df), c("GEM_ID", "Download"))))
      # Rename merge key "Dataset" to avoid conflict and "GEM_ID" to "Dataset" for display
      if ("Dataset" %in% names(df)) setnames(df, "Dataset", ".Dataset")
      if ("GEM_ID" %in% names(df))  setnames(df, "GEM_ID", "Dataset")
      # Rename Perturbagen to PbChem for display
      if ("Perturbagen" %in% names(df)) setnames(df, "Perturbagen", "PbChem")
      # Dataset now contains stored PSCxxxxx/PSGxxxxx GEM_ID
      num_cols <- names(df)[sapply(df, is.numeric)]
      df <- replace_empty_with_dash(df, exclude_cols = "Download")
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        escape = FALSE,
        options = dt_data_options(df, "chemical")
      )
      if (length(num_cols) > 0) dt <- dt %>% DT::formatSignif(columns = num_cols, digits = 4)
      dt
    })

    # ================== Genetic Metadata table (top) ==================
    output$gen_meta_table <- DT::renderDT(server = TRUE, {
      req(data_env$gen_df)
      df <- sanitize_encoding(copy(data_env$gen_df))
      if ("GEM_ID" %in% names(df)) setcolorder(df, c("GEM_ID", setdiff(names(df), "GEM_ID")))
      # Rename merge key "Dataset" to avoid conflict and "GEM_ID" to "Dataset" for display
      if ("Dataset" %in% names(df)) setnames(df, "Dataset", ".Dataset")
      if ("GEM_ID" %in% names(df))  setnames(df, "GEM_ID", "Dataset")
      # Capitalize first letter of displayed column names
      cap_cols <- intersect(c("pbgene", "method"), names(df))
      for (cc in cap_cols) setnames(df, cc, paste0(toupper(substr(cc, 1, 1)), substr(cc, 2, nchar(cc))))
      # Dataset now contains stored PSCxxxxx/PSGxxxxx GEM_ID
      num_cols <- names(df)[sapply(df, is.numeric)]
      df <- replace_empty_with_dash(df)
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        escape = FALSE,
        options = dt_meta_options(df, "genetic")
      )
      if (length(num_cols) > 0) dt <- dt %>% DT::formatSignif(columns = num_cols, digits = 4)
      dt
    })

    # ================== Genetic Data table (bottom, with download buttons) ==================
    output$gen_data_table <- DT::renderDT(server = TRUE, {
      req(data_env$gen_df)
      df <- sanitize_encoding(copy(data_env$gen_df))
      df <- add_download_btn(df)
      df <- escape_html_except(df)
      if ("GEM_ID" %in% names(df)) setcolorder(df, c("GEM_ID", "Download", setdiff(names(df), c("GEM_ID", "Download"))))
      # Rename merge key "Dataset" to avoid conflict and "GEM_ID" to "Dataset" for display
      if ("Dataset" %in% names(df)) setnames(df, "Dataset", ".Dataset")
      if ("GEM_ID" %in% names(df))  setnames(df, "GEM_ID", "Dataset")
      # Capitalize first letter of displayed column names
      cap_cols <- intersect(c("pbgene", "method"), names(df))
      for (cc in cap_cols) setnames(df, cc, paste0(toupper(substr(cc, 1, 1)), substr(cc, 2, nchar(cc))))
      # Dataset now contains stored PSCxxxxx/PSGxxxxx GEM_ID
      num_cols <- names(df)[sapply(df, is.numeric)]
      df <- replace_empty_with_dash(df, exclude_cols = "Download")
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        escape = FALSE,
        options = dt_data_options(df, "genetic")
      )
      if (length(num_cols) > 0) dt <- dt %>% DT::formatSignif(columns = num_cols, digits = 4)
      dt
    })

    # ===== Table CSV downloads (1:1 export for data tables) =====
    output$download_chem_data <- downloadHandler(
      filename = function() paste0("chemical_data_", Sys.Date(), ".csv"),
      content = function(file) {
        req(data_env$chem_df)
        df <- copy(data_env$chem_df)
        if ("Perturbagen" %in% names(df)) setnames(df, "Perturbagen", "PbChem")
        drop <- intersect(c("Download", ".gem_id_raw", ".source_meta_file",
                            ".source_data_file", "perturb_id", "Dataset", "Type",
                            "pbgene", "pb_ENSEMBL", "gene_biotype", "method"), names(df))
        if (length(drop) > 0) df[, (drop) := NULL]
        dots <- grep("^\\.", names(df), value = TRUE)
        if (length(dots) > 0) df[, (dots) := NULL]
        if ("GEM_ID" %in% names(df)) setnames(df, "GEM_ID", "Dataset")
        strip_html(df)
        fwrite(df, file)
      }
    )
    
    output$download_gen_data <- downloadHandler(
      filename = function() paste0("genetic_data_", Sys.Date(), ".csv"),
      content = function(file) {
        req(data_env$gen_df)
        df <- copy(data_env$gen_df)
        cap_cols <- intersect(c("pbgene", "method"), names(df))
        for (cc in cap_cols) setnames(df, cc, paste0(toupper(substr(cc, 1, 1)), substr(cc, 2, nchar(cc))))
        drop <- intersect(c("Download", ".gem_id_raw", ".source_meta_file",
                            ".source_data_file", "perturb_id", "Dataset", "Type",
                            "Perturbagen", "Dose", "Time"), names(df))
        if (length(drop) > 0) df[, (drop) := NULL]
        dots <- grep("^\\.", names(df), value = TRUE)
        if (length(dots) > 0) df[, (dots) := NULL]
        if ("GEM_ID" %in% names(df)) setnames(df, "GEM_ID", "Dataset")
        strip_html(df)
        fwrite(df, file)
      }
    )

    # ================== Batch download Metadata CSV ==================
    output$download_chem_meta <- downloadHandler(
      filename = function() {
        paste0("Chemical_Metadata_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(data_env$chem_df)
        out <- copy(data_env$chem_df)
        # Remove internal columns
        drop <- intersect(c(".gem_id_raw", "Dataset", "perturb_id", "Type", "pb_ENSEMBL",
                             "pbgene", "gene_biotype", "method"), names(out))
        if (length(drop) > 0) out[, (drop) := NULL]
        # Rename GEM_ID to Dataset for CSV output
        if ("GEM_ID" %in% names(out)) setnames(out, "GEM_ID", "Dataset")
        # Rename Perturbagen to PbChem for CSV output
        if ("Perturbagen" %in% names(out)) setnames(out, "Perturbagen", "PbChem")
        strip_html(out)
        fwrite(out, file = file, sep = ",", quote = TRUE, na = "NA")
      }
    )

    output$download_gen_meta <- downloadHandler(
      filename = function() {
        paste0("Genetic_Metadata_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(data_env$gen_df)
        out <- copy(data_env$gen_df)
        # Remove internal columns
        drop <- intersect(c(".gem_id_raw", "Dataset", "perturb_id", "Type", "pb_ENSEMBL",
                             "Perturbagen", "Dose", "Time"), names(out))
        if (length(drop) > 0) out[, (drop) := NULL]
        # Rename GEM_ID to Dataset for CSV output
        if ("GEM_ID" %in% names(out)) setnames(out, "GEM_ID", "Dataset")
        # Capitalize first letter of column names for CSV output
        cap_cols <- intersect(c("pbgene", "method"), names(out))
        for (cc in cap_cols) setnames(out, cc, paste0(toupper(substr(cc, 1, 1)), substr(cc, 2, nchar(cc))))
        strip_html(out)
        fwrite(out, file = file, sep = ",", quote = TRUE, na = "NA")
      }
    )

    # ================== Single dataset dynamic download (Blob method, bypasses downloadLink programmatic click failure) ==================
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

    # ================================================================
    # ================== Filter & ZIP Download Logic ==================
    # ================================================================

    # ================== 字段配置 ==================
    FILTER_FIELDS <- list(
      "Type"             = list(col = "Type",             label = "Data Type"),
      "Cell Line"        = list(col = "Cell",             label = "Cell Line"),
      "Tissue"           = list(col = "Tissue",           label = "Tissue"),
      "Perturbagen"      = list(col = "Perturbagen",      label = "Pbchem"),
      "Gene"             = list(col = "pbgene",           label = "Pbgene"),
      "Accession"        = list(col = "Accession",        label = "Accession")
    )

    LOGICS <- c("AND", "OR")

    # ================== 状态 ==================
    filter_count <- reactiveVal(0)
    filters <- reactiveVal(list())

    # ================== 动态渲染 filter rows ==================
    FILTER_EXAMPLES <- list(
      "Type"        = "e.g. Chem",
      "Cell Line"   = "e.g. MCF7",
      "Tissue"      = "e.g. Liver",
      "Perturbagen" = "e.g. Imatinib",
      "Gene"        = "e.g. CDK4",
      "Accession"   = "e.g. GSE92742"
    )

    output$filter_rows <- renderUI({
      n <- filter_count()
      if (n == 0) {
        return(tags$p("No filters added yet. Click 'Add Filter' to start.",
                      style = "color: #888; font-style: italic;"))
      }
      rows <- lapply(seq_len(n), function(i) {
        f <- filters()[[i]]
        # Row 1: no logic operator; Rows 2+: AND/OR between field and value
        logic_ui <- if (i == 1) {
          NULL
        } else {
          selectInput(
            inputId = ns(paste0("filter_logic_", i)),
            label = NULL,
            choices = setNames(LOGICS, LOGICS),
            selected = f$logic,
            width = "80px"
          )
        }
        current_field <- f$field
        placeholder_text <- if (is.null(current_field) || !current_field %in% names(FILTER_EXAMPLES)) {
          "Enter keyword..."
        } else {
          FILTER_EXAMPLES[[current_field]]
        }
        tags$div(class = "filter-row",
          selectInput(
            inputId = ns(paste0("filter_field_", i)),
            label = NULL,
            choices = setNames(names(FILTER_FIELDS), sapply(FILTER_FIELDS, `[[`, "label")),
            selected = f$field,
            width = "140px"
          ),
          logic_ui,
          textInput(
            inputId = ns(paste0("filter_val_", i)),
            label = NULL,
            value = f$value,
            placeholder = placeholder_text,
            width = "200px"
          ),
          tags$button(class = "remove-filter-btn", `data-idx` = i, HTML("&times;"))
        )
      })
      do.call(tagList, rows)
    })

    # ================== 保存当前 UI 状态到 filters ==================
    save_current_filters <- function() {
      n <- filter_count()
      if (n == 0) return()
      current <- filters()
      for (i in seq_len(n)) {
        field_input <- input[[paste0("filter_field_", i)]]
        val_input   <- input[[paste0("filter_val_", i)]]
        if (!is.null(field_input)) current[[i]]$field <- field_input
        if (!is.null(val_input))   current[[i]]$value <- trimws(val_input)
        if (i > 1) {
          logic_input <- input[[paste0("filter_logic_", i)]]
          if (!is.null(logic_input) && logic_input %in% c("AND", "OR"))
            current[[i]]$logic <- logic_input
        }
      }
      filters(current)
    }

    # ================== 添加筛选条件 ==================
    observeEvent(input$add_filter, {
      save_current_filters()                          # 先保存当前已填内容
      n <- filter_count() + 1
      filter_count(n)
      current <- filters()
      current[[n]] <- list(field = names(FILTER_FIELDS)[1], logic = "AND", value = "")
      filters(current)
    }, ignoreInit = TRUE)

    # ================== 删除筛选条件 ==================
    observeEvent(input$remove_filter, {
      idx <- as.integer(input$remove_filter$idx)
      current <- filters()
      if (idx >= 1 && idx <= length(current)) {
        current[[idx]] <- NULL
        filters(current)
        filter_count(max(0, filter_count() - 1))
      }
    }, ignoreInit = TRUE)

    # ================== 切换字段时同步 placeholder ==================
    observe({
      n <- filter_count()
      if (n == 0) return()
      # Touch each filter_field_* input to register reactive dependency
      for (i in seq_len(n)) {
        input[[paste0("filter_field_", i)]]
      }
      save_current_filters()
    })

    # ================== 收集当前所有 filter 的值 ==================
    collect_filters <- function() {
      n <- filter_count()
      if (n == 0) return(list())
      result <- list()
      for (i in seq_len(n)) {
        field_input <- input[[paste0("filter_field_", i)]]
        val_input   <- input[[paste0("filter_val_", i)]]

        if (is.null(field_input) || is.null(val_input) || trimws(val_input) == "") next

        # 第一行没有 logic 选择器，默认 AND
        logic_input <- if (i == 1) "AND" else input[[paste0("filter_logic_", i)]]
        if (is.null(logic_input) || !logic_input %in% c("AND", "OR")) logic_input <- "AND"

        result[[length(result) + 1]] <- list(
          field = field_input,
          logic = logic_input,
          value = trimws(val_input)
        )
      }
      result
    }

    # ================== 加载全部 metadata（供 filter 使用） ==================
    filter_meta_cache <- reactiveVal(NULL)

    load_filter_metadata <- function() {
      cached <- filter_meta_cache()
      if (!is.null(cached)) return(cached)

      all_meta_files <- c(
        # Chemical
        "CIGSMetadata.fst", "CIGSData2Metadata.fst", "HerbDataMetadata.fst",
        "LINCS_2020_Metadata.fst", "LINCS_L1000_Metadata.fst",
        # Genetic
        "GuoziDataMetadata.fst", "PerturbAtlasMetadata.fst",
        "LINCS_2020_KD_Metadata.fst", "LINCS_2020_KO_Metadata.fst",
        "LINCS_L1000_KD_Metadata.fst", "LINCS_L1000_KO_Metadata.fst"
      )

      dt_list <- list()
      for (f in all_meta_files) {
        real_f <- resolve_file(f)
        if (is.na(real_f)) next
        dt <- tryCatch(
          read_fst(real_f, as.data.table = TRUE),
          error = function(e) NULL
        )
        if (is.null(dt) || nrow(dt) == 0) next
        dt_list[[length(dt_list) + 1]] <- dt
      }

      if (length(dt_list) == 0) return(NULL)
      df <- rbindlist(dt_list, fill = TRUE, use.names = TRUE)

      # Standardize Tissue (Cell is pre-standardized in fst)
      df <- standardize_tissue(df)

      # 确保关键列存在
      if (!("Dataset" %in% names(df))) df[, Dataset := paste0("dataset_", .I)]
      if (!("GEM_ID" %in% names(df))) {
        df[, GEM_ID := Dataset]
      } else {
        missing <- is.na(df$GEM_ID) | df$GEM_ID == ""
        df$GEM_ID[missing] <- df$Dataset[missing]
      }

      filter_meta_cache(df)
      df
    }

    # ================== 执行过滤 ==================
    # 每个 filter 始终用 contains 匹配（忽略大小写 + 特殊字符）
    # 行间用 AND/OR 组合
    apply_filters <- function(df, filter_list) {
      if (length(filter_list) == 0) return(df)

      strip_special <- function(x) {
        x <- as.character(x)
        # 清除无法转换为 UTF-8 的字节
        x <- tryCatch(iconv(x, to = "UTF-8", sub = ""),
                      error = function(e) x)
        # 如果仍然无效，回退到 ASCII
        x <- ifelse(is.na(x) | !nzchar(x, keepNA = TRUE),
                    x,
                    ifelse(!validUTF8(x),
                           tryCatch(iconv(x, to = "ASCII", sub = ""),
                                    error = function(e) ""),
                           x))
        tryCatch(gsub("[^A-Za-z0-9]", "", x),
                 error = function(e) "")
      }

      combined <- rep(TRUE, nrow(df))

      for (i in seq_along(filter_list)) {
        f <- filter_list[[i]]
        col_name <- FILTER_FIELDS[[f$field]]$col
        if (!(col_name %in% names(df))) next

        col_raw <- as.character(df[[col_name]])
        val_raw <- f$value

        matched <- grepl(tolower(strip_special(val_raw)), tolower(strip_special(col_raw)),
                         fixed = TRUE)

        logic <- f$logic
        if (i == 1 || logic == "AND") {
          combined <- combined & matched
        } else {
          combined <- combined | matched
        }
      }

      df[combined]
    }

    # ================== Preview：显示匹配数量 ==================
    match_count_val <- reactiveVal(0)

    observeEvent(input$preview_matches, {
      session$sendCustomMessage("preview_loading", list(loading = TRUE))
      on.exit(session$sendCustomMessage("preview_loading", list(loading = FALSE)))

      df <- load_filter_metadata()
      if (is.null(df)) {
        match_count_val(0)
        showNotification("Failed to load metadata.", type = "error")
        return()
      }

      flt <- collect_filters()
      if (length(flt) == 0) {
        match_count_val(nrow(df))
        showNotification(sprintf("No filters applied. Total datasets: %s", format(nrow(df), big.mark = ",")), type = "info")
        return()
      }

      filtered <- apply_filters(df, flt)
      match_count_val(nrow(filtered))
      showNotification(sprintf("Matched %s datasets.", format(nrow(filtered), big.mark = ",")), type = "message")
    }, ignoreInit = TRUE)

    output$match_count <- renderText({
      format(match_count_val(), big.mark = ",")
    })

    # ================== 下载 ZIP ==================
    MAX_DOWNLOAD <- 5000

    observeEvent(input$trigger_download_zip, {
      df <- load_filter_metadata()
      if (is.null(df)) {
        showNotification("Failed to load metadata.", type = "error")
        return()
      }

      flt <- collect_filters()
      filtered <- apply_filters(df, flt)

      n_total <- nrow(filtered)
      if (n_total == 0) {
        showNotification("No datasets match the filter conditions.", type = "warning")
        return()
      }

      if (n_total > MAX_DOWNLOAD) {
        showNotification(
          sprintf("Too many matches (%s). Please narrow your filters. Max: %s per download.",
                  format(n_total, big.mark = ","), format(MAX_DOWNLOAD, big.mark = ",")),
          type = "error", duration = 8
        )
        return()
      }

      # 显示进度
      showNotification(
        sprintf("Preparing %s datasets for download... This may take a while.", format(n_total, big.mark = ",")),
        type = "message", duration = 5
      )

      # 创建临时目录 & 打包
      tmp_dir <- tempfile("filter_dl_")
      dir.create(tmp_dir)

      withProgress(message = "Building ZIP archive", value = 0, max = n_total, {
        datasets <- filtered$Dataset
        gem_ids   <- if ("GEM_ID" %in% names(filtered)) filtered$GEM_ID else datasets

        success_count <- 0
        for (i in seq_len(n_total)) {
          incProgress(1 / n_total, detail = sprintf("Processing %d/%d", i, n_total))

          ds <- datasets[i]
          gem <- gem_ids[i]

          dt <- tryCatch(resolve_dataset_data(ds), error = function(e) NULL)
          if (is.null(dt) || nrow(dt) == 0) next

          # 只保留 gene + logfc 列
          gene_candidates <- c("Gene", "gene", "symbol", "SYMBOL", "gene_name", "GeneSymbol", "gene_id")
          fc_candidates   <- c("logFC", "LogFC", "log2FC", "Log2FC", "log2FoldChange", "fc", "value", "Log2 (FC_avg)")
          gene_col <- intersect(gene_candidates, names(dt))
          fc_col   <- intersect(fc_candidates, names(dt))

          keep_cols <- c(
            if (length(gene_col) > 0) gene_col[1] else NULL,
            if (length(fc_col) > 0) fc_col[1] else NULL
          )

          if (length(keep_cols) > 0) {
            dt <- dt[, ..keep_cols]
            setnames(dt, old = keep_cols, new = c("Gene", "LogFC")[1:length(keep_cols)])
          }

          safe_name <- gsub("[^A-Za-z0-9_.-]", "_", gem)
          csv_path <- file.path(tmp_dir, paste0(safe_name, ".csv"))
          fwrite(dt, file = csv_path, sep = ",", quote = TRUE, na = "NA")
          success_count <- success_count + 1
        }
      })

      if (success_count == 0) {
        unlink(tmp_dir, recursive = TRUE)
        showNotification("No valid data could be extracted for the matched datasets.", type = "error")
        return()
      }

      # 打包 ZIP
      zip_file <- tempfile("filter_zip_", fileext = ".zip")
      old_wd <- getwd()
      setwd(tmp_dir)
      zip::zip(zip_file, files = list.files(".", pattern = "\\.csv$"))
      setwd(old_wd)

      showNotification(
        sprintf("ZIP ready: %d CSV files packaged.", success_count),
        type = "message", duration = 3
      )

      # 触发浏览器下载
      zip_data <- readBin(zip_file, "raw", file.info(zip_file)$size)
      b64 <- base64enc::base64encode(zip_data)

      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      session$sendCustomMessage("trigger_zip_download", list(
        filename = paste0("PerturbSigDB_filtered_", timestamp, ".zip"),
        content = b64
      ))

      # 清理
      unlink(tmp_dir, recursive = TRUE)
      unlink(zip_file)
    }, ignoreInit = TRUE)

    cat("[DOWNLOAD] Module ready (with filter).\n")
  })
}
