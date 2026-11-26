# ============================================
# Gene Set Query Module for PerturbSigDB (Pathway Query)
# 使用 KEGG/Hallmark 通路，基于 GSQuery MWU 算法寻找影响最大的扰动
# ============================================

# =========================
# GMT File Configuration
# =========================
GMT_FILES <- list(
  KEGG                  = "c2.cp.kegg_legacy.v2024.1.Hs.symbols.gmt",
  Hallmark              = "h.all.v2022.1.Hs.symbols.gmt",
  `BioCarta`       = "通路/BioCarta_2016.txt",
  `Elsevier Pathway`    = "通路/Elsevier_Pathway_Collection.txt",
  `HumanCyc`       = "通路/HumanCyc_2016.txt",
  `Panther`        = "通路/Panther_2016.txt",
  `Reactome`       = "通路/Reactome_Pathways_2024.txt",
  `WikiPathways`   = "通路/WikiPathways_2024_Human.txt"
)

# =========================
# GMT Parsing
# =========================
parse_gmt <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  pathways <- list()
  for (line in lines) {
    parts <- strsplit(line, "\t")[[1]]
    name <- parts[1]
    genes <- unique(toupper(parts[-(1:2)]))
    genes <- genes[nzchar(genes)]
    if (length(genes) >= 3) {
      pathways[[name]] <- genes
    }
  }
  pathways
}

load_all_pathways <- function() {
  all_pw <- list()
  for (db in names(GMT_FILES)) {
    fp <- GMT_FILES[[db]]
    if (file.exists(fp)) {
      pw <- parse_gmt(fp)
      all_pw[[db]] <- pw
      cat("[PWQuery] Loaded", length(pw), "pathways from", db, "\n")
    } else {
      cat("[PWQuery] GMT file not found:", fp, "\n")
    }
  }
  all_pw
}

# =========================
# UI Module
# =========================
PwqueryMUI <- function(id, title = "Gene Set Query") {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        .gsquery-box {
          background-color: #f0f6ff;
          border-radius: 12px;
          padding: 20px;
          margin-bottom: 20px;
        }
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
        @media (max-width: 768px) { .dataset-wide-modal { width: 98vw !important; max-width: 98vw !important; margin: 10px auto !important; } }
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
                          tags$h4(icon("project-diagram"), " Gene Set Modulator Query", class = "gsquery-title"),
                          tags$hr(),
                          p(HTML("<b>Gene Set Query</b> will find which perturbations in the database modulate your selected pathway.")),
                          p("Select a pathway database below — the analysis uses all genes in that pathway as query:"),
                          
                          # Database source selector + pathway selector (dependent)
                          fluidRow(
                            column(3,
                                   selectInput(ns("pathway_db"), "Database Source",
                                               choices = c(
                                                 "KEGG" = "KEGG",
                                                 "Hallmark" = "Hallmark",
                                                 "BioCarta" = "BioCarta",
                                                 "Elsevier Pathway" = "Elsevier Pathway",
                                                 "HumanCyc" = "HumanCyc",
                                                 "Panther" = "Panther",
                                                 "Reactome" = "Reactome",
                                                 "WikiPathways" = "WikiPathways"
                                               ),
                                               selected = "KEGG")
                            ),
                            column(5,
                                   selectizeInput(ns("pathway"), "Select Pathway",
                                                  choices = NULL,
                                                  options = list(
                                                    placeholder = "Start typing to search pathways...",
                                                    maxOptions = 5000
                                                  ))
                            ),
                            column(2,
                                   tags$div(style = "margin-top:25px;",
                                            tags$span(class = "text-muted", "Genes: "),
                                            tags$strong(textOutput(ns("gene_count"), inline = TRUE))
                                   )
                            ),
                            column(2,
                                   numericInput(ns("top_n"), "Top N Results",
                                                value = 500, min = 10, max = 5000, step = 10)
                            ),
                            column(12,
                                   column(3,
                                          radioButtons(ns("mode"), "Direction",
                                                       choices = c("Both" = "both", "Activate" = "activate", "Suppress" = "suppress"),
                                                       selected = "both", inline = TRUE)
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
                                     column(3,
                                            tags$div(style = "display: flex; align-items: center; gap: 0px; margin-top: 15px;",
                                                     checkboxInput(ns("fda_only"), "FDA Approved Only", value = FALSE)
                                            )
                                     )
                                   ),
                                   column(3),
                            )
                          )
                 )
          )
        ),
        tags$hr(style = "border-top: 1px dashed #ccc; margin-top: 20px; margin-bottom: 12px;"),
        p(HTML('<b>Or upload your own gene list</b>'),
          tags$span(' — ', icon('exclamation-triangle'), ' ',
                    tags$u('Uploaded genes take priority'),
                    ' over the selected pathway above.'),
          style = "font-size: 13px; color: #666; margin-bottom: 8px;"),
        fluidRow(
          column(6,
                 fileInput(ns("gene_file"), "Upload Gene File (one gene per line)",
                           accept = c(".csv", ".txt", ".tsv"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected")
          ),
          column(6,
                 div(style = "margin-bottom: 8px;",
                     downloadButton(ns("download_demo_gs"), "Download Demo File",
                                    class = "btn-sm btn-outline-primary",
                                    style = "margin-right: 8px;"),
                     actionButton(ns("fill_demo_gs"), "Demo",
                                  class = "btn btn-sm btn-outline-success",
                                  style = "border-radius: 4px; margin-right: 8px;"),
                     actionButton(ns("clear_upload_gs"), "Clear",
                                  class = "btn btn-sm btn-outline-danger",
                                  style = "border-radius: 4px;")
                 ),
                 textAreaInput(ns("gene_list"), NULL,
                               placeholder = "Paste genes here (one per line)\nExample:\nTP53\nEGFR\nMYC\nBRCA1",
                               rows = 8)
          )
        ),
        
        # ===== Priority Warning =====
        uiOutput(ns("priority_warning")),
        
        # ===== File vs Text Conflict Warning =====
        uiOutput(ns("file_text_conflict_warning")),
        
        # ===== Run Button =====
        fluidRow(
          column(12,
                 actionButton(ns("run_query"),
                              label = "Run Gene Set Query",
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
                     
                     div(class = "progress", style = "height: 22px; margin: 12px 0; background-color: #e9ecef; border-radius: 10px; overflow: hidden;",
                         div(id = ns("progress_bar"), class = "progress-bar progress-bar-striped progress-bar-animated",
                             role = "progressbar", 
                             style = "width: 0%; background-color: #87CEEB;",
                             "0%")
                     )
                 )
          )
        ),
        
        # ===== Results =====
        fluidRow(
          column(12,
                 div(id = ns("results_container"),
                     class = "result-section",
                     style = "display: none;",
                     
                     tags$h4(icon("list"), " Results: Top Perturbations Affecting This Pathway"),
                     
                     fluidRow(
                       column(3,
                              downloadButton(ns("download_results"), "Download CSV",
                                             class = "btn-success", style = "margin-bottom: 10px;")
                       ),
                       column(12,
                              uiOutput(ns("rank_plots_ui"))
                       ),
                       column(12,
                              tags$div(style = "margin-top:10px;",
                                       uiOutput(ns("table_tabs_ui"))
                              )
                       )
                     )
                 )
          )
        ),
        
        # ===== Explore Selected Dataset =====
        div(id = ns("explore_section_container"),
            tags$hr(style = "border-top: 2px solid #3a7bd5; margin: 24px 0 16px 0;"),
            tags$h4(icon("chart-bar"), " Explore Selected Dataset", class = "gsquery-title"),
            tags$p("Select a result dataset above and a pathway database to perform enrichment analysis on its differentially expressed genes."),
            
            fluidRow(
              column(4,
                     selectInput(ns("explore_dataset"), "Choose a result dataset:",
                                 choices = NULL, width = "100%")
              ),
              column(4,
                     selectInput(ns("explore_pw_db"), "Pathway Database",
                                 choices = names(GMT_FILES),
                                 selected = "Hallmark", width = "100%")
              ),
              column(4,
                     selectizeInput(ns("explore_pathway"), "Specific Pathway (GSEA Plot)",
                                    choices = NULL, width = "100%",
                                    options = list(placeholder = "Select a pathway for GSEA plot..."))
              )
            ),
            
            tags$br(),
            
            fluidRow(
              column(12,
                     conditionalPanel(
                       condition = sprintf("input['%s'] != '' && input['%s'] != ''", 
                                           ns("explore_dataset"), ns("explore_pw_db")),
                       
                       fluidRow(
                         column(6,
                                div(style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px;",
                                    tags$h5("Enrichment Dot Plot", style = "text-align:center; color:#2c5f8a;"),
                                    shinycssloaders::withSpinner(
                                      plotOutput(ns("enrich_dotplot"), height = "550px"),
                                      type = 4, color = "#3a7bd5"
                                    ),
                                    div(style = "text-align:center; margin-top:8px;",
                                        downloadButton(ns("download_enrich_dotplot"), "Download PDF", class = "btn-sm btn-info plot-download-btn")
                                    )
                                )
                         ),
                         column(6,
                                div(style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px;",
                                    tags$h5("Enrichment Bar Plot", style = "text-align:center; color:#2c5f8a;"),
                                    shinycssloaders::withSpinner(
                                      plotOutput(ns("enrich_barplot"), height = "550px"),
                                      type = 4, color = "#3a7bd5"
                                    ),
                                    div(style = "text-align:center; margin-top:8px;",
                                        downloadButton(ns("download_enrich_barplot"), "Download PDF", class = "btn-sm btn-info plot-download-btn")
                                    )
                                )
                         )
                       ),
                       
                       # GSEA Running Enrichment Plot for a selected specific pathway
                       conditionalPanel(
                         condition = sprintf("input['%s'] != ''", ns("explore_pathway")),
                         tags$br(),
                         tags$hr(style = "border-top: 1px dashed #ccc;"),
                         tags$h5("GSEA Running Enrichment Score", style = "color:#2c5f8a;font-weight:bold;"),
                         div(style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 15px;",
                             shinycssloaders::withSpinner(
                               plotOutput(ns("gsea_enrich_plot"), height = "500px"),
                               type = 4, color = "#3a7bd5"
                             ),
                             div(style = "text-align:center; margin-top:8px;",
                                 downloadButton(ns("download_gsea_plot"), "Download PDF", class = "btn-sm btn-info plot-download-btn")
                             )
                         )
                       ),
                       
                       tags$br(),
                       tags$h5("Enrichment Results Table", style = "color:#2c5f8a;font-weight:bold;"),
                       div(style = "text-align: left; margin-bottom: 5px;",
                           downloadButton(ns("download_enrich_table"), "CSV",
                                          class = "btn-sm btn-outline-secondary")),
                       div(style = "margin-top:5px;",
                           DT::DTOutput(ns("enrich_table"))
                       )
                     )
              )
            )
        )
      )
    )
  )
}

# =========================
# Server Module
# =========================
PwqueryMServer <- function(
    id, 
    analysis_type_OV = "Gene Set Query",
    help_id = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Register static file path for dataset CSV downloads (same as GQuery)
    if (!"dataset_csv" %in% names(shiny::resourcePaths())) {
      shiny::addResourcePath("dataset_csv", normalizePath("rdata_columns_csv"))
    }
    
    # ===== Constants =====
    GSEA_DIR <- "GSEA"
    
    cat("[PWQuery] Server initialized (Pathway Query mode)\n")
    
    # ===== Load GMT pathways =====
    all_pathways <- load_all_pathways()
    valid_gene_symbols <- unique(unlist(unlist(all_pathways, recursive = FALSE)))
    
    # ===== Load precomputed pathway cache =====
    if (!has_pathway_cache()) {
      init_pathway_cache("内置通路数据处理/results")
    }
    
    # Update pathway choices based on selected database source
    observeEvent(input$pathway_db, {
      db <- input$pathway_db
      if (is.null(db) || !db %in% names(all_pathways)) return()
      choices <- names(all_pathways[[db]])
      updateSelectizeInput(session, "pathway", 
                           choices = choices, 
                           selected = character(0),
                           server = TRUE)
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    # Show gene count when pathway changes
    output$gene_count <- renderText({
      db <- input$pathway_db
      pw <- input$pathway
      if (is.null(db) || is.null(pw) || !nzchar(pw)) return("--")
      if (db %in% names(all_pathways) && pw %in% names(all_pathways[[db]])) {
        return(as.character(length(all_pathways[[db]][[pw]])))
      }
      "--"
    })
    
    # ===== Reactive Values =====
    rv <- reactiveValues(
      results = NULL,
      query_genes = NULL,
      selected_pathway = NULL,
      running = FALSE,
      progress_pct = 0,
      progress_file = NULL,
      polling_id = 0
    )
    
    # Store modal plots for download
    modal_plot_store <- reactiveVal(NULL)
    gsea_table_store <- reactiveVal(NULL)
    
    # ===== Parse Gene List Input =====
    parse_gene_list <- function() {
      genes <- character(0)
      if (!is.null(input$gene_file) && file.exists(input$gene_file$datapath)) {
        tryCatch({
          dt <- fread(input$gene_file$datapath, header = FALSE)
          genes <- toupper(as.character(dt[[1]]))
        }, error = function(e) { cat("[PWQuery] File read error:", e$message, "\n") })
      }
      if (length(genes) == 0 && nzchar(input$gene_list)) {
        lines <- strsplit(trimws(input$gene_list), "\n")[[1]]
        lines <- lines[nzchar(lines)]
        genes <- toupper(trimws(lines))
      }
      genes <- genes[nzchar(genes)]
      genes <- unique(genes)
      if (length(genes) < 3) return(NULL)
      
      cat("[PWQuery] Parsed", length(genes), "gene symbols from upload\n")
      genes
    }
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
    output$download_demo_gs <- downloadHandler(
      filename = function() { "Gene Set Query Demo.txt" },
      content = function(file) {
        file.copy("Gene Set Query Demo.txt", file)
      }
    )
    
    # Auto-fill demo into text area
    observeEvent(input$fill_demo_gs, {
      demo_genes <- readLines("Gene Set Query Demo.txt")
      demo_genes <- demo_genes[demo_genes != ""]
      updateTextAreaInput(session, "gene_list",
                          value = paste(demo_genes, collapse = "\n"))
    })
    
    # Clear uploaded gene file and text area
    observeEvent(input$clear_upload_gs, {
      shinyjs::reset("gene_file")
      updateTextAreaInput(session, "gene_list", value = "")
    })
    
    # ===== Dynamic Priority Warning =====
    # When user has both a pathway selected AND genes uploaded/pasted,
    # show a prominent warning that the uploaded genes will take priority.
    has_uploaded_genes <- reactive({
      has <- FALSE
      if (!is.null(input$gene_file) && file.exists(input$gene_file$datapath)) has <- TRUE
      if (!has && nzchar(trimws(input$gene_list))) {
        lines <- strsplit(trimws(input$gene_list), "\n")[[1]]
        lines <- lines[nzchar(lines)]
        if (length(lines) >= 3) has <- TRUE
      }
      has
    })
    
    has_pathway_selected <- reactive({
      !is.null(input$pathway) && nzchar(input$pathway)
    })
    
    output$priority_warning <- renderUI({
      if (has_uploaded_genes() && has_pathway_selected()) {
        pw_info <- get_pathway_genes()
        ng <- 0
        if (!is.null(input$gene_file) && file.exists(input$gene_file$datapath)) {
          dt <- tryCatch(fread(input$gene_file$datapath, header = FALSE), error = function(e) NULL)
          if (!is.null(dt)) ng <- nrow(dt)
        }
        if (ng == 0 && nzchar(trimws(input$gene_list))) {
          lines <- strsplit(trimws(input$gene_list), "\n")[[1]]
          lines <- lines[nzchar(lines)]
          ng <- length(lines)
        }
        tags$div(
          style = "background: #fff3cd; border: 2px solid #ffc107; border-radius: 8px; padding: 12px 16px; margin: 10px 0;",
          tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            icon("exclamation-triangle", style = "color: #e6a400; font-size: 20px;"),
            tags$div(
              tags$strong("Gene list uploaded — pathway selection will be ignored.", style = "color: #856404;"),
              tags$br(),
              tags$span(
                style = "color: #856404; font-size: 13px;",
                "Your uploaded ", tags$b(paste(ng, "genes")), " will be used for the query. ",
                "The selected pathway (", tags$b(input$pathway), ") will NOT be used. ",
                "Clear the upload to query by pathway instead."
              )
            )
          )
        )
      }
    })
    
    # ===== File vs Text Conflict Warning =====
    # When both a file is uploaded AND text is pasted, warn that file takes priority.
    has_file_input <- reactive({
      !is.null(input$gene_file) && file.exists(input$gene_file$datapath)
    })
    
    has_text_input <- reactive({
      nzchar(trimws(input$gene_list)) && length(nzchar(strsplit(trimws(input$gene_list), "\n")[[1]])) >= 1
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
    
    # ===== Get pathway genes =====
    get_pathway_genes <- function() {
      db <- input$pathway_db
      pw <- input$pathway
      if (is.null(db) || is.null(pw) || !nzchar(pw)) return(NULL)
      if (db %in% names(all_pathways) && pw %in% names(all_pathways[[db]])) {
        return(list(genes = all_pathways[[db]][[pw]], db = db, name = pw))
      }
      NULL
    }
    
    # ===== Run Analysis =====
    run_analysis <- function() {
      if (rv$running) {
        showNotification("Analysis already running...", type = "warning")
        return()
      }
      
      # ===== 立即清除上一轮结果，隐藏结果区域和Explore区域 =====
      rv$results <- NULL
      modal_plot_store(NULL)
      gsea_plot_store(NULL)
      gsea_table_store(NULL)
      shinyjs::hide("results_container")
      shinyjs::hide("explore_section_container")
      updateSelectInput(session, "explore_dataset", choices = character(0), selected = character(0))
      
      # Use uploaded genes if available; fall back to selected pathway
      query_genes <- parse_gene_list()
      is_custom <- FALSE
      selected_db <- NULL
      selected_pw <- NULL
      
      if (is.null(query_genes)) {
        # If user provided input but no valid genes found, stop — don't fall back to pathway
        has_input <- (!is.null(input$gene_file) && file.exists(input$gene_file$datapath)) || nzchar(trimws(input$gene_list))
        if (has_input) {
          showNotification("No valid gene symbols found in your input. Analysis aborted.", type = "error", duration = 8)
          return()
        }
        pw_info <- get_pathway_genes()
        if (is.null(pw_info) || length(pw_info$genes) < 3) {
          showNotification("Upload a gene list or select a pathway with at least 3 genes", type = "error")
          return()
        }
        query_genes <- pw_info$genes
        rv$selected_pathway <- pw_info$name
        selected_db <- pw_info$db
        selected_pw <- pw_info$name
      } else {
        rv$selected_pathway <- paste0("Custom (", length(query_genes), " genes)")
        is_custom <- TRUE
      }
      
      rv$query_genes <- query_genes
      rv$running <- TRUE
      
      top_n_val <- input$top_n
      
      # ===== Check precomputed cache for built-in pathways =====
      if (!is_custom && has_pathway_cache()) {
        cached <- get_pathway_cache(selected_db, selected_pw)
        if (!is.null(cached) && nrow(cached) > 0) {
          cat(sprintf("[PWQuery] Cache hit: %s :: %s (%d rows)\n", selected_db, selected_pw, nrow(cached)))
          
          # Apply filters to cached results
          results <- copy(cached)
          mode_val <- isolate(input$mode)
          type_f <- isolate(input$type_filter)
          
          if (mode_val == "activate") results <- results[ES > 0]
          else if (mode_val == "suppress") results <- results[ES < 0]
          
          if (type_f == "chemical") results <- results[Type == "Chemical"]
          else if (type_f == "genetic") results <- results[Type == "Genetic"]
          
          if (isTRUE(isolate(input$fda_only))) results <- results[clinical_phase == "Launched"]
          
          if (nrow(results) > 0) {
            results <- results[order(-abs(NES))]
            if (nrow(results) > 100000) results <- head(results, 100000)
          }
          
          rv$results <- results
          rv$running <- FALSE
          
          shinyjs::show("results_container")
          shinyjs::show("explore_section_container")
          shinyjs::html("status_msg",
                        paste0(icon("check-circle"), " Results loaded from cache: ",
                               nrow(results), " datasets (instant)"))
          shinyjs::addClass("status_msg", "status-done")
          shinyjs::removeClass("status_msg", "status-running")
          shinyjs::runjs(sprintf("$('#%s').css('width', '100%%').text('100%%');", ns("progress_bar")))
          
          showNotification(
            paste("Loaded", nrow(results), "datasets from precomputed cache (instant)!"),
            type = "message", duration = 3
          )
          return()
        }
        cat(sprintf("[PWQuery] Cache miss: %s :: %s, falling back to computation\n", selected_db, selected_pw))
      }
      
      # Create progress file
      pfile <- tempfile("pwquery_progress_", fileext = ".txt")
      rv$progress_file <- pfile
      writeLines("0.0|Initializing...", pfile)  # Pre-create so polling sees it immediately
      rv$polling_id <- rv$polling_id + 1
      
      # Show progress UI
      shinyjs::show("progress_container")
      shinyjs::hide("results_container")
      shinyjs::html("status_msg", 
                    paste0(icon("hourglass-half"), " Analysis in progress... Due to the large dataset size, this process may take approximately 1 minutes. Please do not close this page or navigate away."))
      shinyjs::addClass("status_msg", "status-running")
      shinyjs::removeClass("status_msg", "status-done")
      shinyjs::html("progress_text", "Starting...")
      shinyjs::runjs(sprintf("$('#%s').css('width', '0%%').text('0%%');", ns("progress_bar")))
      
      # Run in external R process via callr (avoids fork bomb with future::multicore workers)
      # Metadata merge moved inside callr to avoid blocking the Shiny main thread.
      rv$callr_process <- callr::r_bg(function(query_genes, gsea_dir, top_n, progress_file, genelist_source, type_filter, fda_only, chem_meta_files, gen_meta_files, mode_val, fda_anno_file, drug_target_anno_file, gene_anno_file) {
        library(data.table)
        library(fst)
        library(parallel)
        source(genelist_source)
        results <- run_gsquery_global(query_genes, gsea_dir, top_n, progress_file, type_filter)
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
        keep_cols <- intersect(c("Dataset","GEM_ID","Type","ES","NES","Overlap",
                                 "Overlap_Pct","Pvalue","Padj","Leading_Edge","N_Genes",
                                 "Perturbagen","pbgene",
                                 "clinical_phase","moa","target","known_targets",
                                 "uniprot_id","entrez_id","protein_class"), names(results))
        extra <- setdiff(names(results), keep_cols)
        if (length(extra) > 0) results[, (extra) := NULL]
        # Remove rows where Perturbagen contains Chinese characters
        if ("Perturbagen" %in% names(results)) {
          results <- results[!grepl("[\u4e00-\u9fff]", Perturbagen)]
        }
        if (mode_val == "activate") results <- results[ES > 0]
        else if (mode_val == "suppress") results <- results[ES < 0]
        type_f <- type_filter
        if (type_f == "chemical") results <- results[Type == "Chemical"]
        else if (type_f == "genetic") results <- results[Type == "Genetic"]
        if (isTRUE(fda_only)) results <- results[clinical_phase == "Launched"]
        if (nrow(results) > 0) {
          if (type_f == "chemical") {
            cols_drop <- intersect(c("known_targets", "protein_class"), names(results))
          } else if (type_f == "genetic") {
            cols_drop <- intersect(c("target", "known_targets", "uniprot_id", "entrez_id",
                                     "clinical_phase", "moa"), names(results))
          } else {
            cols_drop <- character(0)
          }
          if (length(cols_drop) > 0) results[, (cols_drop) := NULL]
          results <- results[order(-abs(NES))]
          if (nrow(results) > 100000) results <- head(results, 100000)
        }
        results
      }, args = list(
        query_genes = query_genes,
        gsea_dir = GSEA_DIR,
        top_n = 100000L,
        progress_file = pfile,
        genelist_source = "R/Genelist.R",
        type_filter = isolate(input$type_filter),
        fda_only = isolate(input$fda_only),
        chem_meta_files = chem_meta_files,
        gen_meta_files = gen_meta_files,
        mode_val = isolate(input$mode),
        fda_anno_file = fda_anno_file,
        drug_target_anno_file = drug_target_anno_file,
        gene_anno_file = gene_anno_file
      ), stdout = "|", stderr = "|")
    }
    
    observeEvent(input$run_query, {
      run_analysis()
    })
    
    # ===== Callr Result Polling =====
    # Polls the external R process launched by run_analysis() every second.
    # Metadata merge + filtering now runs inside callr — results are ready immediately.
    observe({
      req(rv$running)
      req(rv$callr_process)
      
      invalidateLater(1000)
      
      rx <- rv$callr_process
      if (rx$is_alive()) return()
      
      cat("[PWQuery] Callr process completed\n")
      
      results <- tryCatch(rx$get_result(), error = function(e) {
        cat("[PWQuery] Callr result error:", conditionMessage(e), "\n")
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
          "No significant results found with current criteria",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      rv$results <- results
      rv$running <- FALSE
      
      shinyjs::show("results_container")
      shinyjs::show("explore_section_container")
      showNotification(
        paste("Analysis completed!"),
        type = "message",
        duration = 5
      )
    })
    
    # ===== Progress Polling =====
    observe({
      # Only poll while analysis is running; reactive dependency on rv$running auto start/stop
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
                       tags$h5("Chemical", style = "text-align:center;margin-top:5px;color:#3a7bd5;font-weight:bold;"),
                       shinycssloaders::withSpinner(
                         plotOutput(ns("results_rank_chem"), height = "400px"),
                         type = 4, color = "#3a7bd5"
                       ),
                       div(style = "text-align:center; margin-top:8px;",
                           downloadButton(ns("download_rank_chem"), "Download as PDF", class = "btn-sm btn-info plot-download-btn")
                       )
      )
      
      gen_plot <- div(style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px;",
                      tags$h5("Genetic", style = "text-align:center;margin-top:5px;color:#28a745;font-weight:bold;"),
                      shinycssloaders::withSpinner(
                        plotOutput(ns("results_rank_gen"), height = "400px"),
                        type = 4, color = "#28a745"
                      ),
                      div(style = "text-align:center; margin-top:8px;",
                          downloadButton(ns("download_rank_gen"), "Download as PDF", class = "btn-sm btn-info plot-download-btn")
                      )
      )
      
      if (type_val == "both") {
        fluidRow(column(6, chem_plot), column(6, gen_plot))
      } else if (type_val == "chemical") {
        fluidRow(column(12, chem_plot))
      } else {
        fluidRow(column(12, gen_plot))
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
        dt <- dt[order(-NES)]
        dt[, rank := .I]
        dt <- head(dt, 100000)
        max_abs <- max(abs(dt$NES), na.rm = TRUE)
        y_lim <- max_abs * 1.15
        label_data <- head(dt[order(-abs(NES))], 10)
        label_data <- label_data[!duplicated(plot_label)]
        p <- ggplot() + theme_minimal(base_size = 11)
        p <- p + geom_hline(yintercept = 0, color = "#E74C3C", linetype = "dashed", linewidth = 0.4)
        p <- p + geom_point(data = dt, aes(x = rank, y = NES, color = NES, size = abs(NES)), alpha = 0.7)
        p <- p + scale_size_continuous(range = c(0.3, 3), guide = "none")
        p <- p + scale_color_gradient2(low = "#3498DB", mid = "gray85", high = "#E74C3C",
                                       midpoint = 0, limits = c(-max_abs, max_abs),
                                       name = "NES  (Blue=Suppressed, Red=Activated)")
        p <- p + guides(color = guide_colorbar(barwidth = 8, barheight = 0.4,
                                               title.position = "top", title.hjust = 0.5))
        if (nrow(label_data) > 0)
          p <- p + ggrepel::geom_text_repel(data = label_data,
                                            aes(x = rank, y = NES, label = plot_label), color = "grey20",
                                            size = 3, max.overlaps = 20, box.padding = 0.3, point.padding = 0.2,
                                            min.segment.length = 0.2, segment.color = "grey50", segment.size = 0.3,
                                            show.legend = FALSE, fontface = "bold", force = 1)
        p <- p + labs(x = "Rank", y = "NES") +
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
      filename = function() { paste0("GeneSetQuery_Chemical_Rank_", Sys.Date(), ".pdf") },
      content = function(file) {
        p <- chem_rank_plot_obj()
        ggsave(file, plot = p, device = "pdf", width = 8, height = 6)
      }
    )
    output$download_rank_gen <- downloadHandler(
      filename = function() { paste0("GeneSetQuery_Genetic_Rank_", Sys.Date(), ".pdf") },
      content = function(file) {
        p <- gen_rank_plot_obj()
        ggsave(file, plot = p, device = "pdf", width = 8, height = 6)
      }
    )
    
    # ===== Dynamic Results Tables (show based on type filter) =====
    output$table_tabs_ui <- renderUI({
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
      if (nrow(dt) == 0) {
        empty_dt <- data.table(Message = paste("No", tolower(type_val), "results found"))
        return(DT::datatable(empty_dt, rownames = FALSE,
                             options = list(dom = 't', pageLength = 1, searching = FALSE, ordering = FALSE)))
      }
      
      # Drop irrelevant annotation columns based on table type
      if (type_val == "Chemical") {
        cols_drop <- intersect(c("known_targets", "protein_class"), names(dt))
      } else {
        cols_drop <- intersect(c("target", "known_targets", "uniprot_id",
                                 "clinical_phase", "moa"), names(dt))
      }
      if (length(cols_drop) > 0) dt[, (cols_drop) := NULL]
      
      # Format Pvalue/Padj to 5 significant digits
      dt[, Pvalue := signif(Pvalue, 5)]
      dt[, Padj := signif(Padj, 5)]
      
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
      
      # Prepare GEM_ID for download filename (fill NA, save raw before HTML transform)
      if ("GEM_ID" %in% names(dt) && "Dataset" %in% names(dt)) {
        missing <- is.na(dt$GEM_ID) | dt$GEM_ID == ""
        dt[, .gem_id_raw := fifelse(missing, Dataset, GEM_ID)]
        # Only keep rows with valid PSG/PSC GEM_ID
        valid <- grepl("^(PSG|PSC)\\d+$", dt$GEM_ID)
        dt <- dt[valid]
      } else {
        dt[, .gem_id_raw := Dataset]
      }
      
      # Add Download button column
      if ("Dataset" %in% names(dt)) {
        dt[, Download := sprintf(
          '<button class="btn btn-sm btn-success dl-btn" data-ds="%s" data-gem="%s"><i class="fa fa-download"></i> Download</button>',
          gsub('"', '&quot;', as.character(Dataset), fixed = TRUE),
          gsub('"', '&quot;', as.character(.gem_id_raw), fixed = TRUE)
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
      
      # Rename merge key "Dataset" to avoid conflict and "GEM_ID" to "Dataset" for display
      if ("Dataset" %in% names(dt)) setnames(dt, "Dataset", ".Dataset")
      if ("GEM_ID" %in% names(dt))  setnames(dt, "GEM_ID", "Dataset")
      # Rename Perturbagen to PbChem for display
      if ("Perturbagen" %in% names(dt)) setnames(dt, "Perturbagen", "PbChem")
      # Capitalize first letter of displayed column names
      cap_cols <- intersect(c("pbgene", "method"), names(dt))
      for (cc in cap_cols) setnames(dt, cc, paste0(toupper(substr(cc, 1, 1)), substr(cc, 2, nchar(cc))))
      
      # Remove empty columns and Type column
      dt[, Type := NULL]
      empty_cols <- names(dt)[sapply(dt, function(col) all(is.na(col) | col == "" | col == "NA"))]
      if (length(empty_cols) > 0) dt[, (empty_cols) := NULL]
      
      # Hidden column: absolute NES for sorting
      dt[, abs_NES := abs(NES)]
      
      # ---- Keep Dataset in data, hide via columnDefs (Download.R pattern) ----
      
      dataset_col_idx <- match(".Dataset", names(dt)) - 1
      gem_id_col_idx <- match(".gem_id_raw", names(dt)) - 1
      
      hidden_targets <- c(
        which(names(dt) == ".Dataset") - 1,
        which(names(dt) == ".gem_id_raw") - 1,
        which(names(dt) == "abs_NES") - 1
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
          order = list(list(which(names(dt) == "abs_NES") - 1, "desc")),
          columnDefs = list(list(visible = FALSE, targets = hidden_targets), dt_dash_render())
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
            if (!data) return;\n            var ds = data[%d];
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
        DT::formatSignif(columns = c("ES", "NES", "Overlap_Pct"), digits = 4)
    })
    
    output$results_chemical <- render_type_table("Chemical")
    output$results_genetic <- render_type_table("Genetic")
    
    # ============================================================
    # Explore Selected Dataset — KEGG/Hallmark Enrichment Analysis
    # ============================================================
    
    # Get pathway gene sets for enrichment (reuses pre-loaded all_pathways)
    enrich_pathways <- reactive({
      db <- input$explore_pw_db
      if (is.null(db) || !nzchar(db)) return(NULL)
      if (!db %in% names(all_pathways)) return(NULL)
      all_pathways[[db]]
    })
    
    # Populate individual pathway dropdown when enrich_pathways is loaded
    observe({
      req(enrich_pathways())
      pw <- enrich_pathways()
      choices <- sort(names(pw))
      updateSelectizeInput(session, "explore_pathway",
                           choices = choices,
                           selected = character(0),
                           server = TRUE,
                           options = list(placeholder = "Select a pathway for GSEA plot..."))
    })
    
    # Build ranked gene list for GSEA (shared, avoids recomputing)
    explore_ranked <- reactive({
      req(explore_deg())
      deg <- explore_deg()
      ranked <- deg$logFC
      names(ranked) <- toupper(deg$gene)
      ranked <- sort(ranked, decreasing = TRUE)
      ranked[!duplicated(names(ranked))]
    })
    
    # GSEA running enrichment plot for a single selected pathway
    gsea_plot_obj <- reactive({
      req(explore_ranked(), enrich_pathways(), input$explore_pathway)
      ranked <- explore_ranked()
      pathways <- enrich_pathways()
      pw_name <- input$explore_pathway
      
      if (!pw_name %in% names(pathways)) return(NULL)
      
      pathway_genes <- pathways[[pw_name]]
      
      # Use fgsea::plotEnrichmentData for correct ES computation
      ed <- tryCatch({
        fgsea::plotEnrichmentData(
          pathway = pathway_genes,
          stats   = ranked,
          gseaParam = 1
        )
      }, error = function(e) {
        cat("[Explore] plotEnrichmentData error:", e$message, "\n")
        NULL
      })
      
      if (is.null(ed)) return(NULL)
      
      N <- length(ranked)
      stats <- ranked
      n_hits <- length(ed$ticks[[1]])
      
      if (n_hits < 3) {
        return(ggplot() + 
                 annotate("text", x = 1, y = 1, 
                          label = paste("Too few genes matched (", n_hits, "). Need >= 3.", sep = ""),
                          size = 5, color = "grey50") + theme_void())
      }
      
      # fgsea plotEnrichmentData returns:
      #   curve[[1]] = x positions, curve[[2]] = running ES values
      #   ticks[[1]] = hit positions, ticks[[2]] = hit values
      #   stats[[1]] = ranked stat positions, stats[[2]] = ranked stat values
      es_df <- data.frame(rank = ed$curve[[1]], ES = ed$curve[[2]])
      
      # Clean pathway name for display
      pw_display <- gsub("^KEGG_|^HALLMARK_", "", pw_name)
      pw_display <- gsub("_", " ", pw_display)
      pw_display <- gsub(" Homo sapiens h \\S+$", "", pw_display)
      pw_display <- gsub("_", " ", pw_display)
      if (nchar(pw_display) > 60) pw_display <- paste0(substr(pw_display, 1, 57), "...")
      
      # Resolve GEM_ID for subtitle display
      gem_id_label <- input$explore_dataset
      if (!is.null(rv$results) && "GEM_ID" %in% names(rv$results) && "Dataset" %in% names(rv$results)) {
        gem_row <- rv$results[Dataset == input$explore_dataset]
        if (nrow(gem_row) > 0 && !is.na(gem_row$GEM_ID[1]) && nzchar(gem_row$GEM_ID[1]))
          gem_id_label <- gem_row$GEM_ID[1]
      }
      
      # Get NES & pval from enrichment result for curve annotation
      enrich_res <- enrich_result()
      nes_val <- if (!is.null(enrich_res) && pw_name %in% enrich_res$pathway) {
        enrich_res[pathway == pw_name, NES]
      } else NULL
      pval_val <- if (!is.null(enrich_res) && pw_name %in% enrich_res$pathway) {
        enrich_res[pathway == pw_name, pval]
      } else NULL
      if (!is.null(nes_val)) {
        p_str <- if (!is.null(pval_val)) sprintf("p = %.2e", pval_val) else "p = N/A"
        nes_label <- sprintf("NES = %.3f\n%s", nes_val, p_str)
      } else {
        nes_label <- "NES = N/A\np = N/A"
      }
      
      # Position annotation based on NES direction
      y_range <- max(es_df$ES) - min(es_df$ES)
      if (!is.null(nes_val) && nes_val >= 0) {
        y_pos <- min(es_df$ES) + y_range * 0.80  # activation: upper area
      } else {
        y_pos <- min(es_df$ES) + y_range * 0.20  # suppression: lower area
      }
      
      # Plot: Running ES
      p1 <- ggplot(es_df, aes(x = rank, y = ES)) +
        geom_line(color = "#3a7bd5", linewidth = 0.9) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
        annotate("text", x = N * 0.95, y = y_pos,
                 label = nes_label,
                 hjust = 1, size = 3.8, color = "#E74C3C", fontface = "bold") +
        labs(title = paste("GSEA:", pw_display),
             subtitle = paste0("Dataset: ", gem_id_label, " | ", 
                               n_hits, " genes in pathway"),
             y = "Running Enrichment Score") +
        theme_bw() +
        theme(
          panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.8),
          plot.title    = element_text(size = 13, face = "bold", color = "#2c5f8a"),
          plot.subtitle = element_text(size = 10, color = "black"),
          axis.title.x  = element_blank(),
          axis.text.x   = element_blank(),
          axis.ticks.x  = element_blank()
        )
      
      # Plot: Hits rug from ticks data
      hit_pos <- ed$ticks[[1]]
      hits_df <- data.frame(rank = hit_pos, y = 0)
      p2 <- ggplot(hits_df, aes(x = rank, xend = rank, y = 0, yend = 1)) +
        geom_segment(color = ifelse(ed$ticks[[2]] > 0, "#E74C3C60", "#3498DB60"),
                     linewidth = 0.3) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(x = NULL, y = NULL) +
        theme_void() +
        theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
              plot.margin = margin(2, 10, 0, 10),
              panel.background = element_rect(fill = "white", color = NA))
      
      # Plot: logFC binned
      bin_n <- 200
      bin_w <- max(1, floor(N / bin_n))
      bin_idx <- seq(1, N, by = bin_w)
      bin_end <- c(bin_idx[-1] - 1, N)
      bin_mid <- (bin_idx + bin_end) / 2
      bin_mean <- mapply(function(s, e) mean(stats[s:e]), bin_idx, bin_end)
      lfc_df <- data.frame(x = bin_mid, lfc = bin_mean)
      
      p3 <- ggplot(lfc_df, aes(x = x, y = 0, fill = lfc)) +
        geom_tile(aes(width = bin_w, height = 0.8)) +
        scale_fill_gradient2(low = "#3498DB", mid = "gray90", high = "#E74C3C",
                             midpoint = 0, name = "logFC") +
        labs(x = "Gene Rank (logFC: low → high)", y = NULL) +
        theme_minimal(base_size = 9) +
        theme(
          panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.8),
          panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y        = element_blank(),
          axis.ticks.y       = element_blank(),
          legend.position    = "bottom",
          legend.key.width   = unit(1.5, "cm"),
          legend.key.height  = unit(0.3, "cm"),
          legend.title       = element_text(size = 8),
          legend.text        = element_text(size = 7),
          legend.margin      = margin(0, 0, 0, 0)
        )
      
      patchwork::wrap_plots(p1, p2, p3, ncol = 1, heights = c(4, 0.7, 1.5))
    })
    
    output$gsea_enrich_plot <- renderPlot({
      req(gsea_plot_obj())
      print(gsea_plot_obj())
    }, res = 100)
    
    output$download_gsea_plot <- downloadHandler(
      filename = function() {
        paste0("gsea_", gsub(" ", "_", input$explore_pathway), "_",
               gsub(" ", "_", input$explore_dataset), "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        p <- gsea_plot_obj()
        req(p)
        ggsave(file, plot = p, device = "pdf", width = 10, height = 7)
      }
    )
    
    # Populate dataset dropdown from Gene Set Query results
    observe({
      req(rv$results)
      dt <- copy(rv$results)
      top_n_val <- isolate(input$top_n)
      
      # Match same truncation as tables
      dt_chem <- if ("Type" %in% names(dt)) head(dt[Type == "Chemical"], top_n_val) else dt[0]
      dt_gen  <- if ("Type" %in% names(dt)) head(dt[Type == "Genetic"],  top_n_val) else dt[0]
      dt <- rbindlist(list(dt_chem, dt_gen), fill = TRUE)
      if (nrow(dt) == 0) dt <- head(rv$results, top_n_val)
      
      if ("GEM_ID" %in% names(dt)) {
        dt[is.na(GEM_ID) | GEM_ID == "", GEM_ID := Dataset]
      }
      choices <- setNames(dt$Dataset, dt$GEM_ID)
      choices <- choices[!is.na(names(choices)) & names(choices) != "NA"]
      
      updateSelectInput(session, "explore_dataset", 
                        choices = choices,
                        selected = if (length(choices) > 0) choices[1] else character(0))
    })
    
    # Fetch DEG data for selected dataset
    explore_deg <- reactive({
      req(input$explore_dataset)
      ds <- input$explore_dataset
      if (is.null(ds) || !nzchar(ds)) return(NULL)
      
      deg <- get_deg_dt_cached(ds, GSEA_DIR)
      if (is.null(deg) || nrow(deg) == 0) return(NULL)
      
      # Standardize columns
      if (!"gene" %in% names(deg)) {
        if ("Gene" %in% names(deg)) setnames(deg, "Gene", "gene")
        else if ("GENE" %in% names(deg)) setnames(deg, "GENE", "gene")
        else return(NULL)
      }
      if (!"logFC" %in% names(deg)) {
        if ("log2FoldChange" %in% names(deg)) setnames(deg, "log2FoldChange", "logFC")
        else if ("LogFC" %in% names(deg)) setnames(deg, "LogFC", "logFC")
        else return(NULL)
      }
      
      deg <- deg[!is.na(logFC) & is.finite(logFC)]
      deg <- deg[!duplicated(gene)]
      deg
    })
    
    # Run fgsea enrichment (reuses explore_ranked to avoid duplicate computation)
    enrich_result <- reactive({
      req(explore_ranked(), enrich_pathways())
      
      ranked   <- explore_ranked()
      pathways <- enrich_pathways()
      
      if (length(ranked) < 100) return(NULL)
      
      tryCatch({
        set.seed(42)
        res <- fgsea::fgseaMultilevel(
          pathways = pathways,
          stats    = ranked,
          nPermSimple = 10000,
          eps      = 0
        )
        if (nrow(res) > 0) {
          setorder(res, padj, na.last = TRUE)
        }
        res
      }, error = function(e) {
        cat("[Explore] fgsea error:", e$message, "\n")
        NULL
      })
    })
    
    # Enrichment Dot Plot
    enrich_dotplot_obj <- reactive({
      req(enrich_result())
      res <- enrich_result()
      if (nrow(res) == 0) return(NULL)
      
      # Top 20 pathways by NES magnitude
      res_top <- head(res[order(-abs(NES))], 20)
      res_top[, pathway_label := gsub("^KEGG_|^HALLMARK_", "", pathway)]
      res_top[, pathway_label := gsub("_", " ", pathway_label)]
      res_top[, pathway_label := gsub(" Homo sapiens h \\S+$", "", pathway_label)]
      res_top[, pathway_label := ifelse(nchar(pathway_label) > 50, paste0(substr(pathway_label, 1, 47), "..."), pathway_label)]
      res_top[, pathway_label := factor(pathway_label, levels = rev(pathway_label))]
      res_top[, neg_log10_padj := -log10(padj)]
      res_top[, direction := ifelse(NES > 0, "Activated", "Suppressed")]
      
      max_padj <- max(res_top$neg_log10_padj[is.finite(res_top$neg_log10_padj)], na.rm = TRUE)
      cap_val <- min(20, max_padj * 1.1)
      res_top[!is.finite(neg_log10_padj) | neg_log10_padj > cap_val, neg_log10_padj := cap_val]
      
      # Resolve GEM_ID for subtitle display
      gem_id_label <- input$explore_dataset
      if (!is.null(rv$results) && "GEM_ID" %in% names(rv$results) && "Dataset" %in% names(rv$results)) {
        gem_row <- rv$results[Dataset == input$explore_dataset]
        if (nrow(gem_row) > 0 && !is.na(gem_row$GEM_ID[1]) && nzchar(gem_row$GEM_ID[1]))
          gem_id_label <- gem_row$GEM_ID[1]
      }
      
      ggplot(res_top, aes(x = NES, y = pathway_label)) +
        geom_point(aes(size = size, color = NES), alpha = 0.85) +
        scale_color_gradient2(low = "#4285F4", mid = "grey90", high = "#EA4335",
                              midpoint = 0, name = "NES") +
        scale_size_continuous(name = "Gene Count", range = c(2, 8)) +
        labs(
          title = paste("Enrichment Analysis —", input$explore_pw_db),
          subtitle = paste("Dataset:", gem_id_label),
          x = "Normalized Enrichment Score (NES)",
          y = NULL
        ) +
        theme_bw() +
        theme(
          panel.border   = element_rect(color = "black", fill = NA, linewidth = 0.8),
          plot.title    = element_text(size = 14, face = "bold", color = "#2c5f8a"),
          plot.subtitle = element_text(size = 10, color = "black"),
          axis.text.y   = element_text(size = 9, face = "bold", color = "black"),
          axis.title.x  = element_text(size = 11),
          legend.position = "right"
        )
    })
    
    output$enrich_dotplot <- renderPlot({
      req(enrich_dotplot_obj())
      print(enrich_dotplot_obj())
    }, res = 100)
    
    # Enrichment Bar Plot (top 15 by NES)
    enrich_barplot_obj <- reactive({
      req(enrich_result())
      res <- enrich_result()
      if (nrow(res) == 0) return(NULL)
      
      res_top <- head(res[order(-abs(NES))], 15)
      res_top[, pathway_label := gsub("^KEGG_|^HALLMARK_", "", pathway)]
      res_top[, pathway_label := gsub("_", " ", pathway_label)]
      res_top[, pathway_label := gsub(" Homo sapiens h \\S+$", "", pathway_label)]
      res_top[, pathway_label := ifelse(nchar(pathway_label) > 50, paste0(substr(pathway_label, 1, 47), "..."), pathway_label)]
      res_top[, pathway_label := factor(pathway_label, levels = rev(pathway_label))]
      res_top[, direction := ifelse(NES > 0, "Activated", "Suppressed")]
      
      # Resolve GEM_ID for subtitle display
      gem_id_label <- input$explore_dataset
      if (!is.null(rv$results) && "GEM_ID" %in% names(rv$results) && "Dataset" %in% names(rv$results)) {
        gem_row <- rv$results[Dataset == input$explore_dataset]
        if (nrow(gem_row) > 0 && !is.na(gem_row$GEM_ID[1]) && nzchar(gem_row$GEM_ID[1]))
          gem_id_label <- gem_row$GEM_ID[1]
      }
      
      ggplot(res_top, aes(x = NES, y = pathway_label, fill = NES)) +
        geom_col(width = 0.7) +
        scale_fill_gradient2(low = "#4285F4", mid = "grey90", high = "#EA4335",
                             midpoint = 0, name = "NES") +
        labs(
          title = paste("Top 15 Enriched Pathways —", input$explore_pw_db),
          subtitle = paste("Dataset:", gem_id_label),
          x = "Normalized Enrichment Score (NES)",
          y = NULL
        ) +
        theme_bw() +
        theme(
          panel.border   = element_rect(color = "black", fill = NA, linewidth = 0.8),
          plot.title    = element_text(size = 14, face = "bold", color = "#2c5f8a"),
          plot.subtitle = element_text(size = 10, color = "black"),
          axis.text.y   = element_text(size = 9, face = "bold", color = "black"),
          axis.title.x  = element_text(size = 11),
          legend.position = "right"
        )
    })
    
    output$enrich_barplot <- renderPlot({
      req(enrich_barplot_obj())
      print(enrich_barplot_obj())
    }, res = 100)
    
    # Enrichment Results Table
    output$enrich_table <- DT::renderDT({
      req(enrich_result())
      res <- copy(enrich_result())
      if (nrow(res) == 0) return(DT::datatable(data.table(Message = "No enrichment results")))
      
      res[, pathway_label := gsub("^KEGG_|^HALLMARK_", "", pathway)]
      res[, pathway_label := gsub("_", " ", pathway_label)]
      res[, pathway_label := gsub(" Homo sapiens h \\S+$", "", pathway_label)]
      # Truncate leadingEdge with click-to-expand toggle
      res[, leadingEdge := {
        sapply(leadingEdge, function(genes) {
          n <- length(genes)
          if (n <= 3) return(paste(genes, collapse = ", "))
          short <- paste(head(genes, 3), collapse = ", ")
          short <- paste0(short, " +", n - 3, " more")
          full <- paste(genes, collapse = ", ")
          sprintf(
            '<span class="le-wrap"><span class="le-short">%s</span><span class="le-full" style="display:none">%s</span> <i class="fa fa-caret-down le-toggle-btn" style="cursor:pointer;color:#3a7bd5"></i></span>',
            short, full
          )
        })
      }]
      res[, ES   := round(ES, 4)]
      res[, NES  := round(NES, 4)]
      res[, pval := signif(pval, 4)]
      res[, padj := signif(padj, 4)]
      
      display <- res[, .(Pathway = pathway_label, Size = size, 
                         ES, NES, Pvalue = pval, Padj = padj,
                         Leading_Edge = leadingEdge)]
      
      display <- replace_empty_with_dash(display)
      DT::datatable(display,
                    rownames = FALSE,
                    escape = FALSE,
                    selection = "single",
                    options = list(
                      pageLength = 15,
                      scrollX = TRUE,
                      dom = 'frtip',
                      order = list(list(3, "desc")),
                      columnDefs = list(dt_dash_render())
                    )
      ) %>% DT::formatSignif(columns = c("ES", "NES"), digits = 4)
    })
    
    # Download handlers for enrichment plots
    output$download_enrich_dotplot <- downloadHandler(
      filename = function() {
        paste0("enrich_dotplot_", gsub(" ", "_", input$explore_dataset), "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        p <- enrich_dotplot_obj()
        req(p)
        ggsave(file, plot = p, device = "pdf", width = 10, height = 8)
      }
    )
    
    output$download_enrich_barplot <- downloadHandler(
      filename = function() {
        paste0("enrich_barplot_", gsub(" ", "_", input$explore_dataset), "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        p <- enrich_barplot_obj()
        req(p)
        ggsave(file, plot = p, device = "pdf", width = 10, height = 7)
      }
    )
    
    # ===== Download Handler =====
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
        paste0("pathway_query_", type_suffix, "_", Sys.Date(), ".csv")
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
    
    # ===== Enrichment table CSV download (1:1 export) =====
    output$download_enrich_table <- downloadHandler(
      filename = function() {
        gem_id_label <- input$explore_dataset
        if (!is.null(rv$results) && "GEM_ID" %in% names(rv$results) && "Dataset" %in% names(rv$results)) {
          gem_row <- rv$results[Dataset == input$explore_dataset]
          if (nrow(gem_row) > 0 && !is.na(gem_row$GEM_ID[1]) && nzchar(gem_row$GEM_ID[1]))
            gem_id_label <- gem_row$GEM_ID[1]
        }
        paste0("enrichment_", gsub(" ", "_", gem_id_label), "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(enrich_result())
        res <- copy(enrich_result())
        if (nrow(res) == 0) {
          fwrite(data.table(Message = "No enrichment results"), file)
          return()
        }
        res[, pathway_label := gsub("^KEGG_|^HALLMARK_", "", pathway)]
        res[, pathway_label := gsub("_", " ", pathway_label)]
        res[, pathway_label := gsub(" Homo sapiens h \\S+$", "", pathway_label)]
        res[, leadingEdge := sapply(leadingEdge, paste, collapse = ", ")]
        display <- res[, .(Pathway = pathway_label, Size = size,
                           ES, NES, Pvalue = pval, Padj = padj,
                           Leading_Edge = leadingEdge)]
        strip_html(display)
        fwrite(display, file)
      }
    )
    
    # ===== Dataset Click Modal (same logic as GSQuery) =====
    observeEvent(input$click_dataset, {
      ds <- input$click_dataset
      if (is.null(ds) || !nzchar(ds)) return(NULL)
      
      dt <- isolate(rv$results)
      if (is.null(dt) || nrow(dt) == 0) return(NULL)
      
      row <- dt[grep(ds, Dataset, fixed = TRUE)]
      if (is.null(row) || nrow(row) == 0) return(NULL)
      row <- row[1]
      
      nes_val <- as.numeric(row$NES)
      es_val <- as.numeric(row$ES)
      pval_val <- as.numeric(row$Pvalue)
      
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
      
      link_html <- NULL; mol_img <- NULL
      
      # Chemical structure images
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
        link_html <- tags$a(href = paste0("https://pubchem.ncbi.nlm.nih.gov/compound/", cid_url), target = "_blank", class = "btn btn-sm btn-primary", icon("external-link-alt"), " Open in PubChem")
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
        link_html <- tags$a(href = paste0("https://www.uniprot.org/uniprotkb?query=", g), target = "_blank", class = "btn btn-sm btn-warning", icon("external-link-alt"), " Open in UniProt")
      }
      
      # Build items from row metadata
      skip_cols <- c("dataset", "Dataset", "Download")
      display_map <- c("GEM_ID" = "Dataset", "Perturbagen" = "PbChem")
      items <- list()
      
      for (col in names(row)) {
        if (col %in% skip_cols) next
        val <- row[[col]]
        if (is.null(val) || length(val) == 0 || identical(as.character(val), "") || identical(as.character(val), "NA") || (length(val) == 1 && is.na(val))) next
        if (is.numeric(val) && !is.infinite(val)) val <- round(as.numeric(val), 4)
        if (is.numeric(val) && is.infinite(val)) val <- ifelse(val > 0, "Inf", "-Inf")
        dcol <- if (col %in% names(display_map)) display_map[col] else col
        items[[length(items) + 1]] <- div(class = "detail-item", span(class = "detail-label", paste0(dcol, ": ")), span(class = "detail-value", gsub("<[^>]+>", "", as.character(val))))
      }
      
      # Get DEG data and create plots
      deg_dt <- get_deg_dt_cached(ds, GSEA_DIR)
      
      plot_lollipop <- NULL; plot_density <- NULL; plot_main_1 <- NULL
      plot_main_2 <- NULL; plot_count_bar <- NULL; plot_cdf <- NULL
      valid_data <- FALSE; deg <- NULL; has_pval <- FALSE; has_fdr <- FALSE
      
      if (!is.null(deg_dt) && nrow(deg_dt) > 0) {
        gene_candidates <- c("Gene", "gene", "symbol", "SYMBOL", "gene_name", "GeneSymbol", "gene_id")
        fc_candidates   <- c("logFC", "LogFC", "log2FC", "Log2FC", "log2FoldChange", "fc", "value", "Log2 (FC_avg)")
        pval_candidates <- c("pvalue", "p_value", "p.val", "Pvalue", "P_value", "P.val", "p", "P.Value", "p-value")
        fdr_candidates  <- c("fdr", "FDR", "padj", "p.adj", "p_adj", "Padj", "P.adj", "P_adj", "FDR-BH", "adj.P.Val")
        
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
        group_colors <- c("Down" = "#4285F4", "NS" = "gray", "Up" = "#EA4335")
        
        # Count bar chart
        count_stats <- deg[, .(gene_count = .N), by = group]
        count_stats$group <- factor(count_stats$group, levels = c("Down", "NS", "Up"))
        plot_count_bar <- ggplot(count_stats, aes(x = group, y = gene_count, fill = group)) +
          geom_col(width = 0.7, show.legend = FALSE) + scale_fill_manual(values = group_colors) +
          geom_text(aes(label = gene_count), vjust = -0.3, fontface = "bold") +
          labs(title = "Differential Gene Count", x = "", y = "Gene Number") + theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))
        
        # CDF curve
        deg_sorted <- deg[order(logFC)]
        deg_sorted$cdf <- seq(0, 1, length.out = nrow(deg_sorted))
        deg_sorted$group <- "NS"
        deg_sorted$group[deg_sorted$logFC < -1] <- "Down"
        deg_sorted$group[deg_sorted$logFC > 1]  <- "Up"
        deg_sorted$group <- factor(deg_sorted$group, levels = c("Down", "NS", "Up"))
        deg_sorted$gene <- as.character(deg_sorted$gene)
        deg_sorted$gene[is.na(deg_sorted$gene) | deg_sorted$gene == ""] <- "unknown"
        
        top10_genes <- deg[order(-abs(logFC))][1:min(10, .N)]
        top10_genes <- top10_genes[!is.na(gene) & nzchar(as.character(gene))]
        top10_genes$gene <- as.character(top10_genes$gene)
        setkey(deg_sorted, gene)
        top10_cdf <- merge(copy(top10_genes)[, .(gene, logFC)], deg_sorted[, .(gene, cdf)], by = "gene", all.x = TRUE)
        
        plot_cdf <- ggplot(deg_sorted, aes(x = logFC, y = cdf, color = group, text = gene)) +
          geom_line(linewidth = 1.1, alpha = 0.9) + geom_point(size = 0.7, alpha = 0.8) +
          scale_color_manual(values = group_colors) + geom_vline(xintercept = c(-1, 0, 1), color = "black", linewidth = 0.35, linetype = "dashed") +
          labs(title = "logFC Cumulative Distribution (CDF)", x = "logFC", y = "Cumulative Proportion") +
          ggrepel::geom_text_repel(
            data = top10_cdf,
            aes(x = logFC, y = cdf, label = gene),
            size = 3, max.overlaps = 15, box.padding = 0.3, color = "black"
          ) +
          theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), legend.position = "bottom", plot.title = element_text(face = "bold", size = 13))
        
        {
          deg_sorted2 <- deg[order(logFC)]
          deg_sorted2$rank <- 1:nrow(deg_sorted2)
          
          top10_rank <- merge(copy(top10_genes)[, .(gene, logFC)], deg_sorted2[, .(gene, rank, group)], by = "gene", all.x = TRUE)
          top10_rank <- top10_rank[!is.na(rank)]
          
          plot_main_1 <- ggplot(deg_sorted2, aes(x = rank, y = logFC, color = group, text = gene)) +
            geom_point(size = 0.8, alpha = 0.6) + scale_color_manual(values = group_colors) +
            geom_hline(yintercept = c(-1, 0, 1), linetype = "dashed", linewidth = 0.3) +
            labs(title = "Gene Ranked by logFC", x = "Gene Rank", y = "logFC") +
            ggrepel::geom_text_repel(
              data = top10_rank,
              aes(x = rank, y = logFC, label = gene),
              size = 3, max.overlaps = 15, box.padding = 0.3
            ) +
            theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), legend.position = "bottom")
          
          deg$bin <- cut(deg$logFC, breaks = seq(-10, 10, 1), include.lowest = TRUE)
          bin_stats <- deg[, .(count = .N), by = .(bin, group)]
          plot_main_2 <- ggplot(bin_stats, aes(x = bin, y = count, fill = group)) +
            geom_col(position = "stack", width = 0.8) + scale_fill_manual(values = group_colors) +
            geom_vline(xintercept = which(levels(bin_stats$bin) == "(-0.5,0]"), linetype = "dashed", linewidth = 0.3) +
            labs(title = "logFC Distribution", x = "logFC Bins", y = "Gene Count") +
            theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
          }
        
        # Lollipop plot
        top_up <- head(deg[logFC > 0][order(-logFC)], 20)
        top_down <- head(deg[logFC < 0][order(logFC)], 20)
        top_genes_lolli <- rbindlist(list(top_up, top_down), fill = TRUE)
        if (nrow(top_genes_lolli) > 0) {
          top_genes_lolli$direction <- ifelse(top_genes_lolli$logFC > 0, "Up", "Down")
          top_genes_lolli$gene <- as.character(top_genes_lolli$gene)
          top_genes_lolli <- top_genes_lolli[!is.na(gene) & nzchar(gene)]
          if (nrow(top_genes_lolli) > 0) {
            plot_lollipop <- ggplot(top_genes_lolli, aes(x = reorder(gene, logFC), y = logFC, fill = direction, text = gene)) +
              geom_col(width = 0.2) +
              geom_point(size = 2, shape = 21) +
              coord_flip() +
              labs(title = "Top 20 Genes", x = "", y = "logFC", fill = "Direction") +
              scale_fill_manual(values = c("Up" = "#EA4335", "Down" = "#4285F4")) +
              theme_bw() +
              theme(axis.text.y = element_text(size = 8), legend.position = "bottom")
          }
        }
        
        plot_density <- ggplot(deg, aes(x = logFC)) +
          geom_density(fill = "#76A7FA", alpha = 0.4) + geom_vline(xintercept = 0, linetype = "dashed") +
          labs(title = "Density", x = "logFC", y = "Density") + theme_bw()
      }
      
      # GSEA enrichment data (for Gene Set Query: perturbation DEG vs. selected pathway genes)
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
              set.seed(42)
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
        modal_ggplots <- list(count_bar = plot_count_bar, cdf = plot_cdf, main_1 = plot_main_1,
                              main_2 = plot_main_2, density = plot_density, lollipop = plot_lollipop)
        modal_ggplots <- Filter(function(x) !is.null(x) && inherits(x, "ggplot"), modal_ggplots)
        
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
          dataset = as.character(ds),
          gem_id = if (!is.null(row$GEM_ID) && nzchar(as.character(row$GEM_ID))) as.character(row$GEM_ID) else as.character(ds),
          ggplots = modal_ggplots,
          combined_plot = combined_plot,
          gsea_data = gsea_data
        ))
      } else {
        modal_plot_store(NULL)
      }
      
      # Effect tag
      effect_class <- ifelse(!is.na(es_val) && es_val > 0, "effect-activator", "effect-suppressor")
      effect_tag <- div(class = paste("detail-effect", effect_class),
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
          
          div(class = "detail-card",
              
              effect_tag, br(), link_html, br(), mol_img,
              items,
              
              if (valid_data) {
                tagList(
                  br(), hr(),
                  
                  tags$div(
                    style = "margin: 10px 0 16px 0; text-align: center;",
                    downloadButton(ns("download_combined_pdf"), "Download Plot", class = "btn btn-default btn-lg")
                  ),
                  
                  withSpinner(plotOutput(ns("modal_combined"), height = "650px"), type = 4, color = "#3a7bd5"),
                  
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
                      
                      withSpinner(plotOutput(ns("modal_gsea"), height = "520px"), type = 4, color = "#228B22")
                    )
                  }
                )
              }
          )
        ),
        size = "l", easyClose = TRUE, footer = modalButton("Close")
      ))
      
      if (valid_data) {
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
