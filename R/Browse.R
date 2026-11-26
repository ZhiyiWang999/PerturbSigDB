OneGeneBrowseUI <- function(id, title = "Browse") {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        div.dt-container {
          border-radius: 10px !important;
          overflow: hidden !important;
          box-shadow: 0 1px 3px rgba(0,0,0,0.05);
        }
        table.dataTable {
          border-collapse: collapse !important;
          width: 100% !important;
        }
        table.dataTable thead th {
          background-color: #3a7bd5 !important;
          color: #ffffff !important;
          font-weight: 600 !important;
          font-size: 13px !important;
          padding: 11px 10px !important;
          border: none !important;
          text-align: center !important;
        }
        table.dataTable tbody td {
          text-align: center !important;
          background-color: #ffffff !important;
          vertical-align: middle !important;
          padding: 9px 10px !important;
          font-size: 13px !important;
          color: #2d3748 !important;
          border-bottom: 1px solid #f0f0f0 !important;
        }
        table.dataTable tbody tr:hover td {
          background-color: #f0f7ff !important;
        }
        table.dataTable tbody tr.selected > *,
        table.dataTable.stripe tbody tr.odd.selected > *,
        table.dataTable.display tbody tr.odd.selected > *,
        table.dataTable.hover tbody tr.selected:hover > *,
        table.dataTable.display tbody tr.selected:hover > * {
          box-shadow: inset 0 0 0 9999px rgba(208, 208, 208, 0.9) !important;
          color: #333 !important;
        }
        .dataTables_wrapper .dataTables_filter input,
        .dataTables_wrapper .dataTables_length select {
          border-radius: 6px;
          border: 1px solid #d0d7de;
          padding: 4px 8px;
        }
        .dt-buttons .dt-button {
          padding: 5px 10px !important;
          border-radius: 6px !important;
          background: #eaf3ff !important;
          color: #3a7bd5 !important;
          border: 1px solid #d6e4ff !important;
          font-weight: 500 !important;
        }
        .dataset-link { color: #3a7bd5; cursor: pointer; text-decoration: underline; }
        .dataset-wide-modal { width: 90% !important; max-width: 1200px !important; }
        .detail-card { background: #f8f9fa; border-radius: 12px; padding: 20px; }
        .detail-title { color: #2c3e50; font-weight: 700; font-size: 18px; margin-bottom: 15px; border-left: 4px solid #3a7bd5; padding-left: 10px; }
        .detail-item { background: white; border-radius: 8px; padding: 10px 15px; margin-bottom: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); }
        .detail-label { font-weight: 600; color: #3a7bd5; min-width: 120px; display: inline-block; }
        .detail-value { color: #2c3e50; }
        .modal-image-row { margin-top: 10px; margin-bottom: 14px; }
        .modal-image-card { background: #ffffff; border-radius: 12px; padding: 12px; border: 1px solid #e9eef5; box-shadow: 0 2px 6px rgba(0,0,0,0.04); }
        .modal-img-box { height: 240px; display: flex; align-items: center; justify-content: center; }
        .modal-img-box-large { height: 300px; display: flex; align-items: center; justify-content: center; }
        .modal-structure-img { max-width: 100%; max-height: 100%; object-fit: contain; }
        .modal-img-caption { font-size: 12px; color: #888; margin-top: 6px; }
        .browse-intro-box {
          background-color: #f0f6ff;
          border-radius: 12px;
          padding: 20px;
          margin-bottom: 10px;
        }
        .browse-intro-title {
          color: #2c5f8a;
          font-weight: 600;
          margin-bottom: 15px;
        }
        .browse-search-box {
          background: #f0f6ff;
          border-radius: 10px;
          padding: 14px 16px 8px 16px;
          margin-bottom: 4px;
        }
        .browse-filter-item {
          margin-bottom: 6px;
        }
        .browse-filter-label {
          font-size: 12px;
          font-weight: 600;
          color: #3a7bd5;
          margin-bottom: 2px;
          display: block;
        }
        .browse-filter-item .form-group {
          margin-bottom: 0 !important;
        }
        .browse-filter-item .form-control {
          border: 1px solid #d0d7de;
          border-radius: 6px;
          height: 34px;
          font-size: 13px;
          padding: 4px 10px;
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
      ", ns("download_single_ds")))),
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
      )
    ),
    
    fluidRow(
      shinydashboard::box(
        title = title,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 12,
        tags$div(class = "browse-intro-box",
          tags$h4(icon("search"), " Browse Perturbation Database", class = "browse-intro-title"),
          tags$hr(),
          p(HTML("<b>Browse</b> provides a searchable table of all perturbation signatures in the database. You can filter by gene, PbChem, cell line, tissue, disease, or accession across two tabs: Genetic Perturbagens (KD/KO/OE/Mut) and Chemical Perturbagens (drugs/compounds).")),
          p("Select a dataset row and click ", tags$b("Analysis"), " below to run pathway enrichment analysis on the selected signature.", style = "margin-bottom: 0;")
        ),
        br(),
        tabsetPanel(
          id = ns("tabs"),
          
          tabPanel(
            "Genetic Perturbagens",
            br(),
            div(class = "browse-search-box",
                fluidRow(
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "Gene"),
                             textInput(ns("search_gene"), NULL,
                                       placeholder = "e.g. CDK4", width = "100%")
                         )
                  ),
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "Cell"),
                             textInput(ns("search_cell_gen"), NULL,
                                       placeholder = "e.g. MCF7", width = "100%")
                         )
                  ),
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "Tissue"),
                             textInput(ns("search_tissue_gen"), NULL,
                                       placeholder = "e.g. Breast", width = "100%")
                         )
                  ),
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "Disease"),
                             textInput(ns("search_oncotree_gen"), NULL,
                                       placeholder = "e.g. BRCA", width = "100%")
                         )
                  )
                ),
                fluidRow(
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "Method"),
                             textInput(ns("search_method_gen"), NULL,
                                       placeholder = "e.g. Knockout", width = "100%")
                         )
                  ),
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "Accession"),
                             textInput(ns("search_accession_gen"), NULL,
                                       placeholder = "e.g. GSE92742", width = "100%")
                         )
                  ),
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", HTML("&nbsp;")),
                             div(
                               actionButton(ns("search_btn_gen"), "Search",
                                            icon = icon("search"), class = "btn-primary",
                                            style = "margin-right: 8px;"),
                               actionButton(ns("clear_btn_gen"), "Clear",
                                            icon = icon("times"),
                                            style = "margin-right: 8px;"),
                               actionButton(ns("demo_btn_gen"), "Demo",
                                            icon = icon("play-circle"),
                                            class = "btn-warning")
                             )
                         )
                  )
                )
            ),
            br(),
            div(style = "text-align: left; margin-bottom: 5px;",
                downloadButton(ns("download_table2"), "CSV",
                               class = "btn-sm btn-outline-secondary")),
            uiOutput(ns("load_note_gen")),
            fluidRow(
              column(
                12,
                shinycssloaders::withSpinner(
                  DT::DTOutput(ns("table2")),
                  type = 4,
                  color = "#3a7bd5"
                )
              )
            )
          ),
          
          tabPanel(
            "Chemical Perturbagens",
            br(),
            div(class = "browse-search-box",
                fluidRow(
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "PbChem"),
                             textInput(ns("search_perturbagen"), NULL,
                                       placeholder = "e.g. imatinib", width = "100%")
                         )
                  ),
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "Cell"),
                             textInput(ns("search_cell_chem"), NULL,
                                       placeholder = "e.g. MCF7", width = "100%")
                         )
                  ),
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "Tissue"),
                             textInput(ns("search_tissue_chem"), NULL,
                                       placeholder = "e.g. Breast", width = "100%")
                         )
                  ),
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "Disease"),
                             textInput(ns("search_oncotree_chem"), NULL,
                                       placeholder = "e.g. BRCA", width = "100%")
                         )
                  )
                ),
                fluidRow(
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", "Accession"),
                             textInput(ns("search_accession_chem"), NULL,
                                       placeholder = "e.g. GSE", width = "100%")
                         )
                  ),
                  column(3,
                         div(class = "browse-filter-item",
                             tags$label(class = "browse-filter-label", HTML("&nbsp;")),
                             div(
                               actionButton(ns("search_btn_chem"), "Search",
                                            icon = icon("search"), class = "btn-primary",
                                            style = "margin-right: 8px;"),
                               actionButton(ns("clear_btn_chem"), "Clear",
                                            icon = icon("times"),
                                            style = "margin-right: 8px;")
                             )
                         )
                  )
                )
            ),
            br(),
            div(style = "text-align: left; margin-bottom: 5px;",
                downloadButton(ns("download_table1"), "CSV",
                               class = "btn-sm btn-outline-secondary")),
            uiOutput(ns("load_note_chem")),
            fluidRow(
              column(
                12,
                shinycssloaders::withSpinner(
                  DT::DTOutput(ns("table1")),
                  type = 4,
                  color = "#3a7bd5"
                )
              )
            )
          )
        ),
        
        # ================== Pathway Enrichment Analysis ==================
        br(),
        tags$hr(style = "border-top: 2px solid #3a7bd5; margin: 20px 0;"),
        div(class = "browse-search-box",
            fluidRow(
              column(4,
                     div(class = "browse-filter-item",
                         tags$label(class = "browse-filter-label", "Select a dataset row in the table above, then click Analysis:"),
                         actionButton(ns("analysis_btn"), "Analysis",
                                      icon = icon("chart-line"), class = "btn-primary")
                     )
              )
            )
        ),
        shinyjs::hidden(
          div(id = ns("analysis_panel"),
              tags$h4("Pathway Enrichment Analysis", style = "color:#2c5f8a; font-weight:700; margin:15px 0;"),
              div(class = "browse-search-box",
                  fluidRow(
                    column(4,
                           div(class = "browse-filter-item",
                               tags$label(class = "browse-filter-label", "Pathway Database"),
                               selectInput(ns("explore_pw_db"), NULL,
                                           choices = c("KEGG", "Hallmark", "BioCarta", "Elsevier Pathway",
                                                       "HumanCyc", "Panther", "Reactome", "WikiPathways"),
                                           selected = "KEGG", width = "100%")
                           )
                    ),
                    column(2,
                           div(class = "browse-filter-item",
                               tags$label(class = "browse-filter-label", HTML("&nbsp;")),
                               actionButton(ns("run_analysis_btn"), "Run",
                                            icon = icon("play"), class = "btn-success",
                                            style = "margin-top: 4px;")
                           )
                    )
                  )
              ),
              br(),
              uiOutput(ns("enrich_results_ui"))
          )
        )
      )
    )
  )
}


# ================== 跨会话共享缓存（进程级）==================
# 所有会话读的是同一份 fst 元数据，装配结果与用户无关，没必要每个会话各装一遍。
# 之前缓存环境建在模块内部（每会话一份），后果是：
#   * 每个访客都要重新 read_fst（~100MB）+ 重新装配 92 万行（本地 ~15s，
#     小规格网页服务器可达数十秒）；
#   * 内存按"会话数 × 2.2GB"增长，很容易触发 swap / OOM。
# 放在模块外层：worker 进程启动时创建一次，所有会话共享，第 2 个访客起基本是秒开。
.gemap_shared <- new.env(parent = emptyenv())

# ---- 进程间（磁盘）缓存 ----------------------------------------------------
# .gemap_shared 只在单个 worker 进程内共享；网页服务器通常会跑多个 worker，
# 而且进程会被回收 / 重启 —— 每个 worker 的"第一个访客"仍要重新 read_fst + 装配
# （本地 ~15s，小规格网页服务器可达数十秒），这正是"本地快、外部网页慢"的根源。
# 因此装配完成后额外落一份 fst 到磁盘：新 worker 直接 read_fst 读回，
# 冷启动从"重新装配"降级为"读一个文件"，通常几秒完成。
.gemap_cache_dir <- tryCatch({
  d <- file.path(getwd(), ".gemap_cache")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  if (file.access(d, mode = 2L) == 0L) d else NULL
}, error = function(e) NULL)

.gemap_cache_path <- function(key) {
  if (is.null(.gemap_cache_dir)) return(NULL)
  file.path(.gemap_cache_dir, paste0(key, ".fst"))
}

.gemap_cache_meta_path <- function(key) {
  if (is.null(.gemap_cache_dir)) return(NULL)
  file.path(.gemap_cache_dir, paste0(key, ".rds"))
}

# 缓存指纹：只认"文件名 + 文件大小"。
# 关键：不能只比 mtime —— 把应用同步/拷贝到服务器时，源 fst 的 mtime 会变成拷贝时间
# （一定比缓存新），于是**每个新 worker 都判定缓存失效并重新装配一遍**，
# 首屏就得等几十秒，这正是"外部网页第一次点开 Browse 半天没反应"的主因之一。
# 文件大小在拷贝时不变，元数据真的换了（大小变了）缓存才会失效重建。
.gemap_cache_signature <- function(source_files) {
  list(files = basename(source_files),
       sizes = as.numeric(file.size(source_files)))
}

.gemap_cache_valid <- function(key, source_files = NULL) {
  path <- .gemap_cache_path(key)
  if (is.null(path) || !file.exists(path)) return(FALSE)
  src <- source_files[!is.na(source_files) & file.exists(source_files)]
  if (length(src) == 0L) return(TRUE)
  meta <- tryCatch(readRDS(.gemap_cache_meta_path(key)), error = function(e) NULL)
  if (is.null(meta) || is.null(meta$sizes)) {
    # 旧缓存没有指纹：退回 mtime 比较
    src_mtime <- tryCatch(max(file.mtime(src), na.rm = TRUE),
                          error = function(e) NA_real_)
    if (length(src_mtime) == 0 || is.na(src_mtime)) return(TRUE)
    cache_mtime <- file.mtime(path)
    return(!is.na(cache_mtime) && cache_mtime >= src_mtime)
  }
  cur <- .gemap_cache_signature(src)
  identical(meta$files, cur$files) &&
    length(meta$sizes) == length(cur$sizes) &&
    all(meta$sizes == cur$sizes)
}

.gemap_cache_save <- function(key, x, source_files = NULL) {
  path <- .gemap_cache_path(key)
  if (is.null(path)) return(invisible(NULL))
  tryCatch({
    fst::write_fst(x, path, compress = 50)
    if (!is.null(source_files)) {
      src <- source_files[!is.na(source_files) & file.exists(source_files)]
      if (length(src)) saveRDS(.gemap_cache_signature(src), .gemap_cache_meta_path(key))
    }
  }, error = function(e) message("[Browse] cache write failed (", key, "): ",
                                 conditionMessage(e)))
  invisible(NULL)
}

.gemap_cache_load <- function(key) {
  path <- .gemap_cache_path(key)
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(fst::read_fst(path, as.data.table = TRUE), error = function(e) NULL)
}

OneGeneBrowseServer <- function(
    id,
    analysis_type_OV = "Browse",
    help_id = "help1-1.html"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Register static file path for dataset CSV downloads (same as other modules)
    if (!"dataset_csv" %in% names(shiny::resourcePaths())) {
      shiny::addResourcePath("dataset_csv", normalizePath("rdata_columns_csv"))
    }
    
    library(data.table)
    library(DT)
    library(shiny)
    library(shinyjs)
    library(ggplot2)
    library(fst)
    library(plotly)
    library(shinycssloaders)
    library(patchwork)
    
    # ================== 路径配置 ==================
    data_dir <- "rdata_columns_csv"
    csv_dir  <- "dataset_csv"
    meta_fst_dir <- "signature_by_gene_fst"
    accession_map_file <- "Accession映射/Accession_merged_all最终.txt"
    
    # ================== 懒加载 Accession 映射表 ==================
    # 分片装配会多次调用，这里做进程内缓存
    accession_map_cache <- new.env(parent = emptyenv())
    load_accession_map <- function() {
      if (!is.null(accession_map_cache$dt)) return(accession_map_cache$dt)
      if (!file.exists(accession_map_file)) return(NULL)
      dt <- fread(accession_map_file, sep = "\t", header = TRUE, 
                  na.strings = c("NA", "\"\""), quote = "\"",
                  colClasses = list(character = "PMID"))
      dt <- dt[, .(Accession, PMID, Journal)]
      dt <- dt[!is.na(Accession) & Accession != ""]
      dt <- unique(dt, by = "Accession")
      accession_map_cache$dt <- dt
      dt
    }
    
    search_dirs <- unique(c(
      ".",
      data_dir,
      csv_dir,
      meta_fst_dir
    ))
    
    # ================== metadata 文件 (.fst) ==================
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
    
    # ================== metadata.fst 与真正数据 rdata 的对应关系 ==================
    meta_to_data_file <- c(
      "CIGSMetadata.fst"             = "CIGSData.rdata",
      "CIGSData2Metadata.fst"        = "CIGSData2.rdata",
      "HerbDataMetadata.fst"         = "HerbData.rdata",
      "LINCS_2020_Metadata.fst"      = "LINCS_2020_Chemical.rdata",
      "LINCS_L1000_Metadata.fst"     = "LINCS_L1000_Chemical.rdata",
      
      "GuoziDataMetadata.fst"        = "GuoziData.rdata",
      "PerturbAtlasMetadata.fst"     = "PerturbAtlasData.rdata",
      "LINCS_2020_KD_Metadata.fst"   = "LINCS_2020_KD.rdata",
      "LINCS_2020_KO_Metadata.fst"   = "LINCS_2020_KO.rdata",
      "LINCS_L1000_KD_Metadata.fst"  = "LINCS_L1000_KD.rdata",
      "LINCS_L1000_KO_Metadata.fst"  = "LINCS_L1000_KO.rdata"
    )
    
    # ================== Tissue 标准化（原发肿瘤组织逻辑） ==================
    # 1. 格式统一：下划线→空格，拼写/大小写统一
    tissue_format_map <- c(
      "Soft_Tissue"                    = "Soft Tissue",
      "Biliary_Tract"                  = "Biliary Tract",
      "Peripheral_Nervous_System"      = "Peripheral Nervous System",
      "Pleural_Effusion"               = "Pleural Effusion",
      "pleural_effusion"               = "Pleural Effusion",
      "oesophagus"                     = "Esophagus",
      "Esophagus and Stomach"          = "Esophagus",
      "Testis"                         = "Testes",
      "Unkown"                         = "Unknown",
      "CNS"                            = "Central Nervous System",
      "Embryonal"                      = "Embryonic",
      "Bowel"                          = "Large Intestine",
      "Airway/Lung"                    = "Lung",
      "Bladder/Urinary_Tract"          = "Bladder",
      "autonomic_ganglia"              = "Autonomic Ganglia",
      "epididymis"                     = "Epididymis",
      "synovial_membrane"              = "Synovial Membrane",
      "larynx"                         = "Larynx",
      "Haematopoietic_And_Lymphoid_Tissue" = "Haematopoietic and Lymphoid Tissue",
      "Haematopoietic_And_Lymphoid"    = "Haematopoietic and Lymphoid Tissue",
      "Lymph_Node"                     = "Haematopoietic and Lymphoid Tissue",
      "Lymphoid"                       = "Haematopoietic and Lymphoid Tissue",
      "Myeloid"                        = "Haematopoietic and Lymphoid Tissue",
      "Uterus"                         = "Endometrium",
      "Central_Nervous_System"         = "Central Nervous System",
      "Skeletal_Muscle"                = "Skeletal Muscle",
      "Smooth_Muscle"                  = "Smooth Muscle",
      "Pleural_Cavity"                 = "Pleura",
      "Pericardial_Effusion"           = "Pleural Effusion",
      "Cord_Blood"                     = "Haematopoietic and Lymphoid Tissue",
      "Bone_Marrow"                    = "Bone",
      "Spinal_Cord"                    = "Central Nervous System",
      "Ascites"                        = "Ascites",
      "Pleura"                         = "Pleura",
      "Peritoneum"                     = "Peritoneum",
      "Cartilaginous_Endplate"         = "Cartilage"
    )
    
    # 2. Cell级覆写：对同一细胞系取原发肿瘤组织（非取材部位）
    cell_tissue_overrides <- c(
      "MDA-MB-231"   = "Breast",
      "MDA-MB-436"   = "Breast",
      "MDA-MB-453"   = "Breast",
      "MCF7"         = "Breast",
      "Calu-3"       = "Lung",
      "NCI-H292"     = "Lung",
      "NCI-H2087"    = "Lung",
      "NCI-H1944"    = "Lung",
      "A-431"        = "Skin",
      "HEC-1-B"      = "Endometrium",
      "HeLa"         = "Cervix",
      "HELA"         = "Cervix",
      "SW1463"       = "Large Intestine",
      "LS411N"       = "Large Intestine",
      "HEL"          = "Haematopoietic and Lymphoid",
      "Kasumi-1"     = "Haematopoietic and Lymphoid",
      "THP-1"        = "Haematopoietic and Lymphoid",
      "MUTZ-3"       = "Haematopoietic and Lymphoid",
      "OCI-AML3"     = "Haematopoietic and Lymphoid",
      "OCI-AML5"     = "Haematopoietic and Lymphoid",
      "KMS-11"       = "Haematopoietic and Lymphoid",
      "Rh30"         = "Soft Tissue",
      "Rh41"         = "Soft Tissue",
      "TC71"         = "Bone",
      "NAMALWA"      = "Haematopoietic and Lymphoid",
      "WSU-DLCL2"    = "Haematopoietic and Lymphoid",
      "AMO-1"        = "Haematopoietic and Lymphoid"
    )
    
    standardize_tissue <- function(df) {
      if (!"Tissue" %in% names(df)) return(df)
      if (!"Cell" %in% names(df)) return(df)
      
      # Step 1: Format unification
      df[, Tissue := ifelse(
        Tissue %in% names(tissue_format_map),
        tissue_format_map[Tissue],
        gsub("_", " ", Tissue)
      )]
      
      # Step 2: Cell-level overrides (primary tumor tissue)
      cell_raw <- iconv(as.character(df$Cell), to = "UTF-8", sub = "byte")
      cell_key <- toupper(trimws(cell_raw))
      ov <- cell_tissue_overrides[cell_key]
      idx <- which(!is.na(ov))
      if (length(idx) > 0) {
        df[idx, Tissue := unname(ov[idx])]
      }
      
      # Step 3: Strip trailing " Tissue" suffix
      df[, Tissue := gsub(" Tissue$", "", as.character(Tissue))]
      
      df
    }
    
    # ================== 基础工具函数 ==================
    
    safe_filename <- function(x) {
      x <- as.character(x)
      x <- gsub("[\\\\/:*?\"<>|]", "_", x)
      x <- gsub("\\s+", "_", x)
      x <- trimws(x)
      ifelse(is.na(x) | x == "", "dataset", x)
    }
    
    clean_id <- function(x) {
      x <- as.character(x)
      x <- tolower(x)
      x <- gsub("[^a-z0-9]", "", x)
      x
    }
    
    resolve_file <- function(filename) {
      if (is.na(filename) || filename == "") {
        return(NA_character_)
      }
      
      candidates <- unique(c(
        filename,
        file.path(search_dirs, filename)
      ))
      
      candidates <- candidates[file.exists(candidates)]
      
      if (length(candidates) == 0) {
        return(NA_character_)
      }
      
      candidates[1]
    }
    
    get_data_file_from_meta <- function(meta_file) {
      meta_file <- basename(meta_file)
      
      if (meta_file %in% names(meta_to_data_file)) {
        return(unname(meta_to_data_file[meta_file]))
      }
      
      sub("Metadata\\.rdata$", ".rdata", meta_file, ignore.case = TRUE)
    }
    
    object_to_dt <- function(obj) {
      if (is.data.table(obj)) {
        return(copy(obj))
      }
      
      if (is.data.frame(obj)) {
        return(as.data.table(obj, keep.rownames = "row_id"))
      }
      
      if (is.matrix(obj)) {
        obj_df <- as.data.frame(obj, check.names = FALSE)
        return(as.data.table(obj_df, keep.rownames = "row_id"))
      }
      
      return(NULL)
    }
    
    match_dataset_column <- function(col_names, dataset_id) {
      dataset_id <- as.character(dataset_id)
      
      # 1. 精确匹配
      if (dataset_id %in% col_names) {
        return(dataset_id)
      }
      
      # 2. 忽略大小写和特殊字符匹配
      idx <- which(clean_id(col_names) == clean_id(dataset_id))
      
      if (length(idx) > 0) {
        return(col_names[idx[1]])
      }
      
      NA_character_
    }
    
    extract_dataset_column_from_dt <- function(dt, dataset_id) {
      if (is.null(dt)) return(NULL)
      if (nrow(dt) == 0) return(NULL)
      
      matched_col <- match_dataset_column(names(dt), dataset_id)
      
      if (is.na(matched_col)) {
        return(NULL)
      }
      
      # 尽量保留基因名 / ID 列
      id_candidates <- c(
        "row_id",
        "gene_name",
        "Gene",
        "gene",
        "gene_symbol",
        "GeneSymbol",
        "gene_id",
        "GeneID",
        "ensembl_gene_id",
        "EntrezID",
        "entrez_id",
        "id",
        "ID"
      )
      
      id_cols <- intersect(id_candidates, names(dt))
      
      if (length(id_cols) > 0) {
        out <- dt[, c(id_cols[1], matched_col), with = FALSE]
      } else {
        out <- data.table(
          row_id = seq_len(nrow(dt)),
          value = dt[[matched_col]]
        )
      }
      
      # 表达值列名统一改成当前 Dataset
      value_col <- setdiff(names(out), names(out)[1])
      setnames(out, value_col, dataset_id)
      
      out
    }
    
    extract_dataset_column_from_rdata <- function(rdata_file, dataset_id) {
      real_file <- resolve_file(rdata_file)
      
      if (is.na(real_file)) {
        return(NULL)
      }
      
      e <- new.env()
      
      nms <- tryCatch(
        load(real_file, envir = e),
        error = function(err) character(0)
      )
      
      if (length(nms) == 0) {
        return(NULL)
      }
      
      # ================== 情况 1：rdata 里直接是 data.frame / data.table / matrix ==================
      for (nm in nms) {
        obj <- e[[nm]]
        dt <- object_to_dt(obj)
        
        ans <- extract_dataset_column_from_dt(dt, dataset_id)
        
        if (!is.null(ans)) {
          attr(ans, "source_rdata") <- real_file
          attr(ans, "source_object") <- nm
          return(ans)
        }
      }
      
      # ================== 情况 2：rdata 里是 list ==================
      for (nm in nms) {
        obj <- e[[nm]]
        
        if (is.list(obj) && !is.data.frame(obj)) {
          list_names <- names(obj)
          
          # 2.1 list 的元素名就是 Dataset
          if (!is.null(list_names)) {
            matched_name <- match_dataset_column(list_names, dataset_id)
            
            if (!is.na(matched_name)) {
              vec <- obj[[matched_name]]
              
              # 如果 list 里对应元素本身是 data.frame/matrix，也继续按列提取
              dt <- object_to_dt(vec)
              
              if (!is.null(dt)) {
                ans <- extract_dataset_column_from_dt(dt, dataset_id)
                
                if (!is.null(ans)) {
                  attr(ans, "source_rdata") <- real_file
                  attr(ans, "source_object") <- paste0(nm, "$", matched_name)
                  return(ans)
                }
              }
              
              # 否则按向量处理
              out <- data.table(
                row_id = seq_along(as.vector(vec)),
                value = as.vector(vec)
              )
              
              setnames(out, "value", dataset_id)
              
              attr(out, "source_rdata") <- real_file
              attr(out, "source_object") <- paste0(nm, "$", matched_name)
              
              return(out)
            }
          }
          
          # 2.2 list 的元素里还有 data.frame / matrix
          if (!is.null(list_names)) {
            for (sub_nm in list_names) {
              dt <- object_to_dt(obj[[sub_nm]])
              ans <- extract_dataset_column_from_dt(dt, dataset_id)
              
              if (!is.null(ans)) {
                attr(ans, "source_rdata") <- real_file
                attr(ans, "source_object") <- paste0(nm, "$", sub_nm)
                return(ans)
              }
            }
          }
        }
      }
      
      NULL
    }
    
    find_existing_csv <- function(dataset_id) {
      dataset_id <- as.character(dataset_id)
      
      candidates <- unique(c(
        file.path(csv_dir, paste0(dataset_id, ".csv")),
        file.path(data_dir, paste0(dataset_id, ".csv")),
        paste0(dataset_id, ".csv")
      ))
      
      candidates <- candidates[file.exists(candidates)]
      
      if (length(candidates) == 0) {
        return(NA_character_)
      }
      
      candidates[1]
    }
    
    get_download_ext <- function(dataset_id) {
      csv_file <- find_existing_csv(dataset_id)
      
      if (!is.na(csv_file)) {
        return("csv")
      }
      
      "txt"
    }
    
    write_download_file <- function(dataset_id, row_dt, file) {
      dataset_id <- as.character(dataset_id)
      
      # ================== 1. 已有 CSV，直接下载 CSV ==================
      csv_file <- find_existing_csv(dataset_id)
      
      if (!is.na(csv_file)) {
        file.copy(csv_file, file, overwrite = TRUE)
        return(invisible(TRUE))
      }
      
      # ================== 2. 没有 CSV，去该 metadata 对应的数据 rdata 中提取 Dataset 同名列 ==================
      source_data_file <- NA_character_
      
      if (".source_data_file" %in% names(row_dt)) {
        source_data_file <- as.character(row_dt$.source_data_file[1])
      }
      
      if (is.na(source_data_file) || source_data_file == "") {
        error_msg <- data.table(
          GEM_ID = dataset_id,
          note = "Cannot identify corresponding data file for this metadata row."
        )
        
        fwrite(
          error_msg,
          file = file,
          sep = "\t",
          quote = FALSE,
          na = "NA"
        )
        
        return(invisible(TRUE))
      }
      
      ans <- extract_dataset_column_from_rdata(
        rdata_file = source_data_file,
        dataset_id = dataset_id
      )
      
      # ================== 3. 成功提取，txt 只写对应列 ==================
      if (!is.null(ans)) {
        fwrite(
          ans,
          file = file,
          sep = "\t",
          quote = FALSE,
          na = "NA"
        )
        
        return(invisible(TRUE))
      }
      
      # ================== 4. 提取失败，写干净错误说明 ==================
      error_msg <- data.table(
        GEM_ID = dataset_id,
        note = "No data file was found for this dataset."
      )
      
      fwrite(
        error_msg,
        file = file,
        sep = "\t",
        quote = FALSE,
        na = "NA"
      )
      
      invisible(TRUE)
    }
    
    # ================== 加载并合并多个 metadata.fst ==================
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
        dt[, `.source_data_file` := get_data_file_from_meta(meta_file)]
        
        dt_list[[length(dt_list) + 1]] <- dt
      }
      
      if (length(dt_list) == 0) {
        return(NULL)
      }
      
      rbindlist(dt_list, fill = TRUE, use.names = TRUE)
    }
    
    # Sanitize encoding: replace invalid byte sequences with hex, avoiding tolower() errors
    sanitize_encoding <- function(df) {
      for (col in names(df)) {
        if (is.character(df[[col]])) {
          v <- df[[col]]
          # validUTF8() 是 C 级向量化判断，比 iconv() 快一个量级。绝大多数列本来就是
          # 合法 UTF-8（validUTF8(NA) 也是 TRUE），直接跳过，省掉整表 iconv 的开销。
          # 首屏渲染路径上这个函数会被调用两次（filtered_* 一次、to_display_df 一次），
          # 全表 iconv 在这里就是纯浪费。
          if (!all(validUTF8(v))) {
            set(df, j = col, value = iconv(v, from = "UTF-8", to = "UTF-8", sub = "byte"))
          }
        }
      }
      df
    }

    # 合并分片。用 base::order() 而不是 data.table::setorderv()：
    # 在部分机器（尤其是外部网页服务器那种小规格 / 老版本环境）上，
    # setorderv(rbindlist(...)) 会踩到 data.table 内部的 .check_tzones /
    # lengths() 递归，抛 "节点堆叠上溢"（C stack overflow），
    # 导致该分片被 tryCatch 吞掉 —— 表现就是表只有 3 万行、后面几个文件全丢了。
    # order() 是 base 实现，没有这条路径，行为一致但更稳。
    merge_parts <- function(parts, col = ".gem_id_raw") {
      if (is.null(parts) || length(parts) == 0L) return(NULL)
      merged <- rbindlist(parts, fill = TRUE, use.names = TRUE)
      if (!is.null(col) && col %in% names(merged)) {
        merged <- merged[order(merged[[col]])]
      }
      merged
    }

    # ================== Display-ready 数据 ==================
    # 渲染输出与增量 replaceData() 共用同一份转换逻辑，保证两者列集/列序完全一致。
    to_display_df <- function(df, type = "genetic") {
      df <- sanitize_encoding(copy(df))
      if ("GEM_ID" %in% names(df)) setcolorder(df, c("GEM_ID", setdiff(names(df), "GEM_ID")))
      # Rename merge key "Dataset" to avoid conflict and "GEM_ID" to "Dataset" for display
      if ("Dataset" %in% names(df)) setnames(df, "Dataset", ".Dataset")
      if ("GEM_ID" %in% names(df))  setnames(df, "GEM_ID", "Dataset")
      # Display stored GEM_ID as clickable link
      df[, Dataset := sprintf(
        '<span class="dataset-link" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority: \'event\'})">%s</span>',
        ns("click_dataset"), .Dataset, Dataset
      )]
      if (type == "chemical") {
        # Rename Perturbagen to PbChem for display
        if ("Perturbagen" %in% names(df)) setnames(df, "Perturbagen", "PbChem")
      } else {
        # Capitalize first letter of displayed column names
        cap_cols <- intersect(c("pbgene", "method"), names(df))
        for (cc in cap_cols) setnames(df, cc, paste0(toupper(substr(cc, 1, 1)), substr(cc, 2, nchar(cc))))
      }
      replace_empty_with_dash(df, exclude_cols = c("Dataset", ".Dataset", ".gem_id_raw"))
    }
    
    dt_options <- function(df, type = "chemical") {
      # Note: "Dataset" merge key is renamed to ".Dataset" in render functions before this is called
      always_hidden <- c(".source_data_file", ".gem_id_raw", ".Dataset")
      if (type == "genetic") {
        hidden_cols <- which(names(df) %in% c(always_hidden, "pb_ENSEMBL", "gene_biotype", "Type",
                                               "Perturbagen", "Dose", "Time")) - 1
      } else {
        hidden_cols <- which(names(df) %in% c(always_hidden, "Type",
                                               "pbgene", "pb_ENSEMBL", "gene_biotype", "method")) - 1
      }
      
      opt <- list(
        pageLength = 15,
        scrollX = TRUE,
        search = list(regex = FALSE),  # plain-text search to avoid regex errors on HTML columns
        columnDefs = list(
          list(visible = FALSE, targets = hidden_cols),
          dt_dash_render()
        )
      )
      
      opt
    }
    
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
        # First, sort within each Accession by completeness (na_count asc)
        setorderv(df, c("Accession", ".__na_count"))
        # Then assign global round-robin rank per Accession
        df[, `.__acc_rank` := seq_len(.N), by = Accession]
        # Sort: GSE first (if enabled), then round-robin, then completeness
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
    
    # ================== 缓存环境 ==================
    data_env <- new.env(parent = emptyenv())
    
    # ================== GSEA 数据集清单（磁盘缓存）==================
    # list.files() 一个含十万级文件的目录非常慢（Genetic ~2.3s，Chemical ~23s），
    # 这里把清单缓存到 metadata 目录，仅当目录 mtime 变化时重建。
    gsea_dataset_index <- function(type = "Genetic") {
      dir_now <- file.path(GSEA_DIR, type)
      cache_file <- file.path(meta_fst_dir, paste0(".gsea_index_", type, ".rds"))
      # 进程级内存缓存优先：GSEA/Genetic 有 17.9 万个文件，list.files() 本地就要 2.3s，
      # 网页端（慢磁盘/网络盘）更久。若目录 mtime 与磁盘缓存不一致（部署时文件被重新拷贝，
      # mtime 会变），每个会话都会重扫一遍；内存缓存可以让整个 worker 只扫一次。
      mem_key <- paste0("gsea_index_", type)
      if (!is.null(.gemap_shared[[mem_key]])) return(.gemap_shared[[mem_key]])

      dir_mtime <- tryCatch(as.numeric(file.mtime(dir_now)), error = function(e) NA_real_)
      cached <- if (file.exists(cache_file)) tryCatch(readRDS(cache_file), error = function(e) NULL) else NULL
      if (!is.null(cached) && length(cached$datasets) > 0) {
        .gemap_shared[[mem_key]] <- cached$datasets
        # mtime 不一致时（部署/拷贝会把目录 mtime 刷成当前时间）**不要阻塞首屏**：
        # 直接扫 17.9 万个文件在网页服务器上可能要几十秒，用户会以为网站卡死。
        # 数据集清单极少变动，先用旧清单秒出，再后台重扫刷新缓存。
        if (!identical(cached$mtime, dir_mtime)) gsea_index_refresh(type)
        return(cached$datasets)
      }
      datasets <- sub("\\.fst$", "", list.files(dir_now, pattern = "\\.fst$"))
      tryCatch(saveRDS(list(mtime = dir_mtime, datasets = datasets), cache_file),
               error = function(e) NULL)
      # 即使 saveRDS 失败（部署目录不可写），内存缓存仍然生效
      if (length(datasets) > 0) .gemap_shared[[mem_key]] <- datasets
      datasets
    }

    # 后台重扫 GSEA 目录并刷新磁盘缓存：每个 worker 只做一次，且完全不阻塞页面
    gsea_index_refresh <- function(type) {
      flag <- paste0("gsea_index_", type, "_refreshing")
      if (isTRUE(.gemap_shared[[flag]])) return(invisible(NULL))
      .gemap_shared[[flag]] <- TRUE
      schedule_step(function() {
        dir_now <- file.path(GSEA_DIR, type)
        cache_file <- file.path(meta_fst_dir, paste0(".gsea_index_", type, ".rds"))
        mem_key <- paste0("gsea_index_", type)
        dir_mtime <- tryCatch(as.numeric(file.mtime(dir_now)), error = function(e) NA_real_)
        datasets <- sub("\\.fst$", "", list.files(dir_now, pattern = "\\.fst$"))
        if (length(datasets) > 0) {
          .gemap_shared[[mem_key]] <- datasets
          tryCatch(saveRDS(list(mtime = dir_mtime, datasets = datasets), cache_file),
                   error = function(e) NULL)
        }
      }, delay = 3)
      invisible(NULL)
    }

    # ================== 分片装配：metadata 文件 → 完整派生列 ==================
    # 每个分片独立完成 read_fst → 过滤 → merge → 标准化 → 派生列；
    # 这些步骤都是行内（row-wise）的，因此分片累加的结果与一次性全量装配完全一致，
    # 从而支持"第一片装配完就先渲染，其余分片陆续追加"。
    assemble_meta_part <- function(meta_files, type = "genetic", existing_datasets = NULL) {
      df <- load_metadata_files(meta_files)
      if (is.null(df) || nrow(df) == 0) return(NULL)

      if (type == "genetic") {
        # 移除 GSEA/Genetic 下没有对应数据文件的 GuoziData 条目
        guozi_idx <- which(df$.source_meta_file == "GuoziDataMetadata.fst")
        if (length(guozi_idx) > 0 && !is.null(existing_datasets)) {
          remove_rows <- guozi_idx[!(df$Dataset[guozi_idx] %in% existing_datasets)]
          if (length(remove_rows) > 0) {
            message(sprintf("[Browse] Filtered out %d GuoziData entries with missing signature data", length(remove_rows)))
            df <- df[!seq_len(nrow(df)) %in% remove_rows]
          }
        }
        # 移除 "inhibit" method 条目
        if (nrow(df) > 0 && "method" %in% names(df)) {
          inhibit_count <- sum(df$method == "inhibit", na.rm = TRUE)
          if (inhibit_count > 0) {
            message(sprintf("[Browse] Filtered out %d 'inhibit' method entries", inhibit_count))
            df <- df[!(method == "inhibit" & !is.na(method))]
          }
        }
        if (nrow(df) == 0) return(NULL)
      }

      df[, ".source_meta_file" := NULL]

      # ---- Merge PMID/Journal from accession mapping table ----
      acc_map <- load_accession_map()
      if (!is.null(acc_map) && "Accession" %in% names(df)) {
        df[, PMID := NULL]
        df <- merge(df, copy(acc_map), by = "Accession", all.x = TRUE, sort = FALSE)
        pmid_direct <- grepl("^PMID:", df$Accession) & (is.na(df$PMID) | df$PMID == "")
        df[pmid_direct, PMID := sub("^PMID: ?", "", Accession)]
        df[PMID == "29195078" & (is.na(Journal) | Journal == ""), Journal := "Cell"]
        df[!is.na(PMID) & PMID != "", PMID := sprintf(
          '<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>',
          PMID, PMID
        )]
      }

      # ---- Standardize Tissue / Cell line ----
      df <- standardize_tissue(df)
      df <- standardize_cell_if_needed(df)

      if (type == "chemical" && "Time" %in% names(df)) {
        # 移除 Time 为空的行
        df <- df[!is.na(Time) & as.character(Time) != ""]
      }
      if (nrow(df) == 0) return(NULL)

      if (!("Dataset" %in% names(df))) setnames(df, 1, "Dataset", skip_absent = TRUE)
      if (!("Dataset" %in% names(df))) df[, Dataset := paste0("dataset_", .I)]

      # ---- GEM_ID ----
      if (!("GEM_ID" %in% names(df))) {
        df[, GEM_ID := Dataset]
      } else {
        missing <- is.na(df$GEM_ID) | df$GEM_ID == ""
        df$GEM_ID[missing] <- df$Dataset[missing]
      }
      df[, `.gem_id_raw` := GEM_ID]

      # ---- 派生列：下载按钮 + 可点击 GEM_ID 链接（每个分片只生成一次）----
      df[, Download := sprintf(
        '<button class="btn btn-sm btn-success dl-btn" data-ds="%s" data-gem="%s"><i class="fa fa-download"></i> Download</button>',
        gsub('"', '&quot;', as.character(Dataset), fixed = TRUE),
        gsub('"', '&quot;', as.character(GEM_ID), fixed = TRUE)
      )]
      click_id <- ns("click_dataset")
      df[, GEM_ID := sprintf(
        '<span class="dataset-link" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority: \'event\'})">%s</span>',
        click_id, Dataset, GEM_ID
      )]

      # 同样用 base::order 代替 setorderv（见 merge_parts 的注释：setorderv 在部分
      # 外部环境下会踩到 data.table 内部递归导致 C stack overflow）
      if (".gem_id_raw" %in% names(df)) df <- df[order(.gem_id_raw)]
      df
    }

    # ================== 分片渐进加载 ==================
    # genetic：第一片即可渲染，因为首屏 ID 就是连续的（GuoziData 包含 PSG00001+）。
    # chemical：处理完第 3 个元数据文件（HerbDataMetadata）后再渲染首屏。第 1 片
    # CIGSMetadata 的 ID 从 PSC00009 开始且大量缺号，首屏看起来"编号都是乱的"；等
    # HerbDataMetadata 把 PSC00001-00008 补齐，首屏才连续。前 3 个文件都是小文件
    # （合计约 0.5s），真正的耗时大户 LINCS_2020/LINCS_L1000 放在首屏之后。
    # 其余分片陆续装配但不碰表格；全部装完后用 replaceData() 就地换数据 —— 不重绘、
    # 不闪、不重置分页，也不会出现"表格又刷新一次"。
    # 注意：不要在中间分片里反复 replaceData，每次都要重做整表 display 派生（92 万行 ≈ 10s）。
    gen_note  <- reactiveVal("")
    chem_note <- reactiveVal("")

    # 加载提示做成显眼的卡片，而不是一行小灰字：
    # 数据没就绪时 renderDT 会 req(FALSE) 挂起，DTOutput 区域是**完全空白**的
    # （withSpinner 只在 recalculating 时出现，挂起的输出不触发）。
    # 外部网页第一个访客要装配/索引几十秒，如果只有空白，就会以为网站坏了。
    .load_note_css <- tags$style(shiny::HTML(
      ".gemap-load-box{display:flex;align-items:center;gap:14px;
        border:1px solid #d6e2f0;border-left:4px solid #3a7bd5;background:#f5f9ff;
        border-radius:8px;padding:14px 16px;margin:6px 0 10px 0;}
       .gemap-load-title{font-size:14px;font-weight:600;color:#2c5f8a;}
       .gemap-load-sub{font-size:12px;color:#6b7a8c;margin-top:3px;}
       .gemap-spinner{width:26px;height:26px;flex:0 0 26px;border:3px solid #cfe0f5;
        border-top-color:#3a7bd5;border-radius:50%;animation:gemap-spin .9s linear infinite;}
       @keyframes gemap-spin{to{transform:rotate(360deg);}}"
    ))

    # box_id 用于脚本隐藏自己，table_id 是该卡片对应的 DT 输出容器
    load_note_ui <- function(box_id, table_id, title, note = NULL) {
      tagList(
        .load_note_css,
        div(
          id = box_id, class = "gemap-load-box",
          div(class = "gemap-spinner"),
          div(
            div(class = "gemap-load-title", title),
            if (!is.null(note) && nzchar(note)) div(class = "gemap-load-sub", note),
            div(class = "gemap-load-sub",
                "First visit on a fresh server takes ~20-60 s to build the index. ",
                "It is cached afterwards, so later visits open instantly.")
          )
        ),
        # 表格真正画出来（tbody 有行）之后才把卡片收起来，避免出现"卡片没了、表格还没来"的空窗
        tags$script(shiny::HTML(sprintf(
          "(function(){var box=document.getElementById(%s),tid=%s,n=0;
            if(!box)return;
            var iv=setInterval(function(){
              n++;
              var c=document.getElementById(tid);
              if(c&&c.querySelectorAll('tbody tr').length>0){box.style.display='none';clearInterval(iv);}
              else if(n>600){clearInterval(iv);}
            },300);})();",
          shQuote(box_id, type = "cmd"),
          shQuote(table_id, type = "cmd")
        )))
      )
    }

    output$load_note_gen <- renderUI({
      gen_trigger(); note <- gen_note()
      if (is.null(data_env$df2) || nrow(data_env$df2) == 0) {
        load_note_ui(session$ns("gen_load_box"), session$ns("table2"),
                     "Loading genetic perturbagen metadata ...", note)
      } else if (!is.null(note) && nzchar(note)) {
        div(class = "text-muted", style = "font-size:12px;min-height:18px;", note)
      } else {
        div(style = "min-height:18px;")
      }
    })

    output$load_note_chem <- renderUI({
      chem_trigger(); note <- chem_note()
      if (is.null(data_env$df1) || nrow(data_env$df1) == 0) {
        load_note_ui(session$ns("chem_load_box"), session$ns("table1"),
                     "Loading chemical perturbagen metadata ...", note)
      } else if (!is.null(note) && nzchar(note)) {
        div(class = "text-muted", style = "font-size:12px;min-height:18px;", note)
      } else {
        div(style = "min-height:18px;")
      }
    })

    # ---- 分片调度的统一外壳 ----
    # later::later() 的回调既不在 reactive domain 也不在 reactive context 中执行：
    #   * 读取 reactiveVal（如 gen_trigger()）会直接抛
    #     "Operation not allowed without an active reactive context"；
    #   * DT::dataTableProxy()/session$sendCustomMessage() 需要 domain。
    # 所以这里统一包一层 withReactiveDomain(session, isolate(...))，
    # 并在会话已关闭时停止调度。
    schedule_step <- function(step, delay = 0.1) {
      later::later(function() {
        closed <- isTRUE(tryCatch(session$isClosed(), error = function(e) FALSE))
        if (closed) return(invisible(NULL))
        tryCatch(
          shiny::withReactiveDomain(session, shiny::isolate(step())),
          error = function(e) {
            message("[Browse] load step failed: ", conditionMessage(e))
          }
        )
      }, delay)
      invisible(NULL)
    }

    # ---- Genetic ----
    load_gen_data <- function() {
      if (isTRUE(data_env$loaded_gen) || isTRUE(data_env$loading_gen)) return()
      # 快路径：别的会话已经装配过，直接复用（秒开），
      # 不再重读 fst、也不再重扫 GSEA/Genetic 目录（17.9 万个文件）
      if (!is.null(.gemap_shared$df2) && nrow(.gemap_shared$df2) > 0) {
        data_env$df2          <- .gemap_shared$df2
        data_env$gen_display  <- .gemap_shared$gen_display
        data_env$loading_gen  <- FALSE
        data_env$loaded_gen   <- TRUE
        gen_trigger(gen_trigger() + 1)
        return(invisible(NULL))
      }
      # 磁盘缓存：新 worker / 重启后的"第一个访客"直接读回装配好的总表
      if (.gemap_cache_valid("gen_df2", gen_meta_files)) {
        cdf2 <- .gemap_cache_load("gen_df2")
        if (!is.null(cdf2) && nrow(cdf2) > 0) {
          data_env$df2 <- cdf2
          .gemap_shared$df2 <- cdf2
          if (.gemap_cache_valid("gen_display", gen_meta_files)) {
            cdisp <- .gemap_cache_load("gen_display")
            if (!is.null(cdisp) && nrow(cdisp) == nrow(cdf2)) {
              data_env$gen_display <- cdisp
              .gemap_shared$gen_display <- cdisp
            }
          }
          data_env$loading_gen <- FALSE
          data_env$loaded_gen  <- TRUE
          gen_trigger(gen_trigger() + 1)
          return(invisible(NULL))
        }
      }
      data_env$loading_gen      <- TRUE
      data_env$gen_parts        <- list()
      data_env$gen_display      <- NULL
      data_env$gen_display_cols <- NULL
      data_env$gen_queue        <- gen_meta_files
      data_env$gen_nfiles       <- length(gen_meta_files)
      gen_note(sprintf("Loading genetic perturbagen metadata ... 0/%d sources", data_env$gen_nfiles))
      # 交给事件循环：不阻塞会话启动，其余 UI 先绘制
      schedule_step(gen_load_step, 0.05)
    }

    gen_load_step <- function() {
      q <- data_env$gen_queue
      if (is.null(q) || length(q) == 0) {
        data_env$gen_queue   <- NULL
        data_env$loading_gen <- FALSE
        data_env$loaded_gen  <- TRUE
        if (length(data_env$gen_parts) == 0L) {
          gen_note("Failed to load genetic metadata")
          return(invisible(NULL))
        }
        # 全量已装配进 df2：分片只在装配阶段用，留着就是纯浪费（约 0.2GB）。
        # 网页端内存有限，这里立刻释放并 gc，避免峰值把 worker 挤爆（表现为 Ajax error）。
        data_env$gen_parts <- list()
        gc(verbose = FALSE)
        # 装配结果对所有会话相同，存进进程级缓存，后续访客直接复用
        .gemap_shared$df2 <- data_env$df2
        # 同时落盘：后续新建的 worker / 重启后的第一个访客直接读回
        .gemap_cache_save("gen_df2", data_env$df2, gen_meta_files)
        if (is.null(data_env$gen_display_cols)) {
          gen_trigger(gen_trigger() + 1)      # 首屏还没渲染过（例如表格当时不可见），整表渲染
          return(invisible(NULL))
        }
        # 首屏已经在显示了：先把"准备表格"的提示 flush 给浏览器，再做整表 display 派生，
        # 最后用 replaceData() 就地换数据 —— 不重绘、不闪、不重置分页、不重新出现整张表。
        gen_note(sprintf("Preparing genetic table ... %s rows", format(nrow(data_env$df2), big.mark = ",")))
        schedule_step(function() {
          refresh_gen_table()
          gen_note("")
        })
        return(invisible(NULL))
      }
      # 会话是单线程的：如果当前可见的标签不是自己、而对方的加载还没结束，
      # 就把这一轮整个让出去，避免首屏被对方的装配步骤挤在后面。
      if (isTRUE(data_env$loading_chem) && identical(data_env$active_tab, "Chemical Perturbagens")) {
        schedule_step(gen_load_step, 0.3)
        return(invisible(NULL))
      }
      data_env$gen_queue <- q[-1]
      existing <- if (identical(basename(q[1]), "GuoziDataMetadata.fst")) gsea_dataset_index("Genetic") else NULL
      part <- tryCatch(
        assemble_meta_part(q[1], type = "genetic", existing_datasets = existing),
        error = function(e) {
          message("[Browse] failed to assemble ", q[1], ": ", conditionMessage(e))
          NULL
        }
      )
      # 合并进总表 / 刷新表格：任一分片出错都不能中断整个加载循环
      tryCatch({
        if (!is.null(part) && nrow(part) > 0) {
          data_env$gen_parts[[length(data_env$gen_parts) + 1]] <- part
          data_env$df2 <- merge_parts(data_env$gen_parts)
          if (length(data_env$gen_parts) == 1L) {
            gen_trigger(gen_trigger() + 1)      # 第一片：先把东西显示出来
          }
          # 后续分片不碰表格：等全部装完再用 replaceData() 就地补进（见 gen_load_step 收尾）
          gen_note(sprintf("Loading genetic perturbagen metadata ... %d/%d sources, %s rows",
                           data_env$gen_nfiles - length(data_env$gen_queue),
                           data_env$gen_nfiles,
                           format(nrow(data_env$df2), big.mark = ",")))
        }
      }, error = function(e) {
        message("[Browse] failed to merge genetic part ", q[1], ": ", conditionMessage(e))
      })
      # 让出事件循环：浏览器先把已就绪的数据绘制出来，再装配下一片
      schedule_step(gen_load_step)
    }

    # ---- Chemical ----
    load_chem_data <- function() {
      if (isTRUE(data_env$loaded_chem) || isTRUE(data_env$loading_chem)) return()
      # 快路径：别的会话已经装配过，直接复用（秒开），不再重读 fst / 重装配
      if (!is.null(.gemap_shared$df1) && nrow(.gemap_shared$df1) > 0) {
        data_env$df1            <- .gemap_shared$df1
        data_env$chem_display   <- .gemap_shared$chem_display
        data_env$loading_chem   <- FALSE
        data_env$loaded_chem    <- TRUE
        chem_trigger(chem_trigger() + 1)
        return(invisible(NULL))
      }
      # 磁盘缓存：新 worker / 重启后的"第一个访客"直接读回装配好的总表，
      # 不再重读 5 个 fst + 逐片装配（网页小规格机器上这一步要几十秒）。
      if (.gemap_cache_valid("chem_df1", chem_meta_files)) {
        cdf1 <- .gemap_cache_load("chem_df1")
        if (!is.null(cdf1) && nrow(cdf1) > 0) {
          data_env$df1 <- cdf1
          .gemap_shared$df1 <- cdf1
          # display 一起读回：连"派生 display"这一步都省掉（模块 id 固定为 browse，
          # 里面的 onclick / data-ds 用到的 ns 前缀不随会话变化，可安全复用）
          if (.gemap_cache_valid("chem_display", chem_meta_files)) {
            cdisp <- .gemap_cache_load("chem_display")
            if (!is.null(cdisp) && nrow(cdisp) == nrow(cdf1)) {
              data_env$chem_display <- cdisp
              .gemap_shared$chem_display <- cdisp
            }
          }
          data_env$loading_chem <- FALSE
          data_env$loaded_chem  <- TRUE
          chem_trigger(chem_trigger() + 1)
          return(invisible(NULL))
        }
      }
      data_env$loading_chem        <- TRUE
      data_env$chem_parts          <- list()
      data_env$chem_display        <- NULL
      data_env$chem_display_cols   <- NULL
      data_env$chem_wait_render    <- FALSE
      data_env$chem_render_deadline <- NULL
      data_env$chem_done           <- 0L
      data_env$chem_queue          <- chem_meta_files
      data_env$chem_nfiles         <- length(chem_meta_files)
      chem_note(sprintf("Loading chemical perturbagen metadata ... 0/%d sources", data_env$chem_nfiles))
      schedule_step(chem_load_step, 0.05)
    }

    chem_load_step <- function() {
      q <- data_env$chem_queue
      if (is.null(q) || length(q) == 0) {
        data_env$chem_queue   <- NULL
        data_env$loading_chem <- FALSE
        data_env$loaded_chem  <- TRUE
        if (length(data_env$chem_parts) == 0L) {
          chem_note("Failed to load chemical metadata")
          return(invisible(NULL))
        }
        # 全量已装配进 df1：分片只在装配阶段用，留着就是纯浪费（约 0.7GB）。
        # 全量 chemical 是 92 万行 / 21 列，display 表本身就要 0.8GB，网页端内存有限，
        # 必须立刻释放分片并 gc，否则峰值内存会挤爆 worker（表现为表格 Ajax error）。
        data_env$chem_parts <- list()
        gc(verbose = FALSE)
        # 装配结果对所有会话相同，存进进程级缓存，后续访客直接复用
        .gemap_shared$df1 <- data_env$df1
        # 同时落盘：后续新建的 worker / 重启后的第一个访客直接读回，不用再装配
        .gemap_cache_save("chem_df1", data_env$df1, chem_meta_files)
        if (is.null(data_env$chem_display_cols)) {
          chem_trigger(chem_trigger() + 1)    # 首屏还没渲染过，整表渲染
          return(invisible(NULL))
        }
        # 首屏已经在显示了：92 万行的 display 派生放在后台步骤里做，再用 replaceData()
        # 就地换数据 —— 不重绘、不闪、不重置分页。
        chem_note(sprintf("Preparing chemical table ... %s rows", format(nrow(data_env$df1), big.mark = ",")))
        schedule_step(function() {
          refresh_chem_table()
          chem_note("")
        })
        return(invisible(NULL))
      }
      # 如果还在等首屏渲染，不要装配下一片；让浏览器/R 优先完成表格渲染
      if (isTRUE(data_env$chem_wait_render)) {
        if (!is.null(data_env$chem_display_cols) || Sys.time() > data_env$chem_render_deadline) {
          data_env$chem_wait_render <- FALSE
        } else {
          schedule_step(chem_load_step, 0.2)
          return(invisible(NULL))
        }
      }

      # 会话是单线程的：如果当前可见的标签不是自己、而对方的加载还没结束，
      # 就把这一轮整个让出去，避免首屏被对方的装配步骤挤在后面。
      # （启动时会在后台预加载 genetic，用户切到 Chemical 时它还在跑，必须让路。）
      if (isTRUE(data_env$loading_gen) && !identical(data_env$active_tab, "Chemical Perturbagens")) {
        schedule_step(chem_load_step, 0.05)
        return(invisible(NULL))
      }

      data_env$chem_queue <- q[-1]
      data_env$chem_done  <- data_env$chem_done + 1L
      part <- tryCatch(
        assemble_meta_part(q[1], type = "chemical"),
        error = function(e) {
          message("[Browse] failed to assemble ", q[1], ": ", conditionMessage(e))
          NULL
        }
      )
      tryCatch({
        if (!is.null(part) && nrow(part) > 0) {
          data_env$chem_parts[[length(data_env$chem_parts) + 1]] <- part
          data_env$df1 <- merge_parts(data_env$chem_parts)
        }
        # 首屏：处理完第 3 个元数据文件（HerbDataMetadata）后 PSC00001-00008 才补齐，
        # 首屏 ID 才连续。这里必须按"已处理文件数"计数，不能用 length(chem_parts)：
        # CIGSData2Metadata 装配出来是 0 行、不会进 chem_parts，用后者会把首屏推迟到
        # 装完 LINCS_2020（约 9 秒）之后 —— 表要等 10 秒才出来。
        if (is.null(data_env$chem_display_cols) && data_env$chem_done >= 3L) {
          chem_trigger(chem_trigger() + 1)    # 首屏 ID 已连续，可以渲染
          # 下一个文件是 LINCS_2020（61 万行、约 9 秒），会长时间霸占 R 会话导致
          # renderDT 执行不完。先等首屏真正渲染出来（或 3 秒超时）再继续装大文件。
          if (length(data_env$chem_queue) > 0L) {
            data_env$chem_wait_render      <- TRUE
            data_env$chem_render_deadline  <- Sys.time() + 3
            schedule_step(chem_load_step, 0.2)
            return(invisible(NULL))
          }
        }
        # 后续分片不碰表格：等全部装完再用 replaceData() 就地补进（见 chem_load_step 收尾）
        chem_note(sprintf("Loading chemical perturbagen metadata ... %d/%d sources, %s rows",
                          data_env$chem_done,
                          data_env$chem_nfiles,
                          format(if (is.null(data_env$df1)) 0 else nrow(data_env$df1), big.mark = ",")))
      }, error = function(e) {
        message("[Browse] failed to merge chemical part ", q[1], ": ", conditionMessage(e))
      })
      # 前 3 个文件连续快速装配（不 yield 0.1s），让"编号正常"的首屏尽早出来；
      # 之后恢复 0.1s yield，保证浏览器能刷新进度提示、UI 不卡。
      # 用"已处理文件数"而非"已装配片数"计数，避免空片/失败片把后面的大文件拖进快速通道。
      schedule_step(chem_load_step, if (data_env$chem_done < 3L) 0 else 0.1)
    }

    # ================== 启动时加载默认标签 ==================
    # 只把任务排进事件循环，不会阻塞会话启动
    # 当前可见标签：两个加载器共用同一个 R 会话，靠它决定谁优先（默认落在 Genetic 上）
    data_env$active_tab <- "Genetic Perturbagens"
    observe({
      load_gen_data()
    })
    
    # ================== 切换标签加载 ==================
    observeEvent(input$tabs, {
      data_env$active_tab <- input$tabs
      if (input$tabs == "Chemical Perturbagens") {
        load_chem_data()
      }
      
      if (input$tabs == "Genetic Perturbagens") {
        load_gen_data()
      }
    })

    
    # ================== 渲染表格 ==================
    # 在渲染时生成下载按钮（只对过滤后的行，不会阻塞启动加载）
    # 使用 data-* 属性 + 全局代理事件避免每行生成 onclick 和 URLencode 瓶颈
    render_download_btn <- function(df) {
      if (!nrow(df) || !("Dataset" %in% names(df)) || !("GEM_ID" %in% names(df))) return(df)
      df[, Download := sprintf(
        '<button class="btn btn-sm btn-success dl-btn" data-ds="%s" data-gem="%s"><i class="fa fa-download"></i> Download</button>',
        gsub('"', '&quot;', as.character(Dataset), fixed = TRUE),
        gsub('"', '&quot;', as.character(GEM_ID), fixed = TRUE)
      )]
      df
    }
    
    # Make GEM_ID column clickable, pass Dataset value behind the scenes
    render_gem_id_link <- function(df, ns) {
      if (!nrow(df) || !("GEM_ID" %in% names(df)) || !("Dataset" %in% names(df))) return(df)
      # Fill NA GEM_ID with Dataset value
      missing <- is.na(df$GEM_ID) | df$GEM_ID == ""
      df$GEM_ID[missing] <- df$Dataset[missing]
      df$GEM_ID <- sprintf(
        '<span class="dataset-link" onclick="Shiny.setInputValue(\'%s\', \'%s\', {priority: \'event\'})">%s</span>',
        ns("click_dataset"), df$Dataset, df$GEM_ID
      )
      df
    }
    
    # ===== Helper: normalize text by stripping special chars and lowering =====
    # Makes MCF7, MCF-7, mcf7, mcf-7, Mcf7 all match each other
    normalize_text <- function(x) {
      gsub("[^a-z0-9]", "", tolower(as.character(x)))
    }
    
    # ===== Multi-field Filter =====
    # Each field filters its own column (case-insensitive & special-char-insensitive partial match)
    # All non-empty fields are combined with AND logic
    apply_filters <- function(df, filters) {
      # 没有任何过滤条件时直接返回，省掉整表的 iconv 预处理
      if (!any(vapply(filters, function(v) !is.null(v) && nzchar(trimws(as.character(v))), logical(1)))) {
        return(df)
      }
      # Pre-sanitize all character columns to avoid "invalid multibyte string" errors in tolower()
      for (col in names(df)) {
        if (is.character(df[[col]])) {
          set(df, j = col, value = iconv(df[[col]], from = "UTF-8", to = "UTF-8", sub = "byte"))
        }
      }
      for (col in names(filters)) {
        val <- filters[[col]]
        if (is.null(val) || !nzchar(trimws(val))) next
        if (!col %in% names(df)) next
        pattern <- normalize_text(val)
        if (!nzchar(pattern)) next   # skip if only special chars entered
        df <- df[grepl(pattern, normalize_text(df[[col]]), fixed = TRUE)]
        if (nrow(df) == 0) break
      }
      df
    }
    
    # ===== Triggers for button-based search =====
    chem_trigger <- reactiveVal(0)
    gen_trigger  <- reactiveVal(0)
    
    # ===== Snapshot filter values to avoid race with updateTextInput =====
    chem_filters <- reactiveValues(
      Perturbagen  = "",
      Cell         = "",
      Tissue       = "",
      Oncotreecode = "",
      Accession    = ""
    )
    gen_filters <- reactiveValues(
      pbgene       = "",
      method       = "",
      Cell         = "",
      Tissue       = "",
      Oncotreecode = "",
      Accession    = ""
    )
    
    observeEvent(input$search_btn_chem, {
      chem_filters$Perturbagen  <- input$search_perturbagen
      chem_filters$Cell         <- input$search_cell_chem
      chem_filters$Tissue       <- input$search_tissue_chem
      chem_filters$Oncotreecode <- input$search_oncotree_chem
      chem_filters$Accession    <- input$search_accession_chem
      chem_trigger(chem_trigger() + 1)
    })
    
    observeEvent(input$search_btn_gen, {
      gen_filters$pbgene       <- input$search_gene
      gen_filters$method       <- input$search_method_gen
      gen_filters$Cell         <- input$search_cell_gen
      gen_filters$Tissue       <- input$search_tissue_gen
      gen_filters$Oncotreecode <- input$search_oncotree_gen
      gen_filters$Accession    <- input$search_accession_gen
      gen_trigger(gen_trigger() + 1)
    })
    
    # ===== Clear buttons: reset all filter inputs and trigger search =====
    observeEvent(input$clear_btn_chem, {
      updateTextInput(session, "search_perturbagen",   value = "")
      updateTextInput(session, "search_cell_chem",     value = "")
      updateTextInput(session, "search_tissue_chem",   value = "")
      updateTextInput(session, "search_oncotree_chem", value = "")
      updateTextInput(session, "search_accession_chem", value = "")
      chem_filters$Perturbagen  <- ""
      chem_filters$Cell         <- ""
      chem_filters$Tissue       <- ""
      chem_filters$Oncotreecode <- ""
      chem_filters$Accession    <- ""
      chem_trigger(chem_trigger() + 1)
    })
    
    observeEvent(input$clear_btn_gen, {
      updateTextInput(session, "search_gene",          value = "")
      updateTextInput(session, "search_method_gen",    value = "")
      updateTextInput(session, "search_cell_gen",      value = "")
      updateTextInput(session, "search_tissue_gen",    value = "")
      updateTextInput(session, "search_oncotree_gen",  value = "")
      updateTextInput(session, "search_accession_gen", value = "")
      gen_filters$pbgene       <- ""
      gen_filters$method       <- ""
      gen_filters$Cell         <- ""
      gen_filters$Tissue       <- ""
      gen_filters$Oncotreecode <- ""
      gen_filters$Accession    <- ""
      gen_trigger(gen_trigger() + 1)
    })
    
    # ===== Demo button: Genetic Perturbagens guided tour =====
    observeEvent(input$demo_btn_gen, {
      shinyjs::disable("demo_btn_gen")
      on.exit(shinyjs::enable("demo_btn_gen"))
      
      shinyalert(
        title = "Loading",
        text = "",
        type = "info",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        showConfirmButton = FALSE
      )
      
      # Step 1: Switch to Genetic Perturbagens tab
      updateTabsetPanel(session, "tabs", selected = "Genetic Perturbagens")
      
      # Step 2: Set filters and trigger search
      updateTextInput(session, "search_gene",          value = "CDK4")
      updateTextInput(session, "search_method_gen",    value = "")
      updateTextInput(session, "search_cell_gen",      value = "MCF7")
      updateTextInput(session, "search_tissue_gen",    value = "")
      updateTextInput(session, "search_oncotree_gen",  value = "")
      updateTextInput(session, "search_accession_gen", value = "")
      gen_filters$pbgene       <- "CDK4"
      gen_filters$method       <- ""
      gen_filters$Cell         <- "MCF7"
      gen_filters$Tissue       <- ""
      gen_filters$Oncotreecode <- ""
      gen_filters$Accession    <- ""
      gen_trigger(gen_trigger() + 1)
      
      # Step 3: Wait for table to render, then find PSG59343 and run everything
      shinyjs::delay(800, {
        df <- filtered_gen()
        if (is.null(df) || nrow(df) == 0) {
          shinyalert::closeAlert()
          shinyalert("Demo Error", "No results for CDK4 in MCF7.", type = "error")
        } else {
          target <- "PSG135188"
          match_idx <- NULL
          if (".gem_id_raw" %in% names(df)) {
            match_idx <- which(df$.gem_id_raw == target)
          }
          if (length(match_idx) == 0 && "GEM_ID" %in% names(df)) {
            match_idx <- which(df$GEM_ID == target)
          }
          if (length(match_idx) == 0) {
            shinyalert::closeAlert()
            shinyalert("Demo Warning", sprintf("PSG59343 not found (%d rows).", nrow(df)), type = "warning")
          } else {
            match_idx <- match_idx[1]
            
            ds <- as.character(df$Dataset[match_idx])
            gem_id <- target
            if (".gem_id_raw" %in% names(df)) {
              raw <- as.character(df$.gem_id_raw[match_idx])
              if (!is.na(raw) && nzchar(raw)) gem_id <- raw
            }
            
            # Step 4: Set analysis state, select Hallmark, run enrichment
            selected_analysis_ds(ds)
            selected_analysis_gemid(gem_id)
            shinyjs::show("analysis_panel")
            updateSelectInput(session, "explore_pw_db", selected = "Hallmark")
            
            shinyjs::delay(100, {
              run_enrichment_analysis()
              updateSelectizeInput(session, "explore_pathway", 
                                 selected = "HALLMARK_E2F_TARGETS")
              shinyalert::closeAlert()
              shinyalert("Demo Complete!", "", type = "success", timer = 3000)
            })
          }
        }
      })
    })
    
    
    # ===== Reactive filtered data (reads from snapshot values, not input$) =====
    filtered_chem <- eventReactive(chem_trigger(), {
      req(data_env$df1)
      apply_filters(data_env$df1, list(
        Perturbagen   = chem_filters$Perturbagen,
        Cell          = chem_filters$Cell,
        Tissue        = chem_filters$Tissue,
        Oncotreecode  = chem_filters$Oncotreecode,
        Accession     = chem_filters$Accession
      ))
    }, ignoreNULL = FALSE)
    
    filtered_gen <- eventReactive(gen_trigger(), {
      req(data_env$df2)
      apply_filters(data_env$df2, list(
        pbgene        = gen_filters$pbgene,
        method        = gen_filters$method,
        Cell          = gen_filters$Cell,
        Tissue        = gen_filters$Tissue,
        Oncotreecode  = gen_filters$Oncotreecode,
        Accession     = gen_filters$Accession
      ))
    }, ignoreNULL = FALSE)
    
    # ===== 过滤器快照（供增量刷新直接使用，避免读到 eventReactive 的旧缓存）=====
    chem_filter_list <- function() list(
      Perturbagen   = chem_filters$Perturbagen,
      Cell          = chem_filters$Cell,
      Tissue        = chem_filters$Tissue,
      Oncotreecode  = chem_filters$Oncotreecode,
      Accession     = chem_filters$Accession
    )
    gen_filter_list <- function() list(
      pbgene        = gen_filters$pbgene,
      method        = gen_filters$method,
      Cell          = gen_filters$Cell,
      Tissue        = gen_filters$Tissue,
      Oncotreecode  = gen_filters$Oncotreecode,
      Accession     = gen_filters$Accession
    )
    
    # ===== Display-ready 数据取用（无筛选时复用缓存，避免每次渲染重做整表派生）=====
    gen_display_from <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      if (is.null(data_env$df2) || nrow(df) != nrow(data_env$df2)) {
        return(to_display_df(df, "genetic"))
      }
      if (is.null(data_env$gen_display) || nrow(data_env$gen_display) != nrow(data_env$df2)) {
        data_env$gen_display <- to_display_df(df, "genetic")
        # 同步到进程级缓存：后续会话直接复用这 0.1GB，不必再派生一次
        .gemap_shared$gen_display <- data_env$gen_display
        # 落盘：全量装配完成后再存
        if (isTRUE(data_env$loaded_gen)) .gemap_cache_save("gen_display", data_env$gen_display, gen_meta_files)
      }
      data_env$gen_display
    }
    gen_display_data <- function() {
      gen_display_from(filtered_gen())
    }
    
    chem_display_from <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      if (is.null(data_env$df1) || nrow(df) != nrow(data_env$df1)) {
        return(to_display_df(df, "chemical"))
      }
      if (is.null(data_env$chem_display) || nrow(data_env$chem_display) != nrow(data_env$df1)) {
        data_env$chem_display <- to_display_df(df, "chemical")
        # 同步到进程级缓存：后续会话直接复用这 0.8GB 派生结果，不必再算一遍
        .gemap_shared$chem_display <- data_env$chem_display
        # 落盘：全量装配完成后再存（loaded_chem 为 TRUE 说明是完整数据而非首屏那一片）
        if (isTRUE(data_env$loaded_chem)) .gemap_cache_save("chem_display", data_env$chem_display, chem_meta_files)
      }
      data_env$chem_display
    }
    # 注意：这里不能再 copy(df)。全量 chemical 的 display 表是 92 万行、约 0.8GB，
    # 多复制一份就多一份峰值内存，网页端很容易因此撑爆 worker（表现为表格 Ajax error）。
    # datatable() / replaceData() 都只是读这份数据（DT 的 server 模式不会按引用修改它），
    # 不需要防御性复制。
    chem_display_data <- function() {
      chem_display_from(filtered_chem())
    }
    
    # ===== 增量刷新：把新数据就地补进已渲染的表格（不重绘、不闪、保持当前分页/选中行）=====
    # 关键：replaceData() 内部会走 dataTableAjax()，若不显式给 rownames = FALSE，
    # 它会把行名 cbind 成第一列 → 22 列 vs 客户端 21 列 → dataTablesFilter() 直接判定
    # "length(q$columns) != ncol(data)" 返回 recordsFiltered = 0，
    # 表现就是 "No matching records found (filtered from 171,184 total entries)"。
    refresh_gen_table <- function() {
      if (is.null(data_env$df2) || nrow(data_env$df2) == 0) return(invisible(NULL))
      display <- tryCatch(gen_display_from(apply_filters(data_env$df2, gen_filter_list())),
                          error = function(e) NULL)
      if (is.null(display) || nrow(display) == 0) return(invisible(NULL))
      # 列集变了就不能用 replaceData（列会错位），退回整表重绘
      if (!is.null(data_env$gen_display_cols) && !identical(names(display), data_env$gen_display_cols)) {
        gen_trigger(gen_trigger() + 1)
        return(invisible(NULL))
      }
      ok <- tryCatch({
        proxy <- DT::dataTableProxy("table2", session = session, deferUntilFlush = FALSE)
        DT::replaceData(proxy, display, rownames = FALSE,
                        resetPaging = FALSE, clearSelection = "none")
        TRUE
      }, error = function(e) {
        message("[Browse] incremental refresh (genetic) failed: ", conditionMessage(e))
        FALSE
      })
      if (!ok) gen_trigger(gen_trigger() + 1)   # 兜底：整表重绘
      invisible(NULL)
    }
    
    refresh_chem_table <- function() {
      if (is.null(data_env$df1) || nrow(data_env$df1) == 0) return(invisible(NULL))
      display <- tryCatch(chem_display_from(apply_filters(data_env$df1, chem_filter_list())),
                          error = function(e) NULL)
      if (is.null(display) || nrow(display) == 0) return(invisible(NULL))
      if (!is.null(data_env$chem_display_cols) && !identical(names(display), data_env$chem_display_cols)) {
        chem_trigger(chem_trigger() + 1)
        return(invisible(NULL))
      }
      ok <- tryCatch({
        proxy <- DT::dataTableProxy("table1", session = session, deferUntilFlush = FALSE)
        # 不 copy(display)：92 万行再复制一份就是 0.8GB，DT 只读不写
        DT::replaceData(proxy, display, rownames = FALSE,
                        resetPaging = FALSE, clearSelection = "none")
        TRUE
      }, error = function(e) {
        message("[Browse] incremental refresh (chemical) failed: ", conditionMessage(e))
        FALSE
      })
      if (!ok) chem_trigger(chem_trigger() + 1)
      invisible(NULL)
    }
    
    output$table1 <- DT::renderDT(server = TRUE, {
      # 数据还没就绪时直接挂起输出（DTOutput 上会显示 spinner + 加载提示）。
      # 这里绝不能返回一个"占位 DataTable"：占位表会先初始化 DataTables 并发出第一次
      # Ajax，等真实数据到了列集一变，客户端就会弹
      # "DataTables warning: table id=... - Ajax error"（点确定后表格才出现）。
      fc <- tryCatch(filtered_chem(), shiny.silent.error = function(e) NULL)
      if (is.null(fc)) req(FALSE)
      # 渲染与增量替换共用同一份 display-ready 数据，二者列集/列序必须完全一致
      df <- chem_display_data()
      if (is.null(df)) {
        # 过滤后 0 行：仍用同一套列渲染一张空表，避免表格结构变化再次引发 Ajax error
        if (is.null(data_env$chem_display)) req(FALSE)
        df <- data_env$chem_display[0]
      }
      data_env$chem_display_cols <- names(df)
      dataset_col_idx <- match(".Dataset", names(df)) - 1
      gem_id_col_idx  <- match(".gem_id_raw", names(df)) - 1
      num_cols <- names(df)[sapply(df, is.numeric)]
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        escape = FALSE,
        selection = list(mode = "single", target = "row"),
        options = dt_options(df, "chemical"),
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
      )
      if (length(num_cols) > 0) dt %>% DT::formatSignif(columns = num_cols, digits = 4) else dt
    })
    
    output$table2 <- DT::renderDT(server = TRUE, {
      # 同上：数据未就绪就挂起，绝不返回占位 DataTable（否则会弹 Ajax error）
      fg <- tryCatch(filtered_gen(), shiny.silent.error = function(e) NULL)
      if (is.null(fg)) req(FALSE)
      # 渲染与增量替换共用同一份 display-ready 数据，二者列集/列序必须完全一致
      df <- gen_display_data()
      if (is.null(df)) {
        # 过滤后 0 行：仍用同一套列渲染一张空表，避免表格结构变化再次引发 Ajax error
        if (is.null(data_env$gen_display)) req(FALSE)
        df <- data_env$gen_display[0]
      }
      data_env$gen_display_cols <- names(df)
      dataset_col_idx <- match(".Dataset", names(df)) - 1
      gem_id_col_idx  <- match(".gem_id_raw", names(df)) - 1
      num_cols <- names(df)[sapply(df, is.numeric)]
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        escape = FALSE,
        selection = list(mode = "single", target = "row"),
        options = dt_options(df, "genetic"),
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
      )
      if (length(num_cols) > 0) dt %>% DT::formatSignif(columns = num_cols, digits = 4) else dt
    })
    
    # ===== Table CSV downloads (1:1 export) =====
    output$download_table1 <- downloadHandler(
      filename = function() paste0("browse_chemical_", Sys.Date(), ".csv"),
      content = function(file) {
        req(filtered_chem())
        dt <- sanitize_encoding(copy(filtered_chem()))
        # Mirror display column order: GEM_ID first, then same renames
        if ("GEM_ID" %in% names(dt)) setcolorder(dt, c("GEM_ID", setdiff(names(dt), "GEM_ID")))
        if ("Dataset" %in% names(dt)) setnames(dt, "Dataset", ".Dataset")
        # Replace HTML GEM_ID with raw .gem_id_raw for plain-text CSV, then rename → Dataset
        if (".gem_id_raw" %in% names(dt) && "GEM_ID" %in% names(dt)) {
          dt[, GEM_ID := .gem_id_raw]
          setnames(dt, "GEM_ID", "Dataset")
        }
        if ("Perturbagen" %in% names(dt)) setnames(dt, "Perturbagen", "PbChem")
        drop <- intersect(c(".Dataset", ".gem_id_raw", ".source_meta_file", ".source_data_file",
                            "perturb_id", "Type",
                            "pbgene", "pb_ENSEMBL", "gene_biotype", "method"), names(dt))
        if (length(drop) > 0) dt[, (drop) := NULL]
        dots <- grep("^\\.", names(dt), value = TRUE)
        if (length(dots) > 0) dt[, (dots) := NULL]
        strip_html(dt)
        fwrite(dt, file)
      }
    )
    
    output$download_table2 <- downloadHandler(
      filename = function() paste0("browse_genetic_", Sys.Date(), ".csv"),
      content = function(file) {
        req(filtered_gen())
        dt <- sanitize_encoding(copy(filtered_gen()))
        # Mirror display column order: GEM_ID first, then same renames
        if ("GEM_ID" %in% names(dt)) setcolorder(dt, c("GEM_ID", setdiff(names(dt), "GEM_ID")))
        if ("Dataset" %in% names(dt)) setnames(dt, "Dataset", ".Dataset")
        # Replace HTML GEM_ID with raw .gem_id_raw for plain-text CSV, then rename → Dataset
        if (".gem_id_raw" %in% names(dt) && "GEM_ID" %in% names(dt)) {
          dt[, GEM_ID := .gem_id_raw]
          setnames(dt, "GEM_ID", "Dataset")
        }
        cap_cols <- intersect(c("pbgene", "method"), names(dt))
        for (cc in cap_cols) setnames(dt, cc, paste0(toupper(substr(cc, 1, 1)), substr(cc, 2, nchar(cc))))
        drop <- intersect(c(".Dataset", ".gem_id_raw", ".source_meta_file", ".source_data_file",
                            "perturb_id", "Type", "gene_biotype", "pb_ENSEMBL",
                            "Perturbagen", "Dose", "Time"), names(dt))
        if (length(drop) > 0) dt[, (drop) := NULL]
        dots <- grep("^\\.", names(dt), value = TRUE)
        if (length(dots) > 0) dt[, (dots) := NULL]
        strip_html(dt)
        fwrite(dt, file)
      }
    )
    
    # ================== Dataset 列点击弹窗（与 OneGene 一致）==================
    modal_plot_store <- reactiveVal(NULL)
    GSEA_DIR <- "GSEA"
    
    get_deg_dt_cached <- function(dataset, data_dir) {
      key <- paste0(normalizePath(data_dir, mustWork = FALSE), "::", dataset)
      if (!exists(key, envir = .GlobalEnv)) {
        fst_file <- file.path(data_dir, paste0(dataset, ".fst"))
        if (file.exists(fst_file)) {
          dt <- read_fst(fst_file, as.data.table = TRUE)
        } else {
          # Search in Chemical/ and Genetic/ subdirectories
          chem <- file.path(data_dir, "Chemical", paste0(dataset, ".fst"))
          gen  <- file.path(data_dir, "Genetic", paste0(dataset, ".fst"))
          dt <- if (file.exists(chem)) read_fst(chem, as.data.table = TRUE)
          else if (file.exists(gen)) read_fst(gen, as.data.table = TRUE)
          else NULL
        }
        assign(key, dt, envir = .GlobalEnv)
      }
      get(key, envir = .GlobalEnv)
    }
    
    show_dataset_modal <- function(row, ds) {
      type <- "unknown"; compound <- ""; gene <- ""
      if (!is.null(row$Perturbagen) && !is.na(row$Perturbagen) && nzchar(as.character(row$Perturbagen))) {
        type <- "chemical"; compound <- as.character(row$Perturbagen)
      } else if (!is.null(row$pbgene) && !is.na(row$pbgene) && nzchar(as.character(row$pbgene))) {
        type <- "genetic"; gene <- as.character(row$pbgene)
      } else if (!is.null(row$Gene) && !is.na(row$Gene) && nzchar(as.character(row$Gene))) {
        type <- "genetic"; gene <- as.character(row$Gene)
      }
      link_html <- NULL; mol_img <- NULL
      if (type == "chemical" && nzchar(compound)) {
        cid <- URLencode(compound, reserved = TRUE)
        img2d <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", cid, "/PNG")
        img3d <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", cid, "/PNG?record_type=3d")
        mol_img <- div(class = "modal-image-row",
                       fluidRow(
                         column(6, div(class = "modal-image-card",
                                       div(class = "modal-img-box", tags$img(src = img2d, class = "modal-structure-img", onerror = "this.parentElement.parentElement.style.display='none'")),
                                       div(class = "modal-img-caption", "2D Structure"))),
                         column(6, div(class = "modal-image-card",
                                       div(class = "modal-img-box", tags$img(src = img3d, class = "modal-structure-img", onerror = "this.parentElement.parentElement.style.display='none'")),
                                       div(class = "modal-img-caption", "3D Ball-Stick Model")))
                       ), br())
        link_html <- tags$a(href = paste0("https://pubchem.ncbi.nlm.nih.gov/compound/", cid), target = "_blank",
                            class = "btn btn-sm btn-primary", icon("external-link-alt"), " Open in PubChem")
      }
      if (type == "genetic" && nzchar(gene)) {
        g <- URLencode(gene, reserved = TRUE)
        mol_img <- div(class = "modal-image-row",
                       div(class = "modal-image-card",
                           div(class = "modal-img-box-large",
                               tags$img(src = paste0("https://string-db.org/api/image/network?identifiers=", g, "&species=9606"), class = "modal-structure-img", onerror = "this.parentElement.parentElement.style.display='none'")),
                           div(class = "modal-img-caption", "Protein-Protein Interaction Network (STRING)")
                       ), br())
        link_html <- tags$a(href = paste0("https://www.uniprot.org/uniprotkb?query=", g), target = "_blank",
                            class = "btn btn-sm btn-warning", icon("external-link-alt"), " Open in UniProt")
      }
      items <- list()
      skip_cols <- c(".source_data_file", "Download", "Dataset", "dataset", ".gem_id_raw")
      display_map <- c("GEM_ID" = "Dataset", "Perturbagen" = "PbChem")
      for (col in names(row)) {
        if (col %in% skip_cols) next
        val <- row[[col]]
        if (is.null(val) || length(val) == 0 || identical(as.character(val),"") || identical(as.character(val),"NA") || (length(val)==1&&is.na(val))) next
        if (col == "GEM_ID") val <- row$.gem_id_raw
        if (is.numeric(val)) val <- round(val, 4)
        dcol <- if (col %in% names(display_map)) display_map[col] else col
        items[[length(items)+1]] <- div(class="detail-item",span(class="detail-label",paste0(dcol,": ")),span(class="detail-value",gsub("<[^>]+>", "", as.character(val))))
      }
      deg_dt <- get_deg_dt_cached(ds, GSEA_DIR)
      cb <- NULL; cd <- NULL; m1 <- NULL; m2 <- NULL; lp <- NULL; dn <- NULL
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
          deg <- data.table(gene=as.character(deg_dt[[gene_col]]), logFC=suppressWarnings(as.numeric(deg_dt[[fc_col]])))
          if (!is.na(pval_col) && !is.na(fdr_col)) {
            deg$pval <- suppressWarnings(as.numeric(deg_dt[[pval_col]]))
            deg$fdr  <- suppressWarnings(as.numeric(deg_dt[[fdr_col]]))
            if (sum(!is.na(deg$pval)) > 0 && sum(!is.na(deg$fdr)) > 0) {
              has_pval <- TRUE; has_fdr <- TRUE
            }
          }
          deg <- deg[!is.na(logFC) & !is.na(gene) & nzchar(gene)]
          if (nrow(deg) > 0) valid_data <- TRUE
        }
      }
      if (valid_data) {
        deg$group <- ifelse(deg$logFC>1,"Up",ifelse(deg$logFC<(-1),"Down","NS"))
        gcs <- c("Down"="#4285F4","NS"="gray","Up"="#EA4335")
        # Count bar
        cs <- deg[,.(gc=.N),by=group]; cs$group <- factor(cs$group,levels=c("Down","NS","Up"))
        cb <- ggplot(cs,aes(x=group,y=gc,fill=group))+geom_col(width=0.7,show.legend=FALSE)+scale_fill_manual(values=gcs)+geom_text(aes(label=gc),vjust=-0.3,fontface="bold")+labs(title="Differential Gene Count",x="",y="Gene Number")+theme_bw()
        # CDF
        ds2 <- deg[order(logFC)]; ds2$cdf <- seq(0,1,length.out=nrow(ds2))
        ds2$group <- "NS"; ds2$group[ds2$logFC<(-1)]<-"Down"; ds2$group[ds2$logFC>1]<-"Up"
        ds2$group <- factor(ds2$group,levels=c("Down","NS","Up"))
        cd <- ggplot(ds2,aes(x=logFC,y=cdf,color=group))+geom_line(linewidth=1.1,alpha=0.9)+scale_color_manual(values=gcs)+geom_vline(xintercept=c(-1,0,1),color="black",linewidth=0.35,linetype="dashed")+labs(title="logFC Cumulative Distribution",x="logFC",y="Cumulative Proportion")+theme_bw()+theme(legend.position="bottom")
        
        {
          # Gene ranked by logFC
          ds3 <- deg[order(logFC)]; ds3$rank <- 1:nrow(ds3)
          m1 <- ggplot(ds3,aes(x=rank,y=logFC,color=group))+geom_point(size=0.8,alpha=0.6)+scale_color_manual(values=gcs)+geom_hline(yintercept=c(-1,0,1),linetype="dashed",linewidth=0.3)+labs(title="Gene Ranked by logFC",x="Gene Rank",y="logFC")+theme_bw()+theme(legend.position="bottom")
        }
        # Distribution
        deg$bin <- cut(deg$logFC,breaks=seq(-10,10,1),include.lowest=TRUE)
        bs <- deg[,.(count=.N),by=.(bin,group)]
        m2 <- ggplot(bs,aes(x=bin,y=count,fill=group))+geom_col(position="stack",width=0.8)+scale_fill_manual(values=gcs)+labs(title="logFC Distribution",x="logFC Bins",y="Gene Count")+theme_bw()+theme(legend.position="bottom",axis.text.x=element_text(angle=45,hjust=1,size=8))
        # Lollipop
        tu <- head(deg[logFC>0][order(-logFC)],20); td <- head(deg[logFC<0][order(logFC)],20)
        tl <- rbindlist(list(tu,td),fill=TRUE)
        if(nrow(tl)>0){ tl$direction <- ifelse(tl$logFC>0,"Up","Down"); lp <- ggplot(tl,aes(x=reorder(gene,logFC),y=logFC,fill=direction))+geom_col(width=0.2)+geom_point(size=2,shape=21)+coord_flip()+labs(title="Top 20 Genes",x="",y="logFC")+scale_fill_manual(values=c("Up"="#EA4335","Down"="#4285F4"))+theme_bw()+theme(axis.text.y=element_text(size=8),legend.position="bottom") }
        # Density
        dn <- ggplot(deg,aes(x=logFC))+geom_density(fill="#76A7FA",alpha=0.4)+geom_vline(xintercept=0,linetype="dashed")+labs(title="Density",x="logFC",y="Density")+theme_bw()
      }
      ggs <- list(count_bar=cb,cdf=cd,main_1=m1,main_2=m2,lollipop=lp,density=dn)
      ggs <- Filter(function(x)!is.null(x)&&inherits(x,"ggplot"),ggs)
      cplot <- NULL
      if(length(ggs)>0){
        ts <- theme_bw()+theme(plot.title=element_text(size=11,face="bold",hjust=0.5),axis.title=element_text(size=9),axis.text=element_text(size=8),legend.position="bottom",plot.margin=margin(8,8,8,8))
        ggs <- lapply(ggs,function(p)p+ts)
        np <- length(ggs)
        cplot <- switch(as.character(np),"1"=ggs[[1]],"2"=patchwork::wrap_plots(ggs[[1]],ggs[[2]],ncol=2),"3"=patchwork::wrap_plots(ggs[[1]],ggs[[2]],ggs[[3]],ncol=3),"4"=patchwork::wrap_plots(ggs[[1]],ggs[[2]],ggs[[3]],ggs[[4]],ncol=2),"5"=patchwork::wrap_plots(ggs[[1]],ggs[[2]],ggs[[3]],ggs[[4]],ggs[[5]],ncol=3),"6"=patchwork::wrap_plots(ggs[[1]],ggs[[2]],ggs[[3]],ggs[[4]],ggs[[5]],ggs[[6]],design="\nABC\nDEF\nDEF"),"7"=patchwork::wrap_plots(ggs[[1]],ggs[[2]],ggs[[3]],ggs[[4]],ggs[[7]],ggs[[6]],ggs[[5]],design=c(patchwork::area(1,1),patchwork::area(1,2),patchwork::area(1,3),patchwork::area(2,1),patchwork::area(2,2),patchwork::area(2,3,3,3),patchwork::area(3,1,3,2))))
        
      }
      modal_plot_store(list(
        dataset=as.character(ds),
        gem_id=if (!is.null(row$.gem_id_raw) && nzchar(as.character(row$.gem_id_raw))) as.character(row$.gem_id_raw) else as.character(ds),
        ggplots=ggs,
        combined_plot=cplot
      ))
      showModal(modalDialog(
        tags$div(id=ns("dataset_detail_modal"),tags$script(HTML(sprintf("setTimeout(function(){var el=document.getElementById('%s');if(el){$(el).closest('.modal-dialog').addClass('dataset-wide-modal');}},50);",ns("dataset_detail_modal"))))),
        div(class="detail-card",
            link_html,br(),mol_img,items,
            if(valid_data) tagList(br(),hr(),
                                   tags$div(style="margin: 10px 0 16px 0; text-align: center;",
                                            downloadButton(ns("download_combined_pdf"), "Download Plot", class="btn btn-default btn-lg")
                                   ),
                                   withSpinner(plotOutput(ns("modal_combined"),height="800px"),type=4,color="#3a7bd5"))
        ),size="l",easyClose=TRUE,footer=modalButton("Close")))
      if(valid_data) output$modal_combined <- renderPlot({st<-modal_plot_store();req(st$combined_plot);print(st$combined_plot)})
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
    
    observeEvent(input$click_dataset, {
      ds_html <- input$click_dataset
      if (is.null(ds_html) || !nzchar(ds_html)) return()
      ds <- gsub("<[^>]+>", "", ds_html)
      row <- NULL
      if (!is.null(data_env$df1)) {
        orig <- gsub("<[^>]+>", "", data_env$df1$Dataset)
        idx <- which(orig == ds)[1]
        if (!is.na(idx)) row <- as.list(data_env$df1[idx])
      }
      if (is.null(row) && !is.null(data_env$df2)) {
        orig <- gsub("<[^>]+>", "", data_env$df2$Dataset)
        idx <- which(orig == ds)[1]
        if (!is.na(idx)) row <- as.list(data_env$df2[idx])
      }
      if (is.null(row)) return()
      show_dataset_modal(row, ds)
    })
    
    # ================== Chemical 下载 ==================
    # (Client-side download via onclick in download_btn column)
    
    # ================== Pathway Enrichment Analysis ==================
    # Same GMT loading logic as Synergy.R, but only loaded when user clicks "Run"
    
    GMT_FILES <- list(
      KEGG                  = "c2.cp.kegg_legacy.v2024.1.Hs.symbols.gmt",
      Hallmark              = "h.all.v2022.1.Hs.symbols.gmt",
      `BioCarta`            = "通路/BioCarta_2016.txt",
      `Elsevier Pathway`    = "通路/Elsevier_Pathway_Collection.txt",
      `HumanCyc`            = "通路/HumanCyc_2016.txt",
      `Panther`             = "通路/Panther_2016.txt",
      `Reactome`            = "通路/Reactome_Pathways_2024.txt",
      `WikiPathways`        = "通路/WikiPathways_2024_Human.txt"
    )
    
    parse_gmt <- function(filepath) {
      lines <- readLines(filepath, warn = FALSE)
      pathways <- list()
      for (line in lines) {
        parts <- strsplit(line, "\t")[[1]]
        name <- parts[1]
        genes <- unique(toupper(parts[-(1:2)]))
        genes <- genes[nzchar(genes)]
        if (length(genes) >= 3) pathways[[name]] <- genes
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
          cat("[Browse] Loaded", length(pw), "pathways from", db, "\n")
        } else {
          cat("[Browse] GMT file not found:", fp, "\n")
        }
      }
      all_pw
    }
    
    all_pathways <- NULL  # lazy-loaded on first "Run" click
    
    # Store the dataset selected from the table
    selected_analysis_ds <- reactiveVal(NULL)
    selected_analysis_gemid <- reactiveVal(NULL)
    
    # Track analysis state for pathway DB changes
    analysis_has_run <- reactiveVal(FALSE)
    current_pw_choices <- reactiveVal(character(0))
    
    # Click "Analysis" button: get selected dataset from the active table
    observeEvent(input$analysis_btn, {
      # Determine which tab is active
      tab <- input$tabs
      if (is.null(tab)) {
        showNotification("Please select a tab first.", type = "warning")
        return()
      }
      
      row_idx <- NULL
      df <- NULL
      
      if (tab == "Chemical Perturbagens") {
        req(data_env$df1)
        row_idx <- input$table1_rows_selected
        if (length(row_idx) > 0) {
          df <- copy(filtered_chem())
        }
      } else if (tab == "Genetic Perturbagens") {
        req(data_env$df2)
        row_idx <- input$table2_rows_selected
        if (length(row_idx) > 0) {
          df <- copy(filtered_gen())
        }
      }
      
      if (length(row_idx) == 0 || is.null(df)) {
        showNotification("Please select a dataset row in the table first.", type = "warning")
        return()
      }
      
      # Get the Dataset value from the selected row
      ds <- as.character(df$Dataset[row_idx])
      if (is.null(ds) || !nzchar(ds)) {
        showNotification("Cannot identify selected dataset.", type = "error")
        return()
      }
      
      # Resolve GEM_ID for display
      gem_id <- ds
      if (".gem_id_raw" %in% names(df)) {
        raw <- as.character(df$.gem_id_raw[row_idx])
        if (!is.na(raw) && nzchar(raw)) gem_id <- raw
      }
      
      selected_analysis_ds(ds)
      selected_analysis_gemid(gem_id)
      shinyjs::show("analysis_panel")
    })
    
    # Reactive to hold fgsea result & ranked list (matching Synergy.R)
    enrich_result <- reactiveVal(NULL)
    explore_ranked <- reactiveVal(NULL)
    
    # ---- Enrichment analysis core function (shared by Run button AND DB change) ----
    run_enrichment_analysis <- function() {
      ds <- selected_analysis_ds()
      if (is.null(ds) || !nzchar(ds)) {
        showNotification("No dataset selected. Please select a row and click Analysis first.", type = "warning")
        return()
      }
      
      db <- input$explore_pw_db
      if (is.null(db) || !nzchar(db)) {
        showNotification("Please select a pathway database.", type = "warning")
        return()
      }
      
      # Lazy-load all pathways (Synergy.R style, first click only)
      if (is.null(all_pathways)) {
        all_pathways <<- load_all_pathways()
      }
      
      if (!db %in% names(all_pathways)) {
        showNotification(paste("Pathway database", db, "not found."), type = "error")
        return()
      }
      
      pathways <- all_pathways[[db]]
      pw_choices <- sort(names(pathways))
      
      # Load DEG data
      deg_dt <- get_deg_dt_cached(ds, GSEA_DIR)
      if (is.null(deg_dt) || nrow(deg_dt) == 0) {
        showNotification("No DEG data found for this dataset.", type = "warning")
        return()
      }
      
      gene_candidates <- c("Gene","gene","symbol","SYMBOL","gene_name","GeneSymbol","gene_id")
      fc_candidates   <- c("logFC","LogFC","log2FC","Log2FC","log2FoldChange","fc","value","Log2 (FC_avg)")
      gene_col <- gene_candidates[gene_candidates %in% names(deg_dt)][1]
      fc_col   <- fc_candidates[fc_candidates %in% names(deg_dt)][1]
      if (is.na(gene_col) || is.na(fc_col)) {
        showNotification("Cannot identify gene/logFC columns in DEG data.", type = "error")
        return()
      }
      
      deg <- data.table(
        gene  = as.character(deg_dt[[gene_col]]),
        logFC = suppressWarnings(as.numeric(deg_dt[[fc_col]]))
      )
      deg <- deg[!is.na(logFC) & is.finite(logFC) & !is.na(gene) & nzchar(gene)]
      deg <- deg[!duplicated(gene)]
      
      if (nrow(deg) < 100) {
        showNotification("Too few genes (< 100) for enrichment analysis.", type = "warning")
        return()
      }
      
      # Build ranked list
      ranked <- deg$logFC
      names(ranked) <- toupper(deg$gene)
      ranked <- sort(ranked, decreasing = TRUE)
      ranked <- ranked[!duplicated(names(ranked))]
      explore_ranked(ranked)
      
      # Run fgsea
      withProgress(message = paste("Running", db, "enrichment..."), {
        res <- tryCatch({
          set.seed(42)
          fgsea::fgseaMultilevel(
            pathways    = pathways,
            stats       = ranked,
            nPermSimple = 10000,
            eps         = 0
          )
        }, error = function(e) {
          cat("[Browse] fgsea error:", e$message, "\n")
          NULL
        })
      })
      
      if (is.null(res) || nrow(res) == 0) {
        showNotification("No enrichment results found.", type = "warning")
        return()
      }
      
      setorder(res, padj, na.last = TRUE)
      enrich_result(res)
      
      # Update pathway choices (this triggers renderUI to update the selectizeInput)
      current_pw_choices(pw_choices)
      analysis_has_run(TRUE)
    }
    
    # Click "Run" button: run enrichment → show all plots
    observeEvent(input$run_analysis_btn, {
      run_enrichment_analysis()
    }, ignoreInit = TRUE)
    
    # Pathway Database changed after first Run: re-run enrichment automatically
    observeEvent(input$explore_pw_db, {
      if (analysis_has_run()) {
        run_enrichment_analysis()
      }
    }, ignoreInit = TRUE)
    
    # Render enrichment results UI (reactive to pathway choices, i.e. DB changes)
    output$enrich_results_ui <- renderUI({
      req(analysis_has_run())
      pw_choices <- current_pw_choices()
      
      tagList(
        # Specific pathway selector for GSEA running enrichment plot
        selectizeInput(ns("explore_pathway"), "Specific Pathway (GSEA Plot)",
                       choices = pw_choices, width = "100%",
                       options = list(placeholder = "Select a pathway for GSEA plot...")),
        br(),
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
        )
      )
    })
    
    # Enrichment Dot Plot — exact same logic as Synergy.R
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
      
      gem_id_label <- selected_analysis_gemid()
      current_db <- input$explore_pw_db
      
      ggplot(res_top, aes(x = NES, y = pathway_label)) +
        geom_point(aes(size = size, color = NES), alpha = 0.85) +
        scale_color_gradient2(low = "#4285F4", mid = "grey90", high = "#EA4335",
                              midpoint = 0, name = "NES") +
        scale_size_continuous(name = "Gene Count", range = c(2, 8)) +
        labs(
          title = paste("Enrichment Analysis —", current_db),
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
    
    # Enrichment Bar Plot (top 15 by NES) — exact same logic as Synergy.R
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
      
      gem_id_label <- selected_analysis_gemid()
      current_db <- input$explore_pw_db
      
      ggplot(res_top, aes(x = NES, y = pathway_label, fill = NES)) +
        geom_col(width = 0.7) +
        scale_fill_gradient2(low = "#4285F4", mid = "grey90", high = "#EA4335",
                             midpoint = 0, name = "NES") +
        labs(
          title = paste("Top 15 Enriched Pathways —", current_db),
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
    
    # GSEA Running Enrichment Plot — exact same logic as Synergy.R
    gsea_plot_obj <- reactive({
      req(explore_ranked(), enrich_result(), input$explore_pathway)
      ranked <- explore_ranked()
      pw_name <- input$explore_pathway
      
      db <- input$explore_pw_db
      if (!db %in% names(all_pathways)) return(NULL)
      pathways <- all_pathways[[db]]
      
      if (!pw_name %in% names(pathways)) return(NULL)
      
      pathway_genes <- pathways[[pw_name]]
      
      ed <- tryCatch({
        fgsea::plotEnrichmentData(
          pathway = pathway_genes,
          stats   = ranked,
          gseaParam = 1
        )
      }, error = function(e) {
        cat("[Browse] plotEnrichmentData error:", e$message, "\n")
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
      
      es_df <- data.frame(rank = ed$curve[[1]], ES = ed$curve[[2]])
      
      # Clean pathway name for display
      pw_display <- gsub("^KEGG_|^HALLMARK_", "", pw_name)
      pw_display <- gsub("_", " ", pw_display)
      pw_display <- gsub(" Homo sapiens h \\S+$", "", pw_display)
      if (nchar(pw_display) > 60) pw_display <- paste0(substr(pw_display, 1, 57), "...")
      
      gem_id_label <- selected_analysis_gemid()
      
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
      bin_df <- data.frame(rank = bin_mid, logFC = bin_mean)
      p3 <- ggplot(bin_df, aes(x = rank, y = logFC)) +
        geom_col(aes(fill = logFC > 0), width = max(1, bin_w), show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = "#EA4335", "FALSE" = "#4285F4")) +
        geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
        labs(x = "Gene Rank", y = "logFC") +
        theme_bw() +
        theme(
          panel.border   = element_rect(color = "black", fill = NA, linewidth = 0.8),
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
    
    # Download handlers for enrichment plots
    output$download_enrich_dotplot <- downloadHandler(
      filename = function() {
        paste0("enrich_dotplot_", gsub(" ", "_", selected_analysis_gemid()), "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        p <- enrich_dotplot_obj()
        req(p)
        ggsave(file, plot = p, device = "pdf", width = 10, height = 8)
      }
    )
    
    output$download_enrich_barplot <- downloadHandler(
      filename = function() {
        paste0("enrich_barplot_", gsub(" ", "_", selected_analysis_gemid()), "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        p <- enrich_barplot_obj()
        req(p)
        ggsave(file, plot = p, device = "pdf", width = 10, height = 7)
      }
    )
    
    output$download_gsea_plot <- downloadHandler(
      filename = function() {
        paste0("gsea_", gsub(" ", "_", input$explore_pathway), "_",
               gsub(" ", "_", selected_analysis_gemid()), "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        p <- gsea_plot_obj()
        req(p)
        ggsave(file, plot = p, device = "pdf", width = 10, height = 7)
      }
    )
    
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