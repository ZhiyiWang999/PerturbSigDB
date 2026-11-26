# Public demonstration release: restore original relative paths from data/data-N files.
source("R/bootstrap_data.R")
psdb_restore_data(".")

# PerturbSigDB - 动态模块化版本
# 模块切换时彻底卸载并释放资源

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
shinyjs::useShinyjs()
library(future)
library(promises)
library(callr)
library(DT)
library(ggplot2)
library(scales)
library(data.table)
library(fst)
library(echarts4r)
# The public mini release uses a lightweight plan, not a 48-process server pool.
# Set PSDB_MODE=production only after provisioning a suitable reference database.
if (identical(Sys.getenv("PSDB_MODE", "demo"), "demo")) {
  future::plan(future::sequential)
} else {
  workers <- suppressWarnings(as.integer(Sys.getenv("PSDB_WORKERS", "2")))
  if (is.na(workers) || workers < 1L) workers <- 2L
  if (.Platform$OS.type == "windows") {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::multicore, workers = workers)
  }
}

# 预加载所有模块的Server函数
source("R/global.R")
source("R/global_signature_cache.R")
source("R/global_gpsa_cache.R")
source("R/global_pathway_cache.R")
source("R/query_signature_by_gene_fst.R")
source("R/Browse.R")
source("R/OneGene.R")
source("R/Genelist.R")
source("R/Synergy.R")
source("R/GPSA.R")
source("R/Download.R")

# 预加载 GPSA 倒排索引（避免每次查询 callr 重复加载 1.1GB rds）
init_gpsa_cache("GSEA")

# 模块配置
MODULE_CONFIG <- list(
  browse = list(
    ui_func = OneGeneBrowseUI,
    server_func = OneGeneBrowseServer,
    title = "Browse",
    desc = "Browse perturbation signatures"
  ),
  single = list(
    ui_func = GQueryUI,
    server_func = GQueryServer,
    title = "Gene Query",
    desc = "Single gene query"
  ),
  # genelist = list(
  #   ui_func = GSQueryUI,
  #   server_func = GSQueryServer,
  #   title = "GSQuery",
  #   desc = "Gene Set Query via GSEA"
  # ),
  synergy = list(
    ui_func = PwqueryMUI,
    server_func = PwqueryMServer,
    title = "Gene Set Query",
    desc = "Pathway-based perturbation query"
  ),
  gpsa = list(
    ui_func = GPSAUI,
    server_func = GPSAServer,
    title = "Perturb Query",
    desc = "Gene Perturbation Signature Alignment (Spearman)"
  ),
  download = list(
    ui_func = DownloadUI,
    server_func = DownloadServer,
    title = "Download",
    desc = "Download metadata and perturbation data"
  )
)

# 主UI
ui <- dashboardPage(
  title = "PerturbSigDB Platform",
  header = dashboardHeader(
    title = logo_blue_gradient,
    tags$li(
      class = "dropdown",
      style = "margin-right: 15px; line-height: 50px;",
      tags$a(
        href = "#",
        class = "btn btn-default btn-sm",
        style = "background: transparent; border: 1px solid rgba(255,255,255,0.3); color: white;",
        onclick = "Shiny.setInputValue('back_to_full', Math.random()); return false;",
        icon("expand-arrows-alt"), " Full App"
      )
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(id = "main_sidebar",
      menuItem("Home", tabName = "tab_home", icon = icon("home")),
      menuItem("Modules", icon = icon("puzzle-piece"),
        menuItem("Browse", tabName = "tab_browse", icon = icon("search")),
        menuItem("Gene Query", tabName = "tab_single", icon = icon("dna")),
        # menuItem("GSQuery", tabName = "tab_genelist", icon = icon("list")),
        menuItem("Gene Set Query", tabName = "tab_synergy", icon = icon("flask")),
        menuItem("Perturb Query", tabName = "tab_gpsa", icon = icon("align-center"))
      ),
      menuItem("Download", tabName = "tab_download", icon = icon("download")),
      menuItem("Help", tabName = "tab_help", icon = icon("circle-question")),
      menuItem("About", tabName = "tab_about", icon = icon("circle-info"))
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    tags$div(style = "background:#fff4d6;color:#705000;padding:10px 14px;text-align:center;font-weight:600;", "PUBLIC DEMO: 50 bundled signatures. Overview counts describe the full database; search results here are limited to the demonstration subset."),
    tags$head(
      # No external tracking script in the public demonstration build.
      tags$style(HTML("
        .loading-overlay {
          position: fixed;
          top: 0; left: 0; right: 0; bottom: 0;
          background: rgba(0,0,0,0.7);
          z-index: 9999;
          display: flex;
          align-items: center;
          justify-content: center;
          color: white;
          font-size: 20px;
        }
        .loading-overlay.hidden { display: none; }
        .module-transition {
          animation: fadeIn 0.3s ease-in-out;
        }
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(10px); }
          to { opacity: 1; transform: translateY(0); }
        }
        table.dataTable tbody tr.selected > *,
        table.dataTable.stripe tbody tr.odd.selected > *,
        table.dataTable.display tbody tr.odd.selected > *,
        table.dataTable.hover tbody tr.selected:hover > *,
        table.dataTable.display tbody tr.selected:hover > *,
        table.dataTable.order-column tbody tr.selected > .sorting_1,
        table.dataTable.order-column tbody tr.selected > .sorting_2,
        table.dataTable.order-column tbody tr.selected > .sorting_3,
        table.dataTable.display tbody tr.selected > .sorting_1,
        table.dataTable.display tbody tr.selected > .sorting_2,
        table.dataTable.display tbody tr.selected > .sorting_3 {
          box-shadow: inset 0 0 0 9999px rgba(208, 208, 208, 0.9) !important;
          color: #333 !important;
        }
      "))
    ),
    tags$script(HTML("(function(){var s=document.createElement('style');s.textContent='table.dataTable tbody tr.selected>*{box-shadow:inset 0 0 0 9999px rgba(208,208,208,0.9)!important;color:#333!important}table.dataTable.stripe tbody tr.odd.selected>*,table.dataTable.display tbody tr.odd.selected>*{box-shadow:inset 0 0 0 9999px rgba(208,208,208,0.923)!important;color:#333!important}table.dataTable.hover tbody tr.selected:hover>*,table.dataTable.display tbody tr.selected:hover>*{box-shadow:inset 0 0 0 9999px rgba(180,180,180,1)!important;color:#333!important}';document.head.appendChild(s);})();")),
    # 全局CSV下载处理器（Blob方式，避免downloadLink程序化点击失效）
    tags$script(HTML("
      Shiny.addCustomMessageHandler('trigger_csv_download', function(data) {
        var BOM = new Uint8Array([0xEF, 0xBB, 0xBF]);
        var blob = new Blob([BOM, data.content], {type: 'text/csv;charset=utf-8;'});
        var url = window.URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.style.display = 'none';
        a.href = url;
        a.download = data.filename;
        document.body.appendChild(a);
        a.click();
        setTimeout(function() {
          document.body.removeChild(a);
          window.URL.revokeObjectURL(url);
        }, 200);
      });
      Shiny.addCustomMessageHandler('trigger_zip_download', function(data) {
        var binary = atob(data.content);
        var bytes = new Uint8Array(binary.length);
        for (var i = 0; i < binary.length; i++) {
          bytes[i] = binary.charCodeAt(i);
        }
        var blob = new Blob([bytes], {type: 'application/zip'});
        var url = window.URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.style.display = 'none';
        a.href = url;
        a.download = data.filename;
        document.body.appendChild(a);
        a.click();
        setTimeout(function() {
          document.body.removeChild(a);
          window.URL.revokeObjectURL(url);
        }, 200);
      });
    ")),
    
    # 加载遮罩
    div(id = "module_loading", class = "loading-overlay hidden",
      div(
        icon("spinner", class = "fa-spin"), 
        tags$br(),
        "Loading module..."
      )
    ),
    
    tabItems(
      # Home
      # Home
      tabItem(tabName = "tab_home",
              fluidRow(
                box(width = 12, solidHeader = FALSE,
                    tags$style(HTML("
      .extended-valuebox .small-box {
        height: auto !important;
        padding-bottom: 20px;
      }
      .extended-valuebox .small-box .inner {
        overflow-y: auto;
        max-height: 600px;
      }
      .extended-valuebox .small-box p, .extended-valuebox .small-box ul {
        font-size: 14px;
        text-align: left;
      }
    ")),
                    div(class = "extended-valuebox",
                        valueBox(value = "PerturbSigDB", color = "olive", width = 12,
                                 subtitle = HTML(
                                   "<strong>Perturbation Signature Database.</strong><br>
                    <p>PerturbSigDB is a manually curated perturbation signature database for upstream regulator and chemical modulator discovery. The current release contains 1,091,148 perturbation signatures, including 919,964 chemical and 171,184 genetic signatures, curated from 7,001 source accessions across 1,737 cell lines, with uniformly processed and richly annotated data for cross-study analysis.</p>
                    <hr>
                    <h4>What you can do with PerturbSigDB?</h4>
                    <ul>
                      <li><strong>Browse</strong>: Explore large-scale perturbation signatures and systematically dissect biological patterns by perturbation, gene, cell line, tissue, and functional pathway.</li>
                      <li><strong>Gene Query</strong>: Analyze perturbation-associated expression changes for individual genes, enabling rapid interpretation of gene-level regulatory effects across diverse perturbation contexts.</li>
                      <li><strong>Gene Set Query</strong>: Perform pathway- or gene set-oriented perturbation analysis to identify compounds or genetic perturbations that activate or suppress specific biological programs.</li>
                      <li><strong>Perturb Query</strong>: Upload custom differential expression profiles to identify perturbations with similar or opposite transcriptional effects, supporting signature reversal-based compound prioritization and mechanism exploration with interactive visualizations, including rank plots and signature alignment plots.</li>
                    </ul>
                      <p>Use cases: Help researchers move from genes, pathways or user-defined signatures to candidate regulatory genes, chemical modulators and interpretable perturbation mechanisms.</p>"
                                 ),
                                 icon = icon("map")
                        )
                    )
                ),
                box(width = 12,
                    fluidRow(
                      valueBox("1,091,148", "Perturb signatures", icon("sitemap"), "teal", 3),
                      # valueBox("3027", "Cell Lines", icon("flask"), "teal", 3),
                      # valueBox("51", "Tissues", icon("project-diagram"), "teal", 3),
                      # valueBox("56448", "Genes", icon("dna"), "teal", 3),
                      valueBox("919,964", "Chemical perturb signatures", icon("sitemap"), "teal", 3),
                      valueBox("171,184", "Genetic perturb signatures", icon("dna"), "teal", 3),
                      valueBox("44,943 & 10,029", "Chemical perturbagens & Perturbed genes", icon("project-diagram"), "teal", 3),
                      # valueBox("10029", "PBGenes", icon("dna"), "teal", 3),
                      valueBox("Browse", "One million perturb signature message", icon("search"), "teal", 3),
                      valueBox("Gene Query", "Gene perturbator query", icon("fingerprint"), "teal", 3),
                      valueBox("Gene Set Query", "Gene set modulator query", icon("list"), "teal", 3),
                      valueBox("Perturb Query", "Perturbation similarity query", icon("align-center"), "teal", 3)
                    )
                ),
                
                # ========== 新增：5个tabPanel 区域 ==========
                box(width = 12, title = "Gallery", solidHeader = TRUE, status = "primary",
                    tabsetPanel(
                      tabPanel("PerturbSigDB",
                               div(style = "padding: 10px; text-align: center;",
                                   column(
                                     3,
                                     tags$img(
                                       src = "PerturbSigDB.png",
                                       style = "
        width: 420%;
        max-width: auto;
        transform: scale(1);   /* 0.8 = 缩小到原尺寸80%，0.7就是70% */
        transform-origin: center;
        border: 1px solid #ddd;
        border-radius: 6px;
      "
                                     )
                                   ))
                               
                      ),
                      tabPanel("Gene Query",
                               div(style = "padding: 10px; text-align: center;",
                                   column(
                                     3,
                                     tags$img(
                                       src = "Gene Query.png",
                                       style = "
        width: 420%;
        max-width: auto;
        transform: scale(0.9);   /* 0.8 = 缩小到原尺寸80%，0.7就是70% */
        transform-origin: center;
        border: 1px solid #ddd;
        border-radius: 6px;
      "
                                     )
                                   ))
                      ),
                      tabPanel("Gene Set Query",
                               div(style = "padding: 10px; text-align: center;",
                                   column(
                                     3,
                                     tags$img(
                                       src = "Gene Set.png",
                                       style = "
        width: 420%;
        max-width: auto;
        transform: scale(0.9);   /* 0.8 = 缩小到原尺寸80%，0.7就是70% */
        transform-origin: center;
        border: 1px solid #ddd;
        border-radius: 6px;
      "
                                     )
                                   ))
                      ),
                      tabPanel("Perturb Query",
                               div(style = "padding: 10px; text-align: center;",
                                   column(
                                     3,
                                     tags$img(
                                       src = "perturb query.png",
                                       style = "
        width: 420%;
        max-width: auto;
        transform: scale(0.9);   /* 0.8 = 缩小到原尺寸80%，0.7就是70% */
        transform-origin: center;
        border: 1px solid #ddd;
        border-radius: 6px;
      "
                                     )
                                   ))
                      )

                    )
                ),
                # Creative Commons License
                box(width = 12, solidHeader = FALSE, style = "text-align: center; padding: 10px; background: transparent; border: none;",
                    tags$p(style = "margin: 0; color: #888; font-size: 13px;",
                      icon("creative-commons"), icon("creative-commons-by"),
                      " This work is licensed under a ",
                      tags$a(href = "https://creativecommons.org/licenses/by/4.0/", target = "_blank",
                             style = "color: #3a7bd5;",
                             "Creative Commons Attribution 4.0 International License"),
                      "."
                    )
                )
                # ==========================================
              )
      ),
      
      # 四个模块容器（动态加载）
      tabItem(tabName = "tab_browse",
        div(id = "module_container_browse", class = "module-transition")
      ),
      tabItem(tabName = "tab_single",
        div(id = "module_container_single", class = "module-transition")
      ),
      tabItem(tabName = "tab_genelist",
        div(id = "module_container_genelist", class = "module-transition")
      ),
      tabItem(tabName = "tab_synergy",
        div(id = "module_container_synergy", class = "module-transition")
      ),
      tabItem(tabName = "tab_gpsa",
        div(id = "module_container_gpsa", class = "module-transition")
      ),
      tabItem(tabName = "tab_download",
        div(id = "module_container_download", class = "module-transition")
      ),
      
      # Help
      tabItem(tabName = "tab_help",
        fluidRow(
          box(width = 12, title = "Help & Documentation", status = "primary", solidHeader = TRUE,
            tabsetPanel(id = "help_tabs",
              
              # ========== Tab 1: Overview ==========
              tabPanel("Overview",
                tags$div(style = "padding: 20px; max-width: 1400px; margin: 0 auto;",
                  tags$h3("What is PerturbSigDB?", style = "color: #2c5f8a;"),
                  tags$p("PerturbSigDB (Perturbation Signature Database) is a manually curated, open-access resource for upstream regulator and chemical modulator discovery from large-scale genetic and chemical perturbation signatures. The database enables users to start from a gene, pathway, user-defined gene set or transcriptional signature of interest and identify relevant genetic perturbations, pathway-modulating compounds and matched perturbation states."),
                  tags$p("PerturbSigDB integrates two complementary perturbation modalities within a unified framework. Genetic perturbations, including knockdown, knockout, CRISPR-based perturbation and overexpression, help reveal how perturbing specific genes influences downstream transcriptional programs and may nominate candidate upstream regulatory factors. Chemical perturbations capture compound-induced transcriptional responses and help identify compounds capable of modulating genes, pathways or cellular states of interest."),
                  tags$p("By systematically organizing and harmonizing genetic and chemical perturbation signatures, PerturbSigDB supports gene-level query, pathway- and gene set-level perturbation analysis, custom signature matching, cross-perturbation comparison and downloadable gene-level result exploration. It is designed to facilitate upstream regulator discovery, pathway-centered perturbation mining, chemical modulator identification, mechanism-of-action analysis, disease mechanism interpretation and drug discovery."),
                  
                  tags$h4("Key Features", style = "color: #3a7bd5; margin-top: 20px;"),
                  tags$ul(
                    tags$li(tags$b("Browse"), " — Explore large-scale genetic and chemical perturbation signatures across cell lines, tissues, diseases, perturbagens, target genes and source accessions. Users can inspect curated metadata, gene-level perturbation results, compound structures, protein interaction networks, differential expression profiles and downloadable outputs."),
                    tags$li(tags$b("Gene Query"), " — Identify genetic perturbations and chemical compounds that significantly up-regulate or down-regulate a gene of interest. Genetic perturbation results may nominate candidate upstream regulatory factors, whereas chemical perturbation results indicate compounds capable of modulating the queried gene."),
                    tags$li(tags$b("Gene Set Query"), " — Detect perturbations associated with coordinated activation or suppression of curated pathways or user-defined gene sets. This module supports pathway-level upstream regulator discovery and chemical modulator identification by comparing pathway effects across genetic and chemical perturbations."),
                    tags$li(tags$b("Perturb Query"), " — Upload a custom transcriptional signature and compare it with reference genetic and chemical perturbation profiles using Spearman rank correlation. This module identifies perturbations with similar or opposite global transcriptional effects, supporting mechanism comparison, phenotype matching, signature reversal and cross-perturbation analysis."),
                    tags$li(tags$b("Interactive Visualization"), " — PerturbSigDB provides interactive result tables, differential expression plots, rank plots, signature alignment plots, compound structure diagrams, STRING protein interaction networks, pathway enrichment results and downloadable outputs to support transparent and reusable interpretation.")
                  ),

                  tags$h4("Biological Significance", style = "color: #3a7bd5; margin-top: 20px;"),
                  tags$p("Genetic and chemical perturbations provide complementary views of cellular regulation. Genetic perturbations help reveal how disruption, repression or activation of specific genes affects downstream transcriptional programs, supporting the discovery of candidate upstream regulatory factors. Chemical perturbations reflect cellular responses to small-molecule treatment and can nominate compounds capable of modulating genes, pathways or transcriptional states of interest."),
                  tags$p("By integrating both perturbation modalities, PerturbSigDB allows users to ask whether a genetic perturbation regulates a gene or pathway of interest, whether a compound phenocopies a genetic perturbation, whether genetic and chemical perturbations converge on the same pathway, or whether a compound can counteract a disease-associated or user-defined transcriptional state."),
                  tags$p("Therefore, PerturbSigDB provides a unified perturbation-signature resource for ",tags$b("upstream regulator discovery, chemical modulator identification and matched perturbation-state analysis.")),
                  tags$hr(),

                  tags$h4("Data Curation", style = "color: #3a7bd5; margin-top: 20px;"),
                  tags$p("A total of 7,001 data accessions were processed and analyzed to generate the final dataset. Key metadata fields were then manually extracted and curated for each perturbation entry, including perturbation type, perturbagen name/identifier, target gene (for genetic perturbations), cell line, tissue or disease annotation, source accession, and dose and time information when available. The overall data distribution is shown below."),
                  
                  fluidRow(
                    column(4,
                      div(style = "text-align:center;", tags$b("Perturbation Type Distribution")),
                      echarts4rOutput("curation_pie_overview", height = "350px")
                    ),
                    column(4,
                      div(style = "text-align:center;", tags$b("Genetic Perturbation Subtypes")),
                      echarts4rOutput("curation_pie_genetic", height = "350px")
                    ),
                    column(4,
                      div(style = "text-align:center;", tags$b("Accession Distribution")),
                      echarts4rOutput("curation_pie_accession", height = "350px")
                    )
                  )
                  )
              ),
              
              # ========== Tab 2: Quick Start ==========
              tabPanel("Quick Start",
                tags$div(style = "padding: 20px; max-width: 1400px; margin: 0 auto;",
                  tags$h3("Getting Started with PerturbSigDB", style = "color: #2c5f8a;"),
                  tags$p("PerturbSigDB provides four major analysis modules accessible from the left sidebar: ", tags$b("Browse"), ", ", tags$b("Gene Query"), ", ", tags$b("Gene Set Query"), " and ", tags$b("Perturb Query"), ". Together, these modules support large-scale perturbation signature exploration, gene-centered regulatory interrogation, pathway- and gene set-level perturbation analysis, and user-defined transcriptional signature matching."),
                  
                  tags$div(class = "help-workflow",
                    tags$div(class = "help-step",
                      tags$div(class = "help-step-num", "1"),
                      tags$div(
                        tags$b("Browse — Explore Perturbation Signatures"),
                        tags$p("The Browse module provides a searchable overview of manually curated genetic and chemical perturbation signatures in PerturbSigDB."),
                        tags$p("Users can filter genetic perturbations by perturbed gene, cell line, tissue type, disease annotation or accession ID. Chemical perturbations can be filtered by perturbagen name, cell line, tissue type, disease annotation or accession ID. Partial text matching is supported. For example, entering MCF will return entries such as MCF7 and MCF10A."),
                        tags$p("Clicking a ", tags$b("Dataset"), " link opens a detailed information panel containing curated metadata, differential expression visualizations, compound structure diagrams for chemical perturbations, and STRING protein-protein interaction networks for genetic perturbations. Users can also download the corresponding differential expression data for each perturbation signature.")
                      )
                    ),
                    tags$div(class = "help-step",
                      tags$div(class = "help-step-num", "2"),
                      tags$div(
                        tags$b("Gene Query — Gene-Centered Regulatory Search"),
                        tags$p("The Gene Query module identifies perturbations in which a selected gene is significantly up-regulated or down-regulated."),
                        tags$p("Users can enter a gene symbol, such as ", tags$em("TP53"), ", ", tags$em("EGFR"), " or ", tags$em("MYC"), ", using the searchable gene dropdown. A |logFC| cutoff can be applied to exclude weak expression changes. Results can also be restricted by perturbagen type: ", tags$b("All"), ", ", tags$b("Chemical only"), " or ", tags$b("Genetic only"), "."),
                        tags$p("Results are organized into four categories: chemical suppressors, genetic suppressors, chemical activators and genetic activators. Genetic perturbation results may suggest candidate upstream regulatory genes affecting the queried gene, whereas chemical perturbation results indicate compounds capable of modulating its expression. Each result links to the corresponding ", tags$b("Dataset"), " detail panel and provides a row-level download option for the full differential expression profile.")
                      )
                    ),
                    tags$div(class = "help-step",
                      tags$div(class = "help-step-num", "3"),
                      tags$div(
                        tags$b("Gene Set Query — Pathway- and Gene Set-Level Perturbation Analysis"),
                        tags$p("The Gene Set Query module identifies perturbations associated with coordinated activation or suppression of a curated pathway or user-defined gene set."),
                        tags$p("Users can select a pathway database, such as ", tags$b("KEGG"), ", ", tags$b("Hallmark"), ", ", tags$b("Reactome"), ", ", tags$b("WikiPathways"), " or other supported sources, and search for a pathway using the fuzzy-matching dropdown. Alternatively, users can upload a custom gene list, with one gene symbol per line. Uploaded genes take priority over the selected pathway."),
                        tags$p("Users can refine the analysis by enrichment direction, perturbagen type and FDA approval status for chemical perturbagens. After clicking ", tags$b("Run Pathway Query"), ", the system returns enrichment results, including ", tags$b("NES"), ", nominal P-value, adjusted P-value and leading-edge genes. Downstream visualizations, including dot plots, bar plots and GSEA running enrichment plots, are available for interpreting selected perturbation signatures."),
                        tags$p("This module supports pathway-level upstream regulator discovery and chemical modulator identification. Genetic perturbation results may reveal genes whose perturbation activates or suppresses the queried pathway, whereas chemical perturbation results may nominate compounds capable of modulating the same functional program.")
                      )
                    ),
                    tags$div(class = "help-step",
                      tags$div(class = "help-step-num", "4"),
                      tags$div(
                        tags$b("Perturb Query — Signature Matching and Biological Interpretation"),
                        tags$p("The Perturb Query module allows users to upload a custom transcriptional signature and identify database perturbations with similar or opposite global transcriptomic effects."),
                        tags$p("Users can paste or upload gene-logFC pairs in plain-text, CSV or TSV format. At least five valid gene-logFC pairs are required. Users can set the number of returned results and filter by perturbagen type. For chemical perturbations, users may further restrict results to FDA-approved drugs."),
                        tags$p("Results are ranked by ", tags$b("Spearman_Rho"), ", which ranges from -1 to +1. Positive values indicate transcriptional concordance between the query signature and a database perturbation, whereas negative values indicate an opposing expression pattern."),
                        tags$p("The biological interpretation depends on the input signature:"),
                        tags$ul(
                          tags$li("For a ", tags$b("drug-induced signature"), ", concordant genetic perturbations may suggest candidate targets, upstream regulatory genes or mechanism-of-action hypotheses associated with the drug response."),
                          tags$li("For a ", tags$b("gene knockdown, knockout or overexpression signature"), ", concordant chemical perturbations may represent candidate pathway modulators or compounds that phenocopy the genetic perturbation."),
                          tags$li("For a ", tags$b("disease-associated signature"), ", perturbations with strong negative correlations may indicate candidate reversal agents that potentially counteract disease-associated transcriptional changes."),
                          tags$li("For a ", tags$b("user-defined functional signature"), ", similar or opposite database perturbations may help identify related perturbation states, convergent mechanisms or modulatory compounds.")
                        ),
                        tags$p("Users can click each result to inspect the rank plot and heatmap, which visualize how query genes are distributed and regulated across matched perturbation signatures."),
                        tags$p(tags$em("These results should be interpreted as hypothesis-generating evidence. Transcriptomic concordance or reversal may suggest candidate targets, regulatory mechanisms, chemical modulators or therapeutic leads, but does not by itself establish direct drug-target interactions, causal regulation or therapeutic efficacy. Further validation using target annotation, pathway analysis, literature evidence or experimental assays is recommended."))
                      )
                    )
                  ),
                  
                  tags$hr(),
                  tags$h4("Typical Use Cases", style = "color: #3a7bd5;"),
                  tags$ul(
                    tags$li(tags$b("Upstream Regulator Discovery"), " — Use Gene Query or Gene Set Query to identify genetic perturbations that activate or suppress a gene, pathway or user-defined gene set of interest."),
                    tags$li(tags$b("Chemical Modulator Identification"), " — Use Gene Query or Gene Set Query to discover compounds that modulate a specific gene or pathway-level transcriptional program."),
                    tags$li(tags$b("Drug Discovery / Repurposing"), " — Upload a disease-associated expression signature and identify chemical perturbations with strong negative correlations that may reverse disease-related transcriptional changes."),
                    tags$li(tags$b("Target and Mechanism Inference"), " — Upload a drug-induced signature and examine concordant genetic perturbations to infer candidate targets, upstream regulatory genes or related mechanisms of action."),
                    tags$li(tags$b("Candidate Phenocopy Discovery"), " — Upload a gene knockdown, knockout or overexpression signature and identify concordant chemical perturbations that may phenocopy the genetic perturbation."),
                    tags$li(tags$b("Pathway-Level Interpretation"), " — Use Gene Set Query to identify chemical or genetic perturbations that activate or suppress pathways of interest, such as apoptosis, epithelial-mesenchymal transition or p53 signaling."),
                    tags$li(tags$b("Cross-Perturbation Comparison"), " — Use Browse, Gene Query, Gene Set Query and Perturb Query together to compare genetic and chemical perturbations across cell lines, tissues, disease annotations and transcriptional states.")
                  ) )
              ),
              
              # ========== Tab 3: Browse ==========
              tabPanel("Browse",
                tags$div(style = "padding: 20px; max-width: 1400px; margin: 0 auto;",
                  tags$h3("Browse Module: How to Explore Perturbation Signatures?", style = "color: #2c5f8a;"),
                  tags$p("The Browse module is the entry point for exploring manually curated perturbation signatures in PerturbSigDB. It allows users to search genetic and chemical perturbations, inspect their transcriptomic effects, examine associated metadata, and select datasets for downstream biological interpretation."),
                  
                  tags$h4("What can I find in the Browse module?", style = "color: #3a7bd5;"),
                  tags$p("The Browse module contains two major sections: ", tags$b("Genetic Perturbagens"), " and ", tags$b("Chemical Perturbagens"), "."),
                  tags$p("If your question starts from a gene, such as ", tags$em("\"Has TP53 knockdown, knockout or overexpression been profiled?\""), ", use the ", tags$b("Genetic Perturbagens"), " tab. Users can search by perturbed gene, cell line, tissue type, disease annotation or accession ID."),
                  tags$p("If your question starts from a drug or compound, such as ", tags$em("\"What transcriptional effects have been reported for imatinib or doxorubicin?\""), ", use the ", tags$b("Chemical Perturbagens"), " tab. Users can search by perturbagen name, cell line, tissue type, disease annotation or accession ID."),
                  tags$a(
                    href = "browse1.png",
                    target = "_blank",
                    tags$img(
                      src = "browse1.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$p("Both tabs support keyword-based filtering and partial text matching, enabling users to quickly locate perturbation signatures of interest across different experimental contexts."),
                  
                  tags$h4("How do I select a dataset for exploration?", style = "color: #3a7bd5;"),
                  tags$p("Enter one or more keywords in the filter boxes and click ", tags$b("Search"), ". The result table will return perturbation signatures matching the selected criteria. Click ", tags$b("Clear"), " to reset all filters."),
                  tags$a(
                    href = "browse2.png",
                    target = "_blank",
                    tags$img(
                      src = "browse2.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$p("After identifying a relevant record, click the ", tags$b("Dataset"), " link to open the detailed information panel. This panel provides curated metadata for the selected perturbation, including the perturbagen or perturbed gene, cell line, tissue type, disease annotation, accession ID and perturbation type."),
                  tags$a(
                    href = "browse3.png",
                    target = "_blank",
                    tags$img(
                      src = "browse3.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$p("For chemical perturbations, compound structure information is shown when available. For genetic perturbations, a STRING protein-protein interaction network is provided to place the perturbed gene in its functional interaction context. Each dataset is also accompanied by differential expression visualizations that summarize the transcriptomic response induced by the perturbation."),
                  tags$a(
                    href = "browse4.png",
                    target = "_blank",
                    tags$img(
                      src = "browse4.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$h4("How can I perform pathway-level interpretation?", style = "color: #3a7bd5;"),
                  tags$p("After selecting a dataset, users can directly run pathway enrichment analysis from the Browse module to identify biological pathways significantly affected by the selected perturbation."),
                  tags$p("To perform pathway enrichment analysis, select a row in the result table, click ", tags$b("Analysis"), ", choose a pathway database, and then click ", tags$b("Run"), ". The system uses the differentially expressed genes from the selected perturbation signature to perform enrichment analysis."),
                  tags$a(
                    href = "browse5.png",
                    target = "_blank",
                    tags$img(
                      src = "browse5.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$p("Dot plots and bar plots are provided to visualize the most significantly affected biological pathways."),
                  tags$a(
                    href = "browse6.png",
                    target = "_blank",
                    tags$img(
                      src = "browse6.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$p("This function helps users move from a single perturbation signature to pathway-level biological interpretation. For genetic perturbations, pathway enrichment may reveal downstream biological processes regulated by the perturbed gene. For chemical perturbations, it may suggest pathways modulated by the compound."),
                  
                  tags$h4("Start Exploring", style = "color: #3a7bd5;"),
                  tags$p("Users can start from a gene, compound, cell line, tissue, disease annotation or accession ID to explore genetic and chemical perturbation signatures in PerturbSigDB. After locating a relevant dataset, users can inspect its curated metadata, interpret its differential expression profile, examine pathway-level effects and download the corresponding results. These analyses can help generate testable hypotheses about upstream regulators, chemical modulators, pathway regulation and perturbation-induced cellular states.")
                )
              ),
              
              # ========== Tab 4: Gene Query ==========
              tabPanel("Gene Query",
                tags$div(style = "padding: 20px; max-width: 1400px; margin: 0 auto;",
                  tags$h3("Gene Query — Single-Gene Regulatory Search", style = "color: #2c5f8a;"),
                  tags$p("The Gene Query module identifies genetic and chemical perturbations that significantly affect the expression of a user-specified gene. This module helps users ask a direct biological question: which genetic perturbations or chemical compounds activate or suppress my gene of interest?"),
                  tags$p("For genetic perturbations, the results may nominate candidate upstream regulatory genes whose perturbation influences the query gene. For chemical perturbations, the results indicate compounds capable of modulating the expression of the query gene. Therefore, Gene Query supports single-gene-centered upstream regulator discovery, chemical modulator identification and mechanism-oriented interpretation."),
                  
                  tags$h4("How It Works", style = "color: #3a7bd5;"),
                  tags$p("Gene Query uses a pre-built perturbation signature index that maps each gene to its expression changes across all genetic and chemical perturbation signatures. Each row corresponds to a gene, each column corresponds to a perturbation dataset, and each cell stores the log2 fold-change value of that gene under the corresponding perturbation condition."),
                  tags$p("When a query gene is entered, the system:"),
                  tags$ol(
                    tags$li("Searches the gene across both chemical and genetic perturbation signature matrices."),
                    tags$li("Extracts perturbations in which the query gene shows a valid expression change."),
                    tags$li("Filters results according to the user-defined |logFC| cutoff and perturbagen type."),
                    tags$li("Separates perturbations into suppressors and activators according to the direction of expression change."),
                    tags$li("Ranks results by the absolute magnitude of the expression effect.")
                  ),
                  tags$p("Negative logFC values indicate that the perturbation suppresses the expression of the query gene, whereas positive logFC values indicate that the perturbation activates its expression."),
                  
                  tags$h4("Input Parameters", style = "color: #3a7bd5;"),
                  tags$table(class = "help-table",
                    tags$thead(tags$tr(tags$th("Parameter"), tags$th("Description"), tags$th("Example"))),
                    tags$tbody(
                      tags$tr(tags$td(tags$b("Gene")), tags$td("Searchable gene dropdown. Users can start typing to filter the 56,448-gene index. Fuzzy matching is supported."), tags$td(tags$code("TP53"), ", ", tags$code("EGFR"), ", ", tags$code("MYC"))),
                      tags$tr(tags$td(tags$b("Perturbagen Type")), tags$td("Restrict results to All, Chemical only or Genetic only perturbations. Chemical perturbations correspond to drugs or compounds. Genetic perturbations include knockdown, knockout, CRISPR-based perturbation and overexpression when available."), tags$td(tags$code("Chemical only"))),
                      tags$tr(tags$td(tags$b("|logFC| Cutoff")), tags$td("Minimum absolute log2 fold-change. Perturbations with smaller effects are excluded."), tags$td(tags$code("1.0")))
                    )
                  ),
                  
                  tags$h4("Output Tables", style = "color: #3a7bd5;"),
                  tags$p("Gene Query returns four interactive result tables:"),
                  tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-bottom: 20px;",
                    tags$div(style = "background: #e3f2fd; border-radius: 10px; padding: 15px;",
                      tags$h5(icon("arrow-down"), " Top Suppressors — Genetic", style = "color: #0d6efd;"),
                      tags$p("Genetic perturbations that most strongly ", tags$b("decrease"), " the expression of the query gene. These results may suggest candidate upstream regulatory genes whose perturbation suppresses the queried gene.")
                    ),
                    tags$div(style = "background: #e3f2fd; border-radius: 10px; padding: 15px;",
                      tags$h5(icon("arrow-down"), " Top Suppressors — Chemical", style = "color: #0d6efd;"),
                      tags$p("Chemical compounds that most strongly ", tags$b("decrease"), " the expression of the query gene. Results have negative logFC values and are ranked by absolute effect magnitude.")
                    ),
                    tags$div(style = "background: #fff3cd; border-radius: 10px; padding: 15px;",
                      tags$h5(icon("arrow-up"), " Top Activators — Genetic", style = "color: #fd7e14;"),
                      tags$p("Genetic perturbations that most strongly ", tags$b("increase"), " the expression of the query gene. These results may suggest candidate upstream regulatory genes whose perturbation activates the queried gene.")
                    ),
                    tags$div(style = "background: #fff3cd; border-radius: 10px; padding: 15px;",
                      tags$h5(icon("arrow-up"), " Top Activators — Chemical", style = "color: #fd7e14;"),
                      tags$p("Chemical compounds that most strongly ", tags$b("increase"), " the expression of the query gene. Results have positive logFC values and are ranked by effect magnitude.")
                    )
                  ),
                  tags$h4("Example: Run the Demo query to explore PCNA-related perturbation signatures ", style = "color: #3a7bd5;"),
                  tags$p("Click the ",tags$b("Demo") ,"button to explore a PCNA-related signature and identify candidate chemical modulators, upstream regulators, or functionally related genetic perturbations."),
                  tags$a(
                    href = "Gene Query1.png",
                    target = "_blank",
                    tags$img(
                      src = "Gene Query1.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$p("The Demo query uses PCNA as the target gene, with a logFC cutoff set to 2."),
                  tags$p("Four result tables display candidate genetic regulators and chemical modulators that may suppress or activate PCNA expression."),
                  tags$a(
                    href = "Gene Query2.png",
                    target = "_blank",
                    tags$img(
                      src = "Gene Query2.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$p("Click on any Dataset to view detailed information and downstream analysis."),
                  tags$a(
                    href = "Gene Query3.png",
                    target = "_blank",
                    tags$img(
                      src = "Gene Query3.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$h4("Interpretation Tips", style = "color: #3a7bd5;"),
                  tags$p("Gene Query can be used to identify perturbations that regulate a specific gene of interest. Users are encouraged to combine Gene Query results with pathway analysis, known target annotations, literature evidence, protein interaction information and experimental validation.")
                )
              ),
              
              # ========== Tab 5: Gene Set Query ==========
              tabPanel("Gene Set Query",
                tags$div(style = "padding: 20px; max-width: 1400px; margin: 0 auto;",
                  tags$h3("Gene Set Query — Pathway-Based Perturbation Mining", style = "color: #2c5f8a;"),
                  tags$p("The Gene Set Query module identifies perturbations that significantly modulate a specified pathway or user-defined gene set. This module is designed for pathway-centered perturbation discovery, helping users ask: ",tags$p("which genetic perturbations or chemical compounds activate or suppress my pathway or gene set of interest?")),
                  tags$p("Gene Set Query uses a gene-set enrichment strategy. For each perturbation signature in PerturbSigDB, the system evaluates whether genes in the selected pathway or custom gene set are coordinately up-regulated or down-regulated compared with the genome-wide background. Genetic perturbation results may suggest candidate pathway-regulating genes, whereas chemical perturbation results may nominate compounds capable of modulating the selected pathway or gene set."),
                  
                  tags$h4("Available Pathway Databases", style = "color: #3a7bd5;"),
                  tags$table(class = "help-table",
                    tags$thead(tags$tr(tags$th("Database"), tags$th("Typical # Pathways"))),
                    tags$tbody(
                      tags$tr(tags$td(tags$b("KEGG")), tags$td("186")),
                      tags$tr(tags$td(tags$b("Hallmark")), tags$td("50")),
                      tags$tr(tags$td(tags$b("BioCarta")), tags$td("237")),
                      tags$tr(tags$td(tags$b("Elsevier Pathway")), tags$td("1720")),
                      tags$tr(tags$td(tags$b("HumanCyc")), tags$td("152")),
                      tags$tr(tags$td(tags$b("Panther")), tags$td("112")),
                      tags$tr(tags$td(tags$b("Reactome")), tags$td("2104")),
                      tags$tr(tags$td(tags$b("WikiPathways")), tags$td("828")),
                      tags$tr(tags$td(tags$b("Custom")), tags$td("——"))
                    )
                  ),
                  tags$p("Each pathway must contain at least 3 genes to be included in the analysis."),
                  
                  tags$h4("Exmaple: Which perturbations significantly modulate a gene set of interest?", style = "color: #3a7bd5;"),
                  tags$p(tags$b("Data Input:")," Users can select a built-in pathway or provide a custom gene list. In this example, the Hallmark E2F_TARGETS pathway, containing 50 genes, is selected as the input gene set. Custom gene sets can be provided by directly pasting gene symbols or uploading a file. When both a built-in pathway and a custom gene set are provided, the custom gene set takes priority for analysis."),
                    tags$a(
                      href = "GSQ1.png",
                      target = "_blank",
                      tags$img(
                        src = "GSQ1.png",
                        width = "90%",
                        style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                      )
                    ),
                  tags$p(tags$b("Parameter Settings:")," Users can select the direction of regulation, choose between chemical and genetic perturbations, optionally restrict chemical results to FDA-approved drugs, and then click Run to start the analysis."),
                  tags$a(
                    href = "GSQ2.png",
                    target = "_blank",
                    tags$img(
                      src = "GSQ2.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  
                  ),
                  tags$p(tags$b("Results:")," The identified perturbations are ranked by regulatory significance and visualized in a scatter plot."),
                  tags$a(
                    href = "GSQ3.png",
                    target = "_blank",
                    tags$img(
                      src = "GSQ3.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$p("Click on any Dataset to view detailed information and downstream analysis."),
                  tags$a(
                    href = "GSQ4.png",
                    target = "_blank",
                    tags$img(
                      src = "GSQ4.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$p("Select perturbations and pathways of interest to examine their specific pathway-level effects."),
                  tags$a(
                    href = "GSQ5.png",
                    target = "_blank",
                    tags$img(
                      src = "GSQ5.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$h4("Result Tables", style = "color: #3a7bd5;"),
                  tags$p("Results are displayed in separate Chemical and Genetic tabs. Each table lists perturbations ranked by their effect on the selected pathway or custom gene set."),
                  tags$p("Key result fields include:"),
                  
                  tags$ul(
                    tags$li(tags$b("NES:"), " Normalized Enrichment Score. Positive NES indicates pathway activation or up-regulation; negative NES indicates pathway suppression or down-regulation."),
                    tags$li(tags$b("P-value / Padj:"), " Nominal and Benjamini-Hochberg adjusted P-values for the pathway-level perturbation effect."),
                    tags$li(tags$b("Leading Edge Genes:"), " Pathway genes that contribute most strongly to the observed enrichment. This field can be expanded to inspect the contributing genes."),
                    tags$li(tags$b("Dataset:"), " Clickable dataset identifier that opens the standard differential expression detail panel for the selected perturbation signature."),
                    tags$li(tags$b("Rank Plots:"), " Interactive rank plots are displayed above the result tables to show the distribution of NES values and highlight top-ranked perturbations.")
                  ),
                  
                  tags$h4("Biological Interpretation", style = "color: #3a7bd5;"),
                  tags$p("The interpretation of Gene Set Query results depends on perturbation type and enrichment direction."),
                  tags$ol(
                    tags$li("Genetic activators may suggest genes whose perturbation activates the queried pathway or gene set."),
                    tags$li("Genetic suppressors may suggest genes whose perturbation suppresses the queried pathway or gene set."),
                    tags$li("Chemical activators may nominate compounds capable of inducing the queried pathway-level program."),
                    tags$li("Chemical suppressors may nominate compounds capable of inhibiting, suppressing or reversing the queried pathway-level program."),
                  ),
                  tags$p("For example, if a compound strongly suppresses the E2F Targets gene set, it may indicate cell-cycle inhibitory activity. If a genetic perturbation activates an apoptosis-related pathway, it may suggest a regulatory relationship between the perturbed gene and apoptosis-associated transcriptional programs.
                  These results should be interpreted as hypothesis-generating evidence. Pathway-level activation or suppression does not by itself prove direct regulation, direct drug-target interaction or therapeutic efficacy. Users are encouraged to combine Gene Set Query results with known target annotations, pathway knowledge, protein interaction information, literature evidence and experimental validation."),
                  tags$h4("Interpretation Tip", style = "color: #3a7bd5;"),
                  tags$p("Gene Set Query is most useful when the biological question is centered on a pathway, functional module or custom gene set rather than a single marker gene. Users can use this module to identify pathway-regulating genes, pathway-modulating compounds, genetic perturbations that converge on the same biological program, or chemical perturbations that potentially counteract disease-related pathway activity.")
                )
              ),
              
              # ========== Tab 6: Perturb Query ==========
              tabPanel("Perturb Query",
                tags$div(style = "padding: 20px; max-width: 1400px; margin: 0 auto;",
                  tags$h3("Perturb Query — Signature-Based Perturbation Matching", style = "color: #2c5f8a;"),
                  tags$p("The Perturb Query module compares a user-provided transcriptional signature with both genetic and chemical perturbation signatures curated in PerturbSigDB. This module enables users to identify perturbations that produce similar or opposite global transcriptomic effects."),
                  tags$p("Different from Gene Query, which starts from a single gene, and Gene Set Query, which starts from a pathway or custom gene set, Perturb Query starts from a complete transcriptional signature. The uploaded signature can be derived from a disease state, drug treatment, genetic perturbation, experimental condition or user-defined comparison."),
                  tags$p("By matching the query signature against both genetic and chemical perturbation profiles, Perturb Query supports multiple analysis scenarios, including identifying genetic perturbations that resemble a drug-induced response, finding compounds that phenocopy a gene perturbation, discovering perturbations that reverse a disease-associated signature, and comparing transcriptional states across different perturbation types."),
                  
                  tags$h4("Input Format", style = "color: #3a7bd5;"),
                  tags$p("Users can provide a two-column table containing gene names and logFC values by file upload or text input."),
                  tags$pre(style = "background: #f5f5f5; padding: 15px; border-radius: 8px; font-size: 13px;",
                    "Gene    logFC
TP53    -2.34
EGFR     1.87
MYC     -0.95
CDKN1A   3.21
BRCA1   -1.45
...      ..."
                  ),
                  tags$ul(
                    tags$li(tags$b("Gene:"), " Gene should be an HGNC gene symbol. Gene symbols are case-insensitive and will be automatically converted to uppercase."),
                    tags$li(tags$b("logFC:"), "logFC should be a log2 fold-change value. Positive values indicate up-regulation in the query condition, whereas negative values indicate down-regulation."),
                    tags$li(tags$b("Input columns:"), " can be separated by tab, comma or space. A header row is optional but recommended. At least five valid gene-logFC pairs are required. Duplicate genes are automatically removed, and only the first occurrence is retained. Missing, NA or non-finite logFC values are excluded."),
                    tags$li("Users can click Download Demo File to obtain an example input signature, or click Demo to automatically populate the input area."),
                    ),
                  tags$h4("Exmaple:Which perturbations produce similar or opposite global transcriptomic effects to your query signature?", style = "color: #3a7bd5;"),
                  tags$p(tags$b("Data Input:")," PerturbSigDB accepts two convenient input methods: file upload and copy-and-paste input.After providing the input signature, click Run to start the analysis."),
                  tags$a(
                    href = "PQ1.png",
                    target = "_blank",
                    tags$img(
                      src = "PQ1.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$h4("Visualizations", style = "color: #3a7bd5;"),
                  tags$p(tags$b("Rank Plot:")," The scatter plot allows users to inspect the distribution of matched perturbations and quickly identify top-ranked candidates."),
                  tags$a(
                    href = "PQ2.png",
                    target = "_blank",
                    tags$img(
                      src = "PQ2.png",
                      width = "90%",
                      style = "display: block; margin: 15px auto; border:1px solid #eee; cursor: pointer;"
                    )
                  ),
                  tags$p(tags$b("Heatmap:")," PerturbSigDB generates heatmaps according to signature similarity ranking. Matched perturbations are ranked by Spearman_Rho, and the system separately displays the top 10 most positively correlated perturbations and the bottom 10 most negatively correlated perturbations. The top 10 heatmap represents perturbations whose transcriptional effects are most similar to the query signature, whereas the bottom 10 heatmap represents perturbations with the strongest opposite effects. This design helps users compare concordant and opposing perturbation states side by side."),
                  
                 
                  tags$h4("Interpretation Guide", style = "color: #3a7bd5;"),
                  tags$p("The biological interpretation of Perturb Query depends on the source of the input signature, the matched perturbation type and the correlation direction."),
                  
                  tags$ul(
                    tags$li(tags$b("Strong negative Spearman_Rho"), " The database perturbation opposes the query signature and may reverse the queried transcriptional state."),
                    tags$li(tags$b("Strong positive Spearman_Rho"), " The database perturbation resembles the query signature and may induce a similar transcriptional state."),
                    ),
                  tags$p("Examples of biological interpretation include:"),
                  tags$ul(
                    tags$li(tags$b("For a disease-associated signature, "), " chemical perturbations with strong negative correlations may nominate candidate compounds that potentially reverse disease-associated transcriptional changes."),
                    tags$li(tags$b("For a drug-induced signature,"), " concordant genetic perturbations may suggest candidate targets, upstream regulatory genes or mechanism-of-action hypotheses associated with the compound response."),
                    tags$li(tags$b("For a genetic perturbation signature,"), " concordant chemical perturbations may indicate compounds that phenocopy the genetic perturbation or modulate related pathways."),
                    tags$li(tags$b("For a chemical perturbation signature,"), " concordant genetic perturbations may help identify gene perturbations associated with similar downstream transcriptional effects."),
                    tags$li(tags$b("For a user-defined functional signature,"), " similar or opposite database perturbations may help identify related perturbation states, convergent mechanisms or modulatory compounds."),
                    ),
                  tags$p("These results should be interpreted as hypothesis-generating evidence. Transcriptomic similarity or reversal does not by itself establish direct drug-target interactions, direct causal regulation or therapeutic efficacy. Users are encouraged to combine Perturb Query results with known target annotations, pathway analysis, literature evidence, protein interaction information and experimental validation."),
                   )
              ),
              
              # ========== Tab 7: Case Study 1 (EZH2 KO Immune) ==========
              tabPanel("Case 1",
                tags$iframe(
                  src = "case_study.html",
                  width = "100%",
                  height = "800px",
                  style = "border: none; border-radius: 6px;"
                )
              ),
              
              # ========== Tab 7b: Case Study 2 (ARID1A KO Immune) ==========
              tabPanel("Case 2",
                tags$iframe(
                  src = "case_study2.html",
                  width = "100%",
                  height = "800px",
                  style = "border: none; border-radius: 6px;"
                )
              ),
              # ========== Tab 8: Why is PerturbSigDB ==========
              tabPanel("Why PerturbSigDB?",
                tags$div(style = "padding: 20px; max-width: 1400px; margin: 0 auto;",
                  tags$h4("Comparison with Related Resources", style = "color: #2c5f8a;"),
                  tags$p("PerturbSigDB was developed to provide a manually curated perturbation signature knowledgebase for connecting genes, pathways and user-defined transcriptional signatures with both genetic perturbations and chemical modulators."),
                  tags$p("Existing perturbation resources have made important contributions to the reuse of expression-response data. However, many resources are primarily focused on either chemical perturbations or genetic perturbations, or are designed for specific types of signature browsing and comparison. PerturbSigDB addresses this gap by integrating chemical perturbation signatures with loss-of-function and gain-of-function genetic perturbation signatures in a unified metadata and query framework."),
                  tags$p("The current release of PerturbSigDB contains ", tags$b("1,091,148"), " perturbation signatures, including ", tags$b("919,964"), " chemical perturbation signatures and ", tags$b("171,184"), " genetic perturbation signatures, covering ", tags$b("44,943"), " chemical perturbagens, ", tags$b("10,029"), " genetically perturbed genes across ", tags$b("1,737"), " cell lines. These records were manually curated and harmonized from ", tags$b("7,001"), " public perturbation studies."),
                  tags$p("PerturbSigDB is designed to support both forward and reverse perturbation exploration. Users can browse curated perturbation entries, inspect full gene-level differential expression profiles, perform pathway-level interpretation and identify perturbations that activate, suppress, resemble or oppose user-specified molecular programs."),
                  tags$h4("Feature Comparison"),
                  tags$br(),
                  box(width = 12, title = "Feature Comparison", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    div(style = "overflow-x: auto; width: 100%;", DTOutput("help_overview_comparison", width = "100%"))
                  ),
                  tags$br(),
                  tags$h4("Key Advantages of PerturbSigDB"),
                  tags$p("PerturbSigDB differs from existing perturbation resources in three major aspects."),
                  tags$p(tags$b("First, "), "PerturbSigDB is built as a manually curated and harmonized perturbation signature knowledgebase, rather than a simple index of public perturbation datasets. Perturbation type, perturbagen identity, perturbed gene or target annotation, cell-line context, tissue information, source accession and gene-level results were organized into a unified framework."),
                  tags$p(tags$b("Second, "), "PerturbSigDB integrates genetic perturbation signatures and chemical perturbation signatures within the same database. This enables users to compare candidate regulatory genes and chemical modulators in a unified transcriptomic space."),
                  tags$p(tags$b("Third, "), "PerturbSigDB supports gene-, pathway- and signature-guided reverse discovery. Users can start from a single gene, a curated pathway, a custom gene set or a user-defined transcriptional signature to identify perturbations that activate, suppress, resemble or oppose the queried molecular program."),
                  tags$p("Compared with chemical perturbation resources such as ChemPert, PerturbSigDB further integrates chemical perturbation signatures with genetic perturbation signatures and provides gene-, pathway- and signature-guided query functions. Compared with genetic perturbation-focused resources such as GPSAdb and PerturbAtlas, PerturbSigDB extends perturbation analysis by jointly considering candidate regulatory genes and chemical modulators."),
                  tags$p("Together, these features make PerturbSigDB a value-added resource for functional genomics, perturbation biology, upstream regulator discovery, pathway regulation analysis, mechanism-of-action studies and therapeutic discovery.")
                )
              )


            ),
        
        # Help page CSS
        tags$style(HTML("
          .help-stat {
            background: linear-gradient(135deg, #e8f4fd 0%, #dceefb 100%);
            border-radius: 10px;
            padding: 15px;
            text-align: center;
            font-size: 14px;
            color: #2c5f8a;
            border: 1px solid #c5d9ed;
          }
          .help-stat b {
            font-size: 22px;
            display: block;
            color: #1a5276;
          }
          .help-workflow {
            display: flex;
            flex-direction: column;
            gap: 12px;
            margin: 15px 0;
          }
          .help-step {
            display: flex;
            align-items: flex-start;
            gap: 15px;
            background: #f8f9fa;
            border-radius: 10px;
            padding: 15px;
            border-left: 4px solid #3a7bd5;
          }
          .help-step-num {
            background: #3a7bd5;
            color: white;
            width: 32px;
            height: 32px;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            font-weight: bold;
            font-size: 16px;
            flex-shrink: 0;
          }
          .help-step p { margin: 5px 0 0 0; color: #555; }
          .help-table {
            width: 100%;
            border-collapse: collapse;
            margin: 10px 0 15px 0;
          }
          .help-table th {
            background: #3a7bd5;
            color: white;
            padding: 8px 12px;
            text-align: left;
          }
          .help-table td {
            padding: 8px 12px;
            border-bottom: 1px solid #e0e0e0;
          }
          .help-table tr:nth-child(even) td {
            background: #f8f9fa;
          }
          .faq-item {
            margin-bottom: 20px;
            padding-bottom: 15px;
            border-bottom: 1px solid #eee;
          }
          .faq-item h4 {
            color: #3a7bd5;
            margin-bottom: 8px;
          }
          .faq-item p {
            margin-left: 25px;
            color: #444;
          }
          #help_tabs .nav-tabs > li.active > a {
            border-top: 3px solid #3a7bd5 !important;
          }
          "))
        )
      )
    ),
      
      # About
      tabItem(tabName = "tab_about",
        fluidRow(
          box(width = 12, includeMarkdown("www/about.md"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # 模块环境（用于清理）
  module_envs <- new.env(parent = emptyenv())
  active_modules <- reactiveVal(list())
  
  # 显示/隐藏加载遮罩
  show_loading <- function() {
    shinyjs::removeClass("module_loading", "hidden")
  }
  
  hide_loading <- function() {
    shinyjs::addClass("module_loading", "hidden")
  }
  
  # 卸载指定模块
  unload_module <- function(module_name) {
    cat("\n[MODULE] Unloading:", module_name, "\n")
    
    # 1. 调用模块清理函数（如果存在）
    cleanup_func <- module_envs[[paste0(module_name, "_cleanup")]]
    if (!is.null(cleanup_func)) {
      tryCatch({
        cleanup_func()
        cat("[MODULE] Cleanup called for:", module_name, "\n")
      }, error = function(e) {
        cat("[MODULE] Cleanup error:", e$message, "\n")
      })
    }
    
    # 2. 移除UI元素
    container_id <- paste0("module_container_", module_name)
    shinyjs::runjs(sprintf(
      "var el = document.querySelector('#%s'); if(el) { while(el.firstChild) { el.removeChild(el.firstChild); } }",
      container_id
    ))
    
    # 3. 清理环境
    rm(list = paste0(module_name, "_cleanup"), envir = module_envs, inherits = FALSE)
    
    # 4. 清理reactiveValues
    active <- active_modules()
    active[[module_name]] <- NULL
    active_modules(active)
    
    cat("[MODULE] Unloaded:", module_name, "\n")
  }
  
  # 加载指定模块
  load_module <- function(module_name, silent = FALSE) {
    cat("\n[MODULE] Loading:", module_name, if (silent) "(background)\n" else "\n")
    
    config <- MODULE_CONFIG[[module_name]]
    if (is.null(config)) {
      cat("[MODULE] Error: Unknown module", module_name, "\n")
      return(NULL)
    }
    
    container_id <- paste0("module_container_", module_name)
    module_id <- paste0("mod_", module_name)
    
    # 1. 先卸载已存在的同名模块
    if (!is.null(active_modules()[[module_name]])) {
      unload_module(module_name)
    }
    
    # 2. 显示加载状态（后台预加载不显示遮罩）
    if (!silent) show_loading()
    
    # 3. 创建模块隔离环境
    module_env <- new.env(parent = globalenv())
    module_env$module_id <- module_id
    
    # 4. 插入UI
    ui_content <- config$ui_func(id = module_id, title = config$title)
    
    insertUI(
      selector = paste0("#", container_id),
      where = "beforeEnd",
      ui = ui_content,
      immediate = TRUE
    )
    
    # 5. 调用Server函数
    server_result <- tryCatch({
      config$server_func(module_id, 
                        analysis_type_OV = config$title, 
                        help_id = paste0("help1-", match(module_name, names(MODULE_CONFIG)), ".html"))
      cat("[MODULE] Server registered:", module_name, "\n")
      TRUE
    }, error = function(e) {
      cat("[MODULE] Error loading module:", e$message, "\n")
      removeUI(selector = paste0("#", container_id), immediate = TRUE)
      FALSE
    })
    
    # 6. 保存模块状态
    if (server_result) {
      active <- active_modules()
      active[[module_name]] <- list(
        env = module_env,
        loaded_at = Sys.time()
      )
      active_modules(active)
      
      # 设置清理函数
      module_envs[[paste0(module_name, "_cleanup")]] <- function() {
        rm(list = ls(module_env), envir = module_env)
      }
    }
    
    # 7. 隐藏加载遮罩
    if (!silent) hide_loading()
    
    return(server_result)
  }
  
  # 监听侧边栏切换
  observeEvent(input$main_sidebar, {
    current_tab <- input$main_sidebar
    
    if (current_tab %in% c("tab_browse", "tab_single", "tab_genelist", "tab_synergy", "tab_gpsa", "tab_download")) {
      module_name <- gsub("tab_", "", current_tab)
      
      if (is.null(active_modules()[[module_name]])) {
        load_module(module_name)
      }
    }
  })
  
  # 返回完整应用
  observeEvent(input$back_to_full, {
    session$reload()
  })
  
  # 后台预加载所有模块（同步加载，不依赖事件循环）
  for (mod in c("browse", "single", "genelist", "synergy", "gpsa", "download")) {
    tryCatch({
      load_module(mod, silent = TRUE)
      cat("[PRELOAD] Loaded:", mod, "\n")
    }, error = function(e) {
      cat("[PRELOAD] Failed:", mod, "-", e$message, "\n")
    })
  }
  
  cat("[APP] PerturbSigDB starting...\n")

  # ================== Help 页面：Overview 功能对比表 ==================
  output$help_overview_comparison <- renderDT({
    md_file <- "Accession映射/平台功能对比.md"
    if (!file.exists(md_file)) {
      return(datatable(data.frame(Message = "Comparison table not found.")))
    }
    
    lines <- readLines(md_file, encoding = "UTF-8")
    
    tbl_lines <- lines[grepl("^\\|", lines)]
    tbl_lines <- tbl_lines[!grepl("^\\|[-|: ]+\\|$", tbl_lines)]
    
    parse_row <- function(x) {
      parts <- strsplit(x, "\\|")[[1]]
      parts <- parts[-1]
      trimws(parts)
    }
    
    parsed <- do.call(rbind, lapply(tbl_lines, parse_row))
    colnames(parsed) <- parsed[1, ]
    parsed <- as.data.frame(parsed[-1, , drop = FALSE], stringsAsFactors = FALSE)
    
    for (col in names(parsed)) {
      parsed[[col]] <- gsub("\\*\\*(.+?)\\*\\*", "<b>\\1</b>", parsed[[col]])
    }
    
    datatable(
      parsed,
      options = list(
        pageLength = nrow(parsed),
        scrollX = TRUE,
        scrollCollapse = FALSE,
        autoWidth = FALSE,
        dom = "t",
        ordering = FALSE,
        columnDefs = list(
          list(className = "dt-center dt-nowrap", targets = "_all"),
          list(width = "180px", targets = 0)
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  var api = this.api();",
          "  var ncols = api.columns().count();",
          "  var remainW = ncols - 1;",
          "  $(api.table().header()).find('th').css('white-space','nowrap');",
          "  $(api.table().body()).find('td').css('white-space','nowrap');",
          "  // Equal width for data columns (columns 1..ncols-1)",
          "  var autoPct = Math.floor(100 / remainW) + '%';",
          "  for (var i = 1; i < ncols; i++) {",
          "    api.column(i).nodes().each(function(el) {",
          "      el.style.width = autoPct;",
          "    });",
          "  }",
          "  api.columns.adjust().draw(false);",
          "}"
        )
      ),
      rownames = FALSE,
      escape = FALSE,
      class = "display compact stripe hover cell-border"
    ) %>%
      formatStyle(
        columns = 1,
        fontWeight = "bold",
        backgroundColor = "#f8f9fa",
        `white-space` = "nowrap"
      ) %>%
      formatStyle(
        columns = 2:(ncol(parsed)),
        `white-space` = "nowrap"
      )
  })

  # ================== Help 页面：Data Curation pie charts & stats table ==================
  curation_stats <- reactive({
    base_dir <- "signature_by_gene_fst"
    GSEA_DIR <- "GSEA"

    # Same file lists as Browse module
    chem_meta_files <- file.path(base_dir, c(
      "CIGSMetadata.fst", "CIGSData2Metadata.fst", "HerbDataMetadata.fst",
      "LINCS_2020_Metadata.fst", "LINCS_L1000_Metadata.fst"
    ))
    gen_meta_files <- file.path(base_dir, c(
      "GuoziDataMetadata.fst", "PerturbAtlasMetadata.fst",
      "LINCS_2020_KD_Metadata.fst", "LINCS_2020_KO_Metadata.fst",
      "LINCS_L1000_KD_Metadata.fst", "LINCS_L1000_KO_Metadata.fst"
    ))

    # Load chemical metadata (exact same logic as Browse load_chem_data)
    chem_list <- list()
    for (f in chem_meta_files) {
      if (!file.exists(f)) next
      dt <- tryCatch(as.data.table(fst::read_fst(f)), error = function(e) NULL)
      if (is.null(dt) || nrow(dt) == 0) next
      # Browse: Remove rows where Time is empty or NA
      if ("Time" %in% names(dt)) {
        dt <- dt[!is.na(Time) & as.character(Time) != ""]
      }
      chem_list[[length(chem_list) + 1]] <- dt
    }
    chem_meta <- if (length(chem_list) > 0) rbindlist(chem_list, fill = TRUE, use.names = TRUE) else data.table()

    # Load genetic metadata (exact same logic as Browse load_gen_data)
    gen_list <- list()
    existing_gen_datasets <- sub("\\.fst$", "", list.files(file.path(GSEA_DIR, "Genetic"), pattern = "\\.fst$"))
    for (f in gen_meta_files) {
      if (!file.exists(f)) next
      dt <- tryCatch(as.data.table(fst::read_fst(f)), error = function(e) NULL)
      if (is.null(dt) || nrow(dt) == 0) next
      dt[, `.src` := basename(f)]
      gen_list[[length(gen_list) + 1]] <- dt
    }
    gen_meta <- if (length(gen_list) > 0) rbindlist(gen_list, fill = TRUE, use.names = TRUE) else data.table()
    if (nrow(gen_meta) > 0) {
      # Browse: remove GuoziData entries without signature data
      if (".src" %in% names(gen_meta) && "Dataset" %in% names(gen_meta)) {
        guozi_idx <- which(gen_meta$.src == "GuoziDataMetadata.fst")
        if (length(guozi_idx) > 0) {
          keep_vec <- gen_meta$Dataset[guozi_idx] %in% existing_gen_datasets
          remove_rows <- guozi_idx[!keep_vec]
          if (length(remove_rows) > 0) gen_meta <- gen_meta[-remove_rows]
        }
      }
      # Save pre-inhibit copy for Accession counting (gives 7,001 unique accessions)
      gen_meta_for_acc <- copy(gen_meta)
      # Browse: remove "inhibit" method entries (for signature total & method breakdown)
      if ("method" %in% names(gen_meta)) {
        gen_meta <- gen_meta[!(method == "inhibit" & !is.na(method))]
      }
    }

    # Combine & count by Type (merge MCE/TCM into Chem for cleaner display)
    # Tag source so NA/empty Type can be distributed correctly
    chem_meta[, `.source` := "Chem"]
    gen_meta[, `.source` := "Genetic"]
    if (exists("gen_meta_for_acc")) gen_meta_for_acc[, `.source` := "Genetic"]
    all_meta <- rbindlist(list(chem_meta, gen_meta), fill = TRUE, use.names = TRUE)
    # Merge MCE and TCM into Chem
    all_meta[Type %in% c("MCE", "TCM"), Type := "Chem"]
    # Distribute NA / empty Type based on source — no "Others"
    all_meta[is.na(Type) | Type == "", Type := ifelse(.source == "Chem", "Chem", "Genetic")]
    # Count total BEFORE filtering (must equal 1,091,148)
    total_signatures <- nrow(chem_meta) + nrow(gen_meta)
    type_counts <- all_meta[, .N, by = Type]
    setnames(type_counts, "Type", "Category")

    # Genetic method breakdown - group into KO/KD/OE/Mut/siRNA
    if (nrow(gen_meta) > 0 && "method" %in% names(gen_meta)) {
      gen_meta[, method_group := method]
      gen_meta[method == "knockout", method_group := "KO"]
      gen_meta[method == "CRISPRi", method_group := "KO"]
      gen_meta[method == "shRNA", method_group := "KD"]
      gen_meta[method == "siRNA", method_group := "siRNA"]
      gen_meta[method == "OE", method_group := "OE"]
      gen_meta[method == "Mutation", method_group := "Mut"]
      method_counts <- gen_meta[, .(Count = .N), by = method_group]
      method_counts <- method_counts[!is.na(method_group) & method_group != ""]
      method_counts <- method_counts[order(-Count)]
    } else {
      method_counts <- data.table(method_group = character(), Count = integer())
    }

    # Accession breakdown - count UNIQUE accessions only
    # Use pre-inhibit gen_meta_for_acc to get 7,001 unique accessions
    all_meta_for_acc <- if (exists("gen_meta_for_acc") && nrow(gen_meta_for_acc) > 0) {
      rbindlist(list(chem_meta, gen_meta_for_acc), fill = TRUE, use.names = TRUE)
    } else {
      all_meta
    }
    if ("Accession" %in% names(all_meta_for_acc)) {
      acc <- all_meta_for_acc[!is.na(Accession) & Accession != ""]
      acc[, acc_type := {
        x <- as.character(Accession)
        # PMID
        pmid_idx <- grepl("^PMID:", x, ignore.case = TRUE)
        acc_type <- character(length(x))
        acc_type[pmid_idx] <- "PubMed"
        # Extract prefix for others (uppercase alphabetic chars before digits)
        non_pmid <- !pmid_idx
        if (any(non_pmid)) {
          prefix <- sub("^([A-Za-z][A-Za-z-]*[A-Za-z]).*", "\\1", x[non_pmid])
          too_long <- nchar(prefix) > 10 | prefix == x[non_pmid]
          prefix[too_long] <- "Other"
          acc_type[non_pmid] <- prefix
        }
        acc_type[acc_type == ""] <- "Other"
        acc_type
      }]
      # Count unique accessions (deduplicate by Accession first, then count by type)
      acc_unique <- unique(acc, by = "Accession")
      total_accessions <- nrow(acc_unique)
      acc_counts <- acc_unique[, .(Count = .N), by = acc_type]
      acc_counts <- acc_counts[order(-Count)]
    } else {
      acc_counts <- data.table(acc_type = character(), Count = integer())
      total_accessions <- 0L
    }

    list(type_counts = type_counts, method_counts = method_counts, acc_counts = acc_counts,
         total_signatures = total_signatures, total_accessions = total_accessions)
  })

  output$curation_pie_overview <- renderEcharts4r({
    stats <- curation_stats()
    dt <- stats$type_counts
    total <- stats$total_signatures
    req(nrow(dt) > 0)

    dt <- dt[order(-N)]
    setnames(dt, c("Category", "N"), c("name", "value"))

    colors <- c("Chem" = "#1E88E5", "Genetic" = "#E53935", "Herb" = "#43A047")

    dt |>
      e_charts(name) |>
      e_pie(value, radius = c("45%", "72%"),
            label = list(
              show = TRUE,
              position = "outside",
              formatter = "{b}\n{d}%",
              fontSize = 12,
              fontWeight = "bold",
              color = "#2c3e50"
            ),
            itemStyle = list(
              borderRadius = 4,
              borderColor = "#FAFAFA",
              borderWidth = 2
            )) |>
      e_color(unname(colors[dt$name])) |>
      e_title(
        text = paste0("Total: ", scales::comma(total), " signatures"),
        left = "center", top = "top",
        textStyle = list(fontSize = 14, fontWeight = "bold", color = "#2c3e50")
      ) |>
      e_tooltip(trigger = "item", formatter = "{b}: {c} ({d}%)") |>
      e_legend(orient = "horizontal", bottom = 0, textStyle = list(fontSize = 11))
  })

  output$curation_pie_genetic <- renderEcharts4r({
    dt <- curation_stats()$method_counts
    req(nrow(dt) > 0)

    total <- dt[, sum(Count)]
    dt[, pct := Count / total]
    dt_main <- dt[pct >= 0.03]
    dt_small <- dt[pct < 0.03]
    if(nrow(dt_small) > 0) {
      dt_main <- rbindlist(list(dt_main,
        data.table(method_group = "OE/Mut/SiRNA/ShRNA", Count = sum(dt_small$Count))),
        fill = TRUE, use.names = TRUE)
    }
    dt_main <- dt_main[order(-Count)]
    setnames(dt_main, c("method_group", "Count"), c("name", "value"))

    colors <- c("KO" = "#E53935", "KD" = "#1E88E5", "OE" = "#FB8C00",
                "Mut" = "#8E24AA", "siRNA" = "#00ACC1", "OE/Mut/SiRNA/ShRNA" = "#FDD835")

    dt_main |>
      e_charts(name) |>
      e_pie(value, radius = c("45%", "72%"),
            label = list(
              show = TRUE,
              position = "outside",
              formatter = "{b}\n{d}%",
              fontSize = 12,
              fontWeight = "bold",
              color = "#2c3e50"
            ),
            itemStyle = list(
              borderRadius = 4,
              borderColor = "#FAFAFA",
              borderWidth = 2
            )) |>
      e_color(unname(colors[dt_main$name])) |>
      e_title(
        text = paste0("Total: ", scales::comma(total), " perturbations"),
        left = "center", top = "top",
        textStyle = list(fontSize = 14, fontWeight = "bold", color = "#2c3e50")
      ) |>
      e_tooltip(trigger = "item", formatter = "{b}: {c} ({d}%)") |>
      e_legend(orient = "horizontal", bottom = 0, textStyle = list(fontSize = 11))
  })

  output$curation_pie_accession <- renderEcharts4r({
    stats <- curation_stats()
    dt <- stats$acc_counts
    total <- stats$total_accessions
    req(nrow(dt) > 0)
    
    dt[, pct := Count / sum(Count)]
    dt_main <- dt[pct >= 0.05]
    dt_small <- dt[pct < 0.05]
    if(nrow(dt_small) > 0) {
      dt_main <- rbindlist(list(dt_main,
                                data.table(acc_type = "DRP/ERP/Others", Count = sum(dt_small$Count))),
                           fill = TRUE, use.names = TRUE)
    }
    dt_main <- dt_main[order(-Count)]
    setnames(dt_main, c("acc_type", "Count"), c("raw_name", "value"))
    dt_main[, raw_name := trimws(raw_name)]
    
    # 映射展示名称：GSE→GEO，SRP→SRA，前端全部显示GEO/SRA/ENCODE
    dt_main[, name := raw_name]
    dt_main[name == "GSE", name := "GEO"]
    dt_main[name == "SRP", name := "SRA"]
    dt_main[name == "ENCSR", name := "ENCODE"]
    
    # 配色映射
    colors <- c(
      "GEO" = "#1E88E5",
      "SRA" = "#E53935",
      "ENCODE" = "#43A047",
      "DRP/ERP/Others" = "#7F8C8D"
    )
    
    dt_main |>
      e_charts(name) |>
      e_pie(value, radius = c("45%", "72%"),
            label = list(
              show = TRUE,
              position = "outside",
              formatter = "{b}\n{d}%",
              fontSize = 12,
              fontWeight = "bold",
              color = "#2c3e50"
            ),
            itemStyle = list(
              borderRadius = 4,
              borderColor = "#FAFAFA",
              borderWidth = 2
            )) |>
      e_color(unname(colors[dt_main$name])) |>
      e_title(
        text = paste0("Total: ", scales::comma(total), " unique accessions"),
        left = "center", top = "top",
        textStyle = list(fontSize = 14, fontWeight = "bold", color = "#2c3e50")
      ) |>
      e_tooltip(trigger = "item", formatter = "{b}: {c} ({d}%)") |>
      e_legend(orient = "horizontal", bottom = 0, textStyle = list(fontSize = 11))
  })


}

shinyApp(ui, server)
