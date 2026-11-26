
##### UI #######
library(shinyjs)
library(shinydashboard)
library(dashboardthemes)
library(shinydashboardPlus)
library(shinythemes)
library(shinyWidgets)
library(markdown)
library(data.table)
library(shinycssloaders)
library(plotly)
library(fst)
library(qs)
library(fgsea)
library(future.apply)
library(parallel)
library(zip)

# Spinner CSS for loading overlay (non-blocking)
spinner_css <- function() {
  tags$head(tags$style(HTML("
.loading-overlay {
  display: none;
  position: fixed;
  top: 80px;
  right: 20px;
  z-index: 9999;
  background: white;
  border-radius: 12px;
  box-shadow: 0 4px 20px rgba(0,0,0,0.15);
  padding: 15px 20px;
  text-align: center;
  min-width: 140px;
}
.loading-overlay.active {
  display: block;
}
.loading-spinner {
  display: inline-block;
  position: relative;
  width: 40px;
  height: 40px;
  margin: 0 auto;
}
.spinner {
  display: inline-block;
  position: absolute;
  width: 100%;
  height: 100%;
  border: 3px solid rgba(58, 123, 213, 0.2);
  border-top-color: #3a7bd5;
  border-radius: 50%;
  animation: spin 0.8s linear infinite;
}
@keyframes spin {
  to { transform: rotate(360deg); }
}
.loading-text {
  margin-top: 8px;
  font-size: 13px;
  color: #555;
  font-weight: 500;
}
/* Table row selection: gray instead of default blue */
table.dataTable tbody tr.selected > *,
table.dataTable.stripe tbody tr.odd.selected > *,
table.dataTable.display tbody tr.odd.selected > *,
table.dataTable.hover tbody tr.selected:hover > *,
table.dataTable.display tbody tr.selected:hover > * {
  box-shadow: inset 0 0 0 9999px rgba(208, 208, 208, 0.9) !important;
  color: #333 !important;
}
")))
}

# HTML for loading overlay
loading_overlay_html <- function(module_name = "Genelist") {
  HTML(sprintf('
<div id="loading-overlay" class="loading-overlay">
  <div class="loading-spinner"><div class="spinner"></div></div>
  <div class="loading-text">%s running...</div>
</div>
', module_name))
}

if (exists(".ONEGENE_CACHE", envir = .GlobalEnv)) {
  rm(list = ls(envir = .ONEGENE_CACHE, all.names = TRUE), envir = .ONEGENE_CACHE)
} else {
  .ONEGENE_CACHE <- new.env(parent = emptyenv())
}
if (exists(".ONEGENE_DEG_CACHE", envir = .GlobalEnv)) {
  rm(list = ls(envir = .ONEGENE_DEG_CACHE, all.names = TRUE), envir = .ONEGENE_DEG_CACHE)
} else {
  .ONEGENE_DEG_CACHE <- new.env(parent = emptyenv())
}

logo_blue_gradient <- shinyDashboardLogoDIY(
  boldText = "PerturbSigDB",
  mainText = "Platform",
  textSize = 20,
  badgeText = NULL,
  badgeTextColor = NULL,
  badgeTextSize = NULL,
  badgeBackColor = NULL,
  badgeBorderRadius = NULL
)

### creating custom theme object
theme_blue_gradient <- shinyDashboardThemeDIY(
  appFontFamily = "Arial",
  appFontColor = "rgb(0,0,0)",
  primaryFontColor = "rgb(0,0,0)",
  infoFontColor = "rgb(0,0,0)",
  successFontColor = "rgb(0,0,0)",
  warningFontColor = "rgb(0,0,0)",
  dangerFontColor = "rgb(0,0,0)",
  bodyBackColor = "rgb(248,248,248)",
  logoBackColor = "rgb(23,103,124)",
  headerButtonBackColor = "rgb(238,238,238)",
  headerButtonIconColor = "rgb(75,75,75)",
  headerButtonBackColorHover = "rgb(210,210,210)",
  headerButtonIconColorHover = "rgb(0,0,0)",
  headerBackColor = "rgb(238,238,238)",
  headerBoxShadowColor = "#aaaaaa",
  headerBoxShadowSize = "2px 2px 2px",
  sidebarBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgb(20,97,117)",
    colorMiddle = "rgb(56,161,187)",
    colorEnd = "rgb(3,22,56)",
    colorStartPos = 0,
    colorMiddlePos = 50,
    colorEndPos = 100
  ),
  sidebarPadding = 0,
  sidebarMenuBackColor = "transparent",
  sidebarMenuPadding = 0,
  sidebarMenuBorderRadius = 0,
  sidebarShadowRadius = "3px 5px 5px",
  sidebarShadowColor = "#aaaaaa",
  sidebarUserTextColor = "rgb(255,255,255)",
  sidebarSearchBackColor = "rgb(55,72,80)",
  sidebarSearchIconColor = "rgb(153,153,153)",
  sidebarSearchBorderColor = "rgb(55,72,80)",
  sidebarTabTextColor = "rgb(255,255,255)",
  sidebarTabTextSize = 16,
  sidebarTabBorderStyle = "none none solid none",
  sidebarTabBorderColor = "rgb(35,106,135)",
  sidebarTabBorderWidth = 1,
  sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgba(44,222,235,1)",
    colorMiddle = "rgba(44,222,235,1)",
    colorEnd = "rgba(0,255,213,1)",
    colorStartPos = 0,
    colorMiddlePos = 30,
    colorEndPos = 100
  ),
  sidebarTabTextColorSelected = "rgb(0,0,0)",
  sidebarTabRadiusSelected = "0px 20px 20px 0px",
  sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgba(44,222,235,1)",
    colorMiddle = "rgba(44,222,235,1)",
    colorEnd = "rgba(0,255,213,1)",
    colorStartPos = 0,
    colorMiddlePos = 30,
    colorEndPos = 100
  ),
  sidebarTabTextColorHover = "rgb(50,50,50)",
  sidebarTabBorderStyleHover = "none none solid none",
  sidebarTabBorderColorHover = "rgb(75,126,151)",
  sidebarTabBorderWidthHover = 1,
  sidebarTabRadiusHover = "0px 20px 20px 0px",
  boxBackColor = "rgb(255,255,255)",
  boxBorderRadius = 5,
  boxShadowSize = "0px 1px 1px",
  boxShadowColor = "rgba(0,0,0,.1)",
  boxTitleSize = 16,
  boxDefaultColor = "rgb(210,214,220)",
  boxPrimaryColor = "rgba(44,222,235,1)",
  boxInfoColor = "rgb(210,214,220)",
  boxSuccessColor = "rgba(0,255,213,1)",
  boxWarningColor = "rgb(244,156,104)",
  boxDangerColor = "rgb(255,88,55)",
  tabBoxTabColor = "rgb(255,255,255)",
  tabBoxTabTextSize = 14,
  tabBoxTabTextColor = "rgb(0,0,0)",
  tabBoxTabTextColorSelected = "rgb(0,0,0)",
  tabBoxBackColor = "rgb(255,255,255)",
  tabBoxHighlightColor = "rgba(44,222,235,1)",
  tabBoxBorderRadius = 5,
  buttonBackColor = "rgb(245,245,245)",
  buttonTextColor = "rgb(0,0,0)",
  buttonBorderColor = "rgb(200,200,200)",
  buttonBorderRadius = 5,
  buttonBackColorHover = "rgb(235,235,235)",
  buttonTextColorHover = "rgb(100,100,100)",
  buttonBorderColorHover = "rgb(200,200,200)",
  textboxBackColor = "rgb(255,255,255)",
  textboxBorderColor = "rgb(200,200,200)",
  textboxBorderRadius = 5,
  textboxBackColorSelect = "rgb(255,255,255)",
  textboxBorderColorSelect = "rgb(200,200,200)",
  tableBackColor = "rgb(255,255,255)",
  tableBorderColor = "rgb(240,240,240)",
  tableBorderTopSize = 1,
  tableBorderRowSize = 1
)

#### packages for each page ####
library(ggplot2)
library(stringr)
library(DT)
library(dplyr)
library(rlang)
library(tidyr)
library(visNetwork)
library(patchwork)
library(tibble)
library(ggrepel)
library(ggpubr)
library(ggh4x)
library(scales)

# Set ggplot2 global theme
theme_set(
  theme_test(base_family = "Helvetica") +
    theme(
      axis.title = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 15),
      text = element_text(size = 13)
    )
)

# Helper: Replace NA / empty string / "NA" with "—" in character columns for display
replace_empty_with_dash <- function(df, exclude_cols = NULL) {
  cols <- setdiff(names(df), exclude_cols)
  for (col in cols) {
    vals <- df[[col]]
    if (is.character(vals)) {
      vals[is.na(vals) | vals == "" | vals == "NA"] <- "—"
      set(df, j = col, value = vals)
    }
  }
  df
}

# Helper: columnDefs entry to render empty cells (null/empty) as "—" via JS
# Use as fallback for numeric NAs not handled by replace_empty_with_dash
dt_dash_render <- function() {
  list(
    targets = "_all",
    render = DT::JS(
      "function(data, type, row, meta) {
        if (type === 'display' || type === 'filter') {
          if (data === null || data === undefined || data === '' || data === 'NA') {
            return type === 'display' ? '<span style=\"color:#999\">—</span>' : '';
          }
        }
        return data;
      }"
    )
  )
}

# Set DT global options
options(DT.options = list(
  scrollX = TRUE,
  pageLength = 10
))

# ========================
# Shared: Cell line name standardization
# ========================
# Maps variant Cell names to canonical names (case-insensitive exact match)
cell_name_map <- c(
  # --- 22RV1 ---
  "22RV1" = "22RV1", "CWR22RV1" = "22RV1",
  "PROSTATIC CARCINOMA CELL 22RV1" = "22RV1",
  # --- 786-O ---
  "786O" = "786-O", "786M1A RENAL CANCER" = "786-O",
  "CLEAR CELL RENAL CELL CARCINOMA 786O" = "786-O",
  "RENAL CELL CARCINOMA CELL 786O" = "786-O",
  # --- A172 ---
  "A172" = "A172", "GLIOBLASTOMA CELL A172" = "A172",
  # --- A549 ---
  "A549" = "A549", "HUMAN LUNG ADENOCARCINOMA CELL A549" = "A549",
  "LUNG ADENOCARCINOMA CELL A549" = "A549",
  "LUNG EPITHELIAL A549 CELL LINES" = "A549",
  # --- ACHN ---
  "ACHN" = "ACHN", "CLEAR CELL RENAL CARCINOMA CELL ACHN" = "ACHN",
  "CLEAR CELL RENAL CELL CARCINOMA CELL ACHN" = "ACHN",
  # --- ARPE19 ---
  "ARPE19" = "ARPE19", "RETINAL EPITHELIAL CELLS ARPE19" = "ARPE19",
  # --- BEAS2B ---
  "BEAS2B" = "BEAS2B", "BRONCHIAL EPITHELIAL CELL BEAS2B" = "BEAS2B",
  # --- BJ ---
  "BJ" = "BJ", "FIBROBLASTS BJ" = "BJ", "FORESKIN FIBROBLASTS BJ" = "BJ",
  # --- BJ hTERT ---
  "BJ HTERT" = "BJ hTERT", "BJ1HTERT" = "BJ hTERT",
  "FIBROBLASTS BJTERT" = "BJ hTERT", "IMMORTALIZED BJHTERT" = "BJ hTERT",
  # --- BT145 ---
  "BT145" = "BT145", "BT 145" = "BT145",
  # --- BT187 ---
  "BT187" = "BT187", "BT 187" = "BT187",
  # --- BT474 ---
  "BT474" = "BT474", "BREAST CANCER CELL LINE BT474" = "BT474",
  # --- BT549 ---
  "BT549" = "BT549", "BREAST CANCER CELL BT549" = "BT549",
  # --- C42 ---
  "C42" = "C42", "CASTRATION RESISTANT PROSTATE CANCER CELL C42" = "C42",
  "PROSTATE CANCER C42" = "C42", "LNCAP C42" = "C42",
  # --- C42B ---
  "C42B" = "C42B", "C42B (ATCC CRL3315) PROSTATE TUMOR" = "C42B",
  "PROSTATE CANCER CELL C42B" = "C42B", "LNCAP C42B" = "C42B",
  # --- CACO2 ---
  "CACO2" = "CACO2", "COLORECTAL CANCER CELL CACO2" = "CACO2",
  "CACO2/15" = "CACO2",
  # --- CAKI1 ---
  "CAKI1" = "CAKI1", "CLEAR CELL RENAL CELL CARCINOMA CELL CAKI1" = "CAKI1",
  # --- DLD1 ---
  "DLD1" = "DLD1", "COLORECTAL ADENOCARCINOMA DLD1" = "DLD1",
  "COLORECTAL CANCER CELL DLD1" = "DLD1", "COLORECTAL CANCER DLD1" = "DLD1",
  # --- DU145 ---
  "DU145" = "DU145", "DU 145" = "DU145",
  "PROSTATE CANCER CELL LINES DU145" = "DU145",
  "PROSTATIC CARCINOMA CELL DU145" = "DU145",
  "PROSTATE CANCER DU145" = "DU145",
  # --- EndoC-βH1 / EndoC-βH3 (Latin-1 encoding fix: ß/byte-stripped → β) ---
  "ENDOC-H1" = "EndoC-βH1", "ENDOC-H3" = "EndoC-βH3",
  "ENDOC-ßH1" = "EndoC-βH1", "ENDOC-ßH3" = "EndoC-βH3",
  # --- Spaces / formatting fixes from PerturbAtlas / across sources ---
  "CAR T" = "CART",
  "EMBRYONIC STEM CELL H7" = "Embryonic Stem Cell H7",
  "EMBRYONIC STEM CELL  H7" = "Embryonic Stem Cell H7",
  "GFP+ MCHERRY+ EPCAM+ CD49F+ ESDERIVED PRIMORDIAL GERM" = "GFP+ MCHERRY+ EPCAMCD49FESDERIVED PRIMORDIAL GERM",
  "GFP+ MCHERRY+ EPCAM+ CD49FESDERIVED PRIMORDIAL GERM" = "GFP+ MCHERRY+ EPCAMCD49FESDERIVED PRIMORDIAL GERM",
  "GFPMCHERRY+ EPCAM+ CD49F+ ESDERIVED PRIMORDIAL GERM" = "GFP+ MCHERRY+ EPCAMCD49FESDERIVED PRIMORDIAL GERM",
  "GFPMCHERRY+ EPCAM+ CD49FESDERIVED PRIMORDIAL GERM" = "GFP+ MCHERRY+ EPCAMCD49FESDERIVED PRIMORDIAL GERM",
  "HUES 8" = "HUES8",
  "HUT 78" = "HUT78",
  "IPSC DERIVED MOTOR NEURON" = "IPSCDERIVED MOTOR NEURON",
  "KARPAS 299" = "KARPAS299",
  "LS 180" = "LS180",
  "MDA_MB134VI" = "MDAMB134VI",
  "MIA PACA2" = "MIAPACA2",
  "MONOMAC 6" = "MONOMAC6",
  "NONSMALL CELL LUNG CANCER" = "NONSMALLCELL LUNG CANCER",
  "PATIENT DERIVED GLIOMA STEM" = "PATIENTDERIVED GLIOMA STEM",
  "PROSTATE CANCER CELL  PC3M" = "PROSTATE CANCER CELL PC3M",
  "WA09 EMBRYONIC STEMCELL DERIVED EXCITATORY NEURONS" = "WA09 EMBRYONIC STEM CELLDERIVED EXCITATORY NEURONS",
  "HS 578T" = "HS578T", "HS 944T" = "HS944T",
  # --- HCT116 ---
  "HCT116" = "HCT116", "HCT 116" = "HCT116",
  "COLORECTAL ADENOCARCINOMA CELL HCT116" = "HCT116",
  "COLORECTAL CANCER CELL HCT116" = "HCT116",
  "COLORECTAL CARCINOMA CELL HCT116" = "HCT116",
  # --- HEK293T ---
  "HEK293T" = "HEK293T", "HEK293TPCDNA31" = "HEK293T",
  "FOXM1 KNOCKOUT HEK293T" = "HEK293T",
  # --- HEL ---
  "HEL" = "HEL", "HEL 9217" = "HEL",
  # --- HeLa ---
  "HELA" = "HeLa", "FLPIN TREX HELA" = "HeLa", "H1HELA" = "HeLa",
  "HELA KYOTO" = "HeLa", "HELA S3" = "HeLa", "HELA TETOFF" = "HeLa",
  # --- HepG2 ---
  "HEPG2" = "HepG2", "HEP G2" = "HepG2",
  "LIVER CANCER CELL HEPG2" = "HepG2",
  "HEPATOBLASTOMA CELL HEPG2" = "HepG2",
  # --- HL60 ---
  "HL60" = "HL60",
  "NEUTROPHILLIKE DIFFERENTIATED HUMAN PROMYELOCYTIC LEUKEMIA CELL HL60" = "HL60",
  # --- HMEC ---
  "HMEC" = "HMEC", "HTERT HUMAN MAMMARY EPITHELIAL" = "HMEC",
  # --- HT29 ---
  "HT29" = "HT29", "COLORECTAL ADENOCARCINOMA CELL HT29" = "HT29",
  "COLORECTAL CANCER CELL HT29" = "HT29",
  "COLORECTAL ADENOCARCINOMA HT29" = "HT29",
  # --- HUDEP2 ---
  "HUDEP2" = "HUDEP2", "CHD4 67 KB ELEMENT DELETION HUDEP2" = "HUDEP2",
  # --- HUH7 ---
  "HUH7" = "HUH7", "HCC CELL LINE HUH7" = "HUH7",
  "HEPATOMA HUH 7" = "HUH7", "HEPATOMA HUH7" = "HUH7",
  # --- HUVEC ---
  "HUVEC" = "HUVEC", "ENDOTHELIAL CELL HUVEC" = "HUVEC",
  "HUMAN UMBILICAL VEIN ENDOTHELIAL" = "HUVEC",
  # --- K562 ---
  "K562" = "K562", "CHRONIC MYELOGENOUS LEUKEMIA K562" = "K562",
  "K562/G01" = "K562",
  # --- LN18 ---
  "LN18" = "LN18", "GLIOBLASTOMA CELL LN18" = "LN18",
  # --- LNCaP ---
  "LNCAP" = "LNCaP", "ANDROGEN DEPENDENT (AD) LNCAP" = "LNCaP",
  "HORMONESENSITIVE PROSTATE CANCER CELL LINE LNCAP" = "LNCaP",
  "PROSTATE CANCER CELL LINE LNCAP" = "LNCaP",
  "PROSTATE CANCER LNCAP" = "LNCaP",
  # --- LOVO ---
  "LOVO" = "LOVO", "COLON CANCER CELL LOVO" = "LOVO",
  "COLORECTAL CANCER CELL LOVO" = "LOVO",
  "HUMAN COLORECTAL CANCER CELL LINE LOVO" = "LOVO",
  # --- LS174T ---
  "LS174T" = "LS174T", "COLORECTAL ADENOCARCINOMA LS174T" = "LS174T",
  "COLORECTAL ADENOCARCINOMA LS174T \"L8\"" = "LS174T",
  # --- ISLK BAC16 ---
  "ISLKBAC16" = "ISLK BAC16", "ISLK BAC16" = "ISLK BAC16",
  # --- MCF10A ---
  "MCF 10A" = "MCF10A", "MCF10A" = "MCF10A",
  # --- RA FLSS ---
  "RAFLSS" = "RA FLSS", "RA FLSS" = "RA FLSS",
  # --- MCF-7 ---
  "MCF7" = "MCF-7", "MCF-7" = "MCF-7",
  "MCF-7ARO LETR" = "MCF-7", "MCF7ARO LETR" = "MCF-7",
  "MCF-7-TNR" = "MCF-7", "MCF7 TAMR" = "MCF-7", "MCF7 TAM 1" = "MCF-7",
  "BREAST ADENOCARCINOMA CELL MCF7" = "MCF-7",
  "BREAST CANCER CELL MCF7" = "MCF-7", "BREAST CANCER MCF7" = "MCF-7",
  "MCF-7ARO LET-R" = "MCF-7",
  # --- MDA-MB-231 ---
  "MDA-MB-231" = "MDA-MB-231", "MDAMB231" = "MDA-MB-231",
  "MDA231" = "MDA-MB-231",
  "BREAST ADENOCARCINOMA MDAMB231" = "MDA-MB-231",
  "BREAST CANCER CELL LINE MDAMB231" = "MDA-MB-231",
  "BREAST CANCER CELL MDAMB231" = "MDA-MB-231",
  # --- MDA-MB-468 ---
  "MDA-MB-468" = "MDA-MB-468", "MDAMB468" = "MDA-MB-468",
  "BASALLIKE BREAST CANCER CELLS MDAMB468" = "MDA-MB-468",
  "BREAST CANCER EPITHELIAL CELLS MDAMB468" = "MDA-MB-468",
  # --- MOLM13 ---
  "MOLM13" = "MOLM13", "ICAS9MOLM13" = "MOLM13",
  "MOLM13CAS9DBSD" = "MOLM13",
  # --- MRC5 ---
  "MRC5" = "MRC5", "DIPLOID FIBROBLAST CELLS MRC5" = "MRC5",
  # --- OCIAML2 ---
  "OCIAML2" = "OCIAML2",
  "ACUTE MYELOID LEUKEMIA CELL OCIAML2" = "OCIAML2",
  "ENGINEERED LEUKEMIC STEM CELL OCIAML2" = "OCIAML2",
  # --- PC3 ---
  "PC3" = "PC3", "PC3 (ATCC CRL1435) PROSTATE TUMOR" = "PC3",
  "PROSTATE CANCER CELL LINE PC3" = "PC3",
  "PROSTATE CANCER CELL PC3" = "PC3",
  # --- RAJI ---
  "RAJI" = "RAJI", "BURKITT'S LYMPHOMA CELL RAJI" = "RAJI",
  # --- RKO ---
  "RKO" = "RKO", "COLON CANCER CELL RKO" = "RKO",
  "COLON CARCINOMA CELL RKO" = "RKO",
  "HUMAN COLON CARCINOMA CELL LINE RKO" = "RKO",
  # --- RPE1 ---
  "RPE1" = "RPE1", "HTERT RPE1" = "RPE1",
  "HTERTIMMORTALIZED RETINAL PIGMENT EPITHELIAL CELL RPE1" = "RPE1",
  "HTERTRPE1" = "RPE1",
  # --- SKBR3 ---
  "SKBR3" = "SKBR3", "BREAST CANCER EPITHELIAL CELLS SKBR3" = "SKBR3",
  "BREAT CANCER CELL SKBR3" = "SKBR3",
  # --- SKHEP1 ---
  "SKHEP1" = "SKHEP1",
  "HEPATIC ADENOCARCINOMA CELL LINE SKHEP1" = "SKHEP1",
  "HEPATOCELLULAR CARCINOMA CELL SKHEP1" = "SKHEP1",
  # --- SKOV3 ---
  "SKOV3" = "SKOV3", "OVARIAN ADENOCARCINOMA CELL SKOV3" = "SKOV3",
  "OVARIAN CANCER CELL LINE SKOV3" = "SKOV3",
  "SEROUS OVARIAN CANCER CELL SKOV3" = "SKOV3",
  # --- SW1353 ---
  "SW1353" = "SW1353", "SW 1353" = "SW1353",
  "BONE CHONDROSARCOMA CELL SW1353" = "SW1353",
  "CHONDROSARCOMA CELL SW1353" = "SW1353",
  # --- SW480 ---
  "SW480" = "SW480", "SW 480" = "SW480",
  "COLORECTAL ADENOCARCINOMA CELL SW480" = "SW480",
  "COLORECTAL CANCER CELL SW480" = "SW480",
  "COLORECTAL ADENOCARCINOMA CELL SW480TETONAPC" = "SW480",
  # --- SW620 ---
  "SW620" = "SW620", "SW 620" = "SW620",
  "COLORECTAL ADENOCARCINOMA CELL SW620" = "SW620",
  # --- T47D ---
  "T47D" = "T47D", "BREAST ADENOCARCINOMA CELLS T47D" = "T47D",
  "BREAST CANCER CELL T47D" = "T47D",
  "METASTATIC ER+ BREAST CANCER T47D" = "T47D",
  # --- T98G ---
  "T98G" = "T98G", "GLIOBLASTOMA CELL T98" = "T98G",
  "GLIOBLASTOMA CELL T98G" = "T98G",
  # --- THP1 (leukemia) ---
  "THP1" = "THP1",
  "ACUTE MYELOID LEUKEMIA CELL THP1" = "THP1",
  "HUMAN MONOCYTIC LEUKAEMIA_ THP1" = "THP1",
  # --- THP1 Monocyte / Macrophage (differentiated) ---
  "THP1 MONOCYTE" = "THP1 Monocyte",
  "THP1 MONOCYTIC" = "THP1 Monocyte",
  "THP1DERIVED MACROPHAGES" = "THP1 Monocyte",
  # --- U251 ---
  "U251" = "U251", "U251 MG" = "U251", "U251MG" = "U251",
  "GLIOBLASTOMA CELL U251" = "U251",
  "GLIOBLASTOMA CELL U251MG" = "U251",
  "HUMANDERIVED U251" = "U251",
  # --- U2OS ---
  "U2OS" = "U2OS", "U2 OS" = "U2OS", "U2OS HUMAN" = "U2OS",
  "BONE OSTEOSARCOMA EPITHELIAL CELLS U2OS" = "U2OS",
  "HUMAN OSTEOSARCOMA U2OS" = "U2OS",
  # --- U87MG ---
  "U87MG" = "U87MG", "U87 MG" = "U87MG",
  "GLIOBASTOMA CELL U87 MG" = "U87MG",
  "GLIOBASTOMA CELL U87MG" = "U87MG",
  "XENOGRAFT GLIOBASTOMA CELL U87" = "U87MG",
  # --- U937 ---
  "U937" = "U937", "DIFFUSE LARGE BCELL LYMPHOMA U937" = "U937",
  "MONOCYTIC CELLS U937" = "U937",
  # --- VCAP ---
  "VCAP" = "VCAP", "PROSTATE CANCER CELL VCAP" = "VCAP"
)

# Standardize Cell column using the lookup map (case-insensitive)
standardize_cell <- function(df) {
  if (!"Cell" %in% names(df)) return(df)
  # Fast path: skip entirely if metadata fst files are pre-standardized
  if (is_cell_prestandardized()) return(df)
  # Handle invalid UTF-8 strings to avoid trimws/sub errors
  cell_raw <- iconv(as.character(df$Cell), from = "", to = "UTF-8", sub = "")
  # Fallback: try Latin-1 encoding for strings that are still invalid UTF-8
  needs_fix <- which(is.na(iconv(cell_raw, from = "UTF-8", to = "UTF-8")))
  if (length(needs_fix) > 0) {
    cell_raw[needs_fix] <- iconv(as.character(df$Cell)[needs_fix],
                                 from = "latin1", to = "UTF-8", sub = "")
  }
  # Apply cleaned names to ALL rows (fixes UTF-8 encoding for all cells)
  df[, Cell := cell_raw]

  # --- Strategy 0: Pre-clean common dirty patterns before map lookup ---
  # Quick combined scan: skip all 5 individual regex scans if no parens/brackets/braces exist
  # This is the dominant cost when Cell is already pre-standardized (99.9% of rows)
  cell_pre <- trimws(as.character(df$Cell))
  has_dirty <- grepl("[][(){}]", cell_pre, fixed = FALSE)
  
  if (any(has_dirty)) {
    pre_clean_count <- 0L
    dirty_idx <- which(has_dirty)

    # 0a. Strip parenthetical annotations at start: "(BASALLIKE)HCC38" -> "HCC38"
    pat <- grepl("^\\([^)]+\\)\\s*", cell_pre[dirty_idx])
    if (any(pat)) {
      idx <- dirty_idx[pat]
      cell_pre[idx] <- sub("^\\([^)]+\\)\\s*", "", cell_pre[idx])
      pre_clean_count <- pre_clean_count + sum(pat)
    }

    # 0b. Strip trailing bracket annotations: "PA1 [PA1]" -> "PA1"
    pat <- grepl("\\s*\\[[^]]+\\]\\s*$", cell_pre[dirty_idx])
    if (any(pat)) {
      idx <- dirty_idx[pat]
      cell_pre[idx] <- sub("\\s*\\[[^]]+\\]\\s*$", "", cell_pre[idx])
      pre_clean_count <- pre_clean_count + sum(pat)
    }

    # 0c. Known cell lines with parenthetical suffix: "HEK293T(SUBPOLYSOMAL ...)" -> "HEK293T"
    pat <- grepl("^([A-Za-z0-9/-]+)\\(.+\\)$", cell_pre[dirty_idx])
    if (any(pat)) {
      idx <- dirty_idx[pat]
      base_name <- sub("^([A-Za-z0-9/-]+)\\(.+\\)$", "\\1", cell_pre[idx])
      base_upper <- toupper(base_name)
      base_mapped <- cell_name_map[base_upper]
      for (j in seq_along(idx)) {
        if (!is.na(base_mapped[j])) {
          cell_pre[idx[j]] <- unname(base_mapped[j])
        } else {
          cell_pre[idx[j]] <- base_name[j]
        }
        pre_clean_count <- pre_clean_count + 1L
      }
    }

    # 0d. Fix trailing double-closing parentheses: "1))" -> "1)"
    pat <- grepl("\\)\\)$", cell_pre[dirty_idx]) & !grepl("^\\(.*\\)\\)$", cell_pre[dirty_idx])
    if (any(pat)) {
      idx <- dirty_idx[pat]
      cell_pre[idx] <- sub("\\)\\)$", ")", cell_pre[idx])
      pre_clean_count <- pre_clean_count + sum(pat)
    }

    # 0e. Fix isolated garbage: lone parenthesis/bracket fragments
    pat <- grepl("^[][(){}]+$", cell_pre[dirty_idx])
    if (any(pat)) {
      idx <- dirty_idx[pat]
      cell_pre[idx] <- NA_character_
      pre_clean_count <- pre_clean_count + sum(pat)
    }

    if (pre_clean_count > 0L) {
      message(sprintf("[standardize_cell] Pre-cleaned %d dirty cell name patterns", pre_clean_count))
      df[, Cell := cell_pre]
    }
  }

  # Then look up map and override canonical names
  cell_upper <- toupper(trimws(df$Cell))
  mapped <- cell_name_map[cell_upper]
  idx <- which(!is.na(mapped))
  if (length(idx) > 0) {
    message(sprintf("[standardize_cell] Normalized %d cell line names (explicit map)", length(idx)))
    df[idx, Cell := unname(mapped[idx])]
  }

  # --- Auto-extract cell line names from verbose PerturbAtlas descriptions ---
  # Example: "BLADDER CANCER CELL 5637" -> "5637" if 5637 exists elsewhere in the data
  # Use ALL current Cell values (post-explicit-map) as the reference set
  all_cells_now <- sort(unique(df$Cell))
  all_cells_now <- all_cells_now[!is.na(all_cells_now) & nzchar(all_cells_now)]
  # Filter out verbose cells (multi-word, likely descriptions) from the reference set
  # to avoid matching description against description. Keep short or single-word cells.
  # Vectorized: avoid sapply + strsplit overhead for each unique cell
  n_words <- lengths(strsplit(all_cells_now, "\\s+", fixed = FALSE))
  is_simple <- n_words == 1L | grepl("[0-9]", all_cells_now, fixed = FALSE)
  known_cells <- all_cells_now[is_simple]
  known_upper <- toupper(known_cells)

  # Keywords that typically precede or surround the actual cell line name
  cell_kw <- c("CELL", "CELLS", "CANCER", "CARCINOMA", "ADENOCARCINOMA",
               "LEUKEMIA", "LYMPHOMA", "SARCOMA", "MELANOMA", "TUMOR",
               "LINE", "LINES", "NEUROBLASTOMA", "MESOTHELIOMA",
               "GLIOBLASTOMA", "ASTROCYTOMA", "CARCINOMAS", "XENOGRAFT",
               "CHORIOCARCINOMA", "LIPOSARCOMA", "TUMORS", "ORGANOIDS",
               "TUMORSPHERE", "METASTASIS")

  # Common words that should NOT be extracted as cell names
  non_cell_words <- c(
    "HUMAN", "MOUSE", "RAT", "NORMAL", "PRIMARY", "TISSUE", "TISSUES",
    "BREAST", "LIVER", "LUNG", "SKIN", "BONE", "BRAIN", "KIDNEY", "COLON",
    "OVARY", "STOMACH", "CERVIX", "PROSTATE", "RECTUM", "BLADDER", "THYROID",
    "ESOPHAGUS", "ENDOMETRIUM", "FIBROBLASTS", "FIBROBLAST", "LYMPHOBLAST",
    "EPITHELIAL", "ENDOTHELIAL", "MESENCHYMAL", "PANCREAS", "EMBRYONIC",
    "EMBRYONAL", "HEMATOPOIETIC", "UTERINE", "PROXIMAL", "PULMONARY",
    "CARDIAC", "GASTRIC", "HEPATIC", "RENAL", "OVARIAN", "CERVICAL",
    "PERIPHERAL", "CORD", "BLOOD", "MARROW", "STROMAL", "NEURONAL",
    "GLIAL", "MYELOID", "LYMPHOID", "NEURAL", "PLURIPOTENT",
    "INTESTINE", "TONSIL", "LARYNGEAL", "CORPUS", "CAVERNOSUM",
    "DERIVED", "PATIENT", "PATIENTDERIVED", "XENOGRAFT", "DERIVATIVE",
    "CULTURED", "IMMORTALIZED", "HTERTIMMORTALIZED", "HTERTIMMORTALISED",
    "METASTATIC", "METASTASIS", "CUTANEOUS", "SUBCUTANEOUS",
    "RESISTANT", "SENSITIVE", "TRANSFORMED", "ENGINEERED",
    # Short common words
    "IN", "OF", "TO", "FROM", "WITH", "FOR", "AND", "THE", "NOT", "NO",
    "NEGATIVE", "POSITIVE", "TRIPLE", "DOUBLE", "SINGLE",
    "BASAL", "LUMINAL", "BASALLIKE", "TRIPLENEGATIVE",
    "HIGH", "LOW", "MEDIUM", "TYPE", "SUBTYPE",
    "ADULT", "FETAL", "NEONATAL", "MALE", "FEMALE",
    "LEFT", "RIGHT", "LARGE", "SMALL",
    "WILD", "MUTANT", "KNOCKOUT", "OVEREXPRESSING", "EXPRESSING",
    "BENIGN", "MALIGNANT", "DIFFERENTIATED", "UNDIFFERENTIATED",
    "INVASIVE", "NONINVASIVE", "DUCTAL", "LOBULAR",
    "SQUAMOUS", "ADENOID", "PAPILLARY", "SEROUS", "CLEAR",
    "HORMONESENSITIVE", "CASTRATION", "ENZRESISTANT",
    "PACLITAXELRESISTANT", "GEFITINIBRESISTANT", "CARBOPLATIN",
    "RESISTANCE", "MULTIDRUG", "BONETROPIC",
    "NONSMALLCELL", "NONTUMOR", "ADJACENT", "ASSOCIATED",
    "INTRACRANIAL", "INTRAPERITONEAL", "INTRAHEPATIC",
    "ENDOPHYTICTYPE", "ORGANOIDS", "TUMORSPHERE",
    "ORGANOID", "SPHEROID", "XENOGRAFTS",
    "DISEASE", "SYNDROME", "MODEL", "PATIENT", "PATIENTS",
    "SAMPLE", "SAMPLES", "SPECIMEN", "BIOPSY",
    "CULTURE", "CULTURES", "MONOLAYER", "SUSPENSION",
    "TRANSDUCED", "TRANSFECTED", "INFECTED", "TREATED",
    "UNTREATED", "CONTROL", "STIMULATED", "UNSTIMULATED",
    "ACTIVATED", "RESTING", "NAIVE", "MEMORY",
    "EFFECTOR", "REGULATORY", "CYTOTOXIC", "HELPER"
  )
  non_cell_upper <- toupper(non_cell_words)
  # Combined exclusion: cell keywords + common non-cell words
  cell_exclude_upper <- unique(c(toupper(cell_kw), non_cell_upper))

  still_unmapped <- which(is.na(mapped))
  if (length(still_unmapped) > 0) {
    cells_to_check <- df$Cell[still_unmapped]
    extra_norm <- 0L
    for (i in seq_along(still_unmapped)) {
      cell_val <- trimws(as.character(cells_to_check[i]))
      tokens <- strsplit(cell_val, "\\s+")[[1]]
      n <- length(tokens)
      if (n <= 1) next
      tokens_upper <- toupper(tokens)
      found_canonical <- NA_character_

      # Strategy 1: find token after CELL/CANCER/... keyword that matches a known simple cell
      for (kw_idx in which(tokens_upper %in% cell_kw)) {
        if (kw_idx >= n) next
        for (take in seq_len(min(4L, n - kw_idx))) {
          cand <- paste(tokens[(kw_idx + 1L):(kw_idx + take)], collapse = " ")
          cand_upper <- toupper(cand)
          m <- which(known_upper == cand_upper)[1L]
          if (!is.na(m)) {
            found_canonical <- known_cells[m]
            break
          }
        }
        if (!is.na(found_canonical)) break
      }

      # Strategy 2: try suffix tokens (last 1..3 tokens) - match known_cells
      if (is.na(found_canonical)) {
        for (take in seq_len(min(3L, n))) {
          cand <- paste(tokens[(n - take + 1L):n], collapse = " ")
          cand_upper <- toupper(cand)
          m <- which(known_upper == cand_upper)[1L]
          if (!is.na(m)) {
            found_canonical <- known_cells[m]
            break
          }
        }
      }

      # Strategy 2b: suffix tokens look like cell names (has digit or short alphanum)
      # Prefer tokens with digits over pure-alpha tokens
      if (is.na(found_canonical)) {
        # Pass 1: digit-containing candidates (no parenthesis stripping)
        for (take in seq_len(min(3L, n))) {
          cand <- paste(tokens[(n - take + 1L):n], collapse = " ")
          cand <- trimws(cand)
          if (!nzchar(cand)) next
          if (grepl("[0-9]", cand)) {
            cand_map <- cell_name_map[toupper(cand)]
            found_canonical <- if (!is.na(cand_map)) unname(cand_map) else cand
            break
          }
        }
      }
      # Pass 2: non-digit, short alphanumeric (only if no digit candidate found)
      if (is.na(found_canonical)) {
        for (take in seq_len(min(3L, n))) {
          cand <- paste(tokens[(n - take + 1L):n], collapse = " ")
          cand <- trimws(cand)
          if (!nzchar(cand)) next
          if (nchar(cand) >= 3L && nchar(cand) <= 12L && grepl("^[A-Z][A-Z0-9/-]+$", cand, ignore.case = TRUE) &&
              !(toupper(cand) %in% cell_exclude_upper)) {
            cand_map <- cell_name_map[toupper(cand)]
            found_canonical <- if (!is.na(cand_map)) unname(cand_map) else cand
            break
          }
        }
      }

      if (!is.na(found_canonical)) {
        df[still_unmapped[i], Cell := found_canonical]
        extra_norm <- extra_norm + 1L
      }
    }
    if (extra_norm > 0L) {
      message(sprintf("[standardize_cell] Auto-extracted %d cell line names from verbose descriptions", extra_norm))
    }

    # --- Strategy 3: Aggressive prefix stripping for remaining unmapped verbose cells ---
    # Recompute unmapped cells (some fixed by Strategies 1/2)
    current_cell_upper <- toupper(trimws(df$Cell))
    current_mapped <- cell_name_map[current_cell_upper]
    still_unmapped3 <- which(is.na(current_mapped))
    if (length(still_unmapped3) > 0) {
      extra_norm3 <- 0L
      for (i in seq_along(still_unmapped3)) {
        cell_val <- trimws(as.character(df$Cell[still_unmapped3[i]]))
        tokens <- strsplit(cell_val, "\\s+")[[1]]
        n <- length(tokens)
        if (n <= 1) next
        tokens_upper <- toupper(tokens)

        # Find the LAST keyword position
        kw_pos <- which(tokens_upper %in% cell_kw)
        if (length(kw_pos) == 0) next
        last_kw <- max(kw_pos)
        if (last_kw >= n) next  # nothing after keyword

        # Tokens after the last keyword (no parenthesis stripping to preserve cell names like BE(2)C)
        after_tokens <- tokens[(last_kw + 1):n]
        after_clean <- after_tokens[nzchar(after_tokens)]
        if (length(after_clean) == 0) next

        # Find the first cell-like token (prefer digits over pure-alpha)
        has_digit <- grepl("[0-9]", after_clean)
        no_digit_like <- (!has_digit) & (nchar(after_clean) >= 3L & nchar(after_clean) <= 12L &
          grepl("^[A-Z][A-Z0-9/-]+$", after_clean, ignore.case = TRUE) &
          !(toupper(after_clean) %in% cell_exclude_upper))
        is_cell_like <- has_digit | no_digit_like
        if (!any(is_cell_like)) next

        # Prefer the first digit-containing token; fallback to first non-digit
        if (any(has_digit)) {
          extracted <- after_clean[has_digit][1]
        } else {
          extracted <- after_clean[no_digit_like][1]
        }

        # Apply explicit map to the extracted name
        extracted_map <- cell_name_map[toupper(extracted)]
        if (!is.na(extracted_map)) extracted <- unname(extracted_map)

        df[still_unmapped3[i], Cell := extracted]
        extra_norm3 <- extra_norm3 + 1L
      }
      if (extra_norm3 > 0L) {
        message(sprintf("[standardize_cell] Prefix-strip extracted %d cell line names", extra_norm3))
      }

      # --- Strategy 4: Keyword at end, cell name before (e.g. "A549 TUMOR", "JEG3 CHORIOCARCINOMA") ---
      current_cell_upper4 <- toupper(trimws(df$Cell))
      current_mapped4 <- cell_name_map[current_cell_upper4]
      still_unmapped4 <- which(is.na(current_mapped4))
      if (length(still_unmapped4) > 0) {
        extra_norm4 <- 0L
        for (i in seq_along(still_unmapped4)) {
          cell_val <- trimws(as.character(df$Cell[still_unmapped4[i]]))
          tokens <- strsplit(cell_val, "\\s+")[[1]]
          n <- length(tokens)
          if (n <= 1) next
          tokens_upper <- toupper(tokens)

          # Find the LAST keyword position
          kw_pos <- which(tokens_upper %in% cell_kw)
          if (length(kw_pos) == 0) next
          last_kw <- max(kw_pos)

          # If keyword is at the end (or near end with only non-word tokens after)
          after_count <- n - last_kw
          if (after_count > 2) next  # too many tokens after keyword, likely different pattern

          # Tokens BEFORE the first keyword
          first_kw <- min(kw_pos)
          if (first_kw <= 1) next  # nothing before keyword

          before_tokens <- tokens[1:(first_kw - 1)]
          before_clean <- before_tokens[nzchar(before_tokens)]
          if (length(before_clean) == 0) next

          # Find cell-like token from the end (prefer digit-containing)
          # Last token before keyword closest to keyword is most likely cell name
          has_digit <- grepl("[0-9]", before_clean)
          no_digit_like <- (!has_digit) & (nchar(before_clean) >= 3L & nchar(before_clean) <= 12L &
            grepl("^[A-Z][A-Z0-9/-]+$", before_clean, ignore.case = TRUE) &
            !(toupper(before_clean) %in% cell_exclude_upper))
          is_cell_like <- has_digit | no_digit_like
          if (!any(is_cell_like)) next

          # Prefer the last digit-containing token; fallback to last non-digit
          if (any(has_digit)) {
            extracted <- before_clean[has_digit][sum(has_digit)]
          } else {
            extracted <- before_clean[no_digit_like][sum(no_digit_like)]
          }

          # Apply explicit map to the extracted name
          extracted_map <- cell_name_map[toupper(extracted)]
          if (!is.na(extracted_map)) extracted <- unname(extracted_map)

          df[still_unmapped4[i], Cell := extracted]
          extra_norm4 <- extra_norm4 + 1L
        }
        if (extra_norm4 > 0L) {
          message(sprintf("[standardize_cell] Reverse-extracted %d cell line names (keyword at end)", extra_norm4))
        }
      }
    }
  }
  df
}

# ================== 检查 metadata 是否已预标准化 ==================
# 预标准化标记文件路径，由 R/precompute_cell_standardization.R 创建
# 使用绝对路径防止 Shiny 启动时 CWD 不一致导致找不到标记文件
.CELL_STANDARDIZED_MARKER <- local({
  # 尝试从 source 调用栈获取 global.R 自身路径
  sf <- sys.frames()
  ofile <- NULL
  for (fr in rev(sf)) {
    ofile <- tryCatch(get("ofile", envir = fr, inherits = FALSE), error = function(e) NULL)
    if (!is.null(ofile)) break
  }
  if (!is.null(ofile)) {
    global_dir <- normalizePath(dirname(ofile))
    project_root <- normalizePath(file.path(global_dir, ".."))
  } else {
    project_root <- normalizePath(getwd())
  }
  file.path(project_root, "signature_by_gene_fst", ".cell_standardized")
})

is_cell_prestandardized <- function() {
  file.exists(.CELL_STANDARDIZED_MARKER)
}

# 条件性标准化：如果 metadata 已预标准化则跳过
standardize_cell_if_needed <- function(df) {
  if (is_cell_prestandardized()) {
    return(df)
  }
  standardize_cell(df)
}

# ================== Tissue 标准化 ==================
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

# Standardize Tissue column
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
  cell_raw <- iconv(as.character(df$Cell), from = "", to = "UTF-8", sub = "")
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

# Data loading is handled on-demand by individual modules, no large file preloading

# Helper function - gene processing
refine_genelist <- function(source1, source2, input_genes, show_up = 30, cal = F){
  input_genes <- input_genes %>%
    strsplit("\n") %>%
    unlist() %>%
    gsub(" ", "", .) %>%
    .[1:show_up] %>%
    .[!is.na(.)]
  return(input_genes)
}

# ========================
# Shared: Strip HTML tags from character columns before CSV export
# ========================
# Used before CSV export to avoid raw HTML (links, buttons, etc.) in downloaded files
strip_html <- function(dt) {
  for (col in names(dt)) {
    if (is.character(dt[[col]])) {
      vals <- dt[[col]]
      # Only process columns that actually contain HTML tags
      if (any(grepl("<[a-zA-Z/]", vals, fixed = FALSE, useBytes = TRUE), na.rm = TRUE)) {
        set(dt, j = col, value = gsub("<[^>]+>", "", vals, useBytes = TRUE))
      }
    }
  }
  dt
}

# ========================
# Shared: 1:1 table CSV download helper
# ========================
# Usage: register_csv_download(output, "download_xxx", reactive_data, "prefix")
# Automatically strips HTML columns (Download), internal dot-columns, metadata keys, and HTML tags.
register_csv_download <- function(output, id, data_reactive, filename_prefix,
                                   extra_drop = character(0)) {
  output[[id]] <- downloadHandler(
    filename = function() paste0(filename_prefix, "_", Sys.Date(), ".csv"),
    content = function(file) {
      dt <- copy(data_reactive())
      # Drop HTML button / internal / metadata columns
      drop <- intersect(c("Download", ".gem_id_raw", ".Dataset",
                          ".source_data_file", ".source_meta_file",
                          "perturb_id", "source_id", "type", "dataset"), names(dt))
      if (length(drop) > 0) dt[, (drop) := NULL]
      # Extra caller-specified drops
      extra <- intersect(extra_drop, names(dt))
      if (length(extra) > 0) dt[, (extra) := NULL]
      # Any remaining dot-prefixed columns
      dots <- grep("^\\.", names(dt), value = TRUE)
      if (length(dots) > 0) dt[, (dots) := NULL]
      # Strip HTML tags from remaining character columns
      strip_html(dt)
      data.table::fwrite(dt, file)
    }
  )
}
