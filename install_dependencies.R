# Install required packages for PerturbSigDB demo in a supported R installation.
cran <- c("shiny", "shinydashboard", "shinyjs", "shinyalert", "DT", "ggplot2",
          "scales", "data.table", "fst", "qs", "echarts4r", "future", "promises",
          "callr", "dashboardthemes", "shinydashboardPlus", "shinythemes",
          "shinyWidgets", "markdown", "shinycssloaders", "plotly", "future.apply",
          "zip", "stringr", "dplyr", "rlang", "tidyr", "visNetwork",
          "patchwork", "tibble", "ggrepel", "ggpubr", "ggh4x", "base64enc", "later")
missing <- cran[!vapply(cran, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager", repos = "https://cloud.r-project.org")
if (!requireNamespace("fgsea", quietly = TRUE))
  BiocManager::install("fgsea", ask = FALSE, update = FALSE)
cat("Dependency installation steps completed. Run: Rscript smoke_test.R\\n")
