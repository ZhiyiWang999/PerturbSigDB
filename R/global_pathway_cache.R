# ============================================
# 内置通路预计算结果缓存
# 应用启动时加载，Gene Set Query 内置通路秒级返回
# ============================================

.pathway_cache <- new.env(parent = emptyenv())

#' 初始化通路缓存
#' @param results_dir 预计算结果目录
init_pathway_cache <- function(results_dir = "内置通路数据处理/results") {
  if (isTRUE(.pathway_cache$initialized)) {
    cat("[PathwayCache] Already initialized\n")
    return(invisible(TRUE))
  }
  
  t0 <- proc.time()
  cat("[PathwayCache] Loading precomputed pathway results...\n")
  
  qs_files <- list.files(results_dir, pattern = "\\.qs$", full.names = TRUE)
  qs_files <- qs_files[!grepl("pathway_index", qs_files)]
  
  if (length(qs_files) == 0) {
    cat("[PathwayCache] No precomputed results found in", results_dir, "\n")
    .pathway_cache$initialized <- FALSE
    .pathway_cache$ready <- FALSE
    return(invisible(FALSE))
  }
  
  all_res <- data.table()
  for (f in qs_files) {
    res <- tryCatch(qs::qread(f), error = function(e) NULL)
    if (!is.null(res) && nrow(res) > 0 && "Dataset" %in% names(res)) {
      all_res <- rbindlist(list(all_res, res), fill = TRUE)
    }
  }
  
  cat(sprintf("[PathwayCache] Loaded %d results from %d databases\n",
              nrow(all_res), length(unique(all_res$Database))))
  
  # 按通路构建快速查找表 (key = Database|||Pathway)
  if (nrow(all_res) > 0) {
    all_res[, cache_key := paste(Database, Pathway, sep = "|||")]
    # Split into list for O(1) lookup
    cache_keys <- unique(all_res$cache_key)
    .pathway_cache$lookup <- sapply(cache_keys, function(k) {
      all_res[cache_key == k, -"cache_key"]
    }, simplify = FALSE)
    .pathway_cache$all <- all_res
  } else {
    .pathway_cache$lookup <- list()
    .pathway_cache$all <- data.table()
  }
  
  .pathway_cache$n_pathways <- length(.pathway_cache$lookup)
  .pathway_cache$initialized <- TRUE
  .pathway_cache$ready <- .pathway_cache$n_pathways > 0
  
  cat(sprintf("[PathwayCache] Ready: %d pathways cached in %.1fs\n",
              .pathway_cache$n_pathways, (proc.time() - t0)[3]))
  
  invisible(.pathway_cache$ready)
}

#' 查询通路预计算结果
#' @param db 数据库名 (e.g. "KEGG")
#' @param pathway 通路名 (e.g. "KEGG_CELL_CYCLE")
#' @return data.table or NULL
get_pathway_cache <- function(db, pathway) {
  if (!isTRUE(.pathway_cache$ready)) return(NULL)
  key <- paste(db, pathway, sep = "|||")
  .pathway_cache$lookup[[key]]
}

#' 检查通路是否有缓存
has_pathway_cache <- function() {
  isTRUE(.pathway_cache$ready)
}
