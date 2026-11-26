# ============================================================
# Global GPSA Index Cache
# 在 app 启动时预加载 inverted_index，让 Phase1 可在主进程瞬时完成
# 避免 callr 中重复读取 1.1GB rds 文件（~11s）
# ============================================================

.GPSA_CACHE <- new.env(parent = emptyenv())
.GPSA_CACHE$initialized <- FALSE
.GPSA_CACHE$lookup       <- NULL
.GPSA_CACHE$inverted     <- NULL

#' 初始化 GPSA 缓存：加载 signature_lookup 和 inverted_index
init_gpsa_cache <- function(gsea_dir = "GSEA") {
  if (.GPSA_CACHE$initialized) {
    cat("[GPSA_Cache] Already initialized\n")
    return(invisible(TRUE))
  }
  
  cat("[GPSA_Cache] Initializing GPSA index cache...\n")
  t0 <- proc.time()
  
  lookup_file <- file.path(gsea_dir, "signatures", "signature_lookup.rds")
  inverted_file <- file.path(gsea_dir, "signatures", "inverted_index.rds")
  
  if (!file.exists(lookup_file)) {
    cat("[GPSA_Cache] WARNING: lookup file not found:", lookup_file, "\n")
    return(invisible(FALSE))
  }
  if (!file.exists(inverted_file)) {
    cat("[GPSA_Cache] WARNING: inverted index not found:", inverted_file, "\n")
    return(invisible(FALSE))
  }
  
  # Try .qs first (faster), fall back to .rds
  lookup_qs <- sub("\\.rds$", ".qs", lookup_file)
  inverted_qs <- sub("\\.rds$", ".qs", inverted_file)
  
  if (file.exists(lookup_qs)) {
    .GPSA_CACHE$lookup <- qs::qread(lookup_qs)
    cat("[GPSA_Cache] Loaded lookup from .qs\n")
  } else {
    .GPSA_CACHE$lookup <- readRDS(lookup_file)
    cat("[GPSA_Cache] Loaded lookup from .rds\n")
  }
  
  if (file.exists(inverted_qs)) {
    .GPSA_CACHE$inverted <- qs::qread(inverted_qs)
    cat("[GPSA_Cache] Loaded inverted index from .qs\n")
  } else {
    .GPSA_CACHE$inverted <- readRDS(inverted_file)
    cat("[GPSA_Cache] Loaded inverted index from .rds\n")
  }
  
  .GPSA_CACHE$initialized <- TRUE
  elapsed <- round((proc.time() - t0)[3], 1)
  cat(sprintf("[GPSA_Cache] Initialized in %.1fs (lookup: %d rows, inverted: %d genes)\n",
              elapsed, nrow(.GPSA_CACHE$lookup), nrow(.GPSA_CACHE$inverted)))
  
  invisible(TRUE)
}

#' 从缓存获取 lookup 和 inverted index
get_gpsa_cache <- function() {
  if (!.GPSA_CACHE$initialized) {
    init_gpsa_cache()
  }
  list(
    lookup   = .GPSA_CACHE$lookup,
    inverted = .GPSA_CACHE$inverted,
    ready    = .GPSA_CACHE$initialized
  )
}

#' GPSA Phase 1: 使用预加载的索引快速筛选候选签名
gpsa_phase1 <- function(query_genes_for_idx, lookup, inverted, type_filter = "both", filter_candidates = 10000L) {
  # Inverted index lookup
  matched_genes <- inverted[gene %in% query_genes_for_idx]
  
  if (nrow(matched_genes) == 0) {
    return(list(candidate_idx = integer(0), n_total_hits = 0))
  }
  
  pairs_list <- matched_genes[, .(sig_idx = unlist(sig_indices)), by = gene]
  hit_counts <- pairs_list[, .N, by = sig_idx][order(-N)]
  
  n_total_hits <- nrow(hit_counts)
  
  # Type filter
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
      hit_counts <- hit_counts[hit_counts$sig_idx %in% which(valid_mask)]
    }
  }
  
  n_candidates <- min(filter_candidates, nrow(hit_counts))
  candidate_idx <- if (n_candidates > 0) hit_counts$sig_idx[1:n_candidates] else integer(0)
  
  list(candidate_idx = candidate_idx, n_total_hits = n_total_hits)
}
