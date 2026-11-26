
# =========================================================
# 全局共享缓存：整个 Shiny 进程只加载一次
# GQueryServer 和 GenelistServer 都调用这个缓存
# =========================================================

GLOBAL_SIGNATURE_CACHE <- new.env(parent = emptyenv())
GLOBAL_DEG_CACHE <- new.env(parent = emptyenv())

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

normalize_gene <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x
}

parse_gene_list <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(character(0))
  genes <- unlist(strsplit(x, "[,;\\s\\n\\r\\t]+", perl = TRUE), use.names = FALSE)
  genes <- normalize_gene(genes)
  genes <- genes[nzchar(genes)]
  unique(genes)
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
  cand <- c(
    "gene_name", "Gene", "symbol", "SYMBOL",
    "GeneSymbol", "gene_symbol", "gene"
  )
  hit <- cand[cand %in% names(dt)]
  if (length(hit) > 0) return(hit[1])
  names(dt)[1]
}

# =========================================================
# 建立单个 signature index
# 这个 index 同时支持：
# 1. OneGene 查询 effect
# 2. GeneList 查询多个 gene 是否共同命中同一个 dataset
# =========================================================

build_signature_index_shared <- function(
    sig_dt,
    meta_dt,
    dataset_col = "Dataset",
    type_value = NA_character_,
    source_id = NA_character_
) {
  gene_col <- detect_gene_col(sig_dt)
  
  genes <- normalize_gene(sig_dt[[gene_col]])
  dataset_names <- setdiff(names(sig_dt), gene_col)
  
  # metadata 和 signature 的 dataset 对齐
  meta_idx <- match(dataset_names, meta_dt[[dataset_col]])
  keep <- !is.na(meta_idx)
  
  dataset_names <- dataset_names[keep]
  meta_small <- copy(meta_dt[meta_idx[keep]])
  
  if (length(dataset_names) == 0) {
    stop("No overlapping dataset columns found between signature and metadata.")
  }
  
  if (dataset_col %in% names(meta_small)) {
    meta_small[, (dataset_col) := NULL]
  }
  
  mat <- as.matrix(sig_dt[, ..dataset_names])
  storage.mode(mat) <- "double"
  
  # gene -> row index
  gene_to_row <- match(unique(genes), genes)
  names(gene_to_row) <- unique(genes)
  gene_to_row <- gene_to_row[nzchar(names(gene_to_row))]
  
  # gene -> datasets with non-NA effect
  # 用于 Genelist 查询共同命中的 datasets
  non_na_mat <- !is.na(mat)
  arr <- which(non_na_mat, arr.ind = TRUE)
  
  if (nrow(arr) == 0) {
    gene_to_datasets <- list()
  } else {
    gene_to_datasets <- split(
      dataset_names[arr[, "col"]],
      genes[arr[, "row"]]
    )
    gene_to_datasets <- lapply(gene_to_datasets, unique)
  }
  
  meta_lookup <- copy(meta_small)
  meta_lookup[, dataset := dataset_names]
  meta_lookup <- unique(meta_lookup, by = "dataset")
  setkey(meta_lookup, dataset)
  
  list(
    type = type_value,
    source_id = source_id,
    gene_col = gene_col,
    genes = genes,
    gene_to_row = gene_to_row,
    gene_to_datasets = gene_to_datasets,
    datasets = dataset_names,
    mat = mat,
    meta = meta_small,
    meta_lookup = meta_lookup
  )
}


# =========================================================
# 预建 / 读取 signature index
# 避免每次 App 启动都重新 build matrix
# =========================================================

resolve_existing_file <- function(file, dir = ".") {
  if (file.exists(file)) {
    return(file)
  }
  
  file2 <- file.path(dir, file)
  if (file.exists(file2)) {
    return(file2)
  }
  
  stop("File not found: ", file, " or ", file2)
}

get_fast_source_time <- function(path_rdata) {
  base <- sub("\\.[Rr][Dd]ata$", "", path_rdata)
  
  candidates <- c(
    paste0(base, ".qs"),
    paste0(base, ".fst"),
    paste0(base, ".rds"),
    path_rdata
  )
  
  candidates <- candidates[file.exists(candidates)]
  
  if (length(candidates) == 0) {
    return(as.POSIXct("1970-01-01"))
  }
  
  max(file.info(candidates)$mtime, na.rm = TRUE)
}

build_or_read_signature_index <- function(
    sig_file,
    meta_file,
    type_value,
    dataset_col = "Dataset",
    index_dir = "signature_index_qs",
    overwrite_index = FALSE
) {
  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("Package 'qs' is not installed.")
  }
  
  if (!dir.exists(index_dir)) {
    dir.create(index_dir, recursive = TRUE)
  }
  
  source_id <- tools::file_path_sans_ext(basename(sig_file))
  
  index_file <- file.path(
    index_dir,
    paste0(source_id, "_", type_value, "_index.qs")
  )
  
  sig_time  <- get_fast_source_time(sig_file)
  meta_time <- get_fast_source_time(meta_file)
  src_time  <- max(sig_time, meta_time, na.rm = TRUE)
  
  if (
    !overwrite_index &&
    file.exists(index_file) &&
    file.info(index_file)$mtime >= src_time
  ) {
    message("Reading prebuilt index: ", index_file)
    return(qs::qread(index_file))
  }
  
  message("Building index: ", sig_file)
  
  sig_dt  <- read_table_fast(sig_file)
  meta_dt <- read_table_fast(meta_file)
  
  idx <- build_signature_index_shared(
    sig_dt = sig_dt,
    meta_dt = meta_dt,
    dataset_col = dataset_col,
    type_value = type_value,
    source_id = source_id
  )
  
  message("Saving index: ", index_file)
  
  qs::qsave(
    idx,
    file = index_file,
    preset = "fast",
    nthreads = parallel::detectCores(logical = TRUE)
  )
  
  rm(sig_dt, meta_dt)
  gc()
  
  idx
}
# =========================================================
# 统一获取 signature cache
# 两个模块都调用这个函数
# =========================================================

get_signature_cache <- function(
    chem_sig_files = c(
      "CIGSData.rdata",
      "HerbData.rdata",
      "LINCS_2020_Chemical.rdata",
      "LINCS_L1000_Chemical.rdata"
    ),
    chem_meta_files = c(
      "CIGSMetadata.rdata",
      "HerbDataMetadata.rdata",
      "LINCS_2020_Metadata.rdata",
      "LINCS_L1000_Metadata.rdata"
    ),
    gen_sig_files = c(
      "GuoziData.rdata",
      "PerturbAtlasData.rdata",
      "LINCS_2020_KD.rdata",
      "LINCS_2020_KO.rdata",
      "LINCS_L1000_KD.rdata",
      "LINCS_L1000_KO.rdata"
    ),
    gen_meta_files = c(
      "GuoziDataMetadata.rdata",
      "PerturbAtlasMetadata.rdata",
      "LINCS_2020_KD_Metadata.rdata",
      "LINCS_2020_KO_Metadata.rdata",
      "LINCS_L1000_KD_Metadata.rdata",
      "LINCS_L1000_KO_Metadata.rdata"
    ),
    signature_dir = ".",
    data_dir = "所有signature/",
    index_dir = "signature_index_qs",
    dataset_col = "Dataset",
    overwrite_index = FALSE
)  {
  stopifnot(length(chem_sig_files) == length(chem_meta_files))
  stopifnot(length(gen_sig_files) == length(gen_meta_files))
  
  cache_key <- paste(
    paste(normalizePath(chem_sig_files, mustWork = FALSE), collapse = "|"),
    paste(normalizePath(chem_meta_files, mustWork = FALSE), collapse = "|"),
    paste(normalizePath(gen_sig_files, mustWork = FALSE), collapse = "|"),
    paste(normalizePath(gen_meta_files, mustWork = FALSE), collapse = "|"),
    normalizePath(data_dir, mustWork = FALSE),
    dataset_col,
    sep = "||"
  )
  
  if (exists(cache_key, envir = GLOBAL_SIGNATURE_CACHE, inherits = FALSE)) {
    return(get(cache_key, envir = GLOBAL_SIGNATURE_CACHE, inherits = FALSE))
  }
  
  if (!dir.exists(data_dir)) {
    stop("data_dir does not exist: ", data_dir)
  }
  
  chem_idx_list <- Map(function(sig_file, meta_file) {
    
    sig_file  <- resolve_existing_file(sig_file, signature_dir)
    meta_file <- resolve_existing_file(meta_file, signature_dir)
    
    build_or_read_signature_index(
      sig_file = sig_file,
      meta_file = meta_file,
      type_value = "chemical",
      dataset_col = dataset_col,
      index_dir = index_dir,
      overwrite_index = overwrite_index
    )
    
  }, chem_sig_files, chem_meta_files)
  
  names(chem_idx_list) <- tools::file_path_sans_ext(basename(chem_sig_files))
  
  gen_idx_list <- Map(function(sig_file, meta_file) {
    
    sig_file  <- resolve_existing_file(sig_file, signature_dir)
    meta_file <- resolve_existing_file(meta_file, signature_dir)
    
    build_or_read_signature_index(
      sig_file = sig_file,
      meta_file = meta_file,
      type_value = "genetic",
      dataset_col = dataset_col,
      index_dir = index_dir,
      overwrite_index = overwrite_index
    )
    
  }, gen_sig_files, gen_meta_files)
  
  names(gen_idx_list) <- tools::file_path_sans_ext(basename(gen_sig_files))
  
  all_genes <- sort(unique(c(
    unlist(lapply(chem_idx_list, function(x) names(x$gene_to_row)), use.names = FALSE),
    unlist(lapply(gen_idx_list, function(x) names(x$gene_to_row)), use.names = FALSE)
  )))
  all_genes <- all_genes[nzchar(all_genes)]
  
  cache_obj <- list(
    chem_idx_list = chem_idx_list,
    gen_idx_list = gen_idx_list,
    all_genes = all_genes,
    data_dir = data_dir,
    dataset_col = dataset_col
  )
  
  assign(cache_key, cache_obj, envir = GLOBAL_SIGNATURE_CACHE)
  cache_obj
}

# =========================================================
# OneGene 查询函数
# 根据 gene 找每个 dataset 的 effect
# =========================================================

query_onegene_index <- function(idx, genes) {
  genes <- unique(normalize_gene(genes))
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
    res[, source_id := idx$source_id]
    res[, query_gene := genes[i]]
    
    out[[i]] <- res
  }
  
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(NULL)
  
  rbindlist(out, fill = TRUE)
}

# =========================================================
# GeneList 查询函数
# 找同时包含所有 query genes 的 datasets
# =========================================================

query_genelist_index <- function(idx, genes) {
  genes <- unique(normalize_gene(genes))
  genes <- genes[nzchar(genes)]
  if (length(genes) == 0) return(NULL)
  
  gene_sets <- idx$gene_to_datasets[genes]
  
  if (
    length(gene_sets) != length(genes) ||
    any(vapply(gene_sets, is.null, logical(1)))
  ) {
    return(NULL)
  }
  
  datasets <- Reduce(intersect, gene_sets)
  if (length(datasets) == 0) return(NULL)
  
  res <- copy(idx$meta_lookup[J(datasets), nomatch = 0L])
  if (nrow(res) == 0) return(NULL)
  
  res[, type := idx$type]
  res[, source_id := idx$source_id]
  res[, matched_gene_n := length(genes)]
  res[, query_genes := paste(genes, collapse = ",")]
  res[, row_key := paste(type, source_id, dataset, sep = "||")]
  
  unique(res, by = "row_key")
}

# =========================================================
# 统一 DEG 缓存
# 两个模块点击 dataset 时都用它
# =========================================================

get_deg_dt_cached_shared <- function(dataset, data_dir) {
  key <- paste0(normalizePath(data_dir, mustWork = FALSE), "::", dataset)
  
  if (exists(key, envir = GLOBAL_DEG_CACHE, inherits = FALSE)) {
    return(get(key, envir = GLOBAL_DEG_CACHE, inherits = FALSE))
  }
  
  fst_file <- file.path(data_dir, paste0(dataset, ".fst"))
  csv_file <- file.path(data_dir, paste0(dataset, ".csv"))
  
  dt <- NULL
  
  if (file.exists(fst_file) && requireNamespace("fst", quietly = TRUE)) {
    dt <- as.data.table(fst::read_fst(fst_file))
  } else if (file.exists(csv_file)) {
    dt <- fread(csv_file, showProgress = FALSE)
  }
  
  assign(key, dt, envir = GLOBAL_DEG_CACHE)
  dt
}

# =========================================================
# 通用下载数据解析函数
# 按优先级依次尝试: CSV → GSEA/Chemical .fst → GSEA/Genetic .fst → NULL
# 用于 Download/Browse/Genelist/Synergy/GPSA/OneGene 等模块的动态下载
# =========================================================
resolve_dataset_data <- function(dataset) {
  ds <- as.character(dataset)
  
  # 1. 优先查找已有的 CSV 文件（最快）
  csv_path <- file.path("rdata_columns_csv", paste0(ds, ".csv"))
  if (file.exists(csv_path)) {
    return(fread(csv_path, showProgress = FALSE))
  }
  
  # 2. 尝试 GSEA/Chemical .fst 文件
  fst_chem <- file.path("GSEA", "Chemical", paste0(ds, ".fst"))
  if (file.exists(fst_chem) && requireNamespace("fst", quietly = TRUE)) {
    return(as.data.table(fst::read_fst(fst_chem)))
  }
  
  # 3. 尝试 GSEA/Genetic .fst 文件
  fst_gen <- file.path("GSEA", "Genetic", paste0(ds, ".fst"))
  if (file.exists(fst_gen) && requireNamespace("fst", quietly = TRUE)) {
    return(as.data.table(fst::read_fst(fst_gen)))
  }
  
  # 4. 全部失败
  NULL
}