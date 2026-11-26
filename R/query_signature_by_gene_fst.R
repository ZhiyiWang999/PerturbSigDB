
get_by_gene_files <- function(sig_file, by_gene_dir = "signature_by_gene_fst") {
  source_id <- tools::file_path_sans_ext(basename(sig_file))
  
  list(
    fst = file.path(by_gene_dir, paste0(source_id, "_by_gene.fst")),
    map = file.path(by_gene_dir, paste0(source_id, "_gene_map.qs")),
    source_id = source_id
  )
}

# 辅助函数：为查询结果添加药物/基因注释
add_annotations <- function(dt, anno_dir = "signature_by_gene_fst") {
  if (is.null(dt) || nrow(dt) == 0) return(dt)
  
  # Chemical: FDA annotation
  fda_file <- file.path(anno_dir, "FDA_annotation.fst")
  if (file.exists(fda_file) && "Perturbagen" %in% names(dt)) {
    fda_dt <- tryCatch(fst::read_fst(fda_file, as.data.table = TRUE), error = function(e) NULL)
    if (!is.null(fda_dt)) dt <- merge(dt, fda_dt, by = "Perturbagen", all.x = TRUE)
  }
  
  # Chemical: drug-target mapping
  dt_file <- file.path(anno_dir, "drug_target_annotation.fst")
  if (file.exists(dt_file) && "Perturbagen" %in% names(dt)) {
    dt_dt <- tryCatch(fst::read_fst(dt_file, as.data.table = TRUE), error = function(e) NULL)
    if (!is.null(dt_dt)) dt <- merge(dt, dt_dt, by = "Perturbagen", all.x = TRUE)
  }
  
  # Genetic: gene annotation
  gene_file <- file.path(anno_dir, "gene_annotation.fst")
  if (file.exists(gene_file) && "pbgene" %in% names(dt)) {
    gene_dt <- tryCatch(fst::read_fst(gene_file, as.data.table = TRUE), error = function(e) NULL)
    if (!is.null(gene_dt)) {
      dt <- merge(dt, gene_dt, by = "pbgene", all.x = TRUE)
      if ("entrez_id" %in% names(dt)) dt[, entrez_id := NULL]
    }
  }
  
  # Remove irrelevant annotation columns based on perturbation type
  type_col <- if ("Type" %in% names(dt)) "Type" else if ("type" %in% names(dt)) "type" else NULL
  if (!is.null(type_col)) {
    types <- unique(dt[[type_col]])
    if (length(types) == 1) {
      if (types %in% c("chemical", "Chemical")) {
        cols_drop <- intersect(c("known_targets", "protein_class"), names(dt))
        if (length(cols_drop) > 0) dt[, (cols_drop) := NULL]
      } else if (types %in% c("genetic", "Genetic")) {
        cols_drop <- intersect(c("target", "known_targets", "uniprot_id", "disease_area", "indication"), names(dt))
        if (length(cols_drop) > 0) dt[, (cols_drop) := NULL]
      }
    }
  }
  
  dt
}

query_onegene_by_gene_fst_one_source <- function(
    sig_file,
    meta_file,
    genes,
    type_value,
    signature_dir = ".",
    by_gene_dir = "signature_by_gene_fst",
    dataset_col = "Dataset"
) {
  meta_file <- resolve_existing_file(meta_file, signature_dir)
  
  files <- get_by_gene_files(sig_file, by_gene_dir)
  
  if (!file.exists(files$fst) || !file.exists(files$map)) {
    warning("by-gene fst or map not found for: ", sig_file)
    return(NULL)
  }
  
  gene_map <- qs::qread(files$map)
  
  genes <- unique(normalize_gene(genes))
  genes <- genes[nzchar(genes)]
  
  hit <- gene_map[match(genes, gene)]
  hit <- hit[!is.na(colname)]
  
  if (nrow(hit) == 0) {
    return(NULL)
  }
  
  cols_to_read <- c("dataset", hit$colname)
  
  x <- as.data.table(
    fst::read_fst(
      files$fst,
      columns = cols_to_read
    )
  )
  
  out <- vector("list", nrow(hit))
  
  for (i in seq_len(nrow(hit))) {
    g <- hit$gene[i]
    col <- hit$colname[i]
    
    eff <- x[[col]]
    keep <- !is.na(eff)
    
    if (!any(keep)) next
    
    out[[i]] <- data.table(
      dataset = x$dataset[keep],
      effect = suppressWarnings(as.numeric(eff[keep])),
      query_gene = g,
      type = type_value,
      source_id = files$source_id
    )
  }
  
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) {
    return(NULL)
  }
  
  res <- rbindlist(out, fill = TRUE)
  
  meta_dt <- read_table_fast(meta_file)
  
  if (dataset_col %in% names(meta_dt)) {
    setnames(meta_dt, dataset_col, "dataset")
  }
  
  res <- merge(
    res,
    meta_dt,
    by = "dataset",
    all.x = TRUE
  )
  
  # Standardize Tissue columns (Cell is pre-standardized in fst)
  res <- standardize_tissue(res)

  # Remove rows where Perturbagen contains Chinese characters
  if ("Perturbagen" %in% names(res)) {
    res <- res[!grepl("[\u4e00-\u9fff]", Perturbagen)]
  }
  
  res
}

query_genelist_by_gene_fst_one_source <- function(
    sig_file,
    meta_file,
    genes,
    type_value,
    signature_dir = ".",
    by_gene_dir = "signature_by_gene_fst",
    dataset_col = "Dataset"
) {
  meta_file <- resolve_existing_file(meta_file, signature_dir)
  
  files <- get_by_gene_files(sig_file, by_gene_dir)
  
  if (!file.exists(files$fst) || !file.exists(files$map)) {
    warning("by-gene fst or map not found for: ", sig_file)
    return(NULL)
  }
  
  gene_map <- qs::qread(files$map)
  
  genes <- unique(normalize_gene(genes))
  genes <- genes[nzchar(genes)]
  
  # 找出查询基因中在该源中有对应列的基因
  hit <- gene_map[match(genes, gene)]
  hit <- hit[!is.na(colname)]          # 移除没有对应列的基因
  if (nrow(hit) == 0) return(NULL)
  
  cols_to_read <- c("dataset", hit$colname)
  x <- as.data.table(fst::read_fst(files$fst, columns = cols_to_read))
  
  # 对每个数据集计算匹配的基因数量和列表
  # 使用向量化操作：对每行，找到非空的列（排除 dataset 列）
  mat_cols <- hit$colname
  # 创建逻辑矩阵：哪些列非空
  mat <- !is.na(x[, ..mat_cols])
  # 计算每行匹配的基因数
  n_matched <- rowSums(mat)
  # 保留至少匹配一个基因的行
  keep <- n_matched > 0
  if (!any(keep)) return(NULL)
  
  # 生成每行匹配的基因名称（根据非空列对应的基因）
  matched_genes_list <- apply(mat[keep, , drop = FALSE], 1, function(row) {
    paste(hit$gene[row], collapse = ",")
  })
  
  res <- data.table(
    dataset = x$dataset[keep],
    type = type_value,
    source_id = files$source_id,
    n_matched = n_matched[keep],
    matched_genes = matched_genes_list,
    query_genes = paste(genes, collapse = ",")   # 原始查询基因，用于上下文
  )
  
  # 合并元数据
  meta_dt <- read_table_fast(meta_file)
  if (dataset_col %in% names(meta_dt)) {
    setnames(meta_dt, dataset_col, "dataset")
  }
  
  res <- merge(res, meta_dt, by = "dataset", all.x = TRUE)
  
  # Standardize Tissue columns (Cell is pre-standardized in fst)
  res <- standardize_tissue(res)

  # 去重（可能因元数据重复）
  res[, row_key := paste(type, source_id, dataset, sep = "||")]
  unique(res, by = "row_key")
}

query_onegene_by_gene_fst_all <- function(
    genes,
    signature_dir = ".",
    by_gene_dir = "signature_by_gene_fst",
    dataset_col = "Dataset"
) {
  chem_sig_files <- c(
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
  
  gen_sig_files <- c(
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
  
  out1 <- Map(function(sig, meta) {
    query_onegene_by_gene_fst_one_source(
      sig_file = sig,
      meta_file = meta,
      genes = genes,
      type_value = "chemical",
      signature_dir = signature_dir,
      by_gene_dir = by_gene_dir,
      dataset_col = dataset_col
    )
  }, chem_sig_files, chem_meta_files)
  
  out2 <- Map(function(sig, meta) {
    query_onegene_by_gene_fst_one_source(
      sig_file = sig,
      meta_file = meta,
      genes = genes,
      type_value = "genetic",
      signature_dir = signature_dir,
      by_gene_dir = by_gene_dir,
      dataset_col = dataset_col
    )
  }, gen_sig_files, gen_meta_files)
  
  out <- c(out1, out2)
  out <- Filter(Negate(is.null), out)
  
  if (length(out) == 0) return(NULL)
  
  combined <- rbindlist(out, fill = TRUE)
  add_annotations(combined, by_gene_dir)
}



query_genelist_by_gene_fst_all <- function(
    genes,
    signature_dir = ".",
    by_gene_dir = "signature_by_gene_fst",
    dataset_col = "Dataset",
    top_n = 500
) {
  chem_sig_files <- c(
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
  gen_sig_files <- c(
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
  
  out1 <- Map(function(sig, meta) {
    query_genelist_by_gene_fst_one_source(
      sig_file = sig,
      meta_file = meta,
      genes = genes,
      type_value = "chemical",
      signature_dir = signature_dir,
      by_gene_dir = by_gene_dir,
      dataset_col = dataset_col
    )
  }, chem_sig_files, chem_meta_files)
  
  out2 <- Map(function(sig, meta) {
    query_genelist_by_gene_fst_one_source(
      sig_file = sig,
      meta_file = meta,
      genes = genes,
      type_value = "genetic",
      signature_dir = signature_dir,
      by_gene_dir = by_gene_dir,
      dataset_col = dataset_col
    )
  }, gen_sig_files, gen_meta_files)
  
  out <- c(out1, out2)
  out <- Filter(Negate(is.null), out)
  
  if (length(out) == 0) return(NULL)
  
  combined <- rbindlist(out, fill = TRUE)
  
  # 按匹配基因数降序排序，取前 top_n 条
  setorderv(combined, "n_matched", order = -1L)
  if (nrow(combined) > top_n) {
    combined <- combined[1:top_n, ]
  }
  
  add_annotations(combined, by_gene_dir)
}