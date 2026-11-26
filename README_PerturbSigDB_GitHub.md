# PerturbSigDB — source code and demonstration dataset

[![R](https://img.shields.io/badge/Language-R-blue)](https://www.r-project.org/)

**PerturbSigDB** is a manually curated database of chemical and genetic perturbation transcriptional signatures. This repository provides the **R/Shiny source code** for Browse, Gene Query, Gene Set Query, Perturb Query and Download, together with a small **50-signature demonstration dataset** covering 11 source groups. The complete database is hosted at **https://web.biotcm.net/PSDB/**.

> **Important:** This is a compact demonstration release, **not** a copy of the full 1,091,148-signature database. Web-site summary statistics shown in the application describe the full resource; demo query results cover only the 50 bundled signatures. Full-database ranking, runtime and manuscript benchmark results cannot be reproduced from this subset.

## Quick start

This repository contains a 50-signature **demonstration subset**, not the full production database. The R/Shiny source code is provided for transparency and small-scale demonstrations.

1. Install **R 4.3 or later** and the R packages required by the Shiny application. The optional `install_dependencies.R` helper lists application dependencies; package availability may vary by R version and operating system.
2. Download or clone the repository and open an R session with the repository root (the folder containing `app.R`) as its working directory.
3. Run the application from the repository root:

   ```r
   shiny::runApp(".")
   ```

   Or run from a terminal in that same folder:

   ```bash
   Rscript -e "shiny::runApp('.')"
   ```

4. Open the local URL printed by Shiny, then test Browse, Gene Query, Gene Set Query, and Perturb Query using the bundled demonstration data. The startup script `app.R` calls `R/bootstrap_data.R` to reconstruct the original relative data paths from `data_manifest.csv`; write access to the local repository folder is required.

The demonstration dataset and associated indexes are insufficient to reproduce full-database rankings, manuscript benchmarking results, or production runtime estimates. Some interface pages may require additional assets or production-only reference data. A functioning local installation depends on the R environment and the required packages; the files alone do not guarantee that every Shiny view can run on every computer.

## What is included?

```text
PerturbSigDB/
├── README.md
├── .gitignore
├── app.R                       # Shiny application entry point
├── install_dependencies.R      # Optional package installation helper
├── data_manifest.csv           # Original relative path -> generic filename + SHA-256
├── data/                        # 164 demonstration files (180 mapped paths)
│   ├── data-1.rdata
│   ├── data-2.rdata
│   └── ...
├── R/
│   ├── bootstrap_data.R       # Restores relative data paths at startup
│   ├── global.R
│   ├── global_signature_cache.R
│   ├── global_gpsa_cache.R
│   ├── global_pathway_cache.R
│   ├── query_signature_by_gene_fst.R
│   ├── Browse.R
│   ├── OneGene.R
│   ├── Genelist.R
│   ├── Synergy.R
│   ├── GPSA.R
│   └── Download.R
└── www/                        # Web interface images and static assets
```

## Why are the files named `data-1`, `data-2`, ...?

The **164 physical** bundled binary/example data files have generic names under `data/`, as requested for the release package. The manifest contains **180 path mappings**, including 16 Unicode filename aliases. **Their original paths and dataset source labels remain documented** in `data_manifest.csv`, along with byte sizes and SHA-256 checksums, to maintain transparency and provenance. **Internal chemical/genetic signature identifiers and metadata have not been renamed.** This is filename normalization, **not anonymization**. A small number of assets require extra aliases for the UTF-8 `µ` / `μ` characters, because the original demonstration archive contained `#U00b5` / `#U03bc` filenames while the serialized lookup contained the actual Unicode characters. Both aliases are in the manifest.

The original R code and serialized lookup indexes expect particular relative paths, including `GSEA/signatures/signature_lookup.rds`, `GSEA/Chemical/*.fst`, `GSEA/Genetic/*.fst`, `signature_by_gene_fst/` and the individual CSV files. On launch, `R/bootstrap_data.R` reads `data_manifest.csv` and restores those expected paths as filesystem links (or copies where linking is unavailable). Therefore, **do not rename files inside `data/` or edit the manifest independently**.

The first launch requires write permission to the extracted folder to create these runtime links/copies. The generated paths are ignored by Git. The manifest also provides checksums and the original resource paths for auditing the demonstration files.

## The four main modules

| Module | Entry point / key code | Function |
|---|---|---|
| Browse | `R/Browse.R` | Browse chemical/genetic perturbation signatures, metadata and selected pathway enrichment. |
| Gene Query | `R/OneGene.R` | Retrieve signatures associated with positive or negative gene-level changes. |
| Gene Set Query | `R/Synergy.R` and **`R/Genelist.R`** | Retrieve chemical/genetic signatures enriched for a custom or curated gene set; directional MWU statistic, displayed as NES, and BH correction. |
| Perturb Query | **`R/GPSA.R`** | Two-stage rank-based signature matching and approximate significance calculations; BH and conservative Bonferroni corrections. |

`R/Download.R` provides downloads. `R/global*.R` and `R/query_signature_by_gene_fst.R` provide shared indexing, annotation, cache and conversion utilities. The separate `Help.R` file is omitted because `app.R` already includes an integrated Help section; `Synergy.R` is retained because it implements the **active Gene Set Query UI and server**.

## Core algorithm details

### Gene Set Query: directional gene-set matching

`run_gsquery_global()` in `R/Genelist.R` creates/uses an inverted index of up to 200 top-up and 200 top-down genes per reference signature, ranks candidates by gene overlap, retains at most 10,000 candidates, and scores matched genes against the full ranked signature with a directional Mann–Whitney U statistic. Signatures with fewer than three matched query genes are excluded. The score labelled **NES** is a standardized U statistic (a signed Z-score), **not** the standard GSEA normalized enrichment score. Two-sided asymptotic P values are BH-adjusted over successfully scored second-stage candidates. The user interface for this algorithm lives in `R/Synergy.R`.

By contrast, the enrichment plots for an individual selected signature use `fgsea`; those `fgsea` NES values have a **different definition**. The demo contains KEGG and Hallmark GMT resources but does not contain all pathway collections or production precomputed pathway caches; built-in pathway queries can fall back to on-demand evaluation.

### Perturb Query: ranked perturbation signature matching

`run_gpsa_global()` in `R/GPSA.R` accepts gene–effect pairs (typically `gene` and numeric `logFC`), removes duplicate/invalid rows, and retains up to 10,000 strongest absolute effects. It uses an index built from the top/bottom 200 genes to select at most 10,000 reference candidates, requiring at least five shared genes for downstream matching. Its correlation procedure first ranks **each full gene-effect profile**, then computes the Pearson correlation of rank vectors restricted to shared genes. This is the exact implemented *full-profile-rank Spearman-style* statistic; it may differ from conventional Spearman correlations obtained by reranking shared genes only. Nominal P values use an approximate t statistic on the rounded correlation, with BH correction over successfully evaluated candidates and an additional Bonferroni adjustment over the reference lookup size. Because these are candidate-selected correlations of dependent gene effects, P values should be interpreted as ranking aids rather than formal independent-experiment validation.

**Please note:** The demonstration lookup contains 50 signatures, so its multiple-testing adjustment (especially Bonferroni) does **not** reproduce full-database adjusted P values.

## Reproducibility and outputs

The repository makes the main query implementation available for review: `run_gsquery_global()` in `R/Genelist.R` and `run_gpsa_global()` in `R/GPSA.R`. Each query uses the bundled indexes and reference signatures provided under `data/`, restored to their expected paths by `R/bootstrap_data.R`. Sample query input files are recorded in `data_manifest.csv` under the original relative paths `Gene Set Query Demo.txt` and `Perturb Query Demo.txt`.

To check a local run, compare results from the demonstration queries against the documented algorithm steps, and record your R `sessionInfo()`. Query results generally support CSV export and selected visualizations support PDF export. **This repository does not ship a separate `smoke_test.R` or `run_core_demo.R` script**, and its demonstration outputs do not reproduce results from the full production database.

## Availability and restrictions

The bundled data represent a selected demonstration subset of publicly derived signature resources. They are not raw sequencing reads and not the entire PerturbSigDB collection. Dataset IDs are retained for scientific traceability. Original datasets, relevant accessions and derived-signature context are documented in the accompanying manuscript and at the [PerturbSigDB website](https://web.biotcm.net/PSDB/).

**Licensing:** This source release deliberately does not assert permission to redistribute all third-party components or derived datasets. Before public publication, confirm the original source licenses (including gene-set library licensing), and add a code `LICENSE` appropriate to the project authors' decision. Public visibility on GitHub is not equivalent to an open-source license.

## Notes

- Development, case-study and benchmarking files not required for the demonstration have been removed from this source release. The archive includes production algorithm implementations but **not** the full production database or benchmark reproduction data.
- Some Shiny UI panels depend on assets or production-only data that are absent in the demo. They may show empty/limited output.
- This repository is intended for source transparency and small-scale reproducibility, not for serving a production instance.
