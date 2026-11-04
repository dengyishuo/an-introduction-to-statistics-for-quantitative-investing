# finwr —— 《量化投资的统计学导论》数据集载入脚本
# 用法（在 R 会话中）: source("an-introduction-to-statistics-for-quantitative-investing/data/finwr.R")
#
# 数据来源: xquant-db 真实行情与财务指标，由 script/02_build_datasets.R 与
#           script/05_build_mlpanel.R 固化快照。不含模拟数据。
# 列名保留原始证券代码（如 510300.SS），故 check.names = FALSE。
# 大数据集（mlpanel、mlfin）以 .csv.gz 存储，载入时自动识别。

finwr_dir <- function() {
  cand <- c("an-introduction-to-statistics-for-quantitative-investing/data/csv",
            "data/csv", "../data/csv")
  hit <- cand[dir.exists(cand)]
  if (!length(hit)) stop("找不到 data/csv，请先运行 script/02_build_datasets.R")
  hit[1]
}

# 读单个数据集：优先 .csv，其次 .csv.gz
finwr_load <- function(name, dir = NULL) {
  if (is.null(dir)) dir <- finwr_dir()
  f <- file.path(dir, paste0(name, ".csv"))
  if (!file.exists(f)) {
    fz <- paste0(f, ".gz")
    if (!file.exists(fz)) stop("找不到数据集: ", name)
    f <- fz
  }
  utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
}

finwr_attach <- function(dir = NULL) {
  if (is.null(dir)) dir <- finwr_dir()
  files <- list.files(dir, pattern = "\\.csv(\\.gz)?$")
  files <- files[!startsWith(files, "_")]
  for (f in files) {
    nm <- sub("\\.csv(\\.gz)?$", "", f)
    assign(nm, utils::read.csv(file.path(dir, f), stringsAsFactors = FALSE,
                               check.names = FALSE), envir = .GlobalEnv)
  }
  cat("已载入:", paste(sub("\\.csv(\\.gz)?$", "", files), collapse = ", "), "\n")
  invisible(files)
}

finwr_attach()
