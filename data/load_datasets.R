# load_datasets —— 《量化投资的统计学导论》数据集载入脚本
# 用法（在 R 会话中）: source("an-introduction-to-statistics-for-quantitative-investing/data/load_datasets.R")
#
# 数据: data/csv/ 下的书稿数据集快照，不含模拟数据。
# 列名保留原始证券代码（如 510300.SS），故 check.names = FALSE。
# 大数据集（mlpanel、mlfin）以 .csv.gz 存储，载入时自动识别。

datasets_dir <- function() {
  cand <- c("an-introduction-to-statistics-for-quantitative-investing/data/csv",
            "data/csv", "../data/csv")
  hit <- cand[dir.exists(cand)]
  if (!length(hit)) stop("找不到 data/csv")
  hit[1]
}

# 读单个数据集：优先 .csv，其次 .csv.gz
load_dataset <- function(name, dir = NULL) {
  if (is.null(dir)) dir <- datasets_dir()
  f <- file.path(dir, paste0(name, ".csv"))
  if (!file.exists(f)) {
    fz <- paste0(f, ".gz")
    if (!file.exists(fz)) stop("找不到数据集: ", name)
    f <- fz
  }
  utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
}

load_datasets <- function(dir = NULL) {
  if (is.null(dir)) dir <- datasets_dir()
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

load_datasets()
