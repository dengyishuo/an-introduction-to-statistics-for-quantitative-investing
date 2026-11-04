#!/usr/bin/env Rscript
# 《量化投资的统计学导论》补充数据集（支撑第 12–16 章）
# 全部由 mlpanel / indday 真实数据推导，不含模拟
# 用法: cd an-introduction-to-statistics-for-quantitative-investing
#       Rscript script/07_build_extra.R

root <- Sys.getenv("BOOK_ROOT", unset = ".")
if (!dir.exists(file.path(root, "data"))) root <- "."
csv_dir <- file.path(root, "data", "csv")

rd <- function(n) {
  f <- file.path(csv_dir, paste0(n, ".csv"))
  if (!file.exists(f)) f <- paste0(f, ".gz")
  read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
}
wr <- function(df, n) {
  gz <- gzfile(file.path(csv_dir, paste0(n, ".csv.gz")), "w")
  write.csv(df, gz, row.names = FALSE, fileEncoding = "UTF-8")
  close(gz)
  cat(sprintf("  %-10s %6d 行 %3d 列\n", n, nrow(df), ncol(df)))
}

panel <- rd("mlpanel")
cat("mlpanel:", nrow(panel), "行\n")

# ---------------------------------------------------------------- 1. mlevent
# 二分类与计数事件，供 Logistic 与泊松回归
#   win    : 下月收益是否高于同月中位数（二分类标签）
#   big_up : 下月收益是否超过 +10%
#   down   : 下月收益是否低于 -10%
ev <- panel[!is.na(panel$fwd_ret), ]
ev <- do.call(rbind, lapply(split(ev, ev$ym), function(d) {
  med <- median(d$fwd_ret, na.rm = TRUE)
  d$win    <- as.integer(d$fwd_ret > med)
  d$big_up <- as.integer(d$fwd_ret >  0.10)
  d$down   <- as.integer(d$fwd_ret < -0.10)
  d
}))
mlevent <- ev[, c("secucode", "name", "industry", "ym", "fwd_ret",
                  "win", "big_up", "down", "size", "turnover", "vol_12m",
                  "rev_1m", "mom_12_1")]
wr(mlevent, "mlevent")

# ---------------------------------------------------------------- 2. drawdown
# 生存分析数据：随访 12 个月，事件 = 累计回撤达到 30%
# 删失 = 随访结束时仍未触发（A 股波动大，不设随访上限会导致几乎无删失）
WIN <- 12; TH <- 0.30
panel_s <- panel[order(panel$secucode, panel$ym), ]
dd <- do.call(rbind, lapply(split(panel_s, panel_s$secucode), function(d) {
  if (nrow(d) < WIN) return(NULL)
  d <- d[1:WIN, ]
  nav <- cumprod(1 + ifelse(is.na(d$ret), 0, d$ret))
  drawdown <- 1 - nav / cummax(nav)
  hit <- which(drawdown >= TH)
  data.frame(
    secucode = d$secucode[1], name = d$name[1], industry = d$industry[1],
    months = if (length(hit)) hit[1] - 1 else WIN - 1,
    event  = as.integer(length(hit) > 0),
    size = d$size[1], vol_12m = d$vol_12m[1], turnover = d$turnover[1],
    stringsAsFactors = FALSE)
}))
dd <- dd[dd$months > 0, ]
wr(dd, "drawdown")

# ---------------------------------------------------------------- 3. indcnt
# 行业×月 的计数数据，供泊松回归与率模型
#   ndown : 该月跌幅超过 1% 的交易日数（20 个行业 ETF 的日收益）
#   exposure : 该月交易日数（人年式的暴露量）
ind <- rd("indday")
ind$ym <- substr(ind$date, 1, 7)
code_cols <- setdiff(names(ind), c("date", "ym"))
cnt <- do.call(rbind, lapply(split(ind, ind$ym), function(d) {
  if (nrow(d) < 8) return(NULL)
  data.frame(
    ym = d$ym[1],
    sector = code_cols,
    ndown = vapply(code_cols, function(s) sum(d[[s]] < -0.01, na.rm = TRUE), numeric(1)),
    nbig  = vapply(code_cols, function(s) sum(abs(d[[s]]) > 0.02, na.rm = TRUE), numeric(1)),
    exposure = nrow(d),
    sd_m = vapply(code_cols, function(s) sd(d[[s]], na.rm = TRUE), numeric(1)),
    stringsAsFactors = FALSE
  )
}))
indcnt <- cnt
wr(indcnt, "indcnt")

# ---------------------------------------------------------------- 4. scale
# 幂律关系数据，供非线性回归（nls）与多项式回归
# 市值 vs 换手率：金融上已知的负幂律关系
sc <- panel[, c("secucode", "name", "industry", "ym", "size", "turnover", "vol_12m")]
sc <- sc[complete.cases(sc), ]
# 换手率为 0 的月份无法取对数（当月无成交），剔除
sc <- sc[sc$turnover > 0, ]
sc <- do.call(rbind, lapply(split(sc, sc$ym), function(d) {
  if (nrow(d) < 200) return(NULL)
  d
}))
wr(sc, "scale")

# ---------------------------------------------------------------- 5. divcurve
# 分散化曲线：随机选 n 只股票等权持有，组合的月度波动
# 用于非线性回归（第 16 章）：sigma(n) = s1 * sqrt(rho + (1-rho)/n)
# 每组重复抽样 300 次取均值，随机种子固定，结果可复现
set.seed(2026)
pw <- reshape(panel[!is.na(panel$ret), c("ym", "secucode", "ret")],
              idvar = "ym", timevar = "secucode", direction = "wide")
pw <- pw[order(pw$ym), ]
MM <- as.matrix(pw[, -1])
MM <- MM[, colSums(is.na(MM)) == 0]
cat("分散化曲线：", ncol(MM), "只 ×", nrow(MM), "月\n")
ns <- c(1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 25, 30, 40, 50, 70, 100, 150, 200, 300, 500)
divcurve <- data.frame(n = ns, sd = sapply(ns, function(n) {
  mean(replicate(300, sd(rowMeans(MM[, sample(ncol(MM), n), drop = FALSE]))))
}))
wr(divcurve, "divcurve")

cat("\n完成：4 个补充数据集\n")
