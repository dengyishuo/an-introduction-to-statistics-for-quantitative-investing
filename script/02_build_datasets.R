#!/usr/bin/env Rscript
# 《量化投资的统计学导论》数据集构建
# 数据源: ../../xquant-db/data/（真实行情 + 财务指标）
# 用法: cd an-introduction-to-statistics-for-quantitative-investing && Rscript script/02_build_datasets.R
#
# 已处理的两个源数据问题：
#   1) leading_raw_data.csv 中 4 只股票存在整行完全重复 → 按 date+code 去重
#   2) 行情文件与 universe/leading.csv 的 name 列有错标（两处共用同一错误源）
#      → 以 code 为准，名称取 VERIFIED_NAMES 校正表；未校正者沿用源名称

root <- Sys.getenv("BOOK_ROOT", unset = "")
if (root == "" || !dir.exists(root)) root <- "an-introduction-to-statistics-for-quantitative-investing"
if (!dir.exists(root)) root <- "."
db <- Sys.getenv("XQUANT_DB", unset = file.path(root, "..", "..", "xquant-db"))
if (!dir.exists(db)) db <- file.path("..", "xquant-db")
stopifnot(dir.exists(file.path(db, "data", "raw-data")))

csv_dir <- file.path(root, "data", "csv")
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------- 读取与清洗
read_px <- function(f) {
  d <- read.csv(file.path(db, "data", "raw-data", f), stringsAsFactors = FALSE)
  n0 <- nrow(d)
  d <- d[!duplicated(d[, c("date", "code")]), ]
  d$date <- as.Date(d$date)
  d <- d[order(d$code, d$date), ]
  cat(sprintf("%-26s %6d -> %6d 行（去重 %d）\n", f, n0, nrow(d), n0 - nrow(d)))
  d
}

stk <- read_px("leading_raw_data.csv")
ind <- read_px("industry_raw_data.csv")
idx <- read_px("china_index_raw_data.csv")

# 名称校正：仅修正已用价格量级与公开信息双重核验的错标
VERIFIED_NAMES <- c(
  "002415.SZ" = "海康威视",   # 源标「汉得信息」，价格区间 25-36 元对应海康
  "002129.SZ" = "TCL中环",    # 源标「锦浪科技」，价格区间 7-16 元对应 TCL中环
  "002131.SZ" = "利欧股份",   # 源标「奥飞娱乐」，最低 1.36 元对应利欧
  "603160.SS" = "汇顶科技",   # 源标「兆易创新」，与 603986 重名
  "000826.SZ" = "启迪环境",   # 源标「*ST启环」，同一公司曾用名
  "000002.SZ" = "万科A",
  "000858.SZ" = "五粮液",
  "000876.SZ" = "新希望",
  "002001.SZ" = "新和成"
)

# 行情脏值处理：A 股 ETF 日涨跌限 10%，个股限 20%（科创板/创业板 20%）
# 超过阈值视为源数据错误，置为 NA 后按缺失处理
MAX_DAILY_MOVE <- 0.25

clean_px <- function(d) {
  d <- d[order(d$code, d$date), ]
  d$bad <- FALSE
  for (cd in unique(d$code)) {
    i <- which(d$code == cd)
    r <- c(NA, diff(log(d$adjusted[i])))
    bad <- which(abs(r) > MAX_DAILY_MOVE)
    if (length(bad)) d$bad[i[bad]] <- TRUE
  }
  if (any(d$bad)) cat("  标记脏值行:", sum(d$bad), "\n")
  d[!d$bad, ]
}

fix_names <- function(d) {
  hit <- d$code %in% names(VERIFIED_NAMES)
  d$name[hit] <- VERIFIED_NAMES[d$code[hit]]
  d
}
stk <- fix_names(stk); ind <- fix_names(ind); idx <- fix_names(idx)
stk <- clean_px(stk); ind <- clean_px(ind); idx <- clean_px(idx)

# 宽表：日期 × 代码 的复权收盘价
to_wide <- function(d) {
  w <- reshape(d[, c("date", "code", "adjusted")], idvar = "date",
               timevar = "code", direction = "wide")
  names(w) <- sub("^adjusted\\.", "", names(w))
  w[order(w$date), ]
}
px_stk <- to_wide(stk); px_ind <- to_wide(ind); px_idx <- to_wide(idx)

# 日对数收益率
ret_of <- function(w) {
  m <- as.matrix(w[, -1]); rownames(m) <- as.character(w$date)
  r <- apply(m, 2, function(x) c(NA, diff(log(x))))
  rownames(r) <- rownames(m)
  r[-1, , drop = FALSE]
}
r_stk <- ret_of(px_stk); r_ind <- ret_of(px_ind); r_idx <- ret_of(px_idx)

name_map <- unique(rbind(
  stk[, c("code", "name")],
  ind[, c("code", "name")],
  idx[, c("code", "name")]
))
name_map <- name_map[!duplicated(name_map$code), ]
nm <- function(codes) name_map$name[match(codes, name_map$code)]

save_csv <- function(df, name) {
  f <- file.path(csv_dir, paste0(name, ".csv"))
  write.csv(df, f, row.names = FALSE, fileEncoding = "UTF-8")
  cat(sprintf("  %-10s %6d 行 %3d 列\n", name, nrow(df), ncol(df)))
}

# 季度标签与区间收益
qtr_of <- function(dates) {
  y <- format(dates, "%Y")
  q <- (as.integer(format(dates, "%m")) - 1L) %/% 3L + 1L
  paste0(y, "Q", q)
}
# 注：无 xts 依赖，手工按季度分组算复合收益
qtr_ret <- function(r, dates) {
  q <- qtr_of(dates)
  out <- list()
  for (qq in unique(q)) {
    sel <- q == qq
    if (sum(sel) < 5) next
    sub <- r[sel, , drop = FALSE]
    out[[qq]] <- data.frame(
      qtr = qq,
      code = colnames(r),
      ret = apply(sub, 2, function(x) prod(1 + x, na.rm = TRUE) - 1),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}
mon_ret <- function(r, dates) {
  m <- format(dates, "%Y-%m")
  out <- list()
  for (mm in unique(m)) {
    sel <- m == mm
    if (sum(sel) < 5) next
    sub <- r[sel, , drop = FALSE]
    out[[mm]] <- data.frame(
      month = mm,
      code = colnames(r),
      ret = apply(sub, 2, function(x) prod(1 + x, na.rm = TRUE) - 1),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

d_stk <- as.Date(rownames(r_stk))
d_ind <- as.Date(rownames(r_ind))
d_idx <- as.Date(rownames(r_idx))

cat("\n构建数据集:\n")

# ---------------------------------------------------------------- 1. px1 时间序列
s1 <- stk[stk$code == "600519.SS", ]
px1 <- data.frame(date = format(s1$date, "%Y-%m-%d"), close = round(s1$close, 2),
                  adjusted = round(s1$adjusted, 2), volume = s1$volume,
                  stringsAsFactors = FALSE)
save_csv(px1, "px1")

# ---------------------------------------------------------------- 2. abret 配对/两样本
# 2026Q1（数据内最近的完整季度）两只股票日收益
q1 <- d_stk >= as.Date("2026-01-01") & d_stk <= as.Date("2026-03-31")
abret <- data.frame(
  date = format(d_stk[q1], "%Y-%m-%d"),
  A = round(as.numeric(r_stk[q1, "600519.SS"]), 6),
  B = round(as.numeric(r_stk[q1, "601398.SS"]), 6),
  stringsAsFactors = FALSE
)
abret <- abret[complete.cases(abret), ]
save_csv(abret, "abret")

# ---------------------------------------------------------------- 3. ashr 单变量分布
# 96 股 × 全部季度收益率（长表），n≈700
qa <- qtr_ret(r_stk, d_stk)
qa$name <- nm(qa$code)
ashr <- data.frame(symbol = qa$code, name = qa$name, qtr = qa$qtr,
                   ret = round(qa$ret, 6), stringsAsFactors = FALSE)
ashr <- ashr[is.finite(ashr$ret), ]
save_csv(ashr, "ashr")

# ---------------------------------------------------------------- 4. indret 多组比较
# 20 个行业 ETF 的月度收益（长表）
qm <- mon_ret(r_ind, d_ind)
indret <- data.frame(symbol = qm$code, sector = nm(qm$code), month = qm$month,
                     ret = round(qm$ret, 6), stringsAsFactors = FALSE)
indret <- indret[is.finite(indret$ret), ]
save_csv(indret, "indret")

# ---------------------------------------------------------------- 4b. indday
# 20 个行业 ETF 的日收益率（宽表），用于协方差矩阵与组合优化
# 列名为 ETF 名称，便于正文直接引用；日期列在最前
ind_names <- nm(colnames(r_ind))
r_ind_named <- r_ind
colnames(r_ind_named) <- ifelse(is.na(ind_names), colnames(r_ind), ind_names)
indday <- data.frame(date = format(d_ind, "%Y-%m-%d"),
                     as.data.frame(round(r_ind_named, 6), check.names = FALSE),
                     stringsAsFactors = FALSE, check.names = FALSE)
save_csv(indday, "indday")

# ---------------------------------------------------------------- 5. pair2 两组比较
# 银行ETF vs 半导体ETF：低波动价值型 vs 高波动成长型的典型对照
pm <- mon_ret(r_ind[, c("512800.SS", "512480.SS"), drop = FALSE], d_ind)
p2 <- data.frame(
  month = pm$month,
  bank = round(pm$ret[pm$code == "512800.SS"], 6),
  semi = round(pm$ret[pm$code == "512480.SS"], 6),
  stringsAsFactors = FALSE
)
p2 <- p2[complete.cases(p2), ]
save_csv(p2, "pair2")

# ---------------------------------------------------------------- 6. idxret 宽基指数
idxret <- data.frame(date = format(d_idx, "%Y-%m-%d"),
                     as.data.frame(round(r_idx, 6), check.names = FALSE),
                     stringsAsFactors = FALSE, check.names = FALSE)
save_csv(idxret, "idxret")

# ---------------------------------------------------------------- 7. vol3 三种波动率估计
# 个股 OHLC → 收盘价法 / Parkinson / Garman-Klass（最近 60 个交易日）
vol_est <- function(cd) {
  s <- stk[stk$code == cd, ]
  s <- tail(s, 60)
  if (nrow(s) < 40) return(NULL)
  hl <- log(s$high / s$low); co <- log(s$close / s$open)
  cc <- c(NA, diff(log(s$adjusted)))
  data.frame(code = cd, name = nm(cd),
             close_to_close = round(sd(cc, na.rm = TRUE) * sqrt(252), 4),
             parkinson = round(sqrt(mean(hl^2) / (4 * log(2))) * sqrt(252), 4),
             garman_klass = round(sqrt(mean(0.5 * hl^2 - (2 * log(2) - 1) * co^2)) * sqrt(252), 4),
             stringsAsFactors = FALSE)
}
vol3 <- do.call(rbind, lapply(unique(stk$code), vol_est))
vol3 <- vol3[complete.cases(vol3), ]
save_csv(vol3, "vol3")

# ---------------------------------------------------------------- 8. excess 超额收益
# 个股月度收益 − 沪深300ETF 月度收益
bench <- mon_ret(r_idx[, "510300.SS", drop = FALSE], d_idx)
bench$ret <- bench$ret
sm <- mon_ret(r_stk, d_stk)
pick <- c("600519.SS", "601398.SS", "300750.SZ", "601318.SS")
ex <- sm[sm$code %in% pick, ]
ex$bench <- bench$ret[match(ex$month, bench$month)]
excess <- data.frame(symbol = ex$code, name = nm(ex$code), month = ex$month,
                     excess = round(ex$ret - ex$bench, 6), stringsAsFactors = FALSE)
excess <- excess[is.finite(excess$excess), ]
save_csv(excess, "excess")

# ---------------------------------------------------------------- 9. fin 财务横截面
f <- read.csv(gzfile(file.path(db, "data", "financial_2026", "indicators",
              "main_financial_indicators_2026_q1_q2.csv.gz")), stringsAsFactors = FALSE)
keep <- c("SECUCODE", "SECURITY_NAME_ABBR", "REPORT_DATE", "ROEJQ", "ZCFZL",
          "XSMLL", "TOTALOPERATEREVETZ", "PARENTNETPROFITTZ", "EPSJB", "BPS",
          "TOTALOPERATEREVE", "PARENTNETPROFIT")
fin <- f[, intersect(keep, names(f))]
fin <- fin[fin$REPORT_DATE == "2026-06-30", ]
fin <- data.frame(
  code = fin$SECUCODE, name = fin$SECURITY_NAME_ABBR,
  roe = fin$ROEJQ, debt_ratio = fin$ZCFZL, gross_margin = fin$XSMLL,
  rev_growth = fin$TOTALOPERATEREVETZ, profit_growth = fin$PARENTNETPROFITTZ,
  eps = fin$EPSJB, bps = fin$BPS,
  revenue = fin$TOTALOPERATEREVE, net_profit = fin$PARENTNETPROFIT,
  stringsAsFactors = FALSE
)
num <- c("roe", "debt_ratio", "gross_margin", "rev_growth", "profit_growth",
         "eps", "bps", "revenue", "net_profit")
fin[num] <- lapply(fin[num], as.numeric)
# 量纲说明：ROEJQ 与 ZCFZL 均为百分数（40.8 表示 40.8%）
# 剔除缺失与极端值，保留教学可读的区间
fin <- fin[complete.cases(fin[, c("roe", "debt_ratio")]), ]
fin <- fin[fin$debt_ratio > 0 & fin$debt_ratio < 150, ]
fin <- fin[fin$roe > -100 & fin$roe < 100, ]
fin <- fin[!is.na(fin$revenue) & fin$revenue > 0, ]
save_csv(fin, "fin")

# ---------------------------------------------------------------- 清单
snap <- data.frame(
  dataset = c("px1", "abret", "ashr", "indret", "indday", "pair2", "idxret", "vol3", "excess", "fin"),
  role = c("时间序列", "配对 / 两样本", "单变量分布", "多组比较（方差分析）",
           "组合优化（日频宽表）", "两组比较", "宽基指数面板", "三种测量方法比较",
           "超额收益", "财务横截面回归"),
  rows = c(nrow(px1), nrow(abret), nrow(ashr), nrow(indret), nrow(indday), nrow(p2),
           nrow(idxret), nrow(vol3), nrow(excess), nrow(fin)),
  source = c(rep("xquant-db 真实行情", 9), "xquant-db 财务指标 2026Q2"),
  stringsAsFactors = FALSE
)
write.csv(snap, file.path(csv_dir, "_datasets.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 名称映射（含校正记录）
write.csv(name_map, file.path(csv_dir, "_names.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("\n完成：\n"); print(snap, row.names = FALSE)
cat("\n样本期: 行情", format(min(d_stk)), "至", format(max(d_stk)),
    "（", nrow(r_stk), "个交易日 ）\n")
