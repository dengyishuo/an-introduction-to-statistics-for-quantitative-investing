#!/usr/bin/env Rscript
# 《量化投资的统计学导论》多因子面板构建
# 数据源: xquant-db 的本地 MySQL 库 strategy_monitor（只读）
#   raw_price                        全市场日行情（5244 只，2017-01 至 2026-10）
#   a_share_industry_cap             行业 + 股本 + 市值快照（2026-09-30）
#   a_share_main_financial_indicator 12 期财务指标（JSON payload，含公告日）
#
# 输出: data/csv/mlpanel.csv（股票×月 面板，含价格类因子与未来收益）
#       data/csv/mlfin.csv  （股票×报告期 财务因子，按公告日可用）
#
# 用法: cd an-introduction-to-statistics-for-quantitative-investing
#       Rscript script/05_build_mlpanel.R

suppressMessages({
  library(DBI); library(RMySQL); library(jsonlite)
})

root <- Sys.getenv("BOOK_ROOT", unset = ".")
if (!dir.exists(file.path(root, "data"))) root <- "."
csv_dir <- file.path(root, "data", "csv")
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

db <- Sys.getenv("XQUANT_DB", unset = file.path(root, "..", "..", "xquant-db"))
if (!dir.exists(db)) db <- file.path("..", "..", "xquant-db")

# .Renviron 由 xquant-db 维护（含数据库口令，绝不入库）；用 readRenviron 正确解析引号
renviron <- file.path(db, ".Renviron")
if (file.exists(renviron)) readRenviron(renviron)

con <- dbConnect(MySQL(),
                 host = Sys.getenv("DB_HOST", "127.0.0.1"),
                 port = as.integer(Sys.getenv("DB_PORT", "3306")),
                 user = Sys.getenv("DB_USER"),
                 password = Sys.getenv("DB_PASSWORD"),
                 dbname = Sys.getenv("DB_NAME", "strategy_monitor"))
on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
cat("已连接 strategy_monitor（只读）\n")

# ---------------------------------------------------------------- 1. 月度行情
FROM_Y <- 2019; TO_Y <- 2026
mon <- list()
for (y in FROM_Y:TO_Y) {
  sql <- sprintf(
    "SELECT symbol, DATE_FORMAT(date,'%%Y-%%m') AS ym,
            SUBSTRING_INDEX(GROUP_CONCAT(adjusted ORDER BY date DESC), ',', 1) AS last_adj,
            SUBSTRING_INDEX(GROUP_CONCAT(close    ORDER BY date DESC), ',', 1) AS last_close,
            SUM(volume) AS vol_sum, COUNT(*) AS ndays
     FROM raw_price WHERE date >= '%d-01-01' AND date < '%d-01-01'
     GROUP BY symbol, ym", y, y + 1)
  d <- dbGetQuery(con, sql)
  d$last_adj <- as.numeric(d$last_adj); d$last_close <- as.numeric(d$last_close)
  d$vol_sum  <- as.numeric(d$vol_sum)
  mon[[as.character(y)]] <- d
  cat(sprintf("  %d 年: %5d 行, %4d 只\n", y, nrow(d), length(unique(d$symbol))))
}
px <- do.call(rbind, mon)
rm(mon)
px <- px[!is.na(px$last_adj) & px$last_adj > 0, ]

# 代码格式统一：raw_price 用 .SS/.SZ/.BJ，industry_cap 用 .SH/.SZ/.BJ
px$secucode <- sub("\\.SS$", ".SH", px$symbol)

# ---------------------------------------------------------------- 2. 行业与股本
cap <- dbGetQuery(con,
  "SELECT secucode, name, industry, float_shares, float_mv, close, pe_ttm
   FROM a_share_industry_cap")
cap$float_shares <- as.numeric(cap$float_shares)
cap$float_mv <- as.numeric(cap$float_mv)
cap$pe_ttm <- as.numeric(cap$pe_ttm)
cap <- cap[!is.na(cap$industry) & cap$industry != "" & cap$float_shares > 0, ]
cat("行业快照:", nrow(cap), "只,", length(unique(cap$industry)), "个行业\n")

# ---------------------------------------------------------------- 3. 财务因子
fin_raw <- dbGetQuery(con,
  "SELECT secucode, report_date, notice_date, payload
   FROM a_share_main_financial_indicator")
cat("财务原始:", nrow(fin_raw), "行\n")

keep_fields <- c("ROEJQ", "ZCFZL", "XSMLL", "TOTALOPERATEREVETZ",
                 "PARENTNETPROFITTZ", "EPSJB", "BPS", "LD", "CQBL")
parse_one <- function(txt) {
  j <- tryCatch(fromJSON(txt), error = function(e) NULL)
  if (is.null(j)) return(rep(NA_real_, length(keep_fields)))
  vapply(keep_fields, function(k) {
    v <- j[[k]]
    if (is.null(v) || length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v[1]))
  }, numeric(1))
}
fin <- fin_raw[, c("secucode", "report_date", "notice_date")]
fin[keep_fields] <- t(vapply(fin_raw$payload, parse_one, numeric(length(keep_fields))))
fin$report_date <- as.Date(fin$report_date)
fin$notice_date <- as.Date(fin$notice_date)
fin <- fin[!is.na(fin$notice_date), ]
names(fin)[names(fin) == "ROEJQ"] <- "roe"
names(fin)[names(fin) == "ZCFZL"] <- "debt_ratio"
names(fin)[names(fin) == "XSMLL"] <- "gross_margin"
names(fin)[names(fin) == "TOTALOPERATEREVETZ"] <- "rev_growth"
names(fin)[names(fin) == "PARENTNETPROFITTZ"] <- "profit_growth"
names(fin)[names(fin) == "EPSJB"] <- "eps"
names(fin)[names(fin) == "BPS"] <- "bps"
names(fin)[names(fin) == "LD"] <- "current_ratio"
names(fin)[names(fin) == "CQBL"] <- "equity_mult"
fin <- fin[order(fin$secucode, fin$notice_date), ]
write.csv(fin[, c("secucode", "report_date", "notice_date", "roe", "debt_ratio",
                  "gross_margin", "rev_growth", "profit_growth", "eps", "bps",
                  "current_ratio", "equity_mult")],
          gzfile(file.path(csv_dir, "mlfin.csv.gz"), "w"), row.names = FALSE, fileEncoding = "UTF-8")
cat("财务因子:", nrow(fin), "行,", length(unique(fin$report_date)), "期\n")

# ---------------------------------------------------------------- 4. 面板组装
px$ym <- px$ym
px <- px[order(px$secucode, px$ym), ]

# 只保留在行业快照里、且名称不含 ST 的个股
cap_ok <- cap[!grepl("ST|退", cap$name), c("secucode", "name", "industry",
                                           "float_shares", "float_mv", "pe_ttm")]
px <- merge(px, cap_ok, by = "secucode")
cat("并入行业后:", nrow(px), "行,", length(unique(px$secucode)), "只\n")

# 月度收益
px <- px[order(px$secucode, px$ym), ]
px$ret <- ave(px$last_adj, px$secucode, FUN = function(x) c(NA, diff(log(x))))

# 未来一个月收益（标签）
px$fwd_ret <- ave(px$ret, px$secucode, FUN = function(x) c(x[-1], NA))

# 滚动因子：动量、反转、波动
roll <- function(x, k, fn) {
  n <- length(x)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    lo <- i - k + 1
    if (lo < 1) next
    out[i] <- fn(x[lo:i])
  }
  out
}
comp_mom <- function(x, skip) {
  n <- length(x)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i - skip < 1) next
    seg <- x[max(1, i - 11):(i - skip)]
    if (length(seg) >= 6 && all(is.finite(seg))) out[i] <- prod(1 + seg) - 1
  }
  out
}
px <- do.call(rbind, lapply(split(px, px$secucode), function(d) {
  d <- d[order(d$ym), ]
  d$mom_12_1 <- comp_mom(d$ret, 1)
  d$mom_6_1  <- {
    n <- nrow(d); out <- rep(NA_real_, n)
    for (i in seq_len(n)) {
      if (i - 1 < 1) next
      seg <- d$ret[max(1, i - 5):(i - 1)]
      if (length(seg) >= 4 && all(is.finite(seg))) out[i] <- prod(1 + seg) - 1
    }
    out
  }
  d$rev_1m <- d$ret
  d$vol_12m <- roll(d$ret, 12, function(z) if (all(is.finite(z))) sd(z) else NA_real_)
  d$turnover <- log1p(d$vol_sum / d$float_shares)
  d$size <- log(d$float_shares * d$last_close)
  d
}))

panel <- px[, c("secucode", "name", "industry", "ym", "last_close", "ret",
                "fwd_ret", "mom_12_1", "mom_6_1", "rev_1m", "vol_12m",
                "turnover", "size", "pe_ttm")]
panel <- panel[order(panel$ym, panel$secucode), ]

# 样本期与最小覆盖要求
panel <- panel[panel$ym >= "2021-01" & panel$ym <= "2026-09", ]
cnt <- table(panel$secucode)
panel <- panel[panel$secucode %in% names(cnt)[cnt >= 60], ]
cat("覆盖 ≥60 个月:", length(unique(panel$secucode)), "只\n")

gz <- gzfile(file.path(csv_dir, "mlpanel.csv.gz"), "w")
write.csv(panel, gz, row.names = FALSE, fileEncoding = "UTF-8")
close(gz)

cat("\n完成:\n")
cat("  mlpanel:", nrow(panel), "行,", length(unique(panel$secucode)), "只,",
    length(unique(panel$ym)), "个月\n")
cat("  月份范围:", min(panel$ym), "至", max(panel$ym), "\n")
cat("  行业数:", length(unique(panel$industry)), "\n")
cat("  完整行（含标签）:", sum(complete.cases(panel[, c("fwd_ret", "mom_12_1",
    "vol_12m", "turnover", "size")])), "\n")
print(dbGetQuery(con, "SELECT 1 AS ok"))
