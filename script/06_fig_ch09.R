#!/usr/bin/env Rscript
# 《量化投资的统计学导论》第 9 章插图
# 运行: cd an-introduction-to-statistics-for-quantitative-investing
#       Rscript script/06_fig_ch09.R   （约 8-12 分钟，需跑一次 RF 与 XGB）

root <- Sys.getenv("BOOK_ROOT", unset = ".")
if (!dir.exists(file.path(root, "data"))) root <- "."
source(file.path(root, "data", "load_datasets.R"))
suppressMessages({library(randomForest); library(xgboost); library(MASS)})
set.seed(42)

fig_dir <- file.path(root, "media", "fig")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
FAM <- "PingFang SC"
C1 <- "#185FA5"; C2 <- "#A32D2D"; C3 <- "#0F6E56"; C4 <- "#BA7517"; C5 <- "#8A8A85"
GRID <- "grey92"

png_ <- function(n, w = 1300, h = 820)
  png(file.path(fig_dir, n), width = w, height = h, res = 150,
      type = "cairo", family = FAM)

FEATS <- c("mom_12_1", "mom_6_1", "rev_1m", "vol_12m", "turnover", "size")
d <- mlpanel[, c("secucode", "ym", "fwd_ret", FEATS)]
d <- d[complete.cases(d), ]
for (f in FEATS) {
  d[[f]] <- ave(d[[f]], d$ym, FUN = function(x) {
    q <- quantile(x, c(.01, .99), na.rm = TRUE)
    as.numeric(scale(pmin(pmax(x, q[1]), q[2])))
  })
}
d$fwd_ret <- ave(d$fwd_ret, d$ym, FUN = function(x) {
  q <- quantile(x, c(.01, .99), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
})

TR <- d$ym <= "2024-12"; TE <- d$ym >= "2025-01"
ic <- function(dat, pred) {
  v <- sapply(split(dat, dat$ym), function(x) {
    if (nrow(x) < 50) return(NA_real_)
    cor(x[[pred]], x$fwd_ret, method = "spearman")
  })
  v[!is.na(v)]
}

# ---- 图 9.1 单因子 IC 时序（选三个代表性因子） ----
pick <- c("rev_1m", "size", "mom_12_1")
lab  <- c("反转（取负）", "小市值（取负）", "动量（取负）")
ics <- sapply(pick, function(f) {
  d$.p <- -d[[f]]; ic(d[TE, ], ".p")
})
png_("fig-9-1.png", 1300, 760)
par(mar = c(4.2, 4.6, 2.8, 1.2), family = FAM)
matplot(as.Date(paste0(names(ics[, 1]), "-01")), ics, type = "b", pch = 19,
        lty = 1, lwd = 1.6, col = c(C1, C2, C3), cex = 0.75,
        xlab = "月份", ylab = "月度 IC（Spearman）", las = 1,
        main = "样本外 21 个月：三个因子的 IC 波动")
abline(h = 0, col = GRID, lwd = 2)
grid(col = GRID, lty = 1)
matlines(as.Date(paste0(rownames(ics), "-01")), ics, type = "b", pch = 19,
         lty = 1, lwd = 1.6, col = c(C1, C2, C3), cex = 0.75)
legend("bottomleft", sprintf("%s  均值 %.3f", lab, colMeans(ics)),
       col = c(C1, C2, C3), lty = 1, lwd = 1.8, bty = "n", cex = 0.85)
dev.off()

# ---- 训练模型（供 9.2-9.5 使用） ----
fml <- as.formula(paste("fwd_ret ~", paste(FEATS, collapse = " + ")))
d$.ols <- predict(lm(fml, data = d[TR, ]), d)

sub <- sample(which(TR), min(100000, sum(TR)))
rf <- randomForest(x = as.matrix(d[sub, FEATS]), y = d$fwd_ret[sub],
                   ntree = 120, mtry = 3, nodesize = 100, importance = TRUE)
d$.rf <- predict(rf, as.matrix(d[, FEATS]))

dtr <- xgb.DMatrix(as.matrix(d[TR, FEATS]), label = d$fwd_ret[TR])
set.seed(42)
xb <- xgb.train(list(eta = .05, max_depth = 4, subsample = .8,
                     colsample_bytree = .8, objective = "reg:squarederror",
                     nthread = 2), dtr, nrounds = 150, verbose = 0)
d$.xgb <- NA
d$.xgb[TE] <- predict(xb, xgb.DMatrix(as.matrix(d[TE, FEATS])))

# ---- 图 9.2 十分位单调性 ----
mono <- function(dat, pred) {
  m <- sapply(split(dat, dat$ym), function(x) {
    x <- x[!is.na(x[[pred]]), ]
    if (nrow(x) < 100) return(rep(NA_real_, 10))
    q <- cut(rank(x[[pred]]), 10, labels = FALSE)
    tapply(x$fwd_ret, q, mean)
  })
  rowMeans(m, na.rm = TRUE)
}
M <- rbind(OLS = mono(d[TE, ], ".ols"), 随机森林 = mono(d[TE, ], ".rf"),
           XGBoost = mono(d[TE, ], ".xgb")) * 100
png_("fig-9-3.png")
par(mar = c(4.2, 4.8, 2.8, 1.2), family = FAM)
bp <- barplot(M, beside = TRUE, col = c(C1, C4, C2), border = NA,
              xlab = "按模型打分排序的十分位（10 = 预期收益最高）",
              ylab = "下月平均收益（%）", las = 1, cex.names = 0.86,
              main = "样本外十分位收益：预测能力是否单调")
abline(h = 0, col = "grey40", lwd = 1)
grid(col = GRID, lty = 1, ny = NA)
barplot(M, beside = TRUE, col = c(C1, C4, C2), border = NA, add = TRUE, axes = FALSE)
legend("topleft", rownames(M), fill = c(C1, C4, C2), border = NA,
       bty = "n", cex = 0.86)
dev.off()

# ---- 图 9.3 过拟合：样本内 vs 样本外 ----
models <- c("OLS", "随机森林", "XGBoost")
ins <- c(mean(ic(d[TR, ], ".ols")), mean(ic(d[TR, ], ".rf")), NA)
oos <- c(mean(ic(d[TE, ], ".ols")), mean(ic(d[TE, ], ".rf")), mean(ic(d[TE, ], ".xgb")))
png_("fig-9-2.png", 1200, 780)
par(mar = c(4.2, 4.8, 2.8, 1.2), family = FAM)
Mm <- rbind(样本内 = ins, 样本外 = oos)
colnames(Mm) <- models
barplot(Mm, beside = TRUE, col = c(C1, C2), border = NA, ylim = c(0, 0.52),
        ylab = "平均 IC", las = 1, cex.names = 0.92,
        main = "样本内与样本外的 IC 差距")
grid(col = GRID, lty = 1, ny = NA)
barplot(Mm, beside = TRUE, col = c(C1, C2), border = NA, add = TRUE, axes = FALSE)
text(2.5, 0.45, "随机森林样本内 0.40 → 样本外 0.09", cex = 0.82, col = C2)
legend("topleft", c("样本内（2021-2024）", "样本外（2025-2026）"),
       fill = c(C1, C2), border = NA, bty = "n", cex = 0.86)
dev.off()

# ---- 图 9.4 训练窗口长度 vs 过拟合 ----
ks <- c(6, 12, 24, 48)
gap <- t(sapply(ks, function(k) {
  mo <- sort(unique(d$ym[TR]))[1:k]
  idx <- d$ym %in% mo
  if (sum(idx) < 5000) return(c(NA, NA))
  s <- sample(which(idx), min(60000, sum(idx)))
  m <- randomForest(x = as.matrix(d[s, FEATS]), y = d$fwd_ret[s],
                    ntree = 80, mtry = 3, nodesize = 100)
  d$.t <- predict(m, as.matrix(d[, FEATS]))
  c(mean(ic(d[idx, ], ".t")), mean(ic(d[TE, ], ".t")))
}))
png_("fig-9-5.png", 1200, 780)
par(mar = c(4.4, 4.8, 2.8, 1.4), family = FAM)
matplot(ks, gap, type = c("b", "b"), pch = c(19, 17), lty = 1, lwd = 2.2,
        col = c(C1, C2), cex = 1.1, xaxt = "n",
        xlab = "训练窗口长度（月）", ylab = "平均 IC", las = 1, ylim = c(0, 0.7),
        main = "训练集越小，样本内越好看、样本外越差")
axis(1, at = ks)
grid(col = GRID, lty = 1)
matlines(ks, gap, type = "b", pch = c(19, 17), lty = 1, lwd = 2.2,
         col = c(C1, C2), cex = 1.1)
text(ks, gap[, 1], sprintf("%.2f", gap[, 1]), pos = 4, cex = 0.8, col = C1)
text(ks, gap[, 2], sprintf("%.2f", gap[, 2]), pos = 4, cex = 0.8, col = C2)
legend("topright", c("样本内 IC", "样本外 IC"), col = c(C1, C2), pch = c(19, 17),
       lty = 1, lwd = 2.2, bty = "n", cex = 0.88)
dev.off()

# ---- 图 9.5 十分位多空累计净值 ----
nav <- function(pred, lab) {
  r <- sapply(split(d[TE, ], d[TE, ]$ym), function(x) {
    x <- x[!is.na(x[[pred]]), ]
    if (nrow(x) < 100) return(NA_real_)
    q <- cut(rank(x[[pred]]), 10, labels = FALSE)
    mean(x$fwd_ret[q == 10]) - mean(x$fwd_ret[q == 1])
  })
  cumprod(1 + r[!is.na(r)])
}
png_("fig-9-4.png")
par(mar = c(4.2, 4.8, 2.8, 1.2), family = FAM)
plot(NA, xlim = c(1, 21), ylim = c(0.9, 2.6), xlab = "样本外月份序号",
     ylab = "多空组合累计净值（起点 = 1）", las = 1,
     main = "十分位多空组合：样本外 21 个月")
grid(col = GRID, lty = 1)
lines(nav(".ols", "OLS"), col = C1, lwd = 2.2)
lines(nav(".rf", "RF"), col = C4, lwd = 2.2, lty = 2)
lines(nav(".xgb", "XGB"), col = C2, lwd = 2.4)
legend("topleft", c("OLS", "随机森林", "XGBoost"), col = c(C1, C4, C2),
       lty = c(1, 2, 1), lwd = 2.2, bty = "n", cex = 0.88)
dev.off()

cat("第 9 章插图完成：", length(list.files(fig_dir, "^fig-9-")), "幅\n")
print(round(rbind(样本内 = ins, 样本外 = oos), 4))
print(round(gap, 3))
