#!/usr/bin/env Rscript
# 《量化投资的统计学导论》第 8 章插图
# 用法: cd an-introduction-to-statistics-for-quantitative-investing
#       Rscript script/04_fig_ch08.R

root <- Sys.getenv("BOOK_ROOT", unset = ".")
if (!dir.exists(file.path(root, "data"))) root <- "."
source(file.path(root, "data", "finwr.R"))
suppressMessages(library(quadprog))

fig_dir <- file.path(root, "media", "fig")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

FAM <- "PingFang SC"
C_ASSET <- "#8A8A85"
C_EQ    <- "#5F5E5A"
C_MV    <- "#0F6E56"
C_MS    <- "#A32D2D"
C_FRONT <- "#185FA5"
C_GRID  <- "grey92"

RF <- 0.015

png_ <- function(name, w = 1300, h = 860) {
  png(file.path(fig_dir, name), width = w, height = h, res = 150,
      type = "cairo", family = FAM)
}

# ---- 数据与参数 ----
R <- as.matrix(indday[, -1])
R <- R[, colSums(is.na(R)) < 0.05 * nrow(R)]
R <- R[complete.cases(R), ]
n <- ncol(R); mu <- colMeans(R) * 252; S <- cov(R) * 252

qp <- function(mu, S, short = FALSE, target = NULL) {
  n <- ncol(S); A <- matrix(rep(1, n), 1, n); b <- 1; meq <- 1
  if (!short) { A <- rbind(A, diag(n)); b <- c(b, rep(0, n)) }
  if (!is.null(target)) { A <- rbind(A, mu); b <- c(b, target); meq <- 2 }
  tryCatch(solve.QP(2 * S, rep(0, n), t(A), b, meq = meq)$solution,
           error = function(e) NULL)
}
vsd <- function(w, S) sqrt(as.numeric(crossprod(w, S %*% w)))

w_eq <- rep(1 / n, n); names(w_eq) <- colnames(R)
w_mv <- qp(mu, S); names(w_mv) <- colnames(R)
tg <- seq(sum(w_mv * mu), max(mu), length.out = 200)
front <- do.call(rbind, lapply(tg, function(m) {
  w <- qp(mu, S, target = m); if (is.null(w)) return(NULL)
  data.frame(ret = sum(w * mu), sd = vsd(w, S))
}))
front$sharpe <- (front$ret - RF) / front$sd
bt <- front$ret[1]
bt <- tg[which.max(front$sharpe)]
w_ms <- qp(mu, S, target = bt); names(w_ms) <- colnames(R)

# ---- 图 8.1 风险—收益平面 ----
png_("fig-8-2.png")
par(mar = c(4.4, 4.6, 3.0, 1.4), family = FAM)
plot(NA, xlim = c(0, 0.42), ylim = c(-0.12, 0.50),
     xlab = "年化波动率", ylab = "年化收益率", las = 1,
     main = "17 个行业 ETF 的风险—收益分布与有效前沿")
grid(col = C_GRID, lty = 1)
points(sqrt(diag(S)), mu, pch = 19, col = C_ASSET, cex = 0.95)
text(sqrt(diag(S)), mu, colnames(R), pos = 4, cex = 0.62, col = "#4A4A47", xpd = NA)
lines(front$sd, front$ret, col = C_FRONT, lwd = 2.4)
p <- list(list(w_eq, C_EQ, "等权", 2), list(w_mv, C_MV, "最小方差", 4),
          list(w_ms, C_MS, "最大夏普", 4))
for (x in p) {
  points(vsd(x[[1]], S), sum(x[[1]] * mu), pch = 21, bg = x[[2]], col = "white",
         cex = 2.6, lwd = 1.4)
  text(vsd(x[[1]], S), sum(x[[1]] * mu), x[[3]], pos = x[[4]], cex = 0.92,
       col = x[[2]], font = 2)
}
legend("topleft", c("单个行业 ETF", "有效前沿", "等权 / 最小方差 / 最大夏普"),
       pch = c(19, NA, 21), lty = c(NA, 1, NA), lwd = c(NA, 2.4, NA),
       pt.bg = c(NA, NA, C_MS), col = c(C_ASSET, C_FRONT, C_MS),
       bty = "n", cex = 0.86, pt.cex = c(1, 1, 1.8))
dev.off()

# ---- 图 8.2 两资产组合曲线 ----
i1 <- which(colnames(R) == "中证消费ETF"); i2 <- which(colnames(R) == "半导体ETF")
m2 <- mu[c(i1, i2)]; S2 <- S[c(i1, i2), c(i1, i2)]
w <- seq(0, 1, 0.005)
sdv <- sqrt(w^2 * S2[1, 1] + (1 - w)^2 * S2[2, 2] + 2 * w * (1 - w) * S2[1, 2])
rv <- w * m2[1] + (1 - w) * m2[2]
wmin <- (S2[2, 2] - S2[1, 2]) / (S2[1, 1] + S2[2, 2] - 2 * S2[1, 2])

png_("fig-8-1.png")
par(mar = c(4.4, 4.6, 3.0, 1.4), family = FAM)
plot(sdv, rv, type = "l", lwd = 2.6, col = C_FRONT,
     xlab = "年化波动率", ylab = "年化收益率", las = 1,
     main = "两资产组合：中证消费ETF 与 半导体ETF",
     xlim = c(0.19, 0.40), ylim = c(-0.12, 0.48))
grid(col = C_GRID, lty = 1)
lines(sdv, rv, lwd = 2.6, col = C_FRONT)
points(c(sqrt(S2[1, 1]), sqrt(S2[2, 2])), m2, pch = 19, col = C_ASSET, cex = 1.6)
text(sqrt(S2[1, 1]), m2[1], "全仓消费", pos = 2, cex = 0.9, col = "#4A4A47")
text(sqrt(S2[2, 2]), m2[2], "全仓半导体", pos = 3, cex = 0.9, col = "#4A4A47")
sminv <- sqrt(wmin^2 * S2[1, 1] + (1 - wmin)^2 * S2[2, 2] + 2 * wmin * (1 - wmin) * S2[1, 2])
points(sminv, wmin * m2[1] + (1 - wmin) * m2[2], pch = 21, bg = C_MV, col = "white", cex = 2.6, lwd = 1.4)
text(sminv, wmin * m2[1] + (1 - wmin) * m2[2], "最小方差", pos = 3, cex = 0.92, col = C_MV, font = 2)
se <- sqrt(0.25 * S2[1, 1] + 0.25 * S2[2, 2] + 0.5 * S2[1, 2])
points(se, mean(m2), pch = 21, bg = C_EQ, col = "white", cex = 2.4, lwd = 1.4)
text(se, mean(m2), "等权", pos = 4, cex = 0.9, col = C_EQ, font = 2)
dev.off()

# ---- 图 8.3 权重对比 ----
keepn <- order(-pmax(w_mv, w_ms))
png_("fig-8-5.png", 1300, 820)
par(mar = c(4.2, 8.4, 3.0, 1.4), family = FAM)
M <- rbind(w_mv[keepn], w_ms[keepn]) * 100
colnames(M) <- colnames(R)[keepn]
barplot(M, beside = TRUE, horiz = TRUE, las = 1, cex.names = 0.82,
        col = c(C_MV, C_MS), border = NA,
        xlab = "权重（%）", main = "两种组合的持仓结构")
grid(col = C_GRID, lty = 1, nx = NA)
barplot(M, beside = TRUE, horiz = TRUE, las = 1, cex.names = 0.82,
        col = c(C_MV, C_MS), border = NA, add = TRUE, axes = FALSE)
legend("bottomright", c("最小方差（7 只）", "最大夏普（3 只）"),
       fill = c(C_MV, C_MS), border = NA, bty = "n", cex = 0.88)
dev.off()

# ---- 图 8.4 约束的影响 ----
ws <- qp(mu, S, short = TRUE)
tgs <- seq(sum(ws * mu), max(mu), length.out = 200)
fs <- do.call(rbind, lapply(tgs, function(m) {
  w <- qp(mu, S, short = TRUE, target = m); if (is.null(w)) return(NULL)
  data.frame(ret = sum(w * mu), sd = vsd(w, S))
}))
png_("fig-8-3.png")
par(mar = c(4.4, 4.6, 3.0, 1.4), family = FAM)
plot(NA, xlim = c(0.15, 0.42), ylim = c(-0.02, 0.50),
     xlab = "年化波动率", ylab = "年化收益率", las = 1,
     main = "允许做空如何改变可行域")
grid(col = C_GRID, lty = 1)
lines(front$sd, front$ret, lwd = 2.6, col = C_FRONT)
lines(fs$sd, fs$ret, lwd = 2.0, col = C_MS, lty = 2)
points(vsd(w_mv, S), sum(w_mv * mu), pch = 21, bg = C_MV, col = "white", cex = 2.4, lwd = 1.4)
points(vsd(ws, S), sum(ws * mu), pch = 21, bg = C_MS, col = "white", cex = 2.4, lwd = 1.4)
legend("bottomright", c("不允许做空（长仓）", "允许做空"), lty = c(1, 2), lwd = c(2.6, 2.0),
       col = c(C_FRONT, C_MS), bty = "n", cex = 0.88)
mtext("做空版最低波动更低，但需要 117% 的空头敞口", side = 3, line = 0.1, cex = 0.8, col = "#5F5E5A")
dev.off()

# ---- 图 8.5 样本外检验 ----
h1 <- 1:250; R1 <- R[h1, ]; R2 <- R[-h1, ]
mu1 <- colMeans(R1) * 252; S1 <- cov(R1) * 252
wmv1 <- qp(mu1, S1); names(wmv1) <- colnames(R)
tg1 <- seq(sum(wmv1 * mu1), max(mu1), length.out = 150)
f1 <- do.call(rbind, lapply(tg1, function(m) {
  w <- qp(mu1, S1, target = m); if (is.null(w)) return(NULL)
  data.frame(ret = sum(w * mu1), sd = vsd(w, S1))
}))
f1$s <- (f1$ret - RF) / f1$sd
wms1 <- qp(mu1, S1, target = tg1[which.max(f1$s)])

cum <- function(w) cumprod(1 + as.numeric(R2 %*% w))
png_("fig-8-4.png")
par(mar = c(4.4, 4.8, 3.0, 1.4), family = FAM)
plot(NA, xlim = c(1, nrow(R2)), ylim = c(0.8, 2.1),
     xlab = "后半段交易日序号", ylab = "累计净值（起点 = 1）", las = 1,
     main = "用前半段估权重，在后半段检验")
grid(col = C_GRID, lty = 1)
lines(cum(wms1), col = C_MS, lwd = 2.2)
lines(cum(w_eq), col = C_EQ, lwd = 2.2, lty = 2)
lines(cum(wmv1), col = C_MV, lwd = 2.2, lty = 3)
legend("topleft", c("最大夏普（前期估计）", "等权", "最小方差（前期估计）"),
       col = c(C_MS, C_EQ, C_MV), lty = c(1, 2, 3), lwd = 2.2, bty = "n", cex = 0.86)
dev.off()

cat("第 8 章插图完成：", length(list.files(fig_dir, "^fig-8-")), "幅\n")
