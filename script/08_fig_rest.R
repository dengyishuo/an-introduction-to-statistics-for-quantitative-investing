#!/usr/bin/env Rscript
# 《量化投资的统计学导论》第 2–7、10–16 章插图
# 运行: cd an-introduction-to-statistics-for-quantitative-investing
#       Rscript script/08_fig_rest.R

root <- Sys.getenv("BOOK_ROOT", unset = ".")
if (!dir.exists(file.path(root, "data"))) root <- "."
source(file.path(root, "data", "finwr.R"))
suppressMessages(library(survival))

fig_dir <- file.path(root, "media", "fig")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
FAM <- "PingFang SC"
BL <- "#185FA5"; RD <- "#A32D2D"; GR <- "#0F6E56"; AM <- "#BA7517"
GY <- "#8A8A85"; GRID <- "grey92"

png_ <- function(n, w = 1250, h = 800)
  png(file.path(fig_dir, n), width = w, height = h, res = 150,
      type = "cairo", family = FAM)

# ---------- 图 3.1 季度收益分布：直方图 + 正态 + 密度 ----------
png_("fig-3-1.png")
par(mar = c(4.2, 4.6, 2.8, 1.2), family = FAM)
x <- ashr$ret
hist(x, breaks = 60, probability = TRUE, col = "grey88", border = "white",
     xlab = "季度收益", ylab = "密度",
     main = "960 个季度收益：比正态更尖、尾部更厚")
grid(col = GRID, lty = 1)
hist(x, breaks = 60, probability = TRUE, col = "grey88", border = "white", add = TRUE)
lines(density(x), col = RD, lwd = 2.4)
curve(dnorm(x, mean(x), sd(x)), add = TRUE, col = BL, lty = 2, lwd = 2.4)
legend("topright", c("实际密度", "正态拟合"), col = c(RD, BL), lty = c(1, 2),
       lwd = 2.4, bty = "n", cex = 0.9)
dev.off()

# ---------- 图 3.2 正态 Q-Q 图 ----------
png_("fig-3-2.png", 1100, 780)
par(mar = c(4.2, 4.6, 2.8, 1.2), family = FAM)
qqnorm(x, pch = 19, cex = 0.6, col = rgb(0, 0, 0, 0.35),
       main = "Q-Q 图：两端上翘 = 右尾厚")
qqline(x, col = RD, lwd = 2.4)
grid(col = GRID, lty = 1)
dev.off()

# ---------- 图 4.1 按季度的平行箱线图 ----------
png_("fig-4-1.png", 1300, 780)
par(mar = c(4.6, 4.6, 2.8, 1.2), family = FAM)
boxplot(ret ~ qtr, data = ashr, las = 2, col = "grey92", border = GY,
        ylab = "季度收益", xlab = "", main = "10 个季度的收益分布")
means <- tapply(ashr$ret, ashr$qtr, mean)
points(1:length(means), means, pch = 18, cex = 1.7, col = RD)
legend("topleft", c("中位数（箱内粗线）", "均值（红菱形）"), pch = c(NA, 18),
       lty = c(1, NA), col = c(GY, RD), bty = "n", cex = 0.88)
grid(col = GRID, lty = 1, ny = NA)
dev.off()

# ---------- 图 5.1 银行 vs 半导体 月度收益 ----------
png_("fig-5-1.png", 1200, 780)
par(mar = c(4.2, 4.6, 2.8, 1.2), family = FAM)
boxplot(list(银行ETF = pair2$bank, 半导体ETF = pair2$semi), las = 1,
        col = c(BL, RD), border = "white", outline = TRUE,
        ylab = "月度收益", main = "两个行业 ETF 的月度收益（58 个月）")
grid(col = GRID, lty = 1, ny = NA)
abline(h = 0, lty = 2, col = GY)
dev.off()

# ---------- 图 6.1 ROE 与资产负债率 ----------
png_("fig-6-1.png")
par(mar = c(4.2, 4.6, 2.8, 1.2), family = FAM)
d <- fin[complete.cases(fin[, c("roe", "debt_ratio")]), ]
plot(d$debt_ratio, d$roe, pch = 19, cex = 0.35, col = rgb(0, 0, 0, 0.12),
     xlab = "资产负债率（%）", ylab = "ROE（%）",
     main = "5457 家公司：R² 只有 0.028")
abline(lm(roe ~ debt_ratio, data = d), col = RD, lwd = 2.6)
grid(col = GRID, lty = 1)
text(100, 60, sprintf("ROE = 5.03 - 0.073 × 负债率\nR² = 0.028"),
     cex = 0.86, col = RD)
dev.off()

# ---------- 图 7.1 方差分解 ----------
png_("fig-7-1.png", 1100, 720)
par(mar = c(4.2, 4.6, 2.8, 1.2), family = FAM)
ss <- c(组间 = 8.111, 组内 = 29.600)
bp <- barplot(ss, col = c(RD, GY), border = "white", las = 1,
              ylab = "平方和", main = "总变异的分解（F = 28.9）")
text(bp, ss, sprintf("%.2f\n(%.1f%%)", ss, ss / sum(ss) * 100), pos = 3, cex = 0.9)
dev.off()

# ---------- 图 10.1 市值五分位的下月收益 ----------
png_("fig-10-1.png", 1200, 760)
par(mar = c(4.2, 4.8, 2.8, 1.2), family = FAM)
d <- mlpanel[mlpanel$ym == "2026-06", ]
d$g <- cut(d$size, quantile(d$size, 0:5/5),
           labels = c("最小", "较小", "中等", "较大", "最大"), include.lowest = TRUE)
m <- tapply(d$fwd_ret, d$g, mean) * 100
bp <- barplot(m, col = colorRampPalette(c(RD, "grey80"))(5), border = "white",
              las = 1, ylab = "下月平均收益（%）",
              main = "按市值五分位：单调递减 15.5 个百分点")
abline(h = 0, col = GY)
text(bp, m, sprintf("%.2f", m), pos = ifelse(m > 0, 3, 1), cex = 0.88)
dev.off()

# ---------- 图 11.1 逐步加变量的 R² ----------
png_("fig-11-1.png", 1200, 740)
par(mar = c(4.2, 4.8, 2.8, 1.2), family = FAM)
r2 <- c(0.0406, 0.0712, 0.1368, 0.2407)
labs <- c("负债率", "+毛利率", "+营收增速", "+log(营收)")
bp <- barplot(r2, names.arg = labs, col = c(GY, GY, GY, RD), border = "white",
              las = 1, ylab = expression(R^2), ylim = c(0, 0.28),
              main = "逐步加变量：规模贡献最大")
text(bp, r2, sprintf("%.3f", r2), pos = 3, cex = 0.9)
dev.off()

# ---------- 图 12.1 多项式拟合 ----------
png_("fig-12-1.png")
par(mar = c(4.2, 4.6, 2.8, 1.2), family = FAM)
d <- scale[scale$ym == "2026-06", ]
d <- d[!is.na(d$turnover) & !is.na(d$size) & d$turnover > 0, ]
d$lsize <- d$size; d$lturn <- log(d$turnover)
plot(d$lsize, d$lturn, pch = 19, cex = 0.3, col = rgb(0, 0, 0, 0.1),
     xlab = "log(流通市值)", ylab = "log(换手率)",
     main = "换手率与市值的倒 U 形关系")
xs <- seq(min(d$lsize), max(d$lsize), length.out = 200)
lines(xs, predict(lm(lturn ~ lsize, d), data.frame(lsize = xs)), col = BL, lwd = 2.6)
lines(xs, predict(lm(lturn ~ lsize + I(lsize^2), d), data.frame(lsize = xs)),
      col = RD, lwd = 2.6)
vx <- 22.176
abline(v = vx, lty = 2, col = GR)
legend("topright", c("线性", "二次（顶点在 43 亿市值）"), col = c(BL, RD),
       lwd = 2.6, bty = "n", cex = 0.88)
dev.off()

# ---------- 图 13.1 逻辑回归拟合 ----------
png_("fig-13-1.png")
par(mar = c(4.2, 4.6, 2.8, 1.2), family = FAM)
d <- drawdown[complete.cases(drawdown[, c("event", "vol_12m")]), ]
m <- glm(event ~ vol_12m, d, family = binomial)
plot(d$vol_12m, jitter(d$event, 0.02), pch = 19, cex = 0.3,
     col = rgb(0, 0, 0, 0.05), xlab = "过去 12 个月波动率",
     ylab = "触发 30% 回撤的概率", main = "逻辑回归：S 形拟合")
xs <- seq(min(d$vol_12m), max(d$vol_12m), length.out = 200)
pr <- predict(m, data.frame(vol_12m = xs), type = "link", se.fit = TRUE)
lines(xs, plogis(pr$fit), col = RD, lwd = 2.8)
lines(xs, plogis(pr$fit - 1.96 * pr$se.fit), col = RD, lty = 2, lwd = 1.8)
lines(xs, plogis(pr$fit + 1.96 * pr$se.fit), col = RD, lty = 2, lwd = 1.8)
grid(col = GRID, lty = 1)
dev.off()

# ---------- 图 14.1 KM 曲线（按波动率分组） ----------
png_("fig-14-1.png")
par(mar = c(4.2, 4.6, 2.8, 1.2), family = FAM)
d <- drawdown
d$vq <- cut(d$vol_12m, quantile(d$vol_12m, c(0, 1/3, 2/3, 1), na.rm = TRUE),
            labels = c("低波动", "中波动", "高波动"), include.lowest = TRUE)
km <- survfit(Surv(months, event) ~ vq, data = d)
plot(km, col = c(BL, GR, RD), lty = 1, lwd = 2.4, xlim = c(0, 12),
     xlab = "月数", ylab = "未触发回撤的概率",
     main = "Kaplan-Meier 曲线：波动率越高，越早触雷")
grid(col = GRID, lty = 1)
legend("bottomleft", c("低波动", "中波动", "高波动"), col = c(BL, GR, RD),
       lwd = 2.4, bty = "n", cex = 0.9)
dev.off()

# ---------- 图 15.1 各行业的下跌日率 ----------
png_("fig-15-1.png", 1300, 780)
par(mar = c(4.4, 9.6, 2.8, 1.2), family = FAM)
agg <- aggregate(cbind(ndown, exposure) ~ sector, indcnt, sum)
agg$rate <- agg$ndown / agg$exposure
agg <- agg[order(agg$rate), ]
barplot(agg$rate, horiz = TRUE, names.arg = agg$sector, las = 1,
        col = colorRampPalette(c(BL, RD))(nrow(agg)), border = NA, cex.names = 0.72,
        xlab = "每日跌幅超 1% 的概率", main = "20 个行业的下跌频率（差 3.2 倍）")
grid(col = GRID, lty = 1, nx = NA)
dev.off()

# ---------- 图 16.1 分散化曲线与 nls 拟合 ----------
png_("fig-16-1.png")
par(mar = c(4.2, 4.6, 2.8, 1.2), family = FAM)
d <- divcurve
m <- nls(sd ~ s1 * sqrt(rho + (1 - rho) / n), d, start = list(s1 = 0.126, rho = 0.25))
plot(d$n, d$sd, pch = 19, log = "x", cex = 1.3, col = "black",
     xlab = "持股数量 n（对数轴）", ylab = "组合月度波动",
     main = "分散化曲线：n = 15 已拿到九成收益")
xs <- seq(1, 500, length.out = 400)
cf <- coef(m)
lines(xs, cf[1] * sqrt(cf[2] + (1 - cf[2]) / xs), col = RD, lwd = 2.8)
abline(h = cf[1] * sqrt(cf[2]), lty = 2, col = BL, lwd = 2)
abline(v = 15, lty = 3, col = GR, lwd = 2)
grid(col = GRID, lty = 1)
legend("topright", c(sprintf("拟合：rho = %.3f", cf[2]),
                     sprintf("极限波动 = %.4f", cf[1] * sqrt(cf[2])),
                     "n = 15"),
       col = c(RD, BL, GR), lty = c(1, 2, 3), lwd = 2, bty = "n", cex = 0.86)
dev.off()

cat("完成：", length(list.files(fig_dir, "^fig-(3|4|5|6|7|10|11|12|13|14|15|16)-")), "幅\n")
