#!/usr/bin/env Rscript
# 《量化投资的统计学导论》第 1 章插图（全部由 xquant-db 真实数据绘制）
# 用法: cd an-introduction-to-statistics-for-quantitative-investing && Rscript script/03_fig_ch01.R

root <- Sys.getenv("BOOK_ROOT", unset = ".")
csv <- file.path(root, "data", "csv")
fig <- file.path(root, "media", "fig")
dir.create(fig, recursive = TRUE, showWarnings = FALSE)

rd <- function(n) read.csv(file.path(csv, paste0(n, ".csv")), stringsAsFactors = FALSE)
abret <- rd("abret"); px1 <- rd("px1")

png_ <- function(name, w = 1200, h = 780) {
  png(file.path(fig, name), width = w, height = h, res = 150, type = "cairo")
}

# 图 1.1 单只股票收盘价走势
png_("fig-1-1.png")
par(mar = c(4.2, 4.4, 2.4, 1))
plot(as.Date(px1$date), px1$close, type = "l", lwd = 1.4, col = "#185FA5",
     xlab = "日期", ylab = "收盘价（元）", las = 1,
     main = "贵州茅台日收盘价（2024-01 至 2026-05）")
grid(col = "grey92", lty = 1)
dev.off()

# 图 1.2 两只股票日收益率的散点图
png_("fig-1-2.png")
par(mar = c(4.2, 4.4, 2.4, 1))
plot(abret$A * 100, abret$B * 100, pch = 16, col = "#185FA5", cex = 0.9,
     xlab = "贵州茅台日收益率（%）", ylab = "工商银行日收益率（%）", las = 1,
     main = "2026 年一季度：两只股票的日收益率")
abline(h = 0, v = 0, col = "grey70", lty = 2)
grid(col = "grey93", lty = 1)
dev.off()

# 图 1.3 改用三角形标记
png_("fig-1-3.png")
par(mar = c(4.2, 4.4, 2.4, 1))
plot(abret$A * 100, abret$B * 100, pch = 2, col = "#993C1D", cex = 0.9,
     xlab = "贵州茅台日收益率（%）", ylab = "工商银行日收益率（%）", las = 1,
     main = "同一图形，换成空心三角标记（pch = 2）")
abline(h = 0, v = 0, col = "grey70", lty = 2)
dev.off()

# 图 1.4 叠加同幅同向/反向参考线
png_("fig-1-4.png")
par(mar = c(4.2, 4.4, 2.4, 1))
plot(abret$A * 100, abret$B * 100, pch = 16, col = "#5F5E5A", cex = 0.85,
     xlab = "贵州茅台日收益率（%）", ylab = "工商银行日收益率（%）", las = 1,
     main = "叠加“同幅同向”与“同幅反向”参考线")
lim <- range(c(abret$A, abret$B)) * 100
abline(0, 1, col = "#A32D2D", lwd = 1.6, lty = 2)
abline(0, -1, col = "#0F6E56", lwd = 1.2, lty = 3)
legend("topleft", c("y = x", "y = -x"), col = c("#A32D2D", "#0F6E56"),
       lty = c(2, 3), lwd = c(1.6, 1.2), bty = "n", cex = 0.9)
dev.off()

cat("第 1 章插图完成：", length(list.files(fig, "^fig-1-")), "幅\n")
