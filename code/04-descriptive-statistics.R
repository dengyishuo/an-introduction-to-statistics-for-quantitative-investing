# 《量化投资的统计学导论》第 4 章 描述统计 —— 示例代码
# 运行: Rscript code/04-descriptive-statistics.R

source("data/load_datasets.R")
cairo_pdf(file.path(tempdir(), "ch04.pdf"), width = 9, height = 6,
          family = "PingFang SC")
on.exit(dev.off(), add = TRUE)

# 4.1 单一组别的汇总统计量 ----

x <- ashr$ret      # 全部股票的全部季度收益
length(x)

mean(x); median(x)
sd(x); IQR(x); mad(x)
var(x)
range(x)
quantile(x, c(0, 0.01, 0.25, 0.5, 0.75, 0.99, 1))
summary(x)

# 均值与中位数差多少：偏度的信号
mean(x) - median(x)

# 变异系数
sd(x) / mean(x)

# 4.2 分布图形 ----

# 4.2.1 直方图
hist(x, breaks = 60, main = "全部季度收益分布", xlab = "季度收益")
hist(x, breaks = 60, probability = TRUE, main = "密度直方图", xlab = "季度收益")
lines(density(x), col = "red", lwd = 2)
curve(dnorm(x, mean(x), sd(x)), add = TRUE, col = "blue", lty = 2, lwd = 2)

# 4.2.2 经验累积分布
plot(ecdf(x), main = "经验分布函数", xlab = "季度收益", ylab = "F(x)")
abline(h = c(0.05, 0.5, 0.95), col = "grey60", lty = 2)

# 4.2.3 Q-Q 图
qqnorm(x, main = "季度收益的正态 Q-Q 图"); qqline(x, col = "red", lwd = 2)

# 4.2.4 箱线图
boxplot(x, main = "季度收益箱线图", ylab = "季度收益")
boxplot.stats(x)$out[1:5]
length(boxplot.stats(x)$out)

# 4.3 按组计算汇总 ----

tapply(ashr$ret, ashr$qtr, mean)
tapply(ashr$ret, ashr$qtr, sd)
tapply(ashr$ret, ashr$qtr, median)

aggregate(ret ~ qtr, data = ashr, FUN = function(z) c(n = length(z), mean = mean(z)))

# 按行业
head(sort(tapply(indret$ret, indret$sector, mean), decreasing = TRUE), 5)
head(sort(tapply(indret$ret, indret$sector, sd), decreasing = TRUE), 5)

# 4.4 分组图形 ----

# 4.4.1 分组直方图
par(mfrow = c(1, 3))
for (q in c("2024Q1", "2025Q3", "2026Q1")) {
  hist(ashr$ret[ashr$qtr == q], breaks = 30, main = q, xlab = "季度收益")
}
par(mfrow = c(1, 1))

# 4.4.2 平行箱线图
boxplot(ret ~ qtr, data = ashr, las = 2, main = "按季度分组的收益",
        ylab = "季度收益", xlab = "")

boxplot(ret ~ sector, data = indret, las = 2, cex.axis = 0.7,
        main = "按行业分组的月度收益", ylab = "月度收益", xlab = "")

# 4.4.3 带状图
stripchart(list(银行 = pair2$bank, 半导体 = pair2$semi),
           method = "jitter", pch = 19, cex = 0.6, vertical = TRUE,
           main = "两个行业 ETF 的月度收益", ylab = "月度收益")

# 4.5 表格 ----

# 4.5.1 生成表格
tab <- table(mlpanel$industry)
head(sort(tab, decreasing = TRUE), 8)
length(tab)

# 交叉表：行业 × 是否上涨
d <- mlpanel[mlpanel$ym == "2026-06", ]
d$up <- d$ret > 0
head(table(d$industry, d$up), 5)

# 4.5.2 边际表与相对频率
m <- table(d$up)
m
prop.table(m)
round(prop.table(m) * 100, 1)

ct <- table(d$industry, d$up)
head(round(prop.table(ct, 1) * 100, 1), 5)
margin.table(ct, 1)[1:5]
margin.table(ct, 2)

# 4.6 习题 ----
# 4.1 计算 drawdown$months 的均值、中位数、四分位距，说明分布形状
# 4.2 画 vol3 三种波动率估计的平行箱线图
# 4.3 用 table 统计各行业的股票数，并算占比
# 4.4 对 indcnt$ndown 画直方图并叠加泊松理论频数
