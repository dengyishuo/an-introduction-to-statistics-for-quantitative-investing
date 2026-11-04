# 《量化投资的统计学导论》第 2 章 R 环境 —— 示例代码
# 运行: cd an-introduction-to-statistics-for-quantitative-investing
#       Rscript code/02-r-environment.R

source("data/load_datasets.R")

# 图形中文显示：普通 pdf 设备不支持中文字体，改用 cairo_pdf
cairo_pdf(file.path(tempdir(), "ch02.pdf"), width = 9, height = 6,
          family = "PingFang SC")
on.exit(dev.off(), add = TRUE)

# 2.1 会话管理 ----

# 2.1.1 工作空间：列出、保存、载入
ls()
objects()
ls(pattern = "^ml")
saveRDS(abret, "/tmp/abret.rds")
abret2 <- readRDS("/tmp/abret.rds")
identical(abret, abret2)

# 2.1.2 文本输出
print(mean(abret$A))
cat("日均收益:", round(mean(abret$A) * 100, 4), "%\n")
sprintf("年化收益 %.2f%%", mean(abret$A) * 252 * 100)
format(1234567.891, big.mark = ",")
round(c(0.00012345, 1.234567), 4)
signif(c(0.00012345, 1.234567), 4)

# 2.1.3 脚本编写：source 与 sink
sink("/tmp/out.txt")
cat("写入文件\n")
sink()
readLines("/tmp/out.txt")

# 2.1.4 获取帮助
# ?mean
# args(mean)
# example(mean)

# 2.1.5 包
search()
# install.packages("quadprog")
library(quadprog)
search()[1:4]

# 2.1.6 内置数据
data(package = "datasets")$results[1:5, c("Item", "Title")]
data(EuStockMarkets)
str(EuStockMarkets)
head(EuStockMarkets, 3)

# 2.1.7 attach 与 detach
head(mlpanel[, c("secucode", "ym", "size")], 3)
with(mlpanel[1:3, ], c(size, turnover))

# 2.1.8 subset、transform、within
sub <- subset(mlpanel, industry == "银行" & ym == "2026-06",
              select = c(secucode, size, turnover))
head(sub, 3)
nrow(sub)

t1 <- transform(mlpanel[1:3, ], logsize = log(size), big = turnover > 0.5)
t1[, c("size", "logsize", "turnover", "big")]

w1 <- within(mlpanel[1:3, ], {
  logsize <- log(size)
  big <- turnover > 0.5
  rm(size)
})
head(w1, 2)

# 2.2 图形子系统 ----

# 2.2.1 图形布局
par(mfrow = c(2, 2))
plot(abret$A, type = "l", main = "A 日收益")
plot(abret$B, type = "l", main = "B 日收益")
hist(abret$A, main = "A 分布", xlab = "")
boxplot(abret$A, main = "A 箱线")
par(mfrow = c(1, 1))

# 2.2.2 由各个部分构建图形
plot(px1$close[1:120], type = "n", xlab = "交易日", ylab = "收盘价",
     main = "分段构建图形")
grid()
lines(px1$close[1:120], col = "steelblue", lwd = 2)
points(1:120, px1$close[1:120], pch = 20, cex = 0.5, col = "grey40")
abline(h = mean(px1$close[1:120]), lty = 2, col = "firebrick")
text(60, max(px1$close[1:120]), "茅台前 120 个交易日", pos = 3)

# 2.2.3 使用 par
par(bg = "white", las = 1, cex.axis = 0.9)
plot(abret$A, abret$B, pch = 19, col = rgb(0, 0, 0, 0.4))
par(las = 0, cex.axis = 1)

# 2.2.4 组合图形
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
hist(ashr$ret, breaks = 40, main = "季度收益分布", xlab = "季度收益")
qqnorm(ashr$ret, main = "正态 Q-Q 图"); qqline(ashr$ret, col = "red")
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

layout(matrix(c(1, 2, 1, 3), 2, 2))
plot(px1$close, type = "l", main = "主图")
hist(abret$A, main = "右上")
boxplot(abret$A, main = "右下")
layout(1)

# 2.3 读到数据：三种常用方式 ----

# CSV
write.csv(abret, "/tmp/abret.csv", row.names = FALSE)
a1 <- read.csv("/tmp/abret.csv", stringsAsFactors = FALSE)
str(a1)

# 直接读 URL 或压缩文件（示例）
# read.csv("https://example.com/data.csv")
# read.csv(gzfile("data.csv.gz"))

# 2.4 习题 ----
# 2.1 用 subset 取出 2026-06 中 size 最大的 10 只股票
# 2.2 用 transform 给 mlpanel 增加一列 log_turnover
# 2.3 用 par(mfrow) 画出四个行业 ETF 的收益走势
# 2.4 用 cat 与 sprintf 输出「行业共有 N 个」这类汇总句
