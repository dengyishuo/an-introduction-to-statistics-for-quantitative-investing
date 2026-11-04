# 《量化投资的统计学导论》第 1 章 基础 —— 示例代码
# 依赖: an-introduction-to-statistics-for-quantitative-investing/data/finwr.R（xquant-db 真实行情推导的数据集）
# 运行: cd an-introduction-to-statistics-for-quantitative-investing && Rscript code/01-basics.R

source("data/finwr.R")

# 1.1 第一步 ----

# 计算器：单日涨跌
(23.45 - 22.80) / 22.80

# 复利：连续 3 个交易日累计收益
(1 + 0.012) * (1 - 0.008) * (1 + 0.005) - 1

# 1.1.2 赋值 ----
moutai_close <- 1332.95
moutai_close
moutai_close * 100

# 1.1.3 向量化算术 ----
weight  <- c(0.30, 0.25, 0.20, 0.15, 0.10)
ret     <- c(0.012, -0.008, 0.005, 0.021, -0.014)
weight * ret
sum(weight * ret)

# 组合日收益：加权求和
portfolio_ret <- sum(weight * ret)
portfolio_ret

# 1.1.4 标准过程 ----
abret$A           # 茅台日收益率
mean(abret$A)
sd(abret$A)

# 年化：A 股每年约 252 个交易日
mean(abret$A) * 252
sd(abret$A) * sqrt(252)

# 单样本 t 检验：日均收益是否显著异于 0
t.test(abret$A)
t.test(abret$B)

# 1.1.5 图形 ----
plot(as.Date(px1$date), px1$close, type = "l")

plot(abret$A, abret$B)
plot(abret$A, abret$B, pch = 2)
abline(0, 1, lty = 2)

# 1.2 R 语言要点 ----

# 1.2.1 表达式与对象
1 + 1
log(2)
moutai_close <- 1332.95
moutai_close

# 1.2.2 函数与参数
mean(abret$A)
mean(abret$B)
mean(c(1, 2, NA, 4))
mean(c(1, 2, NA, 4), na.rm = TRUE)

# 1.2.3 向量
prices <- c(1332.95, 7.24, 423.60, 86.87)
prices
length(prices)
prices[2]
prices[c(1, 4)]
prices[-1]
prices[prices > 100]

# 1.2.4 引号与转义序列
sector <- c("银行", "白酒", "新能源", "医药")
sector
paste("板块：", sector, sep = "")

# 1.2.5 缺失值
x <- c(0.012, NA, -0.008, 0.005)
x
is.na(x)
mean(x)
mean(x, na.rm = TRUE)
sum(x, na.rm = TRUE)

# 1.2.6 创建向量的函数
1:6
seq(1, 12, by = 3)
rep(c("A", "B"), each = 3)
rep(0.25, 4)
c(rep(0, 2), seq(1, 3))

# 1.2.7 矩阵与数组
m <- matrix(c(0.012, -0.008, 0.005,
              0.021, -0.014, 0.003),
            nrow = 2, byrow = TRUE)
m
dim(m)
m[2, 3]
m[, 1]
m[1, ]
colSums(m)
rowMeans(m)

# 组合收益：权重 × 收益矩阵
w <- c(0.5, 0.3, 0.2)
m %*% w

# 1.2.8 因子
sector_f <- factor(c("银行", "白酒", "新能源", "白酒", "银行"))
sector_f
levels(sector_f)
table(sector_f)
as.integer(sector_f)

# 有序因子：信用评级
rating <- factor(c("AAA", "AA", "A", "AA", "AAA"),
                 levels = c("A", "AA", "AAA"), ordered = TRUE)
rating
min(rating)

# 数据框里的因子
str(ashr)
ashr$qtr <- factor(ashr$qtr)
levels(ashr$qtr)

# 1.2.9 列表
fit <- lm(B ~ A, data = abret)
fit
names(fit)
fit$coefficients
fit$residuals[1:3]

# 1.2.10 数据框
abret
head(abret)
names(abret)
nrow(abret)
ncol(abret)
str(abret)
summary(abret)

# 1.2.11 索引
abret$A[1:5]
abret[1:3, ]
abret[1:3, "A"]
abret[, c("A", "B")]

# 1.2.12 条件选择
abret$A[abret$A > 0.01]
sum(abret$A > 0)
mean(abret$A > 0)
abret[abret$A > 0.02, ]

# 1.2.13 数据框索引
big_up <- abret[abret$A > 0.015 & abret$B > 0, ]
big_up
nrow(big_up)

# 1.2.14 分组数据与数据框
tapply(ashr$ret, ashr$qtr, mean)
tapply(ashr$ret, ashr$qtr, sd)

aggregate(ret ~ qtr, data = ashr, FUN = mean)

# 1.2.15 隐式循环
sapply(abret[, c("A", "B")], mean)
sapply(abret[, c("A", "B")], sd)
lapply(abret[, c("A", "B")], range)

# 1.2.16 排序
sort(abret$A)[1:5]
order(abret$A)[1:3]
abret[order(-abret$A)[1:5], ]

# 1.3 习题 ----
# 1.1 用 px1 计算全期累计收益与最大回撤
tail(px1$close, 1) / px1$close[1] - 1
dd <- (cummax(px1$close) - px1$close) / cummax(px1$close)
max(dd)

# 1.2 用 abret 计算两股的相关系数
cor(abret$A, abret$B)

# 1.3 用 vol3 找出年化波动最大的三只股票
vol3[order(-vol3$close_to_close)[1:3], c("name", "close_to_close")]

# 1.4 用 ashr 计算各季度收益的中位数
tapply(ashr$ret, ashr$qtr, median)

# 1.5 用 excess 检验茅台月度超额收益是否显著异于 0
t.test(excess$excess[excess$symbol == "600519.SS"])
