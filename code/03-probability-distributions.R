# 《量化投资的统计学导论》第 3 章 概率与分布 —— 示例代码
# 运行: Rscript code/03-probability-distributions.R

source("data/finwr.R")

# 3.1 随机抽样 ----

# 从股票池里随机抽 5 只
set.seed(2026)
pool <- unique(mlpanel$secucode)
sample(pool, 5)
sample(pool, 5)          # 不设种子，每次不同

# 有放回抽样与不放回
sample(1:10, 15, replace = TRUE)
length(sample(pool, 100, replace = FALSE))

# 3.2 概率计算与组合 ----

# 从 10 只股票里选 3 只，有多少种组合
choose(10, 3)
# 全部排列
factorial(3)
# 二项系数表
outer(1:5, 1:5, choose)

# 3.3 离散分布 ----

# 二项分布：20 个交易日里，上涨天数为 12 天及以上的概率
# 假设每天上涨概率 0.5
p_up <- 0.5
dbinom(12, 20, p_up)
pbinom(11, 20, p_up, lower.tail = FALSE)
sum(dbinom(12:20, 20, p_up))

# 用真实上涨概率：茅台 2026Q1 的上涨天数占比
p_real <- mean(abret$A > 0)
p_real
dbinom(12, 20, p_real)

# 泊松分布：某行业每月「跌幅超 1% 的交易日数」
lam <- mean(indcnt$ndown)
lam
dpois(0:10, lam)
ppois(8, lam, lower.tail = FALSE)

# 3.4 连续分布 ----

# 正态：日收益落在 ±2% 内的概率
pnorm(0.02, mean(abret$A), sd(abret$A)) -
  pnorm(-0.02, mean(abret$A), sd(abret$A))

# 收益率 5% 分位数（VaR 的雏形）
qnorm(0.05, mean(abret$A), sd(abret$A))
# 实际数据的 5% 分位数
quantile(abret$A, 0.05)

# 均匀分布
punif(0.5); qunif(c(0.25, 0.75))
runif(5)

# 3.5 R 中的分布函数族 ----
# d 密度 / p 累积 / q 分位数 / r 随机数

x <- seq(-0.06, 0.06, length.out = 200)
d <- dnorm(x, mean(abret$A), sd(abret$A))
head(cbind(x, d), 3)

pnorm(c(-0.02, 0, 0.02), mean(abret$A), sd(abret$A))
qnorm(c(0.01, 0.5, 0.99), mean(abret$A), sd(abret$A))

set.seed(1)
r <- rnorm(1000, mean(abret$A), sd(abret$A))
mean(r); sd(r)

# t 分布：小样本时尾部更厚
qt(0.975, df = 5)
qt(0.975, df = 1000)
qnorm(0.975)

# 3.6 正态假设成立吗：用真实收益检验 ----

# 茅台日收益的偏度与峰度
x <- abret$A
m <- mean(x); s <- sd(x)
skew <- mean((x - m)^3) / s^3
kurt <- mean((x - m)^4) / s^4 - 3
c(skew = skew, excess_kurt = kurt)

# 正态性检验
shapiro.test(x)
ks.test(x, "pnorm", mean = m, sd = s)

# 全部 4145 只股票的季度收益（大样本）
shapiro.test(sample(ashr$ret, 500))
c(n = length(ashr$ret),
  skew = mean((ashr$ret - mean(ashr$ret))^3) / sd(ashr$ret)^3,
  kurt = mean((ashr$ret - mean(ashr$ret))^4) / sd(ashr$ret)^4 - 3)

# 3.7 习题 ----
# 3.1 计算 30 个交易日中至少 20 天上涨的概率（用真实上涨率）
# 3.2 用 qnorm 与 quantile 对比 1% 与 5% 分位数，说明正态假设低估了哪一侧
# 3.3 对 indcnt$nbig 拟合泊松分布，比较观测频数与理论频数
# 3.4 生成 1000 个 t(3) 随机数，与正态比较峰度
