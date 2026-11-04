# 《量化投资的统计学导论》第 5 章 单样本与双样本检验 —— 示例代码
# 运行: Rscript code/05-one-two-sample-tests.R

source("data/load_datasets.R")

# 5.1 单样本 t 检验 ----

# 问题：茅台 2026Q1 的日均收益是否显著异于零
x <- abret$A
c(n = length(x), mean = mean(x), sd = sd(x))
t.test(x)

# 指定理论均值：日均收益是否等于 0.1%
t.test(x, mu = 0.001)

# 单侧检验：是否大于零
t.test(x, alternative = "greater")

# 置信区间与检验的等价性
t.test(x)$conf.int

# 工商银行
t.test(abret$B)

# 5.2 Wilcoxon 符号秩检验 ----
# 不要求正态，只看中位数
wilcox.test(x)
wilcox.test(x, mu = 0, conf.int = TRUE)

# 与 t 检验对比：厚尾时 Wilcoxon 更稳
c(t_p = t.test(x)$p.value, wilcox_p = wilcox.test(x)$p.value)

# 5.3 双样本 t 检验 ----

# 银行 ETF vs 半导体 ETF 的月度收益（不配对，视为两组独立样本）
t.test(pair2$bank, pair2$semi)
t.test(pair2$bank, pair2$semi, var.equal = TRUE)

# 均值差与置信区间
d <- t.test(pair2$bank, pair2$semi)
c(diff = mean(pair2$bank) - mean(pair2$semi), d$conf.int)

# 单侧：银行是否低于半导体
t.test(pair2$bank, pair2$semi, alternative = "less")

# 5.4 方差比较 ----

var(pair2$bank); var(pair2$semi)
var.test(pair2$bank, pair2$semi)

# 三种波动率估计两两比较（配对，因为同一批股票）
t.test(vol3$close_to_close, vol3$garman_klass, paired = TRUE)
cor(vol3$close_to_close, vol3$garman_klass)

# 5.5 双样本 Wilcoxon 检验 ----
wilcox.test(pair2$bank, pair2$semi)
wilcox.test(pair2$bank, pair2$semi, conf.int = TRUE)

# 5.6 配对 t 检验 ----
# 同一批股票、两个行情源的报价差异（本书用两种波动率估计代替）
t.test(vol3$close_to_close, vol3$parkinson, paired = TRUE)

# 配对与不配对的差别
t.test(vol3$close_to_close, vol3$parkinson, paired = TRUE)$p.value
t.test(vol3$close_to_close, vol3$parkinson, paired = FALSE)$p.value

# 配对差值
dv <- vol3$close_to_close - vol3$garman_klass
c(mean = mean(dv), sd = sd(dv), n = length(dv))
t.test(dv)

# 5.7 配对 Wilcoxon 检验 ----
wilcox.test(vol3$close_to_close, vol3$garman_klass, paired = TRUE)

# 5.8 多重检验的陷阱 ----

# 20 个行业 ETF，两两比较共 190 对，若都用 0.05 水平
n_ind <- length(unique(indret$sector))
pairs <- choose(n_ind, 2)
pairs
c(expected_false_positive = pairs * 0.05)

# 用真实数据：所有行业对的 t 检验 p 值分布
secs <- unique(indret$sector)
w <- reshape(indret[, c("month", "sector", "ret")], idvar = "month",
             timevar = "sector", direction = "wide")
names(w) <- sub("^ret\\.", "", names(w))
pv <- c()
for (i in 1:(n_ind - 1)) for (j in (i + 1):n_ind) {
  a <- w[[secs[i]]]; b <- w[[secs[j]]]
  pv <- c(pv, t.test(a, b)$p.value)
}
length(pv)
sum(pv < 0.05)
round(sum(pv < 0.05) / length(pv), 3)

# Bonferroni 校正
p.adjust(pv, method = "bonferroni")[1:5]
sum(p.adjust(pv, method = "bonferroni") < 0.05)

# 5.9 习题 ----
# 5.1 检验 idxret 中沪深300ETF 的日均收益是否异于零
# 5.2 比较银行 ETF 与地产 ETF 的波动率（var.test）
# 5.3 用 wilcox.test 检验 drawdown$months 的中位数是否等于 10
# 5.4 对 5.8 的 p 值用 BH 方法校正，显著对数变化多少？
