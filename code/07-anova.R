# 《量化投资的统计学导论》第 7 章 方差分析与 Kruskal-Wallis —— 示例代码
# 运行: Rscript code/07-anova.R

source("data/load_datasets.R")
cairo_pdf(file.path(tempdir(), "ch07.pdf"), width = 9, height = 6,
          family = "PingFang SC")
on.exit(dev.off(), add = TRUE)

# 7.1 单因素方差分析 ----
# 问题：不同季度的收益均值有差异吗

m <- aov(ret ~ qtr, data = ashr)
summary(m)
anova(m)

# 组均值与组内样本量
tapply(ashr$ret, ashr$qtr, mean)
table(ashr$qtr)

# 组间与组内方差分解
ss_between <- sum(table(ashr$qtr) * (tapply(ashr$ret, ashr$qtr, mean) -
                 mean(ashr$ret))^2)
ss_within <- sum(resid(m)^2)
c(between = ss_between, within = ss_within,
  ratio = ss_between / (ss_between + ss_within))

# F 统计量
summary(m)[[1]][["F value"]][1]
summary(m)[[1]][["Pr(>F)"]][1]

# 7.1.1 成对比较与多重检验
pairwise.t.test(ashr$ret, ashr$qtr, p.adjust.method = "none")$p.value[1:3, 1:3]
pairwise.t.test(ashr$ret, ashr$qtr, p.adjust.method = "bonferroni")$p.value[1:5, 1:5]

# Tukey HSD
tk <- TukeyHSD(m)
head(tk$qtr[order(tk$qtr[, "p adj"]), ], 5)

# 7.1.2 放宽方差假设：Welch
oneway.test(ret ~ qtr, data = ashr)
oneway.test(ret ~ qtr, data = ashr, var.equal = TRUE)

# 7.1.3 图形展示
boxplot(ret ~ qtr, data = ashr, las = 2, main = "按季度的收益分布",
        ylab = "季度收益", xlab = "")
means <- tapply(ashr$ret, ashr$qtr, mean)
points(1:length(means), means, pch = 18, cex = 1.6, col = "red")

# 7.1.4 Bartlett 检验：方差是否齐性
bartlett.test(ret ~ qtr, data = ashr)
# 更稳健的 Levene 检验（用中位数）
library(car)
leveneTest(ret ~ factor(qtr), data = ashr)

# 7.2 Kruskal-Wallis 检验 ----
kruskal.test(ret ~ qtr, data = ashr)

# 与 ANOVA 对比
c(anova_p = summary(m)[[1]][["Pr(>F)"]][1],
  kw_p = kruskal.test(ret ~ qtr, data = ashr)$p.value)

# 行业层面：20 组的比较
m2 <- aov(ret ~ sector, data = indret)
summary(m2)
kruskal.test(ret ~ sector, data = indret)
bartlett.test(ret ~ sector, data = indret)

# 7.3 双因素方差分析 ----
# 行业 × 年份 对收益的影响（交互作用）
d <- indret
d$year <- substr(d$month, 1, 4)
m3 <- aov(ret ~ sector + year, data = d)
summary(m3)

# 有交互项
m4 <- aov(ret ~ sector * year, data = d)
summary(m4)

# 7.3.1 重复测量的图形
interaction.plot(d$year, d$sector, d$ret, legend = FALSE, las = 1,
                 xlab = "年份", ylab = "平均月度收益", trace.label = "")

# 7.4 Friedman 检验 ----
# 每个月份对 20 个行业排序，检验行业的排序是否稳定
w <- reshape(indret[, c("month", "sector", "ret")], idvar = "month",
             timevar = "sector", direction = "wide")
w <- w[complete.cases(w), ]
M <- as.matrix(w[, -1])
friedman.test(M)

# 7.5 回归中的方差分析表 ----
m5 <- lm(ret ~ qtr, data = ashr)
anova(m5)

# 与 aov 结果一致
anova(m)

# 7.6 效应量：组间差异到底有多大 ----
ss <- summary(m)[[1]][["Sum Sq"]]
eta2 <- ss[1] / sum(ss)
c(eta_squared = eta2, sqrt_eta2 = sqrt(eta2))

# 7.7 习题 ----
# 7.1 用 mlevent 的 win 按行业分组做 ANOVA，哪些行业显著不同
# 7.2 对 drawdown$months 按行业做 Kruskal-Wallis 检验
# 7.3 比较 2024 与 2025 两年的季度收益，做双因素分析
# 7.4 计算 7.2 节 Kruskal-Wallis 的效应量
