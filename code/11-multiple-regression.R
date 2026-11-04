# 《量化投资的统计学导论》第 11 章 多元回归 —— 示例代码
# 运行: Rscript code/11-multiple-regression.R

source("data/load_datasets.R")
cairo_pdf(file.path(tempdir(), "ch11.pdf"), width = 9, height = 6,
          family = "PingFang SC")
on.exit(dev.off(), add = TRUE)

# 11.1 绘制多元数据 ----
d <- fin[complete.cases(fin[, c("roe", "debt_ratio", "gross_margin",
                                "rev_growth", "revenue")]), ]
d <- d[d$rev_growth > -100 & d$rev_growth < 300, ]
nrow(d)

pairs(d[, c("roe", "debt_ratio", "gross_margin", "rev_growth")],
      pch = 19, cex = 0.3, col = rgb(0, 0, 0, 0.2))

# 相关矩阵
round(cor(d[, c("roe", "debt_ratio", "gross_margin", "rev_growth")]), 3)

# 11.2 模型设定与输出 ----

m1 <- lm(roe ~ debt_ratio, data = d)
m2 <- lm(roe ~ debt_ratio + gross_margin, data = d)
m3 <- lm(roe ~ debt_ratio + gross_margin + rev_growth, data = d)
m4 <- lm(roe ~ debt_ratio + gross_margin + rev_growth + log(revenue), data = d)

summary(m1)$r.squared
summary(m2)$r.squared
summary(m3)$r.squared
summary(m4)$r.squared

summary(m3)

# 系数含义：控制其他变量后，负债率每升 1 个百分点，ROE 变化多少
coef(m3)["debt_ratio"]
coef(m1)["debt_ratio"]

# 变量间相关导致系数变化（共线性）
round(cor(d$debt_ratio, d$gross_margin), 3)

# 11.3 模型搜索 ----

# 逐步回归
m_full <- lm(roe ~ debt_ratio + gross_margin + rev_growth + log(revenue), data = d)
step(m_full, direction = "both", trace = 0)

# 比较 AIC
AIC(m1, m2, m3, m4)

# 调整 R 方
sapply(list(m1, m2, m3, m4), function(m) summary(m)$adj.r.squared)

# 11.4 分类变量进回归 ----
# 行业作为因子，会生成虚拟变量
# fin 本身不含行业，从 mlpanel 取代码到行业的映射
ind_map <- unique(mlpanel[, c("secucode", "industry")])
d$ind <- factor(ind_map$industry[match(d$code, ind_map$secucode)])
sum(is.na(d$ind))
d <- d[!is.na(d$ind), ]
table(d$ind)[1:5]

m5 <- lm(roe ~ debt_ratio + gross_margin + rev_growth + ind, data = d)
summary(m5)$r.squared
length(coef(m5))

# 与不含行业的同一模型对比：行业解释力有多大
m3b <- lm(roe ~ debt_ratio + gross_margin + rev_growth, data = d)
c(no_ind = summary(m3b)$r.squared, with_ind = summary(m5)$r.squared)
anova(m3b, m5)

# 11.5 交互作用 ----

# 负债率对 ROE 的影响是否因行业而异
m6 <- lm(roe ~ debt_ratio * ind, data = d)
anova(m5, m6)

# 11.6 诊断 ----
par(mfrow = c(2, 2))
plot(m3)
par(mfrow = c(1, 1))

# 异常值与强影响点
cooks <- cooks.distance(m3)
sum(cooks > 4 / nrow(d))
head(sort(cooks, decreasing = TRUE), 5)

# 方差膨胀因子：共线性
vif_manual <- sapply(c("debt_ratio", "gross_margin", "rev_growth"), function(v) {
  f <- as.formula(paste(v, "~", paste(setdiff(c("debt_ratio", "gross_margin", "rev_growth"), v), collapse = "+")))
  1 / (1 - summary(lm(f, data = d))$r.squared)
})
round(vif_manual, 3)

# 11.7 习题 ----
# 11.1 把 EPS 加入模型，R2 提升多少
# 11.2 用 BIC 代替 AIC 做模型选择，结论一致吗
# 11.3 对 mlpanel 的一个月截面，用 size 与 turnover 解释 fwd_ret
# 11.4 计算 11.6 中最大 Cook 距离那一行的各项取值，判断是否该剔除
