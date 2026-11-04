# 《量化投资的统计学导论》第 6 章 回归与相关 —— 示例代码
# 运行: Rscript code/06-regression-correlation.R

source("data/finwr.R")
cairo_pdf(file.path(tempdir(), "ch06.pdf"), width = 9, height = 6,
          family = "PingFang SC")
on.exit(dev.off(), add = TRUE)

# 6.1 简单线性回归 ----
# 问题：资产负债率能解释 ROE 吗
d <- fin[complete.cases(fin[, c("roe", "debt_ratio")]), ]
nrow(d)
m <- lm(roe ~ debt_ratio, data = d)
m
summary(m)

# 系数、标准误、t 值、p 值
summary(m)$coefficients
# R 方与调整 R 方
summary(m)$r.squared
summary(m)$adj.r.squared

# 回归线：ROE = 5.03 - 0.073 × 负债率
# 斜率含义：负债率每高 1 个百分点，ROE 平均低 0.073 个百分点
coef(m)[2] * 10

# 6.2 残差与拟合值 ----
fitted(m)[1:5]
resid(m)[1:5]
head(cbind(d$roe[1:5], fitted(m)[1:5], resid(m)[1:5]), 5)

par(mfrow = c(2, 2))
plot(m)
par(mfrow = c(1, 1))

# 残差与拟合值的散点
plot(fitted(m), resid(m), pch = 19, cex = 0.4, col = rgb(0, 0, 0, 0.3),
     xlab = "拟合值", ylab = "残差", main = "残差图")
abline(h = 0, col = "red", lwd = 2)

# 6.3 预测带与置信带 ----
newd <- data.frame(debt_ratio = c(20, 40, 60, 80))
predict(m, newd, interval = "confidence")
predict(m, newd, interval = "prediction")

# 画出来
xr <- range(d$debt_ratio)
xs <- seq(xr[1], xr[2], length.out = 100)
ci <- predict(m, data.frame(debt_ratio = xs), interval = "confidence")
pi <- predict(m, data.frame(debt_ratio = xs), interval = "prediction")
plot(d$debt_ratio, d$roe, pch = 19, cex = 0.3, col = rgb(0, 0, 0, 0.25),
     xlab = "资产负债率（%）", ylab = "ROE（%）", main = "回归线与预测带")
lines(xs, ci[, "fit"], col = "blue", lwd = 2)
lines(xs, ci[, "lwr"], col = "blue", lty = 2)
lines(xs, ci[, "upr"], col = "blue", lty = 2)
lines(xs, pi[, "lwr"], col = "red", lty = 3)
lines(xs, pi[, "upr"], col = "red", lty = 3)
legend("topright", c("拟合线", "95% 置信带", "95% 预测带"),
       col = c("blue", "blue", "red"), lty = c(1, 2, 3), lwd = 2, bty = "n")

# 6.4 相关 ----

# 6.4.1 Pearson
cor(d$roe, d$debt_ratio)
cor.test(d$roe, d$debt_ratio)

# 相关与回归的关系：r^2 = R^2
cor(d$roe, d$debt_ratio)^2
summary(m)$r.squared

# 多组相关：宽基指数之间
idx <- idxret[, -1]
idx <- idx[complete.cases(idx), ]
C <- cor(idx)
round(C[1:4, 1:4], 3)

# 6.4.2 Spearman
cor(d$roe, d$debt_ratio, method = "spearman")
cor.test(d$roe, d$debt_ratio, method = "spearman")

# 6.4.3 Kendall
cor(d$roe, d$debt_ratio, method = "kendall")

# 三者的差别：有离群值时 Pearson 更敏感
x <- c(1:20, 100)
y <- c(1:20, 1)
c(pearson = cor(x, y), spearman = cor(x, y, method = "spearman"),
  kendall = cor(x, y, method = "kendall"))

# 6.5 相关不等于因果：一个真实的反例 ----

# 市值与换手率负相关，但小市值不是「因为」换手高才收益高
c1 <- scale[scale$ym == "2026-06", ]
cor(c1$size, c1$turnover)
m1 <- lm(turnover ~ size, data = c1)
summary(m1)$r.squared

# 加入行业后关系是否稳定
c1$industry <- factor(c1$industry)
m2 <- lm(turnover ~ size + industry, data = c1)
summary(m2)$r.squared
coef(m2)[2]

# 6.6 习题 ----
# 6.1 用 vol3 的 parkinson 对 close_to_close 做回归，斜率与 R2 各是多少
# 6.2 用 drawdown 的 vol_12m 对 months 做回归，解释斜率方向
# 6.3 计算 idxret 全部宽基指数的相关矩阵，找出相关性最低的一对
# 6.4 比较 roe ~ debt_ratio 的 Pearson 与 Spearman，差异说明什么
