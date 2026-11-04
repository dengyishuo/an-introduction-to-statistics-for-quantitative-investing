# 《量化投资的统计学导论》第 12 章 线性模型 —— 示例代码
# 运行: Rscript code/12-linear-models.R

source("data/finwr.R")
cairo_pdf(file.path(tempdir(), "ch12.pdf"), width = 9, height = 6,
          family = "PingFang SC")
on.exit(dev.off(), add = TRUE)

# 12.1 多项式回归 ----
# 换手率与市值：关系是否非线性
d <- scale[scale$ym == "2026-06", ]
d <- d[!is.na(d$turnover) & !is.na(d$size), ]
d$lsize <- d$size      # 已经是 log(流通市值)
d$lturn <- log(d$turnover)

m1 <- lm(lturn ~ lsize, data = d)
m2 <- lm(lturn ~ lsize + I(lsize^2), data = d)
m3 <- lm(lturn ~ lsize + I(lsize^2) + I(lsize^3), data = d)
summary(m1)$r.squared
summary(m2)$r.squared
summary(m3)$r.squared
anova(m1, m2, m3)

# 二次项的系数与顶点（抛物线极值位置）
cf <- coef(m2)
cf
vertex <- -cf["lsize"] / (2 * cf["I(lsize^2)"])
vertex

# 作图
xs <- seq(min(d$lsize), max(d$lsize), length.out = 200)
plot(d$lsize, d$lturn, pch = 19, cex = 0.3, col = rgb(0, 0, 0, 0.15),
     xlab = "log(流通市值)", ylab = "log(换手率)", main = "多项式回归拟合")
lines(xs, predict(m1, data.frame(lsize = xs)), col = "blue", lwd = 2)
lines(xs, predict(m2, data.frame(lsize = xs)), col = "red", lwd = 2)
legend("topright", c("线性", "二次"), col = c("blue", "red"), lwd = 2, bty = "n")

# 12.2 过原点的回归 ----
m0 <- lm(lturn ~ 0 + lsize, data = d)
m0
summary(m0)$r.squared
# 注意：过原点时 R2 的定义改变，不能直接与含截距模型比较
summary(m1)$r.squared

# 12.3 设计矩阵与虚拟变量 ----
d2 <- indret[indret$month %in% c("2026-05", "2026-06"), ]
d2$sector <- factor(d2$sector)
m4 <- lm(ret ~ sector, data = d2)
length(coef(m4))
coef(m4)[1:5]
# 第一个水平是基准组
levels(d2$sector)[1]

# 手工构建设计矩阵
X <- model.matrix(~ sector, data = d2)
dim(X)
X[1:3, 1:4]

# 12.4 分组内的线性关系 ----
# 不同行业里，市值与换手率的关系是否一致
# scale 自带 industry 列，直接用
d$ind <- factor(d$industry)
d <- d[!is.na(d$ind), ]
length(levels(d$ind))

m5 <- lm(lturn ~ lsize + ind, data = d)
m6 <- lm(lturn ~ lsize * ind, data = d)
summary(m5)$r.squared
summary(m6)$r.squared
anova(m5, m6)

# 分层拟合：几个行业的斜率
slopes <- tapply(seq_len(nrow(d)), d$ind, function(i) {
  if (length(i) < 30) return(NA_real_)
  coef(lm(lturn ~ lsize, data = d[i, ]))["lsize"]
})
head(sort(slopes, decreasing = TRUE), 5)
head(sort(slopes), 5)

# 12.5 交互作用 ----
# 行业与市值是否交互（即斜率是否因行业而异）
summary(m6)$r.squared - summary(m5)$r.squared

# 一个更简单的例子：两个行业（银行 vs 电子）
sub <- d[d$ind %in% c("银行", "电子"), ]
sub$ind <- droplevels(sub$ind)
table(sub$ind)
m7 <- lm(lturn ~ lsize * ind, data = sub)
summary(m7)$coefficients

# 12.6 有重复的双因素方差分析 ----
dd <- indret
dd$year <- substr(dd$month, 1, 4)
m8 <- aov(ret ~ sector * year, data = dd)
summary(m8)

# 主效应
m9 <- aov(ret ~ sector + year, data = dd)
summary(m9)

# 12.7 协方差分析 ----
# 控制波动率后，行业间收益是否还有差异
dm <- mlevent[mlevent$ym == "2026-06", ]
dm <- dm[complete.cases(dm[, c("fwd_ret", "vol_12m", "industry")]), ]
dm$ind <- factor(dm$industry)
dm <- dm[dm$ind %in% names(which(table(dm$ind) > 30)), ]
dm$ind <- droplevels(dm$ind)

m10 <- lm(fwd_ret ~ ind, data = dm)
m11 <- lm(fwd_ret ~ ind + vol_12m, data = dm)
c(no_cov = summary(m10)$r.squared, with_cov = summary(m11)$r.squared)
anova(m10, m11)

# 12.7.1 图形描述
boxplot(fwd_ret ~ ind, data = dm, las = 2, cex.axis = 0.7,
        main = "控制波动率前的行业收益", ylab = "下月收益")

# 12.7.2 回归线的比较
m12 <- lm(fwd_ret ~ ind * vol_12m, data = dm)
anova(m11, m12)

# 12.8 诊断 ----
par(mfrow = c(2, 2))
plot(m11)
par(mfrow = c(1, 1))

# 残差正态性
shapiro.test(sample(resid(m11), 500))

# 异方差：残差绝对值与拟合值的关系
r2 <- lm(abs(resid(m11)) ~ fitted(m11))
summary(r2)$r.squared

# 12.9 习题 ----
# 12.1 用三次多项式拟合 divcurve 的分散化曲线，与 16 章的 nls 比较
# 12.2 在 12.7 的模型中加入 size 作为第二个协变量
# 12.3 检验 dm 中各行业的残差方差是否齐性
# 12.4 用 relevel 改变基准行业，解释截距的变化
