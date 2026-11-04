# 《量化投资的统计学导论》第 15 章 率与泊松回归 —— 示例代码
# 运行: Rscript code/15-poisson.R

source("data/load_datasets.R")
cairo_pdf(file.path(tempdir(), "ch15.pdf"), width = 9, height = 6,
          family = "PingFang SC")
on.exit(dev.off(), add = TRUE)

# 15.1 泊松分布 ----
# 计数数据：某行业每月「跌幅超 1% 的交易日数」
indcnt <- indcnt[complete.cases(indcnt[, c("ndown", "sd_m", "exposure")]), ]
x <- indcnt$ndown
mean(x); var(x)
c(mean = mean(x), var = var(x), ratio = var(x) / mean(x))

hist(x, breaks = 0:16, probability = TRUE, main = "每月下跌日数分布",
     xlab = "跌幅超 1% 的交易日数")
lines(0:15, dpois(0:15, mean(x)), col = "red", lwd = 2, type = "h")

# 3 天及以下的概率
ppois(3, mean(x))

# 15.2 拟合泊松模型 ----
# 率 = 事件数 / 暴露量（交易日数），用 offset 处理
m <- glm(ndown ~ 1, data = indcnt, family = poisson,
         offset = log(exposure))
summary(m)
exp(coef(m))
# 校验：整体均值
mean(indcnt$ndown) / mean(indcnt$exposure)
exp(coef(m)) * mean(indcnt$exposure)

# 15.3 计算率 ----
# 各行业的率
agg <- aggregate(cbind(ndown, exposure) ~ sector, indcnt, sum)
agg$rate <- agg$ndown / agg$exposure
agg <- agg[order(-agg$rate), ]
agg_print <- agg
agg_print$ndown <- round(agg_print$ndown, 0)
agg_print$exposure <- round(agg_print$exposure, 0)
agg_print$rate <- round(agg_print$rate, 4)
head(agg_print, 6)
tail(agg_print, 3)
c(max_rate = max(agg$rate), min_rate = min(agg$rate),
  ratio = max(agg$rate) / min(agg$rate))

# 15.4 行业之间的差异 ----
m_ind <- glm(ndown ~ sector, data = indcnt, family = poisson,
             offset = log(exposure))
summary(m_ind)
# 偏差检验：行业整体是否解释了什么
anova(m_ind, test = "Chisq")

# 过度分散检验
disp <- sum(residuals(m_ind, type = "pearson")^2) / df.residual(m_ind)
c(dispersion = disp)
# 若明显大于 1，需要拟泊松或负二项

m_quasi <- glm(ndown ~ sector, data = indcnt, family = quasipoisson,
               offset = log(exposure))
summary(m_quasi)$dispersion

# 负二项（若装了 MASS）
library(MASS)
m_nb <- glm.nb(ndown ~ sector + offset(log(exposure)), data = indcnt)
c(AIC_pois = AIC(m_ind), AIC_nb = AIC(m_nb))
m_nb$theta

# 15.5 分段常数强度模型 ----
# 把月分成时间段，看下跌频率是否随时间变化
d <- indcnt
d$period <- cut(as.numeric(substr(d$ym, 1, 4)),
                breaks = c(2023, 2024, 2025, 2027),
                labels = c("2024", "2025", "2026"), include.lowest = TRUE)
m_yr <- glm(ndown ~ period, data = d, family = poisson,
            offset = log(exposure))
summary(m_yr)
exp(coef(m_yr))
anova(m_yr, test = "Chisq")

# 用时间段解释
agg2 <- aggregate(cbind(ndown, exposure) ~ period, d, sum)
agg2$rate <- agg2$ndown / agg2$exposure
print(agg2)

# 15.6 用连续协变量 ----
# 波动率越高的月份，下跌天数越多
m_sd <- glm(ndown ~ sd_m, data = indcnt, family = poisson,
            offset = log(exposure))
summary(m_sd)
exp(coef(m_sd))
# 解释：月波动率每增加 1 个百分点（0.01），率变为原来的多少倍
exp(coef(m_sd)["sd_m"] * 0.01)

# 作图
plot(indcnt$sd_m, indcnt$ndown / indcnt$exposure, pch = 19, cex = 0.6,
     col = rgb(0, 0, 0, 0.35), xlab = "月度波动率", ylab = "下跌日占比",
     main = "波动率与下跌频率")
xs <- seq(min(indcnt$sd_m), max(indcnt$sd_m), length.out = 100)
pr <- predict(m_sd, data.frame(sd_m = xs, exposure = 21), type = "response")
lines(xs, pr / 21, col = "red", lwd = 2)

# 15.7 两个计数变量 ----
# nbig：单日涨跌超 2% 的天数
m2 <- glm(nbig ~ sector, data = indcnt, family = poisson,
          offset = log(exposure))
c(AIC_down = AIC(m_ind), AIC_big = AIC(m2))

# 15.8 习题 ----
# 15.1 用 exposure 不取 offset，改用 rate 作为因变量做加权回归，比较结果
# 15.2 检验 ndown 是否符合泊松分布（用方差均值比）
# 15.3 把 period 换成按季度划分，看季节性
# 15.4 用负二项模型重新估计行业效应，与泊松的系数比较
