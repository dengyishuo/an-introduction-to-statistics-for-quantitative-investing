# 《量化投资的统计学导论》第 14 章 生存分析 —— 示例代码
# 运行: Rscript code/14-survival.R

source("data/load_datasets.R")
suppressMessages(library(survival))
cairo_pdf(file.path(tempdir(), "ch14.pdf"), width = 9, height = 6,
          family = "PingFang SC")
on.exit(dev.off(), add = TRUE)

# 14.1 基本概念 ----
# 数据：随访 12 个月，事件 = 累计回撤达到 30%
# 删失：随访结束时仍未触发（这部分信息不能丢）
d <- drawdown
nrow(d)
table(d$event)
round(mean(d$event), 3)
summary(d$months)

# 事件与删失的分布
tapply(d$months, d$event, summary)

# 14.2 生存对象 ----
S <- Surv(d$months, d$event)
head(S, 10)

# 14.3 Kaplan-Meier 估计 ----
km <- survfit(S ~ 1, data = d)
km
summary(km)
# 中位生存时间
km$time[which.min(abs(km$surv - 0.5))]

plot(km, xlab = "月数", ylab = "未触发回撤的概率",
     main = "Kaplan-Meier 曲线（全体）", conf.int = TRUE,
     xlim = c(0, 12), mark.time = TRUE)

# 按行业分组
d$ind <- factor(d$industry)
d$ind <- droplevels(d$ind)
d2 <- d[d$ind %in% names(which(table(d$ind) > 80)), ]
d2$ind <- droplevels(d2$ind)
km2 <- survfit(Surv(months, event) ~ ind, data = d2)
km2
plot(km2, col = 1:length(levels(d2$ind)), lty = 1,
     xlab = "月数", ylab = "未触发回撤的概率",
     main = "按行业的 KM 曲线", xlim = c(0, 12))
legend("bottomleft", levels(d2$ind), col = 1:length(levels(d2$ind)),
       lty = 1, cex = 0.7, bty = "n")

# 14.4 对数秩检验 ----
survdiff(Surv(months, event) ~ ind, data = d2)

# 两个行业单独比
sub <- d[d$ind %in% c("银行", "电子"), ]
sub$ind <- droplevels(sub$ind)
survdiff(Surv(months, event) ~ ind, data = sub)
survdiff(Surv(months, event) ~ ind, data = sub, rho = 1)   # Peto 型

# 按波动率分组（更能说明问题）
d$vq <- cut(d$vol_12m, quantile(d$vol_12m, c(0, 1/3, 2/3, 1), na.rm = TRUE),
            labels = c("低波动", "中波动", "高波动"), include.lowest = TRUE)
km3 <- survfit(Surv(months, event) ~ vq, data = d)
plot(km3, col = c("steelblue", "darkgreen", "firebrick"), lty = 1, lwd = 2,
     xlab = "月数", ylab = "未触发回撤的概率",
     main = "按波动率分组的 KM 曲线", xlim = c(0, 12))
legend("bottomleft", c("低波动", "中波动", "高波动"),
       col = c("steelblue", "darkgreen", "firebrick"), lwd = 2, bty = "n")
survdiff(Surv(months, event) ~ vq, data = d)

# 14.5 Cox 比例风险模型 ----
# 连续协变量
m1 <- coxph(Surv(months, event) ~ vol_12m, data = d)
summary(m1)
exp(coef(m1))
exp(confint(m1))

# 多变量
d$size_c <- as.numeric(scale(d$size))
d$turnover_c <- as.numeric(scale(d$turnover))
m2 <- coxph(Surv(months, event) ~ vol_12m + size_c, data = d)
summary(m2)
exp(cbind(HR = coef(m2), confint(m2)))

m3 <- coxph(Surv(months, event) ~ vol_12m + size_c + turnover_c, data = d)
summary(m3)
# 模型比较
anova(m1, m2, m3)

# 比例风险假设检验
cox.zph(m3)

# 14.6 预测与风险分层 ----
# 用 Cox 模型的线性预测子分组
d$risk <- predict(m3, newdata = d, type = "lp")
d$rg <- cut(d$risk, quantile(d$risk, 0:3/3, na.rm = TRUE), include.lowest = TRUE,
            labels = c("低风险", "中风险", "高风险"))
km4 <- survfit(Surv(months, event) ~ rg, data = d)
plot(km4, col = c("steelblue", "darkgreen", "firebrick"), lwd = 2,
     xlab = "月数", ylab = "未触发回撤的概率",
     main = "按 Cox 风险分组的 KM 曲线", xlim = c(0, 12))
legend("bottomleft", levels(d$rg), col = c("steelblue", "darkgreen", "firebrick"),
       lwd = 2, bty = "n")

# 12 个月内的累计事件率
summary(km4, times = 11)

# 14.7 参数化生存模型 ----
# 假设风险恒定：指数模型
me <- survreg(Surv(months, event) ~ vol_12m, data = d, dist = "exponential")
summary(me)

# Weibull（风险随时间变化）
mw <- survreg(Surv(months, event) ~ vol_12m, data = d, dist = "weibull")
summary(mw)
c(AIC_exp = AIC(me), AIC_weib = AIC(mw))

# 14.8 习题 ----
# 14.1 把事件阈值改为 20% 与 40%，观察事件率与中位时间的变化
# 14.2 用 drawdown 的 turnover 单独建模，HR 是多少
# 14.3 对 14.5 的 m3 做残差诊断，找出异常个股
# 14.4 用 lognormal 分布重拟合 14.7，与 Weibull 比较 AIC
