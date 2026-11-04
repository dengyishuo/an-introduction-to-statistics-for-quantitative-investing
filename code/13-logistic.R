# 《量化投资的统计学导论》第 13 章 逻辑回归 —— 示例代码
# 运行: Rscript code/13-logistic.R

source("data/load_datasets.R")
cairo_pdf(file.path(tempdir(), "ch13.pdf"), width = 9, height = 6,
          family = "PingFang SC")
on.exit(dev.off(), add = TRUE)

# 13.1 广义线性模型的基本思想 ----
# 线性回归预测连续值；逻辑回归预测概率，链接函数是 logit
# logit(p) = log(p / (1-p)) = a + b*x

# 13.2 表格数据的逻辑回归 ----
# 用 drawdown：按波动率分组，各组触发 30% 回撤的比例
d <- drawdown[complete.cases(drawdown[, c("event", "vol_12m", "size", "industry")]), ]
# 行业水平不足 40 只的并入前先剔除，保证后续模型共用同一批样本
d$ind <- factor(d$industry)
d <- d[d$ind %in% names(which(table(d$ind) > 40)), ]
d$ind <- droplevels(d$ind)
d$size_c <- as.numeric(scale(d$size))
nrow(d)
d$vg <- cut(d$vol_12m, quantile(d$vol_12m, 0:5/5),
            include.lowest = TRUE, labels = FALSE)
tab <- aggregate(cbind(event, vol_12m) ~ vg, d, function(z) mean(z))
tab$n <- as.numeric(table(d$vg))
names(tab)[2] <- "p_event"
print(round(tab, 3))

# 13.3 用原始数据拟合 ----
m <- glm(event ~ vol_12m, data = d, family = binomial)
summary(m)

# 系数含义：log 优势比
coef(m)
exp(coef(m))
# 波动率每上升 1 个单位，优势比变为 exp(b)
b <- coef(m)["vol_12m"]
c(or = exp(b), pct_change = (exp(b) - 1) * 100)

# 95% 置信区间
exp(confint(m))

# 13.4 以优势比形式呈现 ----
exp(cbind(OR = coef(m), confint(m)))

# 13.5 偏差分析表 ----
anova(m, test = "Chisq")
# 零模型对比
m0 <- glm(event ~ 1, data = d, family = binomial)
anova(m0, m, test = "Chisq")

# 伪 R2（McFadden）
1 - as.numeric(logLik(m) / logLik(m0))

# 13.6 预测 ----
newd <- data.frame(vol_12m = c(0.10, 0.20, 0.30, 0.40, 0.50))
pred <- predict(m, newd, type = "link", se.fit = TRUE)
newd$p <- plogis(pred$fit)
newd$lo <- plogis(pred$fit - 1.96 * pred$se.fit)
newd$hi <- plogis(pred$fit + 1.96 * pred$se.fit)
round(newd, 3)

# 作图
xs <- seq(min(d$vol_12m), max(d$vol_12m), length.out = 200)
pr <- predict(m, data.frame(vol_12m = xs), type = "link", se.fit = TRUE)
plot(d$vol_12m, d$event, pch = 19, cex = 0.3, col = rgb(0, 0, 0, 0.08),
     xlab = "过去 12 个月波动率", ylab = "触发 30% 回撤的概率",
     main = "逻辑回归拟合")
lines(xs, plogis(pr$fit), col = "red", lwd = 2)
lines(xs, plogis(pr$fit - 1.96 * pr$se.fit), col = "red", lty = 2)
lines(xs, plogis(pr$fit + 1.96 * pr$se.fit), col = "red", lty = 2)
abline(h = 0.5, col = "grey60", lty = 3)

# 13.7 多变量逻辑回归 ----
# 加入市值（size_c 已在 13.2 节标准化）
m2 <- glm(event ~ vol_12m + size_c, data = d, family = binomial)
summary(m2)
exp(cbind(OR = coef(m2), confint(m2)))

# 与单变量模型比较
c(single = AIC(m), multi = AIC(m2))
anova(m, m2, test = "Chisq")

# 加入行业（分类变量，已在前文准备好）
m3 <- glm(event ~ vol_12m + size_c + ind, data = d, family = binomial)
c(AIC_simple = AIC(m2), AIC_with_ind = AIC(m3))
anova(m2, m3, test = "Chisq")

# 13.8 模型检查 ----
# 预测概率分布
d$phat <- fitted(m3)
summary(d$phat)

# 十分位校准：预测概率 vs 实际事件率
d$dec <- cut(d$phat, quantile(d$phat, 0:10/10), include.lowest = TRUE,
             labels = FALSE)
cal <- aggregate(cbind(phat, event) ~ dec, d, mean)
round(cal, 3)

# Hosmer-Lemeshow 型检验（用卡方近似）
hl <- sum((cal$event - cal$phat)^2 / (cal$phat * (1 - cal$phat) /
            as.numeric(table(d$dec))))
c(HL_stat = hl, df = nrow(cal) - 2, p = pchisq(hl, nrow(cal) - 2, lower.tail = FALSE))

# 13.9 用逻辑回归做选股打分 ----
# 目标：下月是否跑赢同月中位数
e <- mlevent[mlevent$ym <= "2024-12", ]
e <- e[complete.cases(e[, c("win", "size", "turnover", "vol_12m", "rev_1m")]), ]
e$size_c <- as.numeric(scale(e$size))
ml <- glm(win ~ size_c + turnover + vol_12m + rev_1m,
          data = e, family = binomial)
summary(ml)$coefficients

# 样本外评估
te <- mlevent[mlevent$ym >= "2025-01", ]
te <- te[complete.cases(te[, c("win", "size", "turnover", "vol_12m", "rev_1m")]), ]
te$size_c <- as.numeric(scale(te$size))
te$phat <- predict(ml, te, type = "response")

# 按预测概率分十组，看实际胜率是否单调
te$dec <- cut(te$phat, quantile(te$phat, 0:10/10), include.lowest = TRUE,
              labels = FALSE)
round(tapply(te$win, te$dec, mean), 3)

# AUC（简化计算）
auc <- function(y, p) {
  r <- rank(p)
  (sum(r[y == 1]) - sum(y == 1) * (sum(y == 1) + 1) / 2) /
    (sum(y == 1) * sum(y == 0))
}
c(AUC = auc(te$win, te$phat))

# 13.10 习题 ----
# 13.1 把事件改成 big_up（下月涨超 10%），重跑 13.3
# 13.2 用 indcnt 的 ndown/exposure 做泊松回归，与逻辑回归比较
# 13.3 计算 13.9 模型的准确率、精确率与召回率
# 13.4 在 13.9 中加入 mom_12_1，AUC 是否提升
