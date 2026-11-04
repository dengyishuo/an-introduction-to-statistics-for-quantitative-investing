# 《量化投资的统计学导论》第 9 章 机器学习与多因子模型 —— 示例代码
# 依赖: data/finwr.R（mlpanel）、randomForest、xgboost、MASS
# 运行: cd an-introduction-to-statistics-for-quantitative-investing
#       Rscript code/09-ml-factor.R
#
# 注：随机森林与 XGBoost 训练较慢（本机 8 GB 内存约 5-10 分钟），
#     代码中已下调树的数量与抽样规模以适配普通笔记本。

source("data/finwr.R")
suppressMessages({
  library(randomForest)
  library(xgboost)
  library(MASS)
})
set.seed(42)

# 9.1 面板数据 ----

FEATS <- c("mom_12_1", "mom_6_1", "rev_1m", "vol_12m", "turnover", "size")

d <- mlpanel[, c("secucode", "ym", "fwd_ret", FEATS)]
d <- d[complete.cases(d), ]
cat("样本:", nrow(d), "行 |", length(unique(d$secucode)), "只 |",
    length(unique(d$ym)), "个月\n")

head(d, 4)
summary(d$fwd_ret)

# 每月股票数
table(d$ym)[1:5]

# 9.2 截面标准化 ----
# 因子值必须做截面（同月）标准化，否则不同月份的绝对水平不可比

for (f in FEATS) {
  d[[f]] <- ave(d[[f]], d$ym, FUN = function(x) {
    q <- quantile(x, c(0.01, 0.99), na.rm = TRUE)
    as.numeric(scale(pmin(pmax(x, q[1]), q[2])))
  })
}
# 标签同样去极值，避免单月极端值主导损失函数
d$fwd_ret <- ave(d$fwd_ret, d$ym, FUN = function(x) {
  q <- quantile(x, c(0.01, 0.99), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
})

# 9.3 训练/测试切分：按时间，不能随机 ----

TR <- d$ym <= "2024-12"
TE <- d$ym >= "2025-01"
c(train_rows = sum(TR), train_months = length(unique(d$ym[TR])),
  test_rows = sum(TE), test_months = length(unique(d$ym[TE])))

# 9.4 评价指标：IC 与 ICIR ----

ic_by_month <- function(dat, pred) {
  v <- sapply(split(dat, dat$ym), function(x) {
    if (nrow(x) < 50) return(NA_real_)
    cor(x[[pred]], x$fwd_ret, method = "spearman")
  })
  v[!is.na(v)]
}
report <- function(v, label) {
  cat(sprintf("%-18s meanIC=%+.4f  sd=%.4f  ICIR=%+.2f  正IC月占比=%2.0f%%\n",
              label, mean(v), sd(v), mean(v) / sd(v), mean(v > 0) * 100))
}

# 9.5 单因子基线 ----
# 把因子取负号，使其方向与「预期收益为正」一致，便于横向比较

for (f in FEATS) {
  d$.p <- -d[[f]]
  report(ic_by_month(d[TE, ], ".p"), f)
}

# 9.6 线性多因子：OLS ----

fml <- as.formula(paste("fwd_ret ~", paste(FEATS, collapse = " + ")))
m_ols <- lm(fml, data = d[TR, ])
d$.ols <- predict(m_ols, d)
report(ic_by_month(d[TR, ], ".ols"), "OLS 样本内")
report(ic_by_month(d[TE, ], ".ols"), "OLS 样本外")
round(coef(m_ols), 5)

# 信噪比：R^2 有多低
r2_in <- summary(m_ols)$r.squared
y_te <- d$fwd_ret[TE]; pr_te <- d$.ols[TE]
r2_out <- 1 - sum((y_te - pr_te)^2) / sum((y_te - mean(y_te))^2)
c(R2_in = r2_in, R2_out = r2_out)

# 9.7 岭回归 ----

Xtr <- as.matrix(d[TR, FEATS]); ytr <- d$fwd_ret[TR]
Xte <- as.matrix(d[TE, FEATS])
rg <- lm.ridge(ytr ~ Xtr, lambda = 10^seq(-2, 4, length.out = 25))
bl <- which.min(rg$GCV)
rg$lambda[bl]
cf <- coef(rg)[bl, ]
Xs <- function(X) scale(X, center = rg$xm, scale = rg$scales)
d$.ridge <- drop(Xs(as.matrix(d[, FEATS])) %*% cf[-1]) + cf[1]
report(ic_by_month(d[TR, ], ".ridge"), "Ridge 样本内")
report(ic_by_month(d[TE, ], ".ridge"), "Ridge 样本外")

# 9.8 随机森林 ----

sub <- sample(which(TR), min(100000, sum(TR)))
rf <- randomForest(x = as.matrix(d[sub, FEATS]), y = d$fwd_ret[sub],
                   ntree = 120, mtry = 3, nodesize = 100, importance = TRUE)
d$.rf <- predict(rf, as.matrix(d[, FEATS]))
report(ic_by_month(d[TR, ], ".rf"), "RF 样本内")
report(ic_by_month(d[TE, ], ".rf"), "RF 样本外")
round(sort(importance(rf)[, 1], decreasing = TRUE), 2)

# 9.9 梯度提升：XGBoost ----

dtr <- xgb.DMatrix(as.matrix(d[TR, FEATS]), label = d$fwd_ret[TR])
dte <- xgb.DMatrix(as.matrix(d[TE, FEATS]))
xgb <- xgb.train(
  params = list(eta = 0.05, max_depth = 4, subsample = 0.8,
                colsample_bytree = 0.8, objective = "reg:squarederror",
                nthread = 2),
  data = dtr, nrounds = 150, verbose = 0)
d$.xgb <- NA
d$.xgb[TE] <- predict(xgb, dte)
report(ic_by_month(d[TE, ], ".xgb"), "XGB 样本外")

imp <- xgb.importance(feature_names = FEATS, model = xgb)
head(imp[, c("Feature", "Gain")], 6)

# 9.10 十分位多空组合 ----

decile <- function(dat, pred, label) {
  r <- sapply(split(dat, dat$ym), function(x) {
    x <- x[!is.na(x[[pred]]), ]
    if (nrow(x) < 100) return(NA_real_)
    q <- cut(rank(x[[pred]]), 10, labels = FALSE)
    mean(x$fwd_ret[q == 10]) - mean(x$fwd_ret[q == 1])
  })
  r <- r[!is.na(r)]
  cat(sprintf("%-10s 月均 %+.2f%%  年化 %+.1f%%  胜率 %2.0f%%\n",
              label, mean(r) * 100, mean(r) * 12 * 100, mean(r > 0) * 100))
  invisible(r)
}
for (m in c(".ols", ".ridge", ".rf", ".xgb")) decile(d[TE, ], m, m)

# 单调性：XGB 打分下，各十分位的平均月收益
mono <- sapply(split(d[TE, ], d[TE, ]$ym), function(x) {
  q <- cut(rank(x$.xgb), 10, labels = FALSE)
  tapply(x$fwd_ret, q, mean)
})
round(rowMeans(mono, na.rm = TRUE) * 100, 2)

# 9.11 过拟合演示：训练集越小，差距越大 ----

for (k in c(6, 12, 24, 48)) {
  months <- sort(unique(d$ym[TR]))[1:k]
  idx <- d$ym %in% months
  if (sum(idx) < 5000) next
  s <- sample(which(idx), min(60000, sum(idx)))
  m <- randomForest(x = as.matrix(d[s, FEATS]), y = d$fwd_ret[s],
                    ntree = 80, mtry = 3, nodesize = 100)
  d$.tmp <- predict(m, as.matrix(d[, FEATS]))
  ins <- mean(ic_by_month(d[idx, ], ".tmp"), na.rm = TRUE)
  oos <- mean(ic_by_month(d[TE, ], ".tmp"), na.rm = TRUE)
  cat(sprintf("训练 %2d 个月: 样本内 IC=%+.3f  样本外 IC=%+.3f  差距=%.3f\n",
              k, ins, oos, ins - oos))
}

# 9.12 习题 ----
# 9.1 把 fwd_ret 换成下月收益的截面排名（而非原值），重跑 OLS。IC 有变化吗？
# 9.2 加入财务因子：把 mlfin 的 roe、debt_ratio、gross_margin 按公告日
#     对齐到月份，看 IC 是否提升。注意只能用公告日之前可得的报告。
# 9.3 把训练期改为 2021-2023，测试期改为 2024-2026，重跑 9.8 与 9.9。
#     随机森林的样本外 IC 是否稳定？
# 9.4 给十分位多空加上单边万分之五的交易成本（月换手约 100%），
#     年化收益还剩多少？
# 9.5 用 industry 做行业中性化（每个行业内做截面标准化），再看 ICIR。
