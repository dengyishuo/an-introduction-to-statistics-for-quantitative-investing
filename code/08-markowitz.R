# 《量化投资的统计学导论》第 8 章 均值—方差组合优化 —— 示例代码
# 依赖: data/load_datasets.R（xquant-db 真实行情）、quadprog
# 运行: cd an-introduction-to-statistics-for-quantitative-investing
#       Rscript code/08-markowitz.R

source("data/load_datasets.R")
suppressMessages(library(quadprog))

RF <- 0.015   # 无风险利率（年化），参数假设，非数据

# 8.1 数据与参数估计 ----

R <- as.matrix(indday[, -1])
dim(R)

# 剔除上市不满全期的标的（缺失超过 5%）
keep <- colSums(is.na(R)) < 0.05 * nrow(R)
colnames(R)[!keep]
R <- R[, keep]
R <- R[complete.cases(R), ]
dim(R)

n  <- ncol(R)
mu <- colMeans(R) * 252
S  <- cov(R) * 252

# 单资产：年化收益、年化波动、夏普
asset <- data.frame(
  name   = colnames(R),
  ret    = mu,
  sd     = sqrt(diag(S)),
  sharpe = (mu - RF) / sqrt(diag(S))
)
asset[order(-asset$ret), ]

summary(asset$ret)
summary(asset$sd)

# 平均相关系数
CC <- cor(R)
mean(CC[upper.tri(CC)])

# 8.2 两资产：解析解 ----

i1 <- which(colnames(R) == "中证消费ETF")
i2 <- which(colnames(R) == "半导体ETF")
m2 <- mu[c(i1, i2)]
S2 <- S[c(i1, i2), c(i1, i2)]

m2
sqrt(diag(S2))
S2[1, 2]
S2[1, 2] / sqrt(S2[1, 1] * S2[2, 2])

# 组合权重扫描
w <- seq(0, 1, by = 0.01)
sd_p  <- sqrt(w^2 * S2[1, 1] + (1 - w)^2 * S2[2, 2] + 2 * w * (1 - w) * S2[1, 2])
ret_p <- w * m2[1] + (1 - w) * m2[2]

# 最小方差权重（解析解）
w_min <- (S2[2, 2] - S2[1, 2]) / (S2[1, 1] + S2[2, 2] - 2 * S2[1, 2])
w_min
sqrt(w_min^2 * S2[1, 1] + (1 - w_min)^2 * S2[2, 2] + 2 * w_min * (1 - w_min) * S2[1, 2])

# 与纯持有、等权对比
c(pure1 = sqrt(S2[1, 1]), pure2 = sqrt(S2[2, 2]),
  equal = sqrt(0.25 * S2[1, 1] + 0.25 * S2[2, 2] + 0.5 * S2[1, 2]),
  minvar = min(sd_p))

# 8.3 多资产：二次规划 ----

qp <- function(mu, S, short = FALSE, target = NULL) {
  n <- ncol(S)
  A <- matrix(rep(1, n), 1, n); b <- 1; meq <- 1
  if (!short) { A <- rbind(A, diag(n)); b <- c(b, rep(0, n)) }
  if (!is.null(target)) { A <- rbind(A, mu); b <- c(b, target); meq <- 2 }
  tryCatch(solve.QP(2 * S, rep(0, n), t(A), b, meq = meq)$solution,
           error = function(e) NULL)
}
vsd <- function(w, S) sqrt(as.numeric(crossprod(w, S %*% w)))

w_eq <- rep(1 / n, n); names(w_eq) <- colnames(R)
w_mv <- qp(mu, S);     names(w_mv) <- colnames(R)

# 有效前沿：按目标收益率逐点求解
tg <- seq(sum(w_mv * mu), max(mu), length.out = 120)
front <- do.call(rbind, lapply(tg, function(m) {
  w <- qp(mu, S, target = m)
  if (is.null(w)) return(NULL)
  data.frame(target = m, ret = sum(w * mu), sd = vsd(w, S))
}))
front$sharpe <- (front$ret - RF) / front$sd

# 最大夏普组合
bt <- front$target[which.max(front$sharpe)]
w_ms <- qp(mu, S, target = bt); names(w_ms) <- colnames(R)

# 三种组合对比
port <- function(w, lab) {
  data.frame(name = lab, ret = sum(w * mu), sd = vsd(w, S),
             sharpe = (sum(w * mu) - RF) / vsd(w, S), npos = sum(w > 1e-6))
}
rbind(port(w_eq, "等权"), port(w_mv, "最小方差"), port(w_ms, "最大夏普"))

sort(w_mv[w_mv > 1e-6], decreasing = TRUE)
sort(w_ms[w_ms > 1e-6], decreasing = TRUE)

# 8.4 约束的影响：允许做空 ----

w_short <- qp(mu, S, short = TRUE); names(w_short) <- colnames(R)
c(sd_long = vsd(w_mv, S), sd_short = vsd(w_short, S))
min(w_short)
sum(w_short[w_short < 0])
sort(w_short)[1:3]

# 8.5 估计误差：同一份数据，换一段就换一套权重 ----

h1 <- 1:250
R1 <- R[h1, ]; mu1 <- colMeans(R1) * 252; S1 <- cov(R1) * 252
w_mv1 <- qp(mu1, S1); names(w_mv1) <- colnames(R)

# 两段样本的最小方差权重差异
sum(abs(w_mv1 - w_mv))
head(sort(w_mv1[w_mv1 > 1e-6], decreasing = TRUE), 5)
head(sort(w_mv[w_mv > 1e-6], decreasing = TRUE), 5)

# 前期估出的最大夏普组合
tg1 <- seq(sum(w_mv1 * mu1), max(mu1), length.out = 100)
fr1 <- do.call(rbind, lapply(tg1, function(m) {
  w <- qp(mu1, S1, target = m); if (is.null(w)) return(NULL)
  data.frame(target = m, ret = sum(w * mu1), sd = vsd(w, S1))
}))
fr1$sharpe <- (fr1$ret - RF) / fr1$sd
bt1 <- fr1$target[which.max(fr1$sharpe)]
w_ms1 <- qp(mu1, S1, target = bt1); names(w_ms1) <- colnames(R)
sort(w_ms1[w_ms1 > 1e-6], decreasing = TRUE)

# 样本内表现
c(ret = sum(w_ms1 * mu1), sd = vsd(w_ms1, S1),
  sharpe = (sum(w_ms1 * mu1) - RF) / vsd(w_ms1, S1))

# 样本外表现
R2 <- R[-h1, ]
for (nm in c("最大夏普", "等权", "最小方差")) {
  ww <- switch(nm, "最大夏普" = w_ms1, "等权" = w_eq, "最小方差" = w_mv1)
  pr <- as.numeric(R2 %*% ww)
  cat(sprintf("样本外 %-8s 年化 %6.2f%%  波动 %6.2f%%  夏普 %5.3f\n",
              nm, mean(pr) * 252 * 100, sd(pr) * sqrt(252) * 100,
              (mean(pr) * 252 - RF) / (sd(pr) * sqrt(252))))
}

# 8.6 一个补救：协方差收缩 ----

shrink <- function(S, delta) (1 - delta) * S + delta * diag(diag(S))
for (d in c(0, 0.2, 0.5)) {
  wd <- qp(mu, shrink(S, d)); names(wd) <- colnames(R)
  # 统一在原始协方差 S 下评估，保证可比
  cat(sprintf("收缩 %3.0f%%: 波动 %6.2f%%  收益 %6.2f%%  持仓 %2d  最大权重 %5.3f\n",
              d * 100, vsd(wd, S) * 100, sum(wd * mu) * 100,
              sum(wd > 1e-6), max(wd)))
}

# 8.7 习题 ----
# 8.1 把 RF 从 1.5% 改为 3%，最大夏普组合的持仓如何变化？
# 8.2 用 idxret 的宽基 ETF 重做一遍，比较与行业 ETF 的差异。
# 8.3 把长仓下限定为 5%（不允许小仓位），有效前沿如何移动？
# 8.4 用 2024 年数据估计、2025 年验证，重跑 8.5 节的样本外检验。
