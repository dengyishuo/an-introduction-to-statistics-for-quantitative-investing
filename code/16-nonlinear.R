# 《量化投资的统计学导论》第 16 章 非线性曲线拟合 —— 示例代码
# 运行: Rscript code/16-nonlinear.R

source("data/finwr.R")
cairo_pdf(file.path(tempdir(), "ch16.pdf"), width = 9, height = 6,
          family = "PingFang SC")
on.exit(dev.off(), add = TRUE)

# 16.1 基本用法 ----
# 核心例子：分散化曲线
# 随机选 n 只股票等权持有，组合波动随 n 下降，但下降速度递减
d <- divcurve
d
plot(d$n, d$sd, pch = 19, log = "x", xlab = "持股数量 n（对数轴）",
     ylab = "组合月度波动", main = "分散化曲线")

# 理论模型：两两相关系数同为 rho 时
# sigma(n) = s1 * sqrt(rho + (1 - rho) / n)
m <- nls(sd ~ s1 * sqrt(rho + (1 - rho) / n), data = d,
         start = list(s1 = 0.126, rho = 0.25))
summary(m)
coef(m)
# confint 的默认剖面法在此参数化下不稳定，用渐近区间
confint(m, method = "asymptotic")

# 拟合优度
pred <- fitted(m)
c(R2 = 1 - sum((d$sd - pred)^2) / sum((d$sd - mean(d$sd))^2),
  resid_se = summary(m)$sigma)

# 参数的含义
s1 <- coef(m)["s1"]; rho <- coef(m)["rho"]
c(single_stock_vol = s1,
  avg_correlation = rho,
  limit_vol = s1 * sqrt(rho))

# 作图
xs <- seq(1, 500, length.out = 300)
ys <- s1 * sqrt(rho + (1 - rho) / xs)
plot(d$n, d$sd, pch = 19, log = "x", xlab = "持股数量 n（对数轴）",
     ylab = "组合月度波动", main = "分散化曲线与理论拟合")
lines(xs, ys, col = "red", lwd = 2)
abline(h = s1 * sqrt(rho), lty = 2, col = "blue")
text(3, s1 * sqrt(rho) * 1.03, "极限波动 = s1·√rho", col = "blue", cex = 0.85)

# 16.2 寻找起始值 ----
# nls 对初值敏感，初值不好会报错或不收敛
bad <- try(nls(sd ~ s1 * sqrt(rho + (1 - rho) / n), data = d,
               start = list(s1 = 1, rho = 10)), silent = TRUE)
class(bad)

# 从图形读初值：n=1 处约等于个股波动，n→∞ 处约等于极限
c(s1_guess = d$sd[d$n == 1], limit_guess = tail(d$sd, 1))
c(rho_guess = (tail(d$sd, 1) / d$sd[d$n == 1])^2)

# 网格搜索初值
grid <- expand.grid(s1 = seq(0.10, 0.15, 0.005), rho = seq(0.15, 0.40, 0.01))
grid$sse <- apply(grid, 1, function(g) {
  fit <- try(nls(sd ~ s1 * sqrt(rho + (1 - rho) / n), data = d,
                 start = list(s1 = g[1], rho = g[2])), silent = TRUE)
  if (inherits(fit, "try-error")) return(Inf)
  sum(resid(fit)^2)
})
best <- grid[which.min(grid$sse), ]
best

# 用最优初值重新拟合
m2 <- nls(sd ~ s1 * sqrt(rho + (1 - rho) / n), data = d,
          start = list(s1 = best$s1, rho = best$rho))
coef(m2)

# 16.3 自启动模型 ----
# R 内置的自启动模型，不必手填初值
m3 <- nls(sd ~ SSasymp(n, Asym, r0, lrc), data = d)
summary(m3)
coef(m3)
# Asym 是 n→∞ 的渐近值，与 s1*sqrt(rho) 应对应
c(asymp = coef(m3)["Asym"], theory = s1 * sqrt(rho))

# 其他自启动模型（SSasympOrig、SSlogis 等）要求曲线形状匹配，
# 本数据的波动随 n 单调递减并在 0.063 处饱和，用 SSasymp 最合适。
# 若换一个递增型的自启动模型，自启动函数会因初值越界而失败：
bad <- try(nls(sd ~ SSasympOrig(n, Asym, lrc), data = d), silent = TRUE)
cat("SSasympOrig 是否可用:", !inherits(bad, "try-error"), "\n")

# 16.4 剖面分析 ----
# 固定 rho，看 s1 的最优值如何变化
rhos <- seq(0.15, 0.40, 0.02)
prof <- t(sapply(rhos, function(r) {
  fit <- try(nls(sd ~ s1 * sqrt(r + (1 - r) / n), data = d,
                 start = list(s1 = 0.13)), silent = TRUE)
  if (inherits(fit, "try-error")) return(c(s1 = NA, sse = NA))
  c(s1 = coef(fit)["s1"], sse = sum(resid(fit)^2))
}))
prof <- data.frame(rho = rhos, prof)
print(round(prof, 5))

plot(prof$rho, prof$sse, type = "b", pch = 19, xlab = "固定 rho",
     ylab = "残差平方和", main = "剖面：rho 的置信区间从最低点附近读出")
abline(v = rho, col = "red", lty = 2)
abline(h = min(prof$sse) * (1 + 3.84 / length(d$n)), col = "blue", lty = 2)

# 16.5 更精细地控制拟合算法 ----
# 指定算法与迭代控制
m5 <- nls(sd ~ s1 * sqrt(rho + (1 - rho) / n), data = d,
          start = list(s1 = 0.126, rho = 0.25),
          algorithm = "port",
          lower = c(s1 = 0, rho = 0), upper = c(s1 = 1, rho = 1))
coef(m5)

# 加权拟合：n 大的点更稳定（重复抽样 300 次，标准误更小）
d$w <- 1 / (1 + log(d$n))
m6 <- nls(sd ~ s1 * sqrt(rho + (1 - rho) / n), data = d,
          start = list(s1 = 0.126, rho = 0.25), weights = w)
coef(m6)

# 16.6 其他非线性模型的比较 ----
# 幂函数：sigma = a * n^(-b)
m7 <- nls(sd ~ a * n^(-b), data = d, start = list(a = 0.12, b = 0.2))
c(AIC_theory = AIC(m), AIC_power = AIC(m7))
coef(m7)

# 指数衰减：sigma = c0 + c1 * exp(-k*n)
# 注意：k 较大时 exp(-k*n) 下溢，数值导数会失败，故用 port 算法加边界
m8 <- tryCatch(
  nls(sd ~ c0 + c1 * exp(-k * n), data = d,
      start = list(c0 = 0.06, c1 = 0.07, k = 0.05),
      algorithm = "port",
      lower = c(c0 = 0, c1 = 0, k = 1e-4), upper = c(c0 = 0.2, c1 = 0.2, k = 1)),
  error = function(e) NULL)
if (!is.null(m8)) {
  c(AIC_theory = AIC(m), AIC_exp = AIC(m8))
  coef(m8)
} else {
  cat("指数模型未收敛，改用自启动渐近模型 SSasymp 的结果\n")
}

# 三个模型的拟合对比
xs <- seq(1, 500, length.out = 300)
plot(d$n, d$sd, pch = 19, log = "x", xlab = "持股数量 n（对数轴）",
     ylab = "组合月度波动", main = "三种非线性模型")
lines(xs, predict(m, data.frame(n = xs)), col = "red", lwd = 2)
lines(xs, predict(m7, data.frame(n = xs)), col = "blue", lwd = 2, lty = 2)
lines(xs, predict(m8, data.frame(n = xs)), col = "darkgreen", lwd = 2, lty = 3)
legend("topright", c("理论模型 √(rho+(1-rho)/n)", "幂函数", "指数衰减"),
       col = c("red", "blue", "darkgreen"), lty = c(1, 2, 3), lwd = 2, bty = "n")

# 16.7 一个经济学解释 ----
# rho 就是市场的平均相关。它决定了分散化的天花板
# rho = 0.243 意味着：即使持有全市场，也只能消掉约 1 - sqrt(rho) 的波动
c(vol_reduction = 1 - sqrt(rho))
c(observed = 1 - tail(d$sd, 1) / d$sd[d$n == 1])

# 需要多少只股票才能达到极限的 90% 效果
target <- s1 * sqrt(rho) + 0.1 * (s1 - s1 * sqrt(rho))
n_needed <- (1 - rho) / ((target / s1)^2 - rho)
c(n_needed = ceiling(n_needed))

# 16.8 习题 ----
# 16.1 用 divcurve 的前 15 行（n ≤ 50）重拟合，rho 的估计变化多少
# 16.2 比较算法 "default" 与 "port"，收敛结果是否一致
# 16.3 用 SSlogis 自启动模型拟合，比较 AIC
# 16.4 如果 rho 降到 0.1，极限波动与 n_needed 各是多少
