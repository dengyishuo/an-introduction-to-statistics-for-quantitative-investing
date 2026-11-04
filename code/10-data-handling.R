# 《量化投资的统计学导论》第 10 章 高级数据处理 —— 示例代码
# 运行: Rscript code/10-data-handling.R

source("data/finwr.R")

# 10.1 变量重编码 ----

# 10.1.1 cut：把连续变量分箱
d <- mlpanel[mlpanel$ym == "2026-06", ]
d$size_g <- cut(d$size, 5)
table(d$size_g)

# 自定义断点与标签
d$size_g2 <- cut(d$size, breaks = quantile(d$size, 0:5/5),
                 labels = c("最小", "较小", "中等", "较大", "最大"),
                 include.lowest = TRUE)
table(d$size_g2)
tapply(d$fwd_ret, d$size_g2, mean)

# 10.1.2 因子水平操作
f <- factor(c("银行", "医药", "银行", "计算机"))
levels(f)
droplevels(f)
factor(f, levels = c("计算机", "银行", "医药"))
relevel(factor(c("银行", "医药", "计算机")), ref = "计算机")

# 10.1.3 处理日期
x <- as.Date("2026-06-30")
format(x, "%Y年%m月")
as.numeric(x - as.Date("2026-01-01"))
seq(as.Date("2026-01-01"), by = "month", length.out = 4)
weekdays(as.Date("2026-06-30"))
months(as.Date("2026-06-30"))

# 从字符串解析
as.Date("2026/06/30", format = "%Y/%m/%d")
ym <- "2026-06"
as.Date(paste0(ym, "-01"))

# 日期差与月份数
d1 <- as.Date(c("2024-01-03", "2025-06-30"))
diff(d1)

# 10.1.4 多个变量的重编码
d2 <- mlpanel[1:3, c("secucode", "size", "turnover", "vol_12m")]
d2[, c("size", "turnover")] <- lapply(d2[, c("size", "turnover")], function(z) round(z, 2))
d2

# 10.2 条件计算 ----
x <- c(-0.05, -0.01, 0, 0.02, 0.08)
ifelse(x > 0, "涨", "跌")
ifelse(x > 0.03, "大涨", ifelse(x < -0.03, "大跌", "平常"))

# 按条件赋值
y <- x
y[x > 0.03] <- 0.03
y[x < -0.03] <- -0.03
y

# 用向量化替代循环
set.seed(1)
z <- rnorm(5)
res <- numeric(5)
for (i in seq_along(z)) res[i] <- if (z[i] > 0) "正" else "负"
res
ifelse(z > 0, "正", "负")

# 10.3 数据框的合并与重构 ----

# 10.3.1 追加（行堆叠）
a <- data.frame(code = c("A", "B"), ret = c(0.01, -0.02))
b <- data.frame(code = c("C", "D"), ret = c(0.03, 0.00))
rbind(a, b)

# 10.3.2 合并（列连接）
meta <- data.frame(code = c("A", "B", "E"), name = c("甲", "乙", "戊"))
merge(a, meta, by = "code")                    # 内连接
merge(a, meta, by = "code", all.x = TRUE)      # 左连接
merge(a, meta, by = "code", all = TRUE)        # 全连接

# 实际：把行业信息接到个股收益上
r1 <- mlpanel[mlpanel$ym == "2026-06", c("secucode", "ret", "industry")]
head(r1, 3)

# 10.3.3 重构（长宽转换）
long <- indret[1:12, c("sector", "month", "ret")]
wide <- reshape(long, idvar = "month", timevar = "sector", direction = "wide")
wide
reshape(wide, direction = "long", varying = 2:ncol(wide),
        v.names = "ret", timevar = "sector", times = names(wide)[-1],
        idvar = "month")

# 10.4 分组与分个案处理 ----
# 按月计算截面排名
d3 <- mlpanel[mlpanel$ym %in% c("2026-05", "2026-06"), ]
d3$rk <- ave(d3$fwd_ret, d3$ym, FUN = function(z) rank(z) / length(z))
head(d3[, c("secucode", "ym", "fwd_ret", "rk")], 3)

# 组内去均值（行业中性化）
d3$ret_dm <- d3$fwd_ret - ave(d3$fwd_ret, d3$ym, FUN = mean, na.rm = TRUE)
head(d3[, c("ym", "fwd_ret", "ret_dm")], 3)

# 10.5 时间拆分 ----
# 把日期拆成 年/月/日
d4 <- data.frame(date = px1$date[1:3])
d4$year <- as.integer(substr(d4$date, 1, 4))
d4$month <- as.integer(substr(d4$date, 6, 7))
d4

# 按时间窗口切片
p1 <- px1[px1$date >= "2025-01-01" & px1$date <= "2025-03-31", ]
nrow(p1)

# 滚动窗口：12 个月收益的移动平均
r <- mlpanel$ret[mlpanel$secucode == mlpanel$secucode[1]]
r <- r[!is.na(r)]
ma <- stats::filter(r, rep(1/6, 6), sides = 1)
head(round(as.numeric(ma), 4), 8)

# 10.6 缺失值处理 ----
x <- c(0.01, NA, -0.02, NA, 0.03)
is.na(x); sum(is.na(x)); mean(is.na(x))
x[!is.na(x)]
na.omit(x)
mean(x, na.rm = TRUE)

# 用前值填充（金融时间序列常用）
library(zoo)
na.locf(c(1, NA, NA, 4, NA))

# 10.7 习题 ----
# 10.1 把 mlpanel 的 size 按行业分组，各行业内做五分位
# 10.2 将 indret 从长表转成 月 × 行业 的宽表，算相关矩阵
# 10.3 给 mlpanel 增加 year 列，并按 year 分组算平均收益
# 10.4 用 cut 把 vol_12m 分成低/中/高三组，比较各组的下月收益
