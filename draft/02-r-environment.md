# 2 R 环境

上一章已经跑过几个例子，但那是「照着敲」。这一章讲怎么真正把 R 用起来：管理会话、组织代码、控制图形、把数据读进来。

## 2.1 会话管理

### 2.1.1 工作空间

启动 R 后你面对的是一个**会话**（session）。会话里创建的所有对象都在工作空间中，`ls()` 列出来：

```r
ls()
```

```text
 [1] "abret"         "ashr"          "datasets_dir"  "divcurve"
 [5] "drawdown"      "excess"        "fin"           "idxret"
 [9] "indcnt"        "indday"        "indret"        "load_dataset"
[13] "load_datasets" "mlevent"       "mlfin"         "mlpanel"
[17] "pair2"         "px1"           "scale"         "vol3"
```

注意里面除了数据集，还有 `load_datasets` 这类函数——它们是载入脚本定义的。`objects()` 与 `ls()` 等价。

`ls(pattern=)` 可以筛：

```r
ls(pattern = "^ml")
```

```text
[1] "mlevent" "mlfin"   "mlpanel"
```

保存与恢复单个对象用 `saveRDS()`，比 `save()` 更适合长期存放——它保存单个对象，读回来用 `readRDS()`，不必记住原来的变量名：

```r
saveRDS(abret, "/tmp/abret.rds")
abret2 <- readRDS("/tmp/abret.rds")
identical(abret, abret2)
```

```text
[1] TRUE
```

`identical()` 返回 `TRUE`，说明存读无损。实践上应尽量用 `saveRDS`，而不是把整个工作空间存成 `.RData` 让下次启动自动加载——那会让你分不清哪些对象是这次算的、哪些是上次残留的。**可复现的代码应当能从空会话跑出来**。

### 2.1.2 文本输出

`print()` 是默认的打印，但它对小数位、对齐的控制有限。要拼一句话给读者看，用 `cat()` 与 `sprintf()`：

```r
print(mean(abret$A))
cat("日均收益:", round(mean(abret$A) * 100, 4), "%\n")
sprintf("年化收益 %.2f%%", mean(abret$A) * 252 * 100)
```

```text
[1] 0.0009200714
日均收益: 0.092 %
[1] "年化收益 23.19%"
```

`cat` 直接输出、不加引号、可以混排文字；`sprintf` 用 C 风格的格式化占位符（`%.2f` 两位小数、`%d` 整数），返回字符串，适合再加工。

数字格式化有两个常用函数：

```r
format(1234567.891, big.mark = ",")
round(c(0.00012345, 1.234567), 4)
signif(c(0.00012345, 1.234567), 4)
```

```text
[1] "1,234,568"
[1] 0.0001 1.2346
[1] 0.0001234 1.2350000
```

`round` 保留**小数位**，`signif` 保留**有效数字**，`format` 只影响显示。金融数字常要千分位，`big.mark` 就是干这个的。

### 2.1.3 脚本编写

交互式输入适合探索，正式分析要写成脚本。把命令存成 `.R` 文件，用 `source()` 运行：

```r
source("code/01-basics.R")
```

本书的代码就是这样组织的：每章一个脚本，都能独立运行。

要把输出写进文件而不是屏幕，用 `sink()`：

```r
sink("/tmp/out.txt")
cat("写入文件\n")
sink()
readLines("/tmp/out.txt")
```

```text
[1] "写入文件"
```

`sink()` 打开重定向，`sink()` 再调一次关闭。忘了关会导致后续输出全部丢失，这是新手常踩的坑。用 `on.exit(sink(), add = TRUE)` 更保险。

### 2.1.4 获取帮助

`?函数名` 查文档，`args()` 看参数列表，`example()` 跑示例：

```r
# ?mean
# args(mean)
# example(mean)
```

查不到时先试 `??关键词` 做全文搜索。R 的文档质量参差，函数签名部分通常可靠，示例部分有时过时。

### 2.1.5 包

R 的能力来自包。`search()` 显示当前已加载的：

```r
search()
```

```text
[1] ".GlobalEnv"        "package:stats"     "package:graphics"
[4] "package:grDevices" "package:utils"     "package:datasets"
[7] "package:methods"   "Autoloads"         "package:base"
```

装包用 `install.packages()`，加载用 `library()`。加载后它出现在搜索路径第二位：

```r
library(quadprog)
search()[1:4]
```

```text
[1] ".GlobalEnv"       "package:quadprog" "package:stats"    "package:graphics"
```

**装一次，每次会话都要加载**。这一点经常被忘记，导致脚本在别人机器上报「找不到函数」。

本书用到的扩展包很少：`quadprog`（二次规划）、`survival`（生存分析）、`randomForest` 与 `xgboost`（机器学习）、`MASS`（负二项与岭回归）、`car`（Levene 检验）、`zoo`（时间序列填充）。

### 2.1.6 内置数据

`datasets` 包里有一批示例数据，不必联网就能练手：

```r
data(package = "datasets")$results[1:5, c("Item", "Title")]
```

```text
     Item                     Title
[1,] "AirPassengers"          "Monthly Airline Passenger Numbers 1949-1960"
[2,] "BJsales"                "Sales Data with Leading Indicator"
[3,] "BJsales.lead (BJsales)" "Sales Data with Leading Indicator"
[4,] "BOD"                    "Biochemical Oxygen Demand"
[5,] "CO2"                    "Carbon Dioxide Uptake in Grass Plants"
```

其中一个和本书直接相关：`EuStockMarkets`——欧洲四个股指 1991–1999 的日收盘价，是 R 内置的唯一一个金融时间序列。

```r
data(EuStockMarkets)
str(EuStockMarkets)
```

```text
 Time-Series [1:1860, 1:4] from 1991 to 1999: 1629 1614 1607 1621 1618 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:4] "DAX" "SMI" "CAC" "FTSE"
```

这本书不用它，因为样本期太老、且是欧洲市场。但它是验证代码的好材料——不需要任何外部文件。

### 2.1.7 attach 与 detach

`attach()` 把数据框的列挂到搜索路径上，之后可以不写 `$`：

```r
head(mlpanel[, c("secucode", "ym", "size")], 3)
```

```text
   secucode      ym     size
1  000001.SZ 2021-01 26.82823
```

`attach(mlpanel)` 之后可以直接写 `size`。**但不建议这么做**，理由有两个：一是容易与你自己的变量重名，二是忘了 `detach` 会让后续所有代码的行为变得难以预料。

更安全的替代是 `with()`，只在那一次调用里生效：

```r
with(mlpanel[1:3, ], c(size, turnover))
```

### 2.1.8 subset、transform、within

这三个函数让数据框操作读起来更接近自然语言。

`subset()` 按条件取行列：

```r
sub <- subset(mlpanel, industry == "银行" & ym == "2026-06",
              select = c(secucode, size, turnover))
nrow(sub)
```

```text
[1] 41
```

`transform()` 在数据框上新增列：

```r
t1 <- transform(mlpanel[1:3, ], logsize = log(size), big = turnover > 0.5)
```

`within()` 更进一步，它允许你在同一个块里先建新变量、再删旧变量：

```r
w1 <- within(mlpanel[1:3, ], {
  logsize <- log(size)
  big <- turnover > 0.5
  rm(size)
})
```

两者的区别：`transform` 里不能引用刚建的变量，`within` 可以。复杂一点的派生计算用 `within` 更顺。

## 2.2 图形子系统

### 2.2.1 图形布局

R 的图形是「画上去」而不是「声明出来」的。`par(mfrow = c(2, 2))` 把画布切成 2×2，之后四条绘图命令依次填进去：

```r
par(mfrow = c(2, 2))
plot(abret$A, type = "l", main = "A 日收益")
plot(abret$B, type = "l", main = "B 日收益")
hist(abret$A, main = "A 分布", xlab = "")
boxplot(abret$A, main = "A 箱线")
par(mfrow = c(1, 1))
```

**画完一定要复位**，否则后面的图会继续往这个网格里挤。

`layout()` 更灵活，用矩阵指定每个位置占多少格：

```r
layout(matrix(c(1, 2, 1, 3), 2, 2))
```

这表示第一张图占左列两行，第 2、3 张占右列上下。`layout(1)` 复位。

### 2.2.2 由各个部分构建图形

R 的绘图分两步：先开一个坐标系（`plot(..., type = "n")` 只画坐标轴不画点），再往上叠加元素。

```r
plot(px1$close[1:120], type = "n", xlab = "交易日", ylab = "收盘价",
     main = "分段构建图形")
lines(px1$close[1:120], col = "steelblue", lwd = 2)
points(1:120, px1$close[1:120], pch = 20, cex = 0.5, col = "grey40")
abline(h = mean(px1$close[1:120]), lty = 2, col = "firebrick")
```

常用的叠加函数：

| 函数 | 作用 |
|---|---|
| `points()` | 加点 |
| `lines()` | 加折线 |
| `abline()` | 加直线（给 `h=`/`v=` 画水平/垂直线） |
| `text()` | 加文字 |
| `legend()` | 加图例 |
| `grid()` | 加网格 |

这个「先建空图再叠加」的模式是 R 基础绘图的核心，理解了它，绝大多数图形参数都能自己推出来。

### 2.2.3 使用 par

`par()` 设置全局图形参数，设定一次对后续所有图生效，直到再改。

```r
par(bg = "white", las = 1, cex.axis = 0.9)
plot(abret$A, abret$B, pch = 19, col = rgb(0, 0, 0, 0.4))
```

常用的几个：

| 参数 | 含义 | 金融场景的用法 |
|---|---|---|
| `las` | 坐标轴标签方向 | `las = 1` 让纵轴标签水平，便于读数 |
| `mar` | 四边留白（行数） | 长标题或长行业名时需要加大 |
| `mfrow` | 一页多图 | 对比多个时期 |
| `cex` | 字号/点大小倍率 | 散点太多时调小 |
| `col` | 颜色 | 用 `rgb(0,0,0,0.3)` 做半透明，解决点重叠 |

半透明是处理「几千个点叠在一起」的最实用技巧。`rgb(0, 0, 0, 0.4)` 的第四个参数是 alpha，0 全透明、1 不透明。

### 2.2.4 组合图形

实际报告里最常见的组合是「左直方图 + 右 Q-Q 图」，用来看分布是否接近正态：

```r
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
hist(ashr$ret, breaks = 40, main = "季度收益分布", xlab = "季度收益")
qqnorm(ashr$ret, main = "正态 Q-Q 图"); qqline(ashr$ret, col = "red")
```

第 4 章会详细讲这两张图。这里只强调一点：`mar` 在组合图形时要单独调，默认留白是按单图设计的，四张图挤在一起会互相压标签。

## 2.3 把数据读进来

本书的数据都是 CSV，用 `read.csv()`：

```r
write.csv(abret, "/tmp/abret.csv", row.names = FALSE)
a1 <- read.csv("/tmp/abret.csv", stringsAsFactors = FALSE)
str(a1)
```

```text
'data.frame':	56 obs. of  3 variables:
 $ date: chr  "2026-01-05" "2026-01-06" "2026-01-07" "2026-01-08" ...
 $ A   : num  0.03483 0.00141 -0.00326 -0.0078 0.0048 ...
 $ B   : num  -0.01397 -0.00256 -0.00643 -0.00777 0.0026 ...
```

三个参数值得记住：

- `stringsAsFactors = FALSE`：R 4.0 之后默认为 `FALSE`，但老代码里常见 `TRUE`。字符列变因子会让 `as.character` 之类的操作出意外。
- `row.names = FALSE`：写出时不带行号，否则读回来会多一列 `X`。
- `check.names = FALSE`：列名里有 `510300.SS` 这种以数字开头、带点号的字符串时，默认会被改成 `X510300.SS`。本书的宽表数据必须加这个参数。

压缩文件直接读，不必先解压：

```r
d <- read.csv(gzfile("data/csv/mlpanel.csv.gz"))
```

本书的 `mlpanel`（28 万行）与 `mlfin` 就是这样存的，59 MB 压到 24 MB。

## 2.4 习题

**2.1** 用 `subset()` 取出 2026-06 中 `size` 最大的 10 只股票。

**2.2** 用 `transform()` 给 `mlpanel` 增加一列 `log_turnover`，取值是 `log(turnover)`。

**2.3** 用 `par(mfrow)` 画出四个宽基 ETF 的收益走势。

**2.4** 用 `cat()` 与 `sprintf()` 输出「数据共有 31 个行业、4145 只股票」这类汇总句。
