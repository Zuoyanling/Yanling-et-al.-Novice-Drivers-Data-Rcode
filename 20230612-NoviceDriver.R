## =============================数据准备=======================================

setwd("E:/我的工作学习/_（正在）_师门项目/2022 若愚文章投稿/英文期刊论文撰写/3 R分析/2不考虑协变量的分析")

library(openxlsx)
Driver01 = read.xlsx("20230612_23名被试的样本.xlsx", "合并数据")
head(Driver01)

## ============================插补缺失值======================================

# 采用随机森林法对缺失值进行插补
# [R语言数据缺失值处理（随机森林，多重插补）_r语言缺失值填补_乌龟出门遛个弯的博客-CSDN博客]
# (https://blog.csdn.net/qq_44877251/article/details/110520177)

library(missForest)
Driver02 <- missForest(Driver01, ntree = 100)
Driver03 <- as.data.frame(Driver02$ximp)

View(Driver03)
str(Driver03)
head(Driver03)
names(Driver03)
summary(Driver03)

## ============================相关性分析======================================

# 描述变量之间的spearman相关系数及相应p值（包含人口特征变量）
library(psych)
data_corr = corr.test(Driver03[,c(2:9,18,21:31,33:35,39:41)], method = "spearman")
# 输出相关系数矩阵的下三角部分，保留两位小数
lowerMat(data_corr$r, digits = 2)
# 输出相关系数显著性检验的p值
lowerMat(data_corr$p, digits = 4)

## ============================数据分析准备=====================================

#设置名义变量（连续变量不用设置）
Driver03$Subject = factor(Driver03$Subject)
Driver03$Gender = factor(Driver03$Gender)
Driver03$Experience = factor(Driver03$Experience)
Driver03$Education = factor(Driver03$Education)
Driver03$Scence = factor(Driver03$Scence)

# 给变量添加标签label
Driver03$Scence <- factor(Driver03$Scence,levels = c(1,2,3,4),
                          labels = c("LABA", "LDBA", "LABD", "LDBD"))
Driver03$Gender <- factor(Driver03$Gender,levels = c(0,1),
                          labels = c("Male", "Female"))
Driver03$Experience <- factor(Driver03$Experience,levels = c(1,2,3),
                              labels = c("Non-driving", "Occasional driving","Frequent driving"))
Driver03$Education <- factor(Driver03$Education,levels = c(0,1),
                             labels = c("Graduate students", "Undergraduate students"))
## ============================参试者特征=====================================

# 统计连续变量的平均值和方差
continuous_vars <- c("Age", "reckless.and.careless.style", "anxious.style", "angry.and.hostile.style", "patient.and.careful.style")
continuous_stats <- aggregate(Driver03[continuous_vars], by = list(Driver03$Gender), FUN = function(x) c(mean = mean(x), variance = var(x)))

# 打印连续变量的平均值和方差
print(continuous_stats)

# 统计名义变量的频次和比例
nominal_vars <- c("Experience", "Education")
nominal_stats <- lapply(nominal_vars, function(var) {
  counts <- table(Driver03$Gender, Driver03[[var]])
  freq <- prop.table(counts, margin = 1)
  list(counts = counts, freq = freq)
})

# 打印名义变量的频次和比例
for (i in seq_along(nominal_vars)) {
  cat("\nVariable:", nominal_vars[i], "\n")
  print(nominal_stats[[i]]$counts)
  cat("\nProportions:\n")
  print(nominal_stats[[i]]$freq)
  cat("\n")
}

## ========================前后车加减速影响分析================================
# ========================因变量：Numberoffixation==============================

# 检验正态性
by(Driver03$Numberoffixation, list(Driver03$Scence), shapiro.test)
m = aov(Numberoffixation ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设（log化也不满足），因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(Numberoffixation ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$Numberoffixation, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")#事后检验
print(friedman_result)
print(conover_result)

# 执行Conover检验
conover_result <- frdAllPairsConoverTest(Driver03$Numberoffixation, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")

# 查看Conover检验的结果，包括p值和统计量
conover_result_summary <- as.data.frame(conover_result)
print(conover_result_summary)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(Numberoffixation ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$Numberoffixation[, "mean"], 1)
sd_values <- round(mean_sd_data$Numberoffixation[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = ", ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = Numberoffixation)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Number of Fixation")

# ========================因变量：Fixationduration==============================

# 检验正态性
by(Driver03$Fixationduration, list(Driver03$Scence), shapiro.test)
m = aov(Fixationduration ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设（log化也不满足），因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
?friedman.test
friedman_result <- friedman.test(Fixationduration ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$Fixationduration, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(Fixationduration ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$Fixationduration[, "mean"], 1)
sd_values <- round(mean_sd_data$Fixationduration[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = ", ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = Fixationduration)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Fixation Duration")

# ========================因变量：NumberofSaccade==============================

# 检验正态性
by(Driver03$NumberofSaccade, list(Driver03$Scence), shapiro.test)
m = aov(NumberofSaccade ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设（log化也不满足），因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(NumberofSaccade ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$NumberofSaccade, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

# 查看Conover检验的结果，包括p值和统计量
conover_result_summary <- as.data.frame(conover_result)
print(conover_result_summary)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(NumberofSaccade ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$NumberofSaccade[, "mean"], 1)
sd_values <- round(mean_sd_data$NumberofSaccade[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = ", ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = NumberofSaccade)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Number of Saccade")
# ========================因变量：Saccadeduarion==============================

# 检验正态性
by(Driver03$Saccadeduarion, list(Driver03$Scence), shapiro.test)
m = aov(Saccadeduarion ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设（log化也不满足），因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(Saccadeduarion ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$Saccadeduarion, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(Saccadeduarion ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$Saccadeduarion[, "mean"], 1)
sd_values <- round(mean_sd_data$Saccadeduarion[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = ", ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = Saccadeduarion)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Saccade Duarion")

# ========================因变量：Saccadeangle==============================

# 检验正态性
by(Driver03$Saccadeangle, list(Driver03$Scence), shapiro.test)
m = aov(Saccadeangle ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设（log化也不满足），因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(Saccadeangle ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$Saccadeangle, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(Saccadeangle ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$Saccadeangle[, "mean"], 1)
sd_values <- round(mean_sd_data$Saccadeangle[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = ", ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = Saccadeangle)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Saccade Angle")

# ========================因变量：FixationTransfer==============================

# 检验正态性
by(Driver03$FixationTransfer, list(Driver03$Scence), shapiro.test)
m = aov(FixationTransfer ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设（log化也不满足），因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(FixationTransfer ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$FixationTransfer, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(FixationTransfer ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$FixationTransfer[, "mean"], 1)
sd_values <- round(mean_sd_data$FixationTransfer[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = ", ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = FixationTransfer)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Fixation Transfer")

# ========================因变量：maxwheelposition==============================

# 检验正态性
by(Driver03$maxwheelposition, list(Driver03$Scence), shapiro.test)
m = aov(maxwheelposition ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设（log化也不满足），因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(maxwheelposition ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$maxwheelposition, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(maxwheelposition ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$maxwheelposition[, "mean"], 1)
sd_values <- round(mean_sd_data$maxwheelposition[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = "°, ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = maxwheelposition)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Max Wheel Position")

# ========================因变量：maxvelocity==============================

# 检验正态性
by(Driver03$maxvelocity, list(Driver03$Scence), shapiro.test)
m = aov(maxvelocity ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设（log化也不满足），因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(maxvelocity ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$maxvelocity, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(maxvelocity ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$maxvelocity[, "mean"], 1)
sd_values <- round(mean_sd_data$maxvelocity[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = "m/s, ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = maxvelocity)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Max Velocity")

# ========================因变量：maxlateralvelocity==============================

# 检验正态性
by(Driver03$maxlateralvelocity, list(Driver03$Scence), shapiro.test)
m = aov(maxlateralvelocity ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设（log化也不满足），因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(maxlateralvelocity ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$maxlateralvelocity, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(maxlateralvelocity ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$maxlateralvelocity[, "mean"], 2)
sd_values <- round(mean_sd_data$maxlateralvelocity[, "sd"], 3)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = "m/s, ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = maxlateralvelocity)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Max Lateral Velocity")

# ========================因变量：maxlateraldistance==============================

# 检验正态性
by(Driver03$maxlateraldistance, list(Driver03$Scence), shapiro.test)
m = aov(maxlateraldistance ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于满足正态假设，因此继续检验One-way repeated measures ANOVA分析方法的其他假设

## 检验方差齐性
library(car)
leveneTest(maxlateraldistance ~ Scence, data=Driver03, center=median) # Brown-Forsythe test
## 由于不满足方差齐性，因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(maxlateraldistance ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$maxlateraldistance, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(maxlateraldistance ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$maxlateraldistance[, "mean"], 1)
sd_values <- round(mean_sd_data$maxlateraldistance[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = "m, ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = maxlateraldistance)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Max Lateral Distance")

# ========================因变量：LVBVTrue==============================

# 检验正态性
by(Driver03$LVBVTrue, list(Driver03$Scence), shapiro.test)
m = aov(LVBVTrue ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于满足正态假设，因此继续检验One-way repeated measures ANOVA分析方法的其他假设

## 检验方差齐性
library(car)
leveneTest(LVBVTrue ~ Scence, data=Driver03, center=median) # Brown-Forsythe test
## 由于满足方差齐性，因此继续检验One-way repeated measures ANOVA分析方法的其他假设

## 球形检验假设
library(ez)
m = ezANOVA(dv=LVBVTrue, within=Scence, wid=Subject, data=Driver03)
m$Mauchly # 球形检验，p值n.s.，满足球形检验可以直接进行m$ANOVA。
m$ANOVA

#整体检验显著之后进行事后检验，使用独立样本配对T检验。
library(reshape2)	
Driver03.wide = dcast(Driver03, Subject ~ Scence, value.var="LVBVTrue") # go wide
View(Driver03.wide)
LDBA.LABA = t.test(Driver03.wide$LDBA, Driver03.wide$LABA, paired=TRUE)
LABD.LABA = t.test(Driver03.wide$LABD, Driver03.wide$LABA, paired=TRUE)
LABD.LDBA = t.test(Driver03.wide$LABD, Driver03.wide$LDBA, paired=TRUE)
LDBD.LABA = t.test(Driver03.wide$LDBD, Driver03.wide$LABA, paired=TRUE)
LDBD.LDBA = t.test(Driver03.wide$LDBD, Driver03.wide$LDBA, paired=TRUE)
LDBD.LABD = t.test(Driver03.wide$LDBD, Driver03.wide$LABD, paired=TRUE)
p.adjust(c(LDBA.LABA$p.value, LABD.LABA$p.value, LABD.LDBA$p.value,
           LDBD.LABA$p.value, LDBD.LDBA$p.value, LDBD.LABD$p.value), method="holm")

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = LVBVTrue)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "LVBV True")

# ========================因变量：LV1TRUE==============================

# 检验正态性
by(Driver03$LV1TRUE, list(Driver03$Scence), shapiro.test)
m = aov(LV1TRUE ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由不满足正态假设，因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(LV1TRUE ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$LV1TRUE, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(LV1TRUE ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$LV1TRUE[, "mean"], 1)
sd_values <- round(mean_sd_data$LV1TRUE[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = "%, ")
# 打印结果
cat(formatted_data)

# ========================因变量：LV2TRUE==============================

# 检验正态性
by(Driver03$LV2TRUE, list(Driver03$Scence), shapiro.test)
m = aov(LV2TRUE ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由不满足正态假设，因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(LV2TRUE ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$LV2TRUE, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(LV2TRUE ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$LV2TRUE[, "mean"], 1)
sd_values <- round(mean_sd_data$LV2TRUE[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = "%, ")
# 打印结果
cat(formatted_data)

# ========================因变量：BVTRUE==============================

# 检验正态性
by(Driver03$BVTRUE, list(Driver03$Scence), shapiro.test)
m = aov(BVTRUE ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由不满足正态假设，因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(BVTRUE ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$BVTRUE, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(BVTRUE ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$BVTRUE[, "mean"], 1)
sd_values <- round(mean_sd_data$BVTRUE[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = "%, ")
# 打印结果
cat(formatted_data)

# ========================因变量：Probility==============================

# 检验正态性
by(Driver03$Probility, list(Driver03$Scence), shapiro.test)
m = aov(Probility ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于不满足正态假设，因此采用Friedman分析方法

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(Probility ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$Probility, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(Probility ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$Probility[, "mean"], 2)
sd_values <- round(mean_sd_data$Probility[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = ", ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = Probility)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Probility")

# ========================因变量：Worry==============================

# 检验正态性
by(Driver03$Worry, list(Driver03$Scence), shapiro.test)
m = aov(Worry ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于满足正态假设，因此继续检验One-way repeated measures ANOVA分析方法的其他假设

## 检验方差齐性
library(car)
leveneTest(Worry ~ Scence, data=Driver03, center=median) # Brown-Forsythe test
## 由于满足方差齐性，因此继续检验One-way repeated measures ANOVA分析方法的其他假设

## 检验球形假设
library(ez)
m = ezANOVA(dv=Worry, within=Scence, wid=Subject, data=Driver03)
m$Mauchly # 球形检验，p显著，不满足球形检验的假设，需要对自由度进行校正。
m$ANOVA

#可以采用Greenhouse-Geisser或者Huynh-Feldt来进行自由度的校正。

pos = match(m$`Sphericity Corrections`$Effect, m$ANOVA$Effect) # positions of within-Ss efx in m$ANOVA
m$Sphericity$GGe.DFn = m$Sphericity$GGe * m$ANOVA$DFn[pos] # Greenhouse-Geisser
m$Sphericity$GGe.DFd = m$Sphericity$GGe * m$ANOVA$DFd[pos]
m$Sphericity$HFe.DFn = m$Sphericity$HFe * m$ANOVA$DFn[pos] # Huynh-Feldt
m$Sphericity$HFe.DFd = m$Sphericity$HFe * m$ANOVA$DFd[pos]
m$Sphericity # show results
#p[GG]、GGe.DFn、GGe.DFd是Greenhouse-Geisser校正后的p值、分子自由度、分母自由度。
#p[HF]、HFe.DFn、HFe.DFd是Huynh-Feldt校正后的~。

# 校正后的p值还是显著，依旧不满足球形检验的假设，故使用Friedman test进行分析

# Friedman test
library(PMCMRplus)
friedman_result <- friedman.test(Worry ~ Scence | Subject, data = Driver03)
conover_result <- frdAllPairsConoverTest(Driver03$Worry, Driver03$Scence, 
                                         Driver03$Subject, p.adjust.method = "holm")
print(friedman_result)
print(conover_result)

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(Worry ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$Worry[, "mean"], 1)
sd_values <- round(mean_sd_data$Worry[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = ", ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = Worry)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Worry")

# ========================因变量：Danger==============================

# 检验正态性
by(Driver03$Danger, list(Driver03$Scence), shapiro.test)
m = aov(Worry ~ Scence, data=Driver03) # fit model
shapiro.test(residuals(m)) # test residuals
## 由于满足正态假设，因此继续检验One-way repeated measures ANOVA分析方法的其他假设

## 检验方差齐性
library(car)
leveneTest(Danger ~ Scence, data=Driver03, center=median) # Brown-Forsythe test
## 由于满足方差齐性，因此继续检验One-way repeated measures ANOVA分析方法的其他假设

## 球形检验假设
library(ez)
m = ezANOVA(dv=Danger, within=Scence, wid=Subject, data=Driver03)
m$Mauchly # 球形检验，p值n.s.，满足球形检验可以直接进行m$ANOVA。
m$ANOVA

#整体检验显著之后进行事后检验，使用独立样本配对T检验。
library(reshape2)	
Driver03.wide = dcast(Driver03, Subject ~ Scence, value.var="Danger") # go wide
View(Driver03.wide)
LDBA.LABA = t.test(Driver03.wide$LDBA, Driver03.wide$LABA, paired=TRUE)
LABD.LABA = t.test(Driver03.wide$LABD, Driver03.wide$LABA, paired=TRUE)
LABD.LDBA = t.test(Driver03.wide$LABD, Driver03.wide$LDBA, paired=TRUE)
LDBD.LABA = t.test(Driver03.wide$LDBD, Driver03.wide$LABA, paired=TRUE)
LDBD.LDBA = t.test(Driver03.wide$LDBD, Driver03.wide$LDBA, paired=TRUE)
LDBD.LABD = t.test(Driver03.wide$LDBD, Driver03.wide$LABD, paired=TRUE)
p.adjust(c(LDBA.LABA$p.value, LABD.LABA$p.value, LABD.LDBA$p.value,
           LDBD.LABA$p.value, LDBD.LDBA$p.value, LDBD.LABD$p.value), method="holm")

#数据报告
# 计算Scence中每个值的平均数和标准差
mean_sd_data <- aggregate(Danger ~ Scence, data = Driver03, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# 提取平均数和标准差
scence_values <- mean_sd_data$Scence
mean_values <- round(mean_sd_data$Danger[, "mean"], 1)
sd_values <- round(mean_sd_data$Danger[, "sd"], 2)
# 创建格式化的字符串
formatted_data <- paste(scence_values, ":", mean_values, "±", sd_values, collapse = ", ")
# 打印结果
cat(formatted_data)

# 绘制箱型图
library(ggplot2)
ggplot(Driver03, aes(x = Scence, y = Danger)) +
  geom_boxplot(fill = "#FF6B6B", alpha = 0.5, position = position_dodge(width = 0.85)) +
  labs(x = "Scence", y = "Danger")

# ========================绘图：眼动行为部分==============================

# 绘图准备：
## 定义一个计算标准误的函数
summarySE <- function(data=NULL, outcome, factor=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) { 
  library(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  datac <- ddply(data, factor, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 outcome
  )
  
  datac <- rename(datac, c("mean" = outcome))
  
  datac$se <- datac$sd / sqrt(datac$N) 
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## 计算每个组合条件下的均值和标准误
data_se_FixationNumber <- summarySE(data = Driver03, outcome = "Numberoffixation", factor = c("Scence"))
data_se_FixationDuration <- summarySE(data = Driver03, outcome = "Fixationduration", factor = c("Scence"))
data_se_SaccadeNumber <- summarySE(data = Driver03, outcome = "NumberofSaccade", factor = c("Scence"))
data_se_SaccadeDuration <- summarySE(data = Driver03, outcome = "Saccadeduarion", factor = c("Scence"))
data_se_SaccadeAngle <- summarySE(data = Driver03, outcome = "Saccadeangle", factor = c("Scence"))
data_se_FixationTransfer <- summarySE(data = Driver03, outcome = "FixationTransfer", factor = c("Scence"))

# 绘制六个小图
library(ggplot2)
library(gridExtra)

dark_blue <- "steelblue"

p1 <- ggplot(data_se_FixationNumber, aes(x = Scence, y = Numberoffixation)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = dark_blue) +
  geom_errorbar(aes(ymin = Numberoffixation - se, ymax = Numberoffixation + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Fixation Number") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_FixationNumber$Numberoffixation + data_se_FixationNumber$se) * 1.1))

p2 <- ggplot(data_se_FixationDuration, aes(x = Scence, y = Fixationduration)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = dark_blue) +
  geom_errorbar(aes(ymin = Fixationduration - se, ymax = Fixationduration + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Fixation Duration (s)") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_FixationDuration$Fixationduration + data_se_FixationDuration$se) * 1.1))

p3 <- ggplot(data_se_SaccadeNumber, aes(x = Scence, y = NumberofSaccade)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = dark_blue) +
  geom_errorbar(aes(ymin = NumberofSaccade - se, ymax = NumberofSaccade + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Saccade Number") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_SaccadeNumber$NumberofSaccade + data_se_SaccadeNumber$se) * 1.1))

p4 <- ggplot(data_se_SaccadeDuration, aes(x = Scence, y = Saccadeduarion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = dark_blue) +
  geom_errorbar(aes(ymin = Saccadeduarion - se, ymax = Saccadeduarion + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Saccade Duration (s)") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_SaccadeDuration$Saccadeduarion + data_se_SaccadeDuration$se) * 1.1))

p5 <- ggplot(data_se_SaccadeAngle, aes(x = Scence, y = Saccadeangle)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = dark_blue) +
  geom_errorbar(aes(ymin = Saccadeangle - se, ymax = Saccadeangle + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Saccade Angle (°)") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_SaccadeAngle$Saccadeangle + data_se_SaccadeAngle$se) * 1.1))

p6 <- ggplot(data_se_FixationTransfer, aes(x = Scence, y = FixationTransfer)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = dark_blue) +
  geom_errorbar(aes(ymin = FixationTransfer - se, ymax = FixationTransfer + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "The Check Number of Rearview Mirrors") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_FixationTransfer$FixationTransfer + data_se_FixationTransfer$se) * 1.1))

# 组合六个小图成一张大图，3列2行排列
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
grid_plot <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

# 保存为PNG文件
ggsave("Eye movement behaviors.png", plot = grid_plot, width = 10, height = 7, dpi = 300)

# ========================绘图：驾驶行为部分==============================

# 绘图准备：

## 计算每个组合条件下的均值和标准误
data_se_MaxWheelPosition <- summarySE(data = Driver03, outcome = "maxwheelposition", factor = c("Scence"))
data_se_MaxVelocity <- summarySE(data = Driver03, outcome = "maxvelocity", factor = c("Scence"))
data_se_MaxLateralVelocity <- summarySE(data = Driver03, outcome = "maxlateralvelocity", factor = c("Scence"))
data_se_MaxLateralDistance <- summarySE(data = Driver03, outcome = "maxlateraldistance", factor = c("Scence"))

# 定义自定义颜色
custom_orange <- "#ffab4c"

# 绘制四个小图
library(ggplot2)
library(gridExtra)

p7 <- ggplot(data_se_MaxWheelPosition, aes(x = Scence, y = maxwheelposition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = custom_orange) +
  geom_errorbar(aes(ymin = maxwheelposition - se, ymax = maxwheelposition + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Max Steering Wheel Angle (°)") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_MaxWheelPosition$maxwheelposition + data_se_MaxWheelPosition$se) * 1.1))

p8 <- ggplot(data_se_MaxVelocity, aes(x = Scence, y = maxvelocity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = custom_orange) +
  geom_errorbar(aes(ymin = maxvelocity - se, ymax = maxvelocity + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Max Velocity (m/s)") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_MaxVelocity$maxvelocity + data_se_MaxVelocity$se) * 1.1))

p9 <- ggplot(data_se_MaxLateralVelocity, aes(x = Scence, y = maxlateralvelocity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = custom_orange) +
  geom_errorbar(aes(ymin = maxlateralvelocity - se, ymax = maxlateralvelocity + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Max Lateral Velocity (m/s)") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_MaxLateralVelocity$maxlateralvelocity + data_se_MaxLateralVelocity$se) * 1.1))

p10 <- ggplot(data_se_MaxLateralDistance, aes(x = Scence, y = maxlateraldistance)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = custom_orange) +
  geom_errorbar(aes(ymin = maxlateraldistance - se, ymax = maxlateraldistance + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Max Lateral Distance (m)") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_MaxLateralDistance$maxlateraldistance + data_se_MaxLateralDistance$se) * 1.1))

# 组合四个小图成一张大图，2列2行排列
grid_plot2 <- grid.arrange(p7, p8, p9, p10, ncol = 2)

# 保存为PNG文件
ggsave("Driving behaviors.png", plot = grid_plot2, width = 7, height = 7, dpi = 300)

# ========================绘图：主观认知部分==============================

# 绘图准备：

## 计算每个组合条件下的均值和标准误
data_se_LV1TRUE <- summarySE(data = Driver03, outcome = "LV1TRUE", factor = c("Scence"))
data_se_LV2TRUE <- summarySE(data = Driver03, outcome = "LV2TRUE", factor = c("Scence"))
data_se_BVTRUE <- summarySE(data = Driver03, outcome = "BVTRUE", factor = c("Scence"))
data_se_Probility <- summarySE(data = Driver03, outcome = "Probility", factor = c("Scence"))
data_se_Worry <- summarySE(data = Driver03, outcome = "Worry", factor = c("Scence"))
data_se_Danger <- summarySE(data = Driver03, outcome = "Danger", factor = c("Scence"))

# 绘制六个小图
library(ggplot2)
library(gridExtra)

red_color <- "#ff7a7e"

p1 <- ggplot(data_se_LV1TRUE, aes(x = Scence, y = LV1TRUE)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = red_color) +
  geom_errorbar(aes(ymin = LV1TRUE - se, ymax = LV1TRUE + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Accuracy of LV1 Velocity Change Cognition (%)") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_LV1TRUE$LV1TRUE + data_se_LV1TRUE$se) * 1.1))

p2 <- ggplot(data_se_LV2TRUE, aes(x = Scence, y = LV2TRUE)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = red_color) +
  geom_errorbar(aes(ymin = LV2TRUE - se, ymax = LV2TRUE + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Accuracy of LV2 Velocity Change Cognition (%)") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_LV2TRUE$LV2TRUE + data_se_LV2TRUE$se) * 1.1))

p3 <- ggplot(data_se_BVTRUE, aes(x = Scence, y = BVTRUE)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = red_color) +
  geom_errorbar(aes(ymin = BVTRUE - se, ymax = BVTRUE + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Accuracy of BV Velocity Change Cognition (%)") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_BVTRUE$BVTRUE + data_se_BVTRUE$se) * 1.1))

p4 <- ggplot(data_se_Probility, aes(x = Scence, y = Probility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = red_color) +
  geom_errorbar(aes(ymin = Probility - se, ymax = Probility + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Accident Probability") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_Probility$Probility + data_se_Probility$se) * 1.1))

p5 <- ggplot(data_se_Worry, aes(x = Scence, y = Worry)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = red_color) +
  geom_errorbar(aes(ymin = Worry - se, ymax = Worry + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Accident Worry") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_Worry$Worry + data_se_Worry$se) * 1.1))

p6 <- ggplot(data_se_Danger, aes(x = Scence, y = Danger)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), fill = red_color) +
  geom_errorbar(aes(ymin = Danger - se, ymax = Danger + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  labs(x = "The Velocity Change of Lead and Behind Vehicles", y = "Driving Danger") +
  theme(text = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, max(data_se_Danger$Danger + data_se_Danger$se) * 1.1))

# 组合六个小图成一张大图，3列2行排列
grid_plot <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

# 保存为PNG文件
ggsave("Subjective perception.png", plot = grid_plot, width = 10, height = 7, dpi = 300)

                  

# ========================其他：GLMM分析==============================

library(lme4) # for glmer
library(car) # for Anova

## 眼动行为部分
model <- glm(Numberoffixation ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(Fixationduration ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(NumberofSaccade ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(Saccadeduarion ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(Saccadeangle ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(FixationTransfer ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

## 主观认知部分
model <- glm(LV1TRUE ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(LV2TRUE ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(BVTRUE ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(Probility ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(Worry ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(Danger ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

## 驾驶行为部分
model <- glm(maxwheelposition ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(maxvelocity ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(maxlateralvelocity ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)

model <- glm(maxlateraldistance ~ Scence + Gender + anxious.style + LaneChange, data = Driver03, family = gaussian)
summary(model)
