####统计分析 ####
#单因素方差分析
#要想做方差分析，数据必须满足3个条件即独立、正态，方差齐次性。
#1. 函数shapiro.test()检验正态性
shapiro.test(YieldRe$SNA) #p值大于0.05时数据正态性得到检验
hist(YieldRe$SNA)
#Q-Q图
SAN.lm<-lm(SNA ~ Treatment, data= YieldRe )
library(car)
qqPlot(SAN.lm,main="Q-Qplot图",las=T) 
#回归曲线在范围内，故数据符合正态性检验。
#2. bartlett.test()检验方差齐性
bartlett.test(SNA ~ Treatment, data= YieldRe )
#p值远大于显著性水平0.05，因此不能拒绝原假设，认为不同水平下数据是等方差的
#进行方差分析
SNA.aov <- aov(SNA ~ Treatment, data= YieldRe )
summary(SNA.aov)
boxplot(YieldRe$SNA ~ YieldRe$Treatment)
#采用最小显著差数检验法(即LSD法)，并用字母标记法对结果进行标记
require(agricolae)
SNA.lsd <- LSD.test(SNA.aov,"Treatment" )
HSD.test(layer.aov,"Treatment",group=TRUE)