#气象数据处理 ####
#### data clean 处理特殊数值####
# 国家级地面观测站历史日值数据，在历史气象日值数据处理过程中，针对降水量特征值处理方式如下：
#### 针对所有数据 ####

# 1. 32766为数据缺测、32744为无数据，如出现，在统计前筛除
# 2. 32700一般作为0mm进行处理；32700	表示降水"微量" 或者 蒸发量 32700	表示蒸发器结冰

#读入示例数据
library(data.table)
load("E:/Data/datav/selected stations merged all weather parameters.rda")
setDT(wthnew)
wthnew[wthnew=="32700"] <- 0
wthnew[wthnew=="32766"] <- NA
wthnew[wthnew=="32744"] <- NA
summary(wthnew)

#### 降雨量数据处理 ####

# 1. 32XXX（除32744、32766外）的处理结果为（32XXX-32000）*0.1mm
#
# 2. 31XXX的处理结果为（31XXX-31000）*0.1mm
#
# 3. 30XXX的处理结果为（30XXX-30000）*0.1mm

#处理32XXX之前，先解决32766和32744

wthnew$PRE[which(wthnew$PRE > 32000)] <- wthnew$PRE[which(wthnew$PRE > 32000)] - 32000
wthnew$PRE[which(wthnew$PRE > 31000)] <- wthnew$PRE[which(wthnew$PRE > 31000)] - 31000
wthnew$PRE[which(wthnew$PRE > 30000)] <- wthnew$PRE[which(wthnew$PRE > 30000)] - 30000

#### 0cm 地温数据处理 ####

# 0cm地温

# +10000	实际温度（零上）超仪器上限刻度，在上限数据基础上加10000
# -10000	实际温度（零下）超仪器下限刻度，在下限数据基础上减10000
#读入其他气象要素数据，以2018年为例
load("E:/Data/datav/气象数据20181130.RData")
summary(DT18_GST) #没有极端值，可以不做以下处理
DT18_GST$GST[which(DT18_GST$GST > 10000)] <- DT18_GST$GST[which(DT18_GST$GST > 10000)] - 10000
DT18_GST$GST[which(DT18_GST$GST < -10000)] <- DT18_GST$GST[which(DT18_GST$GST > -10000)] + 10000



