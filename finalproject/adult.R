# 读取数据：
oridata <- read.table("E:\\R_final\\adult.data",header=F,sep=",")
# Step1: 了解Y的均衡度
oridata$Y[oridata$X..50K==" >50K"] <- 1  # 生成Y
oridata$Y[oridata$X..50K!=" >50K"] <- 0
obs_num = length(oridata$Y)
sum(oridata$Y)/obs_num
