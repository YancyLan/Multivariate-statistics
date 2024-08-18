library(readxl)
oridata<-read.csv("E:\\R_final\\Employee.csv")
Y = oridata$LeaveOrNot
sum(Y)/length(Y)
