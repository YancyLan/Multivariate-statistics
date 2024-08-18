# Pre-Possessing
traindata <- read.table("E:\\R_final\\adult.data", header=F,sep=",", strip.white = T)
testdata <- read.table("E:\\R_final\\adult.test", header=F,sep=",", strip.white = T)
traindata[traindata == "?"] <- NA
traindata <- na.omit(traindata)
testdata[testdata == "?"] <- NA
testdata <- na.omit(testdata)
oridata <- rbind(traindata,testdata)
columns=c("Age","WorkClass","fnlwgt","Education","EducationNum",
"MaritalStatus","Occupation","Relationship","Race","Gender",
"CapitalGain","CapitalLoss","HoursPerWeek","NativeCountry","Income")
colnames(oridata) <- columns
colnames(traindata) <- columns
colnames(testdata) <- columns


Edulevels = c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th",
"12th", "HS-grad", "Some-college", "Assoc-voc","Assoc-acdm", "Bachelors", "Masters",
"Prof-school", "Doctorate")
oridata$Education <- as.numeric(factor(oridata$Education , ordered=TRUE, levels = Edulevels))

oridata <- oridata[ , !names(oridata) %in% c("fnlwgt", "Education")]
traindata <- traindata[ , !names(traindata) %in% c("fnlwgt", "Education")]
testdata <- testdata[ , !names(testdata) %in% c("fnlwgt", "Education")]

# 性别
oridata$Gender <- as.numeric(factor(oridata$Gender, levels = c("Female", "Male"))) - 1
# 收入
oridata$Income <- as.numeric(factor(oridata$Income, levels = c("<=50K", ">50K"))) - 1

onehot <- function(x){
  new <- as.data.frame(model.matrix(~x-1,oridata))
}
onehotlist <- c("WorkClass","MaritalStatus","Occupation","Relationship","Race","NativeCountry")
addcolumn1 <- apply(oridata[onehotlist] , 2, onehot) 
oridata = oridata[ , !names(oridata) %in% onehotlist]
adult <- cbind(oridata , addcolumn1)

traindata0 <- adult[1:30162,]
testdata0 <- adult[30163:45222,]

numericlist = c("Age", "EducationNum", "CapitalGain", "CapitalLoss", "HoursPerWeek")

meanlist <- apply(traindata0[numericlist] , 2, mean)
sdlist <- apply(traindata0[numericlist], 2, sd)

traindata0[numericlist] <- scale(traindata0[numericlist], center = TRUE, scale = TRUE)
testx <- subset(testdata0 , select = -Income)
meanM <- matrix(rep(meanlist),nrow=15060,byrow = TRUE)
sdM <- matrix(rep(sdlist),nrow=15060,byrow=TRUE)
testx[numericlist] <- (testx[numericlist] - meanM) / sdM

# Lasso
cvfit <- cv.glmnet(alpha = 1, x = as.matrix(x), y = y, family="binomial",type.measure="class", nfolds = 11)
cvfit
## 
## Call:  cv.glmnet(x = as.matrix(x), y = y, type.measure = "class", nfolds = 11,      alpha = 1, family = "binomial") 
## 
## Measure: Misclassification Error 
## 
##       Lambda Index Measure       SE Nonzero
## min 0.000378    68  0.1521 0.002242      69
## 1se 0.005115    40  0.1542 0.001959      28
plot(cvfit)

fit1 <- glmnet(alpha = 1, lambda =cvfit$lambda.min, x = as.matrix(x), y = y, family="binomial",type.measure="class", nfolds = 11)

# Ridge Regression
testdataori <- read.table("E:\\R_final\\adult.test", header=F,sep=",", strip.white = T)
testdataori[testdataori == "?"] <- NA
testdataori <- na.omit(testdataori)
colnames(testdataori) <- columns
testdataori$Income <- as.numeric(factor(testdataori$Income, levels = c("<=50K.", ">50K."))) - 1
res_lasso <- confusion.glmnet(fit1, newx = testx, newy = testy)
res_lasso
##          True
## Predicted     0    1 Total
##     0     10598 2460 13058
##     1       762 1240  2002
##     Total 11360 3700 15060
## 
##  Percent Correct:  0.7861
confusion.glmnet(fit2, newx = testx, newy = testy)
##          True
## Predicted     0    1 Total
##     0     10415 2334 12749
##     1       945 1366  2311
##     Total 11360 3700 15060
## 
##  Percent Correct:  0.7823

# Lasso Logistics Regression
# 选出训练集中所有的正样本,共7508条
traindatare1 <- traindata0[which(traindata0$Income==1),]
# 在负样本中随机选出相同条数的负样本
traindatare0 <-  traindata0[which(traindata0$Income==0),]
trainre_sub = sample(22654,7508)
traindatare0 <- traindatare0[trainre_sub,]
# 合并正负样本为重采样的训练集
traindatare <- rbind(traindatare1,traindatare0)
rey <- traindatare$Income
rex <- subset(traindatare , select = -Income)
cvfitre <- cv.glmnet(alpha = 1, x = as.matrix(rex), y = rey, family="binomial",type.measure="class", nfolds = 11)
cvfitre
## 
## Call:  cv.glmnet(x = as.matrix(rex), y = rey, type.measure = "class",      nfolds = 11, alpha = 1, family = "binomial") 
## 
## Measure: Misclassification Error 
## 
##       Lambda Index Measure       SE Nonzero
## min 0.005823    42  0.1820 0.002208      31
## 1se 0.007013    40  0.1831 0.002026      29
fitre <- glmnet(alpha = 1, lambda =cvfitre$lambda.min, x = as.matrix(rex), y = rey, family="binomial",type.measure="class", nfolds = 10)
res1_lasso <- confusion.glmnet(fitre, newx = testx, newy = testy)
res1_lasso
##          True
## Predicted     0    1 Total
##     0      8878 1377 10255
##     1      2482 2323  4805
##     Total 11360 3700 15060
## 
##  Percent Correct:  0.7438

# Decision Tree
library(rpart)
library(e1071)
## Warning: 程辑包'e1071'是用R版本4.2.2 来建造的
traindata0$Income <- factor(traindata0$Income)
fit3 <- tune.rpart(Income ~ ., data = traindata0, minsplit = c(4,5,6),cp = c(0.1, 0.2, 0.05, 0.03, 0.01),tunecontrol = tune.control(sampling = "cross", cross = 10))
fit3
## 
## Parameter tuning of 'rpart.wrapper':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  minsplit   cp
##         4 0.01
## 
## - best performance: 0.1588091
tree <- rpart(Income~., data = traindata0, control = rpart.control(cp = 0.03, minsplit = 5))
obj0 <- predict(tree , traindata0 ,type =  "class")
res_train_tree <- table(list(pred=obj0, real=y))
res_train_tree
##     real
## pred     0     1
##    0 21507  3671
##    1  1147  3837
obj1 <- predict(tree , testdata0 ,type =  "class")
res_tree <- table(list(pred=obj1,real=testy))
res_tree
##     real
## pred    0    1
##    0 7257  372
##    1 4103 3328

# Decision Tree + Resample

tree_traindatare <- traindatare
tree_traindatare$Income <- factor(tree_traindatare$Income)
fit4 <- tune.rpart(Income ~ ., data = tree_traindatare, minsplit = c(5,10,15),cp = c(0.1, 0.2, 0.05, 0.03),tunecontrol = tune.control(sampling = "cross", cross = 10))
fit4
## 
## Parameter tuning of 'rpart.wrapper':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  minsplit   cp
##         5 0.03
## 
## - best performance: 0.2219633
tree1 <- rpart(Income~., data = tree_traindatare, control = rpart.control(cp = 0.03, minsplit = 5))
obj2 <- predict(tree1 , testdata0, type="class")
res1_tree <- table(list(pred=obj2,real=testy))
res1_tree
##     real
## pred    0    1
##    0 7257  372
##    1 4103 3328

# Lightgbm

lgb_res <- table(list(pre=pred_y ,real=testy))
lgb_res
##    real
## pre     0     1
##   0 10549  2146
##   1   811  1554
lgb_acc <- (lgb_res[1,1]+lgb_res[2,2])/15060
lgb_recall <- lgb_res[2,2]/3700
print(lgb_acc)
## [1] 0.8036521
print(lgb_recall)
## [1] 0.42

# Lightgbm + Resample
lgbre <- traindatare
lgbre_x <- subset(lgbre, select = -Income)
lgbre_y <-lgbre$Income
lgbre_res <- table(list(pre=pred_y2 ,real=testy))
lgbre_res
##    real
## pre    0    1
##   0 9425 1413
##   1 1935 2287
lgbre_acc <- (lgbre_res[1,1]+lgbre_res[2,2])/15060
lgbre_recall <- lgbre_res[2,2]/3700
print(lgbre_acc)
## [1] 0.7776892
print(lgbre_recall)
## [1] 0.6181081
model <- c("Lasso","Decision Tree")
train_accuracy <- c((res_train_lasso[1,1]+res_train_lasso[2,2])/30162,(res_train_tree[1,1]+res_train_tree[2,2])/30162)
test_accuracy <- c((res_lasso[1,1]+res_lasso[2,2])/15060,(res_tree[1,1]+res_tree[2,2])/15060)
train_recall <- c(res_train_lasso[2,2]/7508,res_train_tree[2,2]/7508)
test_recall <- c(res_lasso[2,2]/3700,res_tree[2,2]/3700)
resample_accuracy <- c((res1_lasso[1,1]+res1_lasso[2,2])/15060,(res1_tree[1,1]+res1_tree[2,2])/15060)
resample_recall <- c(res1_lasso[2,2]/3700,res1_tree[2,2]/3700)
result <- data.frame(model,train_accuracy,test_accuracy,train_recall,test_recall,resample_accuracy,resample_recall)
result
##           model train_accuracy test_accuracy train_recall test_recall
## 1         Lasso      0.8491479     0.7860558    0.6118807   0.3351351
## 2 Decision Tree      0.8402626     0.7028552    0.5110549   0.8994595
##   resample_accuracy resample_recall
## 1         0.7437583       0.6278378
## 2         0.7028552       0.8994595


# Model Comparision
p1 <- ggplot(result, aes(x=model, y=train_accuracy)) + geom_bar(stat="identity", position="dodge", show.legend = FALSE, aes(fill=model))
p2 <- ggplot(result, aes(x=model, y=test_accuracy)) + geom_bar(stat="identity", position="dodge", show.legend = FALSE, aes(fill=model))
p3 <- ggplot(result, aes(x=model, y=train_recall)) + geom_bar(stat="identity", position="dodge", show.legend = FALSE, aes(fill=model))
p4 <- ggplot(result, aes(x=model, y=test_recall)) + geom_bar(stat="identity", position="dodge",show.legend = FALSE, aes(fill=model))
(p1 + p2)/(p3 + p4)

# Resample Comparison
time <- c("before","after")
lasso_accuracy <- c((res_lasso[1,1]+res_lasso[2,2])/15060,(res1_lasso[1,1]+res1_lasso[2,2])/15060)
lasso_recall <- c(res_lasso[2,2]/3700,res1_lasso[2,2]/3700)
lasso <- data.frame(time,lasso_accuracy,lasso_recall)
p5 <- ggplot(lasso, aes(x=time, y=lasso_accuracy)) + geom_bar(stat="identity", position="dodge", show.legend = FALSE, aes(fill=time))
p6 <- ggplot(lasso, aes(x=time, y=lasso_recall)) + geom_bar(stat="identity", position="dodge", show.legend = FALSE, aes(fill=time))
###tree data
time <- c("before","after")
tree_accuracy <- c((res_tree[1,1]+res_tree[2,2])/15060,(res1_tree[1,1]+res1_tree[2,2])/15060)
tree_recall <- c(res_tree[2,2]/3700,res1_tree[2,2]/3700)
treeframe <- data.frame(time,tree_accuracy,tree_recall)
#### tree plot
p7 <- ggplot(treeframe, aes(x=time, y=tree_accuracy)) + geom_bar(stat="identity", position="dodge", show.legend = FALSE, aes(fill=time))
p8 <- ggplot(treeframe, aes(x=time, y=tree_recall)) + geom_bar(stat="identity", position="dodge", show.legend = FALSE, aes(fill=time))
### lgb data
time <- c("before","after")
lgb_accuracy <- c(lgb_acc,lgbre_acc)
lgb_recall <- c(lgb_recall,lgbre_recall)
lgbframe <- data.frame(time,lgb_accuracy,lgb_recall)
### lgb plot
p9 <- ggplot(lgbframe, aes(x=time, y=lgb_accuracy)) + geom_bar(stat="identity", position="dodge", show.legend = FALSE, aes(fill=time))
p10 <- ggplot(lgbframe, aes(x=time, y=lgb_recall)) + geom_bar(stat="identity", position="dodge", show.legend = FALSE, aes(fill=time))
(p5 + p7 +p9)/(p6 + p8 + p10)

