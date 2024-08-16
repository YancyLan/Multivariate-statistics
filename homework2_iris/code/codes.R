iris
#第一步：定义虚拟变量，处理离散变量
iris$isSetosa <- ifelse(iris$Species == "setosa",1,0)
iris$isVersicolor <- ifelse(iris$Species == "versicolor",1,0)
iris_d=iris[,1:4]

#检验每一个变量的qqplot
# individual Q-Q plot
layout(matrix(1:4,nc=2))
sapply(colnames(iris_d),function(x)
{qqnorm(iris_d[[x]],main=x)
  qqline(iris_d[[x]])
})
library(ggplot2)
ggqqplot(iris[,1:4],combine = TRUE,colnames(iris[,1:4]))
# scatterplot matrix   
pairs(iris_d1)   

# chi-square Q-Q plot with outlier detection
y <- iris_d
cm <- colMeans(y)
S <- cov(y)
d <- apply(y, 1, function(y) t(y - cm) %*% solve(S) %*% (y - cm))
plot(qc <- qchisq((1:nrow(y) - 1/2) / nrow(y), df = 7), sd <- sort(d),
     xlab = expression(paste(chi[7]^2, " Quantile")),
     ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1))
oups <- which(rank(abs(qc - sd), ties = "random") > nrow(y) - 3)
text(qc[oups], sd[oups] - 1.5, names(oups))
abline(a = 0, b = 1)

#正式的数学检验

library(mvnormtest)
iris_d=-t(iris[,1:4])
mshapiro.test(iris_d)
