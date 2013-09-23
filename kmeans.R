# create new dataset without missing data 
# newdata <- na.omit(mydata)

# 产生数据集
# rnorm(n, mean = 0, sd = 1)
x<-c(rnorm(200,30,1),rnorm(200,10,1.5),rnorm(100,5,0.5))
y<-c(rnorm(200,30,1),rnorm(200,10,1.5),rnorm(100,5,0.5))
data<-data.frame(x,y)

# 系统聚类
# 聚类的一些必要的函数
library(cluster)
library(rattle)
#系统聚类函数在包amap中
require(amap, quietly=TRUE)
#聚类结果有包fpc提供
require(fpc, quietly=TRUE)
#绘图 需cba包
require(cba, quietly=TRUE)

# Set the seed to get the same clusters each time.
set.seed(252964)	
?kmeans
# Generate a kmeans cluster of size 3.
data[,c(1:2)]
kmeans <- kmeans(na.omit(data[,c(1:2)]), 3)
## REPORT ON CLUSTER CHARACTERISTICS
kmeans
# Cluster sizes:
paste(kmeans$size, collapse=' ')
# Cluster centers:
kmeans$centers

# Within cluster sum of squares:
kmeans$withinss

# Generate a data plot.
par(bg="orange")
plot(na.omit(data[,c(1:2)]), col=kmeans$cluster)
title(main="", sub=paste("R", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Generate a discriminant coordinates plot.
par(bg="grey")
plotcluster(na.omit(data[,c(1:2)]), kmeans$cluster)
title(main="Discriminant Coordinates data", sub=paste("R", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
?plotcluster
cluster.stats(dist(na.omit(data[,c(1:2)])), kmeans$cluster)
