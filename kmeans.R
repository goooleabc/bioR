# create new dataset without missing data 
# newdata <- na.omit(mydata)

# �������ݼ�
# rnorm(n, mean = 0, sd = 1)
x<-c(rnorm(200,30,1),rnorm(200,10,1.5),rnorm(100,5,0.5))
y<-c(rnorm(200,30,1),rnorm(200,10,1.5),rnorm(100,5,0.5))
data<-data.frame(x,y)

# ϵͳ����
# �����һЩ��Ҫ�ĺ���
library(cluster)
library(rattle)
#ϵͳ���ຯ���ڰ�amap��
require(amap, quietly=TRUE)
#�������а�fpc�ṩ
require(fpc, quietly=TRUE)
#��ͼ ��cba��
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