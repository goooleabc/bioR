# 数据读取 
x<-c(1,2,6,8,11) 
# 定义矩阵 
dim(x)<-c(5,1) 
#计算距离 
d<-dist(x) 
#聚类分析 
hc1<-hclust(d, "single") 
hc2<-hclust(d, "complete") 
hc3<-hclust(d, "median") 
hc4<-hclust(d, "mcquitty") 
#绘图分栏 
par(mfrow = c(2, 2)) 
#绘图 
plot(hc1,hang=-1) 
plot(hc2,hang=-1) 
plot(hc3,hang=-1) 
plot(hc4,hang=-1) 


par(opar) 
#演示plot的用法 
dend1<-as.dendrogram(hc1) 
opar <- par(mfrow = c(2, 2),mar = c(4,3,1,2)) 
plot(dend1) 
plot(dend1, nodePar=list(pch = c(1,NA), cex=0.8, lab.cex=0.8), type = "t", center=TRUE) 
plot(dend1, edgePar=list(col = 1:2, lty = 2:3), dLeaf=1, edge.root = TRUE) 
plot(dend1, nodePar=list(pch = 2:1, cex=.4*2:1, col=2:3), horiz=TRUE) 
par(opar) 

if (false) {
例二 
对305名女中学生测量八个 体型指标，相应的相关矩阵如表所示，将相关系数看成相似系数，定义距离为dij=1-rij,用最长距离法作系统分析。 
}
# 数据读取 
x<-c(1.000, 0.846, 0.805, 0.859, 0.473, 0.398, 0.301, 0.382, 
0.846, 1.000, 0.881, 0.826, 0.376, 0.326, 0.277, 0.277, 
0.805, 0.881, 1.000, 0.801, 0.380, 0.319, 0.237, 0.345, 
0.859, 0.826, 0.801, 1.000, 0.436, 0.329, 0.327, 0.365, 
0.473, 0.376, 0.380, 0.436, 1.000, 0.762, 0.730, 0.629, 
0.398, 0.326, 0.319, 0.329, 0.762, 1.000, 0.583, 0.577, 
0.301, 0.277, 0.237, 0.327, 0.730, 0.583, 1.000, 0.539, 
0.382, 0.415, 0.345, 0.365, 0.629, 0.577, 0.539, 1.000) 
# 命名 
names<-c("身高 "," 手臂长"," 上肢长 "," 下肢长 "," 体重 "," 颈围 ", 
" 胸围 "," 胸宽 ") 
# 转换为矩阵 
r<-matrix(x, nrow=8, dimnames=list(names, names)) 
#转换为距离矩阵 
d<-as.dist(1-r) 
#聚类计算 
hc<-hclust(d) 
#转换为聚类图 
dend<-as.dendrogram(hc) 
#绘图 
plot(hc) 
# 确定类的个数 
plclust(hc, hang=-1); re<-rect.hclust(hc, k=3) 