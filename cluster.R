# ���ݶ�ȡ 
x<-c(1,2,6,8,11) 
# ������� 
dim(x)<-c(5,1) 
#������� 
d<-dist(x) 
#������� 
hc1<-hclust(d, "single") 
hc2<-hclust(d, "complete") 
hc3<-hclust(d, "median") 
hc4<-hclust(d, "mcquitty") 
#��ͼ���� 
par(mfrow = c(2, 2)) 
#��ͼ 
plot(hc1,hang=-1) 
plot(hc2,hang=-1) 
plot(hc3,hang=-1) 
plot(hc4,hang=-1) 


par(opar) 
#��ʾplot���÷� 
dend1<-as.dendrogram(hc1) 
opar <- par(mfrow = c(2, 2),mar = c(4,3,1,2)) 
plot(dend1) 
plot(dend1, nodePar=list(pch = c(1,NA), cex=0.8, lab.cex=0.8), type = "t", center=TRUE) 
plot(dend1, edgePar=list(col = 1:2, lty = 2:3), dLeaf=1, edge.root = TRUE) 
plot(dend1, nodePar=list(pch = 2:1, cex=.4*2:1, col=2:3), horiz=TRUE) 
par(opar) 

if (false) {
���� 
��305��Ů��ѧ�������˸� ����ָ�꣬��Ӧ����ؾ��������ʾ�������ϵ����������ϵ�����������Ϊdij=1-rij,������뷨��ϵͳ������ 
}
# ���ݶ�ȡ 
x<-c(1.000, 0.846, 0.805, 0.859, 0.473, 0.398, 0.301, 0.382, 
0.846, 1.000, 0.881, 0.826, 0.376, 0.326, 0.277, 0.277, 
0.805, 0.881, 1.000, 0.801, 0.380, 0.319, 0.237, 0.345, 
0.859, 0.826, 0.801, 1.000, 0.436, 0.329, 0.327, 0.365, 
0.473, 0.376, 0.380, 0.436, 1.000, 0.762, 0.730, 0.629, 
0.398, 0.326, 0.319, 0.329, 0.762, 1.000, 0.583, 0.577, 
0.301, 0.277, 0.237, 0.327, 0.730, 0.583, 1.000, 0.539, 
0.382, 0.415, 0.345, 0.365, 0.629, 0.577, 0.539, 1.000) 
# ���� 
names<-c("���� "," �ֱ۳�"," ��֫�� "," ��֫�� "," ���� "," ��Χ ", 
" ��Χ "," �ؿ� ") 
# ת��Ϊ���� 
r<-matrix(x, nrow=8, dimnames=list(names, names)) 
#ת��Ϊ������� 
d<-as.dist(1-r) 
#������� 
hc<-hclust(d) 
#ת��Ϊ����ͼ 
dend<-as.dendrogram(hc) 
#��ͼ 
plot(hc) 
# ȷ����ĸ��� 
plclust(hc, hang=-1); re<-rect.hclust(hc, k=3) 