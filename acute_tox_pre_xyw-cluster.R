library('caret')
library('MASS')
library('pls')
# acuteData <- read.table(file = file.choose(), sep="\t", header=TRUE)
acuteData<-read.table("F:\\work\\tox_predict\\acutetoxicity_LD50_QSAR_Analysis\\20121114_compounds_acutetoxicity\\322_compounds_Mold2_descripters.txt",
                      sep="\t",header=TRUE)
head(names(acuteData))

acuteData<-acuteData[44:327,]

acuteDes<-acuteData[,-1]
acuteCla<-acuteData[,1]

#convert to -logld50
acuteCla<--acuteCla
acuteCla<-log10(acuteData[,1]*1000)

#remove spare avariables
zerovar=nearZeroVar(acuteDes)
acuteDes1<-acuteDes[,-zerovar]

#remove avariables with small correlation
descrCorr=cor(acuteDes1)
highCorr=findCorrelation(descrCorr,0.90)
acuteDes2<-acuteDes1[,-highCorr]

#remove multi-line-correlated avariables
comboInfo = findLinearCombos(acuteDes2)
#acuteDes3=acuteDes2[,-comboInfo$remove]
acuteDes3<- acuteDes2

#don't normalize data
#normalizing data.....
Process = preProcess(acuteDes3)
acuteDes4 = predict(Process,acuteDes3)
acuteDes4 = as.data.frame(acuteDes4)
dim(acuteDes4)
names(acuteDes4)
rownames(acuteDes4)
attributes(acuteDes4)
# list data to check it
acuteDes4[1:5,1:5]

## k-means cluster
#	set cluster number to 10
kc.10 <- kmeans(acuteDes4,10)

    ## Hierarchical Clustering
    # ?hclust
    # ?dist
    # method_ = "ward", single, complete, average,mcquitty,median
h.clust <- function(data_,method_,k_cut) {
    hc <- hclust(dist(data_),method=method_)
    sub_title <- paste("R", 
                 format(Sys.time(), "%Y-%b-%d %H:%M:%S"),
                 Sys.info()["user"]
                )
    plot(hc,hang = -1,
          labels=rownames(data_),
          main="HCluster_Dendrogram_data",
          sub = sub_title
         )

    # cut tree into 10 clusters
    # which selects clusters by number (from left to right in the tree)
    g <- rect.hclust(hc,k=k_cut)
    # groups is different from g. "groups" is do with the orignal names
    groups <- cutree(hc,k=k_cut)
    return (groups)
}
if £¨false£© {
    h.clust(acuteDes4,"single",10)
    h.clust(acuteDes4,"complete",10)
    h.clust(acuteDes4,"average",10)
    h.clust(acuteDes4,"mcquitty",10)
    h.clust(acuteDes4,"median",10)
    h.clust(acuteDes4,"centroid",10)
}
groups <- h.clust(acuteDes4,"ward",3)
names(groups)
groups
groups <- as.matrix(groups)
head(groups[, 1])
summary(groups)
table(groups)
pie(table(groups))

### creat data_frame for modeling
acuteDes4 <- as.matrix(acuteDes4)
str(acuteDes4)
testdata <- data.frame("acuteCla"=acuteCla,"acuteDes4"=I(acuteDes4))
str(testdata)

#### use step lm mothod...
# yeli:it better to GA-PLS agriolthm to select descripters.
lm.acute=lm(acuteCla~acuteDes4,data=testdata)
summary(lm.acute)
step.acute = step(lm.acute, direction = "both")
drop.acute = drop1(step.acute)
summary(step.acute)

pred_step.acute <- predict(step.acute,newdata=testdata)
pred_step.acute
plot(acuteCla~pred_step.acute, xlab="Observed acute value (log10(LD50 mg/kg))",
	ylab="Predicted acute value (log10(LD50 mg/kg))",)
lines(-1:5,-1:5)
title("Predicted VS Observed LD50")

#### end of using step lm mothod...




testDes <- acuteData[1:43,-1]
testVal <- acuteData[1:43,1]
testVal<- -testVal
testVal<-log10(testVal*1000)
Process = preProcess(testDes)
testDes1 = predict(Process,testDes)