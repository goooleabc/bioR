## NEED TO BE optimized, ALEX

library(class)
library(kernlab)
library('caret')
library(genalg)
library(pls)


vknn = function(v,data,cl,k){
	#分隔原始数据
	grps = cut(1:nrow(data),v,labels=FALSE)[sample(1:nrow(data))]
	#对每份数据分别运行knn函数
	pred=lapply(1:v,function(i,data,cl,k){
		omit = which(grps == i)
		pcl = knn(data[-omit,],data[omit,],cl[-omit],k=k)	
	}, data,cl,k)
	#整合预测结果
	wh = unlist(pred)
	table(wh,cl[order(grps)])
}
train_dataset_fi <- "C:\\Documents and Settings\\lye\\MOE_file\\nephrotoxicity_SVM_20130325\\slider_344P&532N_MOLD2_descripter.txt"
train_dataset_fi <- "F:\\work\\tox_predict\\Nephrotoxicity\\nephrotoxicity_model\\slider_344P&532N_MOLD2_descripter.txt"
neph<-read.table(train_dataset_fi,sep="\t",header=TRUE)

neph[,1] <- as.factor(neph[,1])
dataDes <- neph[,-1]

#remove spare avariables
zerovar = nearZeroVar(dataDes)
dataDes1 = dataDes[,-zerovar]

#remove avariables with small correlation
descrCorr = cor(dataDes1)
highCorr = findCorrelation(descrCorr,0.9)
dataDes2 = dataDes1[,-highCorr]

#remove multi-line-correlated avariables
comboInfo = findLinearCombos(dataDes2)
dataDes3 = dataDes2[, -comboInfo$remove]

#normalizing data......
Process = preProcess(dataDes3)
dataDes4 = predict(Process, dataDes3)

new_neph <- cbind(neph[,1],dataDes4)

#do 10-cross-validation to construct KNN model using k=10
vknn(10,new_neph[,-1],new_neph[,1],10)

#do external test sets of 20 herbal compounds
#input external test data
#### testset_fi <- "C:\\Documents and Settings\\lye\\MOE_file\\nephrotoxicity_SVM_20130325\\external_20_testset_Mold2.txt"
 testset_fi <- "F:\\work\\tox_predict\\Nephrotoxicity\\nephrotoxicity_model\\external_20_testset_Mold2.txt"
#testset_fi <- "F:\\work\\tox_predict\\Nephrotoxicity\\nephrotoxicity_model\\10tcm_external_testset_des.txt"
external_set <- read.table(testset_fi,header=TRUE,sep="\t")
external_set[,1]<-as.factor(external_set[,1])
exterCla <- external_set[,1]

#get corresponding descriptors
exterDes <- external_set[,names(dataDes4)]

#normalizing data......
Process = preProcess(exterDes)
exterDes = predict(Process, exterDes)

new.exterset=cbind(exterCla,exterDes)
#get prediction for external test using k=10
knn(new_neph[,-1], exterDes, new_neph[,1], k = 10, l = 0, prob = FALSE, use.all = TRUE)

#### 
#do 10-cross-validation to construct KNN model using k=9
vknn(10,new_neph[,-1],new_neph[,1],9)

#get prediction for external test using k=9
knn(new_neph[,-1], exterDes, new_neph[,1], k = 9, l = 0, prob = FALSE, use.all = TRUE)


#### 
#do 10-cross-validation to construct KNN model using k=7
vknn(10,new_neph[,-1],new_neph[,1],7)

#get prediction for external test using k=7
knn(new_neph[,-1], exterDes, new_neph[,1], k = 7, l = 0, prob = FALSE, use.all = TRUE)

#### 
#do 10-cross-validation to construct KNN model using k=5
vknn(10,new_neph[,-1],new_neph[,1],5)

#get prediction for external test using k=5
knn(new_neph[,-1], exterDes, new_neph[,1], k = 5, l = 0, prob = FALSE, use.all = TRUE)

####
#do 10-cross-validation to construct KNN model using k=3
vknn(10,new_neph[,-1],new_neph[,1],3)

#get prediction for external test using k=3
knn(new_neph[,-1], exterDes, new_neph[,1], k = 3, l = 0, prob = FALSE, use.all = TRUE)

#### 
#do 10-cross-validation to construct KNN model using k=1
vknn(10,new_neph[,-1],new_neph[,1],1)

#get prediction for external test using k=1
knn(new_neph[,-1], exterDes, new_neph[,1], k = 1, l = 0, prob = FALSE, use.all = TRUE)






