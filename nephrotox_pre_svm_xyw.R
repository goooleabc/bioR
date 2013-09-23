# LOG:
##### 2013.09.18 alex
#####   This script should be run in R 3.0.0 Env.
#

library("kernlab")
library("caret")
library("genalg")
library("pls")
library("e1071")

# setwd("C:\\Documents and Settings\\lye\\MOE_file\\nephrotoxicity_SVM_20130325")
setwd("F:\\work\\tox_predict\\Nephrotoxicity\\nephrotoxicity_model")
data <- read.table("slider_344P&532N_MOLD2_descripter.txt",sep="\t",header=TRUE)
data[,1] <- as.factor(data[,1])
dataDes <- data[,-1]

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
dataDes3 = predict(Process, dataDes3)

#select proper descripters.
subsets = c(5,10,15,20,25,30,35,40,45,50)
ctrl = rfeControl(functions = rfFuncs, method = "cv", verbose = FALSE, 
	returnResamp = "final")
Profile = rfe(dataDes3, data[,1],sizes=subsets, rfeControl=ctrl)
print(Profile)
Profile$fit
Profile$optVariables
dataDes4 <- dataDes3[,Profile$optVariables]

new_data <- cbind(data[,1],dataDes3)
colnames(new_data)[1]<-"Neprotoxicity"

nep_svm <- ksvm(Neprotoxicity ~ .,data=new_data, kernel="rbfdot", kpar="automatic",
	C=1, cross=10,prob.model=TRUE)
predict(nep_svm,new_data)

 test_fi <- "external_20_testset_Mold2.txt"
test_fi <- "10tcm_external_testset_des.txt"
test <- read.table(test_fi,sep="\t",header=TRUE)
testCla <- test[,1]
testDes <- test[,-1]
testDes <- testDes[,colnames(dataDes3)]

#normalizing data......
Process = preProcess(testDes)
testDes = predict(Process, testDes)
test_new <- cbind(testCla,testDes)
predict(nep_svm, test_new)


