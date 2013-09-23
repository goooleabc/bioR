library('caret')
library('MASS')
library('pls')
# acuteData <- read.table(file = file.choose(), sep="\t", header=TRUE)
acuteData<-read.table("F:\\work\\tox_predict\\LD50_QSAR_Analysis\\20121114_compounds_acutetoxicity\\322_compounds_Mold2_descripters.txt",
                      sep="\t",header=TRUE)
head(names(acuteData))
train_list<- as.numeric(row.names(acuteData)) > 43
train_list

acuteDes <- acuteData[,-1]
head(names( acuteDes ))

#remove spare avariables
zerovar=nearZeroVar(acuteDes)
zerovar
acuteDes1<-acuteDes[,-zerovar]

#remove avariables with small correlation
descrCorr=cor(acuteDes1)
highCorr=findCorrelation(descrCorr,0.90)
acuteDes2<-acuteDes1[,-highCorr]

#remove multi-line-correlated avariables
comboInfo = findLinearCombos(acuteDes2)
comboInfo
acuteDes3 <- acuteDes2 
if (!is.null(comboInfo$remove)) acuteDes3 <- acuteDes2[,-comboInfo$remove]
## ending -- get rid of avariables

#normalizing data.....
Process = preProcess(acuteDes3)
acuteDes4 = predict(Process,acuteDes3)
acuteDes5 = as.data.frame(acuteDes4)

Process = preProcess(testDes)
testDes1 = predict(Process,testDes)


## starting -- combine training data for model
acuteDes5<-as.matrix(acuteDes5)
acute_cmpds <- data.frame(
		desc=I(acuteDes5)
		,LD50=acuteData$LD50
		,train=train_list
		)
## ?I
/*
head(acute_cmpds)
acute_cmpds <- as.data.frame(acute_cmpds)
str(acute_cmpds)
*/
# names(acute_cmpds) <- c("desc","LD50","train")
str(acute_cmpds)

training_data <- acute_cmpds[acute_cmpds$train,]
## test ## training_data <- acute_cmpds
str(training_data)
predict <- acute_cmpds[!acute_cmpds$train,]
str(predict)

### creat mvr(PLS & PCR) model
acute.mvr <- mvr(LD50 ~ desc,ncomp = 6,data=training_data,validation = "LOO")
plot(RMSEP(acute.mvr),legendpos = "topright")
#predict responses for a single model with components 1,2,3,4,5
tcmd09.pred.mvr.resp <- 
	predict(acute.mvr,comps=1:5,type=¡±response¡±,newdata=acute_cmpds[!acute_cmpds$train,])
print(cbind(tcmd09.pred.resp,predict$LD50))

#### this a plsr eg ####
data(yarn)
str(yarn[yarn$train,])
yarn.pls<-plsr(density~NIR, ncomp=6,data=yarn[yarn$train,], validation = "CV")
plot(RMSEP(yarn.pls),legendpos = "topright") 
#### plsr eg ending ###

#### creat PLS  model
str(training_data)
head(training_data)
acute.pls <- plsr(LD50~desc,ncomp = 10,data=acute_cmpds,validation = "LOO")
plot(RMSEP(acute.pls),legendpos = "topright")
plot(acute.pls,ncomp=2,asp=1,line=TRUE)
total_pred <-
	predict(acute.pls,type="response",newdata=acute_cmpds)
str(total_pred)
total_pred<-total_pred[,1,2]

head(total_pred)
str(acute_cmpds$LD50)
training_data[training_data$LD50,]
plot(total_pred~acute_cmpds$LD50)
lines(-1:5,-1:5)

tcmd09.pred.plr.resp <- 
	predict(acute.pls,type="response",newdata=predict)
tcmd09.pred.plr.resp
print(cbind(tcmd09.pred.plr.resp,predict$LD50))
predplot(acute.pls, ncomp = 2, newdata = predict, asp = 1, line = TRUE)




