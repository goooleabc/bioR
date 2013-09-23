library('caret')
library('MASS')
library('pls')
# acuteData <- read.table(file = file.choose(), sep="\t", header=TRUE)
acuteData<-read.table("F:\\work\\tox_predict\\LD50_QSAR_Analysis\\20121114_compounds_acutetoxicity\\322_compounds_Mold2_descripters.txt",
                      sep="\t",header=TRUE)
acuteDes<-acuteData[,-1]
acuteVal<-acuteData[,1]

testDes <- acuteData[1:43,-1]
testVal <- acuteData[1:43,1]

acuteVal<-log10(acuteVal*1000)
testVal<-log10(testVal*1000)

#remove spare avariables
zerovar=nearZeroVar(acuteDes)
acuteDes1<-acuteDes[,-zerovar]

#remove avariables with small correlation
descrCorr=cor(acuteDes1)
highCorr=findCorrelation(descrCorr,0.90)
acuteDes2<-acuteDes1[,-highCorr]

#normalizing data.....
Process = preProcess(acuteDes2)
acuteDes3 = predict(Process,acuteDes2)
acuteDes4 = as.data.frame(acuteDes3)

Process = preProcess(testDes)
testDes1 = predict(Process,testDes)

#use step lm mothod...
#it better to GA-PLS agriolthm to select descripters.
acuteData_1<-cbind(acuteVal,acuteDes4)
lm.acute=lm(acuteVal~.,data=acuteData_1)
summary(lm.acute)

step.acute = step(lm.acute, direction = "both")
drop.acute = drop1(step.acute)
summary(step.acute)

pred_acute <- predict(step.acute, acuteDes4)
plot(pred_acute~acuteVal, xlab="Observed acute value (log10(LD50 mg/kg))",
	ylab="Predicted acute value (log10(LD50 mg/kg))",)
lines(-2:5,-2:5)
title("Predicted VS Observed LD50")

#get 150 descripters from step lm
acuteDes5<-acuteDes4[,names(step.acute$coefficients)[-1]]

#predict the 43 herbal compounds
testDes2 <- testDes1[,colnames(acuteDes5)]
pred_test <- predict(step.acute,testDes2)
str(pred_test)
str(testVal)
plot(pred_test~testVal, xlab="Observed acute value (log10(LD50 mg/kg))",
	ylab="Predicted acute value (log10(LD50 mg/kg))")
lines(-1:5,-1:5)
title("Predicted VS Observed LD50")
RMSE(pred_test,testVal)