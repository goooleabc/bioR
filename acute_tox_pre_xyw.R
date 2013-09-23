library('caret')
library('MASS')
library('pls')
# acuteData <- read.table(file = file.choose(), sep="\t", header=TRUE)
acuteData<-read.table("F:\\work\\tox_predict\\LD50_QSAR_Analysis\\20121114_compounds_acutetoxicity\\322_compounds_Mold2_descripters.txt",
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

#use GA-PLS to select variables.
library(genalg)

TestData <- cbind(acuteCla,acuteDes4)
TestData <- as.data.frame(TestData)
numberOfWavelengths = ncol(TestData)-1
evalVals <- function(chromosome = c()){
  returnVal = 100
  if(sum(chromosome) > 2){
    TestData = TestData[,chromosome == 1];
    pls.model <- plsr(acuteCla ~ ., data = TestData, validation = "LOO")
    rmsep <- RMSEP(pls.model)
    returnVal <- min(rmsep$val[1,1,])
  }
  returnVal
}

rbga.results.pls = rbga.bin(size = numberOfWavelengths, zeroToOneRatio = 10,
                            evalFunc = evalVals, popSize = 200, iters = 150, verbose = TRUE)
summary.rbga.results.pls <- summary.rbga(rbga.results.pls)
ga.best.number<-as.double(strsplit(strsplit(strsplit(summary.rbga.results.pls,
                                                     "Best Solution : ")[[1]][2]," \n")[[1]][1]," ")[[1]])
ga.best.number <- as.logical(ga.best.number)
acuteDes6 <- acuteDes4[,ga.best.number]


### creat data_frame for modeling
acuteDes6 <- as.matrix(acuteDes6)
str(acuteDes6)

testdata <- data.frame("acuteCla"=acuteCla,"acuteDes6"=I(acuteDes6))
str(testdata)
less <- testdata$acuteCla < 0



# predict dataset
acuteCla_pre <- acuteCla[1:43]
str(acuteCla_pre)
acuteDes6_pre <- acuteDes6[1:43,]
str(acuteDes6_pre)
pred_data <- data.frame("acuteCla"=acuteCla_pre,"acuteDes6"=I(acuteDes6_pre))
str(pred_data)

####¡¡use mult-regress to try       
lm.acute <- lm(acuteCla ~ acuteDes6,data=testdata)
summary(lm.acute)
testdata.pred.lm <- 
	predict(lm.acute,newdata=testdata)
summary(testdata.pred.lm)
str(testdata.pred.lm)
str(acuteCla)
plot(acuteCla~testdata.pred.lm
	,xlab="Observed acute value (log10(LD50 mg/kg))"
	,ylab="Predicted acute value (log10(LD50 mg/kg))")
lines(-2:5,-2:5)
####¡¡end of use mult-regress to try     

#### use step lm mothod...
# yeli:it better to GA-PLS agriolthm to select descripters.
lm.acute=lm(acuteCla~acuteDes6,data=testdata)
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

#### start to model with plsr ####
pls.acute <- plsr(acuteCla ~ acuteDes6,
	ncomp = 7, data = testdata, validation = "CV")
## calc rmse for plot               
str(RMSEP(pls.acute))
plot(RMSEP(pls.acute),legendpos = "topright")

#show the cross-validated predictions with different components versus measured
#values.
plot(pls.acute, ncom = 1, asp = 1, line = TRUE)

#plot for the score values for the three first components.
plot(pls.acute, plottype = "scores", comps = 1:3)

#explained variances can be extracted explicitly with:
explvar(pls.acute)
##pls.acute$model
summary(pls.acute)
## to show coefplot
plot(pls.acute, plottype = "coef", ncomp = 1:5, 
	legendpos = "bottomleft")

#the loading plot is much used for interpretation purposes.
plot(pls.acute, comps = 1:2, legendpos = "topleft",
	labels = "numbers", xlab = "nm")
abline(h = 0)

testdata.pred.plr.resp <- 
	predict(pls.acute,type="response",newdata=testdata)
str(testdata.pred.plr.resp)
str(acuteCla)
plot(testdata.pred.plr.resp[,1,1]~acuteCla
	,xlab="Observed acute value (log10(LD50 mg/kg))"
	,ylab="Predicted acute value (log10(LD50 mg/kg))")


## predict pred_dataset
tcmd09.pred.plr.resp <- 
	predict(pls.acute,type="response",newdata=pred_data)
str(tcmd09.pred.plr.resp)
tcmd09.pred.plr.resp[,,2]
pred_data$acuteCla

# list
plot(tcmd09.pred.plr.resp[,,2]~pred_data$acuteCla)

predplot(pls.acute, ncomp = 2, newdata = pred_data, asp = 1, line = TRUE)
#### end of plsr ####


