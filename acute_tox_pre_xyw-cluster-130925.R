# LOG
 # version:2013.09.22
 # author:xyw

  
library('caret')
library('MASS')
library('pls')
## packages("fBasics") for normal test
## install.packages("fBasics")
install.packages('DMwR')
library('DMwR')
library(fBasics)

 # acuteData <- read.table(file = file.choose(), sep="\t", header=TRUE)
acuteData <- read.table("F:\\work\\tox_predict\\acutetoxicity_LD50_QSAR_Analysis\\20121114_compounds_acutetoxicity\\322_compounds_Mold2_descripters.txt",
                      sep="\t",header=TRUE)
head(names(acuteData))

 ## 44:327 lines, compounds' data 
 ## acuteData<-acuteData[44:327,]

acuteDes <- acuteData[,-1]
acuteCla <- acuteData[,1]

# Explore Individual Variables 
summary(acuteCla)
quantile(acuteCla)
quantile(acuteCla,c(.1, .2, .5, .8, 1))
var(acuteCla)

plot(density(acuteCla))
table(acuteCla)
pie(table(acuteCla))
barplot(table(acuteCla))

## Normal curve fitting  
FitNormalCurve <- function(list, xlabel="values") {
    ### to display every image until you close it 
    x11()  
    ### F, is short for FALSE.
    hist(list, freq = F, 
        ### probability	an alias for !freq, for S compatibility.
        probability=,
        xlab = xlabel, 
        ### set the values range of x axis 
        xlim = c(ceiling(min(list)),ceiling(max(list))),
        ### 标题中显示均值
        main = paste('样本的均数：', round(mean(list),2), ' ') 
        )
      ### Kernel Density Estimation
      lines(density(list))
      ### Normal curve fitting 
      ### lwd, line's width
      curve(dnorm(x, mean(list), sd(list)), 
            col='red', add=T,  lwd=1)
    shapiroTest(list)
}

if (FALSE) {
 ## convert to -logld50
acuteCla <- -acuteCla
acuteCla <- log10(acuteData[,1]*1000)
}

 # CONVERT TO LOG(1/(mg/kg))
acuteCla_unit <- acuteCla*1000 # kg/kg to mg/kg
 # find the outliers 
if (FALSE) { # Univariate Outlier Detection
names(acuteCla_unit) <- 1:332
outliers_v <- boxplot.stats(acuteCla_unit)$out
boxplot(acuteCla_unit)
outliers <- names(acuteCla_unit[acuteCla_unit %in% outliers_v ])
outliers <- as.numeric(outliers)
}

outlier.scores <- lofactor(acuteDes, k=15)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:20]
outliers 

str(acuteDes)
acuteCla <- acuteCla_unit[-outliers]
acuteDes <- acuteDes[-outliers,]

str(acuteDes )


# acuteCla <- log(acuteCla, base = exp(1))
acuteCla <- log(acuteCla, base = exp(10))
summary(acuteCla)
FitNormalCurve(acuteCla,"lg(LD50)")


 # remove spare avariables
zerovar = nearZeroVar(acuteDes)
acuteDes1 <- acuteDes[,-zerovar]

 # remove avariables with small correlation
descrCorr = cor(acuteDes1)
highCorr = findCorrelation(descrCorr,0.90)
acuteDes2 <- acuteDes1[,-highCorr]

 # remove multi-line-correlated avariables
comboInfo = findLinearCombos(acuteDes2)
 # acuteDes3 = acuteDes2[,-comboInfo$remove]
acuteDes3 <- acuteDes2

 # don't normalize data
 # normalizing data.....
Process = preProcess(acuteDes3)
acuteDes4 = predict(Process, acuteDes3)
acuteDes4 = as.data.frame(acuteDes4)
dim(acuteDes4)
names(acuteDes4)
rownames(acuteDes4)
attributes(acuteDes4)
 # list data to check it
acuteDes4[1:5, 1:5]

 ## k-means cluster ## no useful section, xyw, 20130922
 #	set cluster number to 10
 # kc.10 <- kmeans(acuteDes4,10)

## Hierarchical Clustering
    ### ?hclust
    ### ?dist
    ### method_ = "ward", single, complete, average,mcquitty,median
h.clust <- function(data_,method_,k_cut) {
    hc <- hclust(dist(data_),method=method_)
    sub_title <- paste("R", 
                 format(Sys.time(), "%Y-%b-%d %H:%M:%S"),
                 Sys.info()["user"]
                )
    plot(hc,hang = -1,
          labels=rownames(data_),
          xlab = paste("hclust mathod:",method_),
          main = paste("HCluster_Dendrogram_data, k=", k_cut),
          sub = sub_title
         )

    # cut tree into 10 clusters
    # which selects clusters by number (from left to right in the tree)
    g <- rect.hclust(hc,k=k_cut)
    # groups is different from g. "groups" is do with the orignal names
    groups <- cutree(hc,k=k_cut)
    return (groups)
}
if (FALSE) {
    h.clust(acuteDes4,"single",10)
    h.clust(acuteDes4,"complete",10)
    h.clust(acuteDes4,"average",10)
    h.clust(acuteDes4,"mcquitty",10)
    h.clust(acuteDes4,"median",10)
    h.clust(acuteDes4,"centroid",10)
}
groups <- h.clust(acuteDes4,"ward",2)
rownames(acuteDes4)
names(groups)
groups
groups <- as.matrix(groups)
head(groups[, 1])
summary(groups)
table(groups)
pie(table(groups))

# ----------------------
### create 2 classes of data_frame 
acuteDes4 <- as.matrix(acuteDes4)
str(acuteDes4)
str(acuteCla)
total_data <- data.frame("class"=groups[,1],"acuteCla"=acuteCla,"acuteDes4"=I(acuteDes4))
str(total_data)
data_1_flag <- total_data$class == 1
data_1_flag
data_1_id <- c(1:312)[data_1_flag]
data_1_id
data_1 <- data.frame("ld50"=total_data$acuteCla[data_1_id]
                    ,"des4"=I(total_data$acuteDes4[data_1_id,]))
str(data_1)
data_1$ld50
###　make sure data frame well
data_1[1:2,]
total_data[c(1,3),]

data_2_id <- c(1:312)[!data_1_flag]
data_2 <- data.frame("ld50"=total_data$acuteCla[data_2_id]
                    ,"des4"=total_data$acuteDes4[data_2_id,])
str(data_2)
data_total <-  data.frame("ld50" = acuteCla, "des4" = I(acuteDes4))
str(data_total )

if (FALSE) {
 ## old codes followed
 ### creat data_frame for modeling
acuteDes4 <- as.matrix(acuteDes4)
str(acuteDes4)
testdata <- data.frame("acuteCla"=acuteCla,"acuteDes4"=I(acuteDes4))
str(testdata)
 ## old codes upper
}

## 残差分析说明
### Residual_Error_Analysis_Meaning
REAM<- function() {
    Residual_Error_Analysis_Meaning <- 
        strwrap("
            得到的四个图依次为：
            4.1普通残差与拟合值的残差图
            4.2正态QQ的残差图（若残差是来自正态总体分布的样本，则QQ图中的点应该在一条直线上）
            4.3标准化残差开方与拟合值的残差图（对于近似服从正态分布的标准化残差，应该有95%的样本点落在[-2,2]的区间内。这也是判断异常点的直观方法）
            4.4cook统计量的残差图（cook统计量值越大的点越可能是异常值，但具体阀值是多少较难判别）
            从图中可见，xx样本存在异常，需要剔除。
        ")
    print(Residual_Error_Analysis_Meaning)
}
REAM()
    ## step lm function
    ### use step lm mothod... multiple linear regression
    ### yeli:it better to GA-PLS agriolthm to select descripters.
    ### Args:
    ###     data_in:    data frame input
    ###     class_:     dependent variable. One dimmer name of data_in, which will be predicted
    ###     descriptors: independent variables.
    ###     verbose:    If TRUE, prints sample covariance; if not, not. Default is TRUE.
    ### 
    ### Returns:
    ###     None.
StepMLR <- function(data_in,class_,descriptors,ResidualE_analysis=FALSE,verbose = TRUE) {
    data_in$class_  # no success to display data_in$ld50
    f <- paste(class_ , "~", descriptors)
    lm.test <- lm(f, data=data_in)
    if (ResidualE_analysis) {
	  print(paste("lm.test",lm.test))
        #### 用残差分析 show and 剔除异常点
        REAM()
        #### plot(lm.test, which = 1:4)
        plot(lm.test, which = 1:4) 
        #### plot is shot for plot.lm
    }
    
    step.acute <- step(lm.test, direction = "both")
    drop.acute <- drop1(step.acute)
    print(summary(step.acute)) #### $coefficients str()

    if (verbose) {
        pred_step.acute <- predict(step.acute,newdata=data_in)
        #### pred_step.acute
        plot( as.vector(as.matrix(data_in[class_])) ~ pred_step.acute
            , xlab="Observed acute value (log10(LD50 mg/kg))"
            , ylab="Predicted acute value (log10(LD50 mg/kg))",)
        lines(-1:5,-1:5)
        title("Predicted VS Observed LD50")
    }
    return(step.acute)
}

StepMLR(data_total,"ld50","des4",,TRUE)
data_1_lm <- StepMLR(data_1,"ld50","des4",,)
AIC(data_1_lm) # calc the 赤池信息统计量
shapiro.test(data_1_lm$residuals) # normality test for residuals

data_2_re <- StepMLR(data_2,"ld50","des4",,)
### 20130917,xwang
#### classification models are better than the model based data_total



testDes <- acuteData[1:43,-1]
testVal <- acuteData[1:43,1]
testVal<- -testVal
testVal<-log10(testVal*1000)
Process = preProcess(testDes)
testDes1 = predict(Process,testDes)