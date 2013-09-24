# Readme.md
Here is some R scripts written by goooleabc.
Update: 2013.09
---------------------------
## terms
### residuals standard error
  A fitted regression model uses the parameters to generate point estimate predictions which are the means of observed responses if you were to replicate the study with the same X values an infinite number of times (and when the linear model is true). The difference between these predicted values and the ones used to fit the model are called "residuals" which, when replicating the data collection process, have properties of random variables with 0 means.

  The observed residuals are then used to subsequently estimate the variability in these values and to estimate the sampling distribution of the parameters. When the residual standard error is exactly 0 then the model fits the data perfectly (likely due to overfitting). If the residual standard error can not be shown to be significantly different from the variability in the unconditional response, then there is little evidence to suggest the linear model has any predictive ability.
### strwrap()
  used for print \n in some strings.
### AIC
  AIC is short for Akaike information criterion, "赤池信息量准则" in Chinese.
  是评估统计模型的复杂度和衡量统计模型吾五雾拟合吾五雾资料之优良性（英语：Goodness of Fit，白话：合身的程度）的一种标准，是由日本统计学家赤池弘次创立和发展的。赤池信息量准则建立在信息熵的概念基础上。
  增加自由参数的数目提高了拟合的优良性，AIC鼓励数据拟合的优良性但是尽量避免出现过度拟合（Overfitting）的情况。
  所以优先考虑的模型应是AIC值最小的那一个。赤池信息量准则的方法是寻找可以最好地解释数据但包含最少自由参数的模型。
  Reference: http://zh.wikipedia.org/wiki/%E8%B5%A4%E6%B1%A0%E4%BF%A1%E6%81%AF%E9%87%8F%E5%87%86%E5%88%99


