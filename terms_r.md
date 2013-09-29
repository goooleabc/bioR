# terms during my studying of R
 # author: xyw
---------------------------
## terms

### Univariate Outlier Detection
  单变量的离群值检测. You can use boxplot.stats function in R.  

### Heirarchical Agglomerative
分层聚类。(update:2013.09.27)  
Ward Heirarchical Clustering is a kind of them.  

### data transfer
数据转换 (update:2013.09.25)  
（1）剔除某些表现“特殊”的观察值、处理或重复。  
（2）将总的试验误差的方差分裂为几个较为同质的试验误差的方差。  
（3）针对数据的主要缺陷，采用相应的变数转换；然后用转换后的数据作方差分析。  
常用的转换方法有：
#### 平方根转换(square root transformation)
	如果样本平均数与其方差有比例关系，如poisson分布那样， ，这种资料用平方根转换是有效的。

#### 对数转换(logarithmic transformation)
	如果数据表现的效应为非可加性，而成倍加性或可乘性，同时样本平均数与其极差或标准差成比例关系，则采用对数转换，可获得一个同质的方差。
	一般将y转换为lg y；如观察值中有零而各数值皆不大于10，则可用1g(y+1)转换。

#### 反正弦转换(arcsine transformation)
     如果资料系成数或百分数，则它将作二项分布，而已知这一分布的方差是决定于其平均数p的。所以，在理论上如果p＜0.3和p＞0.7皆需作反正弦转换，以获得一个比较一致的方差。
	反正弦转换是将百分数的平方根值取反正弦值，即将p转换成 ，从而成为角度。

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

