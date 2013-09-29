# terms during my studying of R
 # author: xyw
---------------------------
## terms

### Univariate Outlier Detection
  ����������Ⱥֵ���. You can use boxplot.stats function in R.  

### Heirarchical Agglomerative
�ֲ���ࡣ(update:2013.09.27)  
Ward Heirarchical Clustering is a kind of them.  

### data transfer
����ת�� (update:2013.09.25)  
��1���޳�ĳЩ���֡����⡱�Ĺ۲�ֵ��������ظ���  
��2�����ܵ��������ķ������Ϊ������Ϊͬ�ʵ��������ķ��  
��3��������ݵ���Ҫȱ�ݣ�������Ӧ�ı���ת����Ȼ����ת��������������������  
���õ�ת�������У�
#### ƽ����ת��(square root transformation)
	�������ƽ�������䷽���б�����ϵ����poisson�ֲ������� ������������ƽ����ת������Ч�ġ�

#### ����ת��(logarithmic transformation)
	������ݱ��ֵ�ЧӦΪ�ǿɼ��ԣ����ɱ����Ի�ɳ��ԣ�ͬʱ����ƽ�������伫����׼��ɱ�����ϵ������ö���ת�����ɻ��һ��ͬ�ʵķ��
	һ�㽫yת��Ϊlg y����۲�ֵ�����������ֵ�Բ�����10�������1g(y+1)ת����

#### ������ת��(arcsine transformation)
     �������ϵ������ٷ�����������������ֲ�������֪��һ�ֲ��ķ����Ǿ�������ƽ����p�ġ����ԣ������������p��0.3��p��0.7������������ת�����Ի��һ���Ƚ�һ�µķ��
	������ת���ǽ��ٷ�����ƽ����ֵȡ������ֵ������pת���� ���Ӷ���Ϊ�Ƕȡ�

### residuals standard error
  A fitted regression model uses the parameters to generate point estimate predictions which are the means of observed responses if you were to replicate the study with the same X values an infinite number of times (and when the linear model is true). The difference between these predicted values and the ones used to fit the model are called "residuals" which, when replicating the data collection process, have properties of random variables with 0 means.

  The observed residuals are then used to subsequently estimate the variability in these values and to estimate the sampling distribution of the parameters. When the residual standard error is exactly 0 then the model fits the data perfectly (likely due to overfitting). If the residual standard error can not be shown to be significantly different from the variability in the unconditional response, then there is little evidence to suggest the linear model has any predictive ability.
### strwrap()
  used for print \n in some strings.
### AIC
  AIC is short for Akaike information criterion, "�����Ϣ��׼��" in Chinese.
  ������ͳ��ģ�͵ĸ��ӶȺͺ���ͳ��ģ���������������������֮�����ԣ�Ӣ�Goodness of Fit���׻�������ĳ̶ȣ���һ�ֱ�׼�������ձ�ͳ��ѧ�ҳ�غ�δ����ͷ�չ�ġ������Ϣ��׼��������Ϣ�صĸ�������ϡ�
  �������ɲ�������Ŀ�������ϵ������ԣ�AIC����������ϵ������Ե��Ǿ���������ֹ�����ϣ�Overfitting���������
  �������ȿ��ǵ�ģ��Ӧ��AICֵ��С����һ���������Ϣ��׼��ķ�����Ѱ�ҿ�����õؽ������ݵ������������ɲ�����ģ�͡�
  Reference: http://zh.wikipedia.org/wiki/%E8%B5%A4%E6%B1%A0%E4%BF%A1%E6%81%AF%E9%87%8F%E5%87%86%E5%88%99

