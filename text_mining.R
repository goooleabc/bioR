## setwd("F:\work\xwang_programs\textmining")

csv <- read.csv("F:\\work\\xwang_programs\\textmining\\train.csv",header=T, stringsAsFactors=F)
mystopwords <- unlist(read.table("F:\\work\\xwang_programs\\textmining\\StopWords.txt",stringsAsFactors=F))

#����Ԥ����
#�ж�tm���Ƿ���ڣ���������װ
tm_pkg <- length( grep("tm",library()) ) >0;
if ( !tm_pkg ) {
	install.packages("tm");
	library(tm);
}
removeNumbers = function(x){ ret = gsub("[0-9 0 1 2 3 4 5 6 7 8 9]","",x) }

#���ķִʣ�Ҳ���Կ���ʹ��rmmseg4j, rsmartcn
tm_pkg <- length( grep("Rwordseg",library()) ) >0;
if ( !tm_pkg ) {
	install.packages("Rwordseg", repos = "http://jliblog.com/cran")
	library(Rwordseg);
}
wordsegment <- function(x){
	segmentCN(x)
}

#ȥ��ֹͣ�ʣ�Ч���Ƚϲ���Խ�һ������
removeStopWords=function(x,words){
	ret = character(0)
	index <- 1
	it_max <- length(x)
	while(index <= it_max){
		if(length(words[words==x[index]])<1) ret <- c(ret,x[index])
		index <- index+1
	}
	ret
}

sample.words <- lapply(csv$text,removeNumbers)
sample.words <- lapply(sample.words,wordsegment)
#�ȴ������ķִʣ��ٴ���stopwords,��ֹȫ���滻��ʧ��Ϣ
sample.words <- lapply(sample.words,removeStopWords,mystopwords)

#�������Ͽ� 
corpus = Corpus(VectorSource(sample.words)) 
meta(corpus,"cluster") <- csv$type 
unique_type <- unique(csv$type) 
#�����ĵ�-�������� 
(sample.dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf)))) 

###3 wordcloudչʾ 
# install.packages("wordcloud") install.packages("RColorBrewer")
library(wordcloud) 
#install.packages("rJava")

 
#��ͬ�ĵ�wordcloud�Ա�ͼ 
sample.tdm <-   TermDocumentMatrix(corpus, control = list(wordLengths = c(2, Inf))) 
tdm_matrix <- as.matrix(sample.tdm) 
 
png(paste("sample_comparison",".png", sep = ""), width = 1500, height = 1500 ) 
comparison.cloud(tdm_matrix) 
title(main = "sample comparision") 
dev.off() 

#��������� wordcloud�Ա�ͼ 
n <- nrow(csv) 
zz1 = 1:n 
cluster_matrix<-sapply(unique_type,function(type){apply(tdm_matrix[,zz1[csv$type==type]],1,sum)}) 
png(paste("sample_ cluster_comparison",".png", sep = ""), width = 800, height = 800 ) 
comparison.cloud(cluster_matrix) 
title(main = "sample cluster comparision") 
dev.off() 

#�������໭ wordcloud 
sample.cloud <- function(cluster , maxwords = 100) { 
     words <- sample.words[which(csv$type==cluster)] 
     allwords <- unlist(words) 
 
     wordsfreq <- sort(table(allwords), decreasing = T) 
     wordsname <- names(wordsfreq)  
 
     png(paste("sample_", cluster , ".png", sep = ""), width = 600, height = 600 ) 
     wordcloud(wordsname, wordsfreq,  scale = c(6, 1.5), min.freq = 2, max.words = maxwords,colors = rainbow(100)) 
     title(main = paste("cluster:", cluster)) 
     dev.off() 
} 
lapply(unique_type,sample.cloud)# unique(csv$type) 