##save.image("C:\\Documents and Settings\\xwang\\My Documents\\.RData")
ls()
## rm(list=ls())

seq(1,100,by =1)
for( i in seq(1,100,by=10)) {print (i*i)}
plot(c(1,2,3,4,5),c(1,4,9,16,25),xlab="num",ylab="num square")
plot(c(1,2,3,4,5),c(1,4,9,16,25),xlab="num",ylab="num square",type="b")
plot(c(1,2,3,4,5),c(1,4,9,16,25),xlab="num",ylab="num square",type="c")

myfunction <- function(x) {return(20+(x*x)) }
myfunction(2)
myfunction
log10
library("seqinr")
install.packages("seqinr")
pwd()
library("seqinr")
lambda <- read.fasta(file = "sequence.fasta")
lambda
head(lambda)
str(lambda)
lambdaseq <- lambda[[1]]
lambdaseq[452:535]

head(lambdaseq)
table(lambdaseq)
sum(table(lambdaseq))
gc(lambdaseq)
GC(lambdaseq)

starts
length(lambdaseq)

sliding_window_plot <- function(window_size,inputseq)
{
    starts <- seq(1,length(inputseq)-window_size,by = window_size)
    n <- length(starts)
    chunkGCs <- numeric(n)
    chunkGCs
    for (i in 1:n) {
    	chunk <- inputseq[starts[i]:(starts[i]+window_size-1)]
    	chunkGC <- GC(chunk)
    	#print (chunkGC)
    	chunkGCs[i] <- chunkGC
    }
    plot(starts,chunkGCs,type="b",xlab="Nucleotide start position",ylab="GC content")
}
sliding_window_plot(1000,lambdaseq)

