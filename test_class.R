## Package ¡®class¡¯
#	August 29, 2013
train1 <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test1 <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
keep <- condense(train1, cl)
knn(train1[keep, , drop=FALSE], test1, cl[keep])
keep2 <- reduce.nn(train1, keep, cl)
keep2
knn(train1[keep2, , drop=FALSE], test1, cl[keep2])

## knn 
knn(train1, test1, cl, k = 5, prob=TRUE)
attributes(.Last.value)
## knn.cv
knn.cv(train1, cl, k = 3, prob = TRUE)

