
## @knitr echo=FALSE,results='hide'
library(xlsx)
library(reshape2)
dat <- read.xlsx('Pharmerit.xlsx',sheetName=1)
names(dat)[-1] <- paste('V',1:(ncol(dat)-1),sep='')
dat11 <- t(dat)
dat2 <- melt(dat, id='Name',na.rm=T)


## @knitr echo=FALSE, results='markdown'
require(pander)
pander(head(dat))



## @knitr echo=FALSE
require(ggplot2)
t1 <- table(dat2$value)
print(qplot(names(t1), unclass(t1),geom='bar', stat='identity')+labs(x='Subjects',y='Frequency')+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1)))



## @knitr echo=FALSE
distance=matrix(0,nrow(dat),nrow(dat))
for(i in 1:(nrow(dat)-1)){
  for (j in 2:nrow(dat)){
    d <- intersect(dat11[-1,i],dat11[-1,j])
    distance[i,j]  <-  distance[j,i] <- 4-length(d[!is.na(d)])
  }
}
colnames(distance) <- row.names(distance) <- as.character(dat$Name
                                                          )
plot(hclust(as.dist(distance)))


