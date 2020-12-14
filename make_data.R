library(data.table)
dat <- fread("raw_v1.tsv")
set.seed(200)
s <- seq(nrow(dat))
id1 <- sample(s,9000)
id2 <- sample(s[!s%in%id1],500)
id3 <- sample(s[!s%in%c(id1,id2)],2000)
train <- dat[id1,]
dev <- dat[id2,]
test <- dat[id3,]
write.table(train,file='train.tsv',quote=FALSE,sep='\t',col.names=FALSE,row.names=FALSE)
write.table(test,file='test.tsv',quote=FALSE,sep='\t',col.names=FALSE,row.names=FALSE)
write.table(dev,file='dev.tsv',quote=FALSE,sep='\t',col.names=FALSE,row.names=FALSE)