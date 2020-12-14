transfer <- function(ab1,ab2)
{
   tmp1 <- strsplit(ab1, " |(?>\\@.*?\\$.*?\\K( |$))", perl = TRUE)
   tmp3 <- tmp2 <- strsplit(ab2, " |(?>\\@.*?\\$.*?\\K( |$))", perl = TRUE)
   tmp3 <- lapply(tmp3,gsub,pattern='\\@(.*)\\$',replacement='\\1')
   id <- lapply(tmp1,grep,pattern="@Disease$",fix=TRUE)
   y1 <- list()
   y2 <- list()
   k <- 1
   for(i in seq(id))
   {
      idi <- id[[i]]
      if(length(idi)>1) 
      {
         for(j in idi)
         {
            yi1 <- tmp1[[i]]
            yi1[j] <- tmp3[[i]][j]
            yi2 <- tmp2[[i]]
            yi2[j] <- tmp3[[i]][j]
            y1[[k]] <- yi1
            y2[[k]] <- yi2
            k <- k+1 
         }
       }
      else
      {
         y1[[k]] <- tmp1[[i]]
         y2[[k]] <- tmp2[[i]]
         k <- k+1
      } 
   }
   y1 <- sapply(y1,paste0,collapse=" ")
   y2 <- sapply(y2,paste0,collapse=" ")
   list(y1,y2)
}







library(httr)
library(RCurl)
library(pubMR)
library(data.table)
library(tidyr)
library(dplyr)
m <- '"Aspirin/adverse effects"[MAJR] AND ("2001/09/01"[PDAT] : "2020/09/01"[PDAT])'
obj <- txtList(input=m)


obj1=data.table(PMID=obj@PMID,MAJR=obj@MAJR)
MAJR <- obj1[,MAJR]
idx <- sapply(MAJR,is.null)
obj1 <- obj1[!idx,]
obj1 = obj1 %>% unnest(MAJR) 
V <- table(obj1[,c("MAJR","PMID")])
rns <- rownames(V)
id1 <- grep("Aspirin/adverse effects",rns,fixed=TRUE)
id <- id1
V <- V[id,]
V1 <- V[V>0]
pmid<-names(V1)






dealab<- function(url2)
{
   myOpts <- curlOptions(connecttimeout=31)
   y <- getURL(url2,.opts = myOpts)
   y1 <- read.csv(textConnection(y), header=F,sep="\t",stringsAsFactors=FALSE)
   y1 <- as.data.table(y1)
   if(ncol(y1)==6)
   {
      idt <- apply(y1,1,function(x) any(grep("\\|t\\|",x)))
      ida <- apply(y1,1,function(x) any(grep("\\|a\\|",x)))
      ti <- y1[idt,V1]
      ti <- strsplit(ti,"\\|")[[1L]][3L]
      ab <- y1[ida,V1]
      ab <- strsplit(ab,"\\|")[[1L]][3L]
      ab <- ifelse(is.na(ab),ti,paste0(ti," ",ab))
      labels <- y1[!is.na(V2),.(V4,V5)] %>% unique
      id <- c("Chemical","Disease")
      labels <- labels[V5%in%id,]
      library(tokenizers)
      ab1 <- ab2 <- tokenize_sentences(ab)[[1L]]
   }
   else
   {
      ab1 <- NA
      ab2<-NA
   }
   if(nrow(labels)>0)
   {
      for(i in seq(nrow(labels)))
      {
         ab1 <- gsub(labels[,V4][i],paste0( "@",labels[,V5][i],"$" ),ab1)
         ab2<-gsub(labels[,V4][i],paste0( "@",labels[,V4][i],"$" ),ab2)
      }
      y <- transfer(ab1,ab2)
      ab1 <- y[[1]]
      ab2 <- y[[2]]
   }
   list(ab1,ab2)
}






y <- list()
url <- 'https://www.ncbi.nlm.nih.gov/research/pubtator-api/publications/export/pubtator?pmids=' 
url1 <- paste0(url,pmid)

for(i in 1:length(pmid))
#for(i in 1:20)
{
   url1i<-URLencode(url1[i])
   yi <- try(dealab(url1i),silent=TRUE)
   if(class(yi)=="try-error")
      y[[i]] <- NULL
   else
   {
      y[[i]]<-data.table(mark_sentence=yi[[1]],sentence=yi[[2]],pmid=pmid[i])
   }
   cat("iteration = ", i, "\n")
}
y <- rbindlist(y)
y[,label:=2]
id <- grepl("\\@Chemical\\$",y[,mark_sentence])&grepl("\\@Disease\\$",y[,mark_sentence])
y <- y[id,]
write.table(y,file='train.tsv',quote=FALSE,sep='\t',col.names=FALSE,row.names=FALSE)




