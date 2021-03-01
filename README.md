

ADENLP
==========
ADENLP is a general framework that can conveniently and practically extract adverse drug reactions from literature database,it can generate  chemical-disease pairs (CDPs) based on embedding text mining and transfer learning for discovering drug-side-effect association.Any potential  drug side pair in a certain research field can be automatically extracted from PubMed database by our developed tool; extracted CDPs can be used for the downstream steps: training the transfer learning models.

## Authors



[周晓北] (Zhou Xiaobei)  
[周淼]  (Zhou Miao)

## Download
Our trained model and data can be downloaded from
```
 https://pan.baidu.com/s/18WLx7DzuPJL-xKMuqaKB9g
```
The extraction password is 6sc7

## Extracting 	CDPs
Our extraction tool base on R packages—“pubMR”, which can install from https://github.com/xizhou/pubMR.git


###  Retrieve literature from PubMed
```r
library(httr)
library(RCurl)
library(pubMR)
library(data.table)
library(tidyr)
library(dplyr)
m <- '"Aspirin/adverse effects"[MAJR] AND ("2001/09/01"[PDAT] : "2020/09/01"[PDAT])'
obj <- txtList(input=m)
```

```text
obj
An object of class "txtList" containing 2089 articles
with slot names: PMID,TI,AB,JT,DP,ISSN,MH,SH,MAJR,AU.
```

**Extracting PMID numbers of paper that including drug side effects:** 

```r
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
```
the results llike this:
```text
  [1] "11496056" "11547294" "11547756" "11551257" "11562331" "11578715"
   [7] "11601671" "11602930" "11678862" "11678863" "11703219" "11720651"
  [13] "11728532" "11735194" "11736865" "11752356" "11752364" "11754803"
  [19] "11760687" "11761886" "11770244" "11779723" "11781303" "11782997"
  [25] "11792017" "11796443" "11799026" "11809245" "11816246" "11817775"
  [31] "11827359" "11827360" "11829001" "11837557" "11853821" "11865848"
  [37] "11870343" "11892091" "11892656" "11895461" "11897936" "11929396"
  [43] "11941383" "11943268" "11954880" "11960062" "11966501" "11994026"
  [49] "11995251" "12013917" "12015402" "12015940" "12017163" "12028114"
  [55] "12030579" "12032100" "12035038" "12036228" "12053594" "12063521"
  [61] "12065343" "12072100" "12072809" "12081628" "12087138" "12092526"
  [67] "12093268" "12096195" "12096197" "12100294" "12100305" "12106925"
  [73] "12107984" "12108311" "12133028" "12136912" "12138601" "12141814"
  [79] "12144557" "12153961" "12166347" "12167070" "12195225" "12196067"
  [85] "12209526" "12215946" "12243613" "12372129" "12372130" "12373279"
  [91] "12373280" "12377879" "12390073" "12390104" "12397524" "12404219"
  [97] "12404234" "12409970" "12411346" "12421891" "12421897" "12428645"
 [103] "12432054" "12452205" "12464047" "12468951" "12473569" "12487218"
 [109] "12492214" "12501074" "12512737" "12532202" "12564293" "12569976"
 [115] "12576709" "12578941" "12589085" "12592057" "12602340" "12602712"
 [121] "12612918" "12618872" "12619198" "12619199" "12622742" "12622751"
 [127] "12624808" "12625225" "12641504" "12651635" "12653694" "12657138"
 [133] "12666521" "12668892" "12668893" "12668894" "12668895" "12668896"
 [139] "12668897" "12669898" "12700329" "12700498" "12704352" "12706946"
 [145] "12714200" "12716444" "12721395" "12723744" "12725142" "12743549"
 [151] "12743569" "12743579" "12767563" "12774247" "12774967" "12780835"
 [157] "12796206" "12800908" "12802356" "12816731" "12834467" "12843912"
 [163] "12843913" "12848632" "12849830" "12852485" "12860580" "12861847"
 [169] "12865603" "12866040" "12876526" "12880537" "12883841" "12889553"
 [175] "12892035" "12897885" "12904140" "12908375" "12910426" "12919986"
 [181] "12923581" "12923584" "12934247" "12936891" "13679808" "14503120"
 [187] "14510727" "14511062" "14519044" "14519047" "14524272" "14532740"
 [193] "14555544" "14555553" "14561202" "14563503" "14567600" "14568792"
...
```
 
**Grab and split the abstract from PubTator:** 
- A  drug side effects sentence set can be generated by:
```r
y <- list()
url <- 'https://www.ncbi.nlm.nih.gov/research/pubtator-api/publications/export/pubtator?pmids=' 
url1 <- paste0(url,pmid)

for(i in 1:length(pmid))
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
```
Labeling the sentence and name "raw_v1":

![Image text](https://raw.githubusercontent.com/Miao-zhou/ADENLP/main/raw%20label.png)



## Training fine-tuning BioBERT model
We label the drug side effect of aspirin sentence set,and randomly divide it into "train","test" and "dev" data set files for training fine-tuning BioBERT model.

### Requriements
use requirements.txt to install the model as follows:
```
$ git clone https://github.com/Miao-zhou/ADENLP.git
$ pip3 install -r requirement.txt 
```
### Training
### BioBERT(Aspirin) (Predicting aspirin)
We train the model by using aspirin "train" dataset and predict the aspirin "test" dataset:

Let $RE_DIR indicate a folder for a single dataset  which contains  train.tsv, dev.tsv and test.tsv, $TASK_NAME denote the name of task (euadr), and $OUTPUT_DIR denote a directory for outputs:
```
$ export BIOBERT_DIR=./biobert_v1.1_pubmed
$ export RE_DIR=./asprin_initial
$ export TASK_NAME=euadr
$ export OUTPUT_DIR=./initial_asprinoutput
$ rm -rf $OUTPUT_DIR
```

Following command runs fine-tuning code：

```
$ python3 run_re.py --task_name=$TASK_NAME --do_train=true --do_eval=true --do_predict=true --vocab_file=$BIOBERT_DIR/vocab.txt --bert_config_file=$BIOBERT_DIR/bert_config.json --init_checkpoint=$BIOBERT_DIR/model.ckpt-1000000 --max_seq_length=64 --train_batch_size=32 --learning_rate=1e-5 --num_train_epochs=20.0 --do_lower_case=true --data_dir=$RE_DIR --output_dir=$OUTPUT_DIR
```

You can get the value of F1 score from the following code:

```
$ python3 ./biocodes/re_eval.py --output_path=$OUTPUT_DIR/test_results.tsv --answer_path=$RE_DIR/test.tsv
```

```text
F1 score: 94.85%
Recall: 93.68%
Precision: 96.04%
```


### BioBERT(Aspirin) (Predicting ADE)
 We use our model to predicting ADE sentence dataset.

```
$ export CHECKPOINT=./initial_asprinoutput/model.ckpt-6250
$ export BIOBERT_DIR=./biobert_v1.1_pubmed
$ export RE_DIR=./ade_paper
$ export TASK_NAME=euadr
$ export OUTPUT_DIR=./ade_aspoutput
rm -rf $OUTPUT_DIR
```
Following command runs predicting code：

```
$ python3 run_re.py --task_name=$TASK_NAME --do_train=false --do_eval=false --do_predict=true --vocab_file=$BIOBERT_DIR/vocab.txt --bert_config_file=$BIOBERT_DIR/bert_config.json --init_checkpoint=$CHECKPOINT --max_seq_length=64 --train_batch_size=32 --learning_rate=1e-5 --num_train_epochs=20.0 --do_lower_case=true --data_dir=$RE_DIR --output_dir=$OUTPUT_DIR
```
You can get F1 score from following code:

```
$ python3 ./biocodes/re_eval.py --output_path=$OUTPUT_DIR/test_results.tsv --answer_path=$RE_DIR/test.tsv
```
```text
F1 score: 83.26%
Recall: 71.36%
Precision: 99.92%
```


## Visualization
### Omalizumab
We take the drug "Omalizumab" as an example,we extract sentence of omalizumab from pubmed and predicting its adverse effect.

Predicting code is following:

```
$ export CHECKPOINT=./initial_asprinoutput/model.ckpt-6250
$ export BIOBERT_DIR=./biobert_v1.1_pubmed
$ export RE_DIR=./omalizumab
$ export TASK_NAME=euadr
$ export OUTPUT_DIR=./omalizumaboutput
$ rm -rf $OUTPUT_DIR
$ python3 run_re.py --task_name=$TASK_NAME --do_train=false --do_eval=false --do_predict=true --vocab_file=$BIOBERT_DIR/vocab.txt --bert_config_file=$BIOBERT_DIR/bert_config.json --init_checkpoint=$CHECKPOINT --max_seq_length=64 --train_batch_size=32 --learning_rate=1e-5 --num_train_epochs=20.0 --do_lower_case=true --data_dir=$RE_DIR --output_dir=$OUTPUT_DIR
$ python3 ./biocodes/re_eval.py --output_path=$OUTPUT_DIR/test_results.tsv --answer_path=$RE_DIR/test.tsv
```
We can get the result like this:
![Image text](https://raw.githubusercontent.com/Miao-zhou/ADENLP/main/omalizumab.png)

**Visualing the result by Grakn**

![Image text](https://raw.githubusercontent.com/Miao-zhou/ADENLP/main/omalizumab%20grakn.png)

### Metformin
We take the all-purpose drug Metformin as an example,we extract sentence of Metformin from pubmed and predicting its adverse effect. Afterwards, our predicted results were compared to the SIDER database of Metformin adverse reactions.
Predicting code is following:
```
$ export CHECKPOINT=./initial_asprinoutput/model.ckpt-6250
$ export BIOBERT_DIR=./biobert_v1.1_pubmed
$ export RE_DIR=./metformin
$ export TASK_NAME=euadr
$ export OUTPUT_DIR=./metforminoutput
$ rm -rf $OUTPUT_DIR
$ python3 run_re.py --task_name=$TASK_NAME --do_train=false --do_eval=false --do_predict=true --vocab_file=$BIOBERT_DIR/vocab.txt --bert_config_file=$BIOBERT_DIR/bert_config.json --init_checkpoint=$CHECKPOINT --max_seq_length=64 --train_batch_size=32 --learning_rate=1e-5 --num_train_epochs=20.0 --do_lower_case=true --data_dir=$RE_DIR --output_dir=$OUTPUT_DIR
$ python3 ./biocodes/re_eval.py --output_path=$OUTPUT_DIR/test_results.tsv --answer_path=$RE_DIR/test.tsv
```

We can get the result like this:
![Image text](https://raw.githubusercontent.com/Miao-zhou/ADENLP/main/metformin.png)


The results of the comparison of adverse reactions with metformin in the SIDER database are as follows:
![Image text](https://raw.githubusercontent.com/Miao-zhou/ADENLP/main/metformin-venn.png)

![Image text](https://raw.githubusercontent.com/Miao-zhou/ADENLP/main/figure4.png)


