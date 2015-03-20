# Package preload ####
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr","ggplot2","tm", "tmcn", "Rwordseg", "topicmodels")
ipak(packages)

Sys.setlocale(locale = "Chinese")

#####################RMRB################################
##Meta data ####
file.name <- list.files(path = "H:/Documents/corpus/rmrb_corpus/monthly", full.names = F, recursive = TRUE)

date <- NULL

for(num in seq(file.name)){
  date <- c(date, as.numeric(gsub("\\D+", "", file.name[num])))
}


metadata <- data.frame(date = date, period = "")

metadata$period <- 5

metadata$period[metadata$date <= 199012] <- 4
metadata$period[metadata$date <= 197712] <- 3
metadata$period[metadata$date <= 196512] <- 2
metadata$period[metadata$date <= 194912] <- 1

##Corpus data ####

d.corpus <- Corpus(DirSource("H:/Documents/data/rmrb_corpus/monthly/4649", encoding = "UTF-8"), list(language = NA))

## 清除標點符號, 數字，和常规用法
d.corpus <- tm_map(d.corpus, function(word){
  gsub("本市消息", "", word)
  gsub("专栏", "", word)
  gsub("（附图片）", "", word)
  gsub("（新华社发）", "", word)
  gsub("（新华社.*?）", "", word)
  gsub("（.*?记者.*?）", "", word)
})

d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

## Segment and selection
d.corpus <- tm_map(d.corpus, segmentCN, nature = T)

#4.1 摘取具有名词性质的词汇
d.corpus <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) %in% c("an", "b", "i", "j", "l", "Ng", "n", "nt", "nz", "s", "vn", "z")] 
  }) 
})

d.corpus <- Corpus(VectorSource(d.corpus))

myStopWords <- c(stopwordsCN(), "专栏", "新华社", "本报", "本报消息", "本报讯", "本市讯", "记者", "报道" )
#d.corpus <- tm_map(d.corpus, removeWords, myStopWords)

d.corpus <- tm_map(d.corpus, content_transformer(function(note){  
  #在VectorSource之后就要用content_transfer来保证corpus结构不变
  note <- gsub("[A-Za-z0-9]", "", note)
}))

#6. 转化成 DTM
corpus <- DocumentTermMatrix(d.corpus, control = list(stopwords = myStopWords, wordLengths = c(2, Inf), bounds = list(global = c(2,Inf)), removePunctuation = T, removeNumbers = T)) 
#去除停止词 + 限制词长度至少为2 + 词频至少出现过2两次+ 去除标点 + 去除数字

inspect(corpus[1:10, 1:30]) # detect result


####################Selections###########################

#Mao ####
#分卷####
mao <- readLines("E:/Dropbox_sync/Method/Data/corpus/Selection corpus/毛泽东选集.txt")

start.vol1 <- grep("中国社会各阶级的分析", mao)[1]
end.vol1 <- grep("反对日本进攻的方针、办法和前途", mao)[1]-1

start.vol2 <- grep("反对日本进攻的方针、办法和前途", mao)[1]
end.vol2 <- grep("《农村调查》的序言和跋", mao)[1]-1

start.vol3 <- grep("《农村调查》的序言和跋", mao)[1]
end.vol3 <- grep("抗日战争胜利后的时局和我们的方针", mao)[1]-1

start.vol4 <- grep("抗日战争胜利后的时局和我们的方针", mao)[1]
end.vol4 <- grep("中国人民站起来了", mao)[1]-1

#start.vol5 <- grep("中国人民站起来了", mao)[1]
#end.vol5 <- length(c(mao,"第五卷终")) #全版第五卷在一个独立文档

vol5 <- readLines("E:/Dropbox_sync/Method/Data/corpus/Selection corpus/mao/maoV.txt")
vol5 <- gsub("（《毛泽东选集》第.*页） ","", mao)


vol1 <- mao[start.vol1:end.vol1]
vol2 <- mao[start.vol2:end.vol2]
vol3 <- mao[start.vol3:end.vol3]
vol4 <- mao[start.vol4:end.vol4]
#vol5 <- mao[start.vol5:end.vol5]

writeLines(vol1, con="E:/Dropbox_sync/Method/Data/corpus/Selection corpus/mao/maoI.txt")
writeLines(vol2, con="E:/Dropbox_sync/Method/Data/corpus/Selection corpus/mao/maoII.txt")
writeLines(vol3, con="E:/Dropbox_sync/Method/Data/corpus/Selection corpus/mao/maoIII.txt")
writeLines(vol4, con="E:/Dropbox_sync/Method/Data/corpus/Selection corpus/mao/maoIV.txt")
writeLines(vol5, con="E:/Dropbox_sync/Method/Data/corpus/Selection corpus/mao/maoV.txt")

#Clean####
#1. Input the data
d.corpus <- Corpus(DirSource("E:/Dropbox_sync/Method/Data/corpus/Selection corpus/mao/"), list(language = NA))

#2. 清除注释： 
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(note){
  note <- gsub("注释", "", note)
  note <- gsub("〔〕|〔\\d{1,2}〕", "", note)
  note <- gsub("[A-Za-z0-9]", "", note)
})

#4. 分词
d.corpus <- tm_map(d.corpus, segmentCN, nature = T)

#4.1 摘取具有名词性质的词汇
d.corpus <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) %in% c("an", "b", "i", "j", "l", "Ng", "n", "nt", "nz", "s", "vn", "z")] 
  }) 
  ## unlist(noun) #don't do unlist, the way in http://cos.name/cn/topic/158164/#post-408754 中ricklovelisa方法
})

#5. Convert to VectorSource, 唯一可以转化为DTM的格式
d.corpus <- Corpus(VectorSource(d.corpus)) 

#5.1 添加停止词
myStopWords <- c(stopwordsCN(), 
                 "毛泽东", "同志", "参见",  "参看", "本书", "卷", "注", "见", "第一", "第一卷", "第二卷", "第三卷", "第四卷", "第五卷")
#5.2 去除"list" 和“c”——这一步至关重要！！！
d.corpus <- tm_map(d.corpus, content_transformer(function(note){  
  #在VectorSource之后就要用content_transfer来保证corpus结构不变
  note <- gsub("[A-Za-z0-9]", "", note)
}))

#6. 转化成 DTM
corpus <- DocumentTermMatrix(d.corpus, control = list(stopwords = myStopWords, wordLengths = c(2, Inf), bounds = list(global = c(2,Inf)), removePunctuation = T, removeNumbers = T)) 
#去除停止词 + 限制词长度至少为2 + 词频至少出现过2两次+ 去除标点 + 去除数字

inspect(corpus[1:5, 1:30]) # detect result



#Deng ####
##分卷####
deng <- readLines("E:/Dropbox_sync/Method/Data/corpus/Selection corpus/邓小平文选.txt", encoding = "UTF-8")

volumn <- grep("^第.卷", deng) #这里需要行的定位，不需要token
deng <- c(deng, "全文终")
last <- length(deng)
volumn <- c(volumn, last)
deng <- gsub("\\s+钱建文E书制作\\s+", "", deng) # 去除 "钱建文E书制作"
vnum <- c("I", "II", "III")
for(i in 1:length(volumn)){
  if(i != length(volumn)){
    volumn.title <- grep("第.卷", deng[volumn[i]], value = T)
    start <- volumn[i]+1
    end <- volumn[i+1]-1
    volumn.lines <- deng[start:end]
    eval(parse(text = paste0("writeLines(volumn.lines, con='deng", vnum[i] ,".txt')"))) #把原文分为三卷
  }
}

#clean####
d.corpus <- Corpus(DirSource("E:/Dropbox_sync/Method/Data/corpus/Selection corpus/deng/"), list(language = NA))

## 清除標點符號, 數字

d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})


##分词和选词

d.corpus <- tm_map(d.corpus,segmentCN, nature = T) 


d.corpus <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) %in% c("an", "b", "i", "j", "l", "Ng", "n", "nt", "nz", "s", "vn", "z")] 
  }) 
})

##转化Matrix
d.corpus <- Corpus(VectorSource(d.corpus))

myStopWords <- c(stopwordsCN(), "邓小平", "同志", "参见",  "参看", "本书", "卷", "注", "见", "第一", "第一卷", "第二卷", "第三卷")
#d.corpus <- tm_map(d.corpus, removeWords, myStopWords)

d.corpus <- tm_map(d.corpus, content_transformer(function(note){
  note <- gsub("[A-Za-z0-9]", "", note)
}))

corpus <- DocumentTermMatrix(d.corpus, control = list(stopwords = myStopWords, wordLengths = c(2, Inf), bounds = list(global = c(2,Inf)), removePunctuation = T, removeNumbers = T)) 

inspect(corpus[1:3, 1:20]) # detect result



#Jiang ####
# 1. 在原文件中找到注释标记“” 和“”， 然后替换为空格
# 2. Input the data
d.corpus <- Corpus(DirSource("E:/Dropbox_sync/Method/Data/corpus/Selection corpus/jiang/"), list(language = NA))

#3. 清除注释： 对于江泽民文选无法彻底删除注释，因为注释符号也都镶嵌在文章中。
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(note){
  note <- gsub("注\\s+释", "", note)
  note <- gsub("〔〕|〔\\d{1,2}〕", "", note)
  note <- gsub("[A-Za-z0-9]", "", note)
})

#4. 分词
d.corpus <- tm_map(d.corpus, segmentCN, nature = T)

#4.1 摘取具有名词性质的词汇
d.corpus <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) %in% c("an", "b", "i", "j", "l", "Ng", "n", "nt", "nz", "s", "vn", "z")] 
  }) 
## unlist(noun) #don't do unlist, the way in http://cos.name/cn/topic/158164/#post-408754 中ricklovelisa方法
})

#5. Convert to VectorSource, 唯一可以转化为DTM的格式
d.corpus <- Corpus(VectorSource(d.corpus)) 

#5.1 添加停止词
myStopWords <- c(stopwordsCN(), "江泽民", "同志", "参见",  "参看", "本书", "卷", "注", "见", "第一", "第一卷", "第二卷", "第三卷")

#5.2 去除"list" 和“c”——这一步至关重要！！！
d.corpus <- tm_map(d.corpus, content_transformer(function(note){  
  #在VectorSource之后就要用content_transfer来保证corpus结构不变
  note <- gsub("[A-Za-z0-9]", "", note)
}))



#6. 转化成 DTM
corpus.jiang <- DocumentTermMatrix(d.corpus, control = list(stopwords = myStopWords, wordLengths = c(2, Inf), bounds = list(global = c(2,Inf)), removePunctuation = T, removeNumbers = T)) 

#去除停止词 + 限制词长度至少为2 + 词频至少出现过2两次+ 去除标点 + 去除数字

inspect(corpus[1:3, 1:10]) # detect result


####Don't Run:将Document转化成csv方法,但无法转化回去##############
matrix <- inspect(corpus.jiang)
DF <- as.data.frame(matrix, stringsAsFactors = FALSE)

DF <- as.matrix(corpus.jiang)
DF <- Corpus(VectorSource(DF))

DF <- DocumentTermMatrix(DF)
write.table(DF)



