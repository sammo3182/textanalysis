# Package preload ####
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr","ggplot2","tm", "tmcn", "Rwordseg")
ipak(packages)



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

d.corpus <- Corpus(DirSource("H:/Documents/corpus/rmrb_corpus/monthly", encoding = "UTF-8"), list(language = NA))

## 清除標點符號, 數字
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)

## 清除大小寫英文與數字
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

## Segment and selection
d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)

d.corpus<- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) %in% c("Ag", "a", "ad", "an", "b", "f", "g", "h", "i", "j", "k", "l", "Ng", "n", "nr", "ns", "nt", "nz", "s", "Vg", "v", "vd", "vn", "z")] 
  })
  unlist(noun)
})

d.corpus <- tm_map(d.corpus, segmentCN, returnType = 'tm')

d.corpus <- Corpus(VectorSource(d.corpus))

myStopWords <- c(stopwordsCN(), "专栏", "新华社", "本报", "本报消息", "本报讯", "本市讯", "记者", "报道" )
#d.corpus <- tm_map(d.corpus, removeWords, myStopWords)

corpus <- TermDocumentMatrix(d.corpus, control = list(stopwords = myStopWords))



##Converge to stm corpus
corpus.bi <-readCorpus(corpus, type = "dtm")


####################Selections###########################

#Mao####
mao <- readLines("E:/Dropbox_sync/Method/Data/corpus/Selection corpus/毛泽东选集.txt", encoding = "UTF-8")

mao <- gsub("注\\s+释", "", mao)
mao <- gsub("\\s+〔\\d{1,2}〕.*", "", mao)
mao <- gsub("-+", "", mao)
mao <- gsub("\\s+", "", mao)
mao <- gsub("[A-Za-z0-9]", "", mao)
mao <- mao[sapply(mao, nchar) > 0] 
mao <- paste0(mao, collapse = "")
mao <- removePunctuation(mao)
mao <- removeNumbers(mao)
mao <- segmentCN(mao, nature = T)%>% unlist()

d.corpus <- Corpus(VectorSource(mao), list(language = NA))

d.corpus<- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) %in% c("Ag", "a", "ad", "an", "b", "f", "g", "h", "i", "j", "k", "l", "Ng", "n", "nr", "ns", "nt", "nz", "s", "Vg", "v", "vd", "vn", "z")] 
  })
  unlist(noun)
})

d.corpus <- tm_map(d.corpus, segmentCN, returnType = 'tm')

d.corpus <- Corpus(VectorSource(d.corpus))

#d.corpus <- tm_map(d.corpus, removeWords, myStopWords)

corpus <- TermDocumentMatrix(d.corpus, control = list(stopwords = stopwordsCN()))


#Deng ####
#分卷
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

#clean
d.corpus <- Corpus(DirSource("E:/Dropbox_sync/Method/Data/corpus/Selection corpus/deng/"), list(language = NA))

## 清除標點符號, 數字
d.corpus <- tm_map(d.corpus, function(word){paste0(word, collapse = " ")})

d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)

## Segment and selection

d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
  #gsub("[\\r\\n]", "", word)
  #gsub("\\r\\n", "", word)
  #gsub("\\n", "", word)
  #paste0(word, collapse = " ")
})

d.corpus <- tm_map(d.corpus,segmentCN, returnType = "tm")
d.corpus <- tm_map(d.corpus,unlist)

d.corpus <- Corpus(VectorSource(d.corpus))

corpus <- DocumentTermMatrix(d.corpus, 
                             control = list(stopwords = stopwordsCN()))
