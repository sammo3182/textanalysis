# Content analysis on RMRB and Selections
#Package load
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tm", "tmcn", "xtable" ,"wordcloud", "slam","igraph", "topicmodels","stm","ggplot2", "dplyr")
ipak(packages)

#Data load
#corpus.rmrb, corpus.mao, corpus.deng, corpus.jiang from corpus_prepare.R

#RMRB####

#Mao####

#Deng####

#Jiang######
#Descriptive analysis####

##出现频率最高的词
findFreqTerms(dtm.jiang, 2000)

##词云
m <- as.matrix(dtm.jiang)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
par(mar = rep(2, 4))

pal2 <- brewer.pal(8,"Dark2")
wordcloud(d$word,d$freq, scale=c(5,.2), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.15, 
          colors=pal2)
#save by the "export" function of R studio. The command way left too much empty in the graph.


##民主相关#####

#查找####
grep("民主", corpus$dimnames$Terms, value = T) #找出跟民主有关的词儿，因为不一定“民主会独立存在”

d <- c("民主化", "民主改革","民主集中制")
dtm.dem <- DocumentTermMatrix(d.corpus, control = list(dictionary = d, stopwords = myStopWords, wordLengths = c(2, Inf), bounds = list(global = c(2,Inf)), removePunctuation = T, removeNumbers = T)) 

matrix <- inspect(dtm.dem)
DF <- as.data.frame(matrix, stringsAsFactors = FALSE)
xtable(DF, booktabs=TRUE, caption = "Democra- Distribution in Jiang", display=c("f","d", "d", "d"), label = "t:minzhufre")

##和民主相关的词####
#这个在小文本里没什么意义，但在大文本（RMRB）中或许有用
# Word association: The number under each word is an association score, so the search term always occurs with the search term.

dem1 <- findAssocs(dtm.jiang, "民主化", 0.999) %>% data.frame() %>% add_rownames() %>% 
  select(-民主化) %>% slice(1:10)
dem2 <- findAssocs(dtm.jiang, "民主改革", 0.999) %>% data.frame() %>% add_rownames() %>% 
  select(-民主改革) %>% slice(1:10)
dem3 <- findAssocs(dtm.jiang, "民主集中制", .999) %>% data.frame() %>% add_rownames() %>% 
  select(-民主集中制) %>% slice(1:10)

dem.com <- cbind(dem1, dem2,  dem3)
colnames(dem.com) <- c("Democratization", "Democratic Reform", "Democratic Centralism")

write.csv(dem.com, "./paper/table/dem.jiang.csv", fileEncoding = "UTF-8")

#Topic Model####
source("./code/extra.function.R") #需要提前根据model调整simulation 类型。
## 确定主题数量####
dtm <- dtm.jiang
##先对文本-词矩阵进行简单处理，以消除高频词被高估和低频词被低估的问题。
term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
l1=term_tfidf >= quantile(term_tfidf, 0.5)       # second quantile, ie. median
dtm <- dtm[,l1]
dtm = dtm[row_sums(dtm)>0, ]; dim(dtm) # 2246 6210
summary(col_sums(dtm))


fold_num = 10
kv_num =  c(5, 10*c(1:5, 10))
seed_num = 313
try_num = 1

sp <- smp(n=dtm$nrow, seed=seed_num) # n = nrow(dtm)

system.time((ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp)))

## plot the perplexity

m_per=apply(ctmK[[1]],1,mean)
m_log=apply(ctmK[[2]],1,mean)

k=c(kv_num)
df = ctmK[[1]]  # perplexity matrix
logLik = ctmK[[2]]  # perplexity matrix

png(".paper/figure/perplexity_jiang.png")

matplot(k, df, type = c("b"), xlab = "Number of topics", 
        ylab = "Perplexity", pch=1:try_num,col = 1, main = '')       
legend("topright", legend = paste("fold", 1:try_num), col=1, pch=1:try_num) 

dev.off()

png(".paper/figure/loglikelihood_jiang.png")

matplot(k, logLik, type = c("b"), xlab = "Number of topics", 
        ylab = "Log-Likelihood", pch=1:try_num,col = 1, main = '')       
legend("topright", legend = paste("fold", 1:try_num), col=1, pch=1:try_num)  #The higher the better


dev.off()

## Gibbs LDA ####
k = 10
SEED <- 313
jss_TM2 <- list(
  Gibbs = LDA(dtm, k = k, method = "Gibbs", 
              control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000))
  
#save(jss_TM2, file = "./code/jss_TM2.Rdata")

termsForSave<- terms(jss_TM2[["Gibbs"]], 10)


write.csv(as.data.frame(t(termsForSave)), "./code/gibs.jiang.csv", fileEncoding = "UTF-8")


#'topic graphs'

tfs = as.data.frame(termsForSave3, stringsAsFactors = F);tfs[,1]
adjacent_list = lapply(1:5, function(i) embed(tfs[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:5, function(i) rep(i, 9)))
edgelist$topic = topic

#需要igraph包
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )
nodeLabel = V(g)$name
E(g)$color =  unlist(lapply(sample(colors()[26:137], 10), function(i) rep(i, 9))); unique(E(g)$color)

# 保存图片格式
#png(  paste(getwd(), "/topic_graph_gibbs.png", sep=""）,
#            width=5, height=5, 
#            units="in", res=700)
      
      plot(g, vertex.label= nodeLabel,  edge.curved=TRUE, 
           vertex.label.cex =1,  edge.arrow.size=0.2, layout=l )
      
# 结束保存图片
#      dev.off()

#STM
##Converge to stm corpus
stm.jiang <-readCorpus(dtm.jiang, type = "slam")

file.name <- list.files(path = "E:/Dropbox_sync/Method/Data/corpus/Selection corpus/jiang", full.names = F, recursive = TRUE)

vol <- gsub(".txt", "",  file.name)
vol.num <- seq(vol)

meta.jiang <- data.frame(vol = vol, vol.num = vol.num)

out.jiang <- prepDocuments(stm.jiang$documents, stm.jiang$vocab, meta.jiang) #double check the format of the data

jiang.stm <- stm(out.jiang$documents,out.jiang$vocab,K=3,
                       prevalence =~ s(vol.num),
                       data=out$meta,seed=313) # K必须要不大于于document数,如数量大时用ngroups option


jiang.sel <- selectModel(out.jiang$documents,out.jiang$vocab,K=3,
                 prevalence =~ s(vol.num),
                 data=out$meta, run = 3, seed=313) #选择topic数

plotModels(jiang.sel) #靠右上的最好，但在这个case，只有一个，所以不用选择
jiang.stm <- jiang.sel$runout[[1]]

#Don't run: 自动选择较好model
#storage <- manyTopics(out.jiang$documents, out.jiang$vocab, K=c(2, 3), 
#                      prevalence =~ s(vol.num), data=out.jiang$meta, run = 3, seed=313)
#data太小，无法run

##Interpretation
###Explain the topics
#列出FREX：
labelTopics(jiang.stm, c(1, 2, 3))

#Topic/Metadata relationships
prep <- estimateEffect(1:3~vol.num, jiang.stm, meta = out.jiang$meta, uncertainty = "Global")

plot.estimateEffect(prep, covariate = "vol.num", topics = 1:3,
                     model=jiang.stm, method="pointestimate", labeltype = "prob",
                     xlim=c(-.1,.4))
#showing that the topics of the three volumn does not change too much

#Cloud of topics
cloud(jiang.stm, topic = 3)

#Expected proportion of the corpus that belongs to each topic.
plot.STM(jiang.stm,type="summary")

#Topic correpation
corr.jiang<-topicCorr(jiang.stm) #method huge cannot be applied, because of the small sample.
plot.topicCorr(corr.jiang)
