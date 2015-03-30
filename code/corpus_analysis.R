# Content analysis on RMRB and Selections
#Package load
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tm", "tmcn", "xtable" ,"wordcloud", "slam","igraph", "topicmodels","stm","ggplot2", "tidyr","dplyr")
ipak(packages)

source("E:/Dropbox_sync/Method/R/functions/multiplot.r")



#RMRB####
#Data load
load("E:/Dropbox_sync/Method/Data/corpus/RMRB corpus/segment/dtm.rmrbII.RData")

# 1. Descriptive###
# 1.1 词云 ####
eval(parse(text = paste0("m <- as.matrix(",dtm.list[1],")")))
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)

pal2 <- brewer.pal(8,"Dark2")

wordcloud(d$word,d$freq, scale=c(3,.2), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.15, 
          colors=pal2)

# 重复上述步骤四遍，由于未知原因，loop不管用####
eval(parse(text = paste0("m <- as.matrix(",dtm.list[2],")")))
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
par(mar = rep(1, 4))

par(mar=rep(.2, 4))
pal2 <- brewer.pal(8,"Dark2")


wordcloud(d$word,d$freq, scale=c(3,.2), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.15, 
          colors=pal2)


eval(parse(text = paste0("m <- as.matrix(",dtm.list[3],")")))
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
par(mar = rep(1, 4))

par(mar=rep(.2, 4))
pal2 <- brewer.pal(8,"Dark2")

wordcloud(d$word,d$freq, scale=c(3,.2), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.15, 
          colors=pal2)


eval(parse(text = paste0("m <- as.matrix(",dtm.list[4],")")))
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
par(mar = rep(1, 4))

par(mar=rep(.2, 4))
pal2 <- brewer.pal(8,"Dark2")

wordcloud(d$word,d$freq, scale=c(1.9,.2), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.15, 
          colors=pal2)



eval(parse(text = paste0("m <- as.matrix(",dtm.list[5],")")))
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
par(mar = rep(1, 4))

par(mar=rep(.2, 4))
pal2 <- brewer.pal(8,"Dark2")


wordcloud(d$word,d$freq, scale=c(2.5,.2), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.15, 
          colors=pal2)

# 1.2 民主相关 ####
grep("民主", dtm.rmrb7891$dimnames$Terms, value = T) 

dem.times <- NULL
month.total <- NULL
term.list <- paste0("dtm.rmrb", file, "$dimnames$Terms")
doc.list <- paste0("dtm.rmrb", file, "$dimnames$Docs")

for(i in seq(term.list)){
  eval(parse(text = paste0("times <- length(grep('民主',", term.list[i],"))")))
  dem.times <- c(dem.times, times)
  
  eval(parse(text = paste0("times <- length(", doc.list[i],") ")))
  month.total <- c(month.total, times)
}

desc.table <- data.frame(period = c("Pre-Liberation", "PRC Founding", "Cultural Revolution", "Pre-Tian'anmen", "Post-Tian'anmen"), year = c("1946-1949", "1950-1965", "1966-1977", "1978-1991", "1992-2003"), month.total, dem.times)

colnames(desc.table) <- c("Period", "Year", "Month", "Types. of 'Democra-'")
#xtable(desc.table, booktabs=TRUE, caption = "Corpus Description", display=c("f","s","s", "d", "d"), label = "t:describ")

# 1.3 民主临近 ####
dem4649 <- data.frame(inspect(dtm.rmrb4649[1:dtm.rmrb4649$nrow, c("民主","民主化", "民主改革")])) %>% summarise(sum(民主), sum(民主化), sum(民主改革))

dem5065 <- data.frame(inspect(dtm.rmrb5065[1:dtm.rmrb4649$nrow, c("民主", "民主化", "民主改革")])) %>% summarise(sum(民主), sum(民主化), sum(民主改革))

dem6677 <- data.frame(inspect(dtm.rmrb6677[1:dtm.rmrb4649$nrow, c("民主", "民主化", "民主改革")])) %>% summarise(sum(民主), sum(民主化), sum(民主改革))

dem7891 <- data.frame(inspect(dtm.rmrb7891[1:dtm.rmrb4649$nrow, c("民主", "民主化", "民主改革")])) %>% summarise(sum(民主), sum(民主化), sum(民主改革))

dem9203 <- data.frame(inspect(dtm.rmrb9203[1:dtm.rmrb4649$nrow, c("民主", "民主化", "民主改革")])) %>% summarise(sum(民主), sum(民主化), sum(民主改革))


dem.stat <- bind_rows(dem4649, dem5065, dem6677, dem7891, dem9203)

desc.table <- bind_cols(desc.table, dem.stat)

colnames(desc.table) <- c("Period", "Year", "Month", "Type of 'Democra-'", "Democracy", "'Democratization'", "'Democratic Reform'")
xtable(desc.table, booktabs=TRUE, caption = "Corpus Description", display=c("f","s","s", "d","d", "d", "d", "d"), label = "t:describ")



desc.graphI <- select(desc.table, Period, 5)

demoI <- ggplot(data=desc.graphI, aes(x=Period, y=Democracy)) + 
  geom_bar(stat="identity",
           position=position_dodge(),
           size=.3) +                        # Thinner lines
  scale_x_discrete(limits=c("Pre-Liberation", "PRC Founding", 
                            "Cultural Revolution","Pre-Tian'anmen", "Post-Tian'anmen")) +
  xlab("Democracy") + ylab("Frequency") + # Set axis labels
  theme_bw() 

ggsave("demodestriI.png", path = "./paper/figure/")




desc.graphII <- select(desc.table, Period, 6:7) %>% gather("Category", "Democra", 2:3)

demoII <- ggplot(data=desc.graphII, aes(x=Period, y=Democra, fill=Category)) + 
  geom_bar(stat="identity",
           position=position_dodge(),
           size=.3) +                        # Thinner lines
  scale_x_discrete(limits=c("Pre-Liberation", "PRC Founding", 
                            "Cultural Revolution","Pre-Tian'anmen", "Post-Tian'anmen")) +
  scale_fill_hue(name="Democra-") +      # Set legend title
  xlab("Period") + ylab("Frequency") + # Set axis labels
  theme_bw() + 
  theme(legend.justification=c(0,0), legend.position=c(0.7,0.5), 
        legend.title = element_text(size = 12, face="bold"),
        legend.text = element_text(size = 12)
        )


ggsave("demodestriII.png", path = "./paper/figure/")


multiplot(demoI, demoII)


# 2. STM####
##Converge to stm corpus
corpus4649 <-readCorpus(dtm.rmrb4649, type = "slam")

file.name <- list.files(path = "H:/Documents/data/rmrb_corpus/monthly/4649", full.names = F, recursive = TRUE)


out4649 <- prepDocuments(corpus4649$documents, corpus4649$vocab, meta4649) #double check the format 

stm4649 <- stm(out4649$documents,out4649$vocab,K=3,
                 prevalence =~ s(vol.num),
                 data=out$meta,seed=313) # K必须要不大于于document数,如数量大时用ngroups option


topicsele4649 <- selectModel(out4649$documents,out4649$vocab,K=20, ngroups = 4,
                         prevalence =~ month + s(time),
                         data=out4649$meta, run = 20, seed=313) #选择topic数

plotModels(jiang.sel) #靠右上的最好，但在这个case，只有一个，所以不用选择
jiang.stm <- jiang.sel$runout[[1]]



#Don't run: 自动选择较好model
#storage <- manyTopics(out4649$documents, out4649$vocab, K=c(2, 3), 
#                      prevalence =~ s(vol.num), data=out4649$meta, run = 3, seed=313)
#data太小，无法run

##Interpretation
###Explain the topics
#列出FREX：
labelTopics(jiang.stm, c(1, 2, 3))

#Topic/Metadata relationships
prep <- estimateEffect(1:3~vol.num, jiang.stm, meta = out4649$meta, uncertainty = "Global")

plot.estimateEffect(prep, covariate = "vol.num", topics = 1:3,
                    model=jiang.stm, method="pointestimate", labeltype = "prob",
                    xlim=c(-.1,.4))
#showing that the topics of the three volumn does not change too much

#Cloud of topics
cloud(jiang.stm, topic = 3)

#Expected proportion of the corpus that belongs to each topic.
plot.STM(jiang.stm,type="summary")

#Topic correpation
corr4649<-topicCorr(jiang.stm) #method huge cannot be applied, because of the small sample.
plot.topicCorr(corr4649)




# Define the topics
## Use the three most representative article, and find which includes democracy or so.
findThoughts(mod.out, topic=2, texts=as.character(meta$path),n=10)$docs




#############################################
# Data loading
load("H:/Documents/data/Selection corpus/dtm.sele.Rdata")
load("E:/Dropbox_sync/Method/Data/corpus/Selection corpus/dtm.sele.Rdata")
source("./code/extra.function.R") #需要提前根据model调整simulation 类型。


#Mao####
##1. 词云####
m <- as.matrix(dtm.mao)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
par(mar = rep(2, 4))

pal2 <- brewer.pal(8,"Dark2")
wordcloud(d$word,d$freq, scale=c(2.5,.2), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.15, 
          colors=pal2)

# 2. 民主相关#####
grep("民主", dtm.mao$dimnames$Terms, value = T) #找出跟民主有关的词儿，因为不一定“民主会独立存在”
times <- length(grep("民主", dtm.mao$dimnames$Terms, value = T))

dem.mao <- data.frame(inspect(dtm.mao[1:dtm.mao$nrow, c("民主","民主化", "民主改革")])) %>% summarise(sum(民主), sum(民主化), sum(民主改革))
dem.mao <- cbind(times, dem.mao)


# 3. Topic Model####
## 3.1 确定主题数量####
dtm <- dtm.mao
##先对文本-词矩阵进行简单处理，以消除高频词被高估和低频词被低估的问题。
term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
l1=term_tfidf >= quantile(term_tfidf, 0.5)       # second quantile, ie. median
dtm <- dtm[,l1]
dtm = dtm[row_sums(dtm)>0, ]; dim(dtm) # 2246 6210
summary(col_sums(dtm))


fold_num = 10
kv_num =  c(5, 10*c(1:5))
seed_num = 313
try_num = 1

sp <- smp(n=dtm$nrow, seed=seed_num) # n = nrow(dtm)

system.time((ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp)))

## plot the perplexity

m_per=apply(ctmK[[1]],1,mean)
m_log=apply(ctmK[[2]],1,mean)

k=c(kv_num)
df = ctmK[[1]]  # perplexity matrix (math expression is in http://qpleple.com/perplexity-to-evaluate-topic-models/)
logLik = ctmK[[2]]  # perplexity matrix

png("./paper/figure/perplexity.mao.png")

par(mar=c(4, 4.1, 2, 2.1))
matplot(k, df, type = c("b"), xlab = "Number of Topics", 
        ylab = "Perplexity", pch=1:try_num,col = 1, main = "")       
axis(side=1, at=seq(0, 100, by = 10)) # therefore 20 topics

dev.off()


## 3.2 Gibbs LDA ####
k = 10
SEED <- 313
jss_TM2 <- list(
  Gibbs = LDA(dtm, k = k, method = "Gibbs", 
              control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)))

#save(jss_TM2, file = "./code/jss_TM2.Rdata")

termsForSave<- terms(jss_TM2[["Gibbs"]], 10)

write.csv(as.data.frame(t(termsForSave)), "./paper/table/gibs.mao.csv", fileEncoding = "UTF-8")
# Topic 1, 6, 7 has democracy

#'topic graphs'

tfs = as.data.frame(termsForSave, stringsAsFactors = F);tfs[,1]
adjacent_list = lapply(1:10, function(i) embed(tfs[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:10, function(i) rep(i, 9)))
edgelist$topic = topic


#需要igraph包
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )
nodeLabel = V(g)$name

E(g)$color =  unlist(lapply(sample(colors()[26:137], 10, replace = T), function(i) rep(i, 9))); unique(E(g)$color)

plot(g, vertex.label= nodeLabel,  edge.curved=TRUE, 
     vertex.label.cex = 1,  edge.arrow.size=0.2, layout=l)

# Then mark the topics manually





#Deng####
##1. 词云####
m <- as.matrix(dtm.deng)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
par(mar = rep(2, 4))

pal2 <- brewer.pal(8,"Dark2")
wordcloud(d$word,d$freq, scale=c(2.5,.2), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.15, 
          colors=pal2)

# 2. 民主相关#####
grep("民主", dtm.deng$dimnames$Terms, value = T) #找出跟民主有关的词儿，因为不一定“民主会独立存在”
times <- length(grep("民主", dtm.deng$dimnames$Terms, value = T))

dem.deng <- data.frame(inspect(dtm.deng[1:dtm.deng$nrow, c("民主","民主化")])) %>% summarise(sum(民主), sum(民主化))
dem.deng <- cbind(times, dem.deng)


# 3. Topic Model####
## 3.1 确定主题数量####
dtm <- dtm.deng
##先对文本-词矩阵进行简单处理，以消除高频词被高估和低频词被低估的问题。
term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
l1=term_tfidf >= quantile(term_tfidf, 0.5)       # second quantile, ie. median
dtm <- dtm[,l1]
dtm = dtm[row_sums(dtm)>0, ]; dim(dtm) # 2246 6210
summary(col_sums(dtm))


fold_num = 10
kv_num =  c(5, 10*c(1:5))
seed_num = 313
try_num = 1

sp <- smp(n=dtm$nrow, seed=seed_num) # n = nrow(dtm)

system.time((ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp)))

## plot the perplexity

m_per=apply(ctmK[[1]],1,mean)
m_log=apply(ctmK[[2]],1,mean)

k=c(kv_num)
df = ctmK[[1]]  # perplexity matrix (math expression is in http://qpleple.com/perplexity-to-evaluate-topic-models/)
logLik = ctmK[[2]]  # perplexity matrix

png("./paper/figure/perplexity.deng.png")

par(mar=c(4, 4.1, 2, 2.1))
matplot(k, df, type = c("b"), xlab = "Number of Topics", 
        ylab = "Perplexity", pch=1:try_num,col = 1, main = "")       
axis(side=1, at=seq(0, 100, by = 10)) # therefore 10 topics

dev.off()


## 3.2 Gibbs LDA ####
k = 10
SEED <- 313
jss_TM2 <- list(
  Gibbs = LDA(dtm, k = k, method = "Gibbs", 
              control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)))

#save(jss_TM2, file = "./code/jss_TM2.Rdata")

termsForSave<- terms(jss_TM2[["Gibbs"]], 20)

write.csv(as.data.frame(t(termsForSave)), "./paper/table/gibs.deng.csv", fileEncoding = "UTF-8")
# Topic 5 has democracy

#'topic graphs'

tfs = as.data.frame(termsForSave, stringsAsFactors = F);tfs[,1]
adjacent_list = lapply(1:10, function(i) embed(tfs[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:10, function(i) rep(i, 9)))
edgelist$topic = topic


#需要igraph包
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )
nodeLabel = V(g)$name
E(g)$color =  unlist(lapply(sample(colors()[26:137], 10, replace = T), function(i) rep(i, 9))); unique(E(g)$color)

plot(g, vertex.label= nodeLabel,  edge.curved=TRUE, 
     vertex.label.cex = 1,  edge.arrow.size=0.2, layout=l)

# Then mark the topics manually




#Jiang######
# 1. Descriptive analysis####

##出现频率最高的词
findFreqTerms(dtm.jiang, 2000)

##词云
m <- as.matrix(dtm.jiang)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
par(mar = rep(2, 4))

pal2 <- brewer.pal(8,"Dark2")
wordcloud(d$word,d$freq, scale=c(3,.2), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.15, 
          colors=pal2)
#save by the "export" function of R studio. The command way left too much empty in the graph.

# 2. 民主相关#####
grep("民主", dtm.jiang$dimnames$Terms, value = T) #找出跟民主有关的词儿，因为不一定“民主会独立存在”
times <- length(grep("民主", dtm.jiang$dimnames$Terms, value = T))

dem.jiang <- data.frame(inspect(dtm.jiang[1:dtm.jiang$nrow, c("民主","民主化", "民主改革")])) %>% summarise(sum(民主), sum(民主化), sum(民主改革))
dem.jiang <- cbind(times, dem.jiang)

##Combine with mao and deng
dem.all <- bind_rows(dem.mao, dem.deng, dem.jiang) %>% mutate(selection = c("Mao", "Deng", "Jiang"))
dem.all <- dem.all[c("selection","times","sum(民主)","sum(民主化)","sum(民主改革)")]

colnames(dem.all) <- c("Selection",  "Type of 'Democra-'", "Democracy", "'Democratization'", "'Democratic Reform'")
desc.table <- xtable(dem.all, caption = "Selection Description", label = "t:describsele")
print(desc.table, booktabs = T, hline.after = c(-1, nrow(desc.table)))


# 3. Topic Model####
## 3.1 确定主题数量####
dtm <- dtm.jiang
##先对文本-词矩阵进行简单处理，以消除高频词被高估和低频词被低估的问题。
term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
l1=term_tfidf >= quantile(term_tfidf, 0.5)       # second quantile, ie. median
dtm <- dtm[,l1]
dtm = dtm[row_sums(dtm)>0, ]; dim(dtm) # 2246 6210
summary(col_sums(dtm))


fold_num = 10
kv_num =  c(5, 10*c(1:5))
seed_num = 313
try_num = 1

sp <- smp(n=dtm$nrow, seed=seed_num) # n = nrow(dtm)

system.time((ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp)))

## plot the perplexity

m_per=apply(ctmK[[1]],1,mean)
m_log=apply(ctmK[[2]],1,mean)

k=c(kv_num)
df = ctmK[[1]]  # perplexity matrix (math expression is in http://qpleple.com/perplexity-to-evaluate-topic-models/)
logLik = ctmK[[2]]  # perplexity matrix

png("./paper/figure/perplexity.jiang.png")

par(mar=c(4, 4.1, 2, 2.1))
matplot(k, df, type = c("b"), xlab = "Number of Topics", 
        ylab = "Perplexity", pch=1:try_num,col = 1, main = "")       
axis(side=1, at=seq(0, 100, by = 10)) # therefore 10 topics

dev.off()

## 3.2 Gibbs LDA ####
k = 10
SEED <- 313
jss_TM2 <- list(
  Gibbs = LDA(dtm, k = k, method = "Gibbs", 
              control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)))
  
#save(jss_TM2, file = "./code/jss_TM2.Rdata")

termsForSave<- terms(jss_TM2[["Gibbs"]], 10)


write.csv(as.data.frame(t(termsForSave)), "./paper/table/gibs.jiang.csv", fileEncoding = "UTF-8")
# Topic 5 has democracy

#'topic graphs'

tfs = as.data.frame(termsForSave, stringsAsFactors = F);tfs[,1]
adjacent_list = lapply(1:10, function(i) embed(tfs[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:10, function(i) rep(i, 9)))
edgelist$topic = topic


#需要igraph包
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )
nodeLabel = V(g)$name
E(g)$color =  unlist(lapply(sample(colors()[26:137], 10, replace = T), function(i) rep(i, 9))); unique(E(g)$color)
  
   plot(g, vertex.label= nodeLabel,  edge.curved=TRUE, 
           vertex.label.cex = 1,  edge.arrow.size=0.2, layout=l)

# Then mark the topics manually
