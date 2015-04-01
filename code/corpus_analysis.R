# Content analysis on RMRB and Selections
#Package load
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("xtable" ,"wordcloud", "slam","igraph", "topicmodels","stm","ggplot2", "tidyr","dplyr")
ipak(packages)

source("E:/Dropbox_sync/Method/R/functions/multiplot.r")



#RMRB####
#Data load
#load("E:/Dropbox_sync/Method/Data/corpus/RMRB corpus/segment/dtm.rmrbII.RData")
load("E:/Dropbox_sync/Method/Data/corpus/RMRB corpus/segment/stm.rmrb_withpath.RData")

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

#2.1 15-Topic version###########
##Converge to stm corpus
meta6677<- meta6677[meta6677$time < 144,] 
# There was when I ran the 6677 an error ("Complete cases in prevalence covariate does not match the number of documents."). It is because in the readCorpus step, only 143 files was read rather than 144. I have no idea why that happens. The solution is just for convenience, that delete one line in the metadata.

for (i in 1:length(file)) {
  eval(parse(text = paste0("corpus <-readCorpus(dtm.rmrb", file[i], ", type = 'slam')")))
  eval(parse(text = paste0("out <- prepDocuments(corpus$documents, corpus$vocab, meta", file[i], ")")))
  eval(parse(text = paste0("topicsele", file[i], "<- selectModel(out$documents,out$vocab,K=15, ngroups = 20, 
                           prevalence =~ month + s(time),
                           data=out$meta, run = 20, seed=313)")))
  
  eval(parse(text = paste0("png('/home/users/yhu/Documents/figure/msele", file[i], "', width = 623, height = 477)")))
  eval(parse(text = paste0("plotModels(topicsele",file[i], ")")))
  
  dev.off()
}

#4649(4) 5065(3) 6677(4) 7891(2) 9203(4)

topicnames.full <- data.frame(c("Production","United Front","Disciplines","Eco-Org","Logistics","Worker Org","Inside Democracy (D)","Governance","Ideology","White Area","Civil War","Taking Over","Instit Construction","Class Division(M)","City Build"),
                              c("State Construction (D)","United Front","Worker Org","Revol Strategy (M)","Culture Construction","Instit Construction", "War Remnants","Production","Foreign Affairs","Planned Economy","Agriculture Tech","Ideology","Cold War","Production","Foreign News"),
                              c("Political Struggle","Thought Remoulding","Cold War","Agr Development","Mao Worship","Class Struggle","Social Movement","Agr Production","Foreign Coorperation","Mass Line (M)","Cultural Revolution","Enemy","Daily Work","Proletarian Dictatorship","International Communism (D)"),
                              c("National Strategy","Collective Ownship (D)","Reform and Opening","Organization","Market Economy","National Policy","Culture Construction","Redressment","Education","Daily Life","Party Leading","Foreign Affairs","Ideology (M)","Trade","United Front"),
                              c("National Strategy","Market Economy","Org and Supervision","Culture Construction","Ideology","Daily Life","China Style Development","Governing","Opening-up","Development Policy (D)","Leadership","Economic Reform","Achievements","Gov Responsibility (M)","Institutional Construction")
                              )
colnames(topicnames.full) <- c("Pre-Liberation", "PRC Founding", "Cultural Revolution","Pre-Tian'anmen", "Post-Tian'anmen")


topic.table <-  xtable(topicnames.full, label = "t:toplist",caption='Topic List of STM in Each Period')
print(topic.table, booktabs = T, hline.after = c(-1,0:14, nrow(topic.table)))

# 2.1.1 Example: 7891####
# 2.3.1 Select the best model (based on the graph produced in 2.2)
mod.out <- topicsele7891$runout[[2]]

# 2.3.2 Draw first 30 terms. Which has "democracy," then the topic is identified as democratic topic.

topicslists <- labelTopics(mod.out,n=10)

write.csv(as.data.frame(topicslists$prob), "./paper/table/stm7891.csv", fileEncoding = "UTF-8")
#search in excel

# 2.3.3 Define the topics
labelTopics(mod.out,n=10)
## Use the three most representative article, and find which includes democracy or so.
### not works, because the file is by month.
#findThoughts(mod.out, topic=1, texts=as.character(meta7891$file.path))$docs

# Identify the topics by "Highest" and "FREX"

topicnames <- as.character(topicnames.full[,4])
#topicnames <- c("National Strategy","Collective Ownship (D)","Reform and Opening","Organization","Market Economy","National Policy","Culture Construction","Redressment","Education","Daily Life","Party Leading","Foreign Affairs","Ideology (M)","Trade","United Front")


## Translation of frequency terms
englishF <- c("problem, economy, production, people, important, government, working unit, relevant, countryside, management", ## topic 1
              "work, production, life, research, serious, mission, democracy, in fact, people, party committee", ## topic 2
              "reform, jobs, economy, international cooperation, life, foundation, years, our country, science and technology", ## topic 3
              " comrades, conference, situation, technique, cadre, planning, development, department, local, our country ", ## topic 4
              "journalist, market, main, representative, business, funding, level, research, institution, law", ## topic 5
              " column, region, nation, business, policy, measure, effect, daily newspaper, education, comrade", ## topic 6
              " technology, culture, game, world, foundation, idea, relevant, product, focus, majority", ## topic 7
              " country, Gang of Four, comrade, leadership, revolution, agriculture, commune member, idea, aspect, region ", ## topic 8
              " country, education, idea, regulation, investment, city, student, worker, individuals, children ", ## topic 9
              " company, government, department, agriculture, people, bank, president, research, advice, military", ## 10
              " construction, staff, committee, world, country, central, spiritual, activity, youth, army ", ## 11
              " country, region, politics, business, measure, policy, effect, education, Li Peng, development", ## 12
              " Masses, development, socialism, international, cadre, history, politics, revolution, experience, delegation", ## 13
              " enterprise, development, the whole country, aspect, leadership, society,peasantry, relationship, career, management", ## 14
              "Prime Minister, enthusiasm, friendship, chairman, solidarity, head, Labor Party, oversea Chinese, complete, united front") ## 15

## Translation of FREX terms
englishFREX <- c("modification, conference, school, rich, family, association of science and technology, weak, position, muslim, cheap", ## 1
                 "textile, bad action, collective ownership, light industry, industry and transportation, modest, agriculture-carpentry-animal-husbandry", ## 2
                 "government and enterprises, computers, ten-thousand household, multi-channel, horizontal, professional ethics, important news, developmental, tiao-kuai relation, opening up ", ## 3
                 "constitution, craft, amateur, distribution, high school, court, proportion, streets, doctor, tropical ", ## 4
                 "concepts, structure, coach, capital, market, chemical weapons, satellite, expense, interest, monographs ", ## 5
                 " column, 12th party conference, guide, four modernizations, five stresses and four points of beauty, national army, suzuki, civility, household responsibility, unemployed youth ", ## 6
                 "game, shopping malls, bill, Chinese team, diving, peasantry, Mainland, popular, fixed assets, TV show", ## 7
                 "Gang of Four, the Revolutionary Committee, pernicious influence, extreme left, revisionism, production team, commune team, Da-zhai, oversea victim, working point", ## 8
                 "author, lack, primary school, morality, census, wealthy, children, vegetable, voter, politics and law ", ## 9
                 "Olympic Games, county head, final, price, bank, patient, motorcycle, body, reputation, series ", ## 10
                 "staff, committee, transformation, external, extensive, compatriot, checking, hard, party branch, central ", ## 11
                 " European Community, clean government, the Asian Games, Li Tieying, manipulation AIDS, Kaifu, dedication, high-tech, three-funded enterprises ", ## 12
                 " experience, delegation, agreement, ambassadors, the General Assembly, contribution, the Speaker, CCP, enthusiasm, martyr ", ## 13
                 " woman, security, exit, agricultural product, certificate, total sale, pharmaceutical, the entire city, provisional ", ## 14
                 " the people, chairman, Prime Minister, policy, issue, friendship, relationship, the whole country, minister, solidarity") ## 15


## simple regression plot
prep1 <- estimateEffect(1:15 ~ month, mod.out, meta7891)
prep <- plot.estimateEffect(prep1, "month", model=mod.out, custom.labels=topicnames,labeltype="custom")

## add the words 
k <- 8  ## this is the number of words to use -- I use 8 because it fits nicely
## put the words into a holder
holder <- c()
for(i in 1:15){
  holder[i] <- paste("F:",paste(strsplit(englishF[i],", ")[[1]][1:k],collapse=", "),
                     ## this line has a special k to make the words fit
                     "\\\\F:",paste(topicslists$prob[i,1:k],collapse=", "),
                     "\\\\FREX:",paste(strsplit(englishFREX[i],", ")[[1]][1:min(c(k,length(strsplit(englishFREX[i],", ")[[1]])))],collapse=", "),
                     "\\\\FREX:",paste(topicslists$frex[i,1:k],collapse=", "), "" )
}


topic.table <- data.frame(topicnames, holder)

colnames(topic.table) <- c("Topic", "Word Frequencies")

write.csv(as.data.frame(topic.table), "./paper/table/topiceg.csv", fileEncoding = "UTF-8")

# Then use excel open it, run "\shortstack[l]{"&C2&"}" to change the lines in latex, and then convert to latex

## Topic network figure#####

## Size of Topic: Size depends on how you calculate it.  mod.out$theta is a D-by-K matrix with document d in 1:D and its loading onto each topic.  If you want frequency by word tokens then you just have to multiply through the word counts within each document.

## I use this vector of wordcounts
i <- 4
eval(parse(text = paste0("corpus <-readCorpus(dtm.rmrb", file[i], ", type = 'slam')")))
eval(parse(text = paste0("out <- prepDocuments(corpus$documents, corpus$vocab, meta", file[i], ")")))

# line 2 is the word count for each word
wordcounts <- unlist(lapply(out$documents, function(x) sum(x[2,]))) 
## there are fractional wordcounts due to variational approximation.
round(mod.out$theta[,1] * wordcounts,2)

## Calculate the proportion of words devoted to topics
topicPropsInCorpus <- rep(NA,15)
for(i in 1:15){
  topicPropsInCorpus[i] <- (sum(mod.out$theta[,i] * wordcounts))/sum(wordcounts)
}
## This now holds the topic proportions in the corpus
topicPropsInCorpus
## sums to one, as it should
sum(topicPropsInCorpus)
## add the topic labels
names(topicPropsInCorpus) <- topicnames

## Plot the network
library(igraph)
## using a non-binary distance matrix
mat2 <- cor(mod.out$theta) 
#correlation of (Number of Documents by Number of Topics matrix of topic proportions).
## setting the negatives to zero
mat2[mat2<0] <- 0
## setting the diagonal to zero
diag(mat2) <- 0
## this gives us positive correlations between topics
mat2
## rename this object as "out"
out <- mat2
maxtopic <- topicnames[which(out == max(out[2,]), arr.in = T)[1]]
corMD <- data.frame(out[2,13], max(out[2,]))


## set a seed so that it's reproducable
set.seed(313)
## make the graph object
g <- graph.adjacency(out, mode="undirected", weighted=T)
if(length(labels)==0) labels = paste("Topic", topics)
## make the edges thickness proportional to correlation
cor(mod.out$theta)[,1]
E(g)
edges <- get.edgelist(g)
edgecors <- rep(NA,nrow(edges))
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(mod.out$theta)[edges[i,1],edges[i,2]]
}
edge.width=35*edgecors
## look at and set other graph parameters
E(g)$weight
E(g)$size <- 1
E(g)$lty <- 1
E(g)$color <- "black"

#Make the colors indicate the direction of the coefficient
## get the estimates
est <- unlist(lapply(prep$means,function(x){return(x[1])}))
## get colors
mycols <- rev(colorRampPalette(c("red", "white", "blue"), bias=1)( 20 )) ## (n)
## assign the color category for each coeff
seq(.04,.09,length.out=21) #Change to the coefficient range
colcat <- rep(NA,length(est))
for(i in 1:length(colcat)){
  colcat[i] <- max(which(est[i] > seq(.04,.09,length.out=21)))
}
## These are now the color category for each coefficient
colcat
## and these are the associated colors
mycols[colcat]
## This checks to make sure the colors are working as I expect
## Blue should be on the left and red on the right.
plot(est,1:15,pch=19,cex=2,col=mycols[colcat]);abline(v=0)

## label the vertices
V(g)$label=topicnames
## set the size of vertices proportional to the proportion in the corpus
V(g)$size <- topicPropsInCorpus*200
## set the color of vertices
vertex.color = mycols[colcat]
## set other vertex characteristics
vertex.label.cex = 1.5
vertex.label.color = "black"
## set the edge color
edge.color = "gray60"
## set a seet so that the layout is reproduceable
set.seed(313)
## pull out the weights to include in the layout
wts <- E(g)$weight
## make the layout
mylayout <- layout.fruchterman.reingold(g,weight=wts)
## start the image file
png("./paper/figure/cor7891.png", width = 3000, height = 1273)
## do the plot
plot(g, layout=mylayout,edge.color=edge.color,vertex.color=vertex.color, vertex.label.cex=vertex.label.cex, vertex.label.color=vertex.label.color,edge.width=edge.width)
## close the image file
dev.off()
## I then rearrange with photoshop to move the outlying vertices closer in



# 2.1.2 4649####
# 2.3.1 Select the best model (based on the graph produced in 2.2)
mod.out <- topicsele4649$runout[[4]]

# 2.3.2 Draw first 30 terms. Which has "democracy," then the topic is identified as democratic topic.

topicslists <- labelTopics(mod.out,n=30)

write.csv(as.data.frame(topicslists$prob), "./paper/table/stm4649.csv", fileEncoding = "UTF-8")
#search in excel (Topic 7)

# 2.3.3 Define the topics
labelTopics(mod.out,n=10)
## Use the three most representative article, and find which includes democracy or so.
### not works, because the file is by month.
#findThoughts(mod.out, topic=1, texts=as.character(meta4649$file.path))$docs

# Identify the topics by "Highest" and "FREX"

topicnames <- as.character(topicnames.full[,1])
#topicnames <- c("Production","United Front","Disciplines","Eco-Org","Logistics","Worker Org","Inside Democracy (D)","Governance","Ideology","White Area","Civil War","Taking Over","Instit Construction","Class Division(M)","City Build")

## Topic network figure#####

## Size of Topic: Size depends on how you calculate it.  mod.out$theta is a D-by-K matrix with document d in 1:D and its loading onto each topic.  If you want frequency by word tokens then you just have to multiply through the word counts within each document.

## I use this vector of wordcounts
i <- 1
eval(parse(text = paste0("corpus <-readCorpus(dtm.rmrb", file[i], ", type = 'slam')")))
eval(parse(text = paste0("out <- prepDocuments(corpus$documents, corpus$vocab, meta", file[i], ")")))


wordcounts <- unlist(lapply(out$documents, function(x) sum(x[2,])))
## there are fractional wordcounts due to variational approximation.
round(mod.out$theta[,1] * wordcounts,2)

## Calculate the proportion of words devoted to topics
topicPropsInCorpus <- rep(NA,15)
for(i in 1:15){
  topicPropsInCorpus[i] <- (sum(mod.out$theta[,i] * wordcounts))/sum(wordcounts)
}
## This now holds the topic proportions in the corpus
topicPropsInCorpus
## sums to one, as it should
sum(topicPropsInCorpus)
## add the topic labels
names(topicPropsInCorpus) <- topicnames

## Plot the network
## using a non-binary distance matrix
mat2 <- cor(mod.out$theta) 
#correlation of (Number of Documents by Number of Topics matrix of topic proportions).
## setting the negatives to zero
mat2[mat2<0] <- 0
## setting the diagonal to zero
diag(mat2) <- 0
## this gives us positive correlations between topics
mat2
## rename this object as "out"
out <- mat2
maxtopic <- c(maxtopic,topicnames[which(out == max(out[7,]), arr.in = T)[1]])
corMD <- rbind(corMD, c(out[7,14], max(out[7,])))

# run the regression on month
prep1 <- estimateEffect(1:15 ~ month, mod.out, meta4649)
prep <- plot.estimateEffect(prep1, "month", model=mod.out, custom.labels=topicnames,labeltype="custom")


## make the graph object
g <- graph.adjacency(out, mode="undirected", weighted=T)
if(length(labels)==0) labels = paste("Topic", topics)
## make the edges thickness proportional to correlation

edges <- get.edgelist(g)
edgecors <- rep(NA,nrow(edges))
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(mod.out$theta)[edges[i,1],edges[i,2]]
}
edge.width=35*edgecors
## look at and set other graph parameters
E(g)$weight
E(g)$size <- 1
E(g)$lty <- 1
E(g)$color <- "black"

#Make the colors indicate the direction of the coefficient
## get the estimates
est <- unlist(lapply(prep$means,function(x){return(x[1])}))
## get colors
mycols <- rev(colorRampPalette(c("red", "white", "blue"), bias=1)( 20 )) ## (n)
## assign the color category for each coeff
summary(est)
colcat <- rep(NA,length(est))
for(i in 1:length(colcat)){
  colcat[i] <- max(which(est[i] > seq(.02,.23,length.out=21)))
}
## These are now the color category for each coefficient
colcat
## and these are the associated colors
mycols[colcat]
## This checks to make sure the colors are working as I expect
## Blue should be on the left and red on the right.
plot(est,1:15,pch=19,cex=2,col=mycols[colcat]);abline(v=0)

## label the vertices
V(g)$label=topicnames
## set the size of vertices proportional to the proportion in the corpus
V(g)$size <- topicPropsInCorpus*200
## set the color of vertices
vertex.color = mycols[colcat]

## pull out the weights to include in the layout
wts <- E(g)$weight
## make the layout
mylayout <- layout.fruchterman.reingold(g,weight=wts)
## start the image file
png("./paper/figure/cor4649.png", width = 3000, height = 1273)
## do the plot
plot(g, layout=mylayout,edge.color=edge.color,vertex.color=vertex.color, vertex.label.cex=vertex.label.cex, vertex.label.color=vertex.label.color,edge.width=edge.width)
## close the image file
dev.off()
## I then rearrange with photoshop to move the outlying vertices closer in


# 2.3 5065####
# 2.3.1 Select the best model (based on the graph produced in 2.2)
mod.out <- topicsele5065$runout[[3]]

# 2.3.2 Draw first 30 terms. Which has "democracy," then the topic is identified as democratic topic.

topicslists <- labelTopics(mod.out,n=30)

write.csv(as.data.frame(topicslists$prob), "./paper/table/stm5065.csv", fileEncoding = "UTF-8")
#search in excel

# 2.3.3 Define the topics
labelTopics(mod.out,n=10)
## Use the three most representative article, and find which includes democracy or so.
### not works, because the file is by month.
#findThoughts(mod.out, topic=1, texts=as.character(meta5065$file.path))$docs

# Identify the topics by "Highest" and "FREX"

topicnames <- as.character(topicnames.full[,2])
#topicnames <- c("State Construction (D)","United Front(M)","Worker Org","Revol Strategy","Culture Construction","Instit Construction", "War Remnants","Production","Foreign Affairs","Planned Economy","Agriculture Tech","Ideology","Cold War","Production","Foreign News")

## Topic network figure#####

## Size of Topic: Size depends on how you calculate it.  mod.out$theta is a D-by-K matrix with document d in 1:D and its loading onto each topic.  If you want frequency by word tokens then you just have to multiply through the word counts within each document.

## I use this vector of wordcounts
i <- 2
eval(parse(text = paste0("corpus <-readCorpus(dtm.rmrb", file[i], ", type = 'slam')")))
eval(parse(text = paste0("out <- prepDocuments(corpus$documents, corpus$vocab, meta", file[i], ")")))


wordcounts <- unlist(lapply(out$documents, function(x) sum(x[2,])))
## there are fractional wordcounts due to variational approximation.

## Calculate the proportion of words devoted to topics
topicPropsInCorpus <- rep(NA,15)
for(i in 1:15){
  topicPropsInCorpus[i] <- (sum(mod.out$theta[,i] * wordcounts))/sum(wordcounts)
}
## This now holds the topic proportions in the corpus
topicPropsInCorpus
## sums to one, as it should
sum(topicPropsInCorpus)
## add the topic labels
names(topicPropsInCorpus) <- topicnames

## Plot the network
## using a non-binary distance matrix
mat2 <- cor(mod.out$theta) 
#correlation of (Number of Documents by Number of Topics matrix of topic proportions).
## setting the negatives to zero
mat2[mat2<0] <- 0
## setting the diagonal to zero
diag(mat2) <- 0
## this gives us positive correlations between topics
mat2
## rename this object as "out"
out <- mat2
maxtopic <- c(maxtopic,topicnames[which(out == max(out[1,]), arr.in = T)[1]])
corMD <- rbind(corMD, c(out[1,4], max(out[1,])))

# run the regression on month
prep1 <- estimateEffect(1:15 ~ month, mod.out, meta5065)
prep <- plot.estimateEffect(prep1, "month", model=mod.out, custom.labels=topicnames,labeltype="custom")


## make the graph object
g <- graph.adjacency(out, mode="undirected", weighted=T)
if(length(labels)==0) labels = paste("Topic", topics)
## make the edges thickness proportional to correlation

edges <- get.edgelist(g)
edgecors <- rep(NA,nrow(edges))
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(mod.out$theta)[edges[i,1],edges[i,2]]
}
edge.width=35*edgecors
## look at and set other graph parameters
E(g)$weight
E(g)$size <- 1
E(g)$lty <- 1
E(g)$color <- "black"

#Make the colors indicate the direction of the coefficient
## get the estimates
est <- unlist(lapply(prep$means,function(x){return(x[1])}))
## get colors
mycols <- rev(colorRampPalette(c("red", "white", "blue"), bias=1)( 20 )) ## (n)
## assign the color category for each coeff
summary(est)
colcat <- rep(NA,length(est))
for(i in 1:length(colcat)){
  colcat[i] <- max(which(est[i] > seq(.04,.1,length.out=21)))
}
## These are now the color category for each coefficient
colcat
## and these are the associated colors
mycols[colcat]
## This checks to make sure the colors are working as I expect
## Blue should be on the left and red on the right.
plot(est,1:15,pch=19,cex=2,col=mycols[colcat]);abline(v=0)

## label the vertices
V(g)$label=topicnames
## set the size of vertices proportional to the proportion in the corpus
V(g)$size <- topicPropsInCorpus*200
## set the color of vertices
vertex.color = mycols[colcat]

## pull out the weights to include in the layout
wts <- E(g)$weight
## make the layout
mylayout <- layout.fruchterman.reingold(g,weight=wts)
## start the image file
png("./paper/figure/cor5065.png", width = 3000, height = 1273)
## do the plot
plot(g, layout=mylayout,edge.color=edge.color,vertex.color=vertex.color, vertex.label.cex=vertex.label.cex, vertex.label.color=vertex.label.color,edge.width=edge.width)
## close the image file
dev.off()


# 2.3 6677####
# 2.3.1 Select the best model (based on the graph produced in 2.2)
mod.out <- topicsele6677$runout[[4]]

# 2.3.2 Draw first 30 terms. Which has "democracy," then the topic is identified as democratic topic.

topicslists <- labelTopics(mod.out,n=30)

write.csv(as.data.frame(topicslists$prob), "./paper/table/stm6677.csv", fileEncoding = "UTF-8")
#search in excel

# 2.3.3 Define the topics
labelTopics(mod.out,n=10)
## Use the three most representative article, and find which includes democracy or so.
### not works, because the file is by month.
#findThoughts(mod.out, topic=1, texts=as.character(meta6677$file.path))$docs

# Identify the topics by "Highest" and "FREX"

topicnames <- as.character(topicnames.full[,3])
#topicnames <- c("Political Struggle","Thought Remoulding","Cold War","Agr Development","Mao Worship","Class Struggle","Social Movement","Agr Production","Foreign Coorperation","Mass Line (M)","Cultural Revolution","Enemy","Daily Work","Proletarian Dictatorship","International Communism (D)")

## Topic network figure#####

## Size of Topic: Size depends on how you calculate it.  mod.out$theta is a D-by-K matrix with document d in 1:D and its loading onto each topic.  If you want frequency by word tokens then you just have to multiply through the word counts within each document.

## I use this vector of wordcounts
i <- 3
eval(parse(text = paste0("corpus <-readCorpus(dtm.rmrb", file[i], ", type = 'slam')")))
eval(parse(text = paste0("out <- prepDocuments(corpus$documents, corpus$vocab, meta", file[i], ")")))


wordcounts <- unlist(lapply(out$documents, function(x) sum(x[2,])))
## there are fractional wordcounts due to variational approximation.

## Calculate the proportion of words devoted to topics
topicPropsInCorpus <- rep(NA,15)
for(i in 1:15){
  topicPropsInCorpus[i] <- (sum(mod.out$theta[,i] * wordcounts))/sum(wordcounts)
}
## This now holds the topic proportions in the corpus
topicPropsInCorpus
## sums to one, as it should
sum(topicPropsInCorpus)
## add the topic labels
names(topicPropsInCorpus) <- topicnames

## Plot the network
## using a non-binary distance matrix
mat2 <- cor(mod.out$theta) 
#correlation of (Number of Documents by Number of Topics matrix of topic proportions).
## setting the negatives to zero
mat2[mat2<0] <- 0
## setting the diagonal to zero
diag(mat2) <- 0
## this gives us positive correlations between topics
mat2
## rename this object as "out"
out <- mat2
maxtopic <- c(maxtopic,topicnames[which(out == max(out[15,]), arr.in = T)[2]])
corMD <- rbind(corMD, c(out[15,10], max(out[15,])))

# run the regression on month
prep1 <- estimateEffect(1:15 ~ month, mod.out, meta6677[1:143,])
prep <- plot.estimateEffect(prep1, "month", model=mod.out, custom.labels=topicnames,labeltype="custom")


## make the graph object
g <- graph.adjacency(out, mode="undirected", weighted=T)
if(length(labels)==0) labels = paste("Topic", topics)
## make the edges thickness proportional to correlation

edges <- get.edgelist(g)
edgecors <- rep(NA,nrow(edges))
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(mod.out$theta)[edges[i,1],edges[i,2]]
}
edge.width=35*edgecors
## look at and set other graph parameters
E(g)$weight
E(g)$size <- 1
E(g)$lty <- 1
E(g)$color <- "black"

#Make the colors indicate the direction of the coefficient
## get the estimates
est <- unlist(lapply(prep$means,function(x){return(x[1])}))
## get colors
mycols <- rev(colorRampPalette(c("red", "white", "blue"), bias=1)( 20 )) ## (n)
## assign the color category for each coeff
summary(est)
colcat <- rep(NA,length(est))
for(i in 1:length(colcat)){
  colcat[i] <- max(which(est[i] > seq(.02,.11,length.out=21)))
}
## These are now the color category for each coefficient
colcat
## and these are the associated colors
mycols[colcat]
## This checks to make sure the colors are working as I expect
## Blue should be on the left and red on the right.
plot(est,1:15,pch=19,cex=2,col=mycols[colcat]);abline(v=0)

## label the vertices
V(g)$label=topicnames
## set the size of vertices proportional to the proportion in the corpus
V(g)$size <- topicPropsInCorpus*200
## set the color of vertices
vertex.color = mycols[colcat]

## pull out the weights to include in the layout
wts <- E(g)$weight
## make the layout
mylayout <- layout.fruchterman.reingold(g,weight=wts)
## start the image file
png("./paper/figure/cor6677.png", width = 3000, height = 1273)
## do the plot
plot(g, layout=mylayout,edge.color=edge.color,vertex.color=vertex.color, vertex.label.cex=vertex.label.cex, vertex.label.color=vertex.label.color,edge.width=edge.width)
## close the image file
dev.off()



# 2.3 9203 ####
# 2.3.1 Select the best model (based on the graph produced in 2.2)
mod.out <- topicsele9203$runout[[4]]

# 2.3.2 Draw first 30 terms. Which has "democracy," then the topic is identified as democratic topic.

topicslists <- labelTopics(mod.out,n=30)

write.csv(as.data.frame(topicslists$prob), "./paper/table/stm9203.csv", fileEncoding = "UTF-8")
#search in excel

# 2.3.3 Define the topics
labelTopics(mod.out,n=10)
## Use the three most representative article, and find which includes democracy or so.
### not works, because the file is by month.
#findThoughts(mod.out, topic=1, texts=as.character(meta9203$file.path))$docs

# Identify the topics by "Highest" and "FREX"
topicnames <- as.character(topicnames.full[,5])
#topicnames <- c("National Strategy","Market Economy","Org and Supervision","Culture Construction","Ideology","Daily Life","Ideo Education","Governing","Opening-up","Development Policy (D)","Leadership","Economic Reform","Achievements","Gov Responsibility (M)","Institutional COnstruction")

## Topic network figure#####

## Size of Topic: Size depends on how you calculate it.  mod.out$theta is a D-by-K matrix with document d in 1:D and its loading onto each topic.  If you want frequency by word tokens then you just have to multiply through the word counts within each document.

## I use this vector of wordcounts
i <- 5
eval(parse(text = paste0("corpus <-readCorpus(dtm.rmrb", file[i], ", type = 'slam')")))
eval(parse(text = paste0("out <- prepDocuments(corpus$documents, corpus$vocab, meta", file[i], ")")))


wordcounts <- unlist(lapply(out$documents, function(x) sum(x[2,])))
## there are fractional wordcounts due to variational approximation.

## Calculate the proportion of words devoted to topics
topicPropsInCorpus <- rep(NA,15)
for(i in 1:15){
  topicPropsInCorpus[i] <- (sum(mod.out$theta[,i] * wordcounts))/sum(wordcounts)
}
## This now holds the topic proportions in the corpus
topicPropsInCorpus
## sums to one, as it should
sum(topicPropsInCorpus)
## add the topic labels
names(topicPropsInCorpus) <- topicnames

## Plot the network
## using a non-binary distance matrix
mat2 <- cor(mod.out$theta) 
#correlation of (Number of Documents by Number of Topics matrix of topic proportions).
## setting the negatives to zero
mat2[mat2<0] <- 0
## setting the diagonal to zero
diag(mat2) <- 0
## this gives us positive correlations between topics
mat2
## rename this object as "out"
out <- mat2
maxtopic <- c(maxtopic,topicnames[which(out == max(out[10,]), arr.in = T)[2]])
corMD <- rbind(corMD, c(out[10,14], max(out[10,])))

# run the regression on month
prep1 <- estimateEffect(1:15 ~ month, mod.out, meta9203)
prep <- plot.estimateEffect(prep1, "month", model=mod.out, custom.labels=topicnames,labeltype="custom")


## make the graph object
g <- graph.adjacency(out, mode="undirected", weighted=T)
if(length(labels)==0) labels = paste("Topic", topics)
## make the edges thickness proportional to correlation

edges <- get.edgelist(g)
edgecors <- rep(NA,nrow(edges))
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(mod.out$theta)[edges[i,1],edges[i,2]]
}
edge.width=35*edgecors
## look at and set other graph parameters
E(g)$weight
E(g)$size <- 1
E(g)$lty <- 1
E(g)$color <- "black"

#Make the colors indicate the direction of the coefficient
## get the estimates
est <- unlist(lapply(prep$means,function(x){return(x[1])}))
## get colors
mycols <- rev(colorRampPalette(c("red", "white", "blue"), bias=1)( 20 )) ## (n)
## assign the color category for each coeff
summary(est)
colcat <- rep(NA,length(est))
for(i in 1:length(colcat)){
  colcat[i] <- max(which(est[i] > seq(.04,.09,length.out=21)))
}
## These are now the color category for each coefficient
colcat
## and these are the associated colors
mycols[colcat]
## This checks to make sure the colors are working as I expect
## Blue should be on the left and red on the right.
plot(est,1:15,pch=19,cex=2,col=mycols[colcat]);abline(v=0)

## label the vertices
V(g)$label=topicnames
## set the size of vertices proportional to the proportion in the corpus
V(g)$size <- topicPropsInCorpus*200
## set the color of vertices
vertex.color = mycols[colcat]

## pull out the weights to include in the layout
wts <- E(g)$weight
## make the layout
mylayout <- layout.fruchterman.reingold(g,weight=wts)
## start the image file
png("./paper/figure/cor9203.png", width = 3000, height = 1273)
## do the plot
plot(g, layout=mylayout,edge.color=edge.color,vertex.color=vertex.color, vertex.label.cex=vertex.label.cex, vertex.label.color=vertex.label.color,edge.width=edge.width)
## close the image file
dev.off()


# Correlation Present ####
corMD2 <- corMD
order <- c("2", "3", "1", "4", "5")
corMD <- corMD[match(order, rownames(corMD)),]
maxtopic <- maxtopic[match(c(2, 3, 1, 4, 5), seq(maxtopic))]

cor.table <- cbind(period = c("Pre-Liberation", "PRC Founding", "Cultural Revolution", "Pre-Tian'anmen", "Post-Tian'anmen"), corMD, maxtopic)
colnames(cor.table) <- c("Period","Democracy vs. Guardianship", "Max(Corr) with Democracy", "Max(Corr) Topic")

cor.graph <- gather(cor.table, "Period", "Correlation", 2:3)[,c(1,3,4)]


pd <- position_dodge(0.1)
ggplot(data = cor.graph, aes(x = Period, y = Correlation, color = Period.1, ymax=max(Correlation)*1.05)) + 
  geom_line(position=pd, aes(group=Period.1), size = 1) + 
  geom_point(position=pd, size = 3) + 
  scale_x_discrete(limits=c("Pre-Liberation", "PRC Founding", "Cultural Revolution", "Pre-Tian'anmen", "Post-Tian'anmen")) + 
  scale_colour_hue(name="Category",  l=40)  +
  ggtitle("Average bill for 2 people") +     # Set title
  theme_bw() + 
  theme(legend.position=c(.85, .25))  

ggsave("./paper/figure/cordyn.png")

cor.table.pr <-  xtable(cor.table, label = "t:cortab",caption='Correlations of the Democratic Topic with Others')
print(cor.table.pr, booktabs = T, hline.after = c(-1, 0, nrow(corMD)))



##### Selection Corpus ########################################
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
