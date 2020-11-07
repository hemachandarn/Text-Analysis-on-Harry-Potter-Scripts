library(dplyr)
library(tidytext)
library(RSentiment)
library(wordcloud)
library(wordcloud2)
library(tm)
library(tidyverse)
library(reshape2)
library(radarchart)
library(textstem)
library(igraph)
library(RSentiment)

setwd("C:/Users/dalle/OneDrive/Documents/MIS612Fall2019/Working Directory")
scripts <- read.csv("Harry Potter Final.csv")

colnames(scripts)<- c("Character1", "Character2", "Narrative")

scripts <- scripts %>% mutate(Narrative_lower = tolower(Narrative)) # mutate() function is from the dplyr package and it is used to create a new column

scripts <- scripts %>% mutate(Narrative_noNumbers = gsub('[[:digit:]]','',Narrative_lower)) # gsub functoin searches for any digit in the text and removes it; 

c(stopwords('en'), "shh", "ahh")

stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')

scripts <- scripts %>% mutate(Narrative_noStopWords = gsub(stopwords_regex,'',Narrative_noNumbers))

scripts <- scripts %>% mutate(Narrative_noPunctuation = gsub('[[:punct:]]','',Narrative_noStopWords))

scripts <-scripts %>% mutate(Narrative_noTypos = gsub('thankssssssss','thanks',Narrative_noPunctuation))

scripts <- scripts %>% mutate(Narrative_noSpaces = gsub('\\s+',' ',Narrative_noTypos))

scripts <-scripts %>% mutate(Narrative_Lemma = lemmatize_strings(Narrative_noSpaces))

my_text <- scripts %>% select(Narrative_Lemma)

my_corpus <- my_text

my_corpus <- my_corpus %>% rename(text = Narrative_Lemma)  %>% mutate(doc_id = rownames(my_text))

my_corpus <- Corpus(DataframeSource(my_corpus))

my_dtm <- as.matrix(DocumentTermMatrix(my_corpus))

my_tdm <- as.matrix(TermDocumentMatrix(my_corpus))

my_tfidf <- as.matrix(DocumentTermMatrix(my_corpus, control = list(weighting = weightTfIdf)))

scripts <- scripts %>% select(Character1, Character2, Narrative_Lemma)

colnames(scripts)<- c("Character1", "Character2", "dialogue")

freq = data.frame(sort(colSums(as.matrix(my_tfidf)), decreasing=TRUE))
freg= (-c("ahh"))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(5, "Reds"))
#most relevant words using tfidf in word cloud


topHarrypottercharacter <- as.data.frame(sort(table(scripts$Character1), decreasing=TRUE))[1:12,]
#12 characters with the most lines


ggplot(data=topHarrypottercharacter, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#D95F02", colour="black") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=30, hjust=1)) +
  labs(x="Character", y="Number of Lines")
#histogram with number of lines


tokens <- scripts %>%  
  mutate(dialogue=as.character(scripts$dialogue)) %>%
  unnest_tokens(word, dialogue)
#store every word as a token


tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=100)
#Polarity cloud on entire script using tidy text


sentiments <- tokens %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE)
#Save sentiment as polarity


ggplot(data=sentiments, aes(x=reorder(sentiment, -n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency") +
  theme_bw()
#plot the level of polarity of tidy text


sentiments <- tokens %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE)
#save sentiment as multiple sentiments

sentiments <- sentiments[-c(1,2,3),]
#remove the word harry as a sentiment because nrc recognizes it as a negative word and it is the most used

ggplot(data=sentiments, aes(x=reorder(sentiment, -n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency") +
  theme_bw()
#plot array of sentiments


tokens %>%
  filter(Character1 %in% c("Harry","Hermione","Ron","Hagrid","Dumbledore",
                           "McGonagall","Draco", "Vernon", "Voldemort")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(Character1, sentiment, sort=TRUE) %>%
  ggplot(aes(x=sentiment, y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~Character1, scales="free_x") +
  labs(x="Sentiment", y="Frequency") +
  coord_flip() +
  theme_bw()
#polarity (bing) of most important characters

mystopwords <- data_frame(word=c(stopwords("english"), 
                                 c("2","3","harry", "â")))
#remove stop words that were showing up on first trial

topchartokens <- scripts %>%
  mutate(dialogue=as.character(scripts$dialogue)) %>%
  filter(Character1 %in% c("Draco","Dumbledore","Hagrid","Harry","Hermione",
                          "McGonagall","Ron","Vernon","Voldemort")) %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(mystopwords, by="word")



topchartokens %>%
  count(Character1, word) %>%
  bind_tf_idf(word, Character1, n) %>%
  group_by(Character1) %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, Character1, sep="__"), 
                      levels=rev(paste(word, Character1, sep="__"))))%>%
  ggplot(aes(x=word2, y=tf_idf)) +
  geom_col(aes(fill=Character1), show.legend=FALSE) +
  facet_wrap(~Character1, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(y="tf-idf", x="Word") +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()




ScriptSentiment<- my_text$Narrative_Lemma
calculate_total_presence_sentiment(ScriptSentiment)
#sentiment analysis on entire script using RSentiment

Harry<- subset(scripts, Character1== "Harry")
#seperate Harrys lines from the script
harrysentiment<- Harry$dialogue
#seperate Harry's diologue
calculate_total_presence_sentiment(harrysentiment)
#sentiment analysis on all of Harrys lines

Ron<- subset(scripts, Character1 == "Ron")
#separate Rons lines
ronsentiment<- Ron$dialogue
#seperate the words
calculate_total_presence_sentiment(ronsentiment)
#sentiment of Rons lines

Hermione<- subset(scripts, Character1 == "Hermione")
#separate Hermiones lines
Hermionesentiment<- Hermione$dialogue
#separate words
calculate_total_presence_sentiment(Hermionesentiment)
#Sentiment on Hermiones lines

Hagrid<- subset(scripts, Character1 == "Hagrid")
#Hagrid Lines
HagridSentiment<- Hagrid$dialogue
#seperate words
calculate_total_presence_sentiment(HagridSentiment)
#sentiment analysis on hagrid words

Draco<- subset(scripts, Character1 == "Draco")
#seperate Dracos Lines
DracoSentiment<- Draco$dialogue
#words only
calculate_total_presence_sentiment(DracoSentiment)
#Analysis on Dracos lines

Voldemort<- subset(scripts, Character1 == "Voldemort")
#Voldemort lines
VoldemortSentiment<- Voldemort$dialogue
#words
calculate_total_presence_sentiment(VoldemortSentiment)
#analysis on voldemort

Dumbledore<- subset(scripts, Character1 == "Dumbledore")
#dumbledorelines
DumbledoreSEntiment<- Dumbledore$dialogue
#just words
calculate_total_presence_sentiment(DumbledoreSEntiment)
#Analysis on Dumbledore

Vernon<- subset(scripts, Character1 == "Vernon")
#Vernon Lines
VernonSentiment<- Vernon$dialogue
#words only
calculate_total_presence_sentiment(VernonSentiment)

McGonagall<- subset(scripts, Character1 == "McGonagall")
#McGonagall lines
McGonagallSentiment<- McGonagall$dialogue
#words only
calculate_total_presence_sentiment(McGonagallSentiment)





HarrytoRon<- subset(Harry, Character2== "Ron")
#instances where Harry is speaking to Ron
HarrytoRonSentiment<- HarrytoRon$dialogue
#seperate all dialogue from Harry speaking to Ron
calculate_total_presence_sentiment(HarrytoRonSentiment)
#Sentiment analysis on Harry to Ron lines

HarrytoHermione<- subset(Harry, Character2 == "Hermione")
#separate Harry to Hermione
HarrytoHermioneSentiment<- HarrytoHermione$dialogue
#sperate dialogues from Harry to Hermione
calculate_total_presence_sentiment(HarrytoHermioneSentiment)
#Sentiment analysis on Harry to Hermione Lines




RontoHarry<- subset(Ron, Character2 == "Harry")
#Ron speaking to Harry
RontoHarrysentiment<- RontoHarry$dialogue
#seperate dialogues from Ron to Harry
calculate_total_presence_sentiment(RontoHarrysentiment)
#SEntiment anaylsis on Ron to Harry lines

RontoHermione<- subset(Ron, Character2== "Hermione")
#Ron speaking to Hermione
RontoHermioneSentiment<- RontoHermione$dialogue
#seperate Ron to Hermione
calculate_total_presence_sentiment(RontoHermioneSentiment)
#Sentiment analysis on Ron to Hermione





HermionetoRon<- subset(Hermione, Character2 == "Ron")
#Hermione speaking to ROn
HermionetoRonSEntiment<- HermionetoRon$dialogue
#seperate hermione to ron
calculate_total_presence_sentiment(HermionetoRonSEntiment)
#sentiment analysis on hermione to ron

HermionetoHarry<- subset(Hermione, Character2 == "Harry")
#Hermione to Harry
HermionetoHarrySentiment<- HermionetoHarry$dialogue
#seperate hermione to harry
calculate_total_presence_sentiment(HermionetoHarrySentiment)





HagridtoHarry<- subset(Hagrid, Character2 =="Harry")
#Hagrid to Harry dialogue
HagridtoHarrySentiment<- HagridtoHarry$dialogue
#seperate Hagrid to Harry 
calculate_total_presence_sentiment(HagridtoHarrySentiment)

McGonagalltoHarry<- subset(McGonagall, Character2 == "Harry")
#eperate McGonagall to Harry dialogue
McGonagalltoHarrySentiment<- McGonagalltoHarry$dialogue
#seperate McGonagall to Harry
calculate_total_presence_sentiment(McGonagalltoHarrySentiment)

DumbledoretoHarry<- subset(Dumbledore, Character2 == "Harry")
#seperate dumbledore and Harry
DumbledoretoHarrySentiment<- DumbledoretoHarry$dialogue
#seperate Dumbledore to Harry
calculate_total_presence_sentiment(DumbledoretoHarrySentiment)
#analysis on dumbledore to Harry

DracotoHarry<- subset(Draco, Character2== "Harry")
#seperate draco to harry
DracotoHarrySentiment<- DracotoHarry$dialogue
#seperate draco to Harry
calculate_total_presence_sentiment(DracotoHarrySentiment)

VernontoHarry<- subset(Vernon, Character2== "Harry")
#seperate Vernon to Harry
VernontoHarrySentiment<- VernontoHarry$dialogue
#eperate Vernon talking to Harry
calculate_total_presence_sentiment(VernontoHarrySentiment)




