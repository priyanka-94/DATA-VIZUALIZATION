setwd("C:/Users/Administrator/Desktop/EDA/quiz1")
str(odi)
odi <- read.csv("odi-batting.csv")
View(odi)


head <- odi%>% group_by(Ground)%>% summarise(frequency = length(unique(MatchDate))) %>%
  arrange(-frequency) %>%
  head(10)


  ggplot(head,aes(x = reorder(Ground,-frequency),y = head$frequency)) + 
  geom_bar(fill = factor(head$frequency),stat = "identity") + coord_flip() +
    xlab("Top 10 Grounds") + ylab("Frequency") + ggtitle("Bar chart of top 10 grounds and frequncies of them")+
    theme(legend.background = element_blank(),rect = element_blank()) +
    geom_text(aes(label = frequency)) 

  
  ggplot(head,aes(x = reorder(Ground,-frequency),y = head$frequency)) + 
    geom_bar(aes(fill = Ground),stat = "identity") +
    xlab("Top 10 Grounds") + ylab("Frequency") + ggtitle("Bar chart of top 10 grounds and frequncies of them")+
    theme(legend.background = element_blank(),rect = element_blank(), axis.ticks = element_blank(), axis.text.x = element_text(angle = 45)) +
    geom_text(aes(label = frequency)) 
  
  
  ggplot(head,aes(x = reorder(Ground,-frequency),y = head$frequency)) + 
    geom_bar(aes(fill = Ground),stat = "identity") +
    xlab("Top 10 Grounds") + ylab("Frequency") + ggtitle("Bar chart of top 10 grounds and frequncies of them")+
    theme(legend.background = element_blank(),legend.key.width = unit(0.3,"cm"),legend.position = "bottom",rect = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) +
    geom_text(aes(label = frequency), vjust = -0.25) 
  
  
  setwd("C:/Users/Administrator/Desktop/Data vizualization/Data Set/rajanand-crime-in-india")
reason <- read.csv("39_Specific_purpose_of_kidnapping_and_abduction.csv", na.strings = "NULL")  
library(dplyr)
library(ggplot2)
View(reason)
a <- reason[,c(1,2,3,13)]
colnames(a) <-c("State","Year","Reason","Total")
View(a)
a[1:3231,] -> a
View(a)

a <- na.omit(a)

res <- a %>% group_by(Reason,State) %>% summarise(sum = sum(Total))
View(res)

res$Reason<- gsub("Kidnap - For ","",res$Reason)

library(ggplot2)
ggplot(res,aes(x = State,y = sum, fill = Reason)) + geom_bar(stat = "identity",position = "fill") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(),
        rect = element_blank()) + ggtitle("Kidnapping in respective states")



ggplot(res,aes(x = State,y = Reason, fill = sum)) + geom_bar(stat = "identity",position = "fill") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(),
        rect = element_blank()) + ggtitle("Kidnapping in respective states") +
  geom_tile(aes(fill = sum), col = "white") +
  scale_fill_gradient2(low = "darkgreen",mid = "white",high = "darkred")

str(res$Reason)

ggplot(res,aes(x = State,y = Reason, fill = sum)) + geom_raster()+
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(),
        rect = element_blank()) + ggtitle("Kidnapping in respective states") +
  geom_tile(aes(fill = sum), col = "white") +
  scale_fill_gradient2(low = "darkgreen",mid = "white",high = "darkred")


library(wordcloud)
library(tm)
tbl <- table(res$Reason)
View(tbl)
wordcloud(words = names(tbl), freq = as.numeric(tbl),scale = c(0.75,0.15),
              min.freq = 1)
