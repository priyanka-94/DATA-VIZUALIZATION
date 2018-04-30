setwd("C:/Users/Administrator/Desktop/Data vizualization/Data Set/rajanand-crime-in-india")
crime <- read.csv("01_District_wise_crimes_committed_IPC_2001_2012.csv")
View(crime)
library(dplyr)
crime_mur <- crime %>% group_by(STATE.UT,YEAR) %>% filter(DISTRICT == "TOTAL" & MURDER > 70)
library(ggplot2)

View(crime_mur)

rape<- ggplot(crime_mur, aes(x = RAPE, y = `KIDNAPPING.AND.ABDUCTION.OF.WOMEN.AND.GIRLS`))+
  geom_point(aes(col = `STATE.UT`, size = MURDER,frame = YEAR), position = "jitter") +
  labs(subtitle = "Rape v/s Kidnap", title = "Bubble Chart",
       caption = "source : crime India") + xlab("RAPE") +
  ylab("KIDNAPPING.AND.ABDUCTION.OF.WOMEN.AND.GIRLS") +
  theme(rect = element_blank(), axis.ticks = element_blank())

rape

#install.packages("plotly")
library(plotly)

rape_plot <- ggplotly(rape)
rape_plot


reason<- ggplot(crime_mur, aes(x = RAPE, y = `KIDNAPPING.AND.ABDUCTION.OF.WOMEN.AND.GIRLS`))+
  geom_point(aes(col = `STATE.UT`, size = MURDER,frame = YEAR), position = "jitter") +
  labs(subtitle = "Rape v/s Kidnap", title = "Bubble Chart",
       caption = "source : crime India") + xlab("RAPE") +
  ylab("KIDNAPPING.AND.ABDUCTION.OF.WOMEN.AND.GIRLS") +
  theme(rect = element_blank(), axis.ticks = element_blank())


purpose <- read.csv("39_Specific_purpose_of_kidnapping_and_abduction.csv")
View(purpose)
purpose$K_A_Grand_Total
purpose$Group_Name
purpose$ï..Area_Name


data("mpg")
mpg
View(mpg)
View(data1)
data1 <- mpg %>% 
  group_by(manufacturer,class) %>% 
  summarize(count = n()) 
mp <- ggplot(data1, aes(x = manufacturer, y = count,fill = class)) + 
  geom_bar(stat = "identity")
mp1 <- mp + 
  geom_text(data = data1, aes(x = manufacturer, y = count, label = count), position = position_stack(vjust = 0.5))



data2 <- data1 %>% 
  group_by(manufacturer,class) %>% 
  summarise(co = n()) %>% 
  mutate(percent = round(co/sum(co) *100,2))

data2 %>% 
  ggplot(aes(x = manufacturer, y = percent,fill = class)) + 
  geom_bar(stat = "identity", position = "fill") + 
  geom_text(data = data2, aes(x = manufacturer, y = percent, label = percent), position = position_fill(vjust = 0.5))


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
View(res)

library(ggplot2)

ggplot(res,aes(x = State,y = sum, fill = Reason)) + geom_bar(stat = "identity",position = "fill") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(),
        rect = element_blank()) + ggtitle("Kidnapping in respective states")




res11 <- a %>% group_by(Reason,State) %>% summarise(count2 = n())
View(res11)

res11$Reason<- gsub("Kidnap - For ","",res11$Reason)
View(res11)

library(ggplot2)

ggplot(res11,aes(x = State,y = count2, fill = Reason)) + geom_bar(stat = "identity",position = "fill") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(),
        rect = element_blank()) + ggtitle("Kidnapping in respective states")


ggplot(res11,aes(x = State,y = Reason))+ coord_flip()+
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(),
        rect = element_blank(), axis.title.y = element_text(angle = 90)) + 
  ggtitle("Kidnapping in respective states") +
  geom_tile(aes(fill = count2), col = "white") + theme_bw() +
  scale_fill_gradient(low = "green",high = "red")
  

# Analyse the city mileage of various classes cars for each cylinder
data("mpg")
View(mpg)
mpg_class <-mpg %>% group_by(class)
unique(mpg$class)
ggplot(mpg_class, aes(x = class, y = cty)) +
  geom_bar(aes(fill = factor(cyl)),stat = "identity", position = "fill")


ggplot(mpg_class, aes(x = class, y = cty)) +
  geom_boxplot(aes(fill = factor(cyl)), varwidth = T)

ggplot(mpg,aes(x = displ)) +
  geom_histogram(aes(fill = class), binwidth = 2) 


ggplot(mpg,aes(x = manufacturer)) +
  geom_histogram(aes(fill = class), stat = "count", binwidth = 0.5)
