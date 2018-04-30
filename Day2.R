setwd("C:/Users/Administrator/Desktop/Data vizualization/Data Set")
atl <- read.csv("Crime in Atlanta 2009-2017.csv")
View(atl)
str(atl)
atl$date <- as.character(atl$date)
atl$date <- as.Date(atl$date, format = "%m/%d/%Y")
View(atl$date)

library(dplyr)
library(ggplot2)
atl$Month <- format(atl$date,"%b")
final_data <- atl %>% group_by(Month) %>% summarise(Count.of.crime = n())
View(final_data)
ggplot(final_data,aes(x = Month, y = Count.of.crime, fill = Month)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = month.abb) 
## scale_x_discrete(limits = month.abb) was used to give the categorical
## variable in the sequencs. Here it is Jan, Feb...
## limits = month.abb is used for abbreavations and 
## limits = month.name is used for a whole name of categorical variable

avg <- mean(final_data$Count.of.crime)
View(final_data)

final_data$Count.of.crime_signs <- ifelse(final_data$Count.of.crime < avg,
                                     -1*final_data$Count.of.crime,
                                     final_data$Count.of.crime)

ggplot(final_data, aes(x = Month,y = Count.of.crime_signs, fill = Count.of.crime)) +
  geom_bar(stat = "identity")+
  scale_x_discrete(limits = month.abb) 



final_data$normalised <- (final_data$Count.of.crime -avg)/sd(final_data$Count.of.crime)
#OR
final_data$norm <- scale(final_data$Count.of.crime)

ggplot(final_data, aes(x= Month, y = norm, fill = Count.of.crime))+ 
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = month.abb)



ggplot(final_data,aes(x = Month, y = Count.of.crime_signs, label = Count.of.crime_signs)) +
  geom_point(size =  10) +
  geom_segment(aes(x = Month, xend = Month,
                   y = 0 ,yend = Count.of.crime_signs)) +
  geom_text(col = "white", size = 3)


library(tidyr)
Name <- c("A","B","C","D")
Disagree <- c(20,30,12,24)
Dont_know <- c(10,5,18,6)
Agree <- c(16,26,20,34)
lies <- data.frame(Name,Disagree,Dont_know,Agree)
lies

a <- gather(lies, key,value,-Name)
ggplot(a, aes(x = Name, y = value, fill = key)) +geom_bar(stat = "identity")


ggplot(a, aes(x = Name, y = value, fill = key)) +geom_bar(stat = "identity", position = "dodge")

data("mpg")

scatter1 <- ggplot(mpg, aes(x= hwy, y = cty)) +
  geom_point(aes(col = class)) +
  labs(subtitle = "City Mileage v/s Highway Mileage", title = "Scatterplot",
       caption = "source : mpg data set") + xlab("Highway Mileage") +
  ylab("City Mileage") +
  theme(rect = element_blank(), axis.ticks = element_blank())

scatter2 <- ggplot(mpg, aes(x= hwy, y = cty)) +
  geom_point(aes(col = class), position = "jitter") +
  labs(subtitle = "City Mileage v/s Highway Mileage", title = "Scatterplot",
       caption = "source : mpg data set") + xlab("Highway Mileage") +
  ylab("City Mileage") +
  theme(rect = element_blank(), axis.ticks = element_blank())

scatter5 <- ggplot(mpg, aes(x= hwy, y = cty)) +
  geom_count(aes(col = class)) +
  labs(subtitle = "City Mileage v/s Highway Mileage", title = "Scatterplot",
       caption = "source : mpg data set") + xlab("Highway Mileage") +
  ylab("City Mileage") +
  theme(rect = element_blank(), axis.ticks = element_blank())



library(gridExtra)
grid.arrange(scatter1,scatter2)

mpg_city <- mpg %>% filter(cty >=15 & cty <= 20)
scatter3 <- ggplot(mpg_city, aes(x= hwy, y = cty)) +
  geom_point(aes(col = class)) +
  labs(subtitle = "City Mileage v/s Highway Mileage", title = "Scatterplot",
       caption = "source : mpg data set") + xlab("Highway Mileage") +
  ylab("City Mileage") +
  theme(rect = element_blank(), axis.ticks = element_blank())


scatter4 <- ggplot(mpg_city, aes(x= hwy, y = cty)) +
  geom_point(aes(col = class), position = "jitter") +
  labs(subtitle = "City Mileage v/s Highway Mileage", title = "Scatterplot",
       caption = "source : mpg data set") + xlab("Highway Mileage") +
  ylab("City Mileage") +
  theme(rect = element_blank(), axis.ticks = element_blank())

grid.arrange(scatter1,scatter2, scatter3,scatter4)


scatter6 <- ggplot(mpg_city, aes(x= hwy, y = cty)) +
  geom_count(aes(col = class)) +
  labs(subtitle = "City Mileage v/s Highway Mileage", title = "Scatterplot",
       caption = "source : mpg data set") + xlab("Highway Mileage") +
  ylab("City Mileage") +
  theme(rect = element_blank(), axis.ticks = element_blank())

grid.arrange(scatter1,scatter2,scatter5)

grid.arrange(scatter3,scatter4,scatter6)


scatter7 <- ggplot(mpg, aes(x = hwy, y = cty)) +
  geom_jitter(aes(col = class, size = displ)) +
  labs(subtitle = "City Mileage v/s Highway Mileage", title = "Bubble Chart",
       caption = "source : mpg data set") + xlab("Highway Mileage") +
  ylab("City Mileage") +
  theme(rect = element_blank(), axis.ticks = element_blank())

grid.arrange(scatter1,scatter2,scatter5,scatter7)

scatter8 <- ggplot(mpg, aes(x = hwy, y = cty)) +
  geom_jitter(aes(col = displ, size = class)) +
  labs(subtitle = "City Mileage v/s Highway Mileage", title = "Bubble Chart",
       caption = "source : mpg data set") + xlab("Highway Mileage") +
  ylab("City Mileage") +
  theme(rect = element_blank(), axis.ticks = element_blank())

grid.arrange(scatter7,scatter8)


scatter7 <- ggplot(mpg, aes(x = hwy, y = cty)) +
  geom_jitter(aes(col = class, size = displ,shape = as.factor(cyl))) +
  labs(subtitle = "City Mileage v/s Highway Mileage", title = "Bubble Chart",
       caption = "source : mpg data set") + xlab("Highway Mileage") +
  ylab("City Mileage") +
  theme(rect = element_blank(), axis.ticks = element_blank())
