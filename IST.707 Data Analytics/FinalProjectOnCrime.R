library(dplyr)
crime_raw <- read.csv('crime.csv', header = T)
str(crime_raw)
summary(crime_raw)

#Check foe missing value in all columns 
colSums(is.na(crime_raw))
#we can see there are 3 columns that contains NA
#REPORTING_AREA 20250, Lat 19999, Long 19999

#consider the data set is quite large, decide to drop those columns with NA
crime_raw1 <- na.omit(crime_raw)

# Encoding categorical data
crime_raw1$SHOOTING = factor(crime_raw1$SHOOTING,
                        levels = c('', 'Y'),
                        labels = c('No','Yes'))

str(crime_raw1)
summary(crime_raw1)
#change variable to factor for some variable
crime_raw1$MONTH = factor(crime_raw1$MONTH)

crime_raw2 <- crime_raw1 %>%
  mutate(REPORTING_AREA = as.factor(REPORTING_AREA),
         YEAR = as.factor(YEAR),
         HOUR = as.factor(HOUR))

#try remove duplicate if there is
crime_raw2 %>% duplicated() %>% table()
#Hey, we found 22 duplicates from the data set
crime_raw3 <- crime_raw2 %>% unique()
table(crime_raw3$MONTH)
#The dataset cover crime data from 2015 to 2018
table(crime_raw3$YEAR)
table(crime_raw3$OFFENSE_CODE_GROUP)

crime2018 = crime_raw3 %>% filter(YEAR== 2018)  
crime2018 %>% count(OFFENSE_DESCRIPTION) %>% arrange(-n) %>% head(5) %>%
  ggplot(aes(reorder(OFFENSE_DESCRIPTION,n),n))+
  geom_col(fill = "gray")+
  coord_flip()+
  xlab("Top 5 Crime in 2018")+
  ylab("Number of Crimes")

#plot using ggplot2
library(ggplot2)
#histogram plot for some crime that happened in 2018
serious_crimes_2018 <- filter(crime_raw3, YEAR == 2018, OFFENSE_CODE_GROUP %in% c('Bomb Hoax','Explosives','Firearm Violations','Firearm Discovery','HUMAN TRAFFICKING'))
#Hour
ggplot(data = filter(serious_crimes_2018)) +
  geom_bar(mapping = aes(x = HOUR, fill = OFFENSE_CODE_GROUP)) +
  xlab("HOUR") +
  ylab("Number of Crime")+
  ggtitle('Crime')

#Month
ggplot(data = filter(serious_crimes_2018)) +
  geom_bar(mapping = aes(x = MONTH, fill = OFFENSE_CODE_GROUP)) +
  xlab("MONTH") +
  ylab("Number of Crime")+
  ggtitle('Crime')

#Year
serious_crimes <- filter(crime_raw3, OFFENSE_CODE_GROUP %in% c('Bomb Hoax','Explosives','Firearm Violations','Firearm Discovery','HUMAN TRAFFICKING'))
ggplot(data = filter(serious_crimes)) +
  geom_bar(mapping = aes(x = YEAR, fill = OFFENSE_CODE_GROUP)) +
  xlab("Year") +
  ylab("Number of Crime")+
  ggtitle('Crime')

#street
street = crime_raw3 %>% count(STREET) %>% arrange(-n) %>% head(5)
ggplot(street)+
  geom_col(aes(x = reorder(STREET,n),y = n), fill = "deepskyblue")+
  coord_flip()+
  xlab("Street Name")+
  ylab("Number of crimes")

#A map for serious crime that happened on 2018 boston. 
library(leaflet)
crimes_2018 <- filter(crime_raw3, YEAR == 2018, OFFENSE_CODE_GROUP %in% c('Bomb Hoax','Explosives','Firearm Violations','Firearm Discovery','HUMAN TRAFFICKING'))
map <- addTiles(leaflet())
colors = c('Red', 'Green', 'Blue', 'yellow')
i <- 1
serious_crime_map <- map
for (i in c('Bomb Hoax','Explosives','Firearm Violations','Firearm Discovery','HUMAN TRAFFICKING'))
{
  crime_set <- filter(crimes_2018, OFFENSE_CODE_GROUP == i)
  serious_crime_map <- addCircleMarkers(setView(serious_crime_map, lng = -71.06, lat = 42.36, zoom = 12), lng = crime_set$Long, lat = crime_set$Lat, radius = 0.1,color = colors[i])
  i <- i + 1
}
serious_crime_map
###################

crimes_2018 <- filter(crime_raw3, YEAR == 2018, OFFENSE_CODE_GROUP %in% c('Bomb Hoax','Explosives','Firearm Violations','Firearm Discovery','HUMAN TRAFFICKING'))

basemap <- addTiles(leaflet())

colors = c('red', 'green', 'blue', 'yellow','purple')

i = 1

crimes <- basemap

for (crime in c('Bomb Hoax','Explosives','Firearm Violations','Firearm Discovery','HUMAN TRAFFICKING'))
{
  c <- filter(crimes_2018, OFFENSE_CODE_GROUP == crime)
  crimes <- addCircleMarkers(setView(crimes, lng = -71.08, lat = 42.33, zoom = 12), lng = c$Long, lat = c$Lat, radius = 1, fillOpacity = 6, color = colors[i])
  i <- i + 1
}

crimes

#####Clustering Analysis
# K-Means Clustering
library(stats)
library(dplyr)
library(ggplot2)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
str(crime_raw3)
summary(crime_raw3)

#OFFENSE_CODE, REPORTING_AREA,lat,long for year 2018
#convert REPORTING_AREA to numerical variable in here
crimes_raw3_2018_10 <- filter(crime_raw3, YEAR == 2018, MONTH == '1', HOUR == '10')
crimes_raw3_2018_10$REPORTING_AREA = as.numeric(crimes_raw3_2018_10$REPORTING_AREA)
crime_raw4 <- crimes_raw3_2018_10 %>% select(c(OFFENSE_CODE,REPORTING_AREA))
str(crime_raw4)
summary(crime_raw4)

# Elbow plot our data
elbow_plot_crime <- crime_raw4 %>%
  fviz_nbclust(kmeans,
               method = 'wss')
elbow_plot_crime

# K-Means clustering
set.seed(1)

km_crime <- crime_raw4 %>% 
  kmeans(centers = 4,
         nstart = 500)

fviz_cluster(km_crime, crime_raw4)


#Naive Bayes
library(tm)
library(SnowballC) 
library(dplyr)
str(crime_raw3)
summary(crime_raw3)

crimes_raw5 <- filter(crime_raw3, YEAR == 2018)
crimes_raw5_train <- crimes_raw5[1:45534,]
crimes_raw5_test <- crimes_raw5[45535:60712,]

crimes_raw5_train_labels <- crimes_raw5[1:45534,]$SHOOTING
crimes_raw5_test_labels <- crimes_raw5[45535:60712,]$SHOOTING

library(e1071)

crime_classifier <- naiveBayes(crimes_raw5_train, crimes_raw5_train_labels)

crime_test_pred <- predict(crime_classifier, crimes_raw5_test)

# install.packages('gmodels')
library(gmodels)

CrossTable(crime_test_pred, crimes_raw5_test_labels,
           prop.chisq = F, prop.c = F, prop.r = F,
           dnn = c('predicted','actual'))

# Adjusting the model with Laplace
crime_classifier_2 <- naiveBayes(crimes_raw5_train, crimes_raw5_train_labels, laplace = 1)
crime_test_pred_2 <- predict(crime_classifier_2, crimes_raw5_test)

CrossTable(crime_test_pred_2, crimes_raw5_test_labels,
           prop.chisq = F, prop.c = F, prop.r = F,
           dnn = c('predicted','actual'))

