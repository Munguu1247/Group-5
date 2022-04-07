library(tidyverse)
library(knitr)
library(skimr)
library(dplyr)
library(modeldata)
library(tidymodels)

data_set <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')


#Data wrangling 

data_set1 <- data_set %>%
  rename(park_size = med_park_size_data, park_size_points = med_park_size_points, 
         size_in_city = park_pct_city_data, size_in_city_points = park_pct_city_points,
         near_park = pct_near_park_data, near_park_points = pct_near_park_points, 
         spending = spend_per_resident_data, spending_points = spend_per_resident_points,
         total_percent = total_pct) 

data_set1 <- data_set1[, -c(12:23, 27, 28)]


actual_data <- data_set1 %>%
  mutate(spending_class = cut_number(spending_points, n = 3, labels = c("low", "med", "high")))



actual_data <- actual_data %>%
  mutate(city_class = cut_number(total_points, n = 3, labels = c("low", "med", "high")))


actual_data <- actual_data[-c(100),]


actual_data <- actual_data %>%
  mutate_if(is.character, factor)

skim(actual_data)

actual_data %>%
  count(year) %>%
  kable()

actual_data %>%
  count(city) %>%
  head(10) %>%
  kable()


actual_data %>%
  ggplot(aes(rank, spending_points)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  labs(x = 'rank', y = "spending points",  
       title = 'Relationship between rank and spending points', caption = 'Figure 4')
  
actual_data %>%
  ggplot(aes(rank, amenities_points)) +
  geom_point(color = 'dark grey') +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  labs(x = 'rank', y = "amenities points",  
       title = 'Relationship between rank and amenities points', caption = 'Figure 5')



#MODELLING: Regression
#Liner model: To predict width using Liner model. As we can see, rank of city and spending + amenities_points per resident have positive relationship.
##Step1: Split the data set to test and train

data_split <- initial_split(actual_data)

data_training <- training(data_split)

data_test <- testing(data_split)


##Step2: Pre-processing of the data

#Recipe

data_recipe_lm <- recipe(rank ~ spending_class + amenities_points, data = data_training)

summary(data_recipe_lm)


#step_xxx
#Check for outliers and wrong data

data_recipe1 <- data_recipe_lm %>%
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_corr(all_numeric())

summary(data_recipe1)

#Prepare

data_prep1 <- prep(data_recipe1, training = data_training)

summary(data_prep1)


#Bake
data_bake1 <- bake(data_prep1, data_training)  



#Step3
#model1


model1 <- lm(rank ~ spending_class + amenities_points, data = data_bake1, na.action = na.omit)

summary(model1)  

#As result of model1, pvalue= ***, residual = 6.912, adjusted R= 0.2196

 

#MODELLING: Classification

##Pre-processing of the data: 4 steps. only creates the logic
#1. Set the recipe and get the ingredients [recipe()]

data_recipe2 <- recipe(city_class ~ rank, data = data_training)

summary(data_recipe2)


#2. Write the recipe steps [step_xxx()]

data_recipe2 <- data_recipe2 %>%
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_corr(all_numeric())

#3. Get ready with the preparations [prep()]

data_prep2 <- prep(data_recipe2, training = data_training)

summary(data_prep2)

#4. Bake the recipe [bake()]

data_bake2 <- bake(data_prep2, data_training)

library(randomForest)

rf1 <- randomForest(city_class ~ rank, data = data_bake2, na.action = na.omit)

print(rf1)

#Error rate is 59.38% thta is not fit well




#Graphs are used in Presentation:

#Figure 1 shows Nationa Park Service 2021 in Presentation

year <- c('2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
spending_billion <- c(14.7, 14.6, 15.7, 16.9, 18.4, 18.2, 20.2, 21.0, 14.5 )
visitors <- c(282.7, 273.6, 292.8, 307.2, 330.9, 330.8, 318.2, 327.5, 237.1)


spending_stat <- data.frame(year, spending_billion, visitors)

spending_stat %>%
  ggplot(aes(x = year, y = spending_billion)) +
  geom_boxplot( color = 'dark orange') +
  theme_bw() +
  geom_text(aes(label = spending_billion), fontface = "bold", size = 3.5, vjust = - 0.2) +
  labs(x = '', y = "",  
       title = 'Park visitor spending contributions in the USA (2012-2020)', caption = 'Figure 1')
  


#Figure 2 shows the Average percentage of the cities interval in Presentation

data_set2 <- data_set1 %>%
  mutate(spending_class = cut_number(spending_points, n = 3, labels = c("low", "med", "high")))

data_set2 <- data_set2 %>%
  group_by(city) %>%
  summarise_at(vars(total_percent), list(average = mean), na.rm = TRUE) %>%
  ungroup()


data_set2 <- data_set2[-c(100),]

data_set3 <- data_set2 %>%
  mutate(city_range = cut_number(average, n = 3, labels = c("low", "med", "high")))
mutate_if(is.numeric, round, digits = 2)


data_set3 %>%
  ggplot(aes(city_range, average)) +
  geom_boxplot( fill = 'green') +
  labs(x = NULL, y = "average point", 
       title = "Average points of the cities interval", caption = "Figure 2") +
  theme_bw()

#Figure 3 shows 

actual_data %>%
  ggplot(aes(spending_class, spending_points)) +
  geom_boxplot( fill = 'pink') +
  theme_bw() +
  coord_flip() +
  labs(x = '', y = "spending points",  
       title = 'Average points of the spending class', caption = 'Figure 3')
