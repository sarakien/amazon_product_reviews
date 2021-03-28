library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(DT)
library(googleVis)
library(zoo)

#Large DataSet, cleaned, mutated, and filtered
my_data = read.csv("my_data.csv")
hist(my_data2$reviews.numHelpful)
my_data3 = my_data %>% filter(.,reviews.numHelpful < 1.5)
hist(my_data3$reviews.numHelpful)

my_data2 = my_data %>% filter(.,reviews.numHelpful < 50)


#Data for Overall Review Analysis

  #Star Rating and Helpful Vote data
my_rating_data = my_data2 %>% select(.,"Star_Rating" = reviews.rating, "Helpful_Review_Votes" = reviews.numHelpful)

star_rating_data = as.data.frame(my_rating_data$Star_Rating)

helpful_data = my_rating_data %>% group_by(.,Star_Rating) %>% 
  summarise(.,sum_helpful = sum(Helpful_Review_Votes))

  #Data and objects for Positive and Negative sentiments, based on sentiment analysis 
  #found in Data Cleaning script

Sentiments = c("Positive Sentiment","Negative Sentiment")

Sentiment_num = c(9139,2067)

Sentiment_Data = as.data.frame(cbind(Sentiments, Sentiment_num))

Sentiment_Data = transform(Sentiment_Data, Sentiment_num = as.numeric(Sentiment_num))

##Choice = c(colnames(my_rating_data),"Sentiment Analysis")


#Tables and Functions for "Word Type Examples" based on sentiment analysis 
#found in Data Cleaning script

word_frequency_table <- read.csv(file = "./word_frequency_table.csv")

word_frequency_exp = word_frequency_table %>%
  filter(., word_type == "Experience") %>% 
  select(.,"Key Word" = word, Frequency = frequency, Example = example)

word_frequency_emo = word_frequency_table %>%
  filter(., word_type == "Emotion") %>% 
  select(.,"Key Word" = word, Frequency = frequency, Example = example)

word_frequency_func = word_frequency_table %>%
  filter(., word_type == "Functionality") %>% 
  select(.,"Key Word" = word, Frequency = frequency, Example = example)

#Data and objects for Word Type Review Trends

my_data_gather = gather(my_data2, key = word_type, value = sum, sum_exp, sum_emo, sum_mat)

my_data_gather = my_data_gather %>% 
  mutate(., word_type = ifelse(word_type == 'sum_exp', "Experience",
                   ifelse(word_type == "sum_emo", "Emotion",
                          "Functionality")))

my_data_gather = my_data_gather %>% 
  mutate(., alexa = ifelse(alexa == 1, "Alexa", "Not Alexa"))

##Product_Options = c("All Products","Alexa Products Only","Echo Products Only")

##Review_Options = c("Star Ratings", "Helpful Review Votes")

  #Multiple Regression Analyses for my records
alexa_data = my_data2 %>% filter(., alexa == 1) 

alexa_data2 = alexa_data %>% filter(.,reviews.numHelpful < 50)

summary(lm(alexa_data2$reviews.numHelpful ~ 
             alexa_data2$sum_emo + 
             alexa_data2$sum_mat + 
             alexa_data2$sum_exp))

summary(lm(alexa_data2$reviews.rating ~ 
             alexa_data2$sum_emo + 
             alexa_data2$sum_mat + 
             alexa_data$sum_exp))

non_alexa_data = my_data2 %>% filter(., alexa == 0) 

non_alexa_data2 = non_alexa_data %>% filter(.,reviews.numHelpful < 50)

summary(lm(non_alexa_data2$reviews.numHelpful ~ 
             non_alexa_data2$sum_emo + 
             non_alexa_data2$sum_mat + 
             non_alexa_data2$sum_exp))

summary(lm(non_alexa_data2$reviews.rating ~ 
             non_alexa_data2$sum_emo + 
             non_alexa_data2$sum_mat + 
             non_alexa_data$sum_exp))


#Data and objects for word type and google trends by year - "Annual Trends"

  #Data for word type trends 2016-2018 under "Annual Trends"

my_data_gather = my_data_gather %>% 
  mutate(., month_day_year = substr(my_data_gather$reviews.date,1,10))


my_data_gather = my_data_gather %>% 
  mutate(.,month_year = as.yearmon(month_day_year, "%Y-%m"))


  #Data for Google Trends 2016-2018 under "Annual Trends"

google_trends = read.csv("google_alexa.csv")

google_trends = google_trends %>% 
  mutate(.,week = as.Date(as.Date(google_trends$week, "%m/%d/%y")))

google_trends = google_trends %>% mutate(.,month_year = substr(week, 1, 7))

google_trends = as.data.frame(google_trends)


#Preparing Data for "Google Trends Analysis" involving relationship between
  # Word Type Trends and Google Trends -- aggregate the data, then join it, 
  # then gather word types and aggregate again

google_trends2 = google_trends %>% group_by(.,month_year) %>% 
  summarise(., mean_index = mean(search_index))

my_data_gather2 = my_data_gather %>% 
  mutate(.,month_year = substr(reviews.date, 1, 7))

my_data_gather2 = my_data_gather2 %>% 
  select(.,name,reviews.numHelpful,reviews.rating, alexa,date,word_type,sum,month_year)

exp_by_date = my_data_gather2 %>% group_by(.,month_year) %>% 
  filter(.,alexa == "Alexa" & word_type == "Experience") %>% 
  summarise(.,mean_sum_exp = mean(sum))

emo_by_date = my_data_gather2 %>% group_by(.,month_year) %>% 
  filter(.,alexa == "Alexa" & word_type == "Emotion") %>% 
  summarise(.,mean_sum_emo = mean(sum))

mat_by_date = my_data_gather2 %>% group_by(.,month_year) %>% 
  filter(.,alexa == "Alexa" & word_type == "Functionality") %>% 
  summarise(.,mean_sum_mat = mean(sum))

final_date_data = inner_join(google_trends2,exp_by_date)

final_date_data = inner_join(final_date_data,emo_by_date)

final_date_data = inner_join(final_date_data,mat_by_date)

final_date_data_gather = gather(final_date_data, key = word_type, value = mean_sum, 
                                mean_sum_exp, mean_sum_emo, mean_sum_mat)

final_date_data_gather = final_date_data_gather %>% 
  mutate(.,word_type = ifelse(word_type == "mean_sum_exp", "Experience",
                              ifelse(word_type == "mean_sum_emo", "Emotion",
                                     "Functionality")))


  #Correlation analyses for the Google Trends Analyses, for my records

cor(final_date_data$mean_index,final_date_data$mean_sum_exp)

cor(final_date_data$mean_index,final_date_data$mean_sum_emo)

cor(final_date_data$mean_index,final_date_data$mean_sum_mat)

















