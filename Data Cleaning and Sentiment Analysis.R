
#Read and clean data, create relevant columns ####

my_data = read.csv("amazon_product_reviews.csv")

my_data = my_data %>% mutate(., alexa = ifelse(
  grepl("Alexa", name) | grepl("Echo", name), 1, 0) 
  )

my_data %>% filter(.,alexa == 1)

my_data = my_data %>% select(., dateAdded, dateUpdated, name, reviews.date, 
                             reviews.dateSeen, reviews.doRecommend, 
                             reviews.numHelpful, reviews.rating, reviews.text,
                             reviews.title, reviews.username, alexa)

my_data = my_data %>% mutate(., echo = ifelse(
  grepl("Echo", name), 1, 0) 
  )


#sentiment analysis ####

install.packages("tidytext")
library(tidytext)
install.packages("textdata")
library(textdata)
get_sentiments("nrc")

nrc_neg = get_sentiments("nrc") %>% 
  filter(sentiment == "negative")

nrc_pos = get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

my_data_unnest = my_data %>% 
  unnest_tokens(word,reviews.text)

nrc_neg = my_data_unnest %>% 
  inner_join(nrc_neg) %>% 
  count(word, sort = TRUE)

nrc_pos = my_data_unnest %>% 
  inner_join(nrc_pos) %>%
  count(word, sort = TRUE)


#Create Columns for relevant Experiential Words ####

my_data = my_data %>% mutate(., exp_fun = ifelse(
  grepl("fun", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., exp_helpful = ifelse(
  grepl("helpful", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., exp_educational = ifelse(
  grepl("education", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., exp_information = ifelse(
  grepl("information", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., exp_communicate = ifelse(
  grepl("talk", reviews.text) | 
    grepl("communicate", reviews.text) | 
    grepl("converse", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., exp_travel = ifelse(
  grepl("travel", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., exp_entertain = ifelse(
  grepl("entertain", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., exp_daily = ifelse(
  grepl("daily", reviews.text) | 
    grepl("every day", reviews.text) | 
    grepl("everyday", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., exp_home = ifelse(
  grepl("home", reviews.text) | 
    grepl("house", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., exp_morning = ifelse(
  grepl("morning", reviews.text), 1, 0)
  )

#Create Columns for relevant Emotion words ####

my_data = my_data %>% mutate(., emo_love = ifelse(
  grepl("love", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., emo_happy = ifelse(
  grepl("happy", reviews.text) | 
    grepl("glad", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., emo_enjoy = ifelse(
  grepl("enjoy", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., emo_satisfied = ifelse(
  grepl("satisfied", reviews.text) | 
    grepl("pleased", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., emo_excited = ifelse(
  grepl("excited", reviews.text), 1, 0)
  )

#Create Columns for relevant Material/Functionality words ####

my_data = my_data %>% mutate(., mat_userfriendly = ifelse(
  grepl("user friendly", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., mat_feature = ifelse(
  grepl("feature", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., mat_battery = ifelse(
  grepl("battery", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., mat_warranty = ifelse(
  grepl("warranty", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., mat_setup = ifelse(
  grepl("set up", reviews.text), 1, 0)
  )

my_data = my_data %>% mutate(., mat_durable = ifelse(
  grepl("durable", reviews.text), 1, 0)
  )

#Sum Experiential Words, create sum variable and create binary ####

x = my_data %>% select(.,c("exp_communicate","exp_fun","exp_daily", 
                           "exp_educational","exp_information",
                           "exp_entertain", "exp_helpful",
                           "exp_travel","exp_home", "exp_morning")
                       )

sum_exp=rowSums(x,na.rm=TRUE)

my_data$sum_exp = sum_exp

my_data = mutate(my_data, sum_exp_bin = ifelse(my_data$sum_exp > 0, 1, 0)
                 )


#Sum Emotion Words, create sum variable and create binary ####

emo_words = my_data %>% select(.,c("emo_love","emo_happy","emo_enjoy",
                                   "emo_satisfied", "emo_excited")
                               )

sum_emo=rowSums(emo_words,na.rm=TRUE)

my_data$sum_emo = sum_emo

my_data = mutate(my_data, sum_emo_bin = ifelse(my_data$sum_emo > 0, 1, 0)
                 )


#Sum Material/Functionality Words, create sum variable and create binary variable ####

mat_words = my_data %>% select(.,c("mat_userfriendly","mat_feature","mat_battery",
                                   "mat_warranty", "mat_setup","mat_durable")
                               )

sum_mat=rowSums(mat_words,na.rm=TRUE)

my_data$sum_mat = sum_mat

my_data = mutate(my_data, sum_mat_bin = ifelse(my_data$sum_mat > 0, 1, 0)
                 )


#write data to csv file ####

write.csv(my_data,file = "my_data.csv")

