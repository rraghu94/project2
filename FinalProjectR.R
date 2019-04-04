#sentiment analysis
read.csv("C:/Users/rohit/Desktop/southwest2.csv")
install.packages("tidytext")


#with some research, came up with this solution using tidytext
library(readr)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(dplyr)
#read in the text file 
swa <- read_file("C:/Users/rohit/Desktop/lilia.txt")
#use the built in glue function 
fileText <- glue(read_file(swa))
fileText <- gsub("\\$", "", fileText)
#tokenize the text
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
#use inner join and separate words into positive and negative words 
tokens %>% inner_join(get_sentiments("bing")) %>% count(sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative)



#Hypothesis testing

#Ho = neg_words = pos_words
#Ha = pos_words > neg_words

#positive words = 20
#negative words = 8

#since positive words are greater than negative words, we can suggest that southwest reacted positively to users on twitter after the flight1380 incident.