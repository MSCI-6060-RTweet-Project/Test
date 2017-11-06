#################################################################################################
#################################################################################################
# 
#Programming in R
#Fall 2017
#Sam Burer
#
#Group Project
#Group Members: Lee Goodlove, Michael Coyle, Matt Knabel, Austin Cappaert & Reed Harris
#
#This script contains all the code that we used to get our results for the group project.
#
#################################################################################################
#################################################################################################
# Load the necessary libraries. Some we have libraries you might have to install.
#################################################################################################
library(ggraph)
library(igraph)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rtweet)
library(openssl)
library(httr)
library(widyr)
library(scales)
library(reshape2)
library(lubridate)
library(magrittr)  # Package needed for %>% to work correctly.
library(stringi)   # Package needed for stri_detect_regex function.
library(stringr)   # Package needed for functions str_detect and str_replace_all.  This package is built on top of package stringi.
library(translateR)
#################################################################################################
rm(list = ls())
#################################################################################################
## whatever name you assigned to your created app
# appname <- "rtweet_deere"
# 
# ## api key (example below is not a real key)
# key <- "PYZkll7T1peMIxh0TbPRDLHrf"
# 
# ## api secret (example below is not a real key)
# secret <- "ugKrLGVcsmxBAxaQKwOBtvIueOF166sZpRrJnkSf1v2WdxdWy5"
# 
# ## create token named "twitter_token"
# twitter_token <- create_token(
#     app = appname,
#     consumer_key = key,
#     consumer_secret = secret,
#     cache = TRUE)

#################################################################################################
#consumerKey <- "PYZkll7T1peMIxh0TbPRDLHrf"
#consumerSecret <- "ugKrLGVcsmxBAxaQKwOBtvIueOF166sZpRrJnkSf1v2WdxdWy5"
#accessToken <- "254240036-p2qPxUOp0J1DqiNvhjvviHkm78cqmHBzOwevXIgd"
#accessTokenSecret <- "tu8WB3tFZWW332d8aW42FWYKDebpO09Qap5YXx5hLEm9n"
#setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
#################################################################################################
#################################################################################################
# Unique Words Script
#################################################################################################
#################################################################################################

#lets go get those tweets
#deerestats <- search_tweets("Deere", n = 10000, include_rts = FALSE)
#replacing with static dataset - deerestatsM.csv
df_source_lang <- read.csv("deerestatM.csv", stringsAsFactors = FALSE)

#adding column as a placeholder for translated data
df_source_lang$translatedContent <- NA

#bar chart of top languages to determine top 6
df_minus_en <- subset(df_source_lang, df_source_lang$lang != "en")
df_minus_und <- subset(df_minus_en, df_minus_en$lang != "und")
lang_plot <- qplot(lang, data = df_minus_und, geom = "bar") + ggtitle("Tweets By Language") + labs(y = "Number of Tweets", x = "Language")
ggsave(filename = "Top Languages Plot.png", plot = lang_plot, width = 6, height = 4)

#subset each langauge into data frame and then translate the stripped text column in each data frame
#data is translated using translateR and Google Translate API
df_fr <- subset(df_source_lang, df_source_lang$lang == "fr")
df_fr <- translate(dataset = df_fr,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "fr", target.lang = "en")

#subset each langauge into data frame and then translate the stripped text column in each data frame
#data is translated using translateR and Google Translate API
df_es <- subset(df_source_lang, df_source_lang$lang == "es")
df_es <- translate(dataset = df_es,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "es", target.lang = "en")

#rbind the previous datafram with next language
df_rbind <- rbind(df_es, df_fr)

#subset each langauge into data frame and then translate the stripped text column in each data frame
#data is translated using translateR and Google Translate API
df_pt <- subset(df_source_lang, df_source_lang$lang == "pt")
df_pt <- translate(dataset = df_pt,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "pt", target.lang = "en")

#rbind the previous datafram with next language
df_rbind <- rbind(df_pt, df_rbind)

#subset each langauge into data frame and then translate the stripped text column in each data frame
#data is translated using translateR and Google Translate API
df_nl <- subset(df_source_lang, df_source_lang$lang == "nl")
df_nl <- translate(dataset = df_nl,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "nl", target.lang = "en")

#rbind the previous datafram with next language
df_rbind <- rbind(df_nl, df_rbind)

#subset each langauge into data frame and then translate the stripped text column in each data frame
#data is translated using translateR and Google Translate API
df_ja <- subset(df_source_lang, df_source_lang$lang == "ja")
df_ja <- translate(dataset = df_ja,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "ja", target.lang = "en")

#rbind the previous datafram with next language
df_rbind <- rbind(df_ja, df_rbind)

#subset each langauge into data frame and then translate the stripped text column in each data frame
#data is translated using translateR and Google Translate API
df_de <- subset(df_source_lang, df_source_lang$lang == "de")
df_de <- translate(dataset = df_de,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "de", target.lang = "en")

#rbind the previous datafram with next language
df_rbind <- rbind(df_de, df_rbind)

#replace the stripped text column with translated content
df_rbind$stripped_text <- df_rbind$translatedContent

#create data frame with subset of languages NOT translated
df_exclude_lang <- subset(df_source_lang, df_source_lang$lang != "de")
df_exclude_lang <- subset(df_exclude_lang, df_exclude_lang$lang != "nl")
df_exclude_lang <- subset(df_exclude_lang, df_exclude_lang$lang != "es")
df_exclude_lang <- subset(df_exclude_lang, df_exclude_lang$lang != "fr")
df_exclude_lang <- subset(df_exclude_lang, df_exclude_lang$lang != "ja")
df_exclude_lang <- subset(df_exclude_lang, df_exclude_lang$lang != "pt")

#rbind the translated data frame with the not translated data frame
df_translated <- rbind(df_exclude_lang, df_rbind)

#remove the translated conent column
df_translated$translatedContent <- NULL

#create deerestatsNew data frame using the translated data frame
deerestatsNew <- df_translated

#hyperlinks aren't works so lets get rid of them
# deerestatsNew$stripped_text <- gsub("http.*","",  deerestatsNew$text)
# deerestatsNew$stripped_text <- gsub("https.*","", deerestatsNew$stripped_text)
#strip and clean
deerestats_cleanNew <- deerestatsNew %>%
     dplyr::select(stripped_text) %>%
     unnest_tokens(word,stripped_text)

#View(deerestats_cleanNew)

# plot the top words
deerestats_cleanNew %>%
    count(word, sort=TRUE) %>%
    top_n(60) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(x="Count",
         y="Unique words",
         title="Count of unique words found in tweets")

#look at the data

#john and deere are going to appear a lot...duh... get rid of them
deerestats_cleanNew <- subset(deerestats_cleanNew, word != "deere")
deerestats_cleanNew <- subset(deerestats_cleanNew, word != "john")

#make stop_words
data("stop_words")
cleaned_tweet_wordsNew <- deerestats_cleanNew %>%
    anti_join(stop_words)

#lets look again.
cleaned_tweet_wordsNew %>%
    count(word, sort=TRUE) %>%
    top_n(60) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(x="Count",
         y="Unique words",
         title="Count of unique words found in tweets")

#lets try wayyyy more tweets and retweets
#deerestats50 <- search_tweets("Deere", n = 50000, include_rts = TRUE, retryonratelimit = TRUE)

#lets make a word network!
#this will let us see the relations words have to each other in pairs

#widyr library is needed for this
#library(widyr)

#make deerestats2
deerestats2 <- deerestatsNew


#remove punctuations and convert to lowercase, also add id for tweets
deere_tweets_paired_words <- deerestats2 %>% 
    dplyr::select(stripped_text) %>% unnest_tokens(paired_words, stripped_text, token = "ngrams", n=2)


#count the number of paired words
deere_tweets_paired_words %>% count(paired_words, sort = TRUE)

#pair the words together
deere_tweets_separated_words <- deere_tweets_paired_words %>%
    separate(paired_words, c("word1", "word2"), sep = " ")
#****can't get this part working, something to do with it being atomic I believe
#replace John with Deere as they will probably almost always be pairs
#levels(deere_tweets_separated_words$word1)[levels(deere_tweets_separated_words$word1) == "john"] <- "deere"
#wh <- levels(deere_tweets_separated_words$word1) == "john"
#levels(deere_tweets_separated_words$word1)[wh] <- "deere"
#*******

#filter by the words
deere_tweets_filtered <- deere_tweets_separated_words %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

#new bigrams
deere_words_counts <- deere_tweets_filtered %>% count(word1, word2, sort = TRUE)

#plot graph
deere_words_counts %>%
    filter(n >= 24) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
    geom_node_point(color = "green", size = 2) +
    geom_node_text(aes(label = name), vjust = 1.8, size=2) +
    labs(title= "Word Network: Tweets having the word deere",
         subtitle="Text mining twitter data ",
         x="", y="")

#the following users are known "bots"
# bots <- c("Deere_w13","RodriguezDaGod","Deere_k","John_F_Deere") 

#botremove function
#meant to remove "bots" or users who have chosen Deere in screen_name
#description - removes screen_name and replies to that screen_name

# botremove <- function(bot, dataframe){
#     dataframe <- subset(deerestatM, deerestatM$screen_name != bot)
#     dataframe <- subset(deerestatM, deerestatM$in_reply_to_status_screen_name != bot)
# }
# 
# botremove(bot = "Deere_w13", dataframe = "deerestatsM1")

#################################################################################################
#################################################################################################
# Word Map Script
#################################################################################################
#################################################################################################

#Read in Data Set
#deerestatsNew <- read.csv("deerestatM.csv", stringsAsFactors = FALSE)
#deerestatsNew <- search_tweets("Deere", n = 10000, include_rts = FALSE)


#hyperlinks aren't works so lets get rid of them
# deerestatsNew$stripped_text <- gsub("http.*","",  deerestatsNew$text)
# deerestatsNew$stripped_text <- gsub("https.*","", deerestatsNew$stripped_text)
# 
# strip and clean
 deerestats_cleanNew <- deerestatsNew %>%
     dplyr::select(stripped_text) %>%
     unnest_tokens(word,stripped_text)



#john and deere are going to appear a lot...duh... get rid of them
#deerestats_cleanNew <- subset(deerestats_cleanNew, word != "deere")
#deerestats_cleanNew <- subset(deerestats_cleanNew, word != "john")

#make stop_words
data("stop_words")
cleaned_tweet_wordsNew <- deerestats_cleanNew %>%
    anti_join(stop_words)

#remove punctuations and convert to lowercase, also add id for tweets
deere_tweets_paired_words <- deerestatsNew %>% 
    dplyr::select(stripped_text) %>% unnest_tokens(paired_words, stripped_text, token = "ngrams", n=2)

#count the number of paired words
deere_tweets_paired_words %>% count(paired_words, sort = TRUE)

#pair the words together
deere_tweets_separated_words <- deere_tweets_paired_words %>%
    separate(paired_words, c("word1", "word2"), sep = " ")

#John and Deere are going to be in sequence alot, obviously. This will change all occurences of the word 'John' to 'Deere'
deere_tweets_separated_words$word1 <- gsub(pattern = "john", "deere", deere_tweets_separated_words$word1)
deere_tweets_separated_words$word2 <- gsub(pattern = "john", "deere", deere_tweets_separated_words$word2)


#filter by the words
deere_tweets_filtered <- deere_tweets_separated_words %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

#new bigrams
deere_words_counts <- deere_tweets_filtered %>% count(word1, word2, sort = TRUE)

#plot graph
deere_words_counts %>%
    filter(n >= 50) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
    geom_node_point(color = "green", size = 2) +
    geom_node_text(aes(label = name), vjust = 1.8, size=2) +
    labs(title= "Word Network: Tweets having the word deere",
         subtitle="Text mining twitter data ", x="", y="")

ggsave(filename = "Deere Word Network_03Nov_After.png", width = 6, height = 4)

#################################################################################################
#################################################################################################
# Hashtag Script
#################################################################################################
#################################################################################################
# This code reads the older dataset                                                             #
#################################################################################################

tweet <- read.csv("deerestats.csv", stringsAsFactors = FALSE)

tweet <- subset(tweet,tweet$hashtags != "NA") #Remove the NAs

tweet <- subset(tweet,select = "hashtags") #Just the Hashtag column

tweet <- strsplit(as.character(tolower(tweet$hashtags)),' ') #Breakout multiple hashtags in a single tweet

tweet <- data.frame(unlist(tweet),stringsAsFactors=FALSE) #Flattening list and inserting into a dataframe

tweet %>% #Graph for top 30 unique hashtags
    count(unlist.tweet., sort=TRUE) %>%
    top_n(30) %>%
    mutate(unlist.tweet. = reorder(unlist.tweet., n)) %>%
    ggplot(aes(x = unlist.tweet., y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(x="Unique Hashtags",y="Count",title="Count of Hashtags Found in Tweets 9/12/17-9/22/17")

ggsave(filename = "UniqueHashtagsOld.png", plot = last_plot(), width = 6, height = 4, dpi = 600)

#################################################################################################
# This code reads the newer dataset                                                             #
#################################################################################################

tweet1 <- read.csv("deerestatsNew.csv", stringsAsFactors = FALSE)

tweet1 <- subset(tweet1,tweet1$hashtags != "NA") #Remove the NAs

tweet1 <- subset(tweet1,select = "hashtags") #Just the Hashtag column

tweet1 <- strsplit(as.character(tolower(tweet1$hashtags)),' ') #Breakout multiple hashtags in a single tweet

tweet1 <- data.frame(unlist(tweet1),stringsAsFactors=FALSE) #Flattening list and inserting into a dataframe

tweet1 %>% #Graph for top 30 unique hashtags
    count(unlist.tweet1., sort=TRUE) %>%
    top_n(30) %>%
    mutate(unlist.tweet1. = reorder(unlist.tweet1., n)) %>%
    ggplot(aes(x = unlist.tweet1., y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(x="Unique Hashtags",y="Count",title="Count of Hashtags Found in Tweets 10/15/17-10/24/17")

ggsave(filename = "UniqueHashtagsNew.png", plot = last_plot(), width = 6, height = 4, dpi = 600)

#################################################################################################
# This code reads both dataset together                                                         #
#################################################################################################

tweet2 <- read.csv("deerestatM.csv", stringsAsFactors = FALSE)

tweet2 <- subset(tweet2,tweet2$hashtags != "NA")

tweet2 <- subset(tweet2,select = "hashtags")

tweet2 <- strsplit(as.character(tolower(tweet2$hashtags)),' ')

tweet2 <- data.frame(unlist(tweet2),stringsAsFactors=FALSE)

tweet2 %>% #Graph for top 30 unique hashtags
    count(unlist.tweet2., sort=TRUE) %>%
    top_n(40) %>%
    mutate(unlist.tweet2. = reorder(unlist.tweet2., n)) %>%
    ggplot(aes(x = unlist.tweet2., y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(x="Unique Hashtags",y="Count",title="Count of Hashtags Found in Tweets 9/12/17-10/24/17")

ggsave(filename = "UniqueHashtagsCombined.png", plot = last_plot(), width = 6, height = 4, dpi = 600)

#################################################################################################
#################################################################################################
# Sentiment & Time Analysis Script
#################################################################################################
#################################################################################################

# Read the saved tweet csv file.
#deerestatsNew <- read.csv("deerestatM.csv", stringsAsFactors = FALSE)

# Load stop_words dataset, typically extremely common words such as 
# "the", "of", "to", and so forth
data(stop_words)

# Break the text into individual tokens, remove unwanted text and remove stop words 
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
deerestatsNew$text <- as.character(deerestatsNew$text)
tidy_tweets <- deerestatsNew %>%
    filter(!str_detect(text, "^RT")) %>%
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
    filter(!word %in% stop_words$word, 
           str_detect(word, "[a-z]"))

# ===============================
# Perform sentiment analysis
# ===============================
# Count the most common positive words in the tweets
nrcpositive <- get_sentiments("nrc") %>% 
    filter(sentiment == "positive")

positive_words <- tidy_tweets %>%
    inner_join(nrcpositive) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common positive words in the tweets
mcp <- positive_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    labs(y="Count",
         x="Positive Words",
         title="The 30 Most Common Positive Words in Tweets")

# Save the plot of the 30 most common positive words in tweets
ggsave(filename = "common_pos_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Count the most common joy words in the tweets
nrcjoy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")

joy_words <- tidy_tweets %>%
    inner_join(nrcjoy) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common joy words in the tweets
mcp <- joy_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    labs(y="Count",
         x="Joy Words",
         title="The 30 Most Common Joy Words in Tweets")

# Save the plot of the 30 most common joy words in tweets
ggsave(filename = "common_joy_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Count the most common trust words in the tweets
nrctrust <- get_sentiments("nrc") %>% 
    filter(sentiment == "trust")

trust_words <- tidy_tweets %>%
    inner_join(nrctrust) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common trust words in the tweets
mcp <- trust_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    labs(y="Count",
         x="Trust Words",
         title="The 30 Most Common Trust Words in Tweets")

# Save the plot of the 30 most common trust words in tweets
ggsave(filename = "common_trust_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Count the most common negative words in the tweets
nrcnegative <- get_sentiments("nrc") %>%
    filter(sentiment == "negative")

negative_words <- tidy_tweets %>%
    inner_join(nrcnegative) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common negative words in the tweets
mcp <- negative_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    labs(y="Count",
         x="Negative Words",
         title="The 30 Most Common Negative Words in Tweets")

# Save the plot of the 30 most common negative words in tweets
ggsave(filename = "common_neg_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Count the most common disgust words in the tweets
nrcdisgust <- get_sentiments("nrc") %>%
    filter(sentiment == "disgust")

disgust_words <- tidy_tweets %>%
    inner_join(nrcdisgust) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common disgust words in the tweets
mcp <- disgust_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    labs(y="Count",
         x="Disgust Words",
         title="The 30 Most Common Disgust Words in Tweets")

# Save the plot of the 30 most common disgust words in tweets
ggsave(filename = "common_disgust_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Count the most common anger words in the tweets
nrcanger <- get_sentiments("nrc") %>%
    filter(sentiment == "anger")

anger_words <- tidy_tweets %>%
    inner_join(nrcanger) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common anger words in the tweets
mcp <- anger_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    labs(y="Count",
         x="Anger Words",
         title="The 30 Most Common Anger Words in Tweets")

# Save the plot of the 30 most common anger words in tweets
ggsave(filename = "common_anger_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)
# =========================
# End of sentiment analysis
# =========================

# Get min created_at date and max created_at date of the data
mindate <- min(deerestatsNew$created_at)
mindate <- substr(mindate, 1, 10)
maxdate <- max(deerestatsNew$created_at)
maxdate <- substr(maxdate, 1, 10)

# Count source of tweets
source_count <- as.data.frame(table(deerestatsNew$source))

# Rename source_count columns
names(source_count)[names(source_count) == "Var1"] <- "tweet_source"
names(source_count)[names(source_count) == "Freq"] <- "n"

# Plot a bar graph of the top 30 sources of deere tweets
mcp <- source_count %>%
    top_n(30) %>%
    mutate(tweet_source = reorder(tweet_source, n)) %>%
    ggplot(aes(x = tweet_source, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    theme(axis.title = element_text(size = 8)) +                  
    theme(axis.text = element_text(size = 6)) +                   
    labs(y="Count",
         x="Source of Tweets",
         title="Top 30 Sources of Tweets")

# Save the plot of the top 30 sources of deere tweets
ggsave(filename = "tweet_source.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Coerce created_at from character to POSIXct class
deerestatsNew$created_at <- as.POSIXct(deerestatsNew$created_at)

# Break records into 4 hour time blocks based on created_at values
# Add time_block column to store the time block of the tweets
deerestatsNew$time_block <- NA

i <- 0
for(i in 1:nrow(deerestatsNew)){
    if(substr(deerestatsNew$created_at[i], 12,19) >= "00:00:00" & substr(deerestatsNew$created_at[i], 12,19) < "04:00:00"){
        deerestatsNew$time_block[i] <- 1
    } else{
        if(substr(deerestatsNew$created_at[i], 12,19) >= "04:00:00" & substr(deerestatsNew$created_at[i], 12,19) < "08:00:00"){
            deerestatsNew$time_block[i] <- 2
        }else{
            if(substr(deerestatsNew$created_at[i], 12,19) >= "08:00:00" & substr(deerestatsNew$created_at[i], 12,19) < "12:00:00"){
                deerestatsNew$time_block[i] <- 3
            }else{
                if(substr(deerestatsNew$created_at[i], 12,19) >= "12:00:00" & substr(deerestatsNew$created_at[i], 12,19) < "16:00:00"){
                    deerestatsNew$time_block[i] <- 4
                }else{
                    if(substr(deerestatsNew$created_at[i], 12,19) >= "16:00:00" & substr(deerestatsNew$created_at[i], 12,19) < "20:00:00"){
                        deerestatsNew$time_block[i] <- 5
                    }else{
                        deerestatsNew$time_block[i] <- 6
                    }
                }
            }
        }
    }
    i <- i + 1
}

# Coerce time_block to factor
deerestatsNew$time_block <- as.factor(deerestatsNew$time_block)

# Change levels of time_block
levels(deerestatsNew$time_block) <- c("12:00 AM - 3:59 AM", "4:00 AM - 7:59 AM", "8:00 AM - 11:59 AM",
                             "12:00 PM - 3:59 PM", "4:00 PM - 7:59 PM", "8:00 PM - 11:59 PM")

# Plot the time blocks of the tweets
mcp <- qplot(time_block, data = deerestatsNew, geom = "bar", fill = time_block)
mcp <- mcp + scale_fill_brewer(palette = "Set1")
plot_title <- paste("Time of Day Tweets Were Created", "(", mindate, "to", maxdate, ")", sep = " ")
mcp <- mcp + ggtitle(plot_title)                                          # Set plot title
mcp <- mcp + theme(plot.title = element_text(size = 10, face = "bold"))   # Change plot title theme 
mcp <- mcp + theme(axis.title = element_text(size = 8))                   # Change both axis' title font size
mcp <- mcp + theme(axis.text = element_text(size = 6))                    # Change both axis' text font size
mcp <- mcp + theme(axis.title.x = element_blank())                        # Remove x axis label
mcp <- mcp + ylab("Tweet Count")                                          # Set y axis label
mcp <- mcp + theme(legend.position = "none")                              # Remove legend

# Save plot of the time block of the tweets
ggsave(filename = "tweet_time_block.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Add day_tweeted column to indicate the day of the week of the tweet
deerestatsNew$day_tweeted <- as.factor(weekdays(deerestatsNew$created_at))

# Reorder levels of day_tweeted
deerestatsNew$day_tweeted <- factor(deerestatsNew$day_tweeted, levels = c("Sunday", "Monday",
                                                        "Tuesday", "Wednesday",
                                                        "Thursday", "Friday",
                                                        "Saturday"))

# Function to take a date range supplied by the user and plot a bar graph of the count of tweets 
# for the days of the week in that date range
tweets.per.day <- function(start_date = mindate, end_date = maxdate){
    # Subset data to date range specified by the user
    daterange_tweets <- subset(deerestatsNew, date(deerestatsNew$created_at) >= start_date & date(deerestatsNew$created_at) <= end_date)
    
    # Plot tweets per day of the week.
    mcp <- qplot(day_tweeted, data = daterange_tweets, geom = "bar", fill = day_tweeted)
    mcp <- mcp + scale_fill_brewer(palette = "Set2")
    plot_title <- paste("Tweets per Day of the Week", "(", start_date, "to", end_date, ")", sep = " ")
    mcp <- mcp + ggtitle(plot_title)                                          # Set plot title
    mcp <- mcp + theme(plot.title = element_text(size = 10, face = "bold"))   # Change plot title theme 
    mcp <- mcp + theme(axis.title = element_text(size = 8))                   # Change both axis' title font size
    mcp <- mcp + theme(axis.text = element_text(size = 6))                    # Change both axis' text font size
    mcp <- mcp + theme(axis.title.x = element_blank())                        # Remove x axis label
    mcp <- mcp + ylab("Tweet Count")                                          # Set y axis label
    mcp <- mcp + theme(legend.position = "none")                              # Remove legend
    
    # Save tweets per day of the week plot
    ggsave(filename = "tweets_per_day.png", plot = mcp, width = 6, height = 4,
           dpi = 600)   
    
    # Return the plot object
    return = mcp
}

