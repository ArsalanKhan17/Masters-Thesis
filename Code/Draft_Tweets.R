#import essential libraries
library(tidytext)
library(tidyverse)
library(tm)
library(readr)
library(tidyr)

#import the tweets data set for each stock

#stocks included in the analysis [Apple, Coca cola, Toyota, Amazon, JP Morgan, Berkshire Hathaway,
#Facebook, AT&T, Microsoft, Exxon Mobile, BP, JnJ, Pfizer, General Electric, 3M]

#path to Master tweets folder
path <- "C:/Users/hp/Desktop/Thesis/Tweets/stocknet-dataset/tweet/raw"
stocks <- c("AAPL", "KO", "TM", "AMZN", "JPM", "BRK-A", "FB", "T", "MSFT", "XOM", "BP", "JNJ",
            "PFE", "GE", "MMM")

#loop through each stock
datasets <- list()

for (stock in stocks){
  
  x <- read_csv(paste(path, stock, paste0(stock,".csv"), sep = "/"))
  #x <- read.csv(paste(path, stock, stock, sep = "/"))
  
  #add to list
  datasets[[stock]] <- x[, -1]
}


#Cleaning the tweets
#remove hyperlinks and numbers from tweets
i <- 0
for (data in datasets){
  i <- i + 1
  data$Cleaned_text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "",
                                       data$text)
  
  data$Cleaned_text <- gsub("[^[:alnum:]]", " ", data$Cleaned_text)
  
  data$Cleaned_text <- gsub("[0-9]*", "", data$Cleaned_text) #since numbers can't be scored for sentiment we thus remove them
  
  datasets[[i]] <- data
  
}


#tokenize the tweets
tokenized <- list()

i <- 0
for (data in datasets){
  i <- i + 1
  x <- data %>% unnest_tokens(output = word, input = Cleaned_text)
  
  tokenized[[stocks[i]]] <- x
}


#remove stopwords
data("stop_words")

i <- 0
for (data in tokenized){
  i <- i + 1
  x <- data %>% anti_join(stop_words)  
  tokenized[[i]] <- x
}

#get sentiment libraries
bing <- get_sentiments("bing") #general binary sentiment scoring dictionary
loughran <- get_sentiments("loughran") #binary sentiment scoring dictionary purpose built for financial articles/tweets

#sentiment scoring
i <- 0
bing_sentiment <- list() #list for sentiment scoring based on bing dictionary
loughran_sentiment <- list() #list for sentiment scoring based on loughran dictionary
for (data in tokenized){

  i <- i + 1
  
  x.bing <- data %>% left_join(bing, by = "word")
  x.bing$sentiment <- replace_na(x.bing$sentiment, "neutral")
  
  x.bing$score <- ifelse(x.bing$sentiment == "positive", 1, ifelse(x.bing$sentiment == "negative", -1, 0))  
  bing_sentiment[[stocks[i]]] <- x.bing
  
  x.loughran <- data %>% left_join(loughran, by = "word")
  x.loughran$sentiment <- replace_na(x.loughran$sentiment, "neutral")
  x.loughran$score <- ifelse(x.loughran$sentiment == "positive", 1, ifelse(x.loughran$sentiment == "negative", -1, 0))
  loughran_sentiment[[stocks[i]]] <- x.loughran
}


#group by tweet id

aggregated_bing <- list()
aggregated_loughran <- list()
for (i in 1:length(stocks)){
  
  x.bing <- bing_sentiment[[i]] %>% group_by(id) %>% summarise(Score = mean(score))  
  
  aggregated_bing[[stocks[i]]] <- x.bing
  
  x.loughran <- loughran_sentiment[[i]] %>% group_by(id) %>% summarise(Score = mean(score))
  
  aggregated_loughran[[stocks[i]]] <- x.loughran
}


#combine aggregated score with Tweets_combined dataframe
datasets_bing <- list()
datasets_loughran <- list()

saveRDS(datasets_bing, file = "datasets_bing")
saveRDS(datasets_loughran, file = "datasets_loughran")
for (i in 1:length(stocks)){
  datasets_bing[[stocks[i]]] <- datasets[[i]] %>% inner_join(aggregated_bing[[i]], by = "id")
  datasets_loughran[[stocks[i]]] <- datasets[[i]] %>% inner_join(aggregated_loughran[[i]], by = "id")
}

#fix date format
for (i in 1:length(stocks)){
  datasets_bing[[i]]$created_at <- as.Date(datasets_bing[[i]]$created_at, format = "%a %b %d %H:%M:%S +0000 %Y")
  datasets_bing[[i]] <- datasets_bing[[i]] %>%
    mutate(Year = format(as.Date(created_at, format = "%Y-%m-%d"), "%Y")) %>%
    mutate(Month = format(as.Date(created_at, format = "%Y-%m-%d"), "%m")) %>%
    mutate(Day = format(as.Date(created_at, format = "%Y-%m-%d"), "%d"))
  
  datasets_loughran[[i]]$created_at <- as.Date(datasets_loughran[[i]]$created_at, format = "%a %b %d %H:%M:%S +0000 %Y")
  datasets_loughran[[i]] <- datasets_loughran[[i]] %>%
    mutate(Year = format(as.Date(created_at, format = "%Y-%m-%d"), "%Y")) %>%
    mutate(Month = format(as.Date(created_at, format = "%Y-%m-%d"), "%m")) %>%
    mutate(Day = format(as.Date(created_at, format = "%Y-%m-%d"), "%d"))
  
}

#Bayesian regression
library(brms)
library(rethinking)
#Running the models
#path for saving models
path_for_models <- "C:\\Users\\hp\\Desktop\\Thesis\\Tweets\\stocknet-dataset\\Models\\Loughran_MLR"
path_to_financial_data <- "C:\\Users\\hp\\Desktop\\Thesis\\Tweets\\stocknet-dataset\\price\\raw"
nasdaq <- read.csv(file.choose())
nasdaq <- select(nasdaq, Date, Close)
nasdaq$Nasdaq_Close_s <- standardize(nasdaq$Close)
nasdaq <- select(nasdaq, -c("Close"))
nasdaq$Date <- as.Date(nasdaq$Date)

for (i in 1:length(stocks)){
  a <- datasets_loughran[[stocks[i]]]
  a$sentiment <- ifelse(a$Score == 0, "neutral", ifelse(a$Score > 0, "positive", "negative"))
  a <- a %>% group_by(created_at, sentiment) %>% summarise(count = n()) 
  
  #load financial data
  stock_data <- read.csv(paste(path_to_financial_data, paste0(stocks[i], ".csv"), sep = "\\"))
  
  stock_data <- stock_data %>% select(Date, Close)
  stock_data$Date <- as.Date(stock_data$Date, format = "%Y-%m-%d")
  
  combined <- a %>% inner_join(stock_data, c("created_at" = "Date")) %>%
    inner_join(nasdaq, c("created_at" = "Date"))
  
  combined <- combined %>% filter(sentiment != "neutral") %>% 
    group_by(created_at) %>%
    mutate(total = sum(count))
  combined <- combined %>% mutate(proportion = count / total)
  combined <- combined %>% mutate(positive = ifelse(sentiment == "positive", proportion, 1 - proportion))
  combined <- combined[!duplicated(combined$created_at), ]
  
  combined$Close_s <- standardize(combined$Close)
  
  print(paste("running", stock[i], "model", sep = " "))
  my_model <- brm(Close_s ~ 1 + positive + Nasdaq_Close_s, data = combined, family = gaussian(),
                  prior = c(prior(normal(0, 0.1), class = Intercept),
                            prior(normal(0, 0.5), class = b),
                            prior(exponential(1), class = sigma)),
                  iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                  file = paste(path_for_models, stocks[i], sep = "\\"))
  
}

#View(posterior_summary(my_model))
