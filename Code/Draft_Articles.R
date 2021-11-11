#import libraries
library(tidyverse)
library(tidytext)
library(rethinking)
library(brms)

#import the dataset
Articles <- read.csv(file = file.choose())
Articles$Year <- format(as.Date(Articles$release_date, format = "%Y-%m-%d"), "%Y")
Articles$Month <- format(as.Date(Articles$release_date, format = "%Y-%m-%d"), "%m")
Articles$day <- format(as.Date(Articles$release_date, format = "%Y-%m-%d"), "%d")

Years <- c(2014:2018) #the specific years we want for analysis

Articles <- filter(Articles, Year == Years)

#We can divide our analysis into two parts. One based on the title of the article and the other based on the
#content of the article

#divide into two dataframes 

Articles_headline <- Articles %>% select(-c("content"))
Articles_content <- Articles %>% select(-c("title"))

# tokenize
tokenized_headline <- Articles_headline %>% unnest_tokens(output = word, input = title)
tokenized_content <- Articles_content %>% unnest_tokens(output = word, input = content)

#remove stopwords

data("stop_words")

tokenized_headline <- tokenized_headline %>% anti_join(stop_words)
tokenized_content <- tokenized_content %>% anti_join(stop_words)

#Sentiment scoring for headlines only dataframe

bing <- get_sentiments("bing")
loughran <- get_sentiments("loughran")

tokenized_headline <- tokenized_headline %>% inner_join(bing)
tokenized_content <- tokenized_content %>% inner_join(bing)

tokenized_headline$score <- ifelse(tokenized_headline$sentiment == "positive", 1,
                                   ifelse(tokenized_headline$sentiment == "negative", -1, 0))
tokenized_content$score <- ifelse(tokenized_content$sentiment == "positive", 1,
                                  ifelse(tokenized_content$sentiment == "negative", -1, 0))

aggregated_headline <- tokenized_headline %>% group_by(id) %>% summarise(Score = mean(score))
aggregated_content <- tokenized_content %>% group_by(id) %>% summarise(Score = mean(score))

#combine back with Articles_headline

Articles_headline <- Articles_headline %>% inner_join(aggregated_headline, by = "id")
Articles_content <- Articles_content %>% inner_join(aggregated_content, by = "id")

#Bayesian Regression
#stocks being included for analysis

stocks <- c("AAPL", "KO", "TM", "AMZN", "JPM", "T", "MSFT", "XOM", "JNJ",
            "PFE", "GE", "MMM")

path_to_financial_data <- "C:\\Users\\hp\\Desktop\\Thesis\\Articles\\Financial Articles 2\\Financial Data"
path_to_model_folder <- "C:\\Users\\hp\\Desktop\\Thesis\\Articles\\Financial Articles 2\\Models\\Bing_MLR"

nasdaq <- read_csv(file.choose())
nasdaq <- nasdaq %>% select(Date, Close)
nasdaq$Nasdaq_Close_s <- standardize(nasdaq$Close)

nasdaq <- nasdaq %>% select(-c(Close))

for (i in 1:length(stocks)){
  
  a <- Articles_content %>% filter(ticker == stocks[i])
    price_data <- read.csv(paste(path_to_financial_data, paste0(stocks[i], ".csv"), sep = "\\"))
  price_data$Date <- as.Date(price_data$Date)
  a$release_date <- as.Date(a$release_date)
  a <- a %>% inner_join(price_data, by = c("release_date" = "Date")) %>%
    inner_join(nasdaq, by = c("release_date" = "Date"))
  a$Close_s <- standardize(a$Close) 
  model <- brm(Close_s ~ 1 + Score + Nasdaq_Close_s, data = a, family = gaussian,
                     prior = c(prior(normal(0, 0.1), class = Intercept),
                               prior(normal(0, 0.5), class = b),
                               prior(exponential(1), class = sigma)),
                     iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
               file = paste(path_to_model_folder, stocks[i], sep = "\\"))
}

