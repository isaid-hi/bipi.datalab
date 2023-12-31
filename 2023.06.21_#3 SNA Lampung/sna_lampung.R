library(igraph)
library(tidyverse)
library(rtweet)
library(tidytext)
library(wordcloud2)

## auth
source("config.R")
auth <- rtweet_app(tweet_bearer_token)

## search tweet
#df_tw <- search_tweets("lampung", token = auth, n = 500000, since = "2023-04-06", lang="id")
df_tw3 <- search_tweets("lampung", token = auth, n = 500000, since_id = "1655462846938103809", lang="id")
## mention_screen_name
mention_uname <- vector()
for (i in 1:nrow(df_tw)) {
  mention_uname[i] <- df_tw[[7]][[i]][["user_mentions"]][["screen_name"]]
}

## user data
user.df_tw <- users_data(df_tw)

## dataframe for igraph
sna.data <- data.frame(
  screen_name = user.df_tw$screen_name,
  mention_screen_name = mention_uname,
  retweet = df_tw$retweet_count,
  text = df_tw$text
)

## filter retweet and graph
sna.data %>%
  select(screen_name, mention_screen_name, text) %>%
  unnest(mention_screen_name) %>%
  filter(!is.na(mention_screen_name)) -> sna.lampung

## cleaning
sna.lampung %>%
  mutate(text = gsub(pattern = "http\\S+", 
                     replacement = "", 
                     x = text)) %>% 
  mutate(text = gsub(pattern = "#", 
                     replacement = "", 
                     x = text)) %>% 
  mutate(text = gsub(pattern = "\\d+",
                     replacement = "",
                     x = text)) %>% 
  mutate(text = gsub(pattern = "@", 
                     replacement = "", 
                     x = text)) %>% 
  plain_tweets() -> sna.lampung.cleaned

## stopwords
stopwords <- read.csv("2023.06.21_#3 SNA Lampung/stopwords_id.csv")
  ### stopwords[nrow(stopwords)+1,1] <- ""

# create token and apply stopwords
sna.lampung.cleaned %>%   
  unnest_tokens(input = text, output = token) %>% 
  count(token, sort = T)

## visual
sna.lampung.cleaned %>% 
  unnest_tokens(input = text, output = token) %>% 
  anti_join(stopwords, by = c("token" = "stopwords")) %>% 
  count(token, sort = T) %>% 
  wordcloud2(size = 0.5, rotateRatio = 0)

sna.lampung.cleaned %>%   
  unnest_tokens(input = text, output = token) %>% 
  anti_join(stopwords, by = c("token" = "stopwords")) %>% 
  count(token, sort = T) %>%
  top_n(100) %>%
  mutate(token = reorder(token, n)) %>%
  ggplot(aes(x = token, y = n)) +
  geom_col(fill="black") +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Kata Yang Paling Banyak Muncul Di Tweet",
       subtitle = "Setelah Stop Words Dihilangkan")

## file sentiment
sentiment_value  <- read.csv("https://raw.githubusercontent.com/garnesian/Analisis-Sentimen-Twitter-dan-YouTube/main/Sentiment_Value.csv", header = TRUE, sep = ";")

## sum up all the sentiment values for each comment
sna.lampung.cleaned$row_num <- seq.int(nrow(sna.lampung.cleaned))

## ngram
text_sentiment <- sna.lampung.cleaned %>%
  unnest_tokens(word, text, token = "ngrams", n = 1)

## gabungkan text_sentiment dengan sentiment_value
text_sentiment_2 <- text_sentiment %>%
  inner_join(sentiment_value) %>%
  group_by(word)

text_sentiment_3 <- text_sentiment_2 %>%
  group_by(row_num) %>%
  summarise(sentiment = sum(Polarity))

# collapse back all together by row_number
sentiment_all <- text_sentiment_3 %>% 
  full_join(sna.lampung.cleaned, by="row_num") %>%
  group_by(row_num)

## graph
sna.lampung.igraph <- graph_from_data_frame(sentiment_all[, c(3, 4)])

# Add sentiment score as edge attribute
E(sna.lampung.igraph)$sentiment <- sentiment_all$sentiment

## write graph
write_graph(simplify(sna.lampung.igraph), "lampung.gml", format = "gml")


#### test
df3 <- get_timeline("lampung", 
              n = 10, 
              language = 'id',
              since = '2023-05-09', 
              until = '2023-05-10',
              token = auth)
