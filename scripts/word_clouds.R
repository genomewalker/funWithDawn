library(tidytext)
library(rvest)
library(wordcloud2)

web_page <- read_html("https://blog.bookbaby.com/2015/12/revising-your-book-eleven-ways-to-take-a-new-look-at-your-story/")
web_nodes <- html_nodes(web_page, ".td-main-content p")
web_text <- html_text(web_nodes)
text_df <- tibble(text = web_text) %>%
  filter(nzchar(text)) %>%
  mutate(paragraph = row_number())

words <- text_df %>% unnest_tokens(word, text)

words <- words %>% anti_join(stop_words, by = "word")

words %>% count(word, sort = TRUE)

words %>% inner_join(get_sentiments("bing"), by = "word") %>%
  filter(sentiment == "positive") %>%
  count(word) %>%
  wordcloud2(size = 0.7, fontFamily = "RobotoCondensed-Regular", color = rep(c("orange", "skyblue"), length.out = nrow(.)), )

words %>% inner_join(get_sentiments("bing"), by = "word") %>%
  filter(sentiment == "negative") %>%
  count(word) %>%
  wordcloud2(size = 0.7, fontFamily = "RobotoCondensed-Regular",
             color = rep(c("black", "grey"), length.out = nrow(.)))
