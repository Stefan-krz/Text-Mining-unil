lyrics.corpus <- corpus(final_lyrics1, text_field = "lyrics")
lyrics.tokens <- tokens(lyrics.corpus, 
                        remove_punct = TRUE, 
                        remove_symbols = TRUE, 
                        remove_url = TRUE,
                      ) 

lyrics.tokens <- tokens_tolower(lyrics.tokens) %>% tokens_wordstem() %>%
  tokens_remove(stopwords("english"))  