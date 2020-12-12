#addin sentiment dictionnary to data set

lyrics.sent <- flyrics.tok1 %>%
  inner_join(get_sentiments("nrc"))

#sentiment per genre
lyrics.sent %>% group_by(genre, sentiment) %>% summarize(n = n()) %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  facet_wrap(~ genre) + coord_flip()

# sentiment per genre but with frequencies i.e taking lenght of songs in account 
lyrics.sent %>% group_by(genre, sentiment) %>%  summarize(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% ggplot(aes(x = sentiment, y = freq, fill = sentiment)) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  facet_wrap(~ genre) + coord_flip()

# now sentiment based on value with afinn
get_sentiments("afinn")

lyrics.sent <- flyrics.tok1 %>%
  inner_join(get_sentiments("afinn"))
# visualise by genre


  aggregate(value~genre, data=lyrics.sent, FUN=mean) %>% 
    ggplot(aes(x = genre, y = value)) + 
    geom_bar(stat = "identity") + coord_flip()
#it may help classify so we add value by song  info to dataset

songval <- aggregate(value~song, data=lyrics.sent, FUN=mean)
final_lyrics <- final_lyrics1 %>% inner_join(songval)


