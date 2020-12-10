library(tidyverse)
library(tidytext)
library(wordcloud)


final_lyrics <- read_csv("final_lyrics.csv")



#Tokenisation

flyrics.tok <- unnest_tokens(final_lyrics, output="word", input="lyrics", to_lower=TRUE, strip_punct=TRUE, 
                             strip_numeric=TRUE)


#remove stop words


flyrics.tok1 <- anti_join(flyrics.tok, stop_words, by = "word")  %>% mutate(nchar= nchar(word)) %>% filter(nchar > 2)



#  tokens_remove(flyrics.tok1$word,pattern="\\b\\w{1,2}\\b", valuetype="regex")



lyrics.fr <- flyrics.tok1 %>% group_by(artist,song,genre) %>% count(word, sort=TRUE) %>% ungroup()

index <- top_n(lyrics.fr, 15)
lyrics.fr %>% filter(word %in% index$word) %>% ggplot(aes(x=word, y=n)) + geom_col() + coord_flip() +facet_wrap(~genre, ncol = 2)

#we sould maybee remove words less than 3 letters to better result ? because a lot of la la la na na na  from song. 


index <- lyrics.fr  %>% mutate(ifelse(nchar())) group_by(genre) %>% top_n(10)
index2 <- lyrics.fr  %>% filter(n> 10)%>% group_by(song) %>% top_n(1)
lyrics.fr %>% filter(word %in% index$word) %>% ggplot(aes(word, n)) + geom_col() + coord_flip() +facet_wrap(~genre, ncol = 2)


#Use quanteda library to create dfm 

library(quanteda)

#creation of the corpus 

lyrics.cp <- corpus(final_lyrics$lyrics)

summary(lyrics.cp)


#lyrics.tok <- tokens(lyrics.cp, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_separators=TRUE)

#lyrics.tok <- lyrics.tok %>% tokens_tolower() %>% tokens_remove(stopwords("english")) 


#creatio of matrix
lyrics.dfm <-  dfm(lyrics.cp,
                   stem=FALSE,
                   tolower=TRUE,
                   remove=c(stopwords("english")),
                   remove_punct=TRUE,
                   remove_number=TRUE,
                   remove_symbols=TRUE)

lyrics.tok <-  tokens(lyrics.cp,
                   remove_punct=TRUE,
                   remove_number=TRUE,
                   remove_symbols=TRUE)




#dfm with tfidf

lyrics.tfidf <- dfm_tfidf(lyrics.dfm)  


#wordcloud 

lyrics_freq <- textstat_frequency(lyrics.dfm)

textplot_wordcloud(lyrics.tfidf)



#add TTR in data set to use later for classification

  lyrics.div <- textstat_lexdiv(lyrics.dfm, measure = "TTR")

# and MATTR (give same result) 
lyrics.div2 <- textstat_lexdiv(lyrics.tok, measure = "MATTR", MATTR_window = 5)


#visualisation of TTR by song
lyrics.div %>% 
  ggplot(aes(x=reorder(document, TTR), y=TTR))+geom_point()+coord_flip()+
  xlab("Text") + ylab("Yule's index")
# add TTR in dataset

final_lyrics1 <- cbind(final_lyrics,TTR = lyrics.div$TTR,MATTR = lyrics.div2$MATTR)

#visualise difference of ttr among genre

final_lyrics1 %>% group_by(genre) %>% summarize(mean= mean(MATTR)) %>% ungroup() %>%   ggplot(aes(x = genre, y = mean, fill = genre)) + 
  geom_bar(stat = "identity", alpha = 0.8)
  

