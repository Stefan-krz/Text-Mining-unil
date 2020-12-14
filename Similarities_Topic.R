library(reshape2)
library(ggplot2)




#tm_map

crude.jac <- textstat_simil(lyrics.tfidf, method = "jaccard", margin = "documents")

rude.jac.mat <- melt(as.matrix(crude.jac)) # Convert the object to matrix then to data frame 
ggplot(data = rude.jac.mat, aes(x=Var1, y=Var2, fill=value))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), name="Jaccard")+
  geom_tile()

crude.hc <- hclust(dist(1 - crude.jac))
plot(crude.hc)


library(topicmodels)

library(quanteda.textmodels)

lyrics.cp <- corpus(final_lyrics$lyrics)

lyrics.dfm <-  dfm(lyrics.cp,
                   stem=FALSE,
                   tolower=TRUE,
                   remove=c(stopwords("english")),
                   remove_punct=TRUE,
                   remove_number=TRUE,
                   remove_symbols=TRUE)

tmod= textmodel_lsa(lyrics.dfm, nd=10)
tmod$docs

biplot(y=tmod$docs[,2:3],x=tmod$features[,2:3], col=c("grey","red"),
       xlab = "Dim 2", ylab="Dim 3",xlim = )


#LDA
# beta's are turned to prob scales
lyrics.cp <- corpus(final_lyrics$lyrics)

lyrics.dfm <-  dfm(lyrics.cp,
                   stem=FALSE,
                   tolower=TRUE,
                   remove=c(stopwords("english")),
                   remove_punct=TRUE,
                   remove_number=TRUE,
                   remove_symbols=TRUE)

K <- 5
lyrics.dfm <- convert(lyrics.dfm, to = "topicmodels")
lda <- LDA(lyrics.dfm, k = K) 

beta.td <- tidy(lda, matrix = "beta") 
beta.td

#visualization of Topics

beta.top.terms <- beta.td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

beta.top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#Gamma's
gamma.td <- tidy(lda, matrix = "gamma")


test <- cbind(distinct( gamma.td[,1]),final_lyrics) %>% select(document,genre)

gamma.td <- left_join(gamma.td,test, by= "document")  

#creating list of levels to arrange documents by genre for better visualization


gamma.td %>%
  ggplot(aes(document, gamma, fill = genre)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()+
 theme(axis.text.x=element_blank(),
       axis.text.y=element_blank()) 

#we get rid of some words that are not common to any genre and pollute analysis
flyrics.tok <- unnest_tokens(final_lyrics, output="word", input="lyrics", to_lower=TRUE, strip_punct=TRUE, 
                             strip_numeric=TRUE)
flyrics.tokRemove <- flyrics.tok %>%  mutate(nchar= nchar(word)) %>% filter(nchar < 3) %>% select(word)
flyrics.tokRemove <- flyrics.tokRemove %>% distinct(word)
freqword1 <- genre_tf_idf %>% filter(tf_idf==0) %>% distinct(word)
freqword2 <- song_tf_idf %>% filter(tf_idf>=0.3) %>% distinct(word)
freqword= rbind(freqword1,freqword2)
a <- rbind(freqword,flyrics.tokRemove)

# new corpus without the dictionary a

newlyrics.dfm <-  dfm(lyrics.cp,
                   stem=FALSE,
                   tolower=TRUE,
                   remove=c(stopwords("english"),a$word),
                   remove_punct=TRUE,
                   remove_number=TRUE,
                   remove_symbols=TRUE) 
  K <- 5
  newlyrics.dfm <- convert(newlyrics.dfm, to = "topicmodels")
  lda <- LDA(newlyrics.dfm, k = K) 
  
  
  beta.td <- tidy(lda, matrix = "beta") 
  beta.td
  
#visualization of Topics with treatment

beta.top.terms <- beta.td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) 

beta.top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#Gamma's
gamma.td <- tidy(lda, matrix = "gamma") 
test <- cbind(distinct( gamma.td[,1]),final_lyrics) %>% select(document,genre)
gamma.td <- left_join(gamma.td,test, by= "document") 

#we create levels to order the gamma's
lvl<- arrange(test,genre)
lvl <- lvl$document

gamma.td %>% ggplot(aes(factor(document,levels = lvl), gamma, fill = genre)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank()) 
