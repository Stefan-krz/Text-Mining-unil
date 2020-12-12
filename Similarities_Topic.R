library(reshape2)
library(ggplot2)

#not working

#freqword <- song_tf_idf %>% filter(tf_idf==0) %>% distinct(word)
#a <- top_n(freqword,30)


#lyrics.tok <- lyrics.tok %>% tokens_remove(pattern= a, value="fixed")
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

tmod= textmodel_lsa(lyrics.dfm, nd=3)
tmod$docs

biplot(y=tmod$docs[,2:3],x=tmod$features[,2:3], col=c("grey","red"),
       xlab = "Dim 2", ylab="Dim 3",xlim = )


#LDA
# beta's are turned to prob scales
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

gamma.td <- left_join(gamma.td,test, by= "document") %>% 

gamma.td %>%
  ggplot(aes(document, gamma, fill = factor(genre))) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

