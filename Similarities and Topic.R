library(reshape2)
library(ggplot2)

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
  
  biplot(y=tmod$docs[,2:3],x=tmod$features[,2:3], col=c("grey","red"),
         xlab = "Dim 2", ylab="Dim 3",xlim = )
  
  
  
  
  
  K <- 5
  lyrics.dtm <- convert(lyrics.dfm, to = "topicmodels")
  lda <- LDA(lyrics.dtm, k = K) 
  
  topics(lda,1)
    
  beta.td <- tidy(lda, matrix = "beta") # beta's are turned to proba scales
  beta.td
  
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