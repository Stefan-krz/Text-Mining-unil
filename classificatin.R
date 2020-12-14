lyrics.corpus <- corpus(final_lyrics, text_field = "lyrics")
lyrics.tokens <- tokens(lyrics.corpus, 
                        remove_punct = TRUE, 
                        remove_symbols = TRUE, 
                        remove_url = TRUE,
                      ) 

lyrics.tokens <- tokens_tolower(lyrics.tokens) %>% tokens_wordstem() %>%
  tokens_remove(c(stopwords("english"),a$word)) 

y <- factor(docvars(lyrics.tokens, "genre"))
lyrics.dfm <- dfm(lyrics.tokens)
dim(lyrics.dfm)
library(quanteda.textmodels)
lyrics.lsa <- textmodel_lsa(lyrics.dfm, nd=25)
head(lyrics.lsa$docs)

df <- data.frame(Class=y, X=lyrics.lsa$docs)
index.tr <- sample(size=round(0.8*length(y)), x=c(1:length(y)), replace=FALSE)
df.tr <- df[index.tr,]
df.te <- df[-index.tr,]
library(ranger)

lyrics.fit <- ranger(Class ~ ., 
                     data = df.tr)
pred.te <- predict(lyrics.fit, df.te)

library(caret)
confusionMatrix(data=pred.te$predictions, reference = df.te$Class)

#check if it was the best choice to choose 25 as number of dimension for lsa
nd.vec <- c(2,5,6,7,8, 9, 10,20,25,30,40,60,80,100)
acc.vec <- numeric(length(nd.vec))
for (j in 1:length(nd.vec)){
  lyrics.lsa <- textmodel_lsa(lyrics.dfm, nd=nd.vec[j])
  df <- data.frame(Class=y, X=lyrics.lsa$docs)
  df.tr <- df[index.tr,]
  df.te <- df[-index.tr,]
  
  lyrics.fit <- ranger(Class ~ ., 
                       data = df.tr)
  pred.te <- predict(lyrics.fit, df.te)
  acc.vec[j] <- confusionMatrix(data=pred.te$predictions, reference = df.te$Class)$overall[1]
}
acc.vec

#plot: yes 25 was the best

plot(acc.vec ~ nd.vec, type='b')

## compare to tf-idf
lyrics.tfidf <- dfm_tfidf(lyrics.dfm)
lyrics.lsa <- textmodel_lsa(lyrics.tfidf, nd=25)

df <- data.frame(Class=y, X=lyrics.lsa$docs)
df.tr <- df[index.tr,]
df.te <- df[-index.tr,]
lyrics.fit <- ranger(Class ~ ., 
                     data = df.tr)
pred.te <- predict(lyrics.fit, df.te)
confusionMatrix(data=pred.te$predictions, reference = df.te$Class)
## Confusion Matrix and Statistics
## 
##                  Reference
## Prediction        HillaryClinton realDonaldTrump
##   HillaryClinton             577              79
##   realDonaldTrump             81             551
##                                           
##                Accuracy : 0.8758          

lyrics.tfidf <- dfm_tfidf(lyrics.dfm)
lyrics.lsa <- textmodel_lsa(lyrics.tfidf, nd=25)

df <- data.frame(Class=y, X=lyrics.lsa$docs)
df <- cbind(df, song_value = final_lyrics$value, MATTR =final_lyrics$MATTR)
df.tr <- df[index.tr,]
df.te <- df[-index.tr,]
lyrics.fit <- ranger(Class ~ ., 
                     data = df.tr, 
                     importance = "impurity")
pred.te <- predict(lyrics.fit, df.te)
confusionMatrix(data=pred.te$predictions, reference = df.te$Class)



