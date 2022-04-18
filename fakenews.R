# load library
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(tm)       
library(textstem)    
library(tidytext)
library(wordcloud2)
library(pROC)
library(ROCR)
library(randomForest)   
library(naivebayes)
library(caret)

fake <- read_csv('C:/Users/veniniyan/Downloads/Fake.csv')
true <- read_csv('C:/Users/veniniyan/Downloads/True.csv')

head(fake)
head(true)

fake$category <- 0
true$category <- 1

glimpse(fake)
glimpse(true)

# Merge 2 datasets
news <- bind_rows(fake, true)
news %>%
  sample_n(10)

glimpse(news)
news$category <- as.factor(news$category)

ggplot(news, aes(x = category, fill = category)) + 
  geom_bar() +
  theme_classic() +
  theme(axis.title = element_text(face = 'bold', size = 15),
        axis.text = element_text(size = 13)) +
  theme(legend.position = 'none')

# Summarize data
summary(news)

# Check for misisng values
summary(is.na(news))

# Change data type of subject to factor
news$subject <- as.factor(news$subject)

# News count by each Subject
news %>%
  group_by(subject) %>%
  count() %>%
  arrange(desc(n))

news %>%
  group_by(subject) %>%
  count(sort = TRUE) %>%
  rename(freq = n) %>%
  ggplot(aes(x = reorder(subject, -freq), y = freq)) + 
  geom_bar(stat = 'identity', fill = 'skyblue') +
  theme_classic() +
  xlab('Subject') +
  ylab('frequency') +
  geom_text(aes(label = freq), vjust = 1.2, fontface = 'bold') +
  theme(axis.title = element_text(face = 'bold', size = 15),
        axis.text = element_text(size = 13, angle = 90))

# Category wise subject bar plot
ggplot(news, aes(x = subject, fill = category)) +
  geom_bar(position = 'dodge', alpha = 0.6) +
  theme_classic() +
  theme(axis.title = element_text(face = 'bold', size = 15),
        axis.text = element_text(size = 13, angle = 90))

# Combine title and text column
news <- news %>% 
  select(title, text, category) %>%
  unite(col = text ,title, text, sep = ' ')  %>%  # Combine 'title' & 'text' column
  mutate(ID = as.character(1:nrow(news)))    # Uniqe row ID for furt
glimpse(news)

# Create a corpus (type of object expected by tm)
doc <- VCorpus(VectorSource(news$text))

#Data Cleaning
# Create a corpus (type of object expected by tm)
doc <- VCorpus(VectorSource(news$text))

# Convert text to lower case
doc <- tm_map(doc, content_transformer(tolower))

# Remove numbers
doc <- tm_map(doc, removeNumbers)

# Remove Punctuations
doc <- tm_map(doc, removePunctuation)

# Remove Stopwords
doc <- tm_map(doc, removeWords, stopwords('english'))

# Remove Whitespace
doc <- tm_map(doc, stripWhitespace)

# inspect output
writeLines(as.character(doc[[45]]))

doc <- tm_map(doc, content_transformer(str_remove_all), "[[:punct:]]")
writeLines(as.character(doc[[45]]))
writeLines(as.character(doc[[50]]))

# Lemmatization
doc <- tm_map(doc, content_transformer(lemmatize_strings))
# Create Document Term Matrix
dtm <- DocumentTermMatrix(doc)
inspect(dtm)

# remove all terms whose sparsity is greater than the threshold (x)
dtm.clean <- removeSparseTerms(dtm, sparse = 0.99)
inspect(dtm.clean)
# Create Tidy data
df.tidy <- tidy(dtm.clean)
df.word<- df.tidy %>% 
  select(-document) %>%
  group_by(term) %>%
  summarize(freq = sum(count)) %>%
  arrange(desc(freq))

# Word cloud
set.seed(1234) # for reproducibility 
wordcloud2(data=df.word, size=1.6, color='random-dark')

# Word cloud for the fake news
set.seed(1234)
df.tidy %>% 
  inner_join(news, by = c('document' = 'ID')) %>% 
  select(-text) %>%
  group_by(term, category) %>%
  summarize(freq = sum(count)) %>%
  filter(category == 0) %>%
  select(-category) %>%
  arrange(desc(freq)) %>%
  wordcloud2(size = 1.4,  color='random-dark')

# Word cloud for the true news
set.seed(1234)
df.tidy %>% 
  inner_join(news, by = c('document' = 'ID')) %>% 
  select(-text) %>%
  group_by(term, category) %>%
  summarize(freq = sum(count)) %>%
  filter(category == 1) %>%
  select(-category) %>%
  arrange(desc(freq)) %>%
  wordcloud2(size = 1.6,  color='random-dark')

# Convert dtm to matrix
dtm.mat <- as.matrix(dtm.clean)
dim(dtm.mat)
# dtm.df <- as.data.frame(dtm.mat)
dtm.mat <- cbind(dtm.mat, category = news$category)
dtm.mat[1:10, c(1, 2, 3, ncol(dtm.mat))]

summary(dtm.mat[,'category'])
as.data.frame(dtm.mat) %>% count(category)
news %>% count(category)

# Convert matrix to data frame
dtm.df <- as.data.frame(dtm.mat)

# Replace valuees in category by original values (1 by 0 & 2 by 1)
dtm.df$category <- ifelse(dtm.df$category == 2, 1, 0)
dtm.df$category <- as.factor(dtm.df$category)
table(dtm.df$category)

# Create 75:25 split
set.seed(1234)
index <- sample(nrow(dtm.df), nrow(dtm.df)*0.75, replace = FALSE)

train_news <- dtm.df[index,]
test_news <- dtm.df[-index,]

# make column names to follow R's variable naming convention
names(train_news) <- make.names(names(train_news))
names(test_news) <- make.names(names(test_news))

table(train_news$category)
table(test_news$category)

# Naive Bayes Model
mdl_nb <- naive_bayes(category ~ ., data = train_news)

# Model Summary
summary(mdl_nb)

# Logistic Regression Model
mdl_lr <- glm(formula = category ~.,
              data = train_news,
              family = 'binomial')

# Random Forest Model
k <- round(sqrt(ncol(train_news)-1))
mdl_rf <- randomForest(formula = category ~ ., 
                       data = train_news,
                       ntree = 100,
                       mtry = k,
                       method = 'class')
mdl_rf

# Predicted values
train_news$pred_nb <- predict(mdl_nb, type = 'class')
train_news$pred_lr <- predict(mdl_lr, type = 'response')
train_news$pred_rf <- predict(mdl_rf, type = 'response')

# Predicted Values for test set
test_news$pred_nb <- predict(mdl_nb, newdata = test_news)
test_news$pred_lr <- predict(mdl_lr, newdata = test_news, type = 'response')
test_news$pred_rf <- predict(mdl_rf, newdata = test_news, type = 'response')

# Plot ROC Curve for train set
prediction(as.numeric(train_news$pred_nb), as.numeric(train_news$category)) %>%
  performance('tpr', 'fpr') %>%
  plot(col = 'red', lwd = 2)

prediction(as.numeric(train_news$pred_lr), as.numeric(train_news$category)) %>%
  performance('tpr', 'fpr') %>%
  plot(add = TRUE, col = 'blue', lwd = 2)

prediction(as.numeric(train_news$pred_rf), as.numeric(train_news$category)) %>%
  performance('tpr', 'fpr') %>%
  plot(add = TRUE, col = 'green', lwd = 2)

legend(0.8, 0.2, legend=c("NB", "Logistic", "RF"),
       col=c("red", "blue", 'green'), lty = 1, cex = 1.2, box.lty = 0)

# Plot ROC Curve for test set
prediction(as.numeric(test_news$pred_nb), as.numeric(test_news$category)) %>%
  performance('tpr', 'fpr') %>%
  plot(col = 'red', lwd = 2)

prediction(as.numeric(test_news$pred_lr), as.numeric(test_news$category)) %>%
  performance('tpr', 'fpr') %>%
  plot(add = TRUE, col = 'blue', lwd = 2)

prediction(as.numeric(test_news$pred_rf), as.numeric(test_news$category)) %>%
  performance('tpr', 'fpr') %>%
  plot(add = TRUE, col = 'green', lwd = 2)

legend(0.8, 0.2, legend=c("NB", "Logistic", "RF"),
       col=c("red", "blue", 'green'), lty = 1, cex = 1.2, box.lty = 0)

# Set Threshold for Logistic Regression Model
roc(test_news$category, test_news$pred_lr) %>% coords()

test_news$pred_lr <- ifelse(test_news$pred_lr > 0.5, 1, 0)
test_news$pred_lr <- as.factor(test_news$pred_lr)

# Confussion Matrix
conf_nb <- caret::confusionMatrix(test_news$category, test_news$pred_nb)
conf_lr <- caret::confusionMatrix(test_news$category, test_news$pred_lr)
conf_rf <- caret::confusionMatrix(test_news$category, test_news$pred_rf)

# Heatmap of Confusion Matrix
bind_rows(as.data.frame(conf_nb$table), as.data.frame(conf_lr$table), as.data.frame(conf_rf$table)) %>% 
  mutate(Model = rep(c('Naive Bayes', 'Logistic Regression', 'Random Forest'), each = 4)) %>%
  ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  labs(x = 'Actual', y = 'Predicted') +
  scale_fill_gradient(low = "#CCE5FF", high = "#000099") +
  scale_x_discrete(limits = c('1', '0'), labels = c('1' = 'Not Fake', '0' = 'Fake')) +
  scale_y_discrete(labels = c('1' = 'Not Fake', '0' = 'Fake')) +
  facet_grid(. ~ Model) +
  geom_text(aes(label = Freq), fontface = 'bold') +
  theme(panel.background = element_blank(),
        legend.position = 'none',
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 11, face = 'bold'),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = 'bold'))

acc <- c(nb = conf_nb[['overall']]['Accuracy'], 
         lr = conf_lr[['overall']]['Accuracy'],
         rf = conf_rf[['overall']]['Accuracy'])
precision <- c(nb = conf_nb[['byClass']]['Pos Pred Value'], 
               lr = conf_lr[['byClass']]['Pos Pred Value'], 
               rf = conf_rf[['byClass']]['Pos Pred Value'])
recall <- c(nb = conf_nb[['byClass']]['Sensitivity'], 
            lr = conf_lr[['byClass']]['Sensitivity'],
            rf = conf_rf[['byClass']]['Sensitivity'])

data.frame(Model = c('Naive Bayes', 'Logistic Regression', 'Random Forest'),
           Accuracy = acc,
           F1_Score = (2 * precision * recall) / (precision + recall),
           row.names = NULL)


