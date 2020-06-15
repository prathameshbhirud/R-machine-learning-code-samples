#install and use libraries

 install.packages("ggthemes")
 install.packages("qdap")
 install.packages("dplyr")
 install.packages("tm")
 install.packages("wordcloud")
 install.packages("plotrix")
 install.packages("dendextend")
 install.packages("ggplot2")
 install.packages("ggthemes")
 install.packages("RWeka")
 install.packages("reshape2")
 install.packages("quanteda")
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)

#get data
data = read.csv("C:/Prathamesh/Sample Codes/R/RTVS-docs-master/examples/datasets/e-commerce/Womens Clothing E-Commerce Reviews.csv",
                header = TRUE,
                stringsAsFactors = FALSE)
head(data)
str(data)

corpus_review = Corpus(VectorSource(data$Review.Text))
corpus_review = tm_map(corpus_review, tolower)
corpus_review = tm_map(corpus_review, removePunctuation)

#Remove stopwords
corpus_review = tm_map(corpus_review, removeWords, stopwords("english"))

# Remove context specific stop words
corpus_review = tm_map(corpus_review, removeWords, c("also", "get", "like", "company", "made", "can", "im", "dress", "just", "i"))

## Stem document
corpus_review = tm_map(corpus_review, stemDocument)

##Viewing the corpus content
corpus_review[[8]][1]

# Find the 20 most frequent terms: term_count
term_count <- freq_terms(corpus_review, 20)
plot(term_count)

review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)

# Convert TDM to matrix
review_m <- as.matrix(review_tdm)
# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)
# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)
# View the top 10 most common words
review_term_freq[1:10]

# Plot a barchart of the 20 most common words
barplot(review_term_freq[1:20], col = "steel blue", las = 2)


review_word_freq <- data.frame(term = names(review_term_freq),
  num = review_term_freq)
# Create a wordcloud for the values in word_freqs
wordcloud(review_word_freq$term, review_word_freq$num,
  max.words = 50, colors = "red")

# Print the word cloud with the specified colors
wordcloud(review_word_freq$term, review_word_freq$num,
  max.words = 50, colors = c("aquamarine", "darkgoldenrod", "tomato"))



## Combine both corpora: all reviews
all_yes <- paste(review_yes, collapse = "")
all_no <- paste(review_no, collapse = "")
all_combine <- c(all_yes, all_no)
## Creating corpus for combination
corpus_review_all = Corpus(VectorSource(all_combine))
## Pre-processing corpus - all
#Convert to lower-case
corpus_review_all = tm_map(corpus_review_all, tolower)
#Remove punctuation
corpus_review_all = tm_map(corpus_review_all, removePunctuation)
#Remove stopwords
corpus_review_all = tm_map(corpus_review_all, removeWords, stopwords("english"))
corpus_review_all = tm_map(corpus_review_all, removeWords, c("also", "get", "like", "company", "made", "can", "im", "dress", "just", "i"))
#Stem document
corpus_review_all = tm_map(corpus_review_all, stemDocument)
review_tdm_all <- TermDocumentMatrix(corpus_review_all)
all_m = as.matrix(review_tdm_all)
colnames(all_m) = c("Yes", "No")
#Sum rows and frequency data frame
review_term_freq_all <- rowSums(all_m)
review_word_freq_all <- data.frame(term = names(review_term_freq_all), num = review_term_freq_all)
#Make commonality cloud
commonality.cloud(all_m,
                  colors = "steelblue1",
                  max.words = 50)



# Create comparison cloud
comparison.cloud(all_m,
                 colors = c("green", "red"),
                 max.words = 50)


# Identify terms shared by both documents
common_words <- subset(all_m, all_m[, 1] > 0 & all_m[, 2] > 0)
# calculate common words and difference
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = T),]
head(common_words)


top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25,]))
# Make pyramid plot
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels,
             main = "Words in Common",
             gap = 2000,
             laxlab = NULL,
             raxlab = NULL,
             unit = NULL,
             top.labels = c("Yes",
                            "Words",
                            "No")
             )


review_tdm2 <- removeSparseTerms(review_tdm, sparse = 0.9)
hc <- hclust(d = dist(review_tdm2, method = "euclidean"), method = "complete")
# Plot a dendrogram
plot(hc)


# Create associations
associations <- findAssocs(review_tdm, "fit", 0.05)
# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]
# Plot the associations_df values 
ggplot(associations_df, aes(y = associations_df[, 1])) +
    geom_point(aes(x = associations_df[, 2]),
             data = associations_df, size = 3) +
             ggtitle("Word Associations to 'fit'") +
             theme_gdocs()


##Create bi-grams
review_bigram <- tokens(review$Review.Text) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding = TRUE) %>%
    tokens_ngrams(n = 2) %>%
    dfm()
topfeatures(review_bigram)


##Create tri-grams
review_trigram <- tokens(review$Review.Text) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding = TRUE) %>%
    tokens_ngrams(n = 3) %>%
    dfm()
topfeatures(review_trigram)

## Load the required libraries
library(irlba)
library(e1071)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(biclust)
library(igraph)
library(fpc)
library(Rcampdf)


# Tokenize descriptions
reviewtokens = tokens(review$Review.Text, what = "word",
remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE)
# Lowercase the tokens
reviewtokens = tokens_tolower(reviewtokens)
# remove stop words and unnecessary words
rmwords <- c("dress", "etc", "also", "xxs", "xs", "s")
reviewtokens = tokens_select(reviewtokens, stopwords(), selection = "remove")
reviewtokens = tokens_remove(reviewtokens, rmwords)
# Stemming tokens
reviewtokens = tokens_wordstem(reviewtokens, language = "english")
reviewtokens = tokens_ngrams(reviewtokens, n = 1:2)


# Creating a bag of words
reviewtokensdfm = dfm(reviewtokens, tolower = FALSE)
# Remove sparsity
reviewSparse <- convert(reviewtokensdfm, "tm")
tm::removeSparseTerms(reviewSparse, 0.7)
# Create the dfm
dfm_trim(reviewtokensdfm, min_docfreq = 0.3)
x = dfm_trim(reviewtokensdfm, sparsity = 0.98)


## Setup a dataframe with features
df = convert(x, to = "data.frame")
##Add the Y variable Recommend.IND
reviewtokensdf = cbind(review$Recommended.IND, df)
head(reviewtokensdf)
## Cleanup names
names(reviewtokensdf)[names(reviewtokensdf) == "review.Recommended.IND"] <- "recommend"
names(reviewtokensdf) = make.names(names(reviewtokensdf))
head(reviewtokensdf)
## Remove the original review.text column
reviewtokensdf = reviewtokensdf[, - c(2)]
head(reviewtokensdf)
reviewtokensdf$recommend = factor(reviewtokensdf$recommend)


## Build the CART model
tree = rpart(formula = recommend ~ ., data = reviewtokensdf, method = "class", control = rpart.control(minsplit = 200, minbucket = 30, cp = 0.0001))
printcp(tree)
plotcp(tree)
##Prune down the tree
bestcp = tree$cptable[which.min(tree$cptable[, "xerror"]), "CP"]
bestcp
ptree = prune(tree, cp = bestcp)
rpart.plot(ptree, cex = 0.6)
prp(ptree, faclen = 0, cex = 0.5, extra = 2)


library(randomForest)
reviewRF = randomForest(recommend ~ ., data = reviewtokensdf)
varImpPlot(reviewRF, cex = .7)


#load required library
library(glmnet)
#convert training data to matrix format
x <- model.matrix(recommend ~ ., reviewtokensdf)
#convert class to numerical variable
y <- as.numeric(reviewtokensdf$recommend)
#perform grid search to find optimal value of lambda
cv.out <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "mse")
#plot result
plot(cv.out)


#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
lambda_1se
#regression coefficients
coef = coef(cv.out, s = lambda_1se)
lassocoef = as.matrix(coef(cv.out, s = lambda_1se))
write.csv(lassocoef, "lasso_coef.csv")


# Find the best lambda using cross-validation
set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
# Fit the final model on the dataframe
review_logreg <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Save the regression coef to a csv file
logregcoef = as.matrix(coef(review_logreg))
odds_ratio = as.matrix(exp(coef(review_logreg)))
write.csv(logregcoef, "logreg_coef.csv")
write.csv(odds_ratio, "odds_ratio.csv")