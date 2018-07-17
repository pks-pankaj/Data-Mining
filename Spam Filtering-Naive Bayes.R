# read the sms data into the sms data frame
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)


# examine the structure of the sms data
str(sms_raw)

# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)


# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
str(sms_raw$type)

table(sms_raw$type)

# build a corpus using the text mining (tm) package
library(tm)
## Loading required package: NLP
sms_corpus <- VCorpus(VectorSource(sms_raw$text))


# examine the sms corpus
print(sms_corpus)
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 0
## Content:  documents: 5559
inspect(sms_corpus[1:2])
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 0
## Content:  documents: 2
## 
## [[1]]
## <<PlainTextDocument>>
## Metadata:  7
## Content:  chars: 49
## 
## [[2]]
## <<PlainTextDocument>>
## Metadata:  7
## Content:  chars: 23
as.character(sms_corpus[[1]])
## [1] "Hope you are having a good week. Just checking in"
lapply(sms_corpus[1:2], as.character)
## $`1`
## [1] "Hope you are having a good week. Just checking in"
## 
## $`2`
## [1] "K..give back my thanks."

# clean up the corpus using tm_map()
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
# show the difference between sms_corpus and corpus_clean
as.character(sms_corpus[[1]])
## [1] "Hope you are having a good week. Just checking in"
as.character(sms_corpus_clean[[1]])
## [1] "hope you are having a good week. just checking in"


sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # remove stop words
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) # remove punctuation


# illustration of word stemming
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))
## [1] "learn" "learn" "learn" "learn"
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) # eliminate unneeded whitespace



# examine the final clean corpus
lapply(sms_corpus[1:3], as.character)
## $`1`
## [1] "Hope you are having a good week. Just checking in"
## $`2`
## [1] "K..give back my thanks."
## $`3`
## [1] "Am also doing in cbe only. But have to pay."
lapply(sms_corpus_clean[1:3], as.character)
## $`1`
## [1] "hope good week just check" 
## $`2`
## [1] "kgive back thank"
## $`3`
## [1] "also cbe pay"


# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)


# alternative solution: create a document-term sparse matrix directly from the SMS corpus
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))


# alternative solution: using custom stop words function ensures identical result
sms_dtm3 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = function(x) { removeWords(x, stopwords()) },
  removePunctuation = TRUE,
  stemming = TRUE
))
# compare the result
sms_dtm
## <<DocumentTermMatrix (documents: 5559, terms: 6518)>>
## Non-/sparse entries: 42113/36191449
## Sparsity           : 100%
## Maximal term length: 40
## Weighting          : term frequency (tf)
sms_dtm2
## <<DocumentTermMatrix (documents: 5559, terms: 6909)>>
## Non-/sparse entries: 43192/38363939
## Sparsity           : 100%
## Maximal term length: 40
## Weighting          : term frequency (tf)
sms_dtm3
## <<DocumentTermMatrix (documents: 5559, terms: 6518)>>
## Non-/sparse entries: 42113/36191449
## Sparsity           : 100%
## Maximal term length: 40
## Weighting          : term frequency (tf)


# creating training and test datasets
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]
The labels of each of the row is taken from the raw data and stored.
# also save the labels
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5559, ]$type
The following function helps in finding out if the subset of the data are proper representation of the complete set of SMS data.
# check that the proportion of spam is similar
prop.table(table(sms_train_labels))
## sms_train_labels
##       ham      spam 
## 0.8647158 0.1352842
prop.table(table(sms_test_labels))
## sms_test_labels
##       ham      spam 
## 0.8683453 0.1316547



# word cloud visualization
library(wordcloud)
## Loading required package: RColorBrewer
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)


# subset the training data into spam and ham groups
spam <- subset(sms_raw, type == "spam")
ham  <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999)
sms_dtm_freq_train
## <<DocumentTermMatrix (documents: 4169, terms: 1101)>>
## Non-/sparse entries: 24834/4565235
## Sparsity           : 99%
## Maximal term length: 19
## Weighting          : term frequency (tf)


# indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)
##    [1] "abiola"              "abl"                 "abt"                
# save frequently-appearing terms to a character vector
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
##  chr [1:1136] "abiola" "abl" "abt" "accept" "access" ...


# create DTMs with only the frequent terms
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]


# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}


# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)


## Step 3: Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)


## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)
head(sms_test_pred)
## [1] ham  ham  ham  ham  spam ham 
## Levels: ham spam


library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))




