
## Read the big text files 


news <- read_lines("en_US.news.txt", skip_empty_rows=TRUE, n_max=-1)

twitter <- read_lines("en_US.twitter.txt", skip_empty_rows=TRUE, n_max=-1)

blogs <- read_lines("en_US.blogs.txt", skip_empty_rows = TRUE, n_max=-1)


## merge them into one single character vector

vector3 <- c(news, twitter, blogs)

## remove special symbols

vector3 <- gsub("[^\x01-\x7F]", "", vector3)

## a vector of swearwords to be removed


swearing <- readLines("swearwords.txt")


## remove swearing :

vector5 <- gsub(paste(swearing, collapse="|"), "", vector3)

## remove urls:

vector5 <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", vector3)


## remove punctuation

vector5 <- gsub("[[:punct:]]", "", vector5)

## seampling

set.seed(1000)

sample_words <- sample(vector5, 0.03*length(vector5), replace=FALSE)

## tokenizing with some additional cleaning 

toks <- tokens(sample_words, what="word", remove_numbers=TRUE, 
               remove_twitter=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_separators=TRUE,
               include_docvars = TRUE, remove_url=TRUE, remove_hyphens=TRUE)

## removing upper cases 

toks <- tokens_tolower(toks)

## creating ngrams

bigrams <- tokens_ngrams(toks, n=2)
trigrams <- tokens_ngrams(toks, n=3)
fourgrams <- tokens_ngrams(toks, n=4)
fivegrams <- tokens_ngrams(toks, n=5)

## unlisting the ngrams
toks2 <- unlist(toks)


bigrams2 <- unlist(bigrams)

trigrams2 <- unlist(trigrams)

fourgrams2 <- unlist(fourgrams)

fivegrams2 <- unlist(fivegrams)

## creating the definition matrices


dfm_toks <- dfm(toks2)

dfm_bigrams <- dfm(bigrams2)

dfm_trigrams <- dfm(trigrams2)

dfm_fourgrams <- dfm(fourgrams2)

dfm_fivegrams <- dfm(fivegrams2)



##  creating dataframes with ngrams and their frequencies

f_toks <- textstat_frequency(dfm_toks)
f_bigrams <- textstat_frequency(dfm_bigrams)
f_trigrams <- textstat_frequency(dfm_trigrams)
f_fourgrams <- textstat_frequency(dfm_fourgrams)
f_fivegrams <-  textstat_frequency(dfm_fivegrams)


## Now it's time to create matrices with ngrams and their frequencies 
## where each column is a word in the ngram


f_toks1 <- subset(f_toks, select="feature")

f_bigrams1 <- subset(f_bigrams, select="feature")

f_trigrams1 <- subset(f_trigrams, select="feature")

f_fourgrams1 <- subset(f_fourgrams, select="feature")

f_fivegrams1 <- subset(f_fivegrams, select="feature")


sep_bi <- f_bigrams1 %>%
  separate(feature, c("word1", "word2"), sep="_")


sep_tri <- f_trigrams1 %>%
  separate(feature, c("word1", "word2", "word3"), sep="_")

sep_four <- f_fourgrams1 %>%
  separate(feature, c("word1", "word2", "word3", "word4"), sep="_")


sep_five <- f_fivegrams1 %>%
  separate(feature, c("word1", "word2", "word3", "word4", "word5"), sep="_")


## some additional cleaning 

drops <- c("feature")




sep_bi <- sep_bi[, !names(sep_bi) %in% drops]


sep_tri <- sep_tri[, !names(sep_tri) %in% drops]

sep_four <- sep_four[, !names(sep_four) %in% drops]

sep_five <- sep_five[, !names(sep_five) %in% drops]


## adding the frequency column again

sep_bi$frequency <- f_bigrams$frequency

sep_tri$frequency <- f_trigrams$frequency

sep_four$frequency <- f_fourgrams$frequency

sep_five$frequency <- f_fivegrams$frequency



## ordering the matrices by their descending frequency

sep_bi<- sep_bi[order(-sep_bi$frequency),]


sep_tri<- sep_tri[order(-sep_tri$frequency),]

sep_four <- sep_four[order(-sep_four$frequency),]

sep_five <- sep_five[order(-sep_five$frequency),]


## writing to rds files: now the data is ready for the Kneser-Kney algo

saveRDS(sep_bi, "sep_bi.rds")

saveRDS(sep_tri, "sep_tri.rds")

saveRDS(sep_four, "sep_four.rds")


saveRDS(sep_five, "sep_five.rds")











