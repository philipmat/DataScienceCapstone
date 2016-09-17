# produces the intermediate files
DATASET_LOCATION = "C:/Dev/datascience/Capstone/final/"

library(tm)
library(stringi)
library(stringr)
library(ggplot2)
library(dplyr)


# file loading
readSourceData <- function(files) {
    ret_data = c()
    for (f in files) {
        file <- file(paste(DATASET_LOCATION, "en_US", f, sep='/'), open="rb")
        data <- readLines(file)
        close(file)
        ret_data <- c(ret_data, data)
        rm(data)
    }
    return(ret_data)
}

summary <- function() {
    words_blogs = stri_count_words(data_blogs)
    words_news = stri_count_words(data_news)
    words_twitter = stri_count_words(data_twitter)
    summary_frame <- data.frame(
        file = c('blogs', 'news', 'twitter'),
        entries = c(length(data_blogs), length(data_news), length(data_twitter)),
        total_words = c(sum(words_blogs), sum(words_news), sum(words_twitter)),
        mean_words_per_entry = c(mean(words_blogs), mean(words_news), mean(words_twitter)))
}

makeSample <- function(data, samplePercent) {
    if (samplePercent == 1) {
        return(data)
    }
    set.seed(123)
    sample <- sample(data, length(data) * samplePercent)
    return(sample)
}

# rm(data_blogs, data_news, data_twitter)  # free up memory
# gc()

preProcess <- function(x) {
    # ret <- tm_map(x,  content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),  mc.cores=1)
    ret <- tm_map(x,  content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),  mc.cores=1)
    
    #Removing special characters and URLs
    # toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x,fixed=TRUE))
    #ret <- tm_map(ret, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
    #ret <- tm_map(ret, toSpace, "@[^\\s]+")
    
    ret <- tm_map(ret, content_transformer(tolower))
    ret <- tm_map(ret, stripWhitespace)
    # ret <- tm_map(ret, stemDocument)
    
    ret <- tm_map(ret, removePunctuation)
    ret <- tm_map(ret, removeNumbers)
    # do not remove stop-word because they're important in phrase construction
    # ret<-tm_map(ret, removeWords, stopwords('english'))
    return(ret)
}

makeCorpus <- function(data) {
    corpus_all <- preProcess(VCorpus(VectorSource(data)))
    #corpus_longform <- preProcess(VCorpus(VectorSource(sample_longform)))
    #corpus_shortform <- preProcess(VCorpus(VectorSource(sample_shortform)))
    return(corpus_all)
}

# term matrices for all tokenizers
# use removeSparseTerms to remove very infrequent terms
make_dtm <- function(corpus) {
    dtm_all_1 <- removeSparseTerms(TermDocumentMatrix(corpus_all), 0.999)
}
# term matrices for long-form tokenizers
# dtm_longform_1 <- removeSparseTerms(TermDocumentMatrix(corpus_longform), 0.999)

# term matrices for short-form tokenizers
# dtm_shortform_1 <- removeSparseTerms(TermDocumentMatrix(corpus_shortform), 0.999)

calc_frequency <- function(tdm) {
    f <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
    df <- data.frame(word=names(f), freq=f)
    return(df)
}

ngramTokenizer <- function(corpus, n) unlist(lapply(ngrams(words(corpus), n), paste, collapse = " "), use.names = FALSE)
tokenize <- function(corpus, ngrams) {
    NgramTokenizer <- function(x) ngramTokenizer(x, ngrams)
    dtm <- removeSparseTerms(TermDocumentMatrix(corpus, control=list(tokenize=NgramTokenizer)), 0.999)
    return(dtm)
}


processTwitter <- function() {
    dataFile <- 'en_US.twitter'
    f_source = sprintf("%s.%s", dataFile, "txt")
    f_sample = sprintf("./intermediate_data/sample_%s.Rda", dataFile)
    f_corpus = sprintf("./intermediate_data/corpus_%s.Rda", dataFile)
    f_dtm = sprintf("./intermediate_data/dtm_%s.Rda", dataFile)
    if (file.exists(f_dtm)) {
        # dtm <- readRDS(f_dtm)
        print(sprintf("DTM already exists: %s", f_dtm))
    } else {
        if (file.exists(f_corpus)) {
            corpus <- readRDS(f_corpus)
        } else {
            if(file.exists(f_sample)) {
                data <- readRDS(f_sample)
            } else {
                data_twitter <- readSourceData(c(f_source))
                data <- makeSample(data_twitter, samplePercent = 1); rm(data_twitter)
                saveRDS(data, f_sample)
            }
            corpus <- makeCorpus(data); rm(data)
            saveRDS(corpus, f_corpus)
        }
        dtm <- tokenize(corpus, 2); rm(corpus)
        saveRDS(dtm, f_dtm)
    }
}