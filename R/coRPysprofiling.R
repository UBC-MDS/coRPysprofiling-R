
library(word2vec)
library(here)
library(tokenizers)
library(stopwords)
library(googledrive)
library(purrr)
library(here)
library(dplyr)
library(word2vec)


#' Download and load pretrained word2vector models (https://github.com/maxoodf/word2vec#basic-usage)
#'
#' @param dir character vector for name of dir where pretrained models will be downloaded, optional (default: "data")
#' @param model_name character vector for name of w2v pretrained model file, optional (default: "cb_ns_500_10.w2v")
#'
#' @return word2vec model object
#' @export
#'
#' @examples
#' load_pretrained()
#' load_pretrained(model_name = "sg_hs_500_10.w2v")
load_pretrained <- function(dir = "data", model_name = "cb_ns_500_10.w2v") {
  urls <- c("https://drive.google.com/file/d/0B1shHLc2QTzzV1dhaVk1MUt2cmc",
            "https://drive.google.com/file/d/0B1shHLc2QTzzTVZESDFpQk5jNG8",
            "https://drive.google.com/file/d/0B1shHLc2QTzzZl9vSS1FOFh1N0k",
            "https://drive.google.com/file/d/0B1shHLc2QTzzWFhpX2kwbWRkaWs")

  names(urls) <- c("cb_hs_500_10.w2v",
                   "cb_ns_500_10.w2v",
                   "sg_hs_500_10.w2v",
                   "sg_ns_500_10.w2v")

  if (!(model_name %in% names(urls))) {
    stop(paste0(c("model_name should be one of: ",
                  paste0(names(urls), collapse=', '))))
  }

  folder_url <- urls[[model_name]]

  path <- here::here(dir, model_name)

  # create directory if it does not exist
  try({
    dir.create(dir, showWarnings = FALSE)
  })

  # download the model if file at the given path does not already exist
  tryCatch(
    expr = {
      read.word2vec(file=path, normalize=TRUE)
      message("Downloaded model found. Loading downloaded model...")
      },
    error = function(cond) {
      message("Downloading pretrained model for sentence embedding. This may take up to 10 minutes with stable internet connection...")
      pretrained <- drive_get(as_id(folder_url))
      drive_download(pretrained, path=path, overwrite = TRUE)
      message("Download Complete!")
      },
    warning = function(cond) {
      message("Warning message while trying to load pretrained model: ")
      message(cond)
      },
    finally={
      model <- read.word2vec(file = path, normalize = TRUE)
      }
    )
  model
}


#' Tokenize words, and remove stopwords from corpus
#'
#' @param corpus character vector representing a corpus
#' @param ignore stopwords to ignore, optional (default: common English words and punctuations)
#'
#' @return character vector of word tokens
#'
#' @examples
#' clean_tokens ("How many species of animals are there in Russia?")
#' clean_tokens("How many species of animals are there in Russia?", ignore='!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~')
clean_tokens <- function(corpus, ignore=stopwords::stopwords("en")) {
  tokenizers::tokenize_words(corpus, stopwords=ignore)
}


#' Generate basic statistic for words from the input corpus
#'
#' @param corpus character vector representing a corpus
#'
#' @return tibble
#'
#' @examples
#' corpus_analysis("How many animals in Russia?")
corpus_analysis <- function(corpus) {

}


#' Generate visualizations for words from the input corpus
#'
#' @param corpus a character vector representing a corpus
#'
#' @return a list of a word cloud, a histogram of word length frequencies, and a histogram of word frequencies 
#'
#' @examples
#' coRPysprofiling::corpus_viz("some text")
#' coRPysprofiling::corpus_viz("some text")['word cloud']
#' coRPysprofiling::corpus_viz("some text")['word freq bar chart']
#' coRPysprofiling::corpus_viz("some text")['word length bar chart']
corpus_viz <- function(corpus) {

if (!is.character(corpus)) {
    stop("inputs must be a character")
  }

# Step 1. To prepare the data frame df and df_30 where df will be used to 
# plot the word cloud, and df_30 will be used to generate the other charts
    
## To get a list of words from the input text
clean_corpus <- clean_tokens(tolower(corpus))

## Convert the list of words into sorted frequency table
df <- as.data.frame(table(clean_corpus))
names(df)[1] <- 'word'
    
## To limit the number of words to display in the plot
## Select top 30 most frequent words to display
df_30 <- head(df[rev(order(df$Freq)),], 30)

## Add word length
df_30$length = stringr::str_length(df_30$word)

# Step 2. To prepare data visualization
## To get a word cloud
    
# Method 1
# wordcloud::wordcloud(words = df$word, freq = df$Freq, min.freq = 1, 
#                 max.words=250, random.order=FALSE, rot.per=0.3, 
#                 colors=RColorBrewer::brewer.pal(5, "Paired"))

    
# Method 2
wc <- ggplot2::ggplot(
    df, ggplot2::aes(label = word, size = Freq, color = Freq,
                    angle =  sample(c(0, 90), size = nrow(df), prob = rep(c(3/5, 2/5)), replace = TRUE))) +
    ggwordcloud::geom_text_wordcloud_area(
        area_corr_power = 1.75, rm_outside=TRUE, shape = 'pentagon') +
    ggplot2::scale_size_area(max_size = 15) +
    ggplot2::theme_minimal() +
    ggplot2::scale_alpha()
    
## To get a histogram of word frequencies 
bar_freq <- ggplot2::ggplot(df_30, ggplot2::aes(x = reorder(word, -Freq), y = Freq)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::labs(x = 'Words', y = 'Frequency', title = 'Frequency of Words') +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

## To get a histogram of word length frequencies
bar_length <- ggplot2::ggplot(df_30, ggplot2::aes(x = length)) +
    ggplot2::geom_bar(stat = 'count') +
    ggplot2::labs(x = 'Word Length', y = 'Frequency', title = 'Frequency of Words by Length') +
    ggplot2::theme_minimal(base_size = 14)
    
## Summarize all plots into a list
l <- list(wc, bar_freq, bar_length)
names(l) <- c('word cloud', 'word freq bar chart', 'word length bar chart')
    
return(l)
}

#' Returns a numeric vector of the distance between the two corpora
#'
#' @param corpus1 character vector
#' @param corpus2 character vector
#' @param metric character vector, optional (default : "cosine_similarity")
#'
#' @return double vector
#' @export
#'
#' @examples
#' corpora_compare("kitten meows", "ice cream is yummy")
corpora_compare <- function(corpus1, corpus2, metric = "cosine_similarity", model_name = "cb_ns_500_10.w2v") {
  if (!is.character(corpus1) || !is.character(corpus2) || length(corpus1) != 1 || length(corpus2) != 1) {
    stop("inputs must be character vectors of length one")
  }

  if (length(metric) != 1 || !(metric %in% c("cosine_similarity", "euclidean"))) {
    stop("metric must be cosine_similarity or euclidean")
  }

  names <- c("cb_hs_500_10.w2v",
             "cb_ns_500_10.w2v",
             "sg_hs_500_10.w2v",
             "sg_ns_500_10.w2v")

  if (!(model_name %in% names) || length(model_name) != 1) {
    stop(paste0(c("model_name should be one of: ",
                  paste0(names, collapse=', '))))
  }

  model <- load_pretrained(model_name = model_name)
  emb1 <- doc2vec(model, corpus1, type = "embedding")
  emb2 <- doc2vec(model, corpus2, type = "embedding")

  if (metric == "cosine_similarity"){
    score <- 1 - (
      emb1 %*% t(emb2) / sqrt(sum(emb1^2)*sum(emb2^2))
    )
  }

  if (metric == "euclidean"){
    score <- sqrt(sum((emb1-emb2)^2))
  }

  abs(as.numeric(score))
}

#' Returns a tibble of distances from the reference document for each corpus in a vector of corpora.
#' This tibble is sorted in the order of increasing distance.
#'
#' @param refDoc character vector for reference document
#' @param corpora character vector for corpora
#' @param metric character vector for metric used to calculate distance, optional (default : "cosine_similarity")
#'
#' @return tibble
#' @export
#'
#' @examples
#' corpora_best_match(("kitten meows", c("ice cream is yummy", "cat meowed", "dog barks", "The Hitchhiker's Guide to the Galaxy has become an international multi-media phenomenon"))
corpora_best_match <- function(refDoc, corpora, metric="cosine_similarity") {

}
