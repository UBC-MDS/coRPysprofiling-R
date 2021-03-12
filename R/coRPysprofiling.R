
library(word2vec)
library(here)
library(tokenizers)
library(stopwords)
library(purrr)
library(here)
library(dplyr)
library(word2vec)

#' Download and load pretrained word2vector models (https://github.com/maxoodf/word2vec#basic-usage)
#'
#' @param dir character vector for name of dir where pretrained models will be downloaded, optional (default: "data")
#' @param model_name character vector for name of pretrained model, optional (default: "cb_ns_500_10")
#'
#' @return word2vec model object
#' @export
#'
#' @examples
#' load_pretrained()
#' load_pretrained(model_name = "sg_hs_500_10")
load_pretrained <- function(dir = "data", model_name = "cb_ns_500_10") {
  model_names <- c("cb_hs_500_10",
                   "cb_ns_500_10",
                   "sg_hs_500_10",
                   "sg_ns_500_10")
  FILE_EXT <- ".w2v"

  if (!(model_name %in% model_names)) {
    stop(paste0(c("model_name should be one of: ",
                  paste0(model_names, collapse=', '))))
  }

  data_repo <- "https://raw.githubusercontent.com/UBC-MDS/glove_w2v_data/main/"

  dir_path <- here::here(dir, model_name)
  file_path <- here::here(dir, model_name, paste0(model_name, FILE_EXT))

  # download the model if file at the given path does not already exist
  tryCatch(
    expr = {
      read.word2vec(file=file_path, normalize=TRUE)
      message("Downloaded model found. Loading downloaded model...")
      },
    error = function(cond) {
      options(timeout=600)
      file_urls <- paste0(data_repo, model_name, "/", model_name, "w2v", 1:9, ".bin")
      dest_files <- here::here(dir, model_name, paste0(model_name, "w2v", 1:9, ".bin"))
      # create files if they do not exist
      try({
        dir.create(dir_path, recursive = TRUE, showWarnings=FALSE)
        file.create(dest_files)
      })
      message("Model was not found locally. Downloading and processing this pretrained model can take up to 20 minutes in total.")
      message("This will only need to be run once for each pretrained model.")
      message("Downloading pretrained model for sentence embedding. This part may take up to 10 minutes with stable internet connection...")
      for(i in seq_along(file_urls)){
        download.file(file_urls[i], destfile = dest_files[i])
      }
      message("Download Complete! Processing raw files. This part may also take up to 10 minutes...")
      raw_data <- list()
      files <- here::here(dir, model_name, dir(dir_path))
      for(i in seq_along(files)) raw_data[[i]] <- readBin(files[i], "raw", 99e6)
      raw_data <- do.call("c", raw_data)
      writeBin(raw_data, con=file_path)
      },
    warning = function(cond) {
      message("Warning message while trying to load pretrained model: ")
      message(cond)
      },
    finally={
      model <- read.word2vec(file=file_path, normalize = TRUE)
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
#' @param corpus a charactor vector representing a corpus
#' @param display a boolean (optional); If display is False, the plots will be hidden from the output
#'
#' @return a dictionary of a word cloud, and a data frame, which can be used to draw a bar chart for words and word lengths
#'
#' @examples
#' coRPysprofiling::corpus_viz("some text")
corpus_viz <- function(corpus, display=TRUE) {

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
corpora_compare <- function(corpus1, corpus2, metric = "cosine_similarity", model_name = "cb_ns_500_10") {
  if (!is.character(corpus1) || !is.character(corpus2) || length(corpus1) != 1 || length(corpus2) != 1) {
    stop("inputs must be character vectors of length one")
  }

  if (length(metric) != 1 || !(metric %in% c("cosine_similarity", "euclidean"))) {
    stop("metric must be cosine_similarity or euclidean")
  }

  names <- c("cb_hs_500_10",
             "cb_ns_500_10",
             "sg_hs_500_10",
             "sg_ns_500_10")

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
