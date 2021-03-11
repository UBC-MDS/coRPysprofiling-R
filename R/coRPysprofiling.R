library(tokenizers)
library(stopwords)
library(googledrive)
library(purrr)
library(here)
library(word2vec)


load_pretrained <- function(dir = "data", model_name = "pretrained.w2v") {
  path <- here::here(dir, model_name)

  # Adapted from https://community.rstudio.com/t/how-to-download-a-google-drives-contents-based-on-drive-id-or-url/16896/13
  # credits to Jenny Bryan
  
  ## store the URL you have
  folder_url <- "https://drive.google.com/file/d/0B1shHLc2QTzzTVZESDFpQk5jNG8"

  ## identify this folder on Drive
  ## let googledrive know this is a file ID or URL, as opposed to file name
  pretrained <- drive_get(as_id(folder_url))

  ## download the model if file at the given path does not already exist
  # TODO: have a robust way of checking that the model is correct (and not empty,for example)
  if (file.exists(path)) {
    paste0("Downloaded model found. Loading downloaded model...")
  }
  else {
    paste0("Downloading pretrained model for sentence embedding. This may take up to 10 minutes...")
    drive_download(pretrained, path=path, overwrite = TRUE)
    paste0("Download Complete!")
    
  }
  model <- read.word2vec(file = path, normalize = TRUE)
  model
}


#' Tokenize words, and remove stopwords from corpus
#'
#' @param corpus a string representing a corpus
#' @param ignore stopwords to ignore (optional, default: common English words and punctuations)
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
#' @param corpus a string representing a corpus
#'
#' @return tibble
#'
#' @examples
#' corpus_analysis("How many animals in Russia?")
corpus_analysis <- function(corpus) {

}


#' Generate visualizations for words from the input corpus
#'
#' @param corpus a string representing a corpus
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
#' @return numeric vector
#' @export
#'
#' @examples
#' corpora_compare("kitten meows", "ice cream is yummy")
corpora_compare <- function(corpus1, corpus2, metric="cosine_similarity") {

}

#' Returns a tibble of distances from the reference document for each corpus in a vector of corpora.
#' This tibble is sorted in the order of increasing distance.
#'
#' @param refDoc character vector
#' @param corpora character vector
#' @param metric character vector, optional (default : "cosine_similarity")
#'
#' @return tibble
#' @export
#'
#' @examples
#' corpora_best_match(("kitten meows", c("ice cream is yummy", "cat meowed", "dog barks", "The Hitchhiker's Guide to the Galaxy has become an international multi-media phenomenon"))
corpora_best_match <- function(refDoc, corpora, metric="cosine_similarity") {

}
