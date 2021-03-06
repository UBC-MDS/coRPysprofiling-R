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
