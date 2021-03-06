corpus_analysis <- function(corpus) {

}

corpus_viz <- function(corpus, stats) {

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
