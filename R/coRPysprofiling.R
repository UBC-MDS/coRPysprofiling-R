library(tokenizers)
library(stopwords)

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
