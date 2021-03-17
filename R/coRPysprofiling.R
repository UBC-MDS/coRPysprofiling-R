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
      word2vec::read.word2vec(file=file_path, normalize=TRUE)
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
      download.file(file_urls, destfile = dest_files, method = "libcurl", mode = "wb")
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
      model <- word2vec::read.word2vec(file=file_path, normalize = TRUE)
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
  if (!is.character(corpus) || length(corpus) != 1) {
    stop("input for corpus must be a character vector of length one")
  }
  
  if (!is.character(ignore)) {
    stop("input for ignore must be a character vector")
  }
  
  tokenizers::tokenize_words(corpus, stopwords=ignore)
}


#' Generate basic statistic for words from the input corpus
#'
#' @param corpus character vector representing a corpus
#'
#' @return data.frame
#'
#' @examples
#' corpus_analysis("How many animals in Russia? and how many in US?")
#' corpus_analysis("How many animals in Russia? and how many in US?")["word_total", ]
corpus_analysis <- function(corpus) {
  if (!is.character(corpus)|length(corpus) != 1) {
    stop("input must be a character vector of length one")
  }

  # get list of tokens and clean tokens from corpus
  token <- clean_tokens(corpus, ignore = '')[[1]]
  token_clean <- clean_tokens(corpus)[[1]]

  # get basic statistics of tokens
  word_total <- length(token)
  token_total <- length(token_clean)
  token_unique <- length(unique(token_clean))
  token_avg_len <- mean(stringi::stri_length(token_clean))

  # get sentences from corpus
  sents <- tokenizers::tokenize_sentences(corpus)[[1]]
  sents_tokenize <- unlist(lapply(sents, clean_tokens), recursive = FALSE)

  # get statistics of sentences
  sent_count <- length(sents)
  sens_avg_token <- mean(lengths(sents_tokenize))

  # organize values into list and output table
  value <- c(word_total, token_total, token_unique, token_avg_len, sent_count,
             sens_avg_token)
  value <- unlist(lapply(value, round, 1))
  output_df <- data.frame(value)
  # add row names as index
  row.names(output_df) <- c("word_total", "token_total", "token_unique",
                            "token_avg_len", "sent_count", "sens_avg_token")

  return(output_df)

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

if (!is.character(corpus) || length(corpus) != 1) {
    stop("input must be a character vector of length 1")
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
#' @param model_name character vector, optional (default : "cb_ns_500_10")
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
  emb1 <- word2vec::doc2vec(model, corpus1, type = "embedding")
  emb2 <- word2vec::doc2vec(model, corpus2, type = "embedding")

  if (metric == "cosine_similarity"){
    score <- 1 - (
      emb1 %*% t(emb2) / sqrt(sum(emb1^2)*sum(emb2^2))
    )
  }

  if (metric == "euclidean"){
    score <- sqrt(sum((emb1-emb2)^2))
  }

  # garbage collection because model hogs memory
  model <- NULL
  gc()

  abs(as.numeric(score))
}

#' Returns a tibble of distances from the reference document for each corpus in a vector of corpora.
#' This tibble is sorted in the order of increasing distance.
#'
#' @param refDoc character vector for reference document
#' @param corpora character vector for corpora
#' @param metric character vector for metric used to calculate distance, optional (default : "cosine_similarity")
#' @param model_name character vector, optional (default : "cb_ns_500_10")
#'
#' @return tibble
#' @export
#'
#' @examples
#' corpora_best_match("kitten meows", c("ice cream is yummy", "cat meowed", "dog barks", "The Hitchhiker's Guide to the Galaxy has become an international multi-media phenomenon"))
corpora_best_match <- function(refDoc, corpora, metric="cosine_similarity", model_name="cb_ns_500_10") {
  if (!is.character(refDoc) || length(refDoc) != 1) {
    stop("refDoc must be a character vectors of length one")
  }

  if (!is.character(corpora)) {
    stop("corpora must be a character vector")
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

  distances = length(corpora)
  for (i in seq_along(corpora)) {
    distances[i] <- corpora_compare(refDoc, corpora[i], metric=metric, model_name=model_name)
  }
  dist_df <- dplyr::tibble(corpora = corpora, metric = distances) %>%
    dplyr::arrange(metric)
  dist_df
}
