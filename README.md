
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coRPysprofiling

<!-- badges: start -->

[![R-CMD-check](https://github.com/UBC-MDS/coRPysprofiling-R/workflows/R-CMD-check/badge.svg)](https://github.com/UBC-MDS/coRPysprofiling-R/actions)
[![codecov](https://codecov.io/gh/UBC-MDS/corpysprofiling-R/branch/master/graph/badge.svg)](https://codecov.io/gh/UBC-MDS/coRPysprofiling-R)
<!-- badges: end -->

## Summary

coRPysprofiling is an open-source library designed to bring exploratory
data analysis and visualization to the domain of natural language
processing. Functions in the package will be used to provide some
elementary statistics and visualizations for a single text corpus or
provide functions to compare multiple corpora with each other.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/coRPysprofiling-R")
```

## Features

Some specific functions include:

  - corpus\_analysis: corpus analysis will generate a statistical report
    about the characteristics of a single corpus (e.g. unique word
    count, average word/sentence length, top words used, topic
    analysis).
  - corpus\_viz: corpus\_viz will generate relevant visualizations of a
    single corpus (e.g. word cloud, histograms for average word/sentence
    length, top words used).
  - corpora\_compare: Given two or more corpora, corpora\_compare will
    find similarity (e.g, Euclidean distance or cosine similarity)
    between each pair of corpora.
  - corpora\_best\_match: Given a reference document and two or more
    corpora, corpora\_best\_match will rank the corpora in the order of
    most relevance to the reference document.

## Relevance to the R Ecosystem

To our knowledge, while `wordcloud` library generates wordcloud
visualization for a given corpus, there is no general-purpose library
for exploratory analysis and visualization of a text corpus in the R
ecosystem. There are several advanced libraries for comparing
similarities between different corpora: most notably, `quanteda`
provides similarity comparison between large corpora using word
embeddings. We believe that coRPysprofiling will provide some useful
functionality for exploratory analysis and visualization and help bridge
the gap between elementary text analysis to more sophisticated
approaches utilizing word embeddings.

## Dependencies

  - dplyr
  - ggplot2
  - ggwordcloud
  - stringr
  - stringi
  - here
  - stopwords
  - tokenizers
  - word2vec

## Usage

See vignette here: <https://ubc-mds.github.io/coRPysprofiling-R/>

## Documentation

The help file can be viewed by:

``` r
?corpus_analysis
#> No documentation for 'corpus_analysis' in specified packages and libraries:
#> you could try '??corpus_analysis'
?corpus_viz
#> No documentation for 'corpus_viz' in specified packages and libraries:
#> you could try '??corpus_viz'
?corpora_compare
#> No documentation for 'corpora_compare' in specified packages and libraries:
#> you could try '??corpora_compare'
?corpora_best_match
#> No documentation for 'corpora_best_match' in specified packages and libraries:
#> you could try '??corpora_best_match'
```

## Contributors

We welcome and recognize all contributions. You can see a list of
current contributors in the contributors tab.

#### Development Team

Anita Li, Elanor Boyle-Stanley, Junghoo Kim, and Ivy Zhang
