# Test related to clean_tokens helper function
corpus <- "How many species of animals are there in Russia? and how many in US"

testCase1 <- clean_tokens(corpus)[[1]]
testCase2 <- clean_tokens(corpus, ignore="")[[1]]

test_that("tokens returned by clean_tokens does not match the expected results", {
  expect_setequal(testCase1, c("many", "species", "animals", "russia", "many", "us"))
  expect_setequal(testCase2, c("how", "many", "species", "of", "animals", "are", "there", "in", "russia", "and", "how", "many", "in", "us"))
})

test_that("Exception handling working as intendend", {
  expect_error(clean_tokens(123), "input for corpus must be a character vector of length one")
  expect_error(clean_tokens(c("Hello!", "Bye!")), "input for corpus must be a character vector of length one")
  expect_error(clean_tokens(corpus, ignore=123), "input for ignore must be a character vector")
})


# Test related to corpora_analysis function
corpus <- "How many species of animals are there in Russia? and how many in US"

testCase1 <- corpus_analysis(corpus)

test_that("corpora_compare returns data frame with 1 column and 6 rows", {
  expect_equal(nrow(testCase1), 6)
  expect_equal(ncol(testCase1), 1)
})

test_that("total words of this corpus should be 14", {
  expect_equal(testCase1["word_total", ], 14)
})

test_that("Exception handling working as intendend", {
  expect_error(corpus_analysis(123), "input must be a character vector of length one")
  expect_error(corpus_analysis(c("Hello!", "Bye!")), "input must be a character vector of length one")
})


# Test related to corpus_viz function
corpus1 <- "How many species of animals are there in Russia?"
corpus2 <- "Let's assume that this unsupervised model is used to 
assist human experts to identify fraud transaction. So instead of making 
humans examine all 284,807 transactions for fraud transactions, this model 
would extract transactions which look suspicious and pass them to humans 
for examination. So our goal is to come up with a list of transactions 
which look suspicious.We said before that PCA won't be able to capture 
characteristic features of fraud transactions because they are like 
outliers (occur very rarely) in our dataset, and so the reconstruction 
error would be higher for them compared to non-fraud transactions. But 
what do we mean by high reconstruction error? What should be threshold 
which makes a transaction suspicious?"

testCase1 <- corpus_viz(corpus1)
testCase2 <- corpus_viz(corpus2)

# Test whether the corpus_viz returns a list
test_that("corpus_viz returns a list", {
    expect_type(testCase1, 'list')
    expect_type(testCase2, 'list')
})

# Tests whether the corpus_viz[['some_corpus']] returns a ggplot
test_that("Return type is a ggplot", {
    expect_s3_class(testCase1[['word cloud']], 'ggplot')
    expect_s3_class(testCase1[['word freq bar chart']], 'ggplot')
    expect_s3_class(testCase1[['word length bar chart']], 'ggplot')
    expect_s3_class(testCase2[['word cloud']], 'ggplot')
    expect_s3_class(testCase2[['word freq bar chart']], 'ggplot')
    expect_s3_class(testCase2[['word length bar chart']], 'ggplot')
})


test_that("Test whether the word cloud displays as expected", {
    expect_equal(summary(testCase1[['word cloud']]$data$word)['animals'][[1]], 1)
    expect_equal(summary(testCase1[['word cloud']]$data$Freq)['Max.'][[1]], 1)
    expect_equal(summary(testCase2[['word cloud']]$data$word)['capture'][[1]], 1)
    expect_equal(summary(testCase2[['word cloud']]$data$Freq)['Max.'][[1]], 6)    
})

test_that("Test whether the freq bar chart displays as expected", {
    expect_true(testCase1[['word freq bar chart']]$data$word[1] == 'species')
    expect_true(testCase1[['word freq bar chart']]$data$Freq[1] == 1)
    expect_equal(summary(testCase1[['word freq bar chart']]$data$word)['animals'][[1]], 1)
    expect_equal(summary(testCase2[['word freq bar chart']]$data$Freq)['Max.'][[1]], 6)    
    expect_true(nrow(testCase2[['word freq bar chart']][1]$data) <= 30)
})

test_that("Test whether the length bar chart displays as expected", {
    expect_true(testCase1[['word length bar chart']]$data$word[1] == 'species')
    expect_true(testCase1[['word length bar chart']]$data$length[[1]] == 7)
    expect_equal(summary(testCase1[['word length bar chart']]$data$word)['animals'][[1]], 1)
    expect_equal(summary(testCase2[['word length bar chart']]$data$length)['Max.'][[1]], 14)    
    expect_true(nrow(testCase2[['word length bar chart']][1]$data) <= 30)
})

# Test related to corpora_compare function
corpus1 <- "kitten meows"
corpus2 <- "kitten meows"

testCase1 <- corpora_compare(corpus1, corpus2)
testCase2 <- corpora_compare(corpus1, corpus2, metric="euclidean")

test_that("corpora_compare returns double vector of length 1", {
  expect_type(testCase1, "double")
  expect_length(testCase1, 1)
})

test_that("Identical corpora should return score of 0.0", {
  expect_equal(testCase1, 0)
  expect_equal(testCase2, 0)
})

test_that("Distances should be between 0 and 2 inclusive for cosine_similarity and greater than 0 for euclidean", {
  expect_lte(testCase1, 2)
  expect_gte(testCase1, 0)
  expect_gte(testCase2, 0)
})

test_that("Exception handling working as intended", {
  expect_error(corpora_compare(corpus1, 123), "inputs must be character vectors of length one")
  expect_error(corpora_compare(c(corpus1, corpus2), corpus1), "inputs must be character vectors of length one")
  expect_error(corpora_compare(123, corpus1), "inputs must be character vectors of length one")
  expect_error(corpora_compare(123, list(1, 2, 3)), "inputs must be character vectors of length one")
  expect_error(corpora_compare(corpus1, corpus2, metric = "other"), "metric must be cosine_similarity or euclidean")
  expect_error(corpora_compare(corpus1, corpus2, metric = c("cosine_similarity","euclidean")), "metric must be cosine_similarity or euclidean")
  expect_error(corpora_compare(corpus1, corpus2, model_name = "other"),"model_name should be one of: cb_hs_500_10, cb_ns_500_10, sg_hs_500_10, sg_ns_500_10")
  expect_error(corpora_compare(corpus1, corpus2, model_name = c("cb_hs_500_10", "cb_ns_500_10")), "model_name should be one of: cb_hs_500_10, cb_ns_500_10, sg_hs_500_10, sg_ns_500_10")
})

# Test related to corpora_best_match function
refDoc = "kitten meows"
corpora = c("kitten meows", "ice cream is yummy", "cat meowed", "dog barks")
testCase1 = corpora_best_match(refDoc, corpora)
testCase2 = corpora_best_match(refDoc, corpora, metric="euclidean")

test_that("corpora_best_match should return tibble with 4 rows and 2 columns", {
  # We expect a tibble...
  expect_s3_class(testCase1, "tbl")
  expect_s3_class(testCase2, "tbl")
  # with 4 rows...
  expect_equal(nrow(testCase1), 4)
  expect_equal(nrow(testCase2), 4)
  # and 2 columns
  expect_length(testCase1, 2)
  expect_length(testCase2, 2)
})

test_that("corpora column in corpora_best_match should match the corpora passed to the function", {
  expect_setequal(testCase1$corpora, corpora)
  expect_setequal(testCase2$corpora, corpora)
})

test_that("distances should be of type double, and between 0 and 2 inclusive for cosine_similarity and greater than or equal to 0 for euclidean", {
  expect_type(testCase1$metric, "double")
  expect_true(all(testCase1$metric >= 0))
  expect_true(all(testCase1$metric <= 2))
  
  expect_type(testCase2$metric, "double")
  expect_true(all(testCase2$metric >= 0))
})  

test_that("Exception handling working as intended", {
  expect_error(corpora_best_match(refDoc, 123), "corpora must be a character vector")
  expect_error(corpora_best_match(refDoc, list()), "corpora must be a character vector")
  expect_error(corpora_best_match(c(refDoc, refDoc), corpora), "refDoc must be a character vectors of length one")
  expect_error(corpora_best_match(123, corpora), "refDoc must be a character vectors of length one")
  expect_error(corpora_best_match(list(1, 2, 3), corpora), "refDoc must be a character vectors of length one")
  expect_error(corpora_best_match(refDoc, corpora, metric = "other"), "metric must be cosine_similarity or euclidean")
  expect_error(corpora_best_match(refDoc, corpora, metric = c("cosine_similarity","euclidean")), "metric must be cosine_similarity or euclidean")
  expect_error(corpora_best_match(refDoc, corpora, model_name = "other"),"model_name should be one of: cb_hs_500_10, cb_ns_500_10, sg_hs_500_10, sg_ns_500_10")
  expect_error(corpora_best_match(refDoc, corpora, model_name = c("cb_hs_500_10", "cb_ns_500_10")),"model_name should be one of: cb_hs_500_10, cb_ns_500_10, sg_hs_500_10, sg_ns_500_10")
})

