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
    expect_is(testCase1[['word cloud']], 'ggplot')
    expect_is(testCase1[['word freq bar chart']], 'ggplot')
    expect_is(testCase1[['word length bar chart']], 'ggplot')
    expect_is(testCase2[['word cloud']], 'ggplot')
    expect_is(testCase2[['word freq bar chart']], 'ggplot')
    expect_is(testCase2[['word length bar chart']], 'ggplot')
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

test_that("Distances should be between 0 and 1 inclusive for cosine_similarity and greater than 0 for euclidean", {
  expect_lte(testCase1, 1)
  expect_gte(testCase1, 0)
  expect_gte(testCase2, 0)
})

test_that("Exception handling working as intendend", {
  expect_error(corpora_compare(corpus1, 123), "inputs must be character vectors of length one")
  expect_error(corpora_compare(c(corpus1, corpus2), corpus1), "inputs must be character vectors of length one")
  expect_error(corpora_compare(123, corpus1), "inputs must be character vectors of length one")
  expect_error(corpora_compare(123, list(1, 2, 3)), "inputs must be character vectors of length one")
  expect_error(corpora_compare(corpus1, corpus2, metric = "other"), "metric must be cosine_similarity or euclidean")
  expect_error(corpora_compare(corpus1, corpus2, metric = c("cosine_similarity","euclidean")), "metric must be cosine_similarity or euclidean")
  expect_error(corpora_compare(corpus1, corpus2, model_name = "other"),"model_name should be one of: cb_hs_500_10.w2v, cb_ns_500_10.w2v, sg_hs_500_10.w2v, sg_ns_500_10.w2v")
  expect_error(corpora_compare(corpus1, corpus2, model_name = c("cb_hs_500_10.w2v", "cb_ns_500_10.w2v")),"model_name should be one of: cb_hs_500_10.w2v, cb_ns_500_10.w2v, sg_hs_500_10.w2v, sg_ns_500_10.w2v")
})
