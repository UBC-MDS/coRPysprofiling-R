options(gargle_oob_default = TRUE)

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

test_that("Exception handling working as intended", {
  expect_error(corpora_compare(corpus1, 123), "inputs must be character vectors of length one")
  expect_error(corpora_compare(c(corpus1, corpus2), corpus1), "inputs must be character vectors of length one")
  expect_error(corpora_compare(123, corpus1), "inputs must be character vectors of length one")
  expect_error(corpora_compare(123, list(1, 2, 3)), "inputs must be character vectors of length one")
  expect_error(corpora_compare(corpus1, corpus2, metric = "other"), "metric must be cosine_similarity or euclidean")
  expect_error(corpora_compare(corpus1, corpus2, metric = c("cosine_similarity","euclidean")), "metric must be cosine_similarity or euclidean")
  expect_error(corpora_compare(corpus1, corpus2, model_name = "other"),"model_name should be one of: cb_hs_500_10, cb_ns_500_10, sg_hs_500_10, sg_ns_500_10")
  expect_error(corpora_compare(corpus1, corpus2, model_name = c("cb_hs_500_10", "cb_ns_500_10")),"model_name should be one of: cb_hs_500_10, cb_ns_500_10, sg_hs_500_10, sg_ns_500_10")
})
