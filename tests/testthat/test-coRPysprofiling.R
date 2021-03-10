# Test related to corpora_compare function
corpus1 <- "kitten meows"
corpus2 <- "kitten meows"

testCase1 <- corpora_compare(corpus1, corpus2)
testCase2 <- corpora_compare(corpus1, corpus2, metric="euclidean")

test_that("corpora_compare returns double vector", {
  expect_type(testCase1, "double")
})

test_that("Identical corpora should return score of 0.0", {
  expect_equal(testCase1, 0)
  expect_equal(testCase2, 0)
})
