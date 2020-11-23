library(testthat)
library(adaboost)

test_check("adaboost")

context("File Name and File Type")

test_that("file type is correct", {
  expect_error(readFile("a.pdf"))
})

dFile <- system.file("extdata", "project.las", package = "adaboost")
test_that("Return type is LAS", {
  expect_equal(class(readFile(dFile))[1], "LAS")
})
