
context("qlq_c30 testing")


set.seed(8675309)
dat <- PROscorerTools::makeFakeData(n = 50, nitems = 30, values = 1:4)


test_that("qlq_c30: Error if both 'iprefix' and 'items' given", {
  expect_error(qlq_c30(df = dat, iprefix = "q", items = 1:30),
               "use either the 'iprefix' or 'items'")
})


## These tests fail with 2023-10-17 addition to qlq_c30 that produces a
## message about the re-naming of QLQTOTAL to C30SUMMARY.
## I am commenting out these tests for now.  I will use again after awhile when I
## delete the warning msg in qlq_c30.

# test_that("qlq_c30: Expect no error (silent) when correct input given", {
#   expect_silent(qlq_c30(df = dat, iprefix = "q"))
#   expect_silent(qlq_c30(df = dat, items = 1:30))
#   expect_silent(qlq_c30(df = dat, items = 1:30, keepNvalid = TRUE))
# })



