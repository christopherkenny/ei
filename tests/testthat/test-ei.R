# Test for the `ei()` function

context("`ei()` function")
data(matproii)

test_that("Running the function correctly", {
  set.seed(225)
  suppressMessages({
    dbuf <- ei(formula = t ~ x, total = "n", data = matproii[1:50, ])
  })
  dbuf_summary <- summary(dbuf)

  expect_equal(dbuf_summary$Erho, 0.5)
  expect_equal(dbuf_summary$Esigma, 0.5)
  expect_equal(dbuf_summary$Ebeta, 0.5)
  expect_equal(dbuf_summary$N, 50)
  expect_equal(dbuf_summary$Resamp, 39)

  expect_equal(dbuf_summary[[6]][1, 1], 1.581521, tolerance = 0.00001)
  expect_equal(dbuf_summary[[6]][2, 5], 1.124315, tolerance = 0.00001)

  expect_equal(dbuf_summary[[7]][2], 1.397145, tolerance = 0.00001)

  expect_equal(dbuf_summary[[8]][3], 0.1889316, tolerance = 0.00001)

  expect_equal(dbuf_summary[[9]][1, 1], 0.2152043, tolerance = 0.00001)
  expect_equal(dbuf_summary[[9]][2, 2], 0.9507709, tolerance = 0.00001)

  expect_equal(dbuf_summary[[10]][1, 1], 0.7029558, tolerance = 0.00001)
  expect_equal(dbuf_summary[[10]][2, 2], 0.01764645, tolerance = 0.00001)
})
