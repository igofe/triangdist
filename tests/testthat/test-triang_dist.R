test_that("dtriang works for normal triangular distributions", {
  x <- c(-1, 0, 2.5, 5, 7.5, 10, 11)
  expected <- c(0, 0, 0.1, 0.2, 0.1, 0, 0)

  expect_equal(dtriang(x, min = 0, max = 10, mode = 5), expected)
})

test_that("dtriang works when the mode is at one side", {
  expect_equal(
    dtriang(c(0, 5, 10), min = 0, max = 10, mode = 0),
    c(0.2, 0.1, 0)
  )

  expect_equal(
    dtriang(c(0, 5, 10), min = 0, max = 10, mode = 10),
    c(0, 0.1, 0.2)
  )
})

test_that("ptriang computes cumulative probabilities", {
  q <- c(-1, 0, 2.5, 5, 7.5, 10, 11)
  expected <- c(0, 0, 0.125, 0.5, 0.875, 1, 1)

  expect_equal(ptriang(q, min = 0, max = 10, mode = 5), expected)
  expect_equal(ptriang(5, min = 0, max = 10, mode = 0), 0.75)
  expect_equal(ptriang(5, min = 0, max = 10, mode = 10), 0.25)
})

test_that("qtriang is the inverse of ptriang", {
  p <- c(0, 0.125, 0.5, 0.875, 1)
  q <- c(0, 2.5, 5, 7.5, 10)

  expect_equal(qtriang(p, min = 0, max = 10, mode = 5), q)
  expect_equal(ptriang(q, min = 0, max = 10, mode = 5), p)
})

test_that("qtriang works when the mode is at one side", {
  expect_equal(qtriang(0.75, min = 0, max = 10, mode = 0), 5)
  expect_equal(qtriang(0.25, min = 0, max = 10, mode = 10), 5)
})

test_that("functions accept vectorized parameters", {
  expect_equal(
    dtriang(c(2.5, 5), min = 0, max = 10, mode = c(5, 0)),
    c(0.1, 0.1)
  )

  expect_equal(
    qtriang(c(0.125, 0.75), min = 0, max = 10, mode = c(5, 0)),
    c(2.5, 5)
  )
})

test_that("invalid inputs give errors", {
  expect_error(
    dtriang(1, min = 1, max = 1, mode = 1),
    "min must be smaller"
  )

  expect_error(
    ptriang(1, min = 0, max = 10, mode = 11),
    "mode must be between"
  )

  expect_error(
    qtriang(1.1, min = 0, max = 1, mode = 0.5),
    "p must be between"
  )

  expect_error(
    rtriang(-1, min = 0, max = 1, mode = 0.5),
    "n must be"
  )
})

test_that("rtriang generates values inside the interval", {
  set.seed(123)
  samples <- rtriang(100, min = 0, max = 10, mode = 5)

  expect_length(samples, 100)
  expect_true(all(samples >= 0))
  expect_true(all(samples <= 10))
  expect_equal(rtriang(0, min = 0, max = 1, mode = 0.5), numeric())
  expect_length(rtriang(1:4, min = 0, max = 1, mode = 0.5), 4)
})
