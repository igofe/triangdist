test_that("dtriang computes densities for the interior mode case", {
  x <- c(-1, 0, 2.5, 5, 7.5, 10, 11)
  expected <- c(0, 0, 0.1, 0.2, 0.1, 0, 0)

  expect_equal(dtriang(x, min = 0, max = 10, mode = 5), expected)
})

test_that("dtriang handles endpoint modes", {
  expect_equal(
    dtriang(c(0, 5, 10), min = 0, max = 10, mode = 0),
    c(0.2, 0.1, 0)
  )
  expect_equal(
    dtriang(c(0, 5, 10), min = 0, max = 10, mode = 10),
    c(0, 0.1, 0.2)
  )
})

test_that("ptriang computes probabilities for the interior mode case", {
  q <- c(-1, 0, 2.5, 5, 7.5, 10, 11)
  expected <- c(0, 0, 0.125, 0.5, 0.875, 1, 1)

  expect_equal(ptriang(q, min = 0, max = 10, mode = 5), expected)
})

test_that("ptriang handles endpoint modes", {
  expect_equal(
    ptriang(c(0, 5, 10), min = 0, max = 10, mode = 0),
    c(0, 0.75, 1)
  )
  expect_equal(
    ptriang(c(0, 5, 10), min = 0, max = 10, mode = 10),
    c(0, 0.25, 1)
  )
})

test_that("qtriang inverts ptriang", {
  probabilities <- c(0, 0.125, 0.5, 0.875, 1)
  quantiles <- c(0, 2.5, 5, 7.5, 10)

  expect_equal(qtriang(probabilities, min = 0, max = 10, mode = 5), quantiles)
  expect_equal(
    ptriang(quantiles, min = 0, max = 10, mode = 5),
    probabilities
  )
})

test_that("qtriang handles endpoint modes", {
  expect_equal(
    qtriang(c(0, 0.75, 1), min = 0, max = 10, mode = 0),
    c(0, 5, 10)
  )
  expect_equal(
    qtriang(c(0, 0.25, 1), min = 0, max = 10, mode = 10),
    c(0, 5, 10)
  )
})

test_that("functions recycle vector inputs", {
  expect_equal(
    dtriang(c(2.5, 5), min = 0, max = 10, mode = c(5, 0)),
    c(0.1, 0.1)
  )
  expect_equal(
    qtriang(c(0.125, 0.75), min = 0, max = 10, mode = c(5, 0)),
    c(2.5, 5)
  )
})

test_that("non-conformable recycling emits a base-R-style warning", {
  expect_warning(
    dtriang(1:3, min = c(0, 0), max = 4, mode = 2),
    "longer object length"
  )
})

test_that("validation catches invalid parameters", {
  expect_error(
    dtriang(1, min = 1, max = 1, mode = 1),
    "`min` must be strictly smaller"
  )
  expect_error(
    ptriang(1, min = 0, max = 10, mode = 11),
    "`mode` must be in the interval"
  )
  expect_error(
    qtriang(c(0.5, 1.1), min = 0, max = 1, mode = 0.5),
    "`p` must contain values"
  )
  expect_error(
    rtriang(-1, min = 0, max = 1, mode = 0.5),
    "`n` must be a non-negative"
  )
})

test_that("missing and zero-length inputs behave consistently", {
  expect_equal(dtriang(numeric(), min = 0, max = 1, mode = 0.5), numeric())
  expect_true(is.na(dtriang(NA_real_, min = 0, max = 1, mode = 0.5)))
  expect_true(is.na(ptriang(NA_real_, min = 0, max = 1, mode = 0.5)))
  expect_true(is.na(qtriang(NA_real_, min = 0, max = 1, mode = 0.5)))
})

test_that("rtriang generates values through qtriang", {
  set.seed(123)
  values <- rtriang(100, min = 0, max = 10, mode = 5)

  expect_length(values, 100)
  expect_true(all(values >= 0 & values <= 10))
  expect_equal(rtriang(0, min = 0, max = 1, mode = 0.5), numeric())
  expect_length(rtriang(1:4, min = 0, max = 1, mode = 0.5), 4)
})
