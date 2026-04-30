test_that("dtriang funciona", {
  expect_equal(dtriang(0, 0, 1, 0.5), 0)
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_error(dtriang(0.5, 1, 0, 0.5))
})

test_that("ptriang funciona", {
  expect_equal(ptriang(0.5, 0, 1, 0.5), 0.5)
})

test_that("qtriang funciona", {
  expect_equal(qtriang(0.5, 0, 1, 0.5), 0.5)
})

test_that("rtriang funciona", {
  set.seed(123)
  expect_length(rtriang(10, 0, 1, 0.5), 10)
})
