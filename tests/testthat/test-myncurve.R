test_that("multiplication works", {
  v <- myncurve(4,2,6)
  expect_equal(v$mu, 4)
  expect_equal(v$sigma, 2)
  expect_equal(v$area, round(pnorm(6,4,2),4))
})
