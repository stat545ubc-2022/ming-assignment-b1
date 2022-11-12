test_that("the consecutive difference function works", {
  expect_error(consecutive_diff(c()), "Sorry, the input number sequence is empty, please input a non-empty number sequence!")
  expect_message(consecutive_diff(c(1,6,11,9, 2, NA)), "The input number sequence contains NAs.")
  expect_equal(consecutive_diff(c(replicate(5,1))), c(0,0,0,0))
})
