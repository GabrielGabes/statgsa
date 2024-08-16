test_that("table response", {
  expect_s3_class(group_shapiro_test(mtcars, 'mpg', 'vs', type_response = 0), "data.frame")
  expect_s3_class(group_ks_test(mtcars, 'mpg', 'vs', type_response = 0), "data.frame")
  expect_s3_class(group_normality_test(mtcars, 'mpg', 'vs', type_response = 0), "data.frame")
})

test_that("binary response", {
  expect_equal(group_shapiro_test(mtcars, 'mpg', 'vs', type_response = 1), T)
  expect_equal(group_ks_test(mtcars, 'mpg', 'vs', type_response = 1), T)
  expect_equal(group_normality_test(mtcars, 'mpg', 'vs', type_response = 1), T)
})
