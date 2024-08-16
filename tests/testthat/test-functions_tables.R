test_that("frequency tables", {
  expect_s3_class(freq_table(mtcars, 'vs'), "data.frame")
  expect_s3_class(count_table(mtcars, 'vs', 'am'), "data.frame")
})

test_that("summary numerical overall", {
  expect_s3_class(summary_num_nonparametric(mtcars, 'mpg'), "data.frame")
  expect_s3_class(summary_num_parametric(mtcars, 'mpg'), "data.frame")
})

test_that("summary numerical for groups", {
  expect_s3_class(summary_num_nonparametric_groups(mtcars, 'mpg', "am"), "data.frame")
  expect_s3_class(summary_num_parametric_groups(mtcars, 'mpg', "am"), "data.frame")
})

test_that("function cross table", {
  expect_s3_class(adaptive_cross_table(mtcars, "am"), "data.frame")
})
