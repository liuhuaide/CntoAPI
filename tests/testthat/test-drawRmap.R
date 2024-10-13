library(testthat)


test_that("Test downloading a big query (large area)", {
  result <- drawRmap("Asia", maptype = "stamen_toner", zoom = 10)
  expect_s3_class(result, "gg")
})


test_that("Test API limits with large geographical area", {
  result <- drawRmap("Earth", maptype = "stamen_toner", zoom = 5)
  expect_s3_class(result, "gg")
  expect_true(!is.null(result), "The map result should not be NULL.")
})

test_that("Test API rate limit by making multiple requests", {
  for (i in 1:5) {
    result <- drawRmap("Harvard University", maptype = "outdoors", zoom = 11)
    expect_s3_class(result, "gg")
  }
})


test_that("Test valid inputs for drawRmap function", {
  result <- drawRmap("Harvard University", maptype = "outdoors", zoom = 11)
  expect_s3_class(result, "gg")
})

test_that("Test invalid location input", {
  expect_error(drawRmap("InvalidLocation123", maptype = "stamen_toner", zoom = 8),
               "Unable to retrieve the coordinates")
})

test_that("Test invalid zoom level input", {
  expect_error(drawRmap("Harvard University", zoom = 50), "Invalid zoom value")
  expect_error(drawRmap("Harvard University", zoom = 0), "Invalid zoom value")
})


test_that("Test specific inputs return consistent results", {
  result1 <- drawRmap("Harvard University", maptype = "outdoors", zoom = 11)
  result2 <- drawRmap("Harvard University", maptype = "outdoors", zoom = 11)
  expect_s3_class(result1, "gg")
  expect_s3_class(result2, "gg")
  expect_equal(result1$labels$title, result2$labels$title)
})
