test_that("check_object_type identifies data frames", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  expect_equal(check_object_type(df), "data.frame")
})

test_that("check_object_type identifies flextable objects", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable(head(mtcars))
  expect_equal(check_object_type(ft), "flextable")
})

test_that("check_object_type identifies datatables objects", {
  skip_if_not_installed("DT")
  dt <- DT::datatable(head(mtcars))
  expect_equal(check_object_type(dt), "datatables")
})

test_that("check_object_type identifies reactable objects", {
  skip_if_not_installed("reactable")
  rt <- reactable::reactable(head(mtcars))
  expect_equal(check_object_type(rt), "reactable")
})

test_that("check_object_type identifies single ggplot object", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  expect_equal(check_object_type(plot1), "ggplot")
})

test_that("check_object_type identifies list of ggplot objects", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()

  plot_list <- list(plot1, plot2)
  expect_equal(check_object_type(plot_list), "list_of_ggplot")
})

test_that("check_object_type identifies single plotly object", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  library(plotly)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plotly1 <- ggplotly(plot1)
  expect_equal(check_object_type(plotly1), "plotly")
})

test_that("check_object_type identifies list of plotly objects", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  library(plotly)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()

  plotly_list <- list(ggplotly(plot1), ggplotly(plot2))
  expect_equal(check_object_type(plotly_list), "list_of_plotly")
})

test_that("check_object_type identifies empty list", {
  empty_list <- list()
  expect_equal(check_object_type(empty_list), "empty_list")
})

test_that("check_object_type identifies mixed list", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  df <- data.frame(x = 1:5)

  mixed_list <- list(plot1, df)
  expect_equal(check_object_type(mixed_list), "mixed_list")
})

test_that("check_object_type returns unknown for unrecognized objects", {
  unknown_obj <- "just a string"
  expect_equal(check_object_type(unknown_obj), "unknown")

  unknown_obj2 <- 42
  expect_equal(check_object_type(unknown_obj2), "unknown")
})
