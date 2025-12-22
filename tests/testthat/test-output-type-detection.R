test_that("is_single_output identifies single ggplot", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  expect_true(is_single_output(plot1))
})

test_that("is_single_output identifies single plotly", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  library(plotly)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plotly1 <- ggplotly(plot1)
  expect_true(is_single_output(plotly1))
})

test_that("is_single_output identifies single flextable", {
  skip_if_not_installed("flextable")

  ft <- flextable::flextable(head(mtcars))
  expect_true(is_single_output(ft))
})

test_that("is_single_output identifies single datatables", {
  skip_if_not_installed("DT")

  dt <- DT::datatable(head(mtcars))
  expect_true(is_single_output(dt))
})

test_that("is_single_output identifies single reactable", {
  skip_if_not_installed("reactable")

  rt <- reactable::reactable(head(mtcars))
  expect_true(is_single_output(rt))
})

test_that("is_single_output identifies single data.frame", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  expect_true(is_single_output(df))
})

test_that("is_single_output returns FALSE for list of outputs", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()
  plot_list <- list(plot1, plot2)

  expect_false(is_single_output(plot_list))
})

test_that("get_single_output_type returns correct types", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  skip_if_not_installed("flextable")
  skip_if_not_installed("DT")
  skip_if_not_installed("reactable")
  library(ggplot2)
  library(plotly)

  plot_gg <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot_ly <- ggplotly(plot_gg)
  ft <- flextable::flextable(head(mtcars))
  dt <- DT::datatable(head(mtcars))
  rt <- reactable::reactable(head(mtcars))
  df <- data.frame(x = 1:5)

  expect_equal(get_single_output_type(plot_gg), "ggplot")
  expect_equal(get_single_output_type(plot_ly), "plotly")
  expect_equal(get_single_output_type(ft), "flextable")
  expect_equal(get_single_output_type(dt), "datatables")
  expect_equal(get_single_output_type(rt), "reactable")
  expect_equal(get_single_output_type(df), "data.frame")
})

test_that("flatten_if_single unwraps single-element list with known type", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  wrapped <- list(plot1)

  result <- flatten_if_single(wrapped)
  expect_true(inherits(result, "ggplot"))
  expect_false(is.list(result) && length(result) == 1)
})

test_that("flatten_if_single unwraps deeply nested single-element lists", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  deeply_wrapped <- list(list(list(plot1)))

  result <- flatten_if_single(deeply_wrapped)
  expect_true(inherits(result, "ggplot"))
})

test_that("flatten_if_single returns as-is for multi-element lists", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()
  plot_list <- list(plot1, plot2)

  result <- flatten_if_single(plot_list)
  expect_equal(length(result), 2)
  expect_true(is.list(result))
})

test_that("flatten_if_single returns single output as-is", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  result <- flatten_if_single(plot1)

  expect_identical(result, plot1)
})

test_that("get_output_structure detects single ggplot", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  result <- get_output_structure(plot1)

  expect_equal(result$structure, "single")
  expect_equal(result$type, "ggplot")
})

test_that("get_output_structure detects list_of_ggplot", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()
  plot_list <- list(plot1, plot2)

  result <- get_output_structure(plot_list)

  expect_equal(result$structure, "list")
  expect_equal(result$type, "list_of_ggplot")
})

test_that("get_output_structure detects list_of_plotly", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  library(plotly)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()
  plotly_list <- list(ggplotly(plot1), ggplotly(plot2))

  result <- get_output_structure(plotly_list)

  expect_equal(result$structure, "list")
  expect_equal(result$type, "list_of_plotly")
})

test_that("get_output_structure detects mixed_list", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  df <- data.frame(x = 1:5)
  mixed_list <- list(plot1, df)

  result <- get_output_structure(mixed_list)

  expect_equal(result$structure, "list")
  expect_equal(result$type, "mixed_list")
})

test_that("get_output_structure detects empty_list", {
  result <- get_output_structure(list())

  expect_equal(result$structure, "list")
  expect_equal(result$type, "empty_list")
})

test_that("get_output_structure flattens wrapped single outputs", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  wrapped <- list(plot1)

  result <- get_output_structure(wrapped)

  expect_equal(result$structure, "single")
  expect_equal(result$type, "ggplot")
})

test_that("get_output_structure flattens elements in list", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()

  # Wrap each plot in a single-element list
  wrapped_list <- list(list(plot1), list(plot2))

  result <- get_output_structure(wrapped_list)

  expect_equal(result$structure, "list")
  expect_equal(result$type, "list_of_ggplot")
})

test_that("check_output_type returns correct string for single outputs", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  library(ggplot2)
  library(plotly)

  plot_gg <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot_ly <- ggplotly(plot_gg)
  df <- data.frame(x = 1:5)

  expect_equal(check_output_type(plot_gg), "ggplot")
  expect_equal(check_output_type(plot_ly), "plotly")
  expect_equal(check_output_type(df), "data.frame")
})

test_that("check_output_type returns correct string for lists", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()
  plot_list <- list(plot1, plot2)

  expect_equal(check_output_type(plot_list), "list_of_ggplot")
})

test_that("check_output_type flattens wrapped single outputs", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  wrapped <- list(list(plot1))

  expect_equal(check_output_type(wrapped), "ggplot")
})

test_that("check_output_type handles all supported types", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  skip_if_not_installed("flextable")
  skip_if_not_installed("DT")
  skip_if_not_installed("reactable")
  library(ggplot2)
  library(plotly)

  plot_gg <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot_ly <- ggplotly(plot_gg)
  ft <- flextable::flextable(head(mtcars))
  dt <- DT::datatable(head(mtcars))
  rt <- reactable::reactable(head(mtcars))
  df <- head(mtcars)

  expect_equal(check_output_type(plot_gg), "ggplot")
  expect_equal(check_output_type(plot_ly), "plotly")
  expect_equal(check_output_type(ft), "flextable")
  expect_equal(check_output_type(dt), "datatables")
  expect_equal(check_output_type(rt), "reactable")
  expect_equal(check_output_type(df), "data.frame")

  # Test lists
  expect_equal(check_output_type(list(plot_gg, plot_gg)), "list_of_ggplot")
  expect_equal(check_output_type(list(plot_ly, plot_ly)), "list_of_plotly")
})

test_that("f_enlist_fig wraps single outputs in list", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  result <- f_enlist_fig(plot1)

  expect_true(is.list(result))
  expect_equal(length(result), 1)
  expect_true(inherits(result[[1]], "ggplot"))
})

test_that("f_enlist_fig returns list as-is for list_of outputs", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()
  plot_list <- list(plot1, plot2)

  result <- f_enlist_fig(plot_list)

  expect_equal(length(result), 2)
  expect_identical(result, plot_list)
})

test_that("get_output_structure handles list_of_datatables", {
  skip_if_not_installed("DT")

  dt1 <- DT::datatable(head(mtcars))
  dt2 <- DT::datatable(tail(mtcars))
  dt_list <- list(dt1, dt2)

  result <- get_output_structure(dt_list)

  expect_equal(result$structure, "list")
  expect_equal(result$type, "list_of_datatables")
})

test_that("get_output_structure handles list_of_flextable", {
  skip_if_not_installed("flextable")

  ft1 <- flextable::flextable(head(mtcars))
  ft2 <- flextable::flextable(tail(mtcars))
  ft_list <- list(ft1, ft2)

  result <- get_output_structure(ft_list)

  expect_equal(result$structure, "list")
  expect_equal(result$type, "list_of_flextable")
})

test_that("get_output_structure handles list_of_reactable", {
  skip_if_not_installed("reactable")

  rt1 <- reactable::reactable(head(mtcars))
  rt2 <- reactable::reactable(tail(mtcars))
  rt_list <- list(rt1, rt2)

  result <- get_output_structure(rt_list)

  expect_equal(result$structure, "list")
  expect_equal(result$type, "list_of_reactable")
})

test_that("get_output_structure handles list_of_data.frame", {
  df1 <- head(mtcars)
  df2 <- tail(mtcars)
  df_list <- list(df1, df2)

  result <- get_output_structure(df_list)

  expect_equal(result$structure, "list")
  expect_equal(result$type, "list_of_data.frame")
})

test_that("enlist_if_not_list wraps single outputs in a list", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  result <- enlist_if_not_list(plot1)

  expect_true(is.list(result))
  expect_equal(length(result), 1)
  expect_true(inherits(result[[1]], "ggplot"))
})

test_that("enlist_if_not_list returns lists as-is", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()
  plot_list <- list(plot1, plot2)

  result <- enlist_if_not_list(plot_list)

  expect_identical(result, plot_list)
})

test_that("enlist_if_not_list works with all output types", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  skip_if_not_installed("flextable")
  skip_if_not_installed("DT")
  skip_if_not_installed("reactable")
  library(ggplot2)
  library(plotly)

  plot_gg <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot_ly <- ggplotly(plot_gg)
  ft <- flextable::flextable(head(mtcars))
  dt <- DT::datatable(head(mtcars))
  rt <- reactable::reactable(head(mtcars))
  df <- head(mtcars)

  expect_equal(length(enlist_if_not_list(plot_gg)), 1)
  expect_equal(length(enlist_if_not_list(plot_ly)), 1)
  expect_equal(length(enlist_if_not_list(ft)), 1)
  expect_equal(length(enlist_if_not_list(dt)), 1)
  expect_equal(length(enlist_if_not_list(rt)), 1)
  expect_equal(length(enlist_if_not_list(df)), 1)
})

test_that("enlist_if_not_list wraps unknown types", {
  unknown_obj <- "just a string"
  result <- enlist_if_not_list(unknown_obj)

  expect_true(is.list(result))
  expect_equal(length(result), 1)
  expect_equal(result[[1]], unknown_obj)
})

test_that("flatten_outputs extracts single output", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  result <- flatten_outputs(plot1)

  expect_equal(length(result), 1)
  expect_true(is_single_output(result[[1]]))
  expect_equal(check_output_type(result[[1]]), "ggplot")
})

test_that("flatten_outputs flattens list of single outputs", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()
  plot_list <- list(a = plot1, b = plot2)

  result <- flatten_outputs(plot_list)

  expect_equal(length(result), 2)
  expect_true(all(sapply(result, is_single_output)))
  expect_equal(names(result), c("a", "b"))
})

test_that("flatten_outputs flattens deeply nested lists", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  library(ggplot2)
  library(plotly)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()
  plot_ly <- ggplotly(plot1)

  nested_list <- list(
    a = plot1,
    nested = list(
      b = plot2,
      c = plot_ly
    )
  )

  result <- flatten_outputs(nested_list)

  expect_equal(length(result), 3)
  expect_true(all(sapply(result, is_single_output)))
  expect_equal(names(result), c("a", "b", "c"))
})

test_that("flatten_outputs preserves names in nested structures", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  plot1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot2 <- ggplot(mtcars, aes(x = wt, y = qsec)) + geom_line()

  nested_list <- list(
    outer = list(
      inner = plot1
    ),
    direct = plot2
  )

  result <- flatten_outputs(nested_list)

  expect_equal(length(result), 2)
  expect_true(all(sapply(result, is_single_output)))
  expect_true("inner" %in% names(result))
  expect_true("direct" %in% names(result))
})

test_that("flatten_outputs handles mixed output types", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  library(ggplot2)
  library(plotly)

  plot_gg <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  plot_ly <- ggplotly(plot_gg)
  df <- head(mtcars)

  mixed_list <- list(a = plot_gg, b = plot_ly, c = df)

  result <- flatten_outputs(mixed_list)

  expect_equal(length(result), 3)
  expect_true(all(sapply(result, is_single_output)))
  expect_equal(check_output_type(result[[1]]), "ggplot")
  expect_equal(check_output_type(result[[2]]), "plotly")
  expect_equal(check_output_type(result[[3]]), "data.frame")
})
