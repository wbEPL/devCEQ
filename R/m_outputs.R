#' Module for generating outputs in a Shiny application
#' @name m_outputs
NULL

#' @describeIn m_outputs Module UI for figure updating
#'
m_output_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("figure_ui"))
  )
}


#' @describeIn m_outputs Check if an object is a single figure type
#'
#' @param a An object to check
#' @return Logical indicating if object is a single figure (not a list of figures)
#' @export
is_single_output <- function(a) {
  # Check for specific single object classes
  if (inherits(a, "ggplot")) return(TRUE)
  if (inherits(a, "flextable")) return(TRUE)
  if (inherits(a, "datatables")) return(TRUE)
  if (inherits(a, "reactable")) return(TRUE)

  # plotly objects are lists, so check if it's a single plotly object
  if (inherits(a, "plotly") && inherits(a, "htmlwidget")) return(TRUE)

  # data.frame is not a list in the traditional sense
  if (is.data.frame(a) && !inherits(a, c("flextable", "datatables", "reactable"))) return(TRUE)

  FALSE
}

#' @describeIn m_outputs Check the type of a single output object
#'
#' Returns the type of a single output object (ggplot, plotly, etc.)
#' Use this after confirming with is_single_output() or get_output_structure()
#'
#' @param a A single output object
#' @return A character string indicating the single object type
#' @export
get_single_output_type <- function(a) {
  if (inherits(a, "flextable")) return("flextable")
  if (inherits(a, "datatables")) return("datatables")
  if (inherits(a, "reactable")) return("reactable")
  if (inherits(a, "ggplot")) return("ggplot")
  if (inherits(a, "plotly")) return("plotly")
  if (is.data.frame(a)) return("data.frame")

  "unknown"
}

#' @describeIn m_outputs Flatten a nested list structure
#'
#' If an object is a list of length 1 containing a known output type,
#' extract that single element. Otherwise return the object as-is.
#' Uses iterative unwrapping instead of recursion.
#'
#' @param a An object to potentially flatten
#' @return The flattened object or the original object
#' @export
flatten_if_single <- function(a) {
  # If it's already a single known output, return as-is
  if (is_single_output(a)) {
    return(a)
  }

  # If it's not a list, return as-is
  if (!is.list(a)) {
    return(a)
  }

  # Keep unwrapping single-element lists until we can't anymore
  current <- a

  while (is.list(current) && length(current) == 1 && !is_single_output(current)) {
    # Extract the single element
    next_element <- current[[1]]

    # If the element is a known output type, return it
    if (is_single_output(next_element)) {
      return(next_element)
    }

    # If it's not a list anymore, return it
    if (!is.list(next_element)) {
      return(next_element)
    }

    # Move to the next level
    current <- next_element
  }

  # Return the final unwrapped result
  return(current)
}

#' @describeIn m_outputs Recursively flatten nested lists to extract all single outputs
#'
#' Flattens a potentially nested list structure, extracting all single known
#' output types and returning them as a flat named list. Preserves names where possible.
#'
#' @param a An object or list to flatten
#' @param prefix Optional prefix for names (used internally for recursion)
#' @return A flat named list of single output objects
#' @export
flatten_outputs <- function(a, prefix = "") {
  # If it's a single output, return as a named list
  if (is_single_output(a)) {
    if (prefix == "") {
      return(list(output = a))
    } else {
      result <- list(a)
      names(result) <- prefix
      return(result)
    }
  }

  # If it's not a list, we can't process it
  if (!is.list(a)) {
    return(list())
  }

  # Process each element
  result <- list()
  for (i in seq_along(a)) {
    element <- a[[i]]

    # Get the name for this element
    element_name <- names(a)[i]
    if (is.null(element_name) || element_name == "") {
      element_name <- as.character(i)
    }

    # Build the full name with prefix
    full_name <- if (prefix == "") {
      element_name
    } else {
      paste0(prefix, ".", element_name)
    }

    # If it's a single output, add it directly
    if (is_single_output(element)) {
      new_item <- list(element)
      names(new_item) <- element_name
      result <- c(result, new_item)
    } else if (is.list(element)) {
      # Recursively flatten nested lists
      flattened <- flatten_outputs(element, full_name)
      if (length(flattened) > 0) {
        result <- c(result, flattened)
      }
    }
  }

  return(result)
}

#' @describeIn m_outputs Enlist an object if it's a single output
#'
#' If an object is a single known output type, wrap it in a list.
#' If it's already a list, return as-is.
#'
#' @param a An object to potentially enlist
#' @return A list containing the object, or the object itself if already a list
#' @export
enlist_if_not_list <- function(a) {
  # Check if it's a single output type
  if (is_single_output(a)) {
    return(list(a))
  }

  # If it's already a list, return as-is
  if (is.list(a)) {
    return(a)
  }

  # For unknown types, wrap in a list
  return(list(a))
}

#' @describeIn m_outputs Get the structure of an output object
#'
#' Determines if an object is a single output or a list of outputs,
#' and identifies the type(s) of outputs contained.
#' Automatically flattens single-element lists containing known output types.
#'
#' @param a An object to check
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{structure} - Either "single" or "list"
#'     \item \code{type} - For single objects: the type string (e.g., "ggplot").
#'                         For lists: "list_of_<type>" if all same type, or "mixed_list"
#'   }
#'
#' @export
get_output_structure <- function(a) {
  # Flatten if it's a single-element list with a known type
  a <- flatten_if_single(a)

  # Check if it's a single output first
  if (is_single_output(a)) {
    return(list(
      structure = "single",
      type = get_single_output_type(a)
    ))
  }

  # If it's a list, check what's inside
  if (is.list(a)) {
    if (length(a) == 0) {
      return(list(structure = "list", type = "empty_list"))
    }

    # Check if all elements are the same type of single output
    types <- sapply(a, function(x) {
      # Flatten each element first
      x <- flatten_if_single(x)

      if (is_single_output(x)) {
        get_single_output_type(x)
      } else {
        "complex"
      }
    })

    unique_types <- unique(types)

    if (length(unique_types) == 1 && unique_types[1] != "complex") {
      return(list(
        structure = "list",
        type = paste0("list_of_", unique_types[1])
      ))
    }

    return(list(structure = "list", type = "mixed_list"))
  }

  return(list(structure = "unknown", type = "unknown"))
}

#' @describeIn m_outputs Check the type of an object (legacy function)
#'
#' Determines if an object is a data frame, flextable, datatables, reactable,
#' a list of ggplot objects, a list of plotly objects, or other types.
#' Automatically flattens single-element lists containing known output types.
#'
#' Note: This function has limitations with objects that are internally lists.
#' Consider using get_output_structure() for more reliable detection.
#'
#' @param a An object to check
#'
#' @return A character string indicating the object type:
#'   \itemize{
#'     \item \code{"data.frame"} - A data frame
#'     \item \code{"flextable"} - A flextable object
#'     \item \code{"datatables"} - A DT datatables object
#'     \item \code{"reactable"} - A reactable object
#'     \item \code{"ggplot"} - A single ggplot object
#'     \item \code{"plotly"} - A single plotly object
#'     \item \code{"list_of_ggplot"} - A list where all elements are ggplot objects
#'     \item \code{"list_of_plotly"} - A list where all elements are plotly objects
#'     \item \code{"empty_list"} - An empty list
#'     \item \code{"mixed_list"} - A list with mixed object types
#'     \item \code{"unknown"} - None of the above types
#'   }
#'
#' @export
check_output_type <- function(a) {
  # Flatten if it's a single-element list
  a <- flatten_if_single(a)

  result <- get_output_structure(a)

  if (result$structure == "single") {
    return(result$type)
  } else {
    return(result$type)
  }
}

#' @describeIn m_outputs Helper function to enlist a figure into a list if it is not already a list
#' @param fig A figure object which can be a single figure or a list of figures
#' @return A list of figure objects
#' @export
f_enlist_fig <- function(fig = NULL, ...) {
  if (check_output_type(fig) %in% c("ggplot", "plotly", "datatables", "flextable", "reactable", "data.frame")) {
    return(list(fig))
  } else if (check_output_type(fig) %in% c("list_of_ggplot", "list_of_plotly", "list_of_datatables", "list_of_flextable", "list_of_reactable")) {
    return(fig)
  }
  cli_warn("Figure is not a recognized type to enlist.")
  return(fig)
}

#' @describeIn m_outputs Helper function to generate sample figures for testing
#' @return A named list of sample figures including ggplot, plotly, DT, flextable, reactable, and data frame
#' @export
f_get_sample_outputs <- function() {
    list(
      `Figure 1 (gg)` = fig_gg_random(),
      `Figure 2 (gg list)` = list(a = fig_gg_random(), b = fig_gg_random()),
      `Figure 3 (ly)` = fig_gg_random() |> plotly::ggplotly(),
      `Figure 4 (ly list)` = list(
        a = fig_gg_random() |> plotly::ggplotly(),
        b = fig_gg_random() |> plotly::ggplotly()
      ),
      `DT` = DT::datatable(head(mtcars)),
      `DT (list)` = list(
        a = DT::datatable(head(mtcars)),
        b = DT::datatable(tail(mtcars))
      ),
      `Flextable` = flextable::flextable(head(mtcars)),
      `Flextable (list)` = list(
        a = flextable::flextable(head(mtcars)),
        b = flextable::flextable(tail(mtcars))
      ),
      `Reactable` = reactable::reactable(head(mtcars)),
      `Reactable (list)` = list(
        a = reactable::reactable(head(mtcars)),
        b = reactable::reactable(tail(mtcars))
      ),
      `DF` = head(mtcars),
      `DF (list)` = list(
        a = head(mtcars),
        b = tail(mtcars)
      ),
      `mix` = list(
        a = fig_gg_random(),
        aa = list(
          aaa = fig_gg_random(),
          aab = fig_gg_random() |> plotly::ggplotly()
        ),
        b = fig_gg_random() |> plotly::ggplotly(),
        b = DT::datatable(head(mtcars)),
        c = flextable::flextable(head(mtcars)),
        d = reactable::reactable(head(mtcars)),
        e = head(mtcars)
      )
    )

}

#' @describeIn m_outputs Single output module server
#'
#' Renders a single output object (ggplot, plotly, datatables, flextable, reactable, or data.frame)
#'
#' @param id Module id
#' @param figure A reactive expression returning a single output object
#' @return Reactive UI for the figure
#' @export
m_one_output_srv <- function(
  id,
  figure = reactive(NULL),
  # type = "ggplot",
  ...
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fig_type <- reactive({
      req(figure())
      # browser()
      check_output_type(figure())
    })

    fig_ui <- reactive({
      switch(
        fig_type(),
        "ggplot" = plotOutput(ns("fig_gg")),
        "plotly" = plotly::plotlyOutput(ns("fig_ly")),
        "flextable" = uiOutput(ns("fig_ft")),
        "datatables" = DT::DTOutput(ns("fig_dt")),
        "data.frame" = tableOutput(ns("fig_tbl")),
        "reactable" = reactable::reactableOutput(ns("fig_rt"))
      )
    })

    fig_render <- reactive({
      switch(
        fig_type(),
        "ggplot" = function(plt) {
          output$fig_gg <- renderPlot({
            req(plt())
            plt()
          })
        },
        "plotly" = function(plt) {
          output$fig_ly <- plotly::renderPlotly({
            req(plt())
            plt() |> plotly_config()
          })
        },
        "flextable" = function(plt) {
          output$fig_ft <- renderUI({
            req(plt())
            flextable::htmltools_value(plt())
          })
        },
        "datatables" = function(plt) {
          output$fig_dt <- DT::renderDT({
            req(plt())
            plt()
          })
        },
        "data.frame" = function(plt) {
          output$fig_tbl <- renderTable({
            req(plt())
            plt()
          })
        },
        "reactable" = function(plt) {
          output$fig_rt <- reactable::renderReactable({
            req(plt())
            plt()
          })
        }
      )
    })
    observe({
      req(figure())
      req(fig_render())
      fig_render()(figure)
    })

    fig_ui
  })
}

#' @describeIn m_outputs Output module wrapper for multiple outputs
#'
#' Automatically detects and renders any output type (single or list).
#' Flattens nested structures and renders each output appropriately.
#'
#' @param id Module id
#' @param figures A reactive expression returning a single output or list of outputs
#' @param selected A reactive expression returning a character vector of names to filter outputs.
#'   If NULL or empty, all outputs are displayed. If names are provided but none match,
#'   all outputs are displayed. Only matching outputs are rendered.
#' @param output_type Character string specifying how to display multiple outputs:
#'   \itemize{
#'     \item \code{"sequential"} (default) - Displays outputs one after another
#'     \item \code{"tabs"} - Displays outputs in a card with tabs
#'   }
#' @return NULL (renders to UI directly)
#' @export
m_output_srv <- function(
  id,
  figures = reactive(NULL),
  selected = reactive(NULL),
  output_type = c("tabs", "sequential"),
  ...
) {
  output_type <- match.arg(output_type)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    flattened_outputs <- reactive({
      req(figures())
      flatten_outputs(figures())
    })

    # Filter outputs based on selected names
    filtered_outputs <- reactive({
      all_outputs <- flattened_outputs()
      sel <- selected()

      # If selected is NULL or empty, return all outputs
      if (is.null(sel) || length(sel) == 0 || all(sel == "")) {
        return(all_outputs)
      }

      # Filter outputs by selected names
      matching_outputs <- all_outputs[names(all_outputs) %in% sel]

      # If no matches found, return all outputs
      if (length(matching_outputs) == 0) {
        return(all_outputs)
      }

      return(matching_outputs)
    })

    output$figure_ui <- renderUI({
      req(filtered_outputs())
      output_uis <- purrr::imap(filtered_outputs(), function(fig, name) {
        mod_id <- paste0("fig_", make.names(name))
        m_one_output_srv(id = mod_id, figure = reactive(fig))()
      })
      if (output_type == "tabs" && length(output_uis) > 1) {
        tab_panels <- purrr::imap(output_uis, function(ui, name) {
          nav_panel(title = name, ui)
        }) |>
          unname()
        do.call(navset_tab, tab_panels)
      } else {
        tagList(output_uis)
      }
    })
    return(invisible(NULL))
  })
}

#' @describeIn m_outputs Test app for output module
#' @export
#'
test_m_one_output <- function() {
  library(shiny)
  library(plotly)
  library(ggplot2)
  library(bslib)

  ui <- page_fixed(
    h3("Output Module Demo"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Sequential Display"),
        selectInput(
          inputId = "fig_select_1",
          label = "Select figure:",
          choices = names(f_get_sample_outputs()),
          selected = "Figure 2 (gg list)"
        ),
        m_output_ui("output_module_1")
      ),
      card(
        card_header("Tabbed Display"),
        selectInput(
          inputId = "fig_select_2",
          label = "Select figure:",
          choices = names(f_get_sample_outputs()),
          selected = "mix"
        ),
        m_output_ui("output_module_2")
      ),
      card(
        card_header("Filtered Display (Tabs)"),
        selectInput(
          inputId = "fig_select_3",
          label = "Select figure:",
          choices = names(f_get_sample_outputs()),
          selected = "mix"
        ),
        selectInput(
          inputId = "name_filter",
          label = "Filter by name:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        m_output_ui("output_module_3")
      )
    )
  )

  server <- function(input, output, session) {
    # Update filter choices based on selected figure
    observe({
      req(input$fig_select_3)
      fig <- f_get_sample_outputs()[[input$fig_select_3]]
      flattened <- flatten_outputs(fig)
      updateSelectInput(
        session,
        "name_filter",
        choices = names(flattened),
        selected = NULL
      )
    })

    # First output module - sequential display
    m_output_srv(
      id = "output_module_1",
      figures = reactive({
        req(input$fig_select_1)
        f_get_sample_outputs()[[input$fig_select_1]]
      }),
      output_type = "sequential"
    )

    # Second output module - tabbed display
    m_output_srv(
      id = "output_module_2",
      figures = reactive({
        req(input$fig_select_2)
        f_get_sample_outputs()[[input$fig_select_2]]
      }),
      output_type = "tabs"
    )

    # Third output module - filtered tabbed display
    m_output_srv(
      id = "output_module_3",
      figures = reactive({
        req(input$fig_select_3)
        f_get_sample_outputs()[[input$fig_select_3]]
      }),
      selected = reactive(input$name_filter),
      output_type = "tabs"
    )
  }

  shinyApp(ui, server)
}
