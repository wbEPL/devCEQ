#' Module to let user select input type and render corresponding input UI basd 
#' on the set of paramters that include input type and choices. 
#' Choices could be passed a preset value or a reactive expression.
#' The module returns the default value of the input even when the input is not 
#' renderred or not initialized yet.
#' 
#' 
#' @name m_inputs
NULL

#' @describeIn m_inputs Income deciles by selection UI wrapper
#' @param id Module id
#' @export
#' 
m_input_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui"))
}

#' @describeIn m_inputs Income deciles by selection server wrapper
#' @param id Module id
#' @param type Type of input to render. One of "numericInput", "selectInput", "selectizeInput", "radioGroupButtons"
#' @param title Title/label for the input: must be a reactive expression.
#' @param choices Choices for the input: must be a reactive expression.
#' @export
#'
m_input_srv <- function(
  id,
  type = "numericInput",
  title = reactive(NULL),
  choices = reactive(NULL),
  ...
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI generation function
    f_ui <-
      switch(
        type,
        # "titleInput" = f_numericInput_ui,
        "numericInput" = f_numericInput_ui,
        "selectInput" = f_selectInput_ui,
        "selectizeInput" = f_selectizeInput_ui,
        "radioGroupButtons" = f_radioGroupButtons_ui,
        stop("Unknown module type")
      )

    choices <- f_make_reactive(choices)
    title <- f_make_reactive(title)
    args_ui <- reactive(c(
      list(id = ns(NULL), title = title(), choices = choices()),
      list(...)
    ))

    output$ui <- renderUI(do.call(f_ui, args_ui()))

    # Update module function (Not used for now)

    f_collect_input(NULL, choices = choices)
  })
}

#' @describeIn m_inputs Helper function to make input reactive if not already
#' @param input Input value
#' @export
f_make_reactive <- function(x) {
  if (is.reactive(x)) return(x)
  reactive(x)
}

#' @describeIn m_inputs Helper function to safely convert to numeric if possible
#' @param x Input value
#' @export
#'
f_safe_numeric <- function(x) {
  # Store original names
  original_names <- names(x)

  # Convert (handles both single values and vectors/lists)
  result <- type.convert(x, as.is = TRUE)

  # If conversion was successful (result is numeric) and original had names
  if (is.numeric(result) && !is.null(original_names)) {
    names(result) <- original_names
  }

  # Return numeric with names if successful, otherwise original x
  if (is.numeric(result)) result else x
}

#' @describeIn m_inputs Collect input providing default choices value
#' @param id Module id
#' @param choices Reactive expression with the named list of default choices
#' @param ... Additional parameters passed to m_input_srv
#' @export 
f_collect_input <- function(id, choices = reactive(NULL), ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    choices <- f_make_reactive(choices)
    out <- reactiveVal()

    # Default value before input is initialized
    observe({
      req(choices())
      isolate(out(choices()[1]))
    })
    
    # Update the value when input changes
    observeEvent(
      input[["inputId"]],
      {
        req(choices())
        new_out <- input[["inputId"]]
        if (all(is.numeric(choices()))) {
          new_out <- as.numeric(new_out)
        }        
        if (all(is.integer(choices()))) {
          new_out <- as.numeric(new_out)
        }
        out(new_out)
      },
      ignoreNULL = TRUE
    )
    
    out
  })
}

#' @describeIn m_inputs Test app for m_input module
#' 
test_m_input <- function() {
  library(shiny)
  library(shinyWidgets)
  library(bslib)
  
  ui <- page_fixed(
    h2("Test m_input module"),
    h3("Genric input value that is passed as reactive choices"),
    layout_columns(
      textInput("name", "Name:", "Item 1"),
      textInput("value", "Value", "item_1"),
    ),
    verbatimTextOutput("inp_r"),
    h3("numerciInput"),
    layout_columns(
      m_input_ui("num1"),
      m_input_ui("num2"),
      m_input_ui("num3")
    ),
    verbatimTextOutput("num"),

    h3("selectInput"),
    layout_columns(
      m_input_ui("sel0"),
      m_input_ui("sel1"),
      m_input_ui("sel2"),
      m_input_ui("sel3")
    ),
    verbatimTextOutput("sel"),

    h3("selectizeInput"),
    layout_columns(
      m_input_ui("selz0"),
      m_input_ui("selz1"),
      m_input_ui("selz2"),
      m_input_ui("selz3")
    ),
    verbatimTextOutput("selz"),

    h3("radioGroupButtons"),
    layout_columns(
      m_input_ui("rad0"),
      m_input_ui("rad1"),
      m_input_ui("rad2"),
      m_input_ui("rad3")
    ),
    verbatimTextOutput("rad")

  )
  
  server <- function(input, output, session) {

    choice_react <- reactive({
      out <- setNames(nm = input$name, object = input$value)
      f_safe_numeric(out)
    })

    output$inp_r <- renderPrint({choice_react()})

    num1 <- m_input_srv("num1", "numericInput", title = reactive("Choices (react):"), choices = choice_react)
    num2 <- m_input_srv("num2", "numericInput", title = reactive("Choices (value):"), choices = reactive(c("A" = 99, "B" = 2, "C" = 3)))
    num3 <- m_input_srv("num3", "numericInput", title = reactive("Choices (NA):"), choices = reactive("NULL REACTIVE"))
    
    output$num <- renderPrint({
      list(num1 = num1(), num2 = num2(), num3 = num3())
    })

    
    sel0 <- m_input_srv("sel0", "selectInput", reactive("One choice:"), choice_react)
    sel1 <- m_input_srv("sel1", "selectInput", reactive("Reactive:"), reactive(c(str_c(choice_react(), "1"), choice_react())))
    sel2 <- m_input_srv("sel2", "selectInput", reactive("Constant (numeric):"), reactive(c("A" = 1, "B" = 2, "C" = 3)))
    sel3 <- m_input_srv("sel3", "selectInput", reactive("Constant (character):"), reactive(c("A" = "1", "B" = "2", "C" = "3")), multiple = TRUE)
    
    output$sel <- renderPrint({
      list(sel0 = sel0(), sel1 = sel1(), sel2 = sel2(), sel3 = sel3())
    })

    selz0 <- m_input_srv("selz0", "selectizeInput", reactive("One choice:"), choice_react)
    selz1 <- m_input_srv("selz1", "selectizeInput", reactive("Reactive:"), reactive(c(str_c(choice_react(), "1"), choice_react())), multiple = TRUE)
    selz2 <- m_input_srv("selz2", "selectizeInput", reactive("Constant (numeric):"), reactive(c("A" = 1, "B" = 2, "C" = 3)))
    selz3 <- m_input_srv("selz3", "selectizeInput", reactive("Constant (character):"), reactive(c("A" = "1", "B" = "2", "C" = "3")), multiple = TRUE)
    output$selz <- renderPrint({
      list(selz0 = selz0(), selz1 = selz1(), selz2 = selz2(), selz3 = selz3())
    })

    rad0 <- m_input_srv("rad0", "radioGroupButtons", reactive(NULL), choice_react)
    rad1 <- m_input_srv("rad1", "radioGroupButtons", reactive("Reactive (with title):"), reactive(c(str_c(choice_react(), "1"), choice_react())))
    rad2 <- m_input_srv("rad2", "radioGroupButtons", reactive(NULL), reactive(c("A" = 1, "B" = 2, "C" = 3)))
    rad3 <- m_input_srv("rad3", "radioGroupButtons", reactive(NULL), reactive(c("A" = "1", "B" = "2", "C" = "3")))
    output$rad <- renderPrint({
      list(rad0 = rad0(), rad1 = rad1(), rad2 = rad2(), rad3 = rad3())
    })

  }
  
  shinyApp(ui, server)
}