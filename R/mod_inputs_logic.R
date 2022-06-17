#' inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @inheritParams mod_inputs_btns_ui
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @export
mod_inputs_ui_wrapper <- function(id, choice_type = "slider", choice_max = 2) {
  ns <- NS(id)
  text_field <-
    if (getOption("ceq_dev", FALSE)) {
      shiny::verbatimTextOutput(ns("inputs_out"))
    } else {
      NULL
    }
  left_col <-
    wellPanel(mod_inputs_btns_ui(id, choice_type = choice_type, choice_max = choice_max)) %>%
    div(id = "well1") %>%
    column(width = 3)
  right_col <-
    mod_dyn_inp_ui(id) %>%
    # shinydashboard::box(width = 10, id = "well2")
    div(style = "min-height:665px") %>%
    div(id = "well2") %>%
    div(id = "well2b") %>%
    tagList(text_field) %>%
    tagList(verbatimTextOutput(ns("highlighted"))) %>%
    column(width = 9) %>%
    tagList(
      if (getOption("ceq_dev", FALSE))
        profvis::profvis_ui("profiler")
      else
        NULL
    )
  fluidRow(left_col, right_col)
}


#' inputs Server Functions
#'
#' @noRd
#' @export
mod_inputs_server <-
  function(id,
           inp_raw_str,
           inp_str_fn,
           ui_gen_fn,
           choice_max = 3,
           run_guide = function() NULL,
           active_tab = function() NULL,
           id_result = NULL,
           target_tab = NULL,
           source_tab = NULL,
           ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # # Inputs guides
      # guides <- compile_inputs_guides()guides$init(session = session)

      # Main module for managing all inputs
      cur_inps <- mod_dyn_inp_srv(
        id = NULL,
        inp_raw_str = inp_raw_str,
        n_choices = reactive(inp_btns$n_choices),
        upd_inp = reactive(inp_btns$upload_sim) %>% debounce(750),
        reseter = reactive(if(!isTruthy(inp_btns$reset)) {0} else {inp_btns$reset}),
        inp_str_fn = inp_str_fn,
        ui_gen_fn = ui_gen_fn
      )

      # sidepanel-related logic
      cur_inps_export <- reactive({req(cur_inps$inp())})

      # Upload-donwload-resetall module
      inp_btns <- mod_inputs_btns_server(NULL, sim_export_dta = cur_inps$inp)

      # Run btn click
      cur_inps$run <- reactive({
        list(value = inp_btns$run,
             timestamp = Sys.time())
      })

      output$inputs_out <- renderPrint({
        list(#validate(need(active_tab(), label = "Active tab")),
             #active_tab(),
             guide_starter(),
             cur_inps$key(),
             cur_inps$run(),
             inp_btns$run_guide) %>%
          str()
      })

      # Guide starters ------------------------------------------------------
      guide_starter <- reactiveVal(NULL)

      observeEvent(#
        inp_btns$run_guide,
        {
          if (!isTruthy(guide_starter())) {
            guide_starter(0)
          } else {
            new_val <- guide_starter() + 1
            guide_starter(new_val)
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE)

      observeEvent(#
        active_tab(),
        {
          req(active_tab()$previous)
          req(active_tab()$current)
          req(source_tab)
          req(target_tab)
          if (active_tab()$previous == source_tab &&
              active_tab()$current == target_tab) {
            if (!isTruthy(guide_starter())) {
              guide_starter(0)
            } else {
              new_val <- guide_starter() + 1
              guide_starter(new_val)
            }
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE)

      # gguide <- guide_inputs(ns_input = ns, ns_result = NS(id_result))

      observeEvent(
        guide_starter(), {
          # gguide$init(session = session)$start()
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      cur_inps
    })
  }

## To be copied in the UI
# mod_inputs_ui("inputs_ui_1")

## To be copied in the server
# mod_inputs_server("inputs_ui_1")


#' UI for dynamic inputs generated / updated by server function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @export
mod_dyn_inp_ui <- function(id) {
  ns <- NS(id)

  shiny::tabsetPanel(
    type = "tabs",
    id = ns("input_tabs"),
    shiny::tabPanel(title = "Policy choices",
                    id = ns("policy_choice"),
                    if (getOption("ceq_inmodule_dev", FALSE)) {
                      actionButton(ns("browser"), "browser")
                    },
                    shiny::uiOutput(ns("dynamic_inputs_ui"))),
    shiny::tabPanel("Summary table",
                    id = ns("summary_table"),
                    DT::DTOutput(ns("inputs_ui_values")))
  ) %>%
    tagList(
      .,
      if (getOption("ceq_inmodule_dev", FALSE)) {
        shiny::verbatimTextOutput(ns("dynamic_inputs_ui_info"))
      }
    )

}




#' Module for generating and re-generating inputs on the server
#'
#' @noRd
#' @import shiny
#' @export
mod_dyn_inp_srv <-
  function(id,
           inp_raw_str,
           n_choices = reactive(1),
           choice_range = c(1,2),
           upd_inp = reactive(NULL),
           reseter = reactive(NULL),
           inp_str_fn,
           ui_gen_fn) {
    shiny::moduleServer(#
      id,
      function(input, output, session) {
        # Reactive values with inputs
        cur_clean_inp <- reactiveValues(inp = NULL)
        cur_clean_inp$n_ch <- reactive(n_choices())

        ## ## ## Gen inputs UI
        cur_clean_inp$inp_str <-
          mod_build_inp_srv(NULL,
                            inp_raw_str = inp_raw_str,
                            inp_str_fn = inp_str_fn,
                            ui_gen_fn = ui_gen_fn,
                            n_choices = cur_clean_inp$n_ch,
                            ncols = 12,
                            reseter = reseter)

        ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
        ## ## ## Update new inputs UI with previous values (if n changes)
        mod_upd_old_vals_to_exist_inp(NULL, cur_clean_inp)

        ## ## ## Update new inputs UI with new values from the file
        mod_upd_inp_srv(NULL, upd_inp)

        ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
        ## ## ## reset scenarios to the baseline after scenario-specific button click
        mod_reset_scenarios(NULL, cur_clean_inp$inp_str)

        ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
        ## ## ## Collecting and validating inputs ## ## ## ## ## ## ##
        ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
        cur_unvalidated_inp <- mod_coll_inp_srv(NULL, cur_clean_inp$inp_str)
        cur_clean_inp$inp <- mod_check_inp_srv(NULL, cur_unvalidated_inp)

        # observeEvent(cur_clean_inp$inp(),
        #              {
        #                browser()
        #                shinyjs::enable("run_sim")
        #                shinyjs::enable(selector = "#main_sidebar li:nth-child(3) a")
        #              }, ignoreInit = TRUE, ignoreNULL = TRUE)

        ## ## ## Exportable inputs and key values tables
        clean_inp_values <- reactive(req(cur_clean_inp$inp()$inp))
        cur_clean_inp$export <- mod_export_inp_srv(NULL, clean_inp_values)
        cur_clean_inp$key <- mod_key_inp_srv(NULL, clean_inp_values)

        ## ## ## Rendering the data table
        output$inputs_ui_values <-
          DT::renderDT({
            # req(cur_clean_inp$export())
            cur_clean_inp$export() %>%
              select(-1) %>%
              fct_config_gen_dt("Policy scenarios summary", group_row = 1)
          },
          server = FALSE)

        ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
        # ## ## ## Testing
        ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
        observeEvent(input$browser, {
          # browser()
        })

        output$dynamic_inputs_ui_info <- renderPrint({
          list(
            cur_clean_inp$export() %>% str(),
            cur_clean_inp$key() %>% str()
          )
        })

        cur_clean_inp
      })
  }




#' Module for generating and re-generating inputs on the server
#'
#' @noRd
#' @import shiny
#' @importFrom waiter Waiter spin_loader transparent
#' @importFrom shinyjs disable enable
#' @export
mod_build_inp_srv <-
  function(id,
           inp_raw_str,
           inp_str_fn,
           ui_gen_fn,
           n_choices = function() {1},
           ncols = 12,
           nmax = 4,
           reseter = function() {0}) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns
      n_choices_checked <- reactiveVal(2)

      # UI generator
      # n_choices_checked <-
      shiny::observe({
          if (n_choices() > nmax)
            shiny::showNotification(
              stringr::str_c("Maximum number of policy choices is ", nmax),
              duration = 10,
              type = "warning"
            )
          new_nchoice <- min(nmax, n_choices())
          if (new_nchoice != n_choices_checked()) n_choices_checked(new_nchoice)
        })

      all_structures <- reactive({
        out <-
          c(1:nmax) %>%
          map(~{
            do.call(inp_str_fn,
                      list(inp_raw_str = inp_raw_str, n_choices = .x, ns = ns)) %>%
              list() %>%
              set_names(.x)
          }) %>%
          unlist(recursive = F)
      })

      all_uis <- reactive({
        out2 <-
          all_structures() %>%
          imap(~{
            do.call(ui_gen_fn, list(.x, add_rest_btn = TRUE, ns = ns))
          })
      })

      inp_str <-
        shiny::eventReactive(#
          list(n_choices_checked(), reseter()),
          {
            shinyjs::disable(("run_sim"))
            shinyjs::disable(selector = "#main_sidebar li:nth-child(3) a")

            new_str <- try({all_structures()[[n_choices_checked()]]}, silent = T)
            validate(#
              need(
                !"try-error" %in% class(new_str),
                "Check the UI-structure preparation function. It fails with an error."
              ),
              need(
                nrow(new_str) > 0,
                "Check the UI-structure preparation function. It does not
                produce any output."
              ))
            list(inp_str = new_str,
                 #if ("try-error" %in% class(new_str)) NULL else new_str,
                 # inp_str = gen_inp_str(inp_raw_str, n_choices_checked(), ns = ns)
                 timestamp = Sys.time(),
                 reseter = reseter())
          },
          ignoreInit = FALSE,
          ignoreNULL = TRUE)

      w <- waiter::Waiter$new(
        id = ns(c("dynamic_inputs_ui", "dynamic_inputs_ui_info")),
        html = waiter::spin_loader(),
        color = waiter::transparent(.5)
      )

      output$dynamic_inputs_ui <-
        shiny::renderUI({
          # w$show()
          n_choices_here <-
            inp_str()$inp_str %>%
            pull(policy_choice ) %>%
            unique() %>%
            length()

          out_ui <- try({all_uis()[[n_choices_here]]}, silent = T)
          shiny::validate(#
            shiny::need(
              !"try-error" %in% class(out_ui),
              "Check the UI-generation function. It fails with an error."
            ))
          req(out_ui$ui)
          shiny::validate(
            shiny::need(
              !is.null(out_ui$ui),
              "Check the UI-generation function. It does not produce a single
              `ui` component that could be build"
            )
          )
          return(out_ui$ui)
        })


      output$dynamic_inputs_ui_info <-
        shiny::renderPrint({
          inp_str()
        })
      #
      # observe({
      #   req(inp_str())
      #   shinyjs::enable(("run_sim"))
      # })

      return(inp_str)

    })

  }




#' Module for resetting scenarios to the baseline after the click on a designates button
#'
#' @noRd
#' @import shiny
#' @importFrom dplyr select filter group_by pull rowwise
#' @importFrom stringr str_c
#' @importFrom purrr map walk transpose pwalk
#' @importFrom tidyr nest
mod_reset_scenarios <-
  function(id, inp_str) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns

      resetters <-
        shiny::eventReactive(
        inp_str()$inp_str,
        {
          req(inp_str()$inp_str)

          reseters_id <-
            inp_str()$inp_str %>%
            dplyr::pull(policy_choice) %>%
            unique() %>%
            stringr::str_c("reset_", .) #%>%
            # ns()

          reseters_str <-
            inp_str()$inp_str %>%
            dplyr::select(policy_choice, inputId_local, type, base_value) %>%
            dplyr::filter(type != "textInput") %>%
            dplyr::group_by(policy_choice) %>%
            tidyr::nest() %>%
            dplyr::pull(data)

          out <-
            list(reseters_id = reseters_id, reseters_str = reseters_str) %>%
            purrr::transpose()

          out %>%
            purrr::map(~{input[[.x$reseters_id]]}) %>%
            resetters_previous_values()
          out
        }
      )

      resetters_previous_values <- shiny::reactiveVal()

      current_reset <-
        shiny::reactive({
          resetters() %>%
            purrr::map(~ {
              input[[.x$reseters_id]]
            })
        })

      shiny::observeEvent(
        current_reset(),
        {
          list(resetters_previous_values(),
               current_reset(),
               resetters()) %>%
            purrr::pwalk(~ {
              if (!is.null(..1) && ..2 > ..1) {
                ..3$reseters_str %>%
                  dplyr::rowwise() %>%
                  purrr::pwalk(~ {
                    dts <- rlang::dots_list(...)
                    if ("numericInput" %in% dts$type) {
                      shiny::updateNumericInput(session,
                                                dts$inputId_local,
                                                value = as.numeric(dts$base_value))
                    }

                    if ("textInput" %in% dts$type) {
                      shiny::updateTextInput(session,
                                             dts$inputId_local,
                                             value = as.character(dts$base_value))
                    }
                  })
                }
            })
          current_reset() %>% resetters_previous_values()
      },
      ignoreInit = TRUE)

    })

  }



#' Module for collecting inputs from existing structure
#'
#' @noRd
#' @importFrom shinyjs disable enable
#' @import shiny
mod_coll_inp_srv <-
  function(id, inp_str, debounce_rate = 50) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # new_inputs <-
      #   reactive({req(inp_str()$inp_str)}) %>%
      #   debounce(debounce_rate)

      # UI values collector
      out <-
      shiny::reactive(#
        label = "Inputs collector",
        {
          req(inp_str()$inp_str)
          current_out <- NULL
          current_out$inp <-
            inp_str()$inp_str %>%
            mutate(current_value = map(inputId_local, ~ {
              # if (is.null(input[[.x]])) {
              #   (NA_real_)
              # } else {
              #   input[[.x]]
              # }
              input[[.x]]
            })) %>%
            select(-single_ui)

          current_out$timestamp <- Sys.time()
          current_out$str_timestamp <- inp_str()$timestamp
          if (getOption("ceq_dev", FALSE)) {
            shiny::showNotification("inputs were recollected!", duration = 3)
          }
          current_out
        }) %>%
        debounce(debounce_rate)

      # reanable <- out %>% debounce(1500)
      observeEvent(out(), {
        shinyjs::enable(("run_sim"))
        shinyjs::enable(selector = "#main_sidebar li:nth-child(3) a")
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      out

    })

  }



#' Module to check inputs on threshold satisfaction and return validated inputs
#'
#' @noRd
#' @import shiny dplyr purrr
#' @importFrom shinyFeedback feedbackDanger
#' @importFrom tidyr replace_na
mod_check_inp_srv <-
  function(id, current_inp, debounce_rate = 50) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns
      clean_feedback <- reactiveVal(list())

      checked_inps <-
        shiny::reactive(#
          # current_inp()$inp,
          {
            req(current_inp())
            req(unlist(current_inp()$inp$current_value))
            n_nulls <-
              current_inp()$inp$current_value %>%
              purrr::map_lgl(is_null) %>%
              unlist() %>%
              as.integer() %>%
              sum()
            req(n_nulls < 3)
            current_inp()$inp %>%
              dplyr::rowwise() %>%
              dplyr::mutate(greater = map_lgl(current_value, ~ .x > max)) %>%
              dplyr::mutate(less = map_lgl(current_value, ~ .x < min)) %>%
              dplyr::mutate(na_val = map_lgl(current_value, ~ is.na(.x) | .x == "")) %>%
              tidyr::replace_na(list(less = FALSE, greater = FALSE, na_val = FALSE))
              # dplyr::mutate(current_value = ifelse(less, list(min), list(current_value)),
              #               current_value = ifelse(greater, list(max), list(current_value)))
          }) %>%
        debounce(debounce_rate)

      shiny::observeEvent(#
        checked_inps(),
        {
          req(nrow(checked_inps() %>% dplyr::filter(less | greater | na_val)) > 0)
          checked_inps() %>%
            dplyr::filter(less | greater | na_val) %>%
            purrr::pmap(~ {
              dts <- rlang::dots_list(...)
              msg <- str_c(
                "Must be ",
                if (!is.na(dts$min))
                  str_c("above '", dts$min, "' "),
                if (!is.na(dts$min) & !is.na(dts$max))
                  "and ",
                if (!is.na(dts$max))
                  str_c("below '", dts$max, "'"),
                ". It was corrected to the nearest value automatically."
              )

              if (dts$na_val) {
                msg <- "Missing values are not allowed. It was corrected to the baseline."
              }

              # Alerts
              shiny::showNotification(str_c("Input '", dts$label, "' ", msg),
                                      duration = 15,
                                      type = "warning")

              shinyFeedback::feedbackDanger(
                inputId = dts$inputId_local,
                show = TRUE,
                text = msg,
                color = "#d9534f",
                icon = shiny::icon("ok", lib = "glyphicon"),
                session = session
              )

              clean_feedback(append(clean_feedback(), list(dts$inputId_local)))

              # Correcting
              if ("numericInput" %in% dts$type) {
                new_val <- dts$min
                if (dts$greater)
                  new_val <- dts$max
                if (dts$na_val)
                  new_val <- as.numeric(dts$base_value)
                shiny::updateNumericInput(session, dts$inputId_local, value = new_val)
              }

              if ("textInput" %in% dts$type) {
                shiny::updateTextInput(session, dts$inputId_local, value = "Specify a name")
              }
            })
        }, ignoreInit = TRUE)

      # Cleaning feedback
      clean_feedback_react <- reactive(clean_feedback()) %>% debounce(10000)
      observeEvent(#
        clean_feedback_react(), {
          req(length(clean_feedback_react()) > 0)
          clean_feedback_react() %>%
            walk(~shinyFeedback::hideFeedback(inputId = .x, session = session))
          clean_feedback(list())
        }, ignoreInit = TRUE
      )

      # Updating and returning validated inputs
      shiny::eventReactive(#
          checked_inps(),
          {
            out <- NULL
            out$inp <-
              checked_inps() %>%
              dplyr::mutate(current_value = ifelse(less, list(min), list(current_value)),
                            current_value = ifelse(greater, list(max), list(current_value)),
                            current_value = ifelse(na_val & type == "textInput" ,
                                                   list("Specify a name"), list(current_value)),
                            previous_value = current_value) %>%
              dplyr::select(-less, -greater, -na_val) %>%
              ungroup()
            out$timestamp <- Sys.time()
            out$str_timestamp <- current_inp()$str_timestamp
            out
          }, ignoreInit = FALSE)

    })
  }



#' Module to update input values if new structure was created (new scenario was added)
#'
#' @noRd
#' @import shiny dplyr purrr
mod_upd_old_vals_to_exist_inp <-
  function(id, cur_clean_inp) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # This is the last version of inputs.
      input_copy <- reactiveValues(inp = NULL)
      last_reset <- reactiveVal(NULL)


      # Save the last version of inputs if they are clean and validated
      observeEvent(#
        cur_clean_inp$inp(), {
          input_copy$inp <-
            cur_clean_inp$inp()$inp %>%
            select(any_of(c("policy_choice", "id", "inputId", "inputId_local",
                            "current_value", "previous_value")))
          input_copy$timestamp <- Sys.time()
          last_reset(cur_clean_inp$inp_str()$reseter)
        },
        priority = 0
      )

      # Bring back some values if number of scenarios change
      observeEvent(#
        cur_clean_inp$inp_str(), {

          shiny::req(input_copy$timestamp < cur_clean_inp$inp_str()$timestamp)

          # Another check in case if reset was done so to prevent inputs from update.
          # This is because at the moment, reset regenerates the entire input structure.
          shiny::req(
            !isTruthy(cur_clean_inp$inp_str()$reseter) |
              last_reset() == cur_clean_inp$inp_str()$reseter)

          check_dts <-
            function(dts) {
              ((
                !is.null(dts$current_value) &&
                  !is.na(dts$current_value) &&
                  !is.na(dts$base_value)
              ) &&
                isTRUE(unlist(dts$current_value)[[1]] != dts$base_value)) ||
                (
                  !is.null(dts$current_value) &&
                    is.na(dts$base_value) &&
                    !is.na(dts$current_value)
                )
            }

          cur_clean_inp$inp_str()$inp %>%
            left_join(input_copy$inp, by = c("policy_choice", "id", "inputId", "inputId_local")) %>%
            purrr::pmap( ~ {
              dts <- rlang::dots_list(...)
              if (check_dts(dts)) {

                if ("numericInput" %in% dts$type) {
                  shiny::updateNumericInput(session,
                                            dts$inputId_local,
                                            value = as.numeric(dts$current_value))
                }

                if ("textInput" %in% dts$type) {
                  # if (is.null(dts$current_value)) {
                  #   browser()
                  #   choice_number <- str_extract(dts$policy_choice, "\\d")
                  #   new_name <- str_c("Policy choice ",  choice_number)
                  # } else
                  # {
                  #   new_name <- dts$current_value
                  # }
                  shiny::updateTextInput(session, dts$inputId_local,
                                         value = as.character(dts$current_value))
                }

                if (getOption("ceq_dev", FALSE)) {
                  str_c(dts$inputId_local,
                        " will update with the old value ",
                        unlist(dts$current_value)) %>%
                    shiny::showNotification(ui = ., duration = 5, type = "warning")
                }
              }
            })
        },
        priority  = 1000)


    })

  }




#' Module to update input values if new structure was created (new scenario was added)
#'
#' @noRd
#' @import shiny dplyr purrr

mod_upd_inp_srv <-
  function(id, new_val) {
    shiny::moduleServer(#
      id, function(input, output, session) {
        ns <- session$ns

        observeEvent(#
          new_val(),
          {
            new_val()$inp %>%
              purrr::pmap( ~ {
                dts <- rlang::dots_list(...)
                if ("numericInput" %in% dts$type) {
                  shiny::updateNumericInput(session,
                                            dts$inputId_local,
                                            value = as.numeric(dts$current_value))
                }

                if ("textInput" %in% dts$type) {
                  shiny::updateTextInput(session,
                                         dts$inputId_local,
                                         value = as.character(dts$current_value))
                }
              })
          },
          ignoreNULL = TRUE,
          ignoreInit = TRUE)

      })
  }


#' Module to prepare scenario data into an exportable table
#'
#' @noRd
#' @import shiny dplyr purrr
#' @importFrom tidyr pivot_wider
mod_export_inp_srv <-
  function(id, cur_clean_inp) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns
      shiny::eventReactive(#
        cur_clean_inp(), {
          req(cur_clean_inp())
          policy_choice <-
            cur_clean_inp() %>%
            dplyr::ungroup() %>%
            dplyr::filter(type == "textInput") %>%
            dplyr::mutate(policy_name = as.character(current_value)) %>%
            dplyr::select(policy_choice, policy_name)
          cur_clean_inp() %>%
            dplyr::ungroup() %>%
            dplyr::filter(type != "textInput") %>%
            dplyr::left_join(policy_choice, by = "policy_choice") %>%
            dplyr::mutate(current_value = as.numeric(unlist(current_value))) %>%
            dplyr::select(
              `Input id` = id,
              `Input name` = label,
              `Group` = group_name,
              `Baseline value` = base_value,
              policy_name,
              current_value
            ) %>%
            tidyr::pivot_wider(names_from = policy_name, values_from = current_value)
        },
        ignoreInit = FALSE,
        ignoreNULL = TRUE)
    })
  }




#' Module to check what policy scenario has changed
#'
#' @noRd
#' @import shiny
mod_key_inp_srv <-
  function(id, cur_clean_inp) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns
      # key_summary <- reactive()
      shiny::eventReactive(#
        cur_clean_inp(), {
          req(cur_clean_inp())
          out <- try({
            cur_clean_inp() %>%
              fct_prep_key_inp()
          }, silent = TRUE)

          validate(#
            need(
              !"try-error" %in% class(out),
              "Check the 'fct_prep_key_inp' function. It fails with an error."
            )
          )

          (out)
          #
          # if (length(previous_summary()) == 0) {
          #   scenario_summary %>%
          #     previous_summary()
          # } else {
          #   seq_along(scenario_summary) %>%
          #     map(~{
          #       xx <- scenario_summary[.x]
          #       yy <- previous_summary()[.x]
          #       xx[[1]]$policy_choices_new = is.null(yy)
          #       if (!xx[[1]]$policy_choices_new) {
          #         xx[[1]]$policy_name_same = xx[[1]]$policy_name == yy[[1]]$policy_name
          #         xx[[1]]$policy_choices_as_before = all(xx[[1]]$policy_choices == yy[[1]]$policy_choices)
          #       }
          #       xx[[1]]$timestamp <- Sys.time()
          #       xx
          #     }
          #   ) %>%
          #     unlist(recursive = F) %>%
          #     previous_summary()
          # }

        },
        ignoreInit = FALSE,
        ignoreNULL = TRUE)

    })
  }



