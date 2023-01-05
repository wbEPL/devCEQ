
# UIs ---------------------------------------------------------------------


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
mod_inputs_ui_wrapper <- function(id, inp_nav_width = NULL, ...) {
  ns <- NS(id)
  dots <- rlang::dots_list(...)

  if (is.null(inp_nav_width)) inp_nav_width <- 4

  text_field <-
    if (golem::app_dev()) {
      shiny::verbatimTextOutput(ns("inputs_out"))
    } else {
      NULL
    }

  left_col <-
    wellPanel(mod_inputs_btns_ui(id)) %>%
    div(id = ns("inputs_controls_holder")) %>%
    column(width = inp_nav_width)

  right_col <-
    mod_inp_tabs_content_ui(id) %>%
    div(style = "min-height:600px") %>%
    div(id = ns("policy_choices_tabs_1")) %>%
    div(id = ns("policy_choices_tabs_2")) %>%
    tagList(text_field) %>%
    column(width = 12 - inp_nav_width) %>%
    tagList(
      # if (golem::app_dev()) {}
    )

  fluidRow(left_col, right_col)
}



# Servers -----------------------------------------------------------------


#' inputs Server Functions
#'
#' @inheritParams mod_inputs_btns_server
#' @noRd
#' @export
mod_inputs_server <-
  function(id,
           inp_raw_str,
           inp_str_fn,
           ui_gen_fn,
           run_guide = function() NULL,
           active_tab = function() NULL,
           id_result = NULL,
           target_tab = NULL,
           source_tab = NULL,
           n_policy = c(1, 2, 1),
           n_policy_type = c("numericInline", "numeric", "slider", "dropdown", "none"),
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
        n_max_choices = reactive(inp_btns$n_max_choices),
        upd_inp = reactive(inp_btns$upload_sim) %>% debounce(750),
        reseter = reactive(if(!isTruthy(inp_btns$reset)) {0} else {inp_btns$reset}),
        inp_str_fn = inp_str_fn,
        ui_gen_fn = ui_gen_fn
      )

      # sidepanel-related logic
      cur_inps_export <- reactive({req(cur_inps$inp())})

      # Upload-donwload-resetall module
      inp_btns <- mod_inputs_btns_server(
        NULL,
        sim_export_dta = cur_inps$inp,
        n_policy = n_policy,
        n_policy_type = n_policy_type
      )

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



#' Module for generating and re-generating inputs on the server
#'
#' @noRd
#' @import shiny
#' @export
mod_dyn_inp_srv <-
  function(id,
           inp_raw_str,
           n_choices = reactive(1),
           n_max_choices = reactive(3),
           upd_inp = reactive(NULL),
           reseter = reactive(NULL),
           inp_str_fn,
           ui_gen_fn,
           ...) {
    shiny::moduleServer(#
      id,
      function(input, output, session) {
        ns <- session$ns
        # Reactive values with inputs
        cur_clean_inp <- reactiveValues(inp = NULL)
        cur_clean_inp$n_ch <- n_choices

        ## ## ## Gen inputs UI
        cur_clean_inp$inp_str <-
          mod_build_inp_srv(NULL,
                            inp_raw_str = inp_raw_str,
                            inp_str_fn = inp_str_fn,
                            ui_gen_fn = ui_gen_fn,
                            n_choices = cur_clean_inp$n_ch,
                            n_max_choices = n_max_choices,
                            reseter = reseter)

        ## ## ## Rendering the Inputs UI
        mod_render_inp_tabs_srv(
          NULL,
          tabs = reactive(cur_clean_inp$inp_str()$all_uis$tabs),
          header = reactive(cur_clean_inp$inp_str()$all_uis$header),
          switches = reactive(cur_clean_inp$inp_str()$all_uis$switches),
          summary_tab = cur_clean_inp$export
        )

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

        ## ## ## Exportable inputs and key values tables
        clean_inp_values <- reactive(req(cur_clean_inp$inp()$inp))
        cur_clean_inp$export <- mod_export_inp_srv(NULL, clean_inp_values)
        cur_clean_inp$key <- mod_key_inp_srv(NULL, clean_inp_values)

        cur_clean_inp
      })
  }




#' Module for generating and re-generating inputs UI on the server
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
           inp_pre_structure = NULL,
           n_choices = reactive(1),
           n_max_choices = reactive(4),
           reseter = reactive(0),
           ...) {

    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # This number of policy choices that is compared with restrictions.
      n_poly <- reactiveVal(NULL)

      # Checking that the number of choices is not more than it should be.
      shiny::observeEvent(#
        n_choices(),{
          if (n_choices() > n_max_choices()) n_poly(n_max_choices())
          else if (n_choices() < 1) n_poly(1)
          else if (n_choices() >= 1 && n_choices() <= n_max_choices() &&
                   !isTRUE(n_choices() == n_poly()))
            n_poly(n_choices())
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE)

      update_ui <- reactive({
        req(n_poly())
        req(inp_raw_str)
        shinyjs::disable(("run_sim"))
        list(inp_raw_str, n_poly(), reseter())
      }) %>%
        debounce(250)


      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
      ### Gen table with UI elements


      # max_out <- reactive({
      #   try({
      #     inp_str_fn(inp_raw_str = inp_raw_str,
      #                n_choices = n_max_choices(),
      #                ns = ns)
      #   }, silent = T)
      # })
      #
      #
      # max_all_uis <- reactive({
      #   try({
      #     1:n_max_choices() %>%
      #       map(~{
      #         max_out() %>%
      #           filter(policy_choice %in% str_c("policy", 1:.x)) %>%
      #           ui_gen_fn(., ns = ns)
      #       })
      #   }, silent = T)
      # })


      all_ui_str <-
        shiny::eventReactive(#
          update_ui(),
          {
            # if (isTruthy(inp_pre_structure)) {
            #   browser()
            #   out <- try({
            #     max_out() %>%
            #       filter(policy_choice %in% str_c("policy", 1: n_poly()))
            #   })
            # } else {

            # Generating UI table  ### ### ### ### ### ### ### ### ### ### ###
            out <- try({
              inp_str_fn(inp_raw_str = inp_raw_str,
                         n_choices = n_poly(),
                         ns = ns)
            }, silent = T
            )

            # Gen UI switches and tabs.  ### ### ### ### ### ### ### ### ### ###
            all_uis <-  try({
              ui_gen_fn(out, ns = ns)
            }, silent = T
            )

            # Validating it  ### ### ### ### ### ### ### ### ### ### ### ###
            validate(#
              need(!"try-error" %in% class(out),
                   "`mod_build_inp_srv`: check `inp_str_fn`. It fails"),
              need(nrow(out) > 0,
                   "`mod_build_inp_srv`: check `inp_str_fn`. It does not produce rows"),
              need(!"try-error" %in% class(all_uis),
                   "`mod_build_inp_srv`: check `ui_gen_fn`. It failed.")
            )

            # }

            list(inp_str = out, timestamp = Sys.time(), reseter = reseter(), all_uis = all_uis)
          },
          ignoreInit = FALSE,
          ignoreNULL = TRUE
        )

      # # Waiter.
      # w <- waiter::Waiter$new(
      #   id = ns(c("dynamic_inputs_ui", "dynamic_inputs_ui_info")),
      #   html = waiter::spin_loader(),
      #   color = waiter::transparent(.5)
      # )

      return(all_ui_str)

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
                      # browser()
                      toval <- as.numeric(dts$base_value)
                      # if (is.na(toval)) toval <- NULL
                      shiny::updateNumericInput(session,
                                                dts$inputId_local,
                                                value = toval)
                    }

                    if ("checkboxInput" %in% dts$type) {
                      shiny::updateCheckboxInput(
                        session = session,
                        inputId = dts$inputId_local,
                        value = isTRUE(
                          dts$base_value == "1" ||
                            dts$base_value == 1 ||
                            dts$base_value == "TRUE" ||
                            isTRUE(dts$base_value)
                        )
                      )
                    }

                    if ("radioButtons" %in% dts$type) {
                      shiny::updateRadioButtons(
                        session = session,
                        inputId = dts$inputId_local,
                        selected = dts$base_value
                      )
                    }

                    if ("textInput" %in% dts$type) {

                      toval <- as.character(dts$base_value)
                      # if (is.na(toval)) toval <- NULL
                      shiny::updateTextInput(session,
                                             dts$inputId_local,
                                             value = as.toval)
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

      str_change <- reactiveVal(0)
      observe({
        req(out())
        isolate({
          str_change(str_change() + 1)
          golem::message_dev("inputs to output change/recollection",
                             str_change(),
                             "\n")
        })
      })

      # UI values collector
      out <-
        shiny::reactive(#
          label = "Inputs collector",
          {
            req(inp_str()$inp_str)
            current_out <- NULL
            current_out$inp <-
              inp_str()$inp_str %>%
              mutate(current_value =
                       map(inputId_local,
                            # type,
                            ~ {
                         # if ("checkboxInput" %in% .y) {
                         #   as.integer(input[[.x]])
                         # } else {
                           input[[.x]]
                         # }
                       })) %>%
              select(-single_ui)
            current_out$timestamp <- Sys.time()
            current_out$str_timestamp <- inp_str()$timestamp
            current_out
          }) %>%
        debounce(debounce_rate)

      # reanable <- out %>% debounce(1500)
      observeEvent(out(), {
        shinyjs::enable(("run_sim"))
        # shinyjs::enable(selector = "#main_sidebar li:nth-child(3) a")
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
            # browser()
            n_nulls <-
              current_inp()$inp$current_value %>%
              purrr::map_lgl(is_null) %>%
              unlist() %>%
              as.integer() %>%
              sum()
            # req(length(current_inp()$inp$current_value) - n_nulls > 10)
            current_inp_dta <- current_inp()$inp
            if (!"allowna" %in% names(current_inp_dta)) {
              current_inp_dta <- current_inp_dta %>% mutate(allowna = FALSE)
            } else {
              current_inp_dta <-
                current_inp_dta %>%
                mutate(allowna = as.logical(allowna)) %>%
                replace_na(list(allowna = FALSE))
            }
            current_inp_dta %>%
              # select(id, inputId, inputId_local, label, min, max,
              #        type, current_value, base_value) %>%
              filter(map_lgl(current_value, ~ !is.null(.x))) %>%
              mutate(
                max = ifelse(is.na(max), Inf, max),
                min = ifelse(is.na(min), -Inf, min)
                ) %>%
              dplyr::rowwise() %>%
              dplyr::mutate(greater = map_lgl(current_value, ~ .x > max )) %>%
              dplyr::mutate(less = map_lgl(current_value, ~ .x < min)) %>%
              dplyr::mutate(
                na_val = map2_lgl(current_value, base_value, ~ (is.na(.x) | .x == "") & !is.na(.y)),
                na_val = if_else(na_val & allowna, FALSE, na_val, na_val)
                ) %>%
              tidyr::replace_na(list(less = FALSE, greater = FALSE, na_val = FALSE)) %>%
              dplyr::mutate(greater = ifelse(type == "textInput", FALSE, greater)) %>%
              dplyr::mutate(less = ifelse(type == "textInput", FALSE, less))# %>%

              # dplyr::filter(less | greater | na_val)
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
                msg <- "Missing values are not allowed. They were corrected to the baseline."
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
                if (dts$greater) new_val <- dts$max
                if (dts$na_val) new_val <- as.numeric(dts$base_value)
                if (is.na(new_val)) new_val <- NULL
                shiny::updateNumericInput(session, dts$inputId_local, value = new_val)
              }

              if ("textInput" %in% dts$type) {
                shiny::updateTextInput(session, dts$inputId_local, value = "Specify a name")
              }

              if ("checkboxInput" %in% dts$type) {
                shiny::updateCheckboxInput(
                  session = session,
                  inputId = dts$inputId_local,
                  value = isTRUE(
                    dts$base_value == "1" || dts$base_value == 1 ||
                      dts$base_value == "TRUE" ||
                      isTRUE(dts$base_value)
                  )
                )
              }

              if ("radioButtons" %in% dts$type) {
                shiny::updateRadioButtons(
                  session = session,
                  inputId = dts$inputId_local,
                  selected = dts$base_value
                )
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
                  toval <- as.numeric(dts$current_value)
                  if (is.na(toval)) toval <- NULL
                  shiny::updateNumericInput(session,
                                            dts$inputId_local,
                                            value = toval)
                }

                if ("textInput" %in% dts$type) {
                  shiny::updateTextInput(session, dts$inputId_local,
                                         value = as.character(dts$current_value))
                }

                if ("checkboxInput" %in% dts$type) {
                  shiny::updateCheckboxInput(
                    session = session,
                    inputId = dts$inputId_local,
                    value = isTRUE(dts$current_value)
                  )
                }

                if ("radioButtons" %in% dts$type) {
                  shiny::updateRadioButtons(
                    session = session,
                    inputId = dts$inputId_local,
                    selected = dts$current_value
                  )
                }

                if (golem::app_dev()) {
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
                  toval <- as.numeric(dts$current_value)
                  # if (is.na(toval)) {
                  #   # browser()
                  #   # toval <- NULL
                  # }
                  shiny::updateNumericInput(session,
                                            dts$inputId_local,
                                            value = toval)
                }

                if ("textInput" %in% dts$type) {
                  shiny::updateTextInput(session,
                                         dts$inputId_local,
                                         value = as.character(dts$current_value))
                }

                if ("checkboxInput" %in% dts$type) {
                  shiny::updateCheckboxInput(
                    session = session,
                    inputId = dts$inputId_local,
                    value = isTRUE(dts$current_value)
                  )
                }

                if ("radioButtons" %in% dts$type) {
                  shiny::updateRadioButtons(
                    session = session,
                    inputId = dts$inputId_local,
                    selected = dts$current_value
                  )
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
          # browser()
          cur_clean_inp() %>%
            dplyr::ungroup() %>%
            dplyr::filter(type != "textInput") %>%
            dplyr::left_join(policy_choice, by = "policy_choice") %>%
            dplyr::mutate(
              current_value =
                map2(current_value, type, ~ {
                  if ("checkboxInput" %in% .y) {
                    as.integer(.x)
                  } else {
                    as.numeric(.x)
                  }
                }),
              label_change = str_detect(label, "^list\\("),
              label = map2_chr(label, label_change, ~ {
                out_label <- .x
                if (.y) {
                  lab <- eval(parse(text = .x))
                  out_label <- lab$label[[1]]
                }
                # out_label %>%
                #   strwrap(width=75, simplify=T) %>%
                #   str_c(collapse = "\n")
                out_label
              })
            ) %>%
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



# Testers -----------------------------------------------------------------





#' test_mod_inputs ui side
#'
#' @noRd
test_mod_inputs_ui <- function(id) {
  fluidPage(
    mod_inputs_ui_wrapper(id, 3),
    golem_add_external_resources()
  )
}


#' test_mod_inputs server side
#'
#' @noRd
test_mod_inputs_server <-
  function(id,
           path,
           n_policy = c(1, 5, 2),
           n_policy_type = c("numericInline", "numeric", "slider", "dropdown", "none"),
           ...) {

    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns
      inp_raw_str <- path %>% load_input_xlsx()
      inp_tab_str <- path %>% load_inputtabs_xlsx()
      inp_table_str <- path %>% load_inputtables_xlsx()

      local_inp_str = gen_inp_str_front(inp_table_str = inp_table_str)
      local_ui_fn = gen_tabinp_ui_front(inp_tab_str = inp_tab_str,
                                        inp_table_str = inp_table_str)

      cur_inps <- mod_dyn_inp_srv(
        id = id,
        inp_raw_str = inp_raw_str,
        n_choices = reactive(inp_btns$n_choices),
        n_max_choices = reactive(inp_btns$n_max_choices),
        upd_inp = reactive(inp_btns$upload_sim) %>% debounce(750),
        reseter = reactive(if(!isTruthy(inp_btns$reset)) {0} else {inp_btns$reset}),
        inp_str_fn = local_inp_str,
        ui_gen_fn = local_ui_fn
      )

      # Run btn click
      cur_inps$run <- reactive({
        list(value = inp_btns$run,
             timestamp = Sys.time())
      })

      # Upload-donwload-resetall module
      inp_btns <- mod_inputs_btns_server(
        NULL,
        sim_export_dta = cur_inps$inp,
        n_policy = n_policy,
        n_policy_type = n_policy_type
      )
    })
  }

#' Test complete inputs in one
#'
#' @noRd
#' @export
test_mod_inputs <-
  function(path,
           n_policy = c(1, 5, 2),
           n_policy_type = c("dropdown", "numericInline", "numeric", "slider", "dropdown", "none"),
           id = NULL,
           type = c("full", "selected")) {
    uui <-
      test_mod_inputs_ui(id) %>%
      tagList(profvis::profvis_ui("prof"))

    if (type == "selected") {
      sserv <- function(input, output, server) {

        callModule(profvis::profvis_server, "prof")

        test_mod_inputs_server(id, path = path, n_policy_type = n_policy_type)
      }
    } else {
      sserv <- function(input, output, server) {

        callModule(profvis::profvis_server, "prof")

        inp_raw_str <- path %>% load_input_xlsx()
        inp_tab_str <- path %>% load_inputtabs_xlsx()
        inp_table_str <- path %>% load_inputtables_xlsx()

        local_inp_str = gen_inp_str_front(inp_table_str = inp_table_str)
        local_ui_fn = gen_tabinp_ui_front(inp_tab_str = inp_tab_str,
                                          inp_table_str = inp_table_str)

        mod_inputs_server(
          id,
          inp_raw_str = inp_raw_str,
          inp_str_fn = local_inp_str,
          ui_gen_fn = local_ui_fn,
          n_policy = n_policy
        )
      }
    }
    shinyApp(uui, sserv)
  }

