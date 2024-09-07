
#' info_page Server Function
#'
#' @noRd
#' @import shiny
#' @importFrom htmltools HTML
#' @export

mod_info_page_server <-
  function(id = NULL,
           first_tab,
           navbar_id = "main_sidebar",
           how_to_tab = "howto",
           info_page_md = NULL,
           info_page_size = "l",
           ui_ns = NS(NULL),
           ...) {
    moduleServer(#
      id,
      function(input, output, session) {
        ns <- session$ns
        active_tab <- reactiveValues(current = NULL, previous = first_tab)

        guide <- compile_guides(ns = ui_ns)$init(session = session)
        run_guid_click <- reactiveVal(0)
        help_tab_click <- reactiveVal(0)
        guid_from_step <- reactiveVal(1)

        # Searching info page file.
        if (is.null(info_page_md)) info_page_md <- "./info-page.md"
        if (!file.exists(info_page_md)) info_page_md <- " "
        else info_page_md  <- readLines(info_page_md, warn = F)

        # Get back to the valid tab observer from info ----------------------
        observeEvent(#
          input$main_sidebar, #
          {
            if (input$main_sidebar == "Info") {
              updateNavbarPage(
                session = session,
                inputId = navbar_id,
                selected =  active_tab$previous
              )

              showModal(modalDialog(
                # title = as.character(getOption("current.app.name", "CEQ")),
                tagList(shiny::markdown(info_page_md)),
                size = info_page_size,
                easyClose = TRUE,
                footer =  tagList(
                  modalButton("Close"),
                  actionButton(
                    ns("tour"),
                    "Show how it works",
                    class = "btn-success",
                    icon = icon(name = "circle-arrow-right")#,
                    # class = "disabled"
                  )
                )
              ))

            } else if (input$main_sidebar == how_to_tab) {
              updateNavbarPage(
                session = session,
                inputId = navbar_id,
                selected =  active_tab$previous
              )
              new_val <- run_guid_click() + 1
              run_guid_click(new_val)
            } else {
              active_tab$previous <- input$main_sidebar
            }

          })


        # Start from info buton ---------------------
        observeEvent(input$tour, {
          removeModal(session = getDefaultReactiveDomain())
          new_val <- run_guid_click() + 1
          run_guid_click(new_val)
          updateNavbarPage(session = session,
                           inputId = navbar_id,
                           selected =  how_to_tab)
        })

        # Next click -------------------------------
        # observeEvent(input$apps_guide_cicerone_next,
        #              {
        #                browser()
        #
        #                # if (input$apps_guide_cicerone_next$previous == names(spatialPovertyExplorer::gifs_base[[7]])) {
        #                #   # browser()
        #                #   updateNavbarPage(session = session,
        #                #                    inputId = "selected_tab",
        #                #                    selected =  "Analysis")
        #                #
        #                #   removeModal(session = getDefaultReactiveDomain())
        #                #   guid_from_step(8)
        #                #   new_val <- run_guid_click() + 1
        #                #   run_guid_click(new_val)
        #                # }
        #              })
        #
        # # Previous click -------------------------------
        # observeEvent(input$apps_guidecicerone_previous,
        #              {
        #                if (input$apps_guidecicerone_previous$previous ==  "country_change") {
        #                  updateNavbarPage(session = session,
        #                                   inputId = "selected_tab",
        #                                   selected =  "Map")
        #
        #                  removeModal(session = getDefaultReactiveDomain())
        #                  guid_from_step(7)
        #                  new_val <- run_guid_click() + 1
        #                  run_guid_click(new_val)
        #                }
        #              })

        # Start from Get help -------------------------
        get_help <- reactive({
          list(help_tab_click(), run_guid_click())
        }) %>%
          debounce(150)

        # Universal restart guide ------------------------
        observeEvent(get_help(), {
          req(any(unlist(get_help()) != 0))
          guide$start(session = session, step = guid_from_step())

        })

        # Current tab to pass on with previous
        previous_tab <- reactiveVal(NULL)
        current_tab <- reactiveVal(NULL)

        observe({
          req(input[[navbar_id]])
          isolate({
            previous_tab(current_tab())
            current_tab(input[[navbar_id]])
          })
        })

        reactive({
          list(current = current_tab(),
               previous = previous_tab())
        })

      })


  }



#' Compile guides functoin
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom htmltools HTML
#' @importFrom cicerone Cicerone
compile_guides <- function(ns = NS(NULL)) {

  out_cice <-
    Cicerone$new(#
      id = "apps_guide",
      opacity = 0.35
      )$

    step(
      el = ns("inputs_controls_holder"),
      is_id = TRUE,
      title = "Here you can change the different policy options.",
      # description = shiny::markdown(
      #   "More elaborate description of the step or some visual aid in form of a GIF images."
      # ),
      position = "right",
      show_btns = TRUE#
    )$

    step(
      el = ns("input_sim_number_holder"),
      is_id = TRUE,
      title = "Start with adjusting the number of policy simulations.",
      # description = shiny::markdown("More elaborate description of the step or some visual aid in form of a GIF images."),
      position = "right",
      show_btns = TRUE
    )$

    step(
      el = ns("policy_choices_tabs_1"),
      is_id = TRUE,
      title = "Then, modify relevant policy parameters one by one.",
      position = "left",
      show_btns = TRUE
    )$

    step(
      el = ns("policy_names_holder"),
      is_id = TRUE,
      title = "Provide self-explanatory titles of the simulated scenarios.",
      position = "bottom",
      show_btns = TRUE
    )$

    step(
      el = ns("policy_choices_holder"),
      is_id = TRUE,
      title = "Revise inputs.",
      description =
        shiny::markdown("Be mindful of limitations within which policy parameters
                        could be modified. Scroll down to see all policy options."),
      position = "left-center",
      show_btns = TRUE
    )$

    step(
      el = ns("reset_policy1"),
      is_id = TRUE,
      title = "Policy-specific 'Reset' buttons revert changes to the baseline.",
      position = "bottom",
      show_btns = TRUE
    )$

    step(
      el = ns("input_tabs_nav_holder"),
      is_id = TRUE,
      title = "Navigate across all policy parameters using these tabs.",
      position = "right-center",
      show_btns = TRUE#,
      # on_highlight_started = str_c(
      #   'function() {
      # $("#', ns("policy_tabs"),'a[data-value=\'panel1\']").tab("show");
      # }')
    )$

    step(
      el = ns("input_tabs_nav_holder_3"),
      is_id = TRUE,
      title =
        "'Summary' tab contains a table with all policy parameters compared side-by side.",
      description =
        shiny::markdown("It compares provided values with the baseline and
                        highlights inputs that are different from the baseline"),
      position = "right-bottom",
      show_btns = TRUE#,
      # on_highlight_started = str_c(
      #   'function() {
      # $("#', ns("policy_tabs"),'a[data-value=\'summary\']").class("active");
      # }')
    )$

    step(
      el = ns("run_btn_holder"),
      is_id = TRUE,
      title = "Press Run to execute simulations.",
      description =
        shiny::markdown("'Run' must be pressed every time when the desired policy parameters are updated."),
      position = "right",
      show_btns = TRUE
    )$

    step(
      el = ns("download_sim_holder"),
      is_id = TRUE,
      title = "Download simulaiton inputs in a single file for later use",
      description =
        shiny::markdown("Such file can be used to restore old policy parameters
                        and continue modifying them. This file has an
                        app-specific extension `.ceqsim`"),
      position = "right",
      show_btns = TRUE
    )$

    step(
      el = ns("upload_sim_holder"),
      is_id = TRUE,
      title = "Restore previously saved simulations.",
      # description = shiny::markdown("More elaborate description of the step or some visual aid in form of a GIF images."),
      position = "right",
      show_btns = TRUE
    )$

    step(
      el = ns("reset_btn_0"),
      is_id = TRUE,
      title = "To start everything from the scratch, use the 'Reset' button or reload the app",
      position = "right",
      show_btns = TRUE,
      on_highlight_started = str_c(
        'function() {
        $("#main_sidebar a[data-value=\'Policy Choices\']").tab(\'show\');
      }')
    )$

    step(
      el = "main_sidebar",
      is_id = TRUE,
      title = "Navigate to the 'Results' tab.",
      position = "bottom",
      show_btns = TRUE,
      on_highlight_started = str_c(
        'function() {$("#main_sidebar a[data-value=\'Results\']").tab(\'show\');}')
    )$

    step(
      el = ns("policy_choices_holder_2"),
      is_id = TRUE,
      title = "Return to the Policy Choices page",
      position = "left-center",
      show_btns = TRUE,
      on_highlight_started = str_c(
        'function() {$("#main_sidebar a[data-value=\'pc2019\']").tab(\'show\');}')
    )


  out_cice


}
