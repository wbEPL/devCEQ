
#' info_page Server Function
#'
#' @noRd
#' @import shiny
#' @importFrom htmltools HTML

mod_info_page_server <-
  function(id = NULL,
           first_tab,
           navbar_id = "main_sidebar",
           how_to_tab = "howto",
           ...) {
    moduleServer(# 
      id,
      function(input, output, session) {
        ns <- session$ns
        active_tab <- reactiveValues(current = NULL, previous = first_tab)
        
        # guide <- compile_guides()$init(session = session)
        run_guid_click <- reactiveVal(0)
        help_tab_click <- reactiveVal(0)
        guid_from_step <- reactiveVal(1)
        
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
                tagList(shiny::markdown(
                  readLines("./inst/app/info-page.md", warn = F)
                )),
                size = "xl",
                easyClose = TRUE,
                footer =  tagList(
                  modalButton("Close"),
                  actionButton(
                    "tour",
                    "Show how it works",
                    class = "btn-success",
                    icon = icon(name = "arrow-circle-right")#,
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
          updateNavbarPage(session = session,
                           inputId = navbar_id,
                           selected =  how_to_tab)
        })
        
        # Next click -------------------------------
        # observeEvent(input$apps_guide_cicerone_next,
        #              {
        #                if (input$apps_guide_cicerone_next$previous == names(spatialPovertyExplorer::gifs_base[[7]])) {
        #                  # browser()
        #                  updateNavbarPage(session = session,
        #                                   inputId = "selected_tab",
        #                                   selected =  "Analysis")
        #
        #                  removeModal(session = getDefaultReactiveDomain())
        #                  guid_from_step(8)
        #                  new_val <- run_guid_click() + 1
        #                  run_guid_click(new_val)
        #                }
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
        # get_help <- reactive({
        #   list(help_tab_click(), run_guid_click())
        # }) %>%
        #   debounce(150)
        
        # # Universal restart guide ------------------------
        # observeEvent(get_help(), {
        #   req(any(unlist(get_help()) != 0))
        #   removeModal(session = getDefaultReactiveDomain())
        #   # browser()
        #   guide$start(session = session, step = guid_from_step())
        #
        # })
        
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
compile_guides <- function() {

  out_cice <-
    Cicerone$new(#
      id = "apps_guide",
      opacity = 0.35)$step(
        el = "well1",
        title = "Key controles",
        description = shiny::markdown("More elaborate description of the step or some visual aid in form of a GIF images."),
        position = "right-top",
        show_btns = TRUE,
        tab = "Policy Choices",
        tab_id = "main_sidebar"
      )$step(
        el = "well2",
        is_id = TRUE,
        title = "Modify simulation parameters",
        description = shiny::markdown("More elaborate description of the step or some visual aid in form of a GIF images."),
        position = "bottom-center",
        show_btns = TRUE,
        tab = "Policy Choices",
        tab_id = "main_sidebar"
      )
  out_cice


}
