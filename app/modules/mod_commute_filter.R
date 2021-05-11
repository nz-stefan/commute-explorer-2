################################################################################
# Module to capture all app filters
#
# Author: Stefan Schliebs
# Created: 2021-04-08 09:19:56
################################################################################



# UI definitions ----------------------------------------------------------

mod_commute_filter_ui <- function(id) {
  ns <- NS(id)
  
  absolutePanel(
    div(
      style = "float: right",
      pickerInput(ns("region"), label = NULL, choices = NULL, width = "200px")
    ),
    top = "0", right = "0", width = "100%",
    style = "padding: 0 7px 0 20px; margin-top: 6px;"
  )
}



# Server logic ------------------------------------------------------------

mod_commute_filter <- function(id, state) {
  server <- function(input, output, session) {
    observe({
      choices <- c("All Regions", sort(unique(c(D_COMMUTE$commute_from_region, D_COMMUTE$commute_to_region))))
      updatePickerInput(session, "region", choices = choices, selected = INITIAL_REGION)
    })
    
    observeEvent(input$region, {
      state$region <- input$region
      state$state <- list(
        id = STATE_NOTHING_SELECTED, 
        store = list()
      )
    })
  }
  
  moduleServer(id, server)
}
