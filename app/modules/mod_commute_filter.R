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
    # div(
    #   style = "clear: both; text-align: right",
    #   prettyRadioButtons(
    #     inputId = ns("direction"),
    #     choices = c(DEPART = "depart", ARRIVE = "arrive"),
    #     selected = INITIAL_DIRECTION,
    #     label = NULL,
    #     width = "auto",
    #     animation = "jelly",
    #     inline = TRUE
    #   )
    # ),
    top = "0", right = "0", width = "100%",
    style = "padding: 0 7px 0 20px; margin-top: -15px;"
  )
}



# Server logic ------------------------------------------------------------

mod_commute_filter <- function(id, state) {
  server <- function(input, output, session) {
    # observeEvent(input$direction, {
    #   state$direction <- input$direction
    # })
    
    observe({
      req(D_COMMUTE)
      choices <- sort(unique(c(D_COMMUTE$commute_from_region, D_COMMUTE$commute_to_region)))
      updatePickerInput(session, "region", choices = choices, selected = choices[2])
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
