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
    prettyRadioButtons(
      inputId = ns("direction"),
      choices = c(DEPART = "depart", ARRIVE = "arrive"),
      selected = INITIAL_DIRECTION,
      label = NULL,
      width = "auto",
      animation = "jelly",
      inline = TRUE
    ),
    top = "0", right = "15px", style = "margin-top: -9px"
  )
}



# Server logic ------------------------------------------------------------

mod_commute_filter <- function(id, state) {
  server <- function(input, output, session) {
    observeEvent(input$direction, {
      state$direction <- input$direction
    })
  }
  
  moduleServer(id, server)
}
