################################################################################
# Server logic of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-13 09:21:18
################################################################################


server <- function(input, output, session) {
  
  # initialise the app state with the default STATE_NOTHING_SELECTED
  app_state <- reactiveValues(
    state = list(id = STATE_NOTHING_SELECTED, store = list()),
    direction = INITIAL_DIRECTION,
    region = INITIAL_REGION,
    window_height = 800,
    d_commute = NULL,
    map_id = NULL,
    data_source = INITIAL_DATA_SOURCE
  )
  
  # update the app state when the region is selected
  observeEvent(c(app_state$region, app_state$data_source, app_state$direction), {
    if (app_state$region == "All Regions") {
      app_state$d_commute <- D_COMMUTE %>% filter(source == app_state$data_source) %>% select(-source)
    } else {
      if (app_state$direction == "depart") {
        app_state$d_commute <- 
          filter(D_COMMUTE, source == app_state$data_source, commute_from_region == app_state$region) %>% 
          select(-source)
      } else {
        app_state$d_commute <- 
          filter(D_COMMUTE, source == app_state$data_source, commute_to_region == app_state$region) %>% 
          select(-source)
      }
    }
  }) 
  
  # update the app state when browser window is re-sized 
  observeEvent(input$window_height, app_state$window_height <- input$window_height)
  
  # add server logic for the commute explorer
  mod_commute_mode("mode", app_state)
  mod_commute_map("map", app_state)
  mod_commute_table("table", app_state)
  mod_commute_filter("filter", app_state)
}
