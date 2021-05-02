################################################################################
# Module encapsulating map functionality
#
# Author: Stefan Schliebs
# Created: 2021-04-08 09:45:11
################################################################################



# UI definitions ----------------------------------------------------------

mod_commute_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    mapboxerOutput(ns("map"), height = "100%"),
    uiOutput(ns("selected_area"))
  )
}



# Server logic ------------------------------------------------------------

mod_commute_map <- function(id, state) {
  server <- function(input, output, session) {
    ns <- session$ns
    
    # Map ---------------------------------------------------------------------
    
    output$map <- renderMapboxer({
      MAP_SRC %>% 
        mapboxer(
          style = "mapbox://styles/mapbox/dark-v9",
          token = MAPBOX_TOKEN,
          center = c(174.763336, -36.848461),
          zoom = 10
        ) %>% 
        add_fill_layer(
          fill_color = c("get", "color"), fill_opacity = 0.5, id = "highlight", 
          source = as_mapbox_source(SF_SHAPE[1,] %>% mutate(color = "#000000"))
        ) %>% 
        add_tooltips(layer_id = "mb", tooltip = "<div style='color:black'>{{SA22018__1}}</div>") %>% 
        add_fill_layer(fill_color = "rgba(1,1,1,0)", fill_outline_color = "rgba(255,255,255,0.5)", id = "mb")
    })
    
    
    # Map clicks --------------------------------------------------------------
    
    observeEvent(input$map_onclick, {
      # unpack the meshblock associated with the selected shape
      selected_mb <- input$map_onclick$props$SA22018_V1
      
      # change the app state
      state$state <- list(
        id = STATE_MB_SELECTED, 
        store = list(selected_mb = selected_mb)
      )
    })
    
    
    # Highlight map -----------------------------------------------------------
    
    d_highlighted <- reactive({
      if (state$state$id == STATE_MB_SELECTED) {
        extract_mb(state$state$store$selected_mb, state$direction) %>% 
          mutate(highlight_val = commute_all)
      } else if (state$state$id == STATE_BUCKET_SELECTED) {
        agg_departure_arrival_summary(state$direction) %>% 
          inner_join(state$state$store$d_areas, by = c("code" = "area")) %>% 
          mutate(highlight_val = .[[state$state$store$selected_mode]])
      }
    })
    
    observeEvent(d_highlighted(), {
      # only highlight map if app is in STATE_MB_SELECTED 
      req(state$state$id %in% c(STATE_MB_SELECTED, STATE_BUCKET_SELECTED), state$direction)
      
      # create a color gradient
      pal_type <- switch(state$direction, depart = "Blues", arrive = "Reds")
      pal <- scales::col_bin(pal_type, d_highlighted()$highlight_val)
      
      # extract highlighted shapes
      sf_shape_subset <- SF_SHAPE %>% 
        inner_join(
          d_highlighted() %>% 
            transmute(code = as.character(code), color = pal(highlight_val)),
          by = c("SA22018_V1" = "code")
        )

      # push new data into the `highlight` map layer
      # NOTE: we need to use `ns()` to retrieve the correct map object
      mapboxer_proxy(ns("map")) %>%
        set_data(data = sf_shape_subset, source_id = "highlight") %>%
        update_mapboxer()
    })
    

    # Selected area panel -----------------------------------------------------

    output$selected_area <- renderUI({
      # only highlight map if app is in STATE_MB_SELECTED 
      req(state$state$id == STATE_MB_SELECTED, state$direction)
      
      area_name <- D_LOOKUP %>% filter(id == state$state$store$selected_mb) %>% pull(name)
      absolutePanel(
        h4(area_name, style = "text-transform: uppercase; font-weight: bold; font-size: 12px;"),
        top = 0, left = 0, width = 400, height = 40, style = "background-color: #191a1ac7; padding: 0 5px 0 15px; border-bottom-right-radius: 5px;"
      )
    })
    
  }
  
  moduleServer(id, server)
}
