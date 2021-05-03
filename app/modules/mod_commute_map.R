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
    
    tooltip_mb <- "<div style='color:black'>{{SA22018__1}}</div>"
    tooltip_selected_mb <- "<div style='color:black; font-weight: bold'>{{SA22018__1}}</div>"
    tooltip_highlighted_bucket <- "<div style='color:black'>Something about {{SA22018__1}}</div>"
    tooltip_highlighted_mb <- "<div style='color:black'><div style='font-weight: bold'>{{SA22018__1}}</div><div style='white-space:nowrap'>{{commute_all}} commuters {{direction}} <strong>{{name}}</strong></div></div>"
    
    output$map <- renderMapboxer({
      MAP_SRC %>% 
        mapboxer(
          style = "mapbox://styles/mapbox/dark-v9",
          token = MAPBOX_TOKEN,
          center = c(174.763336, -36.848461),
          zoom = 10
        ) %>% 
        
        # map overlay shown when mb was clicked on
        add_fill_layer(
          fill_color = c("get", "color"), fill_opacity = 0.5, id = "highlight-mb", 
          source = as_mapbox_source(SF_SHAPE[0,] %>% mutate(color = "#000000")),
          fill_sort_key = 10
        ) %>% 
        
        # map overlay shown when chart bucket was clicked on
        add_fill_layer(
          fill_color = c("get", "color"), fill_opacity = 0.75, id = "highlight-bucket", 
          source = as_mapbox_source(SF_SHAPE[0,] %>% mutate(color = "#000000")),
          fill_sort_key = 11
        ) %>% 
        
        # map overlay showing the clicked-on meshblock
        add_fill_layer(
          fill_color = COLOR_ORANGE, fill_opacity = 0.75, id = "selected-mb", 
          source = as_mapbox_source(SF_SHAPE[0,]), fill_sort_key = 12
        ) %>% 
        
        # map overlay of all meshblocks
        add_fill_layer(
          fill_color = "rgba(1,1,1,0)", fill_outline_color = "rgba(255,255,255,0.5)", id = "mb", 
          fill_sort_key = 1
        ) %>% 
        
        # add tooltips for all layers
        add_tooltips(layer_id = "mb", tooltip = tooltip_mb) %>% 
        add_tooltips(layer_id = "highlight-mb", tooltip = tooltip_highlighted_mb) %>% 
        add_tooltips(layer_id = "highlight-bucket", tooltip = tooltip_highlighted_bucket) %>% 
        add_tooltips(layer_id = "selected-mb", tooltip = tooltip_selected_mb)
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
    
    # highlighted meshblocks after map click
    d_highlighted_mb <- reactive({
      req(state$state$id == STATE_MB_SELECTED)

      extract_mb(state$state$store$selected_mb, state$direction) %>% 
        mutate(
          highlight_val = commute_all, 
          selected_mb = state$state$store$selected_mb
        )
    })
    
    # highlighted meshblocks after chart bucket click
    d_highlighted_bucket <- reactive({
      req(state$state$id == STATE_BUCKET_SELECTED)
      
      agg_departure_arrival_summary(state$direction) %>% 
        inner_join(state$state$store$d_areas, by = c("code" = "area")) %>% 
        mutate(highlight_val = .[[state$state$store$selected_mode]])
    })
    
    # redraw map on chart bucket click
    observeEvent(d_highlighted_bucket(), {
      req(state$state$id == STATE_BUCKET_SELECTED, state$direction)

      # extract highlighted shapes
      sf_shape_subset <- SF_SHAPE %>% 
        inner_join(
          d_highlighted_bucket() %>% transmute(code = as.character(code), color = COLOR_ORANGE),
          by = c("SA22018_V1" = "code")
        )

      # push new data into the `highlight-bucket` map layer
      # NOTE: we need to use `ns()` to retrieve the correct map object
      mapboxer_proxy(ns("map")) %>%
        set_data(data = sf_shape_subset, source_id = "highlight-bucket") %>%
        set_data(data = SF_SHAPE[0,], source_id = "selected-mb") %>%
        set_data(data = SF_SHAPE[0,], source_id = "highlight-mb") %>%
        update_mapboxer()
    })
    
    # redraw map on map click
    observeEvent(d_highlighted_mb(), {
      # only highlight map if app is in STATE_MB_SELECTED
      req(state$state$id == STATE_MB_SELECTED, state$direction)
      
      # create a color gradient
      pal_type <- switch(state$direction, depart = "Blues", arrive = "Reds")
      pal <- scales::col_bin(pal_type, d_highlighted_mb()$highlight_val)
      
      # extract highlighted shapes
      sf_shape_subset <- SF_SHAPE %>% 
        inner_join(
          d_highlighted_mb() %>% 
            filter(code != selected_mb) %>%     # remove the selected mb, we render it in a separate map layer
            inner_join(D_LOOKUP %>% mutate(id = as.character(id)), by = c(selected_mb = "id")) %>% 
            transmute(
              code = as.character(code), 
              color = pal(highlight_val),
              commute_all = commute_all,
              direction = if (state$direction == "depart") "from" else "into",
              name = name
            ),
          by = c("SA22018_V1" = "code")
        )
      
      # retrieve the shape of the selected meshblock
      sf_selected_mb <- SF_SHAPE %>% 
        filter(SA22018_V1 == d_highlighted_mb()$selected_mb[1])

      # push new data into the `highlight-mb` map layer
      # NOTE: we need to use `ns()` to retrieve the correct map object
      mapboxer_proxy(ns("map")) %>%
        set_data(data = sf_shape_subset, source_id = "highlight-mb") %>%
        set_data(data = sf_selected_mb, source_id = "selected-mb") %>% 
        set_data(data = SF_SHAPE[0,], source_id = "highlight-bucket") %>%
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
