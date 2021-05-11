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
    mapboxerOutput(ns("map"), height = "100%")
    # uiOutput(ns("selected_area"))
  )
}



# Server logic ------------------------------------------------------------

mod_commute_map <- function(id, state) {
  server <- function(input, output, session) {
    ns <- session$ns
    
    # Shape data --------------------------------------------------------------

    observeEvent(state$region, {
      req(map_rendered())
      
      # filter shapefile by selected region
      if (state$region == "All Regions") {
        sf_shape <- SF_SHAPE %>% filter(regc2020_name != "Area Outside Region")
      } else {
        sf_shape <- SF_SHAPE %>% filter(regc2020_name == state$region)
      }

      mapboxer_proxy(ns("map")) %>%
        set_data(data = sf_shape, source_id = "mb") %>%
        fit_bounds(sf::st_bbox(sf_shape)) %>%
        update_mapboxer()
    })
    
    
    # Map ---------------------------------------------------------------------
    
    tooltip_mb <- "<div>{{SA22018__1}}</div>"
    tooltip_selected_mb <- "<div><div style='font-weight: bold'>{{SA22018__1}}</div><div style='white-space:nowrap'>{{commute_all}} residents work in <strong>{{name}}</strong></div></div>"
    tooltip_highlighted_bucket <- "<div style='font-weight: bold;'>{{SA22018__1}}</div>"
    tooltip_highlighted_mb <- "<div><div style='font-size: 16px'><strong>{{commute_all}}</strong> commuters</div><div>{{direction}} <strong>{{SA22018__1}}</strong></div></div>"
    # tooltip_highlighted_mb <- "<div><div style='font-weight: bold'>{{SA22018__1}}</div><div style='font-size: 16px;padding-top:10px'><strong>{{commute_all}}</strong> commuters</div><div>{{direction}} <strong style='color:#fd9f02; filter: brightness(85%);'>{{name}}</strong></div></div>"
    
    map_rendered <- reactiveVal(FALSE)
    output$map <- renderMapboxer({
      sf_shape <- SF_SHAPE %>% filter(regc2020_name == INITIAL_REGION)
      state$map_id <- ns("map")
      map_rendered(TRUE)
      
      mapboxer(style = "mapbox://styles/mapbox/dark-v9", token = MAPBOX_TOKEN) %>% 
        
        # meshblock highlights
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
          fill_color = "rgba(255,255,255,0.1)", fill_outline_color = "rgba(255,255,255,0.5)", id = "mb", 
          fill_sort_key = 1, source = as_mapbox_source(sf_shape)
        ) %>% 
        
        # add tooltips for all layers
        add_tooltips(layer_id = "mb", tooltip = tooltip_mb) %>% 
        add_tooltips(layer_id = "highlight-mb", tooltip = tooltip_highlighted_mb) %>% 
        # add_tooltips(layer_id = "highlight-bucket", tooltip = tooltip_highlighted_bucket) %>% 
        add_tooltips(layer_id = "selected-mb", tooltip = tooltip_selected_mb) %>% 
        
        fit_bounds(sf::st_bbox(sf_shape))
    })
    
    
    # Map clicks --------------------------------------------------------------
    
    observeEvent(input$map_onclick, {
      # unpack the meshblock associated with the selected shape
      selected_mb <- input$map_onclick$props$SA22018_V1
      
      # change the app state
      state$state <- list(
        id = STATE_MB_SELECTED, 
        store = list(selected_mb = selected_mb, event_source = "map")
      )
    })
    
    
    # Highlight map -----------------------------------------------------------
    
    # highlighted meshblocks after map click
    d_highlighted_mb <- reactive({
      req(state$state$id == STATE_MB_SELECTED)

      extract_mb(state$d_commute, state$state$store$selected_mb, state$direction) %>% 
        mutate(
          highlight_val = commute_all, 
          selected_mb = state$state$store$selected_mb
        )
    })
    
    # highlighted meshblocks after chart bucket click
    d_highlighted_bucket <- reactive({
      req(state$state$id == STATE_BUCKET_SELECTED)
      
      agg_departure_arrival_summary(state$d_commute, state$direction) %>% 
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
        
        fit_bounds(st_bbox(sf_shape_subset), maxZoom = 10) %>% 
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
            inner_join(D_LOOKUP %>% mutate(id = as.character(id)), by = c(selected_mb = "id")) %>% 
            transmute(
              selected_mb = selected_mb,
              code = as.character(code), 
              color = pal(highlight_val),
              commute_all = commute_all,
              direction = if (state$direction == "depart") "Into" else "From",
              name = name
            ),
          by = c("SA22018_V1" = "code")
        )
      
      # retrieve the shape of the selected meshblock
      sf_selected_mb <- sf_shape_subset %>% 
        filter(SA22018_V1 == d_highlighted_mb()$selected_mb[1])
      
      # remove the selected mb, we render it in a separate map layer
      bbox <- st_bbox(sf_shape_subset)
      sf_shape_subset <- 
        sf_shape_subset %>% 
        filter(SA22018_V1 != selected_mb)
      
      # push new data into the `highlight-mb` map layer
      # NOTE: we need to use `ns()` to retrieve the correct map object
      mapboxer_proxy(ns("map")) %>%
        set_data(data = sf_shape_subset, source_id = "highlight-mb") %>%
        set_data(data = sf_selected_mb, source_id = "selected-mb") %>% 
        set_data(data = SF_SHAPE[0,], source_id = "highlight-bucket") %>%
        
        fit_bounds(bbox, maxZoom = 10) %>% 
        update_mapboxer()
    })
    
    # delete all map highlight if the app state changes to STATE_NOTHING_SELECTED 
    observeEvent(state$state, {
      req(state$state$id == STATE_NOTHING_SELECTED)
      mapboxer_proxy(ns("map")) %>%
        set_data(data = SF_SHAPE[0,], source_id = "highlight-mb") %>%
        set_data(data = SF_SHAPE[0,], source_id = "selected-mb") %>% 
        set_data(data = SF_SHAPE[0,], source_id = "highlight-bucket") %>%
        update_mapboxer()
    })
    

    # Selected area panel -----------------------------------------------------

    output$selected_area <- renderUI({
      req(state$state$id %in% c(STATE_MB_SELECTED, STATE_BUCKET_SELECTED), state$direction)
      
      if (state$state$id == STATE_MB_SELECTED) {
        area_name <- D_LOOKUP %>% filter(id == state$state$store$selected_mb) %>% pull(name)
        ui <- h4(area_name, class = "map-headline")
      } else if (state$state$id == STATE_BUCKET_SELECTED) {
        # build the ratio text snippet for the headline
        ratio <- state$state$store$selected_ratio * 100
        ratio_label <- sprintf("%s &ndash; %s%%", ratio - 2.5, ratio + 2.5)
        
        # build the mode of travel text snippet
        mode <- switch(
          state$state$store$selected_mode,
          "work_at_home" = "Work at Home", 
          "commute_car" = "Commute By Car", 
          "commute_public" = "Commute by Public Transport", 
          "commute_bicycle" = "Commute by Bike", 
          "commute_walk_or_jog" = "Walk or Jog"
        )
        
        # combine everything into a headline
        ui <- h4(HTML(
          paste(ratio_label, tags$span(mode, style = "color:white;font-size:10px;padding-left:5px"))), 
          class = "map-headline", style = "font-size: 16px"
        )
      }
      
      absolutePanel(
        ui,
        top = 0, left = 0, width = "100%", height = 42, 
        style = "background-color: #191a1ac7; padding: 0 5px 0 15px;"
      )
    })
    
  }
  
  moduleServer(id, server)
}
