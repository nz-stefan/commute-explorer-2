################################################################################
# Mapboxer play
#
# Author: Stefan Schliebs
# Created: 2021-04-02 07:49:00
################################################################################


library(shiny)
library(mapboxer)
library(echarts4r)
library(dplyr)



# Config ------------------------------------------------------------------

MAPBOX_TOKEN <- "pk.eyJ1IjoiYWFyb242NjYiLCJhIjoiY2o2M2NmZmJ4MWc0ZDJxbnI3dmZ5OXB2MSJ9.T3trJ5Tu66Kw-w-1ELpzBA"



# Data --------------------------------------------------------------------

data_model <- readRDS("app/data/data-model.rds")
d_sf <- data_model$shape
d_commute <- data_model$d_commute

map_src <- as_mapbox_source(d_sf)



# Shiny app ---------------------------------------------------------------

shinyApp(
  ui = fluidPage(
    h1("mapboxer"),
    mapboxerOutput("map")
  ), 
  server = function(input, output, session) {
    output$map <- renderMapboxer({
      map_src %>% 
        mapboxer(
          style = "mapbox://styles/mapbox/dark-v9",
          token = MAPBOX_TOKEN,
          center = c(174.763336, -36.848461),
          zoom = 10
        ) %>% 
        # add_fill_layer(fill_color = "#eeeeee", fill_opacity = 0.01, id = "mb") %>% 
        add_fill_layer(fill_color = "rgba(1,1,1,0)", fill_outline_color = "rgba(255,255,255,0.5)", id = "mb") %>% 
        add_fill_layer(
          fill_color = c("get", "color"), fill_opacity = 0.5, id = "highlight", 
          source = as_mapbox_source(d_sf[1,] %>% mutate(color = "#000000"))
          # filter = list("==", "SA22018_V1", "")
        ) %>% 
        add_tooltips(layer_id = "mb", tooltip = "{{SA22018__1}}")
    })
    
    observeEvent(input$map_onclick, {
      print(input$map_onclick)
      
      highlighted <- 
        d_commute %>% 
        filter(commute_from_code == input$map_onclick$props$SA22018_V1) %>% 
        select(code = commute_to_code, commute_all)
      print(highlighted)
      
      # clicked <- d_sf %>% filter(SA22018_V1 == input$map_onclick$props$SA22018_V1)
      mapboxer_proxy("map") %>%
        set_data(
          data = d_sf[d_sf$SA22018_V1 %in% highlighted$code,] %>% 
            mutate(color = scales::col_bin("Blues", highlighted$commute_all)(highlighted$commute_all)),
          source_id = "highlight"
        ) %>% 
        # set_filter(layer_id = "highlight", list("==", "SA22018_V1", input$map_onclick$props$SA22018_V1)) %>% 
        update_mapboxer()
    })
  }
)

