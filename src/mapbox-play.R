################################################################################
# Mapbox play
#
# Author: Stefan Schliebs
# Created: 2021-04-01 08:31:51
################################################################################

library(echarts4r)

MAPBOX_TOKEN <- "pk.eyJ1IjoiYWFyb242NjYiLCJhIjoiY2o2M2NmZmJ4MWc0ZDJxbnI3dmZ5OXB2MSJ9.T3trJ5Tu66Kw-w-1ELpzBA"


d <- tibble::tibble(
  lat = c(120.30327558517455, 120.40327558517455), 
  lon = c(31.55755415024492, 31.75755415024492),
  value = c(10000, 100)
)

d %>% 
  e_charts(lon) %>% 
  e_mapbox(
    token = MAPBOX_TOKEN,
    style = "mapbox://styles/mapbox/dark-v9",
    center = c(120.30327558517455, 31.55755415024492), 
    zoom = 14
  ) %>% 
  e_bar_3d(lat, value, coord_system = "mapbox") # %>% 
  # e_inspect(json = TRUE, pretty = TRUE)



# mapboxer ----------------------------------------------------------------

library(mapboxer)

d_sf <- readRDS("app/data/data-model.rds")$shape

d_sf %>% 
  as_mapbox_source() %>%
  mapboxer(
    center = c(174.763336, -36.848461),
    zoom = 10
  ) %>% 
  add_line_layer(line_color = "#eeeeee", line_opacity = 0.2)



# Mapboxer shiny ----------------------------------------------------------


library(shiny)
library(mapboxer)

d_sf <- readRDS("app/data/data-model.rds")$shape

map_src <- as_mapbox_source(d_sf)

ui <- fluidPage(
  h1("mapboxer"),
  mapboxerOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderMapboxer({
    map_src %>% 
      mapboxer(
        style = "mapbox://styles/mapbox/dark-v9",
        token = MAPBOX_TOKEN,
        center = c(174.763336, -36.848461),
        zoom = 10
      ) %>% 
      add_line_layer(line_color = "#eeeeee", line_opacity = 0.25) %>% 
      add_fill_layer(fill_color = "#eeeeee", fill_opacity = 0.1, id = "mb")
  })
  
  observeEvent(input$map_onclick, {
    print(input$map_onclick)

    # clicked <- d_sf %>% filter(SA22018_V1 == input$map_onclick$props$SA22018_V1)
    mapboxer_proxy("map") %>%
      set_filter(layer_id = "mb", list("==", "SA22018_V1", input$map_onclick$props$SA22018_V1)) %>% 
      set_layout_property(layer_id = "mb", "fill_color", "#ff0000") %>% 
      # add_fill_layer(fill_color = "#ff0000", source = as_mapbox_source(clicked), fill_opacity = 1) %>% 
      update_mapboxer()
  })
}

shinyApp(ui, server)
