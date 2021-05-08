################################################################################
# UI of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-04 09:34:50
################################################################################


# Commute Explorer UI -----------------------------------------------------

commute_explorer_ui <- fluidRow(
  style = "margin: 0; height: 100%",
  column(
    width = 4,
    style = "height: 100%; padding-bottom: 65px",
    material_card(
      mod_commute_table_ui("table")
    )
  ),
  column(
    width = 6,
    style = "height: 100%; padding: 0 0 65px 0",
    mod_commute_map_ui("map"),
    mod_commute_filter_ui("filter")
  ),
  column(
    width = 2,
    style = "height: 100%; padding-bottom: 65px",
    material_card(
      mod_commute_mode_ui("mode")
    )
  )
)


# App template ------------------------------------------------------------

htmlTemplate(
  filename = "www/index.html",
  commute_explorer = commute_explorer_ui
)
