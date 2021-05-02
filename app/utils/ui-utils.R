################################################################################
# Generic UI helper functions
#
# Author: Stefan Schliebs
# Created: 2019-02-21 09:40:56
################################################################################


# Material card -----------------------------------------------------------

material_card <- function(..., header = NULL, bgcolor = "#191a1a") {
  div(
    class = "card",
    header, 
    div(class = "card-content", ..., style = sprintf("background-color: %s", bgcolor))
  )
}



# Tile header -------------------------------------------------------------

tile_header <- function(left_ui, right_ui, left_ui_width = 7) {
  right_ui_width = 12 - left_ui_width
  fluidRow(
    column(width = left_ui_width, left_ui),
    if (right_ui_width > 0)
      column(width = right_ui_width, right_ui, style = "margin-top: -7px; padding-right: 7px;")
  )
}



# Simple info box ----------------------------------------------------------

simple_infobox <- function(value, title, color = "orange") {
  div(
    class = paste("simple-infobox-default", paste0("simple-infobox-", color)),
    div(value, class = "simple-infobox-value"),
    div(title, class = "simple-infobox-title")
  )
}



# Default reactable theme -------------------------------------------------

search_icon <- function(fill = "none") {
  # Icon from https://boxicons.com
  svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><path fill="%s" d="M10 18c1.85 0 3.54-.64 4.9-1.69l4.4 4.4 1.4-1.42-4.39-4.4A8 8 0 102 10a8 8 0 008 8.01zm0-14a6 6 0 11-.01 12.01A6 6 0 0110 4z"/></svg>', fill)
  sprintf("url('data:image/svg+xml;base64,%s')", jsonlite::base64_enc(svg))
}

options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "#191a1a",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)"),
  
  # Full-width search bar with search icon
  searchInputStyle = list(
    paddingLeft = "30px",
    paddingTop = "5px",
    paddingBottom = "5px",
    width = "100%",
    border = "none",
    backgroundColor = "#242424",
    backgroundImage = search_icon("hsl(0, 0%, 70%)"),
    backgroundSize = "16px",
    backgroundPosition = "left 8px center",
    backgroundRepeat = "no-repeat",
    "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
    "&:hover, &:focus" = list(backgroundImage = search_icon("hsl(0, 0%, 95%)")),
    "::placeholder" = list(color = "hsl(0, 0%, 55%)"),
    "&:hover::placeholder, &:focus::placeholder" = list(color = "hsl(0, 0%, 95%)")
  )
))
