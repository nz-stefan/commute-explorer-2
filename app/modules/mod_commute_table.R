################################################################################
# Module to encapsulate the commuter summary table
#
# Author: Stefan Schliebs
# Created: 2021-04-08 09:55:19
################################################################################


mod_commute_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tile_header(
      h4("Table", class = "tile-headline"), NULL, left_ui_width = 12
    ),
    div(style = "height: calc(100% - 20px)", reactableOutput(ns("table"), height = "100%"))
  )
}



# Server logic ------------------------------------------------------------

mod_commute_table <- function(id, state) {
  server <- function(input, output, session) {
    ns <- session$ns


    # Table data --------------------------------------------------------------
    
    d_table <- reactive({
      req(state$state, state$direction)
      
      if (state$state$id == STATE_NOTHING_SELECTED) {
        agg_departure_arrival_summary(state$direction)
      } else if (state$state$id == STATE_MB_SELECTED) {
        # identify the IDs of shapes to highlight
        extract_mb(state$state$store$selected_mb, state$direction)
      } else if (state$state$id == STATE_BUCKET_SELECTED) {
        agg_departure_arrival_summary(state$direction) %>% 
          inner_join(state$state$store$d_areas, by = c("code" = "area"))
      }
    })
    

    # Render table ------------------------------------------------------------
    
    onclick_js <- JS(
      glue(
        "function(rowInfo, colInfo) {
          // Only handle click events on the 'mb' column
          if (colInfo.id !== 'mb') {
            return
          }
      
          // Send the click event to Shiny, which will be available in input$show_details
          // Note that the row index starts at 0 in JavaScript, so we add 1
          if (window.Shiny) {
            Shiny.setInputValue('<<< ns('show_details') >>>', { index: rowInfo.index + 1, rnd: Math.random() })
          }
        }", .open = "<<<", .close = ">>>")
    )
    
    footer <- function(values) format(sum(values), big.mark = ",")
    
    output$table <- renderReactable({
      req(state$window_height)
      
      reactable(
        d_table(), 
        compact = TRUE, 
        defaultColDef = colDef(minWidth = 30, footerStyle = "font-weight: bold"),
        highlight = TRUE,
        defaultPageSize = round((state$window_height - 265) / 31),
        paginationType = "simple",
        searchable = TRUE,
        wrap = FALSE,
        onClick = onclick_js,
        defaultSorted = list(commute_all = "desc"),
        
        columns = list(
          code = colDef(show = FALSE),
          region = colDef(show = FALSE),
          mb = colDef(header = " ", footer = "Total", minWidth = 100, style = list(cursor = "pointer", textDecoration = "underline", textDecorationStyle = "dotted")),
          work_at_home = colDef(header = icon("home"), footer = footer, style = list(borderRight = "1px solid hsl(233, 9%, 22%)")),
          commute_car = colDef(header = icon("car"), footer = footer),
          commute_public = colDef(header = icon("train"), footer = footer),
          commute_walk_or_jog = colDef(header = icon("walking"), footer = footer),
          commute_bicycle = colDef(header = icon("bicycle"), footer = footer),
          commute_all = colDef(header = HTML("<strong>&Sigma;</strong>"), footer = footer, html = TRUE, style = list(borderLeft = "1px solid hsl(233, 9%, 22%)"))
        ),
        language = reactableLang(
          searchPlaceholder = "Filter suburbs",
          noData = "No data found",
          pageInfo = "{rowStart}\u2013{rowEnd} of {rows} suburbs",
          pagePrevious = "\u276e",
          pageNext = "\u276f",
        )
      )
    })
    
    observeEvent(input$show_details, {
      req(input$show_details)

      selected_row <- d_table()[input$show_details$index,]
      
      # change the app state
      state$state <- list(
        id = STATE_MB_SELECTED, 
        store = list(selected_mb = selected_row$code)
      )
    })
    
  }
  
  moduleServer(id, server)
}
