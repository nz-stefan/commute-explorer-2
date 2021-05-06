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
      uiOutput(ns("headline")),
      div(
        style = "margin-right: -10px; text-align: right",
        prettyCheckbox(
          ns("percent"),
          label = "%",
          value = FALSE,
          inline = TRUE,
          outline = TRUE, shape = "curve", bigger = TRUE
        )
      ), 
      left_ui_width = 10
    ),
    div(
      style = "margin: 5px -5px 0 -5px; height: calc(100% - 30px)", 
      reactableOutput(ns("table"), height = "100%")
    )
  )
}



# Server logic ------------------------------------------------------------

mod_commute_table <- function(id, state) {
  server <- function(input, output, session) {
    ns <- session$ns


    # Headline ----------------------------------------------------------------

    output$headline <- renderUI({
      req(state$direction)
      
      if (state$state$id == STATE_NOTHING_SELECTED) {
        headline <- if (state$direction == "depart") "All Departures" else "All Arrivals" 
          
        tagList(
          h4(headline, class = "tile-headline"),
          h5(state$region, class = "tile-subheadline")
        )
      } else if (state$state$id == STATE_MB_SELECTED) {
        area_name <- D_LOOKUP %>% filter(id == state$state$store$selected_mb) %>% pull(name)
        if (state$direction == "depart") {
          tagList(
            h4("Departures", class = "tile-headline"),
            h5(paste("From", area_name), class = "tile-subheadline")
          )
        } else {
          tagList(
            h4("Arrivals", class = "tile-headline"),
            h5(paste("Into", area_name), class = "tile-subheadline")
          )
        }
      } else if (state$state$id == STATE_BUCKET_SELECTED) {
        headline <- if (state$direction == "depart") "All Departures" else "All Arrivals" 
        
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
        
        tagList(
          h4(headline, class = "tile-headline"),
          h5(HTML(paste("Where", ratio_label, mode)), class = "tile-subheadline")
        )
      }
    })
    

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
    
    footer <- function(values) format(sum(values, na.rm = TRUE), big.mark = ",")
    
    output$table <- renderReactable({
      req(state$window_height)

      if (input$percent) {
        d <- d_table() %>% 
          mutate(across(c(where(is.double), -commute_all, -work_at_home), ~.x / commute_all)) %>% 
          mutate(across(c(commute_all, work_at_home), ~.x / sum(.x)))
          # mutate(commute_all = commute_all / sum(commute_all))
        format <- colFormat(percent = TRUE, digits = 0)
        footer <- NULL
      } else {
        d <- d_table()
        format <- NULL
      }
      
      reactable(
        d,
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
          mb = colDef(header = " ", footer = if (! input$percent) "Total", minWidth = 100, style = list(cursor = "pointer", textDecoration = "underline", textDecorationStyle = "dotted")),
          work_at_home = colDef(header = icon("home"), format = format, footer = footer, style = list(borderRight = "1px solid hsl(233, 9%, 22%)")),
          commute_car = colDef(header = icon("car"), footer = footer, format = format),
          commute_public = colDef(header = icon("train"), footer = footer, format = format),
          commute_walk_or_jog = colDef(header = icon("walking"), footer = footer, format = format),
          commute_bicycle = colDef(header = icon("bicycle"), footer = footer, format = format),
          commute_all = colDef(header = HTML("<strong>&Sigma;</strong>"), footer = footer, format = format, html = TRUE, style = list(borderLeft = "1px solid hsl(233, 9%, 22%)"))
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
