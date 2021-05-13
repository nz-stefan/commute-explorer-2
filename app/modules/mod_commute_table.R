################################################################################
# Module to encapsulate the commuter summary table
#
# Author: Stefan Schliebs
# Created: 2021-04-08 09:55:19
################################################################################


mod_commute_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      style = "margin: 0px -12px 0 -12px",
      column(
        width = 8,
        prettyRadioButtons(
          inputId = ns("direction"),
          choices = c(FROM = "depart", TO = "arrive"),
          selected = INITIAL_DIRECTION,
          label = "Toggle Percent View",
          width = "auto",
          animation = "jelly",
          inline = TRUE
        )
      ),
      column(
        width = 4,
        style = "margin: -7px 0 0 0; font-size: 14px;",
        div(
          style = "float:right",
          pickerInput(
            ns("data_source"), 
            label = NULL, 
            choices = c(All = "all", Work = "work", Education = "edu"), 
            multiple  = FALSE, selected = INITIAL_DATA_SOURCE,
            width = "100px"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        style = "    margin: -5px 0px 5px 0",
        selectizeInput(
          ns("area"), 
          label = NULL, 
          choices = "", multiple = FALSE, selected = character(0),
          width = "100%", options = list(allowEmptyOption = FALSE, placeholder = "SEARCH..."))
      )
    ),
    tile_header(
      uiOutput(ns("headline")),
      div(
        style = "margin-right: -10px; text-align: right",
        actionButton(ns("center_view"), label = " ", icon = icon("eye-open", lib = "glyphicon"), title = "Center on Area"),
        prettyCheckbox(
          ns("percent"),
          label = "%",
          value = FALSE,
          inline = TRUE,
          outline = TRUE, shape = "curve", bigger = TRUE
        )
      ), 
      left_ui_width = 8
    ),
    div(
      style = "margin: 5px -5px 0 -5px; height: calc(100% - 130px)", 
      reactableOutput(ns("table"), height = "100%")
    )
  )
}



# Server logic ------------------------------------------------------------

mod_commute_table <- function(id, state) {
  server <- function(input, output, session) {
    ns <- session$ns


    # Data selection ----------------------------------------------------------

    observeEvent(input$data_source, {
      state$data_source <- input$data_source
    })
    

    # Area selection ----------------------------------------------------------
    
    observeEvent(input$direction, {
      state$direction <- input$direction
    })
    
    # reactive to store choices of the area input field
    area_choices <- reactive({
      req(state$region)
      
      if (state$region == "All Regions") {
        d <- D_LOOKUP_REGION %>% arrange(name)
      } else {
        d <- D_LOOKUP_REGION %>% filter(region == state$region) %>% arrange(name)
      }
      
      setNames(d$code, d$name)
    })

    # populate the selectizeInput choices
    observe({
      req(area_choices())
      current_selected <- isolate(input$area)
      updateSelectizeInput(session, "area", choices = area_choices(), selected = current_selected, server = TRUE)
    })

    # To change the app state based on the area input we need to respond to two events:
    #   1. A new area was selected -> change app state to STATE_MB_SELECTED
    #   2. The area input field was deleted -> change app state to STATE_NOTHING_SELECTED

    # change the app state if a new area was selected through selectizeInput (Option 1 above)
    observeEvent(input$area, {
      req(input$area)
      print(paste("observeEvent input$area", input$area))
      
      # area was selected -> switch to STATE_MB_SELECTED
      state$state <- list(
        id = STATE_MB_SELECTED, 
        store = list(selected_mb = input$area, event_source = "search")
      )
    })
    
    # change the app state if the area input is empty (Option 2 above)
    observeEvent(input$area, {
      # we only respond to events that originated through the area input field
      # events from other sources, e.g. map clicks, are ignored, otherwise we cause reactive loops
      req(input$area == "", state$state$id != STATE_NOTHING_SELECTED, state$state$store$event_source == "search")
      print(paste("observeEvent:", input$area))

      # no area selected -> switch to STATE_NOTHING_SELECTED
      state$state <- list(
        id = STATE_NOTHING_SELECTED,
        store = list(event_source = "search")
      )
    })
    
    # Update the area input field if the app state changes through other sources (e.g. map clicks)
    observeEvent(state$state, {
      # we only listen to app state events that did not originate from the input field
      # otherwise we cause reactive loops
      req(state$state$store$event_source != "search")
      
      if (state$state$id == STATE_MB_SELECTED) {
        selected <- state$state$store$selected_mb
      } else if (state$state$id %in% c(STATE_NOTHING_SELECTED, STATE_BUCKET_SELECTED)) {
        selected <- character(0)
      }
      
      print(paste("observeEvent state$state:", selected))
      updateSelectizeInput(session, "area", choices = area_choices(), selected = selected, server = TRUE)
    })
    
    
    # Center view button ------------------------------------------------------

    observeEvent(input$center_view, {
      req(state$map_id)
      
      if (state$state$id == STATE_MB_SELECTED) {
        bbox <- SF_SHAPE %>% 
          filter(SA22018_V1 == state$state$store$selected_mb) %>% 
          sf::st_bbox()
        mapboxer_proxy(state$map_id) %>% fit_bounds(bbox, maxZoom = 12) %>% update_mapboxer()
      }
      
    })
    

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
            h4("Destinations", class = "tile-headline"),
            h5(paste("From", area_name), class = "tile-subheadline")
          )
        } else {
          tagList(
            h4("Origins", class = "tile-headline"),
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
      req(state$d_commute, state$state, state$direction)
      
      if (state$state$id == STATE_NOTHING_SELECTED) {
        agg_departure_arrival_summary(state$d_commute, state$direction)
      } else if (state$state$id == STATE_MB_SELECTED) {
        # identify the IDs of shapes to highlight
        extract_mb(state$d_commute, state$state$store$selected_mb, state$direction)
      } else if (state$state$id == STATE_BUCKET_SELECTED) {
        state$d_commute %>% 
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
      req(d_table(), state$window_height)

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
        defaultPageSize = round((state$window_height - 345) / 31),
        paginationType = "simple",
        # searchable = TRUE,
        wrap = FALSE,
        onClick = onclick_js,
        defaultSorted = list(commute_all = "desc"),
        
        columns = list(
          code = colDef(show = FALSE),
          region = colDef(show = FALSE),
          # mb = colDef(header = " ", footer = if (! input$percent) "Total", minWidth = 100, style = list(cursor = "pointer", textDecoration = "underline", textDecorationStyle = "dotted")),
          mb = colDef(header = " ", footer = if (! input$percent) "Total", minWidth = 100, class = "area-link"),
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
        store = list(selected_mb = selected_row$code, event_source = "table")
      )
    })
    
  }
  
  moduleServer(id, server)
}
