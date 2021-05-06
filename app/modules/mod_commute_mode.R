################################################################################
# Module to encapsulate the mode of travel panel
#
# Author: Stefan Schliebs
# Created: 2021-04-08 10:04:50
################################################################################



# UI definitions ----------------------------------------------------------

mod_commute_mode_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tile_header(
      tagList(
        h4("Mode of Travel", class = "tile-headline"),
        uiOutput(ns("headline"))
      ),
      NULL,
      left_ui_width = 12
    ),
    echarts4rOutput(ns("chart_work_at_home"), height = "calc((100% - 50px) * 0.2 )"),
    echarts4rOutput(ns("chart_commute_car"), height = "calc((100% - 50px) * 0.2 )"),
    echarts4rOutput(ns("chart_commute_public"), height = "calc((100% - 50px) * 0.2 )"),
    echarts4rOutput(ns("chart_commute_walk_or_jog"), height = "calc((100% - 50px) * 0.2 )"),
    echarts4rOutput(ns("chart_commute_bicycle"), height = "calc((100% - 55px) * 0.2 )")
  )
}


# Server logic ------------------------------------------------------------

mod_commute_mode <- function(id, state) {
  server <- function(input, output, session) {
    
    ns <- session$ns
    

    # Chart data --------------------------------------------------------------

    d_commute <- reactive({
      req(state$region)
      D_COMMUTE %>% 
        filter(commute_from_region == state$region | commute_to_region == state$region)
    })
    
    # aggregated commute numbers by home or work SA  
    d_aggregated_area <- reactive({
      req(state$direction, d_commute())
      
      if (state$direction == "depart") {
        d <- 
          d_commute() %>%
          select(-starts_with("commute_from"), -starts_with("commute_to"), area = commute_from_code)
      } else {
        d <- 
          d_commute() %>%
          select(-starts_with("commute_from"), -starts_with("commute_to"), area = commute_to_code)
      }
      
      d %>%
        select(-commute_other, -commute_all, -commute_bus, -commute_train, -commute_ferry) %>%
        gather(var, value, -area) %>% 
        group_by(var, area) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>% 
        group_by(area) %>% 
        mutate(ratios = value / sum(value, na.rm = TRUE)) %>% 
        ungroup()
    })
    
    # histograms for all charts
    d_histogram <- reactive({
      req(d_aggregated_area())
      
      d_aggregated_area() %>% 
        group_by(var) %>% 
        nest() %>% 
        mutate(
          histogram = map(data, function(d) {
            if (all(is.na(d$ratios))) return(tibble(bin = c(0, 1), value = c(0, 0)))
            
            h <- hist(d$ratios, breaks = seq(0, 1, length.out = 21), plot = FALSE)
            tibble(bin = h$breaks[-1] - 0.025, value = h$counts)
          })
        ) %>% 
        select(-data) %>% 
        unnest(histogram) %>% 
        ungroup()
    })
    
    d_selected_area <- reactive({
      req(state$state)

      if (state$state$id == STATE_NOTHING_SELECTED) {
        return()
      } else if (state$state$id == STATE_MB_SELECTED) {
        req(d_aggregated_area())
        
        d <- 
          d_aggregated_area() %>% 
          filter(area == state$state$store$selected_mb) %>% 
          select(area, var, ratios) %>% 
          spread(var, ratios)

        if (nrow(d) == 0) d <- NULL
        
        return(d)
      } else if (state$state$id == STATE_BUCKET_SELECTED) {
        d <- list()
        d[[state$state$store$selected_mode]] <- state$state$store$selected_ratio
        return(d)
      }
    })
    
    
    # Headline ----------------------------------------------------------------

    output$headline <- renderUI({
      if (state$state$id == STATE_NOTHING_SELECTED) {
        h5(state$region, class = "tile-subheadline")
      } else if (state$state$id == STATE_MB_SELECTED) {
        area_name <- D_LOOKUP %>% filter(id == state$state$store$selected_mb) %>% pull(name)
        h5(area_name, class = "tile-subheadline")
      } else if (state$state$id == STATE_BUCKET_SELECTED) {
        h5(state$region, class = "tile-subheadline")
      }
    })
    

    # Chart helpers -----------------------------------------------------------

    histogram_tooltip_js <- function(mode_of_travel, from_to) {
      mot <- stringr::str_to_title(mode_of_travel)
      if (from_to == "depart") {
        direction = "From"
        destination = "Places of Residence"
      } else {
        direction = "Into"
        destination = "Places of Work"
      }

      htmlwidgets::JS(glue(
        "function(params, ticket, callback) {{",
        # "  console.log(ticket); console.log(params);",
        "  var ratio_from = Math.round(100 * (params[0].value[0] - 0.025));",
        "  var ratio_to = Math.round(ratio_from + 5);",
        "return('<strong>' + params[0].value[1] + '</strong> {destination}<br>where ' + '<strong>' + ratio_from + '%&ndash;' + ratio_to + '%</strong> {mot}');",
        "}}"
      ))
    }
    
    make_chart <- function(d, selected_val, bar_color, bar_highlight_color, chart_title) {
      
      if (!is.null(selected_val)) {
        chart_title <- paste0("", round(selected_val * 100), "% ", chart_title)
        p <- 
          d %>% 
          mutate(
            diff_to_selected = abs(bin - selected_val),
            selected_bin = diff_to_selected == min(diff_to_selected),
            selected_value = ifelse(selected_bin, value, 0)
          ) %>% 
          select(bin, value, selected_value) %>% 
          e_chart(bin, dispose = FALSE) %>% 
          e_bar(value, legend = FALSE, stack = "grp") %>% 
          e_bar(selected_value, legend = FALSE, stack = "grp") %>% 
          e_color(color = c(bar_color, bar_highlight_color))
      } else {
        p <- 
          d %>% 
          mutate(selected_value = 0) %>% 
          select(bin, value, selected_value) %>% 
          e_chart(bin, dispose = FALSE) %>% 
          e_bar(value, legend = FALSE, stack = "grp") %>% 
          e_bar(selected_value, legend = FALSE, stack = "grp") %>% 
          e_color(color = c(bar_color, bar_highlight_color))
      }
      
      p %>% 
        e_y_axis(show = FALSE) %>%
        e_x_axis(formatter = e_axis_formatter("percent"), min = 0, max = 1) %>%
        e_grid(top = "25px", left = "10px", right = "20px", bottom = "25px") %>% 
        e_title(subtext = chart_title, right = "1px", subtextStyle = list(fontFamily = "Roboto Condensed")) %>%
        e_tooltip(
          trigger = "axis",
          formatter = histogram_tooltip_js(chart_title, state$direction),
          confine = TRUE,
          textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)
        ) %>% 
        e_theme(ECHARTS_THEME)
    }
    

    # Charts ------------------------------------------------------------------
    
    bar_colors <- function(mode_of_travel) {
      bar_color <- COLOR_GREY
      bar_highlight_color <- COLOR_ORANGE
      
      if (state$state$id == STATE_BUCKET_SELECTED) {
        if (state$state$store$selected_mode == mode_of_travel) {
          bar_color <- COLOR_WHITE
          bar_highlight_color <- COLOR_ORANGE
        } else {
          bar_color <- COLOR_DARK_GREY
          bar_highlight_color <- COLOR_ORANGE
        }
      }
      
      list(bar_color = bar_color, bar_highlight_color = bar_highlight_color)
    }
    
    output$chart_work_at_home <- renderEcharts4r({
      req(d_histogram())

      cols <- bar_colors("work_at_home")
      d_histogram() %>% 
        filter(var == "work_at_home") %>% 
        make_chart(d_selected_area()$work_at_home, cols$bar_color, cols$bar_highlight_color, "Work At Home")
    })
    
    output$chart_commute_car <- renderEcharts4r({
      req(d_histogram())
      
      cols <- bar_colors("commute_car")
      d_histogram() %>% 
        filter(var == "commute_car") %>% 
        make_chart(d_selected_area()$commute_car, cols$bar_color, cols$bar_highlight_color, "Commute By Car")
    })
    
    output$chart_commute_public <- renderEcharts4r({
      req(d_histogram())
      
      cols <- bar_colors("commute_public")
      d_histogram() %>% 
        filter(var == "commute_public") %>% 
        make_chart(d_selected_area()$commute_public, cols$bar_color, cols$bar_highlight_color, "Commute By Public Transport")
    })
    
    output$chart_commute_walk_or_jog <- renderEcharts4r({
      req(d_histogram())

      cols <- bar_colors("commute_walk_or_jog")
      d_histogram() %>% 
        filter(var == "commute_walk_or_jog") %>% 
        make_chart(d_selected_area()$commute_walk_or_jog, cols$bar_color, cols$bar_highlight_color, "Walk Or Jog")
    })
  
    output$chart_commute_bicycle <- renderEcharts4r({
      req(d_histogram())
      
      cols <- bar_colors("commute_bicycle")
      d_histogram() %>% 
        filter(var == "commute_bicycle") %>% 
        make_chart(d_selected_area()$commute_bicycle, cols$bar_color, cols$bar_highlight_color, "Commute by Bike")
    })
    

    # Chart clicks ------------------------------------------------------------

    c("work_at_home", "commute_car", "commute_public", "commute_bicycle", "commute_walk_or_jog") %>% 
    lapply(function(mode_id) {
      observeEvent(input[[paste0("chart_", mode_id, "_clicked_data")]], {
        selected_ratio <- input[[paste0("chart_", mode_id, "_clicked_data")]]$value[1]
        upper_bound <- if (selected_ratio == 0.975) 1.025 else selected_ratio + 0.025
        d_areas <- 
          d_aggregated_area() %>%  
          filter(var == mode_id, ratios >= selected_ratio - 0.025, ratios < upper_bound) %>% 
          select(area)
        
        # change the app state
        state$state <- list(
          id = STATE_BUCKET_SELECTED, 
          store = list(selected_mode = mode_id, selected_ratio = selected_ratio, d_areas = d_areas)
        )
      })
    })
    
  }
  
  moduleServer(id, server)
}
