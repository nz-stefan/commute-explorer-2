################################################################################
# Logic helpers
#
# Author: Stefan Schliebs
# Created: 2021-04-07 09:43:23
################################################################################


agg_departure_arrival_summary <- function(d_commute, direction) {
  if (direction == "depart") {
    d_commute %>% 
      group_by(code = commute_from_code, mb = commute_from, region = commute_from_region) %>% 
      summarise(across(c(work_at_home, commute_car, commute_public, commute_walk_or_jog, commute_bicycle, commute_all), .fns = sum), .groups = "drop")
  } else {
    d_commute %>% 
      group_by(code = commute_to_code, mb = commute_to, region = commute_to_region) %>% 
      summarise(across(c(work_at_home, commute_car, commute_public, commute_walk_or_jog, commute_bicycle, commute_all), .fns = sum), .groups = "drop")
  }
}

extract_mb <- function(d_commute, mb, direction) {
  if (direction == "depart") {
    d_commute %>%
      filter(commute_from_code == mb) %>%
      select(
        code = commute_to_code, mb = commute_to, region = commute_to_region, 
        work_at_home, commute_car, commute_public, commute_walk_or_jog, commute_bicycle, commute_all
      )
  } else {
    d_commute %>%
      filter(commute_to_code == mb) %>%
      select(
        code = commute_from_code, mb = commute_from, region = commute_from_region, 
        work_at_home, commute_car, commute_public, commute_walk_or_jog, commute_bicycle, commute_all
      )
  }
}
