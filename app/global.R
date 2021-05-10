################################################################################
# Entrypoint of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-04 09:30:40
################################################################################


library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(echarts4r)
library(glue)
library(sf)
library(mapboxer)
library(reactable)
library(shinyWidgets)



# Config ------------------------------------------------------------------

MAPBOX_TOKEN <- "pk.eyJ1IjoiYWFyb242NjYiLCJhIjoiY2o2M2NmZmJ4MWc0ZDJxbnI3dmZ5OXB2MSJ9.T3trJ5Tu66Kw-w-1ELpzBA"
F_DATA_MODEL <- "data/data-model.rds"



# Colors and themes -------------------------------------------------------

ECHARTS_THEME <- "auritus"

COLOR_BLUE <- "#00a2eb"
COLOR_GREEN <- "#adb514"
COLOR_ORANGE <- "#fd9f02"
COLOR_PINK <- "#ce2c78"
COLOR_RED <- "#d32d05"
COLOR_PURPLE <- "#7522b8"
COLOR_GREY <- "#ffffffaa"
COLOR_DARK_GREY <- "#888888aa"
COLOR_WHITE <- "#eee"


# Data --------------------------------------------------------------------

# load data model
data_model <- readRDS(F_DATA_MODEL)

# extract model components
SF_SHAPE <- data_model$shape
D_COMMUTE <- data_model$d_commute
D_LOOKUP <- data_model$d_lookup
D_LOOKUP_REGION <- data_model$d_lookup_region
MAP_SRC <- as_mapbox_source(SF_SHAPE)



# App state ---------------------------------------------------------------

# We implement a state machine to capture the views of the app. The application 
# state is stored in a reactive list which is shared across all Shiny modules.

# app states
STATE_NOTHING_SELECTED <- 1
STATE_MB_SELECTED <- 2
STATE_BUCKET_SELECTED <- 3

# initial state values
INITIAL_DIRECTION <- "depart"
INITIAL_REGION <- "Auckland Region"
INITIAL_DATA_SOURCE <- "work"

# Utilities ---------------------------------------------------------------

source("utils/ui-utils.R")
source("utils/app-logic.R")



# Modules -----------------------------------------------------------------

source("modules/mod_commute_map.R")
source("modules/mod_commute_mode.R")
source("modules/mod_commute_table.R")
source("modules/mod_commute_filter.R")
