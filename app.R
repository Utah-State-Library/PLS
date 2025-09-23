library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(leaflet)
library(shinycssloaders)
library(reactable)
library(bslib)
library(bsicons)
library(shinyalert)
library(sjmisc)
library(htmlwidgets)
library(tidyverse)


#### Set Options ####
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


#### Load Data ####

# Created in `./helper script/Process Data.R`
pls <- readRDS("data/pls_national.rds") %>%
  mutate(CNTY = str_to_title(CNTY), CITY = str_to_title(CITY)) %>%
  filter(hide_lib == 0)
variable_key <- read.csv("data/pls_variable_key.csv")
# librarykey <- readRDS("data/librarykey.rds")

#### Input Lists ####

current_year <- max(as.numeric(pls$FISCAL_YEAR))

years <- pls %>%
  reframe(unique(FISCAL_YEAR)) %>%
  pull() %>%
  sort(decreasing = TRUE)

national_libnames <- pls %>%
  reframe(CURRENT_LIBNAME_DISAMB) %>%
  distinct() %>%
  arrange(CURRENT_LIBNAME_DISAMB) %>%
  pull()


utah_libnames <- pls %>%
  filter(STABR == "UT") %>%
  reframe(CURRENT_LIBNAME) %>%
  distinct() %>%
  arrange(CURRENT_LIBNAME) %>%
  pull()

current_FSCS <- pls %>%
  filter(FISCAL_YEAR == current_year) %>%
  reframe(FSCSKEY) %>%
  unique() %>%
  pull()

states <- pls %>%
  reframe(unique(STABR)) %>%
  pull() %>%
  sort()

#counties <- librarykey %>%
#  reframe(unique(COUNTY)) %>%
#  pull() %>%
#  sort()

#cities <- pls %>%
#  reframe(unique(CITY)) %>%
#  pull() %>% sort()

source("R/functions.R", local = T)


#### UI ####

ui <- page_navbar(
  title = "",
  navbar_options = navbar_options(
    underline = TRUE
  ),

  #source("RScripts/state_ui.R", local = TRUE)$value,
  #source("RScripts/single_library_ui.R", local = TRUE)$value,
  #source("RScripts/peer_groups_ui.R", local = TRUE)$value,
  source("R/tables_ui.R", local = TRUE)$value
)


#### Server ####
server <- function(input, output, session) {
  #source("RScripts/state_server.R", local = TRUE)$value
  #source("RScripts/single_library_server.R", local = TRUE)$value
  #source("RScripts/peer_groups_server.R", local = TRUE)$value
  source("R/tables_server.R", local = TRUE)$value
}


#### Run App ####
shinyApp(ui = ui, server = server)
