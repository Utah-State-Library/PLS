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


#### Source Helper Objects and Functions ####
source("R/lists.R", local = TRUE)
source("R/functions.R", local = TRUE)


#### Load Data ####

# Created in `./helper script/Process Data.R` - UPDATE
pls <- readRDS("data/pls_national.rds") %>%
  mutate(CNTY = str_to_title(CNTY), CITY = str_to_title(CITY)) %>%
  filter(hide_lib == 0)
variable_key <- read.csv("data/pls_variable_key.csv")
# librarykey <- readRDS("data/librarykey.rds")

pls_table <- pls %>%
  select(
    STABR,
    CURRENT_LIBNAME_DISAMB,
    FISCAL_YEAR,
    POPU_LSA,
    all_of(collection_cols),
    all_of(circulation_cols),
    all_of(expenses_cols),
    all_of(staffexpenses_cols),
    all_of(collectionexpenses_cols),
    all_of(revenue_cols),
    all_of(services_cols),
    all_of(internetaccess_cols),
    all_of(programming_cols),
    all_of(programAttend_cols)
  )


#### UI ####

ui <- page_navbar(
  title = "",
  navbar_options = navbar_options(
    underline = TRUE
  ),

  shiny::includeCSS("www/style.css"),

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

## nonresident fees model -> budget/POPU_LSA
