###############################################################################
#
# Global script ---- 
#
###############################################################################

# Setting up for new bslib format:

# 1. Required packages ----------------------------------------------------------
library(shiny) # for shiny functions
library(bslib) # app layout functions/theming
library(phsstyles) # for phs colour palette
library(shinyjs) # for various functions to expand/collapse geography filters 
library(htmltools) # for landing page template to read
library(purrr) # needed for sourcing modules with map
library(arrow) # for reading parquet files
library(reactable) # data tables
#library(highcharter) # visualisations
library(data.table) # faster data wrangling
library(dplyr) # data wrangling
library(htmlwidgets) # for download buttons
library(shinycssloaders) # for spinners when ui loading
library(jsonlite) # for download data in json format/reading in .json shapefiles
library(reactable) # for data tables
library(lubridate) # for dates
#library(leaflet) # for map
#library(jsTreeR) # for data tab geography filters
library(shinyWidgets)
library(bsicons) # for icons
library(openxlsx) # writing xlsx spreadsheets
#library(cicerone) #for guided tours of tabs

# # As well as webshot, phantomjs is needed to download Plotly charts
# # https://github.com/rstudio/shinyapps-package-dependencies/pull/180
# if (is.null(suppressMessages(webshot:::find_phantom()))) {
#   webshot::install_phantomjs()
# }


## 2. Sourcing modules and narrative text -------------------------------------------
# list.files("modules", full.names = TRUE, recursive = TRUE) |>
#   map(~ source(.))
# 
# list.files("narrative", full.names = TRUE, recursive = TRUE) |>
#   map(~ source(.))

# sourcing functions created for app (see functions folder) -------------------------------
list.files("functions") %>% 
  map(~ source(paste0("functions/", .)))




# 2. required datafiles ------------------------------------------------------------

# main datasets 
all_data <- setDT(arrow::read_parquet("data/all_data.parquet"))  # main dataset with indicator data
db_metadata <- setDT(arrow::read_parquet("data/metadata.parquet")) # deprivation/inequalities dataset
ineq_data <- setDT(arrow::read_parquet("data/ineq_data.parquet")) # metadata including indicator definitions

# # lookups 
# geo_lookup <- readRDS("data/geo_lookup.rds") # geography lookup
# profile_lookup <- readRDS("data/profile_lookup.rds") # profiles lookup
# 
# 
# # shapefiles (for map) 
# ca_bound<-readRDS("data/CA_boundary.rds") # Council area 
# hb_bound<-readRDS("data/HB_boundary.rds") # Health board
# also need police div map

# Identify the indicators with duplicate Scotland data: 1 series for plotting in isolation (single year values), and 1 for plotting when HB/CA data also plotted (nchar>4)
scotland_dups <- c("Suicide rate", # 1 from NRS data (formats = "2000" and "2000-2004")
                   "Housing condition", # 1 from SHCS data (formats = "2000" and "2013-2015")
                   "Common mental health problems", "Fruit and vegetable consumption", "Life satisfaction", # 8 from SHeS data (formats = "2000" and "2016-2019" or "2017-2021" (2020 missed))
                   "Long-standing physical conditions", "Mental wellbeing", "Physical activity", 
                   "Self-assessed general health", "Unpaid caring for others")

shes_scotland_dups <- c("Common mental health problems", "Fruit and vegetable consumption", "Life satisfaction", # 8 from SHeS data (formats = "2000" and "2016-2019" or "2017-2021" (2020 missed))
                        "Long-standing physical conditions", "Mental wellbeing", "Physical activity", 
                        "Self-assessed general health", "Unpaid caring for others")



#3. Objects, names, lists ------------------------------------------------------

# get today's date for naming downloads
mydate <- Sys.Date()

# identify when each indicator was last updated
last_update <- as.character(db_metadata %>% 
                              group_by(last_update) %>% 
                              summarise() %>% 
                              ungroup() %>%
                              filter(last_update!="NA") %>%
                              mutate(last_update_date = my(last_update)) %>% # convert to date
                              filter(last_update_date == max(last_update_date)) %>%
                              select(last_update))[1]

# LANDING PAGE ----

# List of sections in side tab
home_list <- c("Background" = "background",
               "The indicator set" = "indicators",
               "Using the dashboard" = "use",
               "Developing the indicator set" = "development",
               "Further information" = "info",
               "Glossary" = "glossary",
               "Accessibility" = "accessibility",
               "Contact" = "contact")


# FOR SELECTIONS ----

# Domain names (for filter applied on indicator definitions page)
domain_names <- setNames(c("Mental health outcomes",
                           "Individual determinants",
                           "Community determinants",
                           "Structural determinants"),
                         c("Mental health outcomes",
                           "Individual determinants",
                           "Community determinants",
                           "Structural determinants"))

domain_names_filter <- c(setNames("Show all", "Show all"), domain_names)

# Area types (= spatial.scale) (for filter applied on indicator definitions page)
area_types <- c("Scotland",
                "Health Boards",
                "Council Areas",
                #  "Police Regions",
                "Police Divisions")

area_types_filter <- c(setNames("Show all", "Show all"), area_types)


# Area names (= spatial.unit)
hb_names <- as.character(sort(unique(all_data$spatial.unit[all_data$spatial.scale=="HB"]))) 
la_names <- as.character(sort(unique(all_data$spatial.unit[all_data$spatial.scale=="LA"]))) 
pd_names <- as.character(sort(unique(all_data$spatial.unit[all_data$spatial.scale=="PD"]))) 

# Indicator names 
outcome_names <- sort(unique(db_metadata$ind_name[db_metadata$domain=="Mental health outcomes" & !is.na(db_metadata$ind_name)]))
#outcome_names_wrap <- str_replace_all(str_wrap(outcome_names, width = 32), "\\n", "<br>")
indiv_names <- sort(unique(db_metadata$ind_name[db_metadata$domain=="Individual determinants" & !is.na(db_metadata$ind_name)]))
comm_names <- sort(unique(db_metadata$ind_name[db_metadata$domain=="Community determinants" & !is.na(db_metadata$ind_name)]))
struc_names <- sort(unique(db_metadata$ind_name[db_metadata$domain=="Structural determinants" & !is.na(db_metadata$ind_name)]))

#identify the indicators with data by sex
inds_by_sex <- setDT(unique(all_data[all_data$sex=="Female"][, .(ind_name)])) 
# filter to those in each domain  
mhout_inds_sex <- sort(db_metadata$ind_name[db_metadata$domain == "Mental health outcomes" & db_metadata$ind_name %in% inds_by_sex$ind_name])
indiv_inds_sex <- sort(db_metadata$ind_name[db_metadata$domain == "Individual determinants" & db_metadata$ind_name %in% inds_by_sex$ind_name])
comm_inds_sex <- sort(db_metadata$ind_name[db_metadata$domain == "Community determinants" & db_metadata$ind_name %in% inds_by_sex$ind_name])
struc_inds_sex <- sort(db_metadata$ind_name[db_metadata$domain == "Structural determinants" & db_metadata$ind_name %in% inds_by_sex$ind_name])


#identify the indicators with data by SIMD
inds_by_simd <- setDT(unique(ineq_data[ineq_data$spatial.scale == "SIMD"][, .(ind_name)]))
# filter to those in each domain
mhout_inds_simd <- sort(db_metadata$ind_name[db_metadata$domain == "Mental health outcomes" & db_metadata$ind_name %in% inds_by_simd$ind_name])
indiv_inds_simd <- sort(db_metadata$ind_name[db_metadata$domain == "Individual determinants" & db_metadata$ind_name %in% inds_by_simd$ind_name])
comm_inds_simd <- sort(db_metadata$ind_name[db_metadata$domain == "Community determinants" & db_metadata$ind_name %in% inds_by_simd$ind_name])
struc_inds_simd <- sort(db_metadata$ind_name[db_metadata$domain == "Structural determinants" & db_metadata$ind_name %in% inds_by_simd$ind_name])




# 4. Dashboard theme ---------------------------------------------------------------

# see https://rstudio.github.io/bslib/articles/bs5-variables/ for list of all variables 
phs_theme <- bs_theme(version = 5, # bootstrap version 5
                      "nav-tabs-link-active-bg" = phs_colours(colourname = "phs-magenta"), # multi-tab cards colour when selected
                      "nav-tabs-link-active-color" = "white", # multi-tab cards font colour when selected
                      "form-label-font-weight" = "700") |> # filter labels font weight
  
  # adding custom styling for particular bits of ui (for instance making some bits of text purple without affecting all text)
  # note: could move over some stuff from css file into here i.e. for some of the landing page styling?
  bs_add_rules(
    list(
      ".geography-header { color: #9B4393; font-weight: 600 !important; }", # geography header light phs purple colour
      ".profile-header { color: #3F3685; font-weight: bold !important; }", # profile header darker phs purple colour
      ".btn-download_btns_menu { padding: 0}", # remove padding from download buttons menu so fits nicely in card footers
      ".chart-header { font-weight: bold !important;}", # make chart titles bold
      "strong { color: #9B4393 !important;}", # make the domain names purple for homepage
      ".btn-hero {color:black; background-color:#def4ff; border:none;}", # make buttons in the hero on landing page light blue
      ".info-box-header { background-color: #9B4393; color: #fff; font-size: 1.2em !important; }", # info box header lighter phs purple colour with white text
      ".metadata-header {font-weight: 600;}", # for indicator definitions tab - make headers in expandable rows bolder 
      ".rt-tr-details {padding: 24px; box-shadow: inset 0 1px 3px #dbdbdb; background: #FDFDFC ;}", # for indificator definitions tab - make expandable panel grey
      ".methodology-table th{border:thin solid black; background-color:purple; color:white; padding:3px; word-break: break-all;}", # for indicator def tab - make nested table headers purple
      ".methodology-table td{ border:thin solid black; padding:3px;}", # for indicator def tab - make nested table cells have black border
      ".shiny-output-error {color: white;}", # hiding auto-generated error messages
      ".shiny-output-error-validation {color: #8e8f90;}", # showing custom error messages
      ".info-box-header { background-color: #9B4393; color: #fff; font-size: 1.2em !important; }" # info box header lighter phs purple colour with white text
      
    )
  )

# phs colours for charts with dynamic number of lines/bars
phs_palette <- unname(unlist(phs_colours()))


# Set styles for openxlsx (spreadsheet download)  
general_style <- createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL) 
general_style_wrap <- createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL,  wrapText = TRUE) 
heading1_style <- createStyle(fontName = "Arial", fontSize = 16, fontColour = NULL, fgFill = NULL, halign = NULL, valign = NULL, textDecoration = "bold")
heading2_style <- createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold")
heading2_shade_style <- createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold", fgFill = "#D3D3D3")
heading2_shade_style_wrap <- createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold", fgFill = "#D3D3D3", wrapText = TRUE)
heading3_style <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold", fgFill = "#D3D3D3", border="TopBottom")
heading3_style_wrap <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold", fgFill = "#D3D3D3", border="TopBottom", wrapText = TRUE)
heading3_noshade_style <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold")
heading4_style <-createStyle(fontName = "Arial", fontSize = 11, fontColour = NULL)
border_style <- createStyle(border= c("top", "bottom", "left", "right") )
integers <- createStyle(numFmt = "#,##0")
dp0 <- createStyle(numFmt = "0")
dp1 <- createStyle(numFmt = "0.0")
dp2 <- createStyle(numFmt = "0.00")
dp3 <- createStyle(numFmt = "0.000")



# 5. extra UI components  ----------------------------------------------------------

# Tab tours -----------------------------------------------------------------

# guide_trend <- Cicerone$
#   new(
#     padding = 8
#   )$
#   step(
#     "trend_card_wrapper",
#     "Chart Tab",
#     "The trend chart is designed to explore how a single indicator has changed over time for one or more geograpical area.<br>
#     Use the mouse to hover over a data point to see detailed information on its value, time period and area.<br>
#     The tabs at the top of this panel switch between the chart, data and further information to aid interpretation.",
#     position = "left"
#   )$
#   step(
#     "trend_indicator_filter_wrapper", # id of div wrapper - specified in trend module rather than indicator filter module
#     "Indicator Filter",
#     "First select an indicator.<br>
#     The indicator list has been filtered based on profile and area type selected at the top of the page.<br>
#     The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
#     position = "bottom"
#   )$
#   step(
#     "trend_indicator_definition_wrapper",
#     "Indicator Definition Button",
#     "Click here for a more detailed definition of an selected indicator.",
#     position = "bottom"
#   )$
#   step(
#     "trend_geography_wrapper",
#     "Geography Filters",
#     "Add one or more geographical areas of any type to the chart to compare with your selected geography.<br>
#     There may be some indicators for which data is not available for the full time series or at a particular geography level.<br>
#      If an area type other than Scotland is selected in the global options, the Scotland checkbox can be clicked to add or remove the trend line.",
#     position = "right"
#   )$
#   step(
#     "trend_download_chart",
#     "Download Chart Button",
#     "Click here to download the chart with all selected geographies as a PNG.",
#     position = "bottom"
#   )$
#   step(
#     "trend_download_data",
#     "Download Data Button",
#     "Click here to download the selected data as a CSV, RDS or JSON file.",
#     position = "left"
#     #popovers help not working just yet - revist after merging of changes to popover design
#     # )$
#     # step(
#     #   "trend_popovers",
#     #   "Adjust Chart Settings",
#     #   "Click here to see chart settings. Confidence intervals (95%) can be added to the chart. They are shown as shaded areas and give an indication of the precision of a rate or percentage. The width of a confidence interval is related to sample size.
#     #   The chart can also be switched from a measure (e.g. rate or percentage) to actual numbers (e.g. the number of births with a healthy birthweight)."
#   )



# cookie box to appear along the top of dashboard
cookie_box <-
  div(
    class = "alert alert-info",
    style = "margin-bottom: 0; background-color: white; color:black",
    "This website places cookies on your device to help us improve our service
    to you. To find out more, see our ",
    tags$a(href = 'https://www.scotpho.org.uk/about-us/scotpho-website-policies-and-statements/privacy-and-cookies',
           " Privacy and Cookies"),
    "statement.",
    HTML(
      '<a href="#" class="close" data-dismiss="alert" aria-label="close">&check;</a>'
    )
  )


# # updates modal to appear when user clicks 'latest updates' button on homepage
# updates_modal <- modalDialog(
#   reactable(indicators_updated,
#             columns = list(
#               indicator_name = colDef(show = T, name = "Indicator"),
#               last_updated= colDef(show = T, name = "Last updated")
#             )),
#   size = "l", align= "left",
#   easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
# )









##END
