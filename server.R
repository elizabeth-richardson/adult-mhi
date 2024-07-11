###############################################
#
# Define server for the Shiny app
#
##############################################

# Set password protection

credentials <- readRDS("admin/credentials.rds")


function(input, output, session) {
  
  # Shinymanager Auth
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  
  # Keeps the shiny app from timing out quickly 
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })

  
  # Source files with server code for each tab (new ScotPHO has modules instead)-----------------------------------------
  source(file.path("server/server_intro_page.R"), local = TRUE)$value # Homepage tab
  source(file.path("server/server_national_profile.R"), local = TRUE)$value # National profile tab
  source(file.path("server/server_local_profile.R"), local = TRUE)$value #Local profile tab (spine chart)
  source(file.path("server/server_trends.R"), local = TRUE)$value # differences by area
  source(file.path("server/server_inequalities.R"), local = TRUE)$value # inequalities by deprivation tab
  source(file.path("server/server_inequals_sex.R"), local = TRUE)$value # inequalities by sex
  source(file.path("server/server_data_sources.R"), local = TRUE)$value # indicator definitions tab
  
  
  #init reactive value storage
  rv = reactiveValues()
  
  #trigger event on tab selection change # from https://stackoverflow.com/questions/48584808/track-previous-tab-in-shiny-r
  observeEvent(input$intabset, {
    #store old current tab as last tab reactive value
    rv$last_tab = rv$current_tab
    #store new current tab as cur tab reactive value
    rv$current_tab = input$intabset
  })
  
  
  
  # stores selected 'areaname' and selected 'areatype' which can be used throughout other modules
  # to filter data by geography, like this: geo_selections()$areaname 
  geo_selections <- global_geography_filters_server("geo_filters", geo_lookup)
  
  
  # reactive values to store info on selected profile (will always be MHI here)
  profile_name <- reactiveVal() # to store full name (i.e. Health and Wellbeing)
  profile <- reactiveVal() # to store abbreviated name (i.e. HWB)
  
  
  # when a user switches tab, update the 2 x reactive values created above 
  observeEvent(input$nav, {
    profile(input$nav) # update the object called 'profile' with the nav id (i.e. HWB)
    profile_name(profiles_list[[input$nav]]) # update the object called 'profile_name' with the long version of the name (i.e. Health and wellbeing)
  })
  
  
  # dynamic header showing selected profile 
  output$profile_header <- renderUI({
    tags$h1("Profile:", profile_name(), class = "profile-header")
  })
  
  
  # dynamic header showing selected areatype
  output$areatype_header <- renderUI({
    
    # don't show this section in the header if Scotland is selected
    # to avoid having both areatype: scotland and areaname: scotland in the header
    shiny::validate(
      need(geo_selections()$areatype != "Scotland", "")
    )
    
    # if scotland is not selected then show the areatype in the header
    tags$h2("Area type:", geo_selections()$areatype, class = "geography-header")
  })
  
  
  # dynamic header showing selected areaname
  output$areaname_header <- renderUI({
    tags$h2("Area name:", geo_selections()$areaname, class = "geography-header")
  })
  
  
  
  
}


## END 