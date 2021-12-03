#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shiny.info::powered_by(company_name = "Fair Analytics", link = "https://www.fairanalytics.net/"),
    # Your application UI logic 
    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(title = "Corona Dashboard"),
      sidebar = shinydashboardPlus::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("COVID Statistics", tabName = "Attan_n_Corona", icon = icon("fas fa-virus")),
          shinydashboard::menuItem("COVID Vaccine", tabName = "vaccin_n_corona", icon = icon("fas fa-virus-slash")),
          uiOutput("covid_tamurt"),
          uiOutput("tamurt_province"),
          uiOutput("covid_frequency")
          )
        
    
      
      ),
      body = shinydashboard::dashboardBody(
        
        shinydashboard::tabItems(
          # First tab content
          shinydashboard::tabItem(tabName = "Attan_n_Corona",
                                  
                                  uiOutput("data_source"),
                                  fluidRow(
                                    
                                    column(width = 4,uiOutput("covid_tamurt_text")),
                                    column(width = 4,uiOutput("keep_it_empty")),
                                    column(width = 4,uiOutput("covid_last_update"))
                                    
                                  ),
                                  imageOutput(outputId = "tamurt_flag",width = "50%",
                                              height = "150px"),
   
                                  uiOutput("download_covid_data_ui"),
                    shinydashboardPlus::box(title = "Daily Cases",width = 12,
                                            fluidRow(
                                              column(width = 4,
                                                     uiOutput("covid_info_daily1")),
                                              column(width = 4,
                                                     uiOutput("covid_info_daily2")),
                                              column(width = 4,
                                                     uiOutput("covid_info_daily3"))
                                            ),
                                            plotly::plotlyOutput("covid_daily_charts")
                                            
                                            ),
                    shinydashboardPlus::box(title = "Total Cases",width = 12,
                                            fluidRow(
                                              column(width = 4,
                                                     uiOutput("covid_info1")),
                                              column(width = 4,
                                                     uiOutput("covid_info2")),
                                              column(width = 4,
                                                     uiOutput("covid_info3"))
                                            ),
                                            
                                            plotly::plotlyOutput("covid_total_charts")
                                        )# Box: totol 
                  
          ),# item : attan n corona
          shinydashboard::tabItem(tabName = "vaccin_n_corona",
                                  uiOutput("data_source2"),
                                  fluidRow(
                                    column(width = 4,uiOutput("covid_tamurt_text2")),
                                    column(width = 3,uiOutput("keep_it_empty2")),
                                    column(width = 3,uiOutput("covid_last_update2")),
                                    column(width = 2,uiOutput("tutlayt"))
                                    
                                  ),
                                  imageOutput(outputId = "tamurt_flag2",width = "50%",
                                              height = "150px"),
                                  uiOutput("download_covac_data_ui"),
                                  shinydashboardPlus::box(title = "Daily Cases",width = 12,
                                                          fluidRow(
                                                            column(width = 4,
                                                                   uiOutput("covac_info_daily1")),
                                                            column(width = 4,
                                                                   uiOutput("covac_info_daily2")),
                                                            column(width = 4,
                                                                   uiOutput("covac_info_daily3"))
                                                          ),
                                                          plotly::plotlyOutput("vaccine_daily_charts")
                                                          
                                  ),
                                  shinydashboardPlus::box(title = "Total Cases",width = 12,
                                                          fluidRow(
                                                            column(width = 4,
                                                                   uiOutput("covac_info1")),
                                                            column(width = 4,
                                                                   uiOutput("covac_info2")),
                                                            column(width = 4,
                                                                   uiOutput("covac_info3"))
                                                          ),
                                                          
                                                          plotly::plotlyOutput("vaccine_total_charts")
                                  )# Box: totol
                                  )
        )#tabItems
      ),#body
      skin = "green-light"
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CovidFairAnalytics'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

