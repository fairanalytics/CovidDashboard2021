#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  library("dplyr")
  # calling the translator sent as a golem option
  #i18n <- golem::get_golem_options(which = "translator")
  #i18n$set_translation_language("en")
  
  output$data_source <- output$data_source2 <- renderUI({
    url <- a("Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)", href="https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases")
    tagList("Data Source:", url)
  })
  
  output$tutlayt <- renderUI({
    # select language
    radioButtons(
      inputId = "tutlayt",
      label = "Select language",
      inline = TRUE,
      choices = i18n$get_languages()
    )
  })
  # Your application server logic 
  covid_tisefka <- reactive({
    covid_tisefka_file <- paste0("covid_tisefka_assa_",format(Sys.Date(),"%Y_%m_%d"),".csv")
    if(file.exists(covid_tisefka_file)){
      covid_data <- readr::read_csv(covid_tisefka_file)
    }else{
      covid_data <- covid_import_data()%>%purrr::imap(~covid_data_prep(.x,.y))%>%dplyr::bind_rows()
      readr::write_csv(covid_data,covid_tisefka_file)
    }
    return(covid_data)
  })
  
  
  output$covid_tamurt <- renderUI({
    req(covid_tisefka())
    timura <- unique(covid_tisefka()$country)%>%paste0()
    selectInput(inputId = "covid_tamurt",label = "Country",choices = timura,selected = "Germany")
  })
  
  
  
  tamurt_provinces <- reactive({
    req(covid_tisefka())
    req(input$covid_tamurt)
    tamurt_provinces <- covid_tisefka()%>%
      dplyr::filter(country == !!input$covid_tamurt)%>%
      dplyr::pull(province )%>%na.omit()%>%unique()
    if(length(tamurt_provinces)==0)return(input$covid_tamurt)
    
    tamurt_provinces <- c(input$covid_tamurt,tamurt_provinces)
    
  })
  
  output$tamurt_province <- renderUI({
    req(tamurt_provinces())
    if(length(tamurt_provinces())>1){
      selectInput(inputId = "tamurt_province",label = "Province",choices = tamurt_provinces())  
    }
  })
  
  output$covid_frequency <- renderUI({
    req(covid_tisefka())
    covid_freqs <- c("daily","weekly","monthly")
    selectInput(inputId = "covid_frequency",label = "Frequency",choices = covid_freqs)
  })
  output$covid_last_update  <- output$covid_last_update2 <- renderUI({
    req(input$covid_tamurt)
    h3(paste0("Date : ", Sys.Date()))
  })
  output$covid_tamurt_text <- output$covid_tamurt_text2 <- renderUI({
    req(input$covid_tamurt)
    h2(input$covid_tamurt)
  })
  output$tamurt_flag <- output$tamurt_flag2 <- renderImage({
    req(input$covid_tamurt)
    flag_file <- paste0("flag/flag-of-",input$covid_tamurt,".png")
    img_path <- system.file(flag_file, package = "CovidFairAnalytics")
    
    
    outfile <- tempfile(fileext = ".png")
    file.copy(img_path,outfile,overwrite = TRUE)
   
    # Return a list containing information about the image
    list(src = outfile,
         contentType = "image/png",
         width = "40%",
         height = "70%",
         alt = "Flag not found")
  }, deleteFile = TRUE)
  
   
  covid_tisefka_aggregated <- reactive({
    req(covid_tisefka())
    req(input$covid_tamurt)
    req(input$covid_frequency)
    req(tamurt_provinces())
    
    if(length(tamurt_provinces())>1){
      req(input$tamurt_province)
      if(input$covid_tamurt != input$tamurt_province ){
        covid_data <- covid_tisefka()%>%
          dplyr::filter(province ==!!input$tamurt_province)  
      }else{
        covid_data <- covid_tisefka()%>%
          dplyr::filter(is.na(province))
      }
      
    }else{
      covid_data <- covid_tisefka()
    }
    covid_data <- covid_data%>%dplyr::filter(country == !!input$covid_tamurt)%>%
      timetk::tk_augment_timeseries_signature()%>%dplyr::select(province,country,Lat,Long,date,cases,statistic,daily,year,month.lbl,week.iso)
    if(input$covid_frequency == "monthly"){
      covid_data <- covid_data%>%dplyr::group_by(country,Lat,Long,statistic,year,month.lbl)%>%
        dplyr::summarise(date = head(date,1),cases = tail(cases,1),monthly = sum(daily))%>%
        dplyr::arrange(date)
    }else if(input$covid_frequency == "weekly"){
      covid_data <- covid_data%>%dplyr::group_by(country,Lat,Long,statistic,year,week.iso)%>%
        dplyr::summarise(date = tail(date,1),cases = tail(cases,1),weekly = sum(daily))%>%
        dplyr::arrange(date)
    }
    return(covid_data)
  })
  
  
  output$covid_info_daily1 <- renderUI({
    req(covid_tisefka_aggregated())
    info_values <- covid_tisefka_aggregated()%>%dplyr::filter(statistic == "confirmed")%>%tail(1)%>%dplyr::pull(!!input$covid_frequency)
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    shinydashboard::valueBox(subtitle = "Confirmed", width = 12, value = info_values,color ="orange" ,icon = icon("fas fa-virus") )
  })
  output$covid_info_daily2 <- renderUI({
    req(covid_tisefka_aggregated())
    info_values <- covid_tisefka_aggregated()%>%dplyr::filter(statistic == "recovered")%>%tail(1)%>%dplyr::pull(!!input$covid_frequency)
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    shinydashboard::valueBox(subtitle = "Recovered", width = 12, value = info_values,color ="green" , icon =icon("fas fa-heartbeat") )
  })
  output$covid_info_daily3 <- renderUI({
    req(covid_tisefka_aggregated())
    info_values <- covid_tisefka_aggregated()%>%dplyr::filter(statistic == "deaths")%>%tail(1)%>%dplyr::pull(!!input$covid_frequency)
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    
    shinydashboard::valueBox(subtitle = "Deaths", width = 12,value = info_values,color ="red" ,icon = icon("fas fa-bible"))
  })
  
  output$covid_info1 <- renderUI({
    req(covid_tisefka_aggregated())
    info_values <- covid_tisefka_aggregated()%>%dplyr::filter(statistic == "confirmed")%>%tail(1)%>%dplyr::pull(cases)
    
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    shinydashboard::valueBox(subtitle = "Confirmed", width = 12, value = info_values,color ="orange" ,icon = icon("fas fa-virus") )
  })
  output$covid_info2 <- renderUI({
    req(covid_tisefka_aggregated())
    info_values <- covid_tisefka_aggregated()%>%dplyr::filter(statistic == "recovered")%>%tail(1)%>%dplyr::pull(cases)
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    
    shinydashboard::valueBox(subtitle = "Recovered", width = 12, value = info_values,color ="green" , icon =icon("fas fa-heartbeat") )
  })
  output$covid_info3 <- renderUI({
    req(covid_tisefka_aggregated())
    info_values <- covid_tisefka_aggregated()%>%dplyr::filter(statistic == "deaths")%>%tail(1)%>%dplyr::pull(cases)
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    shinydashboard::valueBox(subtitle = "Deaths", width = 12,value = info_values,color ="red" ,icon = icon("fas fa-bible"))
  })
  
  output$covid_daily_charts <- plotly::renderPlotly({
    req(covid_tisefka_aggregated())
    req(input$covid_tamurt)
    covid_stat <- input$covid_frequency
    covid_charts <- list()
    covid_charts$conv_rec <- generate_covid_plot(covid_data = covid_tisefka_aggregated(),tamurt = input$covid_tamurt,acu = c("confirmed","recovered"),covid_stat = covid_stat)
    covid_charts$deaths <- generate_covid_plot(covid_data= covid_tisefka_aggregated(),tamurt = input$covid_tamurt,acu = c("deaths"),covid_stat = covid_stat)
    covid_main_chart <- covid_charts%>%plotly::subplot(shareY = FALSE,nrows = 2,shareX = TRUE)
    return(covid_main_chart)
  })
  
  output$covid_total_charts <- plotly::renderPlotly({
    req(covid_tisefka_aggregated())
    req(input$covid_tamurt)
    covid_stat   <- "cases" # cases
    covid_charts <- list()
    covid_charts$conv_rec <- generate_covid_plot(covid_data = covid_tisefka_aggregated(),tamurt = input$covid_tamurt,acu = c("confirmed","recovered"),covid_stat = covid_stat)
    covid_charts$deaths <- generate_covid_plot(covid_data= covid_tisefka_aggregated(),tamurt = input$covid_tamurt,acu = c("deaths"),covid_stat = covid_stat)
    covid_main_chart <- covid_charts%>%plotly::subplot(shareY = FALSE,nrows = 2,shareX = TRUE)
    return(covid_main_chart)
  })

  output$download_covid_data_ui <- renderUI({
    req(covid_tisefka_aggregated())
    downloadButton(outputId = "download_covid_data", label = "Download Covid Data")
  })
  output$download_covid_data <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      readr::write_csv(covid_tisefka_aggregated(), con)
    }
  )
  # COVID Vaccine panel
  
  covid_vaccin_tisefka <- reactive({
    req(input$covid_tamurt)
    vaccine_data <- vaccine_import_data()%>%dplyr::filter(country == !!input$covid_tamurt)
    return(vaccine_data)
  })
  
  output$covac_info_daily1 <- renderUI({
    req(covid_vaccin_tisefka())
    aa <<-covid_vaccin_tisefka()
    info_values <- covid_vaccin_tisefka()%>%dplyr::filter(statistic == "total_vaccinations")%>%tail(1)%>%dplyr::pull(!!input$covid_frequency)
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    shinydashboard::valueBox(subtitle = "total_vaccinations", width = 12, value = info_values,color ="navy" ,icon = icon("fas fa-virus") )
  })
  
  output$covac_info_daily2 <- renderUI({
    req(covid_vaccin_tisefka())
    info_values <- covid_vaccin_tisefka()%>%dplyr::filter(statistic == "people_vaccinated")%>%tail(1)%>%dplyr::pull(!!input$covid_frequency)
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    
    shinydashboard::valueBox(subtitle = "People Vaccinated", width = 12,value = info_values,color ="light-blue" ,icon = icon("fas fa-syringe"))
  })
  output$covac_info_daily3 <- renderUI({
    req(covid_vaccin_tisefka())
    info_values <- covid_vaccin_tisefka()%>%dplyr::filter(statistic == "people_fully_vaccinated")%>%tail(1)%>%dplyr::pull(!!input$covid_frequency)
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    shinydashboard::valueBox(subtitle = "People Fully Vaccinated", width = 12, value = info_values,color ="green" , icon =icon("fas fa-heartbeat") )
  })
  output$covac_info1 <- renderUI({
    req(covid_vaccin_tisefka())
    info_values <- covid_vaccin_tisefka()%>%dplyr::filter(statistic == "total_vaccinations")%>%tail(1)%>%dplyr::pull(total)
    
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    shinydashboard::valueBox(subtitle = "Total Vaccinations", width = 12, value = info_values,color ="navy" ,icon = icon("fas fa-virus") )
  })
  
  output$covac_info2 <- renderUI({
    req(covid_vaccin_tisefka())
    info_values <- covid_vaccin_tisefka()%>%dplyr::filter(statistic == "people_vaccinated")%>%tail(1)%>%dplyr::pull(total)
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    shinydashboard::valueBox(subtitle = "People Vaccinated", width = 12,value = info_values,color ="light-blue" ,icon = icon("fas fa-syringe"))
  })
  output$covac_info3 <- renderUI({
    req(covid_vaccin_tisefka())
    info_values <- covid_vaccin_tisefka()%>%dplyr::filter(statistic == "people_fully_vaccinated")%>%tail(1)%>%dplyr::pull(total)
    info_values <- formatC(as.numeric(info_values), format="f", digits=0, big.mark=",")
    
    shinydashboard::valueBox(subtitle = "People Fully Vaccinated", width = 12, value = info_values,color ="green" , icon =icon("fas fa-heartbeat") )
  })
  
  
  output$vaccine_daily_charts <- plotly::renderPlotly({
    req(covid_vaccin_tisefka())
    req(input$covid_tamurt)
    covid_stat   <- "daily" # vaccine
    target_stats <- c("total_vaccinations","people_fully_vaccinated","people_vaccinated")
    vaccine_main_chart <- generate_vaccine_plot(vaccine_data = covid_vaccin_tisefka(),tamurt = input$covid_tamurt,acu = target_stats,covid_stat = covid_stat)
    return(vaccine_main_chart)
  })
  
  output$vaccine_total_charts <- plotly::renderPlotly({
    req(covid_vaccin_tisefka())
    req(input$covid_tamurt)
    covid_stat   <- "total" # vaccine
    target_stats <-  c("total_vaccinations","people_fully_vaccinated","people_vaccinated")
    vaccine_main_chart <- generate_vaccine_plot(vaccine_data = covid_vaccin_tisefka(),tamurt = input$covid_tamurt,acu = target_stats,covid_stat = covid_stat)
    return(vaccine_main_chart)
  })
  
  output$download_covac_data_ui <- renderUI({
    req(covid_vaccin_tisefka())
    downloadButton(outputId = "download_covac_data", label = "Download Covid(Vaccine) Data")
  })
  output$download_covac_data <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      readr::write_csv(covid_vaccin_tisefka(), con)
    }
  )
  
}
