covid_import_data <- function(){
  data_source_file <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"
  
  data_source_files <- c(data_source_file,gsub("confirmed","deaths",data_source_file),gsub("confirmed","recovered",data_source_file))
  names(data_source_files) <- c("confirmed","deaths","recovered")
  
  covid_data <- data_source_files%>%purrr::map(~readr::read_csv(.))
  return(covid_data)
}



vaccine_import_data <- function(){
  vaccine_data_file <- paste0("covid_vaccine_data_",Sys.Date(),".csv")
  if(!file.exists(vaccine_data_file)){
    vaccination_data_file <- "https://proxy.hxlstandard.org/data.csv?tagger-match-all=on&tagger-01-header=location&tagger-01-tag=%23country%2Bname&tagger-02-header=iso_code&tagger-02-tag=%23country%2Bcode&tagger-03-header=date&tagger-03-tag=%23date&tagger-04-header=total_vaccinations&tagger-04-tag=%23total%2Bvaccinations&tagger-08-header=daily_vaccinations&tagger-08-tag=%23total%2Bvaccinations%2Bdaily&url=https%3A%2F%2Fraw.githubusercontent.com%2Fowid%2Fcovid-19-data%2Fmaster%2Fpublic%2Fdata%2Fvaccinations%2Fvaccinations.csv"
                             # "https://proxy.hxlstandard.org/data.csv?tagger-match-all=on&tagger-01-header=location&tagger-01-tag=%23country%2Bname&tagger-02-header=iso_code&tagger-02-tag=%23country%2Bcode&tagger-03-header=date&tagger-03-tag=%23date&tagger-04-header=total_vaccinations&tagger-04-tag=%23total%2Bvaccinations&tagger-08-header=daily_vaccinations&tagger-08-tag=%23total%2Bvaccinations%2Bdaily&url=https%3A%2F%2Fraw.githubusercontent.com%2Fowid%2Fcovid-19-data%2Fmaster%2Fpublic%2Fdata%2Fvaccinations%2Fvaccinations.csv"
    
    vaccinated <- vaccination_data_file%>%readr::read_csv()%>%
      dplyr::mutate(date = as.Date(date,format ="%Y-%m-%d"))%>%
      dplyr::select(-iso_code)
    colnames(vaccinated)[1] <- "country"
    vaccinated <- vaccinated[-1,]%>%
      dplyr::mutate(total_vaccinations = as.numeric(total_vaccinations),daily_vaccinations = as.numeric(daily_vaccinations))%>%
      tidyr::pivot_longer(
        cols = 3:ncol(vaccinated),
        names_to = "statistic",
        values_to = "total",
        values_drop_na = TRUE
      )%>%dplyr::arrange(date)%>%
      dplyr::group_by(country,statistic)%>%dplyr::mutate(daily = total - lag(total)) 
    vaccinated%>%readr::write_csv(vaccine_data_file)
  }else{
    vaccinated <- readr::read_csv(vaccine_data_file)
  }
  
  return(vaccinated)
}



covid_data_prep <- function(covid_data = NULL,target_metric = NULL){
  covid_data <- covid_data%>%tidyr::pivot_longer(
    cols = 5:ncol(covid_data),
    names_to = "date",
    names_prefix = "date",
    values_to = "cases",
    values_drop_na = TRUE
  )%>%dplyr::mutate(date = as.Date(date,format ="%m/%d/%y"),statistic = target_metric)
  
  colnames(covid_data)[1:2] <- c('province','country')
  covid_data%>%dplyr::group_by(province,country)%>%dplyr::mutate(daily = cases - lag(cases))
  #return(target_metric)
}


#tamurt <- "Algeria"
#acu <- "deaths"
generate_covid_plot <- function(covid_data,tamurt = NULL,acu = NULL, covid_stat = NULL,tazwara= NULL,tagara = NULL){
  covid_plot <- covid_data%>%dplyr::filter(country ==!!tamurt,statistic%in%!!acu)%>%
    plotly::plot_ly()%>%
    plotly::add_lines(x = ~date,y = ~base::get(covid_stat),color = ~statistic, colors = c(I("orange"), I("red"),I("darkgreen")))%>%
    plotly::layout(legend = list(orientation = "h",x = 0.5, y = 1.1), title = list(text  = paste0(tamurt) ))
  return(covid_plot)
}

generate_vaccine_plot <- function(vaccine_data,tamurt = NULL,acu = NULL, covid_stat = NULL,tazwara= NULL,tagara = NULL){
  vaccine_plot <- vaccine_data%>%dplyr::filter(country ==!!tamurt & statistic %in% acu)%>%
  plotly::plot_ly()%>%
    plotly::add_lines(x = ~date,y = ~base::get(covid_stat) ,color = ~statistic, colors = c(I("lightgreen"), I("lightblue"),I("navy")))%>%
    plotly::layout(legend = list(orientation = "h",x = 0.5, y = 1.1), yaxis = list(title = "Covid Vaccine"),title = list(text  = paste0(tamurt) ))
  return(vaccine_plot)
}
