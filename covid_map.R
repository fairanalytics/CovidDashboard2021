library(leaflet)
covid_data_raw <- readr::read_csv("covid_tisefka_assa_2021_05_04")

covid_data <- covid_data_raw%>%dplyr::filter((date =="2021-03-05") & (statistic == "confirmed") )%>%
  dplyr::distinct(country,.keep_all = TRUE)%>%dplyr::mutate(prop = cases/max(cases))


# Create a continuous palette function
pal <- colorNumeric(
  palette = "Greens",
  domain = covid_data$cases)

 


covid_data %>% leaflet() %>%
  addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat,  weight = 5,
             radius = ~cases,color = ~pal(cases))
