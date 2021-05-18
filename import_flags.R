#

list_of_countries <- readr::read_csv("./covid_tisefka_assa_2021_05_04")
list_of_countries <- list_of_countries$country%>%unique()
list_of_countries%>%purrr::map(
  ~load_country_flag(.x)
)


load_country_flag <- function(x = NULL){
  tryCatch(
    download.file(paste0("https://flagshub.com/images/flag-of-",tolower(x),".png"),
                  destfile = paste0("./flag/flag-of-",x,".png") ,
                  mode = "wb"),
    finally = print(paste(x,"failed")),
    error = function(e) e
  )
}
