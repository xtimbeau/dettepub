get_bdf <- function(keys) {
  
  #remplacez par votre clef d'API
  headers <- httr::add_headers(
    Authorization = "Apikey dd7eb50cce47895d7ef4b000bc189b850e19cd6f0e276f8cb3797446",  
    accept = "application/json"
  )
  
  raw <- purrr::imap_dfr(keys, ~{
    url <- "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/observations/exports/json?order_by=time_period_start&refine=series_key:{.x}"
    url <- glue::glue(url)
    
    response <- httr::GET(url,  headers, httr::config(ssl_verifypeer = FALSE))
    
    if (httr::status_code(response) == 200) {
      # Extraire le contenu JSON de la réponse
      content_json <- httr::content(response, "text", encoding = "UTF-8")
      
      # Convertir le JSON en dataframe
      data <- jsonlite::fromJSON(content_json, flatten = TRUE)
      data <- tibble::as_tibble(data) |> 
        mutate(name = .y)
      return(data)  
      
    } else {
      cli::cli_alert("La requête {.x} a échoué avec le statut:{status_code(response)}")
      return(NULL)
    }
  })
}