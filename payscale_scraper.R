library(rvest)
library(jsonlite)
library(pbapply)

### you may choose the country by modifying the URL here
base_url <- read_html('https://www.payscale.com/research/US/Job')

categories <- 
  base_url %>% 
  html_nodes(".related-content-card") %>% 
  html_attr('href')

categories_links <- 
  paste0('https://www.payscale.com', categories)

alpha_maker <- function(job_type) {
  job_type <- read_html(job_type)
  job_type_alphabetic <- 
    job_type %>% 
    html_nodes(".alpha-nav__link") %>% 
    html_attr('href')
  
  return(job_type_alphabetic)
}

job_type_alphabetic <- 
  unlist(pblapply(categories_links, alpha_maker))

job_type_alphabetic_links <- 
  paste0('https://www.payscale.com', job_type_alphabetic)

get_job_links <- function(job_type_alphabetic) {
  jobs <- 
    read_html(job_type_alphabetic) %>%
    html_nodes('.subcats__links__item')%>%
    html_attr('href')
  return(jobs)
}

rel_links <- unlist(pblapply(job_type_alphabetic_links, get_job_links))

job_links <- 
  paste0('https://www.payscale.com', rel_links)

data_retriever <- function(job) {
  job <- read_html(job)
  retrieved_json_1  <- fromJSON(job %>%
                                  html_nodes(xpath = "//script[@type='application/ld+json']")%>%
                                  html_text()
  )
  retrieved_json_2  <- fromJSON(job %>%
                                  html_nodes(xpath = "//script[@type='application/json']")%>%
                                  html_text())
  return (c(retrieved_json_1, retrieved_json_2))
}

final <- pblapply(job_links, data_retriever)
saveRDS(final, file = "all_US_payscale.rds")
