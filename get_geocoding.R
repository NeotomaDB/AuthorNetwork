library(neotoma)
library(leaflet)
library(geosphere)
library(nominatim)
library(ggmap)
library(purrr)
library(dplyr)

contacts <- neotoma::get_contact()

contact_locs <- list()

for(i in 1:length(contact_locs)){
  
  if('try-error' %in% class(contact_locs[[i]]) & !is.na(contacts$address[i])) {
    cat(contacts$address[i], '\n')
    contact_locs[[i]] <- try(geocode(contacts$address[i], 
                            output = 'all'))
    for(i in 1:5000000){i%%31}
  }
  if(i %% 100 == 0) { saveRDS(contact_locs, 'locations.RDS') }
}

contact_table <- function(x) {
  if(length(contact_locs[[x]]) > 1) {
    if(length(contact_locs[[x]]$results) > 0) {
      output <- data.frame(contacts[x,],
                 addr = contact_locs[[x]]$results[[1]]$formatted_address,
                 addr_json = as.character(jsonlite::toJSON(contact_locs[[x]]$results[[1]]$address_components)),
                 geom = as.character(jsonlite::toJSON(contact_locs[[x]]$results[[1]]$geometry)))
    } else {
      output <- data.frame(contacts[x,])
    }
  } else {
    output <- data.frame(contacts[x,])
  }
  return(output)
}

new_cont <- (1:length(contact_locs)) %>% map(contact_table) %>% bind_rows()

readr::write_csv(new_cont, 'contacts_full.csv')

