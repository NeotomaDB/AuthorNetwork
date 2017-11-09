#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(leaflet)
library(readr)
library(sf)
library(geosphere)
library(purrr)
library(jsonlite)
library(shinycssloaders)

ds_cont <- readr::read_csv('ds_cont.csv')

contacts <- readr::read_csv('contacts_full.csv') %>% 
  filter(!is.na(geom) & contact.id %in% ds_cont$pi) %>%
  arrange(contact.name)

get_contact_ll <- function(x) {
  loc <- sapply(contacts$geom[x], function(y) fromJSON(y)$location %>% unlist) %>% t
  loc <- data.frame(lng = loc[,2],
                    lat = loc[,1])
  loc
}

#' @title Make the curved lines for network plot.
#' @param author_id The name of the author to be matched.
#' @param datasets A file read in from disk, with information about datasets.
#' 
make_lines <- function(author_id, datasets = ds_cont) {
  
  site <- datasets %>% filter(pi %in% author_id)
  
  cont_row <- which(contacts$contact.id %in% author_id)
  
  if(!any(is.na(contacts$geom[cont_row]))) {
    
    max_dist <- site[,1:2] %>% dist %>% max %>% floor + 1
    
    make_slope <- cont_row %>% 
      map(function(x){ gcIntermediate(site[,1:2], get_contact_ll(x),
                                 n = 50 * ceiling(max_dist),
                                 addStartEnd = TRUE, 
                                 sp = TRUE ) %>% st_as_sf } )
    
    make_slope <- do.call(rbind, make_slope)
    
    make_slope$dst <- site$dst
    
    return(make_slope)
  } else {
    return(NULL)
  }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  output$leafletPlot <- renderLeaflet({
    
    # generate bins based on input$bins from ui.R
    
    # draw the histogram with the specified number of bins
        leaflet() %>%
        addTiles()

  })

  observe({
    shinyjs::toggleState("author", input$selectionType == "Author")
    shinyjs::toggleState("latbounds", input$selectionType == "Location")
    shinyjs::toggleState("lonbounds", input$selectionType == "Location")
    shinyjs::toggleState("mapit", input$selectionType == "Location")
  })
  
  observeEvent(input$author, {
    
    x    <- contacts$contact.id[match(input$author, contacts$contact.name)]
    
    if (!is.na(x)) {
      
      joined_curves <- x %>% 
        map(make_lines) %>%
        Filter(Negate(is.null), .) %>% 
        do.call(rbind, .)
      
      sites <- ds_cont %>% filter(pi == x)
      
      home <- contacts[match(x, contacts$contact.id),]
      
      contact_html <- paste0('<b>', home$contact.name, '</b><hr>',
                             home$address)
      
      site_html <- paste0('<b>', sites$stn, '</b><br>',
                          'types: ', sites$dsts, '<br>',
                          'link: <a href=https://apps.neotomadb.org/explorer/?siteids=', sites$sid, '>Neotoma Explorer</a>')
      
      # pollen_sites <- data.frame(lat = )
      cont_row <- match(x, contacts$contact.id)
      
      bbox <- st_bbox(joined_curves) %>% as.numeric %>% unlist
      
      leafletProxy("leafletPlot", data = joined_curves) %>%
        clearShapes() %>%
        clearMarkers() %>% 
        addPolylines(opacity = .2, weight = 2) %>% 
        addCircleMarkers(lng = sites$lng, lat = sites$lat,
                   popup = site_html,
                   fillOpacity = 1, radius = 4, weight = 8) %>% 
        addCircleMarkers(color = 'red', 
                         lng = get_contact_ll(cont_row)$lng, lat = get_contact_ll(cont_row)$lat,
                         popup = contact_html, fillOpacity = 1, radius = 4, weight = 8) %>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    } else {
      leaflet() %>%
        addTiles()
    }
  })
  
  observeEvent(input$selectionType, {
    if (input$selectionType == "Location") {
      
      leafletProxy("leafletPlot") %>% 
        clearShapes() %>%
        clearMarkers() %>% 
        addPolygons(lng = rep(input$lonbounds, each = 2), 
                    lat = c(input$latbounds, rev(input$latbounds)),
                    layerId = "boundbox")
      
    } else {
      
      x    <- contacts$contact.id[match(input$author, contacts$contact.name)]
      
      if (!is.na(x)) {
        
        joined_curves <- x %>% 
          map(make_lines) %>%
          Filter(Negate(is.null), .) %>% 
          do.call(rbind, .)
        
        sites <- ds_cont %>% filter(pi == x)
        
        home <- contacts[match(x, contacts$contact.id),]
        
        contact_html <- paste0('<b>', home$contact.name, '</b><hr>',
                               home$address)
        
        site_html <- paste0('<b>', sites$stn, '</b><br>',
                            'types: ', sites$dsts, '<br>',
                            'link: <a href=https://apps.neotomadb.org/explorer/?siteids=', sites$sid, '>Neotoma Explorer</a>')
        
        # pollen_sites <- data.frame(lat = )
        cont_row <- match(x, contacts$contact.id)
        
        bbox <- st_bbox(joined_curves) %>% as.numeric %>% unlist
        
        leafletProxy("leafletPlot", data = joined_curves) %>%
          clearShapes() %>%
          clearMarkers() %>% 
          addPolylines(opacity = .2, weight = 2) %>% 
          addCircleMarkers(lng = sites$lng, lat = sites$lat,
                           popup = site_html,
                           fillOpacity = 1, radius = 4, weight = 8) %>% 
          addCircleMarkers(color = 'red', 
                           lng = get_contact_ll(cont_row)$lng, lat = get_contact_ll(cont_row)$lat,
                           popup = contact_html, fillOpacity = 1, radius = 4, weight = 8) %>%
          fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      } else {
        leaflet() %>%
          addTiles()
      }
    }
  })
  
  observeEvent(input$lonbounds, {
    if (input$selectionType == "Location") {
      
      leafletProxy("leafletPlot") %>% 
        clearShapes() %>%
        clearMarkers()
      
      leafletProxy("leafletPlot") %>% 
        addPolygons(lng = rep(input$lonbounds, each = 2), 
                    lat = c(input$latbounds, rev(input$latbounds)),
                    layerId = "boundbox")
    }
  })
  
  observeEvent(input$latbounds, {
    if (input$selectionType == "Location") {
      
      leafletProxy("leafletPlot") %>% 
        clearShapes() %>%
        clearMarkers()
      
      leafletProxy("leafletPlot") %>% 
        addPolygons(lng = rep(input$lonbounds, each = 2), 
                    lat = c(input$latbounds, rev(input$latbounds)),
                    layerId = "boundbox")
    }
  })
  
  observeEvent(input$mapit, {
    
    if (input$selectionType == "Location") {
      datasets <- ds_cont %>%
        filter(lng < max(input$lonbounds) & lng > min(input$lonbounds) &
                 lat < max(input$latbounds) & lat > min(input$latbounds))
      
      x <- contacts %>% filter(contact.id %in% datasets$pi) %>% select(contact.id) %>% unlist
      
      if (length(x) > 0) {
        
        joined_curves <- x %>% 
          make_lines(., datasets) %>%
          Filter(Negate(is.null), .)
        
        sites <- datasets %>% filter(pi %in% x)
        
        home <- contacts[match(x, contacts$contact.id),]
        
        contact_html <- paste0('<b>', home$contact.name, '</b><hr>',
                               home$address)
        
        site_html <- paste0('<b>', sites$stn, '</b><br>',
                            'types: ', sites$dsts, '<br>',
                            'link: <a href=https://apps.neotomadb.org/explorer/?siteids=', sites$sid, '>Neotoma Explorer</a>')
        
        # pollen_sites <- data.frame(lat = )
        cont_row <- match(x, contacts$contact.id)
        
        bbox <- st_bbox(joined_curves) %>% as.numeric %>% unlist
        
        leafletProxy("leafletPlot", data = joined_curves) %>%
          clearShapes() %>%
          clearMarkers() %>% 
          addPolylines(opacity = .2, weight = 2) %>% 
          addPolygons(lng = rep(input$lonbounds, each = 2), 
                      lat = c(input$latbounds, rev(input$latbounds))) %>% 
          addCircleMarkers(color = 'red', 
                           lng = get_contact_ll(cont_row)$lng, 
                           lat = get_contact_ll(cont_row)$lat,
                           popup = contact_html, 
                           fillOpacity = 1, 
                           radius = 4, 
                           weight = 8) %>%
          addCircleMarkers(lng = sites$lng, 
                           lat = sites$lat,
                           color = 'blue',
                           popup = site_html,
                           fillOpacity = .5, radius = 2, weight = 8) %>% 
          fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
        
      } else {
        leaflet() %>%
          addTiles()
      }
    }
  })
  
})
