#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(purrr)
library(geosphere)
library(jsonlite)
library(shinyjs)
library(shinycssloaders)

ds_cont <- readr::read_csv('ds_cont.csv')

contacts <- readr::read_csv('contacts_full.csv') %>% 
  filter(!is.na(geom) & contact.id %in% ds_cont$pi) %>%
  arrange(contact.name)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(theme = 'network.css',
    shinyjs::useShinyjs(),

    # Application title
    titlePanel("Neotoma Author Networks", windowTitle = "NeotomaAuthors"),
  
    # Sidebar with a selection input for author names:
    sidebarLayout(
      sidebarPanel(
        HTML('<b>Select Network by Author or Location:</b><br>'),
        radioButtons("selectionType",
                     label = NULL,
                     choices = c("Author", "Location"),
                     selected = "Author"),
        HTML('<hr><b>Choose an Author to see their associated sites:</b><br>'),
        selectInput("author",
                    label = NULL,
                    choices = c("", contacts$contact.name)),
        HTML('<hr><b>Latitude</b>'),
        sliderInput("latbounds", label = NULL, min=-90, max=90, value=c(43, 45), step = 1),
        HTML('<b>Longitude</b>'),
        sliderInput("lonbounds", label = NULL, min=-180, max=180, value=c(-110, -100), step = 1),
        actionButton("mapit", label = "Check Bounding Box"),
        HTML('<hr><br><img src="packrat.png" width = "100%" alt="Neotoma logo" style = "display: block;margin: 0 auto;max-width:300px;"/>'),
        HTML('<hr>This map represents all sites associated with a particular Principal Investigator within the Neotoma Database.'),
        HTML('The Neotoma Paleoecological Database is a database of Pleio-Pleistocene (~5Myr) fossil records.<br>To learn more about Neotoma, visit the <a href=http://neotomadb.org>Neotoma Website</a>.')
      ),
      
      mainPanel(
         leafletOutput("leafletPlot", height = 600) %>% withSpinner()
      )
    )
  )
)