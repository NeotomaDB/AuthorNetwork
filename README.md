![lifecycle](https://img.shields.io/badge/lifecycle-archived-orange.svg)

# Author Networks

A repository for the test version of the [Neotoma Author Network Shiny app](https://simongoring.shinyapps.io/authorNetwork/).  This work is sponsored by a grant from the National Sciences Foundation ([Grant #1550855](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1550855&HistoricalAwards=false)) to Simon Goring at the University of Wisconsin-Madison as part of the FlyOver Country application.

## Contributors

* [Simon Goring](http://goring.org) - University of Wisconsin -- Madison

## Description

This project is hosted as a Shiny app on [shinyapps.io](https://simongoring.shinyapps.io/authorNetwork/).  The project links specific spatially located datasets to home institutions for the individual researchers.  Data can be searched by individual or by geographic bounding box.

## Development

The project was developed as an R Shiny app, and is served through [shinyapps.io](http://shinyapps.io).  Currently the app uses data previously downloaded from Neotoma and geolocation is provided through the Google API, as coded in [get_geocoding.R](https://github.com/NeotomaDB/AuthorNetwork/blob/master/get_geocoding.R).  Future development of the app will improve the visualization elements as well as provide dynamic support for geocoding and data access.
