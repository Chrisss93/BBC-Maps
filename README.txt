Hello! This is my first attempt at using the leaflet package in R in conjunction with Shiny! This App is designed to test jcheng5’s Leaflet library for reactive Shiny programming.

To run this app, you will need the following packages: XML, RCurl, plyr, stringr, ggplot2, devtools, shiny
You will also need leaflet (NOT leafletR) but you can only acquire this package through devtools. If you are missing any of these libraries, write the necessary lines in your R console.

install.packages("XML")
install.packages("RCurl")
install.packages("plyr")
install.packages("stringr")
install.packages("gglot2")
install.packages("devtools")
install.packages("shiny")
devtools::install_github("leaflet-shiny", "jcheng5")

Now, to run this app, write in your R console:

runGitHub("BBC-Maps", "Chrisss93")

Please report any errors to this repository. Thanks!