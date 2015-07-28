library(RCurl)     # Accessing HTML content
library(rgdal)     # Coordinate re-projection
library(RSQLite)   # Database connectivity
library(gWidgets)  # High-level GUI production
library(RGtk2)     # GUI framework. Could use Tcl or Java, too
library(gWidgets(RGtk2)  # gWidgets toolkit for given framework
options("guiToolkit"="RGtk2")

source("cacheExtracts.r")
source("loadDB.r")
source("crimeGUI.r")


filename <- cacheExtracts()   # Pass FALSE to download first time
dbname   <- loadDB(filename)  # Creates crime.db database by default
rm(cacheExtracts, loadDB)     # Clean up

crimeGUI()              # Source and calls gmap.r function











