# Honduras Remittances Shiny App

This folder contains a standalone Shiny app and the required data files.

## Files
- app.R: Main Shiny app
- Municipios.xlsx: Municipality coordinates
- Remesas.xlsx: Remittance data

## Usage
1. Open this folder in RStudio or set your working directory here in R.
2. Run:
   library(shiny)
   shiny::runApp('app.R')

## Deployment
To deploy to shinyapps.io, upload all three files (app.R, Municipios.xlsx, Remesas.xlsx).

## Requirements
- R (>= 4.0)
- Packages: shiny, tidyverse, readxl, sf, leaflet, rnaturalearth, stringi, scales, here
