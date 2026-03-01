# Honduras Remittances

This repository contains a Shiny app and supporting files for exploring remittance data by department and municipality in Honduras.

## Features
- Interactive Shiny app with filters for year, month, department, and municipality
- Leaflet maps and summary tables
- Data normalization and robust joining
- You can see the app in https://elviscasco.shinyapps.io/remittances/

## Files
- `app.R`: Main Shiny app
- `Code_Honduras.qmd`: Quarto analysis (optional)
- `Remesas.xlsx`: Remittance data
- `Municipios.xlsx`: Municipality coordinates
- `sheets_csv/Departamento.csv`, `sheets_csv/Municipio.csv`: Additional reference data

## Getting Started
1. Clone this repository:
   ```sh
   git clone https://github.com/yourusername/honduras-remittances-app.git
   cd honduras-remittances-app
   ```
2. Open `app.R` in RStudio or run:
   ```r
   shiny::runApp('app.R')
   ```
3. All data files are included for reproducibility.

## Requirements
- R (>= 4.0)
- Packages: shiny, tidyverse, readxl, leaflet, sf, rnaturalearth, stringi, scales, here

## License
This project is licensed under the MIT License.

## Credits
Developed for Geospatial Data Science coursework.
