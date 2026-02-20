# =============================================================================
# Shiny App – Honduras Remittances Explorer
# =============================================================================
# Run with:
#   shiny::runApp("app.R")
# Required packages: shiny, tidyverse, readxl, sf, leaflet, rnaturalearth,
#                    rnaturalearthdata, stringi, scales, here
# =============================================================================

library(shiny)
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringi)
library(scales)
library(here)

# ── 1. Load & prepare data (runs once on app start) ─────────────────────────

# Municipality coordinates
df_municipios <- read_excel(
  here("Municipios.xlsx"),
  sheet = "municipios_completo"
) %>%
  rename(Municipio = nomMun, Departamento = nomDepto) %>%
  mutate(
    Departamento = stri_trans_general(str_trim(Departamento), "Latin-ASCII"),
    Municipio    = stri_trans_general(str_trim(Municipio),    "Latin-ASCII")
  )

# Remittance transactions
df_remesas <- read_excel(
  here("Remesas.xlsx"),
  sheet = "Municipio"
) %>%
  mutate(
    Departamento = stri_trans_general(str_trim(Departamento), "Latin-ASCII"),
    Municipio    = stri_trans_general(str_trim(Municipio),    "Latin-ASCII"),
    AÑO          = as.integer(`AÑO`)
  )

# Join coordinates
df_remesas <- df_remesas %>%
  left_join(
    df_municipios %>% select(Departamento, Municipio, lat, lon),
    by = c("Departamento", "Municipio")
  )

# Honduras boundary shapes
honduras_states  <- ne_states(country = "Honduras", returnclass = "sf") %>%
  mutate(Dept_clean = stri_trans_general(str_trim(name), "Latin-ASCII"))
honduras_country <- ne_countries(scale = "medium", country = "Honduras",
                                 returnclass = "sf")

# Choices for filters
all_years   <- sort(unique(df_remesas$AÑO))
all_months  <- sort(unique(df_remesas$Mes))
all_depts   <- sort(unique(df_remesas$Departamento))

# ── 2. UI ────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("Honduras Remittances Explorer"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Year filter
      checkboxGroupInput(
        "sel_year", "Year",
        choices  = c("All", all_years),
        selected = "All",
        inline   = TRUE
      ),

      # Month filter
      checkboxGroupInput(
        "sel_month", "Month",
        choices  = c("All", all_months),
        selected = "All",
        inline   = TRUE
      ),

      # Department filter
      checkboxGroupInput(
        "sel_dept", "Department",
        choices  = c("All", all_depts),
        selected = "All"
      ),

      # Municipality filter (updates dynamically)
      uiOutput("muni_selector"),

      hr(),
      helpText("Select filters and explore the maps."),
      helpText("The Department map shows the share of total remittances."),
      helpText("The Municipality map shows point size by USD amount.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",

        # ── Tab 1: Department choropleth (leaflet) ──
        tabPanel(
          "Department Map",
          br(),
          leafletOutput("dept_map", height = "600px"),
          br(),
          tableOutput("dept_table")
        ),

        # ── Tab 2: Municipality bubble map (leaflet) ──
        tabPanel(
          "Municipality Map",
          br(),
          leafletOutput("muni_map", height = "600px"),
          br(),
          tableOutput("muni_table")
        )
      )
    )
  )
)

# ── 3. Server ────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Dynamic municipality selector ──
  output$muni_selector <- renderUI({
    deps <- input$sel_dept
    if (is.null(deps) || "All" %in% deps) {
      choices <- sort(unique(df_remesas$Municipio))
    } else {
      choices <- sort(unique(
        df_remesas$Municipio[df_remesas$Departamento %in% deps]
      ))
    }
    checkboxGroupInput(
      "sel_muni", "Municipality",
      choices  = c("All", choices),
      selected = "All"
    )
  })

  # ── Filtered data reactive ──
  filtered <- reactive({
    d <- df_remesas

    # Year
    if (!is.null(input$sel_year) && !("All" %in% input$sel_year)) {
      d <- d %>% filter(AÑO %in% as.integer(input$sel_year))
    }
    # Month
    if (!is.null(input$sel_month) && !("All" %in% input$sel_month)) {
      d <- d %>% filter(Mes %in% input$sel_month)
    }
    # Department
    if (!is.null(input$sel_dept) && !("All" %in% input$sel_dept)) {
      d <- d %>% filter(Departamento %in% input$sel_dept)
    }
    # Municipality
    if (!is.null(input$sel_muni) && !("All" %in% input$sel_muni)) {
      d <- d %>% filter(Municipio %in% input$sel_muni)
    }

    d
  })

  # ── Department-level aggregation ──
  dept_data <- reactive({
    d <- filtered() %>%
      group_by(Departamento) %>%
      summarise(monto_sum = sum(Monto, na.rm = TRUE), .groups = "drop")

    total <- sum(d$monto_sum, na.rm = TRUE)
    d <- d %>% mutate(prop = ifelse(total > 0, monto_sum / total, 0))

    # Join to shapes
    sf_dept <- honduras_states %>%
      left_join(d, by = c("Dept_clean" = "Departamento"))
    sf_dept$monto_sum <- as.numeric(sf_dept$monto_sum)
    sf_dept$prop      <- as.numeric(sf_dept$prop)
    sf_dept$monto_sum[is.na(sf_dept$monto_sum)] <- 0
    sf_dept$prop[is.na(sf_dept$prop)]           <- 0

    list(sf = sf_dept, table = d, total = total)
  })

  # ── Municipality-level aggregation ──
  muni_data <- reactive({
    filtered() %>%
      filter(!is.na(lat), !is.na(lon)) %>%
      group_by(Departamento, Municipio, lat, lon) %>%
      summarise(
        monto_sum    = sum(Monto, na.rm = TRUE),
        beneficiario = sum(Beneficiario, na.rm = TRUE),
        .groups      = "drop"
      )
  })

  # ── Tab 1: Department leaflet choropleth ──
  output$dept_map <- renderLeaflet({
    dd   <- dept_data()
    sf_d <- dd$sf

    pal <- colorNumeric(
      palette = c("#deebf7", "#08306b"),
      domain  = sf_d$monto_sum
    )

    leaflet(sf_d) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(monto_sum),
        fillOpacity = 0.75,
        color       = "white",
        weight      = 1,
        popup       = ~paste0(
          "<b>", Dept_clean, "</b><br>",
          "Total: $", formatC(monto_sum, format = "f", big.mark = ",", digits = 0), "<br>",
          "Share: ", round(prop * 100, 1), "%"
        )
      ) %>%
      addLegend(
        position  = "bottomright",
        pal       = pal,
        values    = ~monto_sum,
        labFormat = function(type, cuts, p) {
          paste0("$", formatC(cuts, format = "f", big.mark = ",", digits = 0))
        },
        title     = "Total USD"
      )
  })

  output$dept_table <- renderTable({
    tab <- dept_data()$table
    total <- sum(tab$monto_sum, na.rm = TRUE)
    tab %>%
      arrange(desc(monto_sum)) %>%
      mutate(
        raw_share = percent(monto_sum / total, accuracy = 0.1),
        monto_sum = comma(monto_sum, accuracy = 1)
      ) %>%
      select(Department = Departamento, `Total (USD)` = monto_sum, `Raw Share (%)` = raw_share)
  })

  # ── Tab 2: Municipality leaflet bubble map ──
  output$muni_map <- renderLeaflet({
    md <- muni_data()

    max_amount <- max(md$monto_sum, na.rm = TRUE)
    md <- md %>% mutate(radius = sqrt(monto_sum / max(max_amount, 1)) * 25 + 2)

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data        = honduras_country,
        fillColor   = "transparent",
        color       = "black",
        weight      = 2,
        fillOpacity = 0
      ) %>%
      addPolygons(
        data        = honduras_states,
        fillColor   = "transparent",
        color       = "gray50",
        weight      = 1,
        fillOpacity = 0
      ) %>%
      addCircleMarkers(
        data        = md,
        lng         = ~lon,
        lat         = ~lat,
        radius      = ~radius,
        color       = "#08519c",
        fillColor   = "#2171b5",
        fillOpacity = 0.6,
        weight      = 1,
        popup       = ~paste0(
          "<b>", Municipio, "</b> (", Departamento, ")<br>",
          "Amount: $", comma(monto_sum, accuracy = 1), "<br>",
          "Beneficiaries: ", comma(beneficiario, accuracy = 1)
        )
      )
  })

  output$muni_table <- renderTable({
    d <- muni_data()
    if (inherits(d, "sf")) d <- sf::st_drop_geometry(d)
    d <- d %>% arrange(desc(monto_sum)) %>% head(20)
    total <- sum(d$monto_sum, na.rm = TRUE)
    d %>%
      mutate(
        raw_share = percent(monto_sum / total, accuracy = 0.1),
        monto_sum = comma(monto_sum, accuracy = 1)
      ) %>%
      select(Departamento, Municipio, `Total (USD)` = monto_sum, `Raw Share (%)` = raw_share)
  })
}

# ── 4. Run ───────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
