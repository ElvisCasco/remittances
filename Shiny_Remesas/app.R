# =============================================================================
# Shiny App – Honduras Remittances Explorer
# =============================================================================
# Run with:
#   shiny::runApp("app.R")
# Required packages: shiny, tidyverse, readxl, sp, leaflet, stringi, scales, here
# Spatial data pre-processed as sp objects (honduras_states.rds, honduras_country.rds)
# =============================================================================

library(shiny)
library(tidyverse)
library(readxl)
library(sp)
library(leaflet)
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

# Honduras boundary shapes (pre-processed sp objects – no sf/units needed)
honduras_states  <- readRDS(here("honduras_states.rds"))
honduras_country <- readRDS(here("honduras_country.rds"))

# Choices for filters
all_years   <- sort(unique(df_remesas$AÑO))
all_months  <- sort(unique(df_remesas$Mes))
all_depts   <- sort(unique(df_remesas$Departamento))

# ── 2. UI ────────────────────────────────────────────────────────────────────

ui <- fluidPage(

  # ── Mobile-friendly head: viewport + responsive CSS ──
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      /* ── General ── */
      body {
        font-size: 16px;
        -webkit-text-size-adjust: 100%;
      }
      h2 { font-size: 1.5rem; }

      /* ── Collapsible filter toggle button ── */
      #filter_toggle {
        display: none;            /* hidden on desktop */
        width: 100%;
        padding: 14px;
        margin-bottom: 10px;
        font-size: 1.1rem;
        font-weight: 600;
        background: #337ab7;
        color: #fff;
        border: none;
        border-radius: 6px;
        cursor: pointer;
      }
      #filter_toggle:active {
        background: #23527c;
      }

      /* ── Larger checkboxes for touch ── */
      .checkbox label, .checkbox-inline label {
        padding-left: 28px;
        font-size: 1rem;
        line-height: 1.8;
      }
      .checkbox input[type='checkbox'],
      .checkbox-inline input[type='checkbox'] {
        width: 22px;
        height: 22px;
        margin-top: 2px;
      }

      /* ── Scrollable tables ── */
      .table-wrap {
        width: 100%;
        overflow-x: auto;
        -webkit-overflow-scrolling: touch;
      }
      .table-wrap table {
        min-width: 480px;
        font-size: 0.95rem;
      }

      /* ── Tabs: larger tap targets ── */
      .nav-tabs > li > a {
        padding: 12px 18px;
        font-size: 1.05rem;
      }

      /* ── Map container: responsive height ── */
      .map-container {
        width: 100%;
        height: 65vh;
        min-height: 350px;
      }

      /* ──────────── MOBILE (≤ 768 px) ──────────── */
      @media (max-width: 768px) {

        /* Show the toggle button */
        #filter_toggle { display: block; }

        /* Stack sidebar above main content, full width */
        .col-sm-3, .col-sm-9 {
          width: 100% !important;
          padding-left: 10px;
          padding-right: 10px;
        }

        /* Sidebar panel: start collapsed */
        #filter_panel {
          display: none;
        }
        #filter_panel.open {
          display: block;
        }

        /* Shorter map on small screens */
        .map-container {
          height: 50vh;
          min-height: 280px;
        }

        /* Compact title */
        h2 { font-size: 1.25rem; text-align: center; }

        /* Tabs: equal-width for two tabs */
        .nav-tabs { display: flex; }
        .nav-tabs > li { flex: 1; text-align: center; }
        .nav-tabs > li > a {
          padding: 14px 6px;
          font-size: 0.95rem;
        }

        /* Help text smaller on mobile */
        .help-block { font-size: 0.85rem; }
      }
    "))
  ),

  titlePanel("Honduras Remittances Explorer"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Toggle button (visible only on mobile via CSS)
      tags$button(
        id = "filter_toggle",
        "\U0001F50D Filters \u25BC",
        onclick = "var p=document.getElementById('filter_panel');
                   p.classList.toggle('open');
                   this.textContent=p.classList.contains('open')
                     ?'\u2716 Close Filters':'\uD83D\uDD0D Filters \u25BC';"
      ),

      # Wrap all filters in a collapsible div
      tags$div(
        id = "filter_panel",

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
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",

        # ── Tab 1: Department choropleth ──
        tabPanel(
          "Department Map",
          br(),
          tags$div(class = "map-container",
            leafletOutput("dept_map", height = "100%", width = "100%")
          ),
          br(),
          tags$div(class = "table-wrap", tableOutput("dept_table"))
        ),

        # ── Tab 2: Municipality bubble map ──
        tabPanel(
          "Municipality Map",
          br(),
          tags$div(class = "map-container",
            leafletOutput("muni_map", height = "100%", width = "100%")
          ),
          br(),
          tags$div(class = "table-wrap", tableOutput("muni_table"))
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

    # Join to shapes (sp merge)
    sp_dept <- honduras_states
    sp_dept@data <- sp_dept@data %>%
      left_join(d, by = c("Dept_clean" = "Departamento"))
    sp_dept@data$monto_sum <- as.numeric(sp_dept@data$monto_sum)
    sp_dept@data$prop      <- as.numeric(sp_dept@data$prop)
    sp_dept@data$monto_sum[is.na(sp_dept@data$monto_sum)] <- 0
    sp_dept@data$prop[is.na(sp_dept@data$prop)]           <- 0

    list(sp = sp_dept, table = d, total = total)
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
    sp_d <- dd$sp

    pal <- colorNumeric(
      palette = c("#deebf7", "#08306b"),
      domain  = sp_d@data$monto_sum
    )

    leaflet(sp_d) %>%
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
