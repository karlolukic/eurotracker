# Packages ----------------
library(shiny)
library(bslib)
library(plotly)
library(tidyverse)
library(data.table)
library(ecb)
library(DT)
library(dygraphs)
library(xts)
library(zoo)
library(lubridate)
library(readr)
library(here)

# Load data from GitHub ------
inflation_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/inflation_data.rds"))
m3_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/m3_data.rds"))
gdp_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/gdp_data.rds"))
unemployment_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/unemployment_data.rds"))
usd_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/usd_data.rds"))
gov_debt_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/gov_debt_data.rds"))

euro_rate_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/euro_rate_data.rds"))
gov_bond_yield_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/gov_bond_yield_data.rds"))
loans_to_corp_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/loans_to_corp_data.rds"))
loans_to_households_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/loans_to_households_data.rds"))
cost_borrowing_house_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/cost_borrowing_house_data.rds"))
cost_borrowing_corp_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/cost_borrowing_corp_data.rds"))

current_account_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/current_account_data.rds"))
direct_investment_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/direct_investment_data.rds"))
portfolio_investment_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/portfolio_investment_data.rds"))
real_effective_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/real_effective_data.rds"))
reserve_assets_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/reserve_assets_data.rds"))
nominal_effective_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/nominal_effective_data.rds"))

total_assets_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/total_assets_data.rds"))
non_perf_loans_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/non_perf_loans_data.rds"))
significant_inst_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/significant_inst_data.rds"))
return_on_equity_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/return_on_equity_data.rds"))
cet1_ratio_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/cet1_ratio_data.rds"))
liquidity_coverage_data <- readRDS(url("https://raw.githubusercontent.com/karlolukic/eurotracker/main/data/liquidity_coverage_data.rds"))


# Helper functions --------------
get_arrow <- function(diff) {
  if (diff > 0) {
    "↑"
  } else if (diff < 0) {
    "↓"
  } else {
    "="
  }
}

get_change_direction <- function(latest_value, previous_value) {
  if (latest_value > previous_value) {
    "increased"
  } else if (latest_value < previous_value) {
    "decreased"
  } else {
    "remained the same"
  }
}

# Set theme ------
my_theme <- bs_theme(
  version = 4,
  bootswatch = "yeti"
)


# UI ---------
ui <- navbarPage(
  title = "EuroTrackeR",

  # UI First Tab ------------
  tabPanel(
    "Euro Area at a Glance",
    fluidPage(
      theme = my_theme, # assign theme

      titlePanel("Euro Area at a Glance"),
      fluidRow(
        column(6, div(
          style = "border: 2px solid #D3D3D3; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
          uiOutput("inflation_header"),
          plotlyOutput("inflation_plot", height = "300px"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_inflation", ""),
            actionButton("info_inflation", "ℹ"),
            actionButton("show_inflation_modal", "🔍")
          )
        )),
        column(6, div(
          style = "border: 2px solid #D3D3D3; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
          uiOutput("m3_header"),
          plotlyOutput("m3_plot", height = "300px"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_m3", ""),
            actionButton("info_m3", "ℹ"),
            actionButton("show_m3_modal", "🔍")
          )
        ))
      ),
      fluidRow(
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("gdp_growth_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_gdp", ""),
            actionButton("info_gdp", "ℹ"),
            actionButton("show_gdp_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("unemployment_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_unemployment", ""),
            actionButton("info_unemployment", "ℹ"),
            actionButton("show_unemployment_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("usd_exchange_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_usd", ""),
            actionButton("info_usd", "ℹ"),
            actionButton("show_usd_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("gov_debt_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_gov", ""),
            actionButton("info_gov_debt", "ℹ"),
            actionButton("show_gov_debt_modal", "🔍")
          )
        ))
      ),

      # Footer with short disclaimer -------
      fluidRow(
        column(
          12,
          tags$hr(),
          tags$p(
            "Disclaimer: This Shiny dashboard is unofficial and is not endorsed by the European Central Bank (ECB). ",
            "Data are sourced from publicly available ECB resources, and any interpretation is solely the author’s responsibility."
          )
        )
      )
    )
  ),

  # UI Second Tab ------------
  tabPanel(
    "Financial Developments",
    fluidPage(
      titlePanel("Financial Developments"),
      fluidRow(
        column(6, div(
          style = "border: 2px solid #D3D3D3; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
          uiOutput("estr_header"),
          plotlyOutput("estr_plot", height = "300px"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_estr", ""),
            actionButton("info_estr", "ℹ"),
            actionButton("show_estr_modal", "🔍")
          )
        )),
        column(6, div(
          style = "border: 2px solid #D3D3D3; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
          uiOutput("gov_bond_header"),
          plotlyOutput("gov_bond_plot", height = "300px"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_gov_bond", ""),
            actionButton("info_gov_bond", "ℹ"),
            actionButton("show_gov_bond_modal", "🔍")
          )
        ))
      ),
      fluidRow(
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("loans_to_corp_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_loans_to_corp", ""),
            actionButton("info_loans_to_corp", "ℹ"),
            actionButton("show_loans_to_corp_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("loans_to_households_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_loans_to_households", ""),
            actionButton("info_loans_to_households", "ℹ"),
            actionButton("show_loans_to_households_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("cost_borrowing_house_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_cost_borrowing_house", ""),
            actionButton("info_cost_borrowing_house", "ℹ"),
            actionButton("show_cost_borrowing_house_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("cost_borrowing_corp_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_cost_borrowing_corp", ""),
            actionButton("info_cost_borrowing_corp", "ℹ"),
            actionButton("show_cost_borrowing_corp_modal", "🔍")
          )
        ))
      )
    )
  ),

  # UI Third Tab --------
  tabPanel(
    "External Sector and Exchange Rates",
    fluidPage(
      titlePanel("External Sector and Exchange Rates"),
      fluidRow(
        column(6, div(
          style = "border: 2px solid #D3D3D3; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
          uiOutput("cab_header"),
          plotlyOutput("cab_plot", height = "300px"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_cab", ""),
            actionButton("info_cab", "ℹ"),
            actionButton("show_cab_modal", "🔍")
          )
        )),
        column(6, div(
          style = "border: 2px solid #D3D3D3; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
          uiOutput("direct_invest_header"),
          plotlyOutput("direct_invest_plot", height = "300px"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_direct_invest", ""),
            actionButton("info_direct_invest", "ℹ"),
            actionButton("show_direct_invest_modal", "🔍")
          )
        ))
      ),
      fluidRow(
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("portfolio_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_portfolio", ""),
            actionButton("info_portfolio", "ℹ"),
            actionButton("show_portfolio_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("real_effective_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_reer", ""),
            actionButton("info_reer", "ℹ"),
            actionButton("show_reer_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("reserve_assets_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_reserve_assets", ""),
            actionButton("info_reserve_assets", "ℹ"),
            actionButton("show_reserve_assets_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("nominal_effective_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_nominal_effective", ""),
            actionButton("info_nominal_effective", "ℹ"),
            actionButton("show_nominal_effective_modal", "🔍")
          )
        ))
      )
    )
  ),

  # UI Fourth Tab --------
  tabPanel(
    "Banking Supervision",
    fluidPage(
      titlePanel("Banking Supervision"),
      fluidRow(
        column(6, div(
          style = "border: 2px solid #D3D3D3; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
          uiOutput("total_assets_header"),
          plotlyOutput("total_assets_plot", height = "300px"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_total_assets", ""),
            actionButton("info_total_assets", "ℹ"),
            actionButton("show_total_assets_modal", "🔍")
          )
        )),
        column(6, div(
          style = "border: 2px solid #D3D3D3; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
          uiOutput("non_perf_loans_header"),
          plotlyOutput("non_perf_loans_plot", height = "300px"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_non_perf_loans", ""),
            actionButton("info_non_perf_loans", "ℹ"),
            actionButton("show_non_perf_loans_modal", "🔍")
          )
        ))
      ),
      fluidRow(
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("significant_inst_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_significant_inst", ""),
            actionButton("info_significant_inst", "ℹ"),
            actionButton("show_significant_inst_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("return_on_equity_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_return_on_equity", ""),
            actionButton("info_return_on_equity", "ℹ"),
            actionButton("show_return_on_equity_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("cet1_ratio_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_cet1_ratio", ""),
            actionButton("info_cet1_ratio", "ℹ"),
            actionButton("show_cet1_ratio_modal", "🔍")
          )
        )),
        column(3, div(
          style = "border: 2px solid #D3D3D3; padding: 10px; border-radius: 5px; text-align: center;",
          uiOutput("liquidity_coverage_box"),
          div(
            style = "margin-top: 10px;",
            downloadButton("download_liquidity_coverage", ""),
            actionButton("info_liquidity_coverage", "ℹ"),
            actionButton("show_liquidity_coverage_modal", "🔍")
          )
        ))
      )
    )
  ),

  # UI About Tab -------
  tabPanel(
    "About This Dashboard",
    fluidPage(
      titlePanel("About This Dashboard"),
      p(
        "Welcome to the EuroTrackeR dashboard. I created this dashboard
      to replicate the key features of the official ",
        a("ECB Data Portal", href = "https://data.ecb.europa.eu/#dashboard-tab-1"),
        " while showcasing interactive data visualizations using R/Shiny."
      ),
      p(
        "This dashboard leverages the powerful capabilities of the ",
        a("ecb package", href = "https://cran.r-project.org/package=ecb"),
        " by Eric Persson, which facilitates seamless retrieval of ECB data in R."
      ),
      p(
        "If you have any suggestions on how to further improve this dashboard,
       please open an issue on my ",
        a("GitHub repository", href = "https://github.com/karlolukic"),
        " or contact me directly."
      ),
      tags$hr(),
      p(
        "Disclaimer: This Shiny application is unofficial and is not endorsed by the European Central Bank (ECB). ",
        "Data are sourced from publicly available ECB resources, and any interpretation is solely the author’s responsibility."
      )
    )
  )
)


# SERVER ----------
server <- function(input, output, session) {
  current_account_data[, obsvalue_billions := obsvalue / 1e3]
  direct_investment_data[, obsvalue_billions := obsvalue / 1e3]
  portfolio_investment_data[, obsvalue_billions := obsvalue / 1e3]
  reserve_assets_data[, obsvalue_billions := obsvalue / 1e3]
  total_assets_data[, obsvalue_billions := obsvalue / 1e3]

  # TAB 1 REACTIVES ----

  inflation_data_with_date <- inflation_data %>%
    mutate(obstime_date = as.Date(obstime))

  table_visible_inflation <- reactiveVal(FALSE)

  observeEvent(input$toggle_table_inflation, {
    table_visible_inflation(!table_visible_inflation())
  })

  observeEvent(input$show_inflation_modal, {
    updateDateInput(session, "start_date_inflation", value = min(inflation_data_with_date$obstime_date))
    updateDateInput(session, "end_date_inflation", value = max(inflation_data_with_date$obstime_date))
  })

  observeEvent(input$one_year_inflation, {
    updateDateInput(session, "start_date_inflation", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_inflation", value = Sys.Date())
  })
  observeEvent(input$three_years_inflation, {
    updateDateInput(session, "start_date_inflation", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_inflation", value = Sys.Date())
  })
  observeEvent(input$five_years_inflation, {
    updateDateInput(session, "start_date_inflation", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_inflation", value = Sys.Date())
  })
  observeEvent(input$full_range_inflation, {
    updateDateInput(session, "start_date_inflation", value = min(inflation_data_with_date$obstime_date))
    updateDateInput(session, "end_date_inflation", value = max(inflation_data_with_date$obstime_date))
  })

  filtered_inflation_data <- reactive({
    req(input$start_date_inflation, input$end_date_inflation)
    inflation_data_with_date %>%
      filter(obstime_date >= input$start_date_inflation & obstime_date <= input$end_date_inflation)
  })

  # 2) M3 ----
  m3_data_with_date <- m3_data %>%
    mutate(obstime_date = as.Date(obstime))

  table_visible_m3 <- reactiveVal(FALSE)

  observeEvent(input$toggle_table_m3, {
    table_visible_m3(!table_visible_m3())
  })

  observeEvent(input$show_m3_modal, {
    updateDateInput(session, "start_date_m3", value = min(m3_data_with_date$obstime_date))
    updateDateInput(session, "end_date_m3", value = max(m3_data_with_date$obstime_date))
  })

  observeEvent(input$one_year_m3, {
    updateDateInput(session, "start_date_m3", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_m3", value = Sys.Date())
  })
  observeEvent(input$three_years_m3, {
    updateDateInput(session, "start_date_m3", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_m3", value = Sys.Date())
  })
  observeEvent(input$five_years_m3, {
    updateDateInput(session, "start_date_m3", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_m3", value = Sys.Date())
  })
  observeEvent(input$full_range_m3, {
    updateDateInput(session, "start_date_m3", value = min(m3_data_with_date$obstime_date))
    updateDateInput(session, "end_date_m3", value = max(m3_data_with_date$obstime_date))
  })

  filtered_m3_data <- reactive({
    req(input$start_date_m3, input$end_date_m3)
    m3_data_with_date %>%
      filter(obstime_date >= input$start_date_m3 & obstime_date <= input$end_date_m3)
  })

  # 3) GDP ----
  gdp_data_with_date <- gdp_data %>%
    mutate(obstime_date = as.Date(obstime, frac = 1))

  table_visible_gdp <- reactiveVal(FALSE)

  observeEvent(input$toggle_table_gdp, {
    table_visible_gdp(!table_visible_gdp())
  })

  observeEvent(input$show_gdp_modal, {
    updateDateInput(session, "start_date_gdp", value = min(gdp_data_with_date$obstime_date))
    updateDateInput(session, "end_date_gdp", value = max(gdp_data_with_date$obstime_date))
  })

  observeEvent(input$one_year_gdp, {
    updateDateInput(session, "start_date_gdp", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_gdp", value = Sys.Date())
  })
  observeEvent(input$three_years_gdp, {
    updateDateInput(session, "start_date_gdp", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_gdp", value = Sys.Date())
  })
  observeEvent(input$five_years_gdp, {
    updateDateInput(session, "start_date_gdp", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_gdp", value = Sys.Date())
  })
  observeEvent(input$full_range_gdp, {
    updateDateInput(session, "start_date_gdp", value = min(gdp_data_with_date$obstime_date))
    updateDateInput(session, "end_date_gdp", value = max(gdp_data_with_date$obstime_date))
  })

  filtered_gdp_data <- reactive({
    req(input$start_date_gdp, input$end_date_gdp)
    gdp_data_with_date %>%
      filter(obstime_date >= input$start_date_gdp & obstime_date <= input$end_date_gdp)
  })

  # 4) Unemployment ----
  unemployment_data_with_date <- unemployment_data %>%
    mutate(obstime_date = as.Date(obstime))

  table_visible_unemployment <- reactiveVal(FALSE)

  observeEvent(input$toggle_table_unemployment, {
    table_visible_unemployment(!table_visible_unemployment())
  })

  observeEvent(input$show_unemployment_modal, {
    updateDateInput(session, "start_date_unemployment", value = min(unemployment_data_with_date$obstime_date))
    updateDateInput(session, "end_date_unemployment", value = max(unemployment_data_with_date$obstime_date))
  })

  observeEvent(input$one_year_unemployment, {
    updateDateInput(session, "start_date_unemployment", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_unemployment", value = Sys.Date())
  })
  observeEvent(input$three_years_unemployment, {
    updateDateInput(session, "start_date_unemployment", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_unemployment", value = Sys.Date())
  })
  observeEvent(input$five_years_unemployment, {
    updateDateInput(session, "start_date_unemployment", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_unemployment", value = Sys.Date())
  })
  observeEvent(input$full_range_unemployment, {
    updateDateInput(session, "start_date_unemployment", value = min(unemployment_data_with_date$obstime_date))
    updateDateInput(session, "end_date_unemployment", value = max(unemployment_data_with_date$obstime_date))
  })

  filtered_unemployment_data <- reactive({
    req(input$start_date_unemployment, input$end_date_unemployment)
    unemployment_data_with_date %>%
      filter(obstime_date >= input$start_date_unemployment & obstime_date <= input$end_date_unemployment)
  })

  # 5) USD Exchange Rate ----
  usd_data_with_date <- usd_data %>%
    mutate(obstime_date = as.Date(obstime))

  table_visible_usd <- reactiveVal(FALSE)

  observeEvent(input$toggle_table_usd, {
    table_visible_usd(!table_visible_usd())
  })

  observeEvent(input$show_usd_modal, {
    updateDateInput(session, "start_date_usd", value = min(usd_data_with_date$obstime_date))
    updateDateInput(session, "end_date_usd", value = max(usd_data_with_date$obstime_date))
  })

  observeEvent(input$one_year_usd, {
    updateDateInput(session, "start_date_usd", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_usd", value = Sys.Date())
  })
  observeEvent(input$three_years_usd, {
    updateDateInput(session, "start_date_usd", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_usd", value = Sys.Date())
  })
  observeEvent(input$five_years_usd, {
    updateDateInput(session, "start_date_usd", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_usd", value = Sys.Date())
  })
  observeEvent(input$full_range_usd, {
    updateDateInput(session, "start_date_usd", value = min(usd_data_with_date$obstime_date))
    updateDateInput(session, "end_date_usd", value = max(usd_data_with_date$obstime_date))
  })

  filtered_usd_data <- reactive({
    req(input$start_date_usd, input$end_date_usd)
    usd_data_with_date %>%
      filter(obstime_date >= input$start_date_usd & obstime_date <= input$end_date_usd)
  })

  # 6) Government Debt ----
  gov_debt_data_with_date <- gov_debt_data %>%
    mutate(obstime_date = as.Date(obstime, frac = 1))

  table_visible_gov_debt <- reactiveVal(FALSE)

  observeEvent(input$toggle_table_gov_debt, {
    table_visible_gov_debt(!table_visible_gov_debt())
  })

  observeEvent(input$show_gov_debt_modal, {
    updateDateInput(session, "start_date_gov_debt", value = min(gov_debt_data_with_date$obstime_date))
    updateDateInput(session, "end_date_gov_debt", value = max(gov_debt_data_with_date$obstime_date))
  })

  observeEvent(input$one_year_gov_debt, {
    updateDateInput(session, "start_date_gov_debt", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_gov_debt", value = Sys.Date())
  })
  observeEvent(input$three_years_gov_debt, {
    updateDateInput(session, "start_date_gov_debt", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_gov_debt", value = Sys.Date())
  })
  observeEvent(input$five_years_gov_debt, {
    updateDateInput(session, "start_date_gov_debt", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_gov_debt", value = Sys.Date())
  })
  observeEvent(input$full_range_gov_debt, {
    updateDateInput(session, "start_date_gov_debt", value = min(gov_debt_data_with_date$obstime_date))
    updateDateInput(session, "end_date_gov_debt", value = max(gov_debt_data_with_date$obstime_date))
  })

  filtered_gov_debt_data <- reactive({
    req(input$start_date_gov_debt, input$end_date_gov_debt)
    gov_debt_data_with_date %>%
      filter(obstime_date >= input$start_date_gov_debt & obstime_date <= input$end_date_gov_debt)
  })

  # Reactive expressions for data
  inflation_data_reactive <- reactive({
    inflation_data
  })

  m3_data_reactive <- reactive({
    m3_data
  })

  # Tab 1: Inflation Rate ---------------------
  latest_inflation_value <- inflation_data[.N, obsvalue]
  previous_inflation_value <- inflation_data[.N - 1, obsvalue]
  latest_inflation_date <- format(inflation_data[.N, obstime], "%B %Y")
  previous_inflation_date <- format(inflation_data[.N - 1, obstime], "%B %Y")
  inflation_diff <- latest_inflation_value - previous_inflation_value
  inflation_arrow <- get_arrow(inflation_diff)

  output$inflation_header <- renderUI({
    HTML(paste0("Inflation Rate<br>
                 <span style='font-size: 12px; color: gray;'>", latest_inflation_date, "</span><br>
                <span style='font-size: 16px; color: black;'><b>", format(latest_inflation_value, nsmall = 1), "%</b> ", inflation_arrow, "</span>"))
  })

  output$inflation_plot <- renderPlotly({
    plot_ly(inflation_data[(.N - 7):.N, ],
      x = ~obstime, y = ~obsvalue, type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Percentage change"))
  })

  observeEvent(input$show_inflation_modal, {
    additional_info <- paste0(
      "The timeline shows the inflation rate measured by the Harmonised Index of Consumer Prices (HICP) ",
      "in Euro area (changing composition) from January 1997 to December 2024. ",
      "In ", latest_inflation_date, ", the HICP inflation rate in the euro area ",
      get_change_direction(latest_inflation_value, previous_inflation_value),
      " to ",
      round(latest_inflation_value, 1), " compared to ", round(previous_inflation_value, 1), " in ", previous_inflation_date, ".<br>",
      "Inflation is the general increase in the overall price level of goods and services typically bought by households (citizens). ",
      "It is measured as the average price change over a given period of time for a basket of goods and services that are typically bought in the economy. ",
      "In the euro area, it is measured as changes in the Harmonised Index of Consumer Prices (HICP) compared with the same period one year earlier (“year-on-year” changes). ",
      "The HICP is broken down following the European classification of individual consumption according to purpose (ECOICOP) ",
      "and by goods and services aggregates derived from it."
    )

    showModal(modalDialog(
      title = "HICP - Overall index, Euro area (changing composition), Monthly",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_inflation", "Start Date"),
          dateInput("end_date_inflation", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_inflation", "1 Year"),
          actionButton("three_years_inflation", "3 Years"),
          actionButton("five_years_inflation", "5 Years"),
          actionButton("full_range_inflation", "Full Range")
        ),
        dygraphOutput("inflation_dygraph", height = "300px"),
        div(
          style = "text-align: center; margin-top: 15px;",
          actionButton("toggle_table_inflation", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_inflation % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("inflation_table"))
        ),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML(additional_info))
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  output$inflation_dygraph <- renderDygraph({
    req(filtered_inflation_data())
    inflation_xts <- xts(filtered_inflation_data()$obsvalue,
      order.by = filtered_inflation_data()$obstime_date
    )
    dygraph(inflation_xts) %>%
      dyAxis("y",
        label = "Percentage change",
        axisLabelFormatter = "function(d) { return d.toFixed(1); }",
        valueFormatter = "function(d) { return d.toFixed(1); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE)
  })

  output$inflation_table <- renderDT({
    req(table_visible_inflation())
    filtered_inflation_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "obstime",
          "Value (Percentage change)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatDate(
        columns = c("Period"),
        method = "toLocaleString",
        params = list("en-DE", list(year = "numeric", month = "short"))
      )
  })

  output$download_inflation <- downloadHandler(
    filename = function() {
      paste("inflation_data.csv")
    },
    content = function(file) {
      write.csv(inflation_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_inflation, {
    showModal(modalDialog(
      title = "Inflation Rate Quick Info",
      "Consumer price inflation measured by the Harmonised Index of Consumer Prices (HICP) - Overall index; euro area (changing composition); annual rate of change; Eurostat; neither seasonally nor working day adjusted.",
      easyClose = TRUE
    ))
  })

  # Tab 1: Monetary Aggregate M3 -----------
  latest_m3_value <- m3_data[.N, obsvalue]
  previous_m3_value <- m3_data[.N - 1, obsvalue]
  latest_m3_date <- format(m3_data[.N, obstime], "%B %Y")
  previous_m3_date <- format(m3_data[.N - 1, obstime], "%B %Y")
  m3_diff <- latest_m3_value - previous_m3_value
  m3_arrow <- get_arrow(m3_diff)

  output$m3_header <- renderUI({
    HTML(paste0("Monetary Aggregate M3<br>
             <span style='font-size: 12px; color: gray;'>", latest_m3_date, "</span><br>
             <span style='font-size: 16px; color: black;'><b>", round(latest_m3_value, 1), "%</b> ", m3_arrow, "</span>"))
  })

  output$m3_plot <- renderPlotly({
    plot_ly(m3_data[(.N - 11):.N, ],
      x = ~obstime, y = ~obsvalue, type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(
          title = "Percentage change",
          tickformat = ".2f"
        )
      )
  })

  observeEvent(input$show_m3_modal, {
    additional_info <- c(
      "Monetary aggregates (i.e. M1, M2 and M3) comprise monetary liabilities of MFIs and central government (post office, treasury, etc.) vis-à-vis non-MFI euro area residents excluding central government."
    )

    showModal(modalDialog(
      title = "Monetary aggregate M3 reported by MFIs, central gov. and post office giro institutions in the euro area (annual growth rate), Euro area (changing composition), Monthly",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_m3", "Start Date"),
          dateInput("end_date_m3", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_m3", "1 Year"),
          actionButton("three_years_m3", "3 Years"),
          actionButton("five_years_m3", "5 Years"),
          actionButton("full_range_m3", "Full Range")
        ),
        dygraphOutput("m3_dygraph", height = "300px"),
        div(
          style = "text-align: center; margin-top: 15px;",
          actionButton("toggle_table_m3", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_m3 % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("m3_table"))
        ),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML(additional_info))
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  output$m3_dygraph <- renderDygraph({
    req(filtered_m3_data())
    xts_data <- xts(filtered_m3_data()$obsvalue, order.by = filtered_m3_data()$obstime_date)
    dygraph(xts_data) %>%
      dyAxis("y", label = "Percentage change") %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE)
  })

  output$m3_table <- renderDT({
    req(table_visible_m3())
    filtered_m3_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "obstime",
          "Value (Percentage change)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatDate(
        columns = c("Period"),
        method = "toLocaleString",
        params = list("en-DE", list(year = "numeric", month = "short"))
      ) %>%
      formatRound(columns = c("Value (Percentage change)"), digits = 1)
  })

  output$download_m3 <- downloadHandler(
    filename = function() {
      paste("monetary_aggregate_m3.csv")
    },
    content = function(file) {
      write.csv(m3_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_m3, {
    showModal(modalDialog(
      title = "Monetary Aggregate M3 Quick Info",
      "Annual growth rate of the broad monetary aggregate M3 vis-a-vis euro area non-MFI excl. central gov. (index); euro area (changing composition).",
      easyClose = TRUE
    ))
  })

  # Tab 1: Real GDP Growth -----------
  latest_gdp_value <- gdp_data[.N, obsvalue]
  previous_gdp_value <- gdp_data[.N - 1, obsvalue]
  latest_gdp_date <- format(gdp_data[.N, obstime], "Q%q %Y")
  previous_gdp_date <- format(gdp_data[.N - 1, obstime], "Q%q %Y")
  gdp_diff <- latest_gdp_value - previous_gdp_value
  gdp_arrow <- get_arrow(gdp_diff)

  output$gdp_growth_box <- renderUI({
    HTML(paste0("Real GDP Growth<br>
               <span style='font-size: 12px; color: gray;'>", latest_gdp_date, "</span><br>
               <span style='font-size: 16px; color: black;'><b>", round(latest_gdp_value, 1), "%</b> ", gdp_arrow, "</span>"))
  })

  observeEvent(input$show_gdp_modal, {
    showModal(modalDialog(
      title = paste0("GDP volume growth (economic growth), Euro area 20 (fixed composition) as of ", latest_gdp_date, ", Quarterly"),
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_gdp", "Start Date"),
          dateInput("end_date_gdp", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_gdp", "1 Year"),
          actionButton("three_years_gdp", "3 Years"),
          actionButton("five_years_gdp", "5 Years"),
          actionButton("full_range_gdp", "Full Range")
        ),
        plotlyOutput("gdp_modal_plot", height = "25vh"),
        div(
          style = "text-align: center; margin-top: 15px;",
          actionButton("toggle_table_gdp", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_gdp % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("gdp_table"))
        ),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML("<br><br>"))
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  output$gdp_modal_plot <- renderPlotly({
    req(filtered_gdp_data())
    plot_ly(filtered_gdp_data(),
      x = ~obstime_date, y = ~obsvalue,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "Q%q %Y"
        ),
        yaxis = list(title = "Percentage")
      )
  })

  output$gdp_table <- renderDT({
    req(table_visible_gdp())
    filtered_gdp_data() %>%
      arrange(desc(obstime)) %>%
      mutate(obstime = format(obstime, "Q%q %Y")) %>%
      select(obstime, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "obstime",
          "Value (Tens of Euro)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatRound(columns = c("Value (Tens of Euro)"), digits = 1)
  })

  output$download_gdp <- downloadHandler(
    filename = function() {
      paste("gdp_data.csv")
    },
    content = function(file) {
      write.csv(gdp_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_gdp, {
    showModal(modalDialog(
      title = "Real GDP Growth Quick Info",
      "Annual growth, seasonality adjusted.",
      easyClose = TRUE
    ))
  })

  # Tab 1: Unemployment Rate ----------------
  latest_unemployment_value <- unemployment_data[.N, obsvalue]
  previous_unemployment_value <- unemployment_data[.N - 1, obsvalue]
  latest_unemployment_date <- format(unemployment_data[.N, obstime], "%B %Y")
  previous_unemployment_date <- format(unemployment_data[.N - 1, obstime], "%B %Y")
  unemployment_diff <- latest_unemployment_value - unemployment_data[1, obsvalue]
  unemployment_arrow <- get_arrow(unemployment_diff)

  output$unemployment_box <- renderUI({
    HTML(paste0("Unemployment Rate<br>
               <span style='font-size: 12px; color: gray;'>", latest_unemployment_date, "</span><br>
               <span style='font-size: 16px; color: black;'><b>", round(latest_unemployment_value, 1), "%</b> ", unemployment_arrow, "</span>"))
  })

  observeEvent(input$show_unemployment_modal, {
    showModal(modalDialog(
      title = paste0("Unemployment rate, Euro area 20 (fixed composition) as of ", latest_unemployment_date, ", Monthly"),
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_unemployment", "Start Date"),
          dateInput("end_date_unemployment", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_unemployment", "1 Year"),
          actionButton("three_years_unemployment", "3 Years"),
          actionButton("five_years_unemployment", "5 Years"),
          actionButton("full_range_unemployment", "Full Range")
        ),
        dygraphOutput("unemployment_dygraph", height = "300px"),
        div(
          style = "text-align: center; margin-top: 15px;",
          actionButton("toggle_table_unemployment", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_unemployment % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("unemployment_table"))
        ),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML("<br><br>"))
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  output$unemployment_dygraph <- renderDygraph({
    req(filtered_unemployment_data())
    xts_data <- xts(filtered_unemployment_data()$obsvalue,
      order.by = filtered_unemployment_data()$obstime_date
    )
    dygraph(xts_data) %>%
      dyAxis("y",
        label = "Unemployment Rate (Percent)",
        valueFormatter = 'function(d) { return d.toFixed(2) + "%"; }',
        axisLabelFormatter = 'function(d) { return d.toFixed(0) + "%"; }'
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE)
  })

  output$unemployment_table <- renderDT({
    req(table_visible_unemployment())
    filtered_unemployment_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "obstime",
          "Value (Percent)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatDate(
        columns = c("Period"),
        method = "toLocaleString",
        params = list("en-DE", list(year = "numeric", month = "short"))
      ) %>%
      formatRound(columns = c("Value (Percent)"), digits = 1)
  })

  output$download_unemployment <- downloadHandler(
    filename = function() {
      paste("unemployment_data.csv")
    },
    content = function(file) {
      write.csv(unemployment_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_unemployment, {
    showModal(modalDialog(
      title = "Unemployment Rate Quick Info",
      "Seasonally adjusted.",
      easyClose = TRUE
    ))
  })

  # Tab 1: USD Exchange Rate -----------
  latest_usd_value <- usd_data[.N, obsvalue]
  previous_usd_value <- usd_data[.N - 1, obsvalue]
  latest_usd_date <- format(usd_data[.N, obstime], "%d %B %Y")
  previous_usd_date <- format(usd_data[.N - 1, obstime], "%d %B %Y")
  usd_diff <- latest_usd_value - previous_usd_value
  usd_arrow <- get_arrow(usd_diff)

  output$usd_exchange_box <- renderUI({
    HTML(paste0("US Dollar Exchange Rate<br>
               <span style='font-size: 12px; color: gray;'>", latest_usd_date, "</span><br>
               <span style='font-size: 16px; color: black;'><b>", round(latest_usd_value, 4), "</b> ", usd_arrow, "</span>"))
  })

  observeEvent(input$show_usd_modal, {
    showModal(modalDialog(
      title = "Euro/US dollar, Daily",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_usd", "Start Date"),
          dateInput("end_date_usd", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_usd", "1 Year"),
          actionButton("three_years_usd", "3 Years"),
          actionButton("five_years_usd", "5 Years"),
          actionButton("full_range_usd", "Full Range")
        ),
        dygraphOutput("usd_dygraph", height = "300px"),
        div(
          style = "text-align: center; margin-top: 15px;",
          actionButton("toggle_table_usd", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_usd % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("usd_table"))
        ),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML("<br><br>"))
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  output$usd_dygraph <- renderDygraph({
    req(filtered_usd_data())
    xts_data <- xts(filtered_usd_data()$obsvalue, order.by = filtered_usd_data()$obstime_date)
    dygraph(xts_data) %>%
      dyAxis("y",
        label = "US Dollar",
        axisLabelFormatter = "function(d) { return d.toFixed(4); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE)
  })

  output$usd_table <- renderDT({
    req(table_visible_usd())
    filtered_usd_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "obstime",
          "Value (US dollar)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatDate(
        columns = "Period",
        method = "toLocaleString",
        params = list("en-DE", list(year = "numeric", month = "short", day = "2-digit"))
      ) %>%
      formatRound(columns = c("Value (US dollar)"), digits = 4)
  })

  output$download_usd <- downloadHandler(
    filename = function() {
      paste("usd_exchange_rate.csv")
    },
    content = function(file) {
      write.csv(usd_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_usd, {
    showModal(modalDialog(
      title = "USD Exchange Rate Quick Info",
      "ECB reference exchange rate, US dollar/Euro.",
      easyClose = TRUE
    ))
  })

  # Tab 1: Government Debt ----------
  latest_gov_debt_value <- gov_debt_data[.N, obsvalue]
  previous_gov_debt_value <- gov_debt_data[.N - 1, obsvalue]
  latest_gov_debt_date <- format(gov_debt_data[.N, obstime], "Q%q %Y")
  previous_gov_debt_date <- format(gov_debt_data[.N - 1, obstime], "Q%q %Y")
  gov_debt_diff <- latest_gov_debt_value - gov_debt_data[1, obsvalue]
  gov_debt_arrow <- get_arrow(gov_debt_diff)

  output$gov_debt_box <- renderUI({
    HTML(paste0("Government Debt<br>
               <span style='font-size: 12px; color: gray;'>", latest_gov_debt_date, "</span><br>
               <span style='font-size: 16px; color: black;'><b>", sprintf("%.1f%%", latest_gov_debt_value), "</b> ", gov_debt_arrow, "</span>"))
  })

  observeEvent(input$show_gov_debt_modal, {
    showModal(modalDialog(
      title = paste0("Government debt (consolidated) (as % of GDP), Euro area 20 (fixed composition) as of ", latest_gov_debt_date, ", Quarterly"),
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_gov_debt", "Start Date"),
          dateInput("end_date_gov_debt", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_gov_debt", "1 Year"),
          actionButton("three_years_gov_debt", "3 Years"),
          actionButton("five_years_gov_debt", "5 Years"),
          actionButton("full_range_gov_debt", "Full Range")
        ),
        plotlyOutput("gov_debt_modal_plot", height = "25vh"),
        div(
          style = "text-align: center; margin-top: 15px;",
          actionButton("toggle_table_gov_debt", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_gov_debt % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("gov_debt_table"))
        ),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML("<br><br>"))
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  output$gov_debt_modal_plot <- renderPlotly({
    req(filtered_gov_debt_data())
    plot_ly(filtered_gov_debt_data(),
      x = ~obstime_date, y = ~obsvalue,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "Q%q %Y"
        ),
        yaxis = list(title = "Percentage")
      )
  })

  output$gov_debt_table <- renderDT({
    req(table_visible_gov_debt())
    filtered_gov_debt_data() %>%
      arrange(desc(obstime)) %>%
      mutate(obstime = format(obstime, "Q%q %Y")) %>%
      select(obstime, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "obstime",
          "Value (Tens of Euro)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatRound(columns = c("Value (Tens of Euro)"), digits = 1)
  })

  output$download_gov <- downloadHandler(
    filename = function() {
      paste("government_debt.csv")
    },
    content = function(file) {
      write.csv(gov_debt_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_gov_debt, {
    showModal(modalDialog(
      title = "Government Debt Quick Info",
      "Consolidated; as % of GDP.",
      easyClose = TRUE
    ))
  })


  # TAB 2 REACTIVES ----

  # 1) ESTR data is Daily => convert obstime to Date
  euro_rate_data_with_date <- euro_rate_data %>%
    mutate(obstime_date = as.Date(obstime))

  # 2) AAA-rated government bond yield (Daily as well)
  gov_bond_yield_data_with_date <- gov_bond_yield_data %>%
    mutate(obstime_date = as.Date(obstime))

  # 3) Loans to non-financial corporations (Monthly)
  loans_to_corp_data_with_date <- loans_to_corp_data %>%
    mutate(obstime_date = as.Date(obstime))

  # 4) Loans to households (Monthly)
  loans_to_households_data_with_date <- loans_to_households_data %>%
    mutate(obstime_date = as.Date(obstime))

  # 5) Cost of borrowing for house purchase (Monthly)
  cost_borrowing_house_data_with_date <- cost_borrowing_house_data %>%
    mutate(obstime_date = as.Date(obstime))

  # 6) Cost of borrowing for corporations (Monthly)
  cost_borrowing_corp_data_with_date <- cost_borrowing_corp_data %>%
    mutate(obstime_date = as.Date(obstime))

  # ESTR
  table_visible_estr <- reactiveVal(FALSE)
  filtered_estr_data <- reactive({
    req(input$start_date_estr, input$end_date_estr)
    euro_rate_data_with_date %>%
      filter(obstime_date >= input$start_date_estr & obstime_date <= input$end_date_estr)
  })

  # AAA Bond Yield
  table_visible_gov_bond <- reactiveVal(FALSE)
  filtered_gov_bond_data <- reactive({
    req(input$start_date_gov_bond, input$end_date_gov_bond)
    gov_bond_yield_data_with_date %>%
      filter(obstime_date >= input$start_date_gov_bond & obstime_date <= input$end_date_gov_bond)
  })

  # Loans to Corporations
  table_visible_loans_corp <- reactiveVal(FALSE)
  filtered_loans_corp_data <- reactive({
    req(input$start_date_loans_corp, input$end_date_loans_corp)
    loans_to_corp_data_with_date %>%
      filter(obstime_date >= input$start_date_loans_corp & obstime_date <= input$end_date_loans_corp)
  })

  # Loans to Households
  table_visible_loans_households <- reactiveVal(FALSE)
  filtered_loans_households_data <- reactive({
    req(input$start_date_loans_households, input$end_date_loans_households)
    loans_to_households_data_with_date %>%
      filter(obstime_date >= input$start_date_loans_households & obstime_date <= input$end_date_loans_households)
  })

  # Cost Borrowing House
  table_visible_cost_house <- reactiveVal(FALSE)
  filtered_cost_house_data <- reactive({
    req(input$start_date_cost_house, input$end_date_cost_house)
    cost_borrowing_house_data_with_date %>%
      filter(obstime_date >= input$start_date_cost_house & obstime_date <= input$end_date_cost_house)
  })

  # Cost Borrowing Corp
  table_visible_cost_corp <- reactiveVal(FALSE)
  filtered_cost_corp_data <- reactive({
    req(input$start_date_cost_corp, input$end_date_cost_corp)
    cost_borrowing_corp_data_with_date %>%
      filter(obstime_date >= input$start_date_cost_corp & obstime_date <= input$end_date_cost_corp)
  })


  # Tab 2: Euro Short Term Rate ----------
  latest_estr_value <- euro_rate_data[.N, obsvalue]
  previous_estr_value <- euro_rate_data[.N - 1, obsvalue]
  latest_estr_date <- format(euro_rate_data[.N, obstime], "%d %B %Y")
  previous_estr_date <- format(euro_rate_data[.N - 1, obstime], "%d %B %Y")
  estr_diff <- latest_estr_value - previous_estr_value
  estr_arrow <- get_arrow(estr_diff)

  output$estr_header <- renderUI({
    HTML(paste0("Euro Short-Term Rate (€STR)<br>
                 <span style='font-size: 12px; color: gray;'>", latest_estr_date, "</span><br>
                 <span style='font-size: 16px; color: black;'><b>", format(latest_estr_value, nsmall = 3), "%</b> ", estr_arrow, "</span>"))
  })

  observeEvent(input$show_estr_modal, {
    updateDateInput(session, "start_date_estr", value = min(euro_rate_data_with_date$obstime_date))
    updateDateInput(session, "end_date_estr", value = max(euro_rate_data_with_date$obstime_date))

    showModal(modalDialog(
      title = "Euro short-term rate - Volume-weighted trimmed mean rate, Daily - businessweek",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_estr", "Start Date"),
          dateInput("end_date_estr", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_estr", "1 Year"),
          actionButton("three_years_estr", "3 Years"),
          actionButton("five_years_estr", "5 Years"),
          actionButton("full_range_estr", "Full Range")
        ),
        dygraphOutput("estr_dygraph", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_estr", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_estr % 2 == 1",
          DTOutput("estr_table")
        )
      ),
      size = "xl", easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_estr, {
    updateDateInput(session, "start_date_estr", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_estr", value = Sys.Date())
  })
  observeEvent(input$three_years_estr, {
    updateDateInput(session, "start_date_estr", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_estr", value = Sys.Date())
  })
  observeEvent(input$five_years_estr, {
    updateDateInput(session, "start_date_estr", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_estr", value = Sys.Date())
  })
  observeEvent(input$full_range_estr, {
    updateDateInput(session, "start_date_estr", value = min(euro_rate_data_with_date$obstime_date))
    updateDateInput(session, "end_date_estr", value = max(euro_rate_data_with_date$obstime_date))
  })

  observeEvent(input$toggle_table_estr, {
    table_visible_estr(!table_visible_estr())
  })

  output$estr_dygraph <- renderDygraph({
    req(filtered_estr_data())
    xts_data <- xts(filtered_estr_data()$obsvalue, order.by = filtered_estr_data()$obstime_date)
    dygraph(xts_data) %>%
      dyAxis("y",
        label = "Percent",
        axisLabelFormatter = "function(d) { return d.toFixed(3); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE)
  })

  output$estr_table <- renderDT({
    req(table_visible_estr())
    filtered_estr_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(
        colnames = c("Date", "Value (Percent)"),
        options = list(pageLength = 10)
      ) %>%
      formatDate(
        "obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short", day = "2-digit"))
      ) %>%
      formatRound("obsvalue", digits = 3)
  })

  observeEvent(input$info_estr, {
    showModal(modalDialog(
      title = "Euro Short-Term Rate (€STR) Quick Info",
      "Euro short-term rate; Volume-weighted trimmed mean rate; Unsecured; Overnight; Borrowing; Financial corporations",
      easyClose = TRUE
    ))
  })

  output$estr_plot <- renderPlotly({
    plot_ly(euro_rate_data[(.N - 11):.N, ],
      x = ~obstime, y = ~obsvalue, type = "scatter", mode = "lines",
      line = list(color = "black")
    ) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Percent"))
  })

  output$download_estr <- downloadHandler(
    filename = function() {
      "estr_data.csv"
    },
    content = function(file) {
      write.csv(euro_rate_data, file, row.names = FALSE)
    }
  )

  # Tab 2: AAA-rated ten-year bond yield -------
  latest_gov_bond_value <- gov_bond_yield_data[.N, obsvalue]
  previous_gov_bond_value <- gov_bond_yield_data[.N - 1, obsvalue]
  latest_gov_bond_date <- format(gov_bond_yield_data[.N, obstime], "%d %B %Y")
  gov_bond_diff <- latest_gov_bond_value - gov_bond_yield_data[1, obsvalue]
  gov_bond_arrow <- get_arrow(gov_bond_diff)

  output$gov_bond_header <- renderUI({
    HTML(paste0("AAA-Rated Ten-Year Government Bond Yield<br>
                 <span style='font-size: 12px; color: gray;'>", latest_gov_bond_date, "</span><br>
                 <span style='font-size: 16px; color: black;'><b>", round(latest_gov_bond_value, 2) %>% format(., nsmall = 2), "%</b> ", gov_bond_arrow, "</span>"))
  })

  output$gov_bond_plot <- renderPlotly({
    plot_ly(gov_bond_yield_data[(.N - 7):.N, ],
      x = ~obstime, y = ~obsvalue, type = "scatter", mode = "lines",
      line = list(color = "black"),
      hovertemplate = "Date: %{x}<br>Value: %{y:.2f}%<extra></extra>"
    ) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Percent per annum"))
  })

  observeEvent(input$show_gov_bond_modal, {
    updateDateInput(session, "start_date_gov_bond", value = min(gov_bond_yield_data_with_date$obstime_date))
    updateDateInput(session, "end_date_gov_bond", value = max(gov_bond_yield_data_with_date$obstime_date))

    additional_info <- paste0(
      "As of ", format(gov_bond_yield_data[.N, obstime], "%d %B %Y"),
      ", the euro area AAA-rated ten-year bond yield is ",
      round(gov_bond_yield_data[.N, obsvalue], 2), " percent per annum."
    )

    showModal(modalDialog(
      title = "Yield curve spot rate, 10-year maturity ...",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_gov_bond", "Start Date"),
          dateInput("end_date_gov_bond", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_gov_bond", "1 Year"),
          actionButton("three_years_gov_bond", "3 Years"),
          actionButton("five_years_gov_bond", "5 Years"),
          actionButton("full_range_gov_bond", "Full Range")
        ),
        dygraphOutput("gov_bond_dygraph", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_gov_bond", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_gov_bond % 2 == 1",
          DTOutput("gov_bond_table")
        )
      ),
      size = "xl", easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_gov_bond, {
    updateDateInput(session, "start_date_gov_bond", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_gov_bond", value = Sys.Date())
  })
  observeEvent(input$three_years_gov_bond, {
    updateDateInput(session, "start_date_gov_bond", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_gov_bond", value = Sys.Date())
  })
  observeEvent(input$five_years_gov_bond, {
    updateDateInput(session, "start_date_gov_bond", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_gov_bond", value = Sys.Date())
  })
  observeEvent(input$full_range_gov_bond, {
    updateDateInput(session, "start_date_gov_bond", value = min(gov_bond_yield_data_with_date$obstime_date))
    updateDateInput(session, "end_date_gov_bond", value = max(gov_bond_yield_data_with_date$obstime_date))
  })

  observeEvent(input$toggle_table_gov_bond, {
    table_visible_gov_bond(!table_visible_gov_bond())
  })

  output$gov_bond_dygraph <- renderDygraph({
    req(filtered_gov_bond_data())
    xts_data <- xts(filtered_gov_bond_data()$obsvalue, order.by = filtered_gov_bond_data()$obstime_date)
    dygraph(xts_data) %>%
      dyAxis("y",
        label = "Percent per annum",
        axisLabelFormatter = "function(d) { return d.toFixed(3); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE) %>%
      dyLimit(0, color = "black", strokePattern = "solid")
  })

  output$gov_bond_table <- renderDT({
    req(table_visible_gov_bond())
    filtered_gov_bond_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(colnames = c("Period", "Value (Percent per annum)"), options = list(pageLength = 10)) %>%
      formatDate("obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short", day = "2-digit"))
      ) %>%
      formatRound("obsvalue", digits = 6)
  })

  output$download_gov_bond <- downloadHandler(
    filename = function() {
      "ten_year_gov_bond_yield.csv"
    },
    content = function(file) {
      write.csv(gov_bond_yield_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_gov_bond, {
    showModal(modalDialog(
      title = "AAA-Rated Ten-Year Government Bond Yield Quick Info",
      "Yield curve spot rate, 10-year maturity - government bond; nominal; all issuers whose rating is triple A; euro area (changing composition)",
      easyClose = TRUE
    ))
  })

  # Tab 2: Loans to non-financial corporations (monthly) ---------
  latest_loans_corp_value <- loans_to_corp_data[.N, obsvalue]
  previous_loans_corp_value <- loans_to_corp_data[.N - 1, obsvalue]
  latest_loans_corp_date <- format(loans_to_corp_data[.N, obstime], "%B %Y")
  loans_corp_diff <- latest_loans_corp_value - loans_to_corp_data[1, obsvalue]
  loans_corp_arrow <- get_arrow(loans_corp_diff)

  output$loans_to_corp_header <- renderUI({
    HTML(paste0("Loans to Non-Financial Corporations<br>
                 <span style='font-size: 12px; color: gray;'>", latest_loans_corp_date, "</span><br>
                 <span style='font-size: 16px; color: black;'><b>", round(latest_loans_corp_value, 1) %>% format(., nsmall = 1), "%</b> ", loans_corp_arrow, "</span>"))
  })

  output$loans_to_corp_box <- renderUI({
    HTML(paste0(
      "Loans to Non-Financial Corporations<br>",
      "<span style='font-size: 12px; color: gray;'>", latest_loans_corp_date, "</span><br>",
      "<span style='font-size: 16px; color: black;'><b>",
      round(latest_loans_corp_value, 1), "%</b> ", loans_corp_arrow,
      "</span>"
    ))
  })

  observeEvent(input$show_loans_to_corp_modal, {
    updateDateInput(session, "start_date_loans_corp", value = min(loans_to_corp_data_with_date$obstime_date))
    updateDateInput(session, "end_date_loans_corp", value = max(loans_to_corp_data_with_date$obstime_date))

    showModal(modalDialog(
      title = "...loans vis-a-vis euro area NFCs ...",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_loans_corp", "Start Date"),
          dateInput("end_date_loans_corp", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_loans_corp", "1 Year"),
          actionButton("three_years_loans_corp", "3 Years"),
          actionButton("five_years_loans_corp", "5 Years"),
          actionButton("full_range_loans_corp", "Full Range")
        ),
        dygraphOutput("loans_to_corp_dygraph", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_loans_corp", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_loans_corp % 2 == 1",
          DTOutput("loans_to_corp_table")
        )
      ),
      size = "xl", easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_loans_corp, {
    updateDateInput(session, "start_date_loans_corp", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_loans_corp", value = Sys.Date())
  })
  observeEvent(input$three_years_loans_corp, {
    updateDateInput(session, "start_date_loans_corp", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_loans_corp", value = Sys.Date())
  })
  observeEvent(input$five_years_loans_corp, {
    updateDateInput(session, "start_date_loans_corp", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_loans_corp", value = Sys.Date())
  })
  observeEvent(input$full_range_loans_corp, {
    updateDateInput(session, "start_date_loans_corp", value = min(loans_to_corp_data_with_date$obstime_date))
    updateDateInput(session, "end_date_loans_corp", value = max(loans_to_corp_data_with_date$obstime_date))
  })

  observeEvent(input$toggle_table_loans_corp, {
    table_visible_loans_corp(!table_visible_loans_corp())
  })

  output$loans_to_corp_dygraph <- renderDygraph({
    req(filtered_loans_corp_data())
    xts_data <- xts(filtered_loans_corp_data()$obsvalue, order.by = filtered_loans_corp_data()$obstime_date)
    dygraph(xts_data) %>%
      dyAxis("y",
        label = "Percent change",
        axisLabelFormatter = "function(d) { return d.toFixed(1); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE) %>%
      dyLimit(0, color = "black", strokePattern = "solid")
  })

  output$loans_to_corp_table <- renderDT({
    req(table_visible_loans_corp())
    filtered_loans_corp_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(colnames = c("Period", "Value (Percentage change)"), options = list(pageLength = 10)) %>%
      formatDate("obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short"))
      ) %>%
      formatRound("obsvalue", digits = 1)
  })


  output$download_loans_to_corp <- downloadHandler(
    filename = function() {
      "loans_to_corporations.csv"
    },
    content = function(file) {
      write.csv(loans_to_corp_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_loans_to_corp, {
    showModal(modalDialog(
      title = "Loans to Non-Financial Corporations Quick Info",
      "Total adjusted loans, reported by MFls in the euro area; index",
      easyClose = TRUE
    ))
  })

  # Tab 2: Loans to households ----------
  latest_loans_hh_value <- loans_to_households_data[.N, obsvalue]
  previous_loans_hh_value <- loans_to_households_data[.N - 1, obsvalue]
  latest_loans_hh_date <- format(loans_to_households_data[.N, obstime], "%B %Y")
  loans_hh_diff <- latest_loans_hh_value - previous_loans_hh_value
  loans_hh_arrow <- get_arrow(loans_hh_diff)

  output$loans_to_households_header <- renderUI({
    HTML(paste0("Loans to Households<br>
                 <span style='font-size: 12px; color: gray;'>", latest_loans_hh_date, "</span><br>
                 <span style='font-size: 16px; color: black;'><b>", round(latest_loans_hh_value, 1), "%</b> ", loans_hh_arrow, "</span>"))
  })

  output$loans_to_households_box <- renderUI({
    HTML(paste0(
      "Loans to Households<br>",
      "<span style='font-size: 12px; color: gray;'>", latest_loans_hh_date, "</span><br>",
      "<span style='font-size: 16px; color: black;'><b>",
      round(latest_loans_hh_value, 1), "%</b> ", loans_hh_arrow,
      "</span>"
    ))
  })

  observeEvent(input$show_loans_to_households_modal, {
    updateDateInput(session, "start_date_loans_households", value = min(loans_to_households_data_with_date$obstime_date))
    updateDateInput(session, "end_date_loans_households", value = max(loans_to_households_data_with_date$obstime_date))

    showModal(modalDialog(
      title = "Adjusted loans vis-a-vis euro area households ...",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_loans_households", "Start Date"),
          dateInput("end_date_loans_households", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_loans_households", "1 Year"),
          actionButton("three_years_loans_households", "3 Years"),
          actionButton("five_years_loans_households", "5 Years"),
          actionButton("full_range_loans_households", "Full Range")
        ),
        dygraphOutput("loans_to_households_dygraph", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_loans_households", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_loans_households % 2 == 1",
          DTOutput("loans_to_households_table")
        )
      ),
      size = "xl", easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_loans_households, {
    updateDateInput(session, "start_date_loans_households", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_loans_households", value = Sys.Date())
  })
  observeEvent(input$three_years_loans_households, {
    updateDateInput(session, "start_date_loans_households", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_loans_households", value = Sys.Date())
  })
  observeEvent(input$five_years_loans_households, {
    updateDateInput(session, "start_date_loans_households", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_loans_households", value = Sys.Date())
  })
  observeEvent(input$full_range_loans_households, {
    updateDateInput(session, "start_date_loans_households", value = min(loans_to_households_data_with_date$obstime_date))
    updateDateInput(session, "end_date_loans_households", value = max(loans_to_households_data_with_date$obstime_date))
  })

  observeEvent(input$toggle_table_loans_households, {
    table_visible_loans_households(!table_visible_loans_households())
  })

  output$loans_to_households_dygraph <- renderDygraph({
    req(filtered_loans_households_data())
    xts_data <- xts(filtered_loans_households_data()$obsvalue, order.by = filtered_loans_households_data()$obstime_date)
    dygraph(xts_data) %>%
      dyAxis("y",
        label = "Percent change",
        axisLabelFormatter = "function(d) { return d.toFixed(1); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE) %>%
      dyLimit(0, color = "black", strokePattern = "solid")
  })

  output$loans_to_households_table <- renderDT({
    req(table_visible_loans_households())
    filtered_loans_households_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(colnames = c("Period", "Value (Percentage change)"), options = list(pageLength = 10)) %>%
      formatDate("obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short"))
      ) %>%
      formatRound("obsvalue", digits = 1)
  })


  output$download_loans_to_households <- downloadHandler(
    filename = function() {
      "loans_to_households.csv"
    },
    content = function(file) {
      write.csv(loans_to_households_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_loans_to_households, {
    showModal(modalDialog(
      title = "Loans to Households Quick Info",
      "Total adjusted loans, reported by MFls in the euro area; index",
      easyClose = TRUE
    ))
  })

  # Tab 2: Cost of borrowing for house purchase (monthly) ---------
  latest_cost_borrow_house_value <- cost_borrowing_house_data[.N, obsvalue]
  previous_cost_borrow_house_value <- cost_borrowing_house_data[.N - 1, obsvalue]
  latest_cost_borrow_house_date <- format(cost_borrowing_house_data[.N, obstime], "%B %Y")
  cbh_diff <- latest_cost_borrow_house_value - cost_borrowing_house_data[1, obsvalue]
  cbh_arrow <- get_arrow(cbh_diff)

  output$cost_borrowing_house_header <- renderUI({
    HTML(paste0("Cost of Borrowing for House Purchase<br>
                 <span style='font-size: 12px; color: gray;'>", latest_cost_borrow_house_date, "</span><br>
                 <span style='font-size: 16px; color: black;'><b>", round(latest_cost_borrow_house_value, 2), "%</b> ", cbh_arrow, "</span>"))
  })

  output$cost_borrowing_house_box <- renderUI({
    HTML(paste0(
      "Cost of Borrowing for House Purchase<br>",
      "<span style='font-size: 12px; color: gray;'>", latest_cost_borrow_house_date, "</span><br>",
      "<span style='font-size: 16px; color: black;'><b>",
      round(latest_cost_borrow_house_value, 2), "%</b> ", cbh_arrow,
      "</span>"
    ))
  })

  observeEvent(input$show_cost_borrowing_house_modal, {
    updateDateInput(session, "start_date_cost_house", value = min(cost_borrowing_house_data_with_date$obstime_date))
    updateDateInput(session, "end_date_cost_house", value = max(cost_borrowing_house_data_with_date$obstime_date))

    showModal(modalDialog(
      title = "Cost of borrowing for households for house purchase ...",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_cost_house", "Start Date"),
          dateInput("end_date_cost_house", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_cost_house", "1 Year"),
          actionButton("three_years_cost_house", "3 Years"),
          actionButton("five_years_cost_house", "5 Years"),
          actionButton("full_range_cost_house", "Full Range")
        ),
        dygraphOutput("cost_borrowing_house_dygraph", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_cost_house", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_cost_house % 2 == 1",
          DTOutput("cost_borrowing_house_table")
        )
      ),
      size = "xl", easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_cost_house, {
    updateDateInput(session, "start_date_cost_house", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_cost_house", value = Sys.Date())
  })
  observeEvent(input$three_years_cost_house, {
    updateDateInput(session, "start_date_cost_house", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_cost_house", value = Sys.Date())
  })
  observeEvent(input$five_years_cost_house, {
    updateDateInput(session, "start_date_cost_house", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_cost_house", value = Sys.Date())
  })
  observeEvent(input$full_range_cost_house, {
    updateDateInput(session, "start_date_cost_house", value = min(cost_borrowing_house_data_with_date$obstime_date))
    updateDateInput(session, "end_date_cost_house", value = max(cost_borrowing_house_data_with_date$obstime_date))
  })

  observeEvent(input$toggle_table_cost_house, {
    table_visible_cost_house(!table_visible_cost_house())
  })

  output$cost_borrowing_house_dygraph <- renderDygraph({
    req(filtered_cost_house_data())
    xts_data <- xts(filtered_cost_house_data()$obsvalue, order.by = filtered_cost_house_data()$obstime_date)
    dygraph(xts_data) %>%
      dyAxis("y",
        label = "Percent per annum",
        axisLabelFormatter = "function(d) { return d.toFixed(4); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE)
  })

  output$cost_borrowing_house_table <- renderDT({
    req(table_visible_cost_house())
    filtered_cost_house_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(colnames = c("Period", "Value (Percent per annum)"), options = list(pageLength = 10)) %>%
      formatDate("obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short"))
      ) %>%
      formatRound("obsvalue", digits = 4)
  })


  output$download_cost_borrowing_house <- downloadHandler(
    filename = function() {
      "cost_borrowing_house_purchase.csv"
    },
    content = function(file) {
      write.csv(cost_borrowing_house_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_cost_borrowing_house, {
    showModal(modalDialog(
      title = "Cost of Borrowing for House Purchase Quick Info",
      "Percent per annum",
      easyClose = TRUE
    ))
  })

  # Tab 2: Cost of borrowing for corporations ----------
  latest_cost_borrow_corp_value <- cost_borrowing_corp_data[.N, obsvalue]
  previous_cost_borrow_corp_value <- cost_borrowing_corp_data[.N - 1, obsvalue]
  latest_cost_borrow_corp_date <- format(cost_borrowing_corp_data[.N, obstime], "%B %Y")
  cbc_diff <- latest_cost_borrow_corp_value - cost_borrowing_corp_data[1, obsvalue]
  cbc_arrow <- get_arrow(cbc_diff)

  output$cost_borrowing_corp_header <- renderUI({
    HTML(paste0("Cost of Borrowing for Corporations<br>
                 <span style='font-size: 12px; color: gray;'>", latest_cost_borrow_corp_date, "</span><br>
                 <span style='font-size: 16px; color: black;'><b>", round(latest_cost_borrow_corp_value, 2), "%</b> ", cbc_arrow, "</span>"))
  })

  output$cost_borrowing_corp_box <- renderUI({
    HTML(paste0(
      "Cost of Borrowing for Corporations<br>",
      "<span style='font-size: 12px; color: gray;'>", latest_cost_borrow_corp_date, "</span><br>",
      "<span style='font-size: 16px; color: black;'><b>",
      round(latest_cost_borrow_corp_value, 2), "%</b> ", cbc_arrow,
      "</span>"
    ))
  })

  observeEvent(input$show_cost_borrowing_corp_modal, {
    updateDateInput(session, "start_date_cost_corp", value = min(cost_borrowing_corp_data_with_date$obstime_date))
    updateDateInput(session, "end_date_cost_corp", value = max(cost_borrowing_corp_data_with_date$obstime_date))

    showModal(modalDialog(
      title = "Cost of borrowing for corporations - euro area...",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_cost_corp", "Start Date"),
          dateInput("end_date_cost_corp", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_cost_corp", "1 Year"),
          actionButton("three_years_cost_corp", "3 Years"),
          actionButton("five_years_cost_corp", "5 Years"),
          actionButton("full_range_cost_corp", "Full Range")
        ),
        dygraphOutput("cost_borrowing_corp_dygraph", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_cost_corp", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_cost_corp % 2 == 1",
          DTOutput("cost_borrowing_corp_table")
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_cost_corp, {
    updateDateInput(session, "start_date_cost_corp", value = Sys.Date() - years(1))
    updateDateInput(session, "end_date_cost_corp", value = Sys.Date())
  })
  observeEvent(input$three_years_cost_corp, {
    updateDateInput(session, "start_date_cost_corp", value = Sys.Date() - years(3))
    updateDateInput(session, "end_date_cost_corp", value = Sys.Date())
  })
  observeEvent(input$five_years_cost_corp, {
    updateDateInput(session, "start_date_cost_corp", value = Sys.Date() - years(5))
    updateDateInput(session, "end_date_cost_corp", value = Sys.Date())
  })
  observeEvent(input$full_range_cost_corp, {
    updateDateInput(session, "start_date_cost_corp", value = min(cost_borrowing_corp_data_with_date$obstime_date))
    updateDateInput(session, "end_date_cost_corp", value = max(cost_borrowing_corp_data_with_date$obstime_date))
  })

  observeEvent(input$toggle_table_cost_corp, {
    table_visible_cost_corp(!table_visible_cost_corp())
  })

  output$cost_borrowing_corp_dygraph <- renderDygraph({
    req(filtered_cost_corp_data())
    xts_data <- xts(filtered_cost_corp_data()$obsvalue, order.by = filtered_cost_corp_data()$obstime_date)
    dygraph(xts_data) %>%
      dyAxis("y",
        label = "Percent per annum",
        axisLabelFormatter = "function(d) { return d.toFixed(4); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE)
  })

  output$cost_borrowing_corp_table <- renderDT({
    req(table_visible_cost_corp())
    filtered_cost_corp_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(colnames = c("Period", "Value (Percent per annum)"), options = list(pageLength = 10)) %>%
      formatDate("obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short"))
      ) %>%
      formatRound("obsvalue", digits = 4)
  })


  output$download_cost_borrowing_corp <- downloadHandler(
    filename = function() {
      "cost_borrowing_corporations.csv"
    },
    content = function(file) {
      write.csv(cost_borrowing_corp_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_cost_borrowing_corp, {
    showModal(modalDialog(
      title = "Cost of Borrowing for Corporations Quick Info",
      "Percent per annum",
      easyClose = TRUE
    ))
  })


  # TAB 3 REACTIVES ----

  # 1) Current Account => monthly => as.Date is straightforward
  current_account_data_with_date <- current_account_data %>%
    mutate(obstime_date = as.Date(obstime))

  table_visible_cab <- reactiveVal(FALSE)
  filtered_cab_data <- reactive({
    req(input$start_date_cab, input$end_date_cab)
    current_account_data_with_date %>%
      filter(obstime_date >= input$start_date_cab & obstime_date <= input$end_date_cab)
  })

  # 2) Direct Investment => monthly as well
  direct_investment_data_with_date <- direct_investment_data %>%
    mutate(obstime_date = as.Date(obstime))

  table_visible_direct_invest <- reactiveVal(FALSE)
  filtered_direct_invest_data <- reactive({
    req(input$start_date_direct_invest, input$end_date_direct_invest)
    direct_investment_data_with_date %>%
      filter(obstime_date >= input$start_date_direct_invest & obstime_date <= input$end_date_direct_invest)
  })

  # 3) Portfolio Investment => monthly
  portfolio_investment_data_with_date <- portfolio_investment_data %>%
    mutate(obstime_date = as.Date(obstime))

  table_visible_portfolio <- reactiveVal(FALSE)
  filtered_portfolio_data <- reactive({
    req(input$start_date_portfolio, input$end_date_portfolio)
    portfolio_investment_data_with_date %>%
      filter(obstime_date >= input$start_date_portfolio & obstime_date <= input$end_date_portfolio)
  })

  # 4) Real Effective Exchange Rate => monthly
  real_effective_data_with_date <- real_effective_data %>%
    mutate(obstime_date = as.Date(obstime))

  table_visible_reer <- reactiveVal(FALSE)
  filtered_reer_data <- reactive({
    req(input$start_date_reer, input$end_date_reer)
    real_effective_data_with_date %>%
      filter(obstime_date >= input$start_date_reer & obstime_date <= input$end_date_reer)
  })

  # 5) Reserve Assets => monthly
  reserve_assets_data_with_date <- reserve_assets_data %>%
    mutate(obstime_date = as.Date(obstime))

  table_visible_reserve_assets <- reactiveVal(FALSE)
  filtered_reserve_assets_data <- reactive({
    req(input$start_date_reserve_assets, input$end_date_reserve_assets)
    reserve_assets_data_with_date %>%
      filter(obstime_date >= input$start_date_reserve_assets & obstime_date <= input$end_date_reserve_assets)
  })

  # 6) Nominal Effective => monthly
  nominal_effective_data_with_date <- nominal_effective_data %>%
    mutate(obstime_date = as.Date(obstime))

  table_visible_nominal_effective <- reactiveVal(FALSE)
  filtered_nominal_effective_data <- reactive({
    req(input$start_date_nominal_effective, input$end_date_nominal_effective)
    nominal_effective_data_with_date %>%
      filter(obstime_date >= input$start_date_nominal_effective & obstime_date <= input$end_date_nominal_effective)
  })


  # Tab 3: Current Account Balance ----------
  latest_cab_value <- current_account_data[.N, obsvalue_billions]
  previous_cab_value <- current_account_data[.N - 1, obsvalue_billions]
  latest_cab_date <- format(current_account_data[.N, obstime], "%B %Y")
  cab_diff <- latest_cab_value - previous_cab_value
  cab_arrow <- get_arrow(cab_diff)

  output$cab_header <- renderUI({
    HTML(paste0(
      "Current Account Balance<br>",
      "<span style='font-size: 12px; color: gray;'>", latest_cab_date, "</span><br>",
      "<span style='font-size: 16px; color: black;'><b>",
      round(latest_cab_value, 1), " EUR bln</b> ", cab_arrow,
      "</span>"
    ))
  })

  output$cab_plot <- renderPlotly({
    req(current_account_data$obsvalue_billions)
    plot_ly(current_account_data[(.N - 11):.N, ],
      x = ~obstime, y = ~obsvalue_billions,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(
          title = "Billions of Euro",
          tickformat = ".1f"
        )
      )
  })
  
  observeEvent(input$show_cab_modal, {
    updateDateInput(session, "start_date_cab", value = min(current_account_data_with_date$obstime_date))
    updateDateInput(session, "end_date_cab", value = max(current_account_data_with_date$obstime_date))

    showModal(modalDialog(
      title = paste0("Current account, Balance, Euro area 20 (fixed composition), Monthly"),
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_cab", "Start Date"),
          dateInput("end_date_cab", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_cab", "1 Year"),
          actionButton("three_years_cab", "3 Years"),
          actionButton("five_years_cab", "5 Years"),
          actionButton("full_range_cab", "Full Range")
        ),
        dygraphOutput("cab_dygraph", height = "300px"),
        div(style = "margin: 20px 0;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_cab", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_cab % 2 == 1",
          DTOutput("cab_table")
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_cab, {
    updateDateInput(session, "start_date_cab", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_cab", value = Sys.Date())
  })
  observeEvent(input$three_years_cab, {
    updateDateInput(session, "start_date_cab", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_cab", value = Sys.Date())
  })
  observeEvent(input$five_years_cab, {
    updateDateInput(session, "start_date_cab", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_cab", value = Sys.Date())
  })
  observeEvent(input$full_range_cab, {
    updateDateInput(session, "start_date_cab", value = min(current_account_data_with_date$obstime_date))
    updateDateInput(session, "end_date_cab", value = max(current_account_data_with_date$obstime_date))
  })

  observeEvent(input$toggle_table_cab, {
    table_visible_cab(!table_visible_cab())
  })

  output$cab_dygraph <- renderDygraph({
    req(filtered_cab_data())
    cab_dy_data <- xts(filtered_cab_data()$obsvalue_billions,
      order.by = filtered_cab_data()$obstime_date
    )
    dygraph(cab_dy_data) %>%
      dyAxis("y",
        label = "Billions of EUR",
        axisLabelFormatter = "function(d) {return d.toFixed(1);}"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyLegend(show = "follow") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = TRUE
      ) %>%
      dyLimit(0, color = "black", strokePattern = "solid")
  })

  output$cab_table <- renderDT({
    req(table_visible_cab())
    filtered_cab_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue_billions) %>%
      datatable(
        colnames = c("Period", "Value (EUR bln)"),
        options = list(pageLength = 10)
      ) %>%
      formatDate(
        "obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short"))
      ) %>%
      formatRound("obsvalue_billions", digits = 2)
  })

  output$download_cab <- downloadHandler(
    filename = function() {
      "current_account_balance.csv"
    },
    content = function(file) {
      write.csv(current_account_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_cab, {
    showModal(modalDialog(
      title = "Current Account Balance Quick Info",
      "Current account balance (credits minus debits), transactions vis-à-vis the rest of the World; not seasonally or working day-adjusted; BPM6 standards.",
      easyClose = TRUE
    ))
  })

  # Tab 3: Direct Investment Balance ----------
  latest_di_value <- direct_investment_data[.N, obsvalue_billions]
  previous_di_value <- direct_investment_data[.N - 1, obsvalue_billions]
  latest_di_date <- format(direct_investment_data[.N, obstime], "%B %Y")
  di_diff <- latest_di_value - previous_di_value
  di_arrow <- get_arrow(di_diff)

  output$direct_invest_header <- renderUI({
    HTML(paste0(
      "Direct Investment Balance<br>",
      "<span style='font-size: 12px; color: gray;'>", latest_di_date, "</span><br>",
      "<span style='font-size: 16px; color: black;'><b>",
      round(latest_di_value, 1), " EUR bln</b> ", di_arrow,
      "</span>"
    ))
  })

  output$direct_invest_plot <- renderPlotly({
    req(direct_investment_data$obsvalue_billions)
    plot_ly(direct_investment_data[(.N - 11):.N, ],
      x = ~obstime, y = ~obsvalue_billions,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Billions of Euro")
      )
  })

  observeEvent(input$show_direct_invest_modal, {
    updateDateInput(session, "start_date_direct_invest",
      value = min(direct_investment_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_direct_invest",
      value = max(direct_investment_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = paste0("Financial account, Direct Investment, Net, Euro area 20 ..."),
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_direct_invest", "Start Date"),
          dateInput("end_date_direct_invest", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_direct_invest", "1 Year"),
          actionButton("three_years_direct_invest", "3 Years"),
          actionButton("five_years_direct_invest", "5 Years"),
          actionButton("full_range_direct_invest", "Full Range")
        ),
        dygraphOutput("direct_invest_dygraph", height = "300px"),
        div(style = "margin: 20px 0;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_direct_invest", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_direct_invest % 2 == 1",
          DTOutput("direct_invest_table")
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_direct_invest, {
    updateDateInput(session, "start_date_direct_invest", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_direct_invest", value = Sys.Date())
  })
  observeEvent(input$three_years_direct_invest, {
    updateDateInput(session, "start_date_direct_invest", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_direct_invest", value = Sys.Date())
  })
  observeEvent(input$five_years_direct_invest, {
    updateDateInput(session, "start_date_direct_invest", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_direct_invest", value = Sys.Date())
  })
  observeEvent(input$full_range_direct_invest, {
    updateDateInput(session, "start_date_direct_invest",
      value = min(direct_investment_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_direct_invest",
      value = max(direct_investment_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_direct_invest, {
    table_visible_direct_invest(!table_visible_direct_invest())
  })

  output$direct_invest_dygraph <- renderDygraph({
    req(filtered_direct_invest_data())
    di_dy_data <- xts(filtered_direct_invest_data()$obsvalue_billions,
      order.by = filtered_direct_invest_data()$obstime_date
    )
    dygraph(di_dy_data) %>%
      dyAxis("y",
        label = "Billions of Euro",
        axisLabelFormatter = "function(d) { return d.toFixed(1); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = TRUE
      ) %>%
      dyLimit(0, color = "black", strokePattern = "solid")
  })

  output$direct_invest_table <- renderDT({
    req(table_visible_direct_invest())
    filtered_direct_invest_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue_billions) %>%
      datatable(
        colnames = c("Period", "Value (EUR bln)"),
        options = list(pageLength = 10)
      ) %>%
      formatDate(
        "obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short"))
      ) %>%
      formatRound("obsvalue_billions", digits = 2)
  })

  output$download_direct_invest <- downloadHandler(
    filename = function() {
      "direct_investment_balance.csv"
    },
    content = function(file) {
      write.csv(direct_investment_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_direct_invest, {
    showModal(modalDialog(
      title = "Direct Investment Quick Info",
      "Net financial account, direct investment (assets minus liabilities, transactions the rest of the World; not seasonally or working day-adjusted; BPM6 standards.",
      easyClose = TRUE
    ))
  })

  # Tab 3: Reserve Assets -------
  latest_reserve_value <- reserve_assets_data[.N, obsvalue_billions]
  previous_reserve_value <- reserve_assets_data[.N - 1, obsvalue_billions]
  latest_reserve_date <- format(reserve_assets_data[.N, obstime], "%B %Y")
  reserve_diff <- latest_reserve_value - previous_reserve_value
  reserve_arrow <- get_arrow(reserve_diff)

  output$reserve_assets_box <- renderUI({
    HTML(paste0(
      "Reserve Assets<br>",
      "<span style='font-size: 12px; color: gray;'>", latest_reserve_date, "</span><br>",
      "<span style='font-size: 16px; color: black;'><b>",
      formatC(latest_reserve_value, format = "f", big.mark = ",", decimal.mark = ".", digits = 1), " EUR bln</b> ", reserve_arrow,
      "</span>"
    ))
  })

  observeEvent(input$show_reserve_assets_modal, {
    updateDateInput(session, "start_date_reserve_assets",
      value = min(reserve_assets_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_reserve_assets",
      value = max(reserve_assets_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = "Total financial assets/liabilities, Euro area ...",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_reserve_assets", "Start Date"),
          dateInput("end_date_reserve_assets", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_reserve_assets", "1 Year"),
          actionButton("three_years_reserve_assets", "3 Years"),
          actionButton("five_years_reserve_assets", "5 Years"),
          actionButton("full_range_reserve_assets", "Full Range")
        ),
        dygraphOutput("reserve_assets_dygraph", height = "300px"),
        div(style = "margin: 20px 0;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_reserve_assets", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_reserve_assets % 2 == 1",
          DTOutput("reserve_assets_table")
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_reserve_assets, {
    updateDateInput(session, "start_date_reserve_assets", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_reserve_assets", value = Sys.Date())
  })
  observeEvent(input$three_years_reserve_assets, {
    updateDateInput(session, "start_date_reserve_assets", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_reserve_assets", value = Sys.Date())
  })
  observeEvent(input$five_years_reserve_assets, {
    updateDateInput(session, "start_date_reserve_assets", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_reserve_assets", value = Sys.Date())
  })
  observeEvent(input$full_range_reserve_assets, {
    updateDateInput(session, "start_date_reserve_assets",
      value = min(reserve_assets_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_reserve_assets",
      value = max(reserve_assets_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_reserve_assets, {
    table_visible_reserve_assets(!table_visible_reserve_assets())
  })

  # Reserve Assets Dygraph
  output$reserve_assets_dygraph <- renderDygraph({
    req(filtered_reserve_assets_data()$obsvalue_billions)
    reserve_dy_data <- xts(filtered_reserve_assets_data()$obsvalue_billions,
      order.by = filtered_reserve_assets_data()$obstime_date
    )
    dygraph(reserve_dy_data) %>%
      dyAxis("y",
        label = "Billions of Euro",
        axisLabelFormatter = "function(d) { return d.toFixed(1); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = TRUE
      )
  })

  output$reserve_assets_table <- renderDT({
    req(table_visible_reserve_assets())
    filtered_reserve_assets_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue_billions) %>%
      datatable(
        colnames = c("Period", "Value (EUR bln)"),
        options = list(pageLength = 10)
      ) %>%
      formatDate(
        "obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short"))
      ) %>%
      formatRound("obsvalue_billions", digits = 1)
  })


  output$download_reserve_assets <- downloadHandler(
    filename = function() {
      "reserve_assets.csv"
    },
    content = function(file) {
      write.csv(reserve_assets_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_reserve_assets, {
    showModal(modalDialog(
      title = "Reserve Assets Quick Info",
      "End of period position.",
      easyClose = TRUE
    ))
  })


  # Tab 3: Portfolio Investment Balance -------
  latest_portfolio_value <- portfolio_investment_data[.N, obsvalue_billions]
  previous_portfolio_value <- portfolio_investment_data[.N - 1, obsvalue_billions]
  latest_portfolio_date <- format(portfolio_investment_data[.N, obstime], "%B %Y")
  portfolio_diff <- latest_portfolio_value - previous_portfolio_value
  portfolio_arrow <- get_arrow(portfolio_diff)

  output$portfolio_box <- renderUI({
    HTML(paste0(
      "Portfolio Investment Balance<br>",
      "<span style='font-size: 12px; color: gray;'>", latest_portfolio_date, "</span><br>",
      "<span style='font-size: 16px; color: black;'><b>",
      round(latest_portfolio_value, 1), " EUR bln</b> ", portfolio_arrow,
      "</span>"
    ))
  })

  observeEvent(input$show_portfolio_modal, {
    updateDateInput(session, "start_date_portfolio",
      value = min(portfolio_investment_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_portfolio",
      value = max(portfolio_investment_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = paste0("Financial account, Portfolio Investment, Net, Euro area 20 ..."),
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_portfolio", "Start Date"),
          dateInput("end_date_portfolio", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_portfolio", "1 Year"),
          actionButton("three_years_portfolio", "3 Years"),
          actionButton("five_years_portfolio", "5 Years"),
          actionButton("full_range_portfolio", "Full Range")
        ),
        dygraphOutput("portfolio_dygraph", height = "300px"),
        div(style = "margin: 20px 0;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_portfolio", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_portfolio % 2 == 1",
          DTOutput("portfolio_table")
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_portfolio, {
    updateDateInput(session, "start_date_portfolio", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_portfolio", value = Sys.Date())
  })
  observeEvent(input$three_years_portfolio, {
    updateDateInput(session, "start_date_portfolio", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_portfolio", value = Sys.Date())
  })
  observeEvent(input$five_years_portfolio, {
    updateDateInput(session, "start_date_portfolio", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_portfolio", value = Sys.Date())
  })
  observeEvent(input$full_range_portfolio, {
    updateDateInput(session, "start_date_portfolio",
      value = min(portfolio_investment_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_portfolio",
      value = max(portfolio_investment_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_portfolio, {
    table_visible_portfolio(!table_visible_portfolio())
  })

  output$portfolio_dygraph <- renderDygraph({
    req(filtered_portfolio_data()$obsvalue_billions)
    portfolio_dy_data <- xts(filtered_portfolio_data()$obsvalue_billions,
      order.by = filtered_portfolio_data()$obstime_date
    )
    dygraph(portfolio_dy_data) %>%
      dyAxis("y",
        label = "Billions of Euro",
        axisLabelFormatter = "function(d) { return d.toFixed(2); }",
        valueFormatter = "function(d) { return d.toFixed(2); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = TRUE
      ) %>%
      dyLimit(0, color = "black", strokePattern = "solid")
  })

  output$portfolio_table <- renderDT({
    req(table_visible_portfolio())
    filtered_portfolio_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue_billions) %>%
      datatable(
        colnames = c("Period", "Value (EUR bln)"),
        options = list(pageLength = 10)
      ) %>%
      formatDate(
        "obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short"))
      ) %>%
      formatRound("obsvalue_billions", digits = 2)
  })

  output$download_portfolio <- downloadHandler(
    filename = function() {
      "portfolio_investment_balance.csv"
    },
    content = function(file) {
      write.csv(portfolio_investment_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_portfolio, {
    showModal(modalDialog(
      title = "Portfolio Investment Quick Info",
      "",
      easyClose = TRUE
    ))
  })

  # Tab 3: Real Effective Exchange Rate ------
  latest_reer_value <- real_effective_data[.N, obsvalue]
  previous_reer_value <- real_effective_data[.N - 1, obsvalue]
  latest_reer_date <- format(real_effective_data[.N, obstime], "%B %Y")
  reer_diff <- latest_reer_value - previous_reer_value
  reer_arrow <- get_arrow(reer_diff)

  output$real_effective_box <- renderUI({
    HTML(paste0(
      "Real Effective Exchange Rate<br>",
      "<span style='font-size: 12px; color: gray;'>", latest_reer_date, "</span><br>",
      "<span style='font-size: 16px; color: black;'><b>",
      round(latest_reer_value, 1), "</b> ", reer_arrow,
      "</span>"
    ))
  })

  observeEvent(input$show_reer_modal, {
    updateDateInput(session, "start_date_reer",
      value = min(real_effective_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_reer",
      value = max(real_effective_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = "CPI deflated EER-41/Euro, Monthly",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_reer", "Start Date"),
          dateInput("end_date_reer", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_reer", "1 Year"),
          actionButton("three_years_reer", "3 Years"),
          actionButton("five_years_reer", "5 Years"),
          actionButton("full_range_reer", "Full Range")
        ),
        dygraphOutput("reer_dygraph", height = "300px"),
        div(style = "margin: 20px 0;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_reer", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_reer % 2 == 1",
          DTOutput("reer_table")
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_reer, {
    updateDateInput(session, "start_date_reer", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_reer", value = Sys.Date())
  })
  observeEvent(input$three_years_reer, {
    updateDateInput(session, "start_date_reer", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_reer", value = Sys.Date())
  })
  observeEvent(input$five_years_reer, {
    updateDateInput(session, "start_date_reer", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_reer", value = Sys.Date())
  })
  observeEvent(input$full_range_reer, {
    updateDateInput(session, "start_date_reer",
      value = min(real_effective_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_reer",
      value = max(real_effective_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_reer, {
    table_visible_reer(!table_visible_reer())
  })

  output$reer_dygraph <- renderDygraph({
    req(filtered_reer_data())
    reer_dy_data <- xts(filtered_reer_data()$obsvalue,
      order.by = filtered_reer_data()$obstime_date
    )
    dygraph(reer_dy_data) %>%
      dyAxis("y",
        label = "99Q1=100",
        axisLabelFormatter = "function(d) { return d.toFixed(2); }",
        valueFormatter = "function(d) { return d.toFixed(2); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = TRUE
      )
  })

  output$reer_table <- renderDT({
    req(table_visible_reer())
    filtered_reer_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(
        colnames = c("Period", "Value (99Q1=100)"),
        options = list(pageLength = 10)
      ) %>%
      formatDate(
        "obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short"))
      ) %>%
      formatRound("obsvalue", digits = 2)
  })


  output$download_reer <- downloadHandler(
    filename = function() {
      "real_effective_exchange_rate.csv"
    },
    content = function(file) {
      write.csv(real_effective_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_reer, {
    showModal(modalDialog(
      title = "Real Effective Exchange Rate Quick Info",
      "CPI deflated EER-41/Euro; index 1999 Q1=100).",
      easyClose = TRUE
    ))
  })

  # Tab 3 : Nominal Effective Exchange Rate ------
  latest_neer_value <- nominal_effective_data[.N, obsvalue]
  previous_neer_value <- nominal_effective_data[.N - 1, obsvalue]
  latest_neer_date <- format(nominal_effective_data[.N, obstime], "%B %Y")
  neer_diff <- latest_neer_value - previous_neer_value
  neer_arrow <- get_arrow(neer_diff)

  output$nominal_effective_box <- renderUI({
    HTML(paste0(
      "Nominal Effective Exchange Rate<br>",
      "<span style='font-size: 12px; color: gray;'>", latest_neer_date, "</span><br>",
      "<span style='font-size: 16px; color: black;'><b>",
      round(latest_neer_value, 1), "</b> ", neer_arrow,
      "</span>"
    ))
  })

  observeEvent(input$show_nominal_effective_modal, {
    updateDateInput(session, "start_date_nominal_effective",
      value = min(nominal_effective_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_nominal_effective",
      value = max(nominal_effective_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = "EER-41/Euro, Monthly",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_nominal_effective", "Start Date"),
          dateInput("end_date_nominal_effective", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_nominal_effective", "1 Year"),
          actionButton("three_years_nominal_effective", "3 Years"),
          actionButton("five_years_nominal_effective", "5 Years"),
          actionButton("full_range_nominal_effective", "Full Range")
        ),
        dygraphOutput("nominal_effective_dygraph", height = "300px"),
        div(style = "margin: 20px 0;", HTML("<br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_nominal_effective", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_nominal_effective % 2 == 1",
          DTOutput("nominal_effective_table")
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_nominal_effective, {
    updateDateInput(session, "start_date_nominal_effective", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_nominal_effective", value = Sys.Date())
  })
  observeEvent(input$three_years_nominal_effective, {
    updateDateInput(session, "start_date_nominal_effective", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_nominal_effective", value = Sys.Date())
  })
  observeEvent(input$five_years_nominal_effective, {
    updateDateInput(session, "start_date_nominal_effective", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_nominal_effective", value = Sys.Date())
  })
  observeEvent(input$full_range_nominal_effective, {
    updateDateInput(session, "start_date_nominal_effective",
      value = min(nominal_effective_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_nominal_effective",
      value = max(nominal_effective_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_nominal_effective, {
    table_visible_nominal_effective(!table_visible_nominal_effective())
  })

  output$nominal_effective_dygraph <- renderDygraph({
    req(filtered_nominal_effective_data())
    nominal_effective_dy_data <- xts(filtered_nominal_effective_data()$obsvalue,
      order.by = filtered_nominal_effective_data()$obstime_date
    )
    dygraph(nominal_effective_dy_data) %>%
      dyAxis("y",
        label = "99Q1=100",
        axisLabelFormatter = "function(d) { return d.toFixed(2); }",
        valueFormatter = "function(d) { return d.toFixed(2); }"
      ) %>%
      dySeries("V1", label = "Value") %>%
      dyOptions(labelsUTC = TRUE, fillGraph = FALSE, drawGrid = TRUE, colors = "#000000") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = TRUE
      )
  })

  output$nominal_effective_table <- renderDT({
    req(table_visible_nominal_effective())
    filtered_nominal_effective_data() %>%
      arrange(desc(obstime)) %>%
      select(obstime, obsvalue) %>%
      datatable(
        colnames = c("Period", "Value (99Q1=100)"),
        options = list(pageLength = 10)
      ) %>%
      formatDate(
        "obstime",
        method = "toLocaleString",
        params = list("en-GB", list(year = "numeric", month = "short"))
      ) %>%
      formatRound("obsvalue", digits = 2)
  })


  output$download_nominal_effective <- downloadHandler(
    filename = function() {
      "nominal_effective_exchange_rate.csv"
    },
    content = function(file) {
      write.csv(nominal_effective_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_nominal_effective, {
    showModal(modalDialog(
      title = "Nominal Effective Exchange Rate Quick Info",
      "EER-41/Euro; index 1999 Q1=100.",
      easyClose = TRUE
    ))
  })


  # TAB 4 REACTIVES ----

  # 1) Total Assets => quarterly => convert yearqtr -> Date
  total_assets_data_with_date <- total_assets_data %>%
    mutate(obstime_date = as.Date(obstime, frac = 1))

  table_visible_total_assets <- reactiveVal(FALSE)
  filtered_total_assets_data <- reactive({
    req(input$start_date_total_assets, input$end_date_total_assets)
    total_assets_data_with_date %>%
      filter(obstime_date >= input$start_date_total_assets & obstime_date <= input$end_date_total_assets)
  })

  # 2) Non-Performing Loans => quarterly
  non_perf_loans_data_with_date <- non_perf_loans_data %>%
    mutate(obstime_date = as.Date(obstime, frac = 1))

  table_visible_non_perf_loans <- reactiveVal(FALSE)
  filtered_non_perf_loans_data <- reactive({
    req(input$start_date_non_perf_loans, input$end_date_non_perf_loans)
    non_perf_loans_data_with_date %>%
      filter(obstime_date >= input$start_date_non_perf_loans & obstime_date <= input$end_date_non_perf_loans)
  })

  # 3) Significant Institutions => quarterly
  significant_inst_data_with_date <- significant_inst_data %>%
    mutate(obstime_date = as.Date(obstime, frac = 1))

  table_visible_significant_inst <- reactiveVal(FALSE)
  filtered_significant_inst_data <- reactive({
    req(input$start_date_significant_inst, input$end_date_significant_inst)
    significant_inst_data_with_date %>%
      filter(obstime_date >= input$start_date_significant_inst & obstime_date <= input$end_date_significant_inst)
  })

  # 4) Return on Equity => quarterly
  return_on_equity_data_with_date <- return_on_equity_data %>%
    mutate(obstime_date = as.Date(obstime, frac = 1))

  table_visible_return_on_equity <- reactiveVal(FALSE)
  filtered_return_on_equity_data <- reactive({
    req(input$start_date_return_on_equity, input$end_date_return_on_equity)
    return_on_equity_data_with_date %>%
      filter(obstime_date >= input$start_date_return_on_equity & obstime_date <= input$end_date_return_on_equity)
  })

  # 5) CET1 Ratio => quarterly
  cet1_ratio_data_with_date <- cet1_ratio_data %>%
    mutate(obstime_date = as.Date(obstime, frac = 1))

  table_visible_cet1_ratio <- reactiveVal(FALSE)
  filtered_cet1_ratio_data <- reactive({
    req(input$start_date_cet1_ratio, input$end_date_cet1_ratio)
    cet1_ratio_data_with_date %>%
      filter(obstime_date >= input$start_date_cet1_ratio & obstime_date <= input$end_date_cet1_ratio)
  })

  # 6) Liquidity Coverage => quarterly
  liquidity_coverage_data_with_date <- liquidity_coverage_data %>%
    mutate(obstime_date = as.Date(obstime, frac = 1))

  table_visible_liquidity_coverage <- reactiveVal(FALSE)
  filtered_liquidity_coverage_data <- reactive({
    req(input$start_date_liquidity_coverage, input$end_date_liquidity_coverage)
    liquidity_coverage_data_with_date %>%
      filter(obstime_date >= input$start_date_liquidity_coverage & obstime_date <= input$end_date_liquidity_coverage)
  })

  # Tab 4: Total Assets ------
  latest_total_assets_value <- total_assets_data[.N, obsvalue_billions]
  previous_total_assets_value <- total_assets_data[.N - 1, obsvalue_billions]
  latest_total_assets_date <- format(total_assets_data[.N, obstime], "Q%q %Y")
  previous_total_assets_date <- format(total_assets_data[.N - 1, obstime], "Q%q %Y")
  total_assets_diff <- latest_total_assets_value - previous_total_assets_value
  total_assets_arrow <- get_arrow(total_assets_diff)

  output$total_assets_header <- renderUI({
    HTML(paste0(
      "Total Assets<br>
               <span style='font-size: 12px; color: gray;'>", latest_total_assets_date, "</span><br>
              <span style='font-size: 16px; color: black;'><b>",
      formatC(latest_total_assets_value, format = "f", big.mark = ".", decimal.mark = ",", digits = 3),
      " EUR bln</b> ", total_assets_arrow, "</span>"
    ))
  })

  output$total_assets_plot <- renderPlotly({
    req(total_assets_data$obsvalue_billions)
    plot_ly(total_assets_data[(.N - 7):.N, ],
      x = ~as.Date(obstime), y = ~obsvalue_billions,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "Q%q %Y"
        ),
        yaxis = list(
          title = "Billions of Euro",
          tickformat = ",.2f"
        )
      )
  })

  observeEvent(input$show_total_assets_modal, {
    updateDateInput(session, "start_date_total_assets",
      value = min(total_assets_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_total_assets",
      value = max(total_assets_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = "Total assets, SIs, EU countries participating in the Single Supervisory Mechanism (SSM) (changing composition), Quarterly",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_total_assets", "Start Date"),
          dateInput("end_date_total_assets", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_total_assets", "1 Year"),
          actionButton("three_years_total_assets", "3 Years"),
          actionButton("five_years_total_assets", "5 Years"),
          actionButton("full_range_total_assets", "Full Range")
        ),
        plotlyOutput("total_assets_modal_plot", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML("<br><br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_total_assets", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_total_assets % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("total_assets_table"))
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_total_assets, {
    updateDateInput(session, "start_date_total_assets", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_total_assets", value = Sys.Date())
  })
  observeEvent(input$three_years_total_assets, {
    updateDateInput(session, "start_date_total_assets", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_total_assets", value = Sys.Date())
  })
  observeEvent(input$five_years_total_assets, {
    updateDateInput(session, "start_date_total_assets", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_total_assets", value = Sys.Date())
  })
  observeEvent(input$full_range_total_assets, {
    updateDateInput(session, "start_date_total_assets",
      value = min(total_assets_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_total_assets",
      value = max(total_assets_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_total_assets, {
    table_visible_total_assets(!table_visible_total_assets())
  })

  output$total_assets_modal_plot <- renderPlotly({
    req(filtered_total_assets_data())
    plot_ly(filtered_total_assets_data(),
      x = ~obstime_date, y = ~obsvalue_billions,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "Q%q %Y"
        ),
        yaxis = list(
          title = "Billions of Euro",
          tickformat = ".3f"
        )
      )
  })

  output$total_assets_table <- renderDT({
    req(table_visible_total_assets())
    filtered_total_assets_data() %>%
      arrange(desc(obstime)) %>%
      mutate(
        formatted_value = obsvalue_billions, # Already in billions
        year_quarter = format(obstime, "Q%q %Y")
      ) %>%
      select(year_quarter, formatted_value) %>%
      datatable(
        colnames = c(
          "Period" = "year_quarter",
          "Value (Billions)" = "formatted_value"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatRound(
        columns = "Value (Billions)",
        digits = 4,
        mark = ".",
        dec.mark = ","
      )
  })

  output$download_total_assets <- downloadHandler(
    filename = function() {
      paste("total_assets_data.csv")
    },
    content = function(file) {
      write.csv(total_assets_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_total_assets, {
    showModal(modalDialog(
      title = "Total Assets Quick Info",
      "Total assets of EU countries participating in the Single Supervisory Mechanism (SSM), changing composition.",
      easyClose = TRUE
    ))
  })

  # Tab 4: Non-Performing Loans Ratio ------
  latest_non_perf_loans_value <- non_perf_loans_data[.N, obsvalue]
  previous_non_perf_loans_value <- non_perf_loans_data[.N - 1, obsvalue]
  latest_non_perf_loans_date <- format(non_perf_loans_data[.N, obstime], "Q%q %Y")
  previous_non_perf_loans_date <- format(non_perf_loans_data[.N - 1, obstime], "Q%q %Y")
  non_perf_loans_diff <- latest_non_perf_loans_value - previous_non_perf_loans_value
  non_perf_loans_arrow <- get_arrow(non_perf_loans_diff)

  output$non_perf_loans_header <- renderUI({
    HTML(paste0(
      "Non-Performing Loans Ratio<br>
               <span style='font-size: 12px; color: gray;'>", latest_non_perf_loans_date, "</span><br>
              <span style='font-size: 16px; color: black;'><b>",
      sprintf("%.1f%%", latest_non_perf_loans_value),
      "</b> ", non_perf_loans_arrow, "</span>"
    ))
  })


  output$non_perf_loans_plot <- renderPlotly({
    plot_ly(non_perf_loans_data[(.N - 7):.N, ],
      x = ~ as.Date(obstime), y = ~obsvalue, type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "Q%q %Y"
        ),
        yaxis = list(title = "Percent")
      )
  })

  observeEvent(input$show_non_perf_loans_modal, {
    updateDateInput(session, "start_date_non_perf_loans",
      value = min(non_perf_loans_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_non_perf_loans",
      value = max(non_perf_loans_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = "Non-performing loans ratio (excluding cash balances ...), Quarterly",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_non_perf_loans", "Start Date"),
          dateInput("end_date_non_perf_loans", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_non_perf_loans", "1 Year"),
          actionButton("three_years_non_perf_loans", "3 Years"),
          actionButton("five_years_non_perf_loans", "5 Years"),
          actionButton("full_range_non_perf_loans", "Full Range")
        ),
        plotlyOutput("non_perf_loans_modal_plot", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML("<br><br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_non_perf_loans", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_non_perf_loans % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("non_perf_loans_table"))
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_non_perf_loans, {
    updateDateInput(session, "start_date_non_perf_loans", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_non_perf_loans", value = Sys.Date())
  })
  observeEvent(input$three_years_non_perf_loans, {
    updateDateInput(session, "start_date_non_perf_loans", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_non_perf_loans", value = Sys.Date())
  })
  observeEvent(input$five_years_non_perf_loans, {
    updateDateInput(session, "start_date_non_perf_loans", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_non_perf_loans", value = Sys.Date())
  })
  observeEvent(input$full_range_non_perf_loans, {
    updateDateInput(session, "start_date_non_perf_loans",
      value = min(non_perf_loans_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_non_perf_loans",
      value = max(non_perf_loans_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_non_perf_loans, {
    table_visible_non_perf_loans(!table_visible_non_perf_loans())
  })

  output$non_perf_loans_modal_plot <- renderPlotly({
    req(filtered_non_perf_loans_data())
    plot_ly(filtered_non_perf_loans_data(),
      x = ~obstime_date, y = ~obsvalue,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "Q%q %Y"
        ),
        yaxis = list(
          title = "Percent",
          tickformat = ".2f"
        )
      )
  })

  output$non_perf_loans_table <- renderDT({
    req(table_visible_non_perf_loans())
    filtered_non_perf_loans_data() %>%
      arrange(desc(obstime)) %>%
      mutate(
        year_quarter = format(obstime, "Q%q %Y")
      ) %>%
      select(year_quarter, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "year_quarter",
          "Value (Percent)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatRound(
        columns = "Value (Percent)",
        digits = 2,
        dec.mark = "."
      )
  })

  output$download_non_perf_loans <- downloadHandler(
    filename = function() {
      paste("non_perf_loans_data.csv")
    },
    content = function(file) {
      write.csv(non_perf_loans_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_non_perf_loans, {
    showModal(modalDialog(
      title = "Non-Performing Loans Ratio Quick Info",
      "Non-performing loans excluding cash balances at central banks and other demand deposits.",
      easyClose = TRUE
    ))
  })

  # Tab 4: Significant Institutions ------
  latest_significant_inst_value <- significant_inst_data[.N, obsvalue]
  previous_significant_inst_value <- significant_inst_data[.N - 1, obsvalue]
  latest_significant_inst_date <- format(significant_inst_data[.N, obstime], "Q%q %Y")
  previous_significant_inst_date <- format(significant_inst_data[.N - 1, obstime], "Q%q %Y")
  significant_inst_diff <- latest_significant_inst_value - previous_significant_inst_value
  significant_inst_arrow <- get_arrow(significant_inst_diff)

  output$significant_inst_box <- renderUI({
    HTML(paste0(
      "Significant Institutions<br>
               <span style='font-size: 12px; color: gray;'>", latest_significant_inst_date, "</span><br>
              <span style='font-size: 16px; color: black;'><b>",
      round(latest_significant_inst_value),
      "</b> ", significant_inst_arrow, "</span>"
    ))
  })

  observeEvent(input$show_significant_inst_modal, {
    updateDateInput(session, "start_date_significant_inst",
      value = min(significant_inst_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_significant_inst",
      value = max(significant_inst_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = "Number of supervised institutions, SIs, EU countries participating in the Single Supervisory Mechanism (SSM) (changing composition), Quarterly",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_significant_inst", "Start Date"),
          dateInput("end_date_significant_inst", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_significant_inst", "1 Year"),
          actionButton("three_years_significant_inst", "3 Years"),
          actionButton("five_years_significant_inst", "5 Years"),
          actionButton("full_range_significant_inst", "Full Range")
        ),
        plotlyOutput("significant_inst_modal_plot", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML("<br><br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_significant_inst", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_significant_inst % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("significant_inst_table"))
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_significant_inst, {
    updateDateInput(session, "start_date_significant_inst", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_significant_inst", value = Sys.Date())
  })
  observeEvent(input$three_years_significant_inst, {
    updateDateInput(session, "start_date_significant_inst", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_significant_inst", value = Sys.Date())
  })
  observeEvent(input$five_years_significant_inst, {
    updateDateInput(session, "start_date_significant_inst", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_significant_inst", value = Sys.Date())
  })
  observeEvent(input$full_range_significant_inst, {
    updateDateInput(session, "start_date_significant_inst",
      value = min(significant_inst_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_significant_inst",
      value = max(significant_inst_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_significant_inst, {
    table_visible_significant_inst(!table_visible_significant_inst())
  })

  output$significant_inst_modal_plot <- renderPlotly({
    req(filtered_significant_inst_data())
    plot_ly(filtered_significant_inst_data(),
      x = ~obstime_date, y = ~obsvalue,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "Q%q %Y"
        ),
        yaxis = list(title = "Pure Number")
      )
  })

  output$significant_inst_table <- renderDT({
    req(table_visible_significant_inst())
    filtered_significant_inst_data() %>%
      arrange(desc(obstime)) %>%
      mutate(
        year_quarter = format(obstime, "Q%q %Y")
      ) %>%
      select(year_quarter, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "year_quarter",
          "Value (Pure Number)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatRound(
        columns = "Value (Pure Number)",
        digits = 0
      )
  })


  output$download_significant_inst <- downloadHandler(
    filename = function() {
      paste("significant_inst_data.csv")
    },
    content = function(file) {
      write.csv(significant_inst_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_significant_inst, {
    showModal(modalDialog(
      title = "Significant Institutions Quick Info",
      "Number of banks directly supervised by the ECB.",
      easyClose = TRUE
    ))
  })

  # Tab 4: Return on Equity ------
  latest_return_on_equity_value <- return_on_equity_data[.N, obsvalue]
  previous_return_on_equity_value <- return_on_equity_data[.N - 1, obsvalue]
  latest_return_on_equity_date <- format(return_on_equity_data[.N, obstime], "Q%q %Y")
  previous_return_on_equity_date <- format(return_on_equity_data[.N - 1, obstime], "Q%q %Y")
  return_on_equity_diff <- latest_return_on_equity_value - previous_return_on_equity_value
  return_on_equity_arrow <- get_arrow(return_on_equity_diff)

  output$return_on_equity_box <- renderUI({
    HTML(paste0(
      "Return on Equity<br>
               <span style='font-size: 12px; color: gray;'>", latest_return_on_equity_date, "</span><br>
              <span style='font-size: 16px; color: black;'><b>",
      sprintf("%.1f%%", latest_return_on_equity_value),
      "</b> ", return_on_equity_arrow, "</span>"
    ))
  })

  observeEvent(input$show_return_on_equity_modal, {
    updateDateInput(session, "start_date_return_on_equity",
      value = min(return_on_equity_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_return_on_equity",
      value = max(return_on_equity_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = "Return on equity, SIs, EU countries participating in the Single Supervisory Mechanism (SSM) (changing composition), Quarterly",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_return_on_equity", "Start Date"),
          dateInput("end_date_return_on_equity", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_return_on_equity", "1 Year"),
          actionButton("three_years_return_on_equity", "3 Years"),
          actionButton("five_years_return_on_equity", "5 Years"),
          actionButton("full_range_return_on_equity", "Full Range")
        ),
        plotlyOutput("return_on_equity_modal_plot", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML("<br><br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_return_on_equity", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_return_on_equity % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("return_on_equity_table"))
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_return_on_equity, {
    updateDateInput(session, "start_date_return_on_equity", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_return_on_equity", value = Sys.Date())
  })
  observeEvent(input$three_years_return_on_equity, {
    updateDateInput(session, "start_date_return_on_equity", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_return_on_equity", value = Sys.Date())
  })
  observeEvent(input$five_years_return_on_equity, {
    updateDateInput(session, "start_date_return_on_equity", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_return_on_equity", value = Sys.Date())
  })
  observeEvent(input$full_range_return_on_equity, {
    updateDateInput(session, "start_date_return_on_equity",
      value = min(return_on_equity_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_return_on_equity",
      value = max(return_on_equity_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_return_on_equity, {
    table_visible_return_on_equity(!table_visible_return_on_equity())
  })

  output$return_on_equity_modal_plot <- renderPlotly({
    req(filtered_return_on_equity_data())
    plot_ly(filtered_return_on_equity_data(),
      x = ~obstime_date, y = ~obsvalue,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "Q%q %Y"
        ),
        yaxis = list(
          title = "Percent",
          tickformat = ".2f"
        )
      )
  })

  output$return_on_equity_table <- renderDT({
    req(table_visible_return_on_equity())
    filtered_return_on_equity_data() %>%
      arrange(desc(obstime)) %>%
      mutate(year_quarter = format(obstime, "Q%q %Y")) %>%
      select(year_quarter, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "year_quarter",
          "Value (Percent)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatRound(
        columns = "Value (Percent)",
        digits = 2,
        dec.mark = "."
      )
  })


  output$download_return_on_equity <- downloadHandler(
    filename = function() {
      paste("return_on_equity_data.csv")
    },
    content = function(file) {
      write.csv(return_on_equity_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_return_on_equity, {
    showModal(modalDialog(
      title = "Return on Equity Quick Info",
      "%, SSM countries, changing composition.",
      easyClose = TRUE
    ))
  })

  # Tab 4: CET1 Ratio -------
  latest_cet1_ratio_value <- cet1_ratio_data[.N, obsvalue]
  previous_cet1_ratio_value <- cet1_ratio_data[.N - 1, obsvalue]
  latest_cet1_ratio_date <- format(cet1_ratio_data[.N, obstime], "Q%q %Y")
  previous_cet1_ratio_date <- format(cet1_ratio_data[.N - 1, obstime], "Q%q %Y")
  cet1_ratio_diff <- latest_cet1_ratio_value - previous_cet1_ratio_value
  cet1_ratio_arrow <- get_arrow(cet1_ratio_diff)

  output$cet1_ratio_box <- renderUI({
    HTML(paste0(
      "CET1 Ratio<br>
               <span style='font-size: 12px; color: gray;'>", latest_cet1_ratio_date, "</span><br>
              <span style='font-size: 16px; color: black;'><b>",
      sprintf("%.1f%%", latest_cet1_ratio_value),
      "</b> ", cet1_ratio_arrow, "</span>"
    ))
  })

  observeEvent(input$show_cet1_ratio_modal, {
    updateDateInput(session, "start_date_cet1_ratio",
      value = min(cet1_ratio_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_cet1_ratio",
      value = max(cet1_ratio_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = "CET1 Ratio of Significant Institutions",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_cet1_ratio", "Start Date"),
          dateInput("end_date_cet1_ratio", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_cet1_ratio", "1 Year"),
          actionButton("three_years_cet1_ratio", "3 Years"),
          actionButton("five_years_cet1_ratio", "5 Years"),
          actionButton("full_range_cet1_ratio", "Full Range")
        ),
        plotlyOutput("cet1_ratio_modal_plot", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML("<br><br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_cet1_ratio", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_cet1_ratio % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("cet1_ratio_table"))
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_cet1_ratio, {
    updateDateInput(session, "start_date_cet1_ratio", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_cet1_ratio", value = Sys.Date())
  })
  observeEvent(input$three_years_cet1_ratio, {
    updateDateInput(session, "start_date_cet1_ratio", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_cet1_ratio", value = Sys.Date())
  })
  observeEvent(input$five_years_cet1_ratio, {
    updateDateInput(session, "start_date_cet1_ratio", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_cet1_ratio", value = Sys.Date())
  })
  observeEvent(input$full_range_cet1_ratio, {
    updateDateInput(session, "start_date_cet1_ratio",
      value = min(cet1_ratio_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_cet1_ratio",
      value = max(cet1_ratio_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_cet1_ratio, {
    table_visible_cet1_ratio(!table_visible_cet1_ratio())
  })

  output$cet1_ratio_modal_plot <- renderPlotly({
    req(filtered_cet1_ratio_data())
    plot_ly(filtered_cet1_ratio_data(),
      x = ~obstime_date, y = ~obsvalue,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "Q%q %Y"
        ),
        yaxis = list(
          title = "Percent",
          tickformat = ".2f"
        )
      )
  })

  output$cet1_ratio_table <- renderDT({
    req(table_visible_cet1_ratio())
    filtered_cet1_ratio_data() %>%
      arrange(desc(obstime)) %>%
      mutate(
        year_quarter = format(obstime, "Q%q %Y")
      ) %>%
      select(year_quarter, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "year_quarter",
          "Value (Percent)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatRound(
        columns = "Value (Percent)",
        digits = 2,
        dec.mark = "."
      )
  })

  output$download_cet1_ratio <- downloadHandler(
    filename = function() {
      paste("cet1_ratio_data.csv")
    },
    content = function(file) {
      write.csv(cet1_ratio_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_cet1_ratio, {
    showModal(modalDialog(
      title = "CET1 Ratio Quick Info",
      "%, SSM countries, changing composition.",
      easyClose = TRUE
    ))
  })

  # Tab 4: Liquidity Coverage Ratio ------
  latest_liquidity_coverage_value <- liquidity_coverage_data[.N, obsvalue]
  previous_liquidity_coverage_value <- liquidity_coverage_data[.N - 1, obsvalue]
  latest_liquidity_coverage_date <- format(liquidity_coverage_data[.N, obstime], "Q%q %Y")
  previous_liquidity_coverage_date <- format(liquidity_coverage_data[.N - 1, obstime], "Q%q %Y")
  liquidity_coverage_diff <- latest_liquidity_coverage_value - previous_liquidity_coverage_value
  liquidity_coverage_arrow <- get_arrow(liquidity_coverage_diff)

  output$liquidity_coverage_box <- renderUI({
    HTML(paste0(
      "Liquidity Coverage Ratio<br>
               <span style='font-size: 12px; color: gray;'>", latest_liquidity_coverage_date, "</span><br>
              <span style='font-size: 16px; color: black;'><b>",
      sprintf("%.1f%%", latest_liquidity_coverage_value),
      "</b> ", liquidity_coverage_arrow, "</span>"
    ))
  })

  observeEvent(input$show_liquidity_coverage_modal, {
    updateDateInput(session, "start_date_liquidity_coverage",
      value = min(liquidity_coverage_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_liquidity_coverage",
      value = max(liquidity_coverage_data_with_date$obstime_date)
    )

    showModal(modalDialog(
      title = "Liquidity coverage ratio, SIs, EU countries participating in the Single Supervisory Mechanism (SSM) (changing composition), Quarterly",
      fluidPage(
        div(
          style = "display: flex; gap: 10px; justify-content: center;",
          dateInput("start_date_liquidity_coverage", "Start Date"),
          dateInput("end_date_liquidity_coverage", "End Date")
        ),
        div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
          actionButton("one_year_liquidity_coverage", "1 Year"),
          actionButton("three_years_liquidity_coverage", "3 Years"),
          actionButton("five_years_liquidity_coverage", "5 Years"),
          actionButton("full_range_liquidity_coverage", "Full Range")
        ),
        plotlyOutput("liquidity_coverage_modal_plot", height = "300px"),
        div(style = "margin: 20px 0; font-size: 14px; line-height: 1.6;", HTML("<br><br>")),
        div(
          style = "text-align: center;",
          actionButton("toggle_table_liquidity_coverage", "⊞")
        ),
        conditionalPanel(
          condition = "input.toggle_table_liquidity_coverage % 2 == 1",
          div(style = "margin-top: 10px;", DTOutput("liquidity_coverage_table"))
        )
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$one_year_liquidity_coverage, {
    updateDateInput(session, "start_date_liquidity_coverage", value = Sys.Date() - lubridate::years(1))
    updateDateInput(session, "end_date_liquidity_coverage", value = Sys.Date())
  })
  observeEvent(input$three_years_liquidity_coverage, {
    updateDateInput(session, "start_date_liquidity_coverage", value = Sys.Date() - lubridate::years(3))
    updateDateInput(session, "end_date_liquidity_coverage", value = Sys.Date())
  })
  observeEvent(input$five_years_liquidity_coverage, {
    updateDateInput(session, "start_date_liquidity_coverage", value = Sys.Date() - lubridate::years(5))
    updateDateInput(session, "end_date_liquidity_coverage", value = Sys.Date())
  })
  observeEvent(input$full_range_liquidity_coverage, {
    updateDateInput(session, "start_date_liquidity_coverage",
      value = min(liquidity_coverage_data_with_date$obstime_date)
    )
    updateDateInput(session, "end_date_liquidity_coverage",
      value = max(liquidity_coverage_data_with_date$obstime_date)
    )
  })

  observeEvent(input$toggle_table_liquidity_coverage, {
    table_visible_liquidity_coverage(!table_visible_liquidity_coverage())
  })

  output$liquidity_coverage_modal_plot <- renderPlotly({
    req(filtered_liquidity_coverage_data())
    plot_ly(filtered_liquidity_coverage_data(),
      x = ~obstime_date, y = ~obsvalue,
      type = "scatter", mode = "lines", line = list(color = "black")
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "Q%q %Y"
        ),
        yaxis = list(
          title = "Percent",
          tickformat = ".2f"
        )
      )
  })

  output$liquidity_coverage_table <- renderDT({
    req(table_visible_liquidity_coverage())
    filtered_liquidity_coverage_data() %>%
      arrange(desc(obstime)) %>%
      mutate(
        year_quarter = format(obstime, "Q%q %Y")
      ) %>%
      select(year_quarter, obsvalue) %>%
      datatable(
        colnames = c(
          "Period" = "year_quarter",
          "Value (Percent)" = "obsvalue"
        ),
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
      formatRound(
        columns = "Value (Percent)",
        digits = 2,
        dec.mark = "."
      )
  })


  output$download_liquidity_coverage <- downloadHandler(
    filename = function() {
      paste("liquidity_coverage_data.csv")
    },
    content = function(file) {
      write.csv(liquidity_coverage_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$info_liquidity_coverage, {
    showModal(modalDialog(
      title = "Liquidity Coverage Ratio Quick Info",
      "%, SSM countries, changing composition.",
      easyClose = TRUE
    ))
  })
}

# Run the app -------
shinyApp(ui = ui, server = server)
