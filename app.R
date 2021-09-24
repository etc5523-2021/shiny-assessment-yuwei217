library(shiny)
library(DT)
library(tidyverse)
library(plotly)
library(ggplot2)
library(here)
library(dplyr)
library(crosstalk)


tidy_fuels <- read_csv(here("data", "cooking.csv"))

# UI
# -------------
ui <- fluidPage(
  title = "Indoor Air Pollution",
  tabsetPanel(
    tabPanel("chart",
      icon = icon("line-chart"),
      fluidRow(
        column(
          2,
          checkboxInput("linear_scale",
            "Linearize x-axis",
            value = FALSE
          )
        ),
        column(
          6,
          offset = 1,
          selectizeInput("countries", "Select Countries",
            choices = tidy_fuels$country,
            multiple = TRUE
          )
        ),
        column(
          2,
          offset = 1,
          checkboxInput("small_countries",
            "Hide countries < 1 million",
            value = FALSE
          )
        )
      ),
      plotlyOutput("chart", width = "100%"),
      sliderInput("year",
        "Year",
        min = 2000,
        max = 2016,
        value = 2016,
        sep = "",
        width = "100%"
      )
    ),
    tabPanel("table", dataTableOutput("table"), icon = icon("table")),

    tabPanel("about", icon = icon("question"), uiOutput("tab"), uiOutput("tab2"))
  )
)

# SERVER
# -------------
server <- function(input, output, session) {


  output$chart <- renderPlotly({

    if (input$small_countries) {
      tidy_fuels <- tidy_fuels %>%
        filter(total_population >= 1e6)
    }

    if(!is.null(input$countries)){
      tidy_fuels <- tidy_fuels %>%
        filter(country %in% input$countries)
    }

    tidy_fuels$tooltip <-
      glue::glue_data( tidy_fuels,
                      "Country: {country}",
                      "\nPopulation: {scales::label_number_auto()(total_population)}",
                      "\nProportion: {scales::percent(cooking, scale = 1, accuracy = 1)}",
                      "\nGDP per capita: {scales::dollar(gdp_per_capita)}")

    if(input$linear_scale){
      tidy_fuels1 <- tidy_fuels %>%
        filter(year == input$year) %>%
        highlight_key(~country) %>%
        ggplot(aes(
          x = gdp_per_capita,
          y = cooking,
          size = total_population,
          color = continent,
          label = country), alpha = 0.5
        ) +
        geom_point(aes(text = tooltip)) +
        scale_x_continuous(labels = ~scales::dollar(., accuracy = 1)) +
        scale_y_continuous(labels = ~scales::percent(., scale = 1)) +
        scale_size_continuous(breaks = c(100e6, 500e6, 1e9),
                              labels = c("100 million", "500 million", "1 billion"),limits = c(100e6,1e9)) +

        labs(x = "GDP per capita",
             y = "Access to clean fuels for cooking (% access)",
             color = "continent",
             size = "")
    }

    else{
        tidy_fuels1 <- tidy_fuels %>%
        filter(year == input$year) %>%
        highlight_key(~country) %>%
        ggplot(aes(
          x = gdp_per_capita,
          y = cooking,
          size = total_population,
          color = continent,
          label = country), alpha = 0.5
        ) +
        geom_point(aes(text = tooltip)) +
        scale_x_log10(breaks = c(1000, 2000, 5000, 10000, 20000,
                                 50000, 100000),
                      labels = ~scales::dollar(., accuracy = 1), limits = c(1000,100000)) +
        scale_y_continuous(labels = ~scales::percent(., scale = 1)) +
        scale_size_continuous(breaks = c(100e6, 500e6, 1e9),
                              labels = c("100 million", "500 million", "1 billion"), limits = c(100e6,1e9)) +

        labs(x = "GDP per capita",
             y = "Access to clean fuels for cooking (% access)",
             color = "continent",
             size = "")
    }

    p <- ggplotly(
        tidy_fuels1 ,
        tooltip = "text") %>%
        config(displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d")) %>%
        highlight( on = "plotly_hover", off = "plotly_deselect")

  })

  output$table <- renderDataTable({
                    (tidy_fuels) %>%
                   mutate(cooking = round(cooking, digits = 2),
                   gdp_per_capita = round(gdp_per_capita, digits = 2)) %>%
                   filter(year == input$year)



  })

  url <- a("Source Link", href="https://ourworldindata.org/grapher/access-to-clean-fuels-for-cooking-vs-gdp-per-capita")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })

  url2 <- a("Creator: Yuwei Jiang", href="https://etc5523-2021.github.io/blog-yuwei217/about.html")
  output$tab2 <- renderUI({
    tagList("URL link:", url2)
  })
}

runApp(shinyApp(ui, server))
