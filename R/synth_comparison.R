
library(shiny)
library(ggplot2)
library(dplyr)
library(arsenal)


# Load Datasets ----
synth2017 <- qs::qread("C:/WBG/repos/100points_distributions/data/album/20240326_2017_01_02_PROD/world_100bin.qs")
nonsynth2017 <- qs::qread("20240326_2017_01_02_PROD/world_100bin.qs")

df1 <- synth2017
df2 <- nonsynth2017

# Different ones ----
different_ones <- df1 |>
  mutate(reporting_level = as.character(reporting_level))|>
  full_join(df2 |>
              mutate(reporting_level = as.character(reporting_level)),
            by = c("country_code", "year"), suffix = c(".df1", ".df2")) |>
  filter(reporting_level.df1 != reporting_level.df2) |>
  select(country_code, year, reporting_level.df1, reporting_level.df2)|>
  distinct(country_code, year)

# Define UI -----
ui <- fluidPage(
  titlePanel("Synth VS PROD"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("filterDiff", "Only countries with discrepancies", FALSE),
      selectInput("country_code", "Country:", choices = NULL),
      selectInput("year", "Year:", choices = NULL),
      selectInput("variable", "Variable to Compare:",
                  choices = c("welfare_share", "avg_welfare", "quantile")),
      actionButton("update", "Update Plot")
    ),
    mainPanel(
      plotOutput("comparisonPlot")
    )
  )
)


# Define server logic v2----
server <- function(input, output, session) {

  # setup
  observe({
    if (input$filterDiff) {
      updateSelectInput(session, "country_code", choices = unique(different_ones$country_code))
    } else {
      updateSelectInput(session, "country_code", choices = unique(df1$country_code))
    }
  })

  # update years available
  observe({
    if (input$country_code %in% different_ones$country_code) {
      years <- unique(different_ones[different_ones$country_code == input$country_code,]$year)
    } else {
      years <- unique(df1[df1$country_code == input$country_code,]$year)
    }
    updateSelectInput(session, "year", choices = years)
  })

  observeEvent(input$update, {
    filtered_data1 <- reactive({
      df1 |>
        filter(country_code == input$country_code, year == as.numeric(input$year)) %>%
        mutate(dataset = "Synth")
    })

    filtered_data2 <- reactive({
      df2 |>
        filter(country_code == input$country_code, year == as.numeric(input$year)) %>%
        mutate(dataset = "Non-Synth")
    })

    # combine
    combined_data <- reactive({
      bind_rows(filtered_data1(), filtered_data2())
    })

    # render
    output$comparisonPlot <- renderPlot({
      ggplot(data = combined_data(), aes_string(x = "percentile", y = input$variable, color = "reporting_level", shape = "dataset")) +
        geom_point() +
        labs(x = "percentile", y = input$variable, title = paste("Comparison of", input$variable, "between two datasets")) +
        scale_shape_manual(values = c("Synth" = 16, "Non-Synth" = 17)) +
        theme_minimal()
    })
  })
}


# Run the application ----
shinyApp(ui = ui, server = server)









