library(shiny)
library(tidyverse)


principals <- read.csv("principals.csv")
jobs <- read.csv("jobs.csv")
joined <- principals |>
  left_join(jobs, by = "nconst")

ui <- fluidPage(
  titlePanel("IMDb Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Job Category:", choices = unique(joined$category)),
      sliderInput("year", "Start Year:", min = 1900, max = 2025, value = c(1950, 2025)),
      checkboxGroupInput("ratingGroup", "Rating Group:", choices = c("Low","Mid","High"))
      
    ),
    mainPanel(
      plotOutput("ratingPlot"),
      plotOutput("genrePlot"),
      plotOutput("ratingplot", click = "plot_click"),
      verbatimTextOutput("click_info")
    )
  )
)

server <- function(input, output) {

  filtered <- reactive({
    joined |>
      filter(category == input$category,
             startYear >= input$year[1],
             startYear <= input$year[2])
  })
  
  output$ratingPlot <- renderPlot({
    ggplot(filtered(), aes(startYear, averageRating)) +
      geom_point(alpha = 0.3) +
      geom_smooth()
  })
  output$genrePlot <- renderPlot({
    ggplot(filtered(), aes(category)) +
      geom_bar()
  })
  
  
  output$click_info <- renderPrint({
    input$plot_click
  })
}

shinyApp(ui = ui, server = server)