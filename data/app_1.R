library(shiny)
library(tidyverse)


principals <- read_rds("../data/title_principals.rda")
names_tbl  <- read_rds("../data/name_basics.rda")
ratings    <- read_rds("../data/title_ratings.rda")
basics     <- read_rds("../data/title_basics.rda")


joined <- principals |>
  left_join(ratings, by = "tconst") |>
  left_join(basics |> select(tconst, startYear, primaryTitle, genres), by = "tconst")

top10_genres <- joined$genres |>
  str_split(",") |>
  unlist() |>
  tibble(genre = _) |>
  count(genre, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(genre)

joined_top10 <- joined |>
  filter(
    map_lgl(genres, ~ any(str_split(.x, ",")[[1]] %in% top10_genres))
  )

ui <- fluidPage(
  titlePanel("IMDb Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Job Category:", choices = unique(joined_top10$category)),
      sliderInput("year", "Start Year:", min = 2000, max = 2025, value = c(2000, 2025)),
      checkboxGroupInput("genres", "Genres: ", choices = top10_genres, selected = top10_genres)
      
    ),
    mainPanel(
      plotOutput("genrePlot"),
      plotOutput("ratingPlot", click = "plot_click"),
      verbatimTextOutput("click_info")
    )
  )
)

server <- function(input, output) {

  filtered <- reactive({
    joined_top10 |>
      filter(category == input$category,
            !is.na(startYear),
            startYear >= input$year[1],
            startYear <= input$year[2])
  })
  
  output$ratingPlot <- renderPlot({
    ggplot(
      filtered(), aes(startYear, averageRating, color = averageRating)) +
      geom_point(alpha = 0.4, size = 2) +
      geom_smooth(se = FALSE, color = "black", linewidth = 1) +
      scale_color_viridis_c(option = "plasma") +
      labs(
        title = "Ratings Over Time",
        x = "Release Year",
        y = "Average IMDb Rating",
        color = "Rating"
    )
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