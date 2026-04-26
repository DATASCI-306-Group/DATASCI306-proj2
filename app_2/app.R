# In the principals table, there is a `category` column. Use this column as a primary 
# filter to allow users to then select specific job categories. After select the specific 
# job categories, display information from another table.

library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel("Job Categories"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Job Category:", choices = NULL)
    ),
    mainPanel(
      h4("People in this category (with title ratings)"),
      tableOutput("job_info")
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  # Load the correct .rda files
  principals <- read_rds("data/title_principals_sample.rda")
  names_tbl  <- read_rds("data/name_basics_sample.rda")
  ratings    <- read_rds("data/title_ratings_sample.rda")
  
  # Populate dropdown from actual category column
  observe({
    categories <- sort(unique(principals$category))
    updateSelectInput(session, "category", choices = categories)
  })
  
  output$job_info <- renderTable({
    req(input$category)
    
    # Filter principals by selected category, then join names + ratings
    principals |>
      filter(category == input$category) |>
      left_join(names_tbl |> select(nconst, primaryName), by = "nconst") |>
      left_join(ratings, by = "tconst") |>
      select(primaryName, tconst, category, characters, averageRating, numVotes) |>
      arrange(desc(averageRating)) |>
      head(50)  # limit rows for display
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
