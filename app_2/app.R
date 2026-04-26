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
<<<<<<< HEAD
      h4("People in this category (with title ratings)"),
=======
>>>>>>> c61a004 (App 2 created)
      tableOutput("job_info")
    )
  )
)


# Define server logic
<<<<<<< HEAD
server <- function(input, output, session) {
  # Load the correct .rda files
  principals <- read_rds("../data/title_principals.rda")
  names_tbl  <- read_rds("../data/name_basics.rda")
  ratings    <- read_rds("../data/title_ratings.rda")
  
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
=======
server <- function(input, output) {
  # Load data
  principals <- read.csv("principals.csv")
  jobs <- read.csv("jobs.csv")
  
  # Update category choices based on the principals table
  observe({
    categories <- unique(principals$category)
    updateSelectInput(session, "category", choices = categories)
  })
  
  # Display job information based on selected category
  output$job_info <- renderTable({
    req(input$category) # Ensure a category is selected
    selected_jobs <- principals[principals$category == input$category, ]
    job_info <- merge(selected_jobs, jobs, by = "job_id") # Assuming 'job_id' is the common column
    return(job_info)
>>>>>>> c61a004 (App 2 created)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
