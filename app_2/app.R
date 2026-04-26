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
      tableOutput("job_info")
    )
  )
)


# Define server logic
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
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
