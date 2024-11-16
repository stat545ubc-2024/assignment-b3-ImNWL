library(shiny)
library(dplyr)
library(DT)
library(tibble)

# Load dataset and extract brand names
cars_data <- mtcars %>%
  rownames_to_column("car_name") %>%
  mutate(
    brand = sub(" .*", "", car_name),  # Extract brand from car_name
    cyl = as.factor(cyl)              # Convert cyl to factor for potential categorical analysis
  )

# UI
ui <- fluidPage(
  titlePanel("Mtcars Explorer"),
  h4("Explore the mtcars dataset interactively."),
  sidebarLayout(
    sidebarPanel(
      # Feature 1: Slider and Checkboxes
      # Allow users to filter data interactively based on horsepower (slider) and car brand (checkboxes)
      sliderInput("hpInput", "Horsepower (hp)", 
                  min = min(cars_data$hp), 
                  max = max(cars_data$hp), 
                  value = c(52, 335)),
      checkboxGroupInput("brandInput", 
                         "Car Brand", 
                         choices = c("All", sort(unique(cars_data$brand))), 
                         selected = "All"),
      # Feature 3: Download Filtered Data
      # Provide users with a button to download the filtered data for further offline analysis
      downloadButton("downloadData", "Download Filtered Data")
    ),
    mainPanel(
      # Feature 2: Interactive Table
      # Display a dynamic and sortable table, allowing users to interact with the filtered dataset
      DTOutput("carTable")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to store the previous state of brandInput
  previous_brandInput <- reactiveVal()
  user_deselected_all <- reactiveVal(TRUE)
  
  observeEvent(input$brandInput, {
    current_input <- input$brandInput
    previous_input <- previous_brandInput() # Get the previous state
    
    if (!is.null(current_input)) {
      # Case 1: "All" was just selected
      if ("All" %in% current_input && !"All" %in% previous_input) {
        updateCheckboxGroupInput(
          session, "brandInput",
          selected = c("All", sort(unique(cars_data$brand)))
        )
      } 
      # Case 2: "All" was just deselected
      else if (!"All" %in% current_input && "All" %in% previous_input) {
        if (user_deselected_all()) {
          updateCheckboxGroupInput(
            session, "brandInput",
            selected = character(0)
          )
        } else {
          user_deselected_all(TRUE)
        }
      } 
      # Case 3: A specific brand was deselected while "All" was selected
      else if ("All" %in% previous_input && length(current_input) < length(previous_input)) {
        updateCheckboxGroupInput(
          session, "brandInput",
          selected = setdiff(current_input, "All")
        )
        user_deselected_all(FALSE)
      }
    }
    previous_brandInput(current_input)
  })
  
  # Reactive expression to filter data based on user input
  filtered_data <- reactive({
    data <- cars_data %>%
      filter(hp >= input$hpInput[1], hp <= input$hpInput[2])
    if (!"All" %in% input$brandInput || length(input$brandInput) == 0) {
      data <- data %>%
        filter(brand %in% input$brandInput)
    }
    data %>% arrange(desc(hp)) # Default sorting by horsepower in descending order
  })
  
  # Feature 2: Render Interactive Table
  # This table dynamically updates based on the filtered data and allows sorting and pagination.
  output$carTable <- renderDT({
    filtered_data() %>%
      datatable(
        options = list(pageLength = 15),
        rownames = FALSE
      )
  })
  
  # Feature 3: Download Filtered Data
  # Enable users to download the current filtered dataset as a CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_cars", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
