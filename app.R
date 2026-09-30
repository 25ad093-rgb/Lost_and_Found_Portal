library(shiny)

# Load data
lost_found <- read.csv("lost_found.csv")

ui <- fluidPage(
  
  titlePanel("Lost and Found Portal"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Report an Item"),
      
      textInput(
        "item_name",
        "Item Name:",
        placeholder = "Example: Black Bag"
      ),
      
      selectInput(
        "category",
        "Category:",
        choices = c(
          "Electronics",
          "ID Card",
          "Books",
          "Wallet",
          "Bag",
          "Bottle",
          "Stationery",
          "Other"
        )
      ),
      
      selectInput(
        "item_status",
        "Status:",
        choices = c("Lost", "Found")
      ),
      
      textInput(
        "location",
        "Location:",
        placeholder = "Example: Library"
      ),
      
      dateInput(
        "date",
        "Date:"
      ),
      
      textInput(
        "contact",
        "Contact Number:"
      ),
      
      actionButton(
        "submit",
        "Report Item"
      )
    ),
    
    mainPanel(
      
      h3("Search Reported Items"),
      
      textInput(
        "search",
        "Search Item:",
        placeholder = "Enter item name..."
      ),
      
      selectInput(
        "status",
        "Filter Status:",
        choices = c("All", "Lost", "Found")
      ),
      
      tableOutput("items")
    )
  )
)

server <- function(input, output, session) {
  
  # Store data
  data <- reactiveVal(lost_found)
  
  # Add new item
  observeEvent(input$submit, {
    
    if (input$item_name == "" ||
        input$location == "" ||
        input$contact == "") {
      
      showNotification(
        "Please fill all required fields.",
        type = "error"
      )
      
    } else {
      
      new_item <- data.frame(
        ID = max(data()$ID) + 1,
        Item_Name = input$item_name,
        Category = input$category,
        Status = input$item_status,
        Location = input$location,
        Date = as.character(input$date),
        Contact = input$contact
      )
      
      updated_data <- rbind(data(), new_item)
      
      data(updated_data)
      
      write.csv(
        updated_data,
        "lost_found.csv",
        row.names = FALSE
      )
      
      showNotification(
        "Item reported successfully!",
        type = "message"
      )
    }
  })
  
  # Filter items
  filtered_data <- reactive({
    
    result <- data()
    
    if (input$status != "All") {
      result <- result[result$Status == input$status, ]
    }
    
    if (input$search != "") {
      result <- result[
        grepl(
          input$search,
          result$Item_Name,
          ignore.case = TRUE
        ),
      ]
    }
    
    result
  })
  
  # Display table
  output$items <- renderTable({
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)