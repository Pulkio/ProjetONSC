library(shiny)
library(readxl)
library(dplyr)
library(DT)

# Global definition of the file names
file_names <- c("anthropometriques", "hematologie_iron", "hormes", 
                "performance", "serum_chemistry_blood", "sujet", "vitamin", "whole_blood_analysis")

# Server function
server <- function(input, output, session) {
  
  # Reactive values to store the data from each Excel file
  data_list <- reactiveValues()
  
  # Define the base directory for the data files
  base_dir <- "../Data"
  
  # Load data when a file is uploaded and prepare the player names for the dropdown
  observeEvent(input$file, {
    req(input$file)
    
    # Read each file and store it in the reactiveValues object
    for (name in file_names) {
      file_path <- file.path(base_dir, paste0(name, ".xlsx"))
      if (file.exists(file_path)) {
        data_list[[name]] <- readxl::read_excel(file_path, sheet = 1)
      } else {
        print(paste("File does not exist:", file_path))
      }
    }
    
    # Update the selectInput choices with the 'sujet' file's column names, if it exists
    if (!is.null(data_list$sujet)) {
      player_names <- colnames(data_list$sujet)[-1]  # Exclude the first column which is 'Sujet'
      updateSelectInput(session, 'selected_player', choices = player_names)
    }
  })
  
  # Create individual output objects for each data table
  for (name in file_names) {
    local({ 
      local_name <- name  # create a local copy to be used in the function closure
      output[[paste0("table_", local_name)]] <- renderDataTable({
        req(input$selected_player)
        if (is.null(data_list[[local_name]])) {
          return()
        }
        selected_col <- which(colnames(data_list[[local_name]]) == input$selected_player)
        # Extracting only the rows and the selected column (player)
        player_data <- data_list[[local_name]][, c(1, selected_col), drop = FALSE]
        colnames(player_data) <- c("Category", local_name)  # Set column names to 'Category' and the file name
        datatable(player_data, options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE))
      })
    })
  }
}
