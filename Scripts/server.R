library(shiny)
library(readxl)
library(dplyr)
library(DT)

# Global definition of the file names
file_names <- c("anthropometriques", "hematologie_iron", "hormes", 
                "performance", "serum_chemistry_blood", "sujet", "vitamin", "whole_blood_analysis")

server <- function(input, output, session) {
  
  # Reactive values to store the unique player identifiers and instances
  unique_players <- reactiveVal()
  player_instances <- reactiveVal(list())
  
  observeEvent(input$file, {
    req(input$file)
    
    player_data <- readxl::read_excel("../Data/sujet.xlsx")
    player_identifiers <- unique(gsub("\\d+", "", colnames(player_data)[-1]))
    
    unique_players(player_identifiers)
    updateSelectInput(session, 'selected_player', choices = player_identifiers)
  })
  
  observeEvent(input$selected_player, {
    req(input$selected_player, unique_players())
    
    # Create a list to store instances of selected player across all files
    instances_list <- list()
    
    for (file_name in file_names) {
      # Attempt to read the excel file
      tryCatch({
        sheet_data <- readxl::read_excel(paste0("../Data/", file_name, ".xlsx"), sheet = 1)
        player_cols <- grep(paste("^", input$selected_player, "\\d+$", sep = ""), colnames(sheet_data), value = TRUE)
        if (length(player_cols) > 0) {
          instances_list[[file_name]] <- sheet_data[, player_cols, drop = FALSE]
        }
      }, error = function(e) {
        # Handle the error
        cat("Error reading file:", file_name, "\n")
      })
    }
    
    player_instances(instances_list)
    
    # Create tabs UI for instances of the selected player
    output$player_tabs <- renderUI({
      myTabs <- lapply(names(player_instances()), function(file_name) {
        tabPanel(
          title = file_name,
          dataTableOutput(outputId = paste0("table_", file_name))
        )
      })
      do.call(tabsetPanel, myTabs)
    })
    
    # Render data tables for each instance
    lapply(names(player_instances()), function(file_name) {
      output[[paste0("table_", file_name)]] <- renderDataTable({
        req(player_instances())
        datatable(player_instances()[[file_name]], options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE))
      })
    })
  })
}





