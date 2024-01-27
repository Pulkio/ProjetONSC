library(shiny)

ui <- fluidPage(
  titlePanel("Extraction de données"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sélectionner un fichier Excel")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Sujet", tableOutput("sujet_table")),
        tabPanel("Anthropometriques", tableOutput("anthropometriques_table")),
        tabPanel("Performance", tableOutput("performance_table")),
        tabPanel("Serum Chemistry Blood", tableOutput("serum_chemistry_blood_table")),
        tabPanel("Whole Blood Analysis", tableOutput("whole_blood_analysis_table")),
        tabPanel("Hematologie Iron", tableOutput("hematologie_iron_table")),
        tabPanel("Hormes", tableOutput("hormes_table")),
        tabPanel("Vitamin", tableOutput("vitamin_table"))
      )
    )
  )
)
