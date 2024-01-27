library(shiny)
library(DT) 


ui <- fluidPage(
  titlePanel("Extraction de données"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sélectionner un fichier Excel")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Sujet", DT::dataTableOutput("sujet_table")),
        tabPanel("Anthropometriques", DT::dataTableOutput("anthropometriques_table")),
        tabPanel("Performance", DT::dataTableOutput("performance_table")),
        tabPanel("Serum Chemistry Blood", DT::dataTableOutput("serum_chemistry_blood_table")),
        tabPanel("Whole Blood Analysis", DT::dataTableOutput("whole_blood_analysis_table")),
        tabPanel("Hematologie Iron", DT::dataTableOutput("hematologie_iron_table")),
        tabPanel("Hormes", DT::dataTableOutput("hormes_table")),
        tabPanel("Vitamin", DT::dataTableOutput("vitamin_table"))
      )
    )
  )
)
