#librerias

library(shiny)
library(plotly)

# Define UI 
ui<-fluidPage( 
  sidebarLayout(
  sidebarPanel(
    fileInput("tabla", "Suba su base datos", buttonLabel = "Subir tabla", accept = ".csv"),
    selectInput("Variables","Ingrese la Variable", choices = NULL),
    selectInput("Exten", "Tipo Archivo Descarga", choices = c("png", "pdf", "svg"))
    ),
  mainPanel(
    h3("Muestra de la base de datos"),
    tableOutput("vista"),
    plotOutput("histo"),
    downloadButton("dl", "Descarga tu histograma")
  )
)
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  basedatos <- reactive({
    req(input$tabla)
    vroom::vroom(input$tabla$datapath)
    
  })
  
  output$vista <- renderTable(head(basedatos()))
 
  

  observeEvent(input$tabla, {
    req(basedatos())
    colnumero <- dplyr::select_if(basedatos(), is.numeric)
    updateSelectInput(session, "Variables", choices = colnames(colnumero))
  })
  

  histo <- reactive({
    if(!is.null(input$Variables))
      ggplot(basedatos()) +
      aes_string(x = input$Variables) +
      geom_histogram()
      
   # plot_ly(x = input$Variables, type = "histogram")
  })
  
  output$histo <- renderPlot(histo())  
  
  output$dl <- downloadHandler(
    
    filename = paste("Histograma", input$Exten, sep = "."),
    content = function(file) {
      ggsave(file, histo(), device = input$Exten)
    }
  )
  
  
}
 

# Run the application 
shinyApp(ui = ui, server = server)
