#librerias

library(shiny)


# Define UI
ui<-fluidPage( 
  sidebarLayout(
  sidebarPanel(
    fileInput("tabla", "Suba su base datos", buttonLabel = "Subir tabla", accept = ".csv"),
    selectInput("Variables","Ingrese la Variable", choices = "variables")
    ),
  mainPanel(
    h3("Muestra de la base de datos"),
    tableOutput("vista"),
    verbatimTextOutput("ttest")
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
    num_cols <- dplyr::select_if(basedatos(), is.numeric)
    updateSelectInput(session, "Variables", choices = colnames(num_cols))
  })
  
  output$ttest <- renderPrint({
    if(!is.null(input$Variables))
      t.test(basedatos()[input$Variables])
  }) 
}
 

# Run the application 
shinyApp(ui = ui, server = server)
