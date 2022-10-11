library(shiny)
library(plotrix)
require(shiny)
require(plotrix)

ui <- fluidPage(
  titlePanel("Schwarzschild Radius of Black Holes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("masstype", "Mass Unit:",
                  c("Suns" = "su",
                    "Earths" = "et",
                    "Tonnes" = "t")),
      
      numericInput("massquant", "Mass:", 5, min = 1, max = 50, step=1),
      verbatimTextOutput("value")
    ),
    
    mainPanel(
      
      h3("Plotted Black hole:"),
      
      h5(textOutput("caption")),
      h5(textOutput("radiustext")),
      
      plotOutput("CirclePlot"),
      
      h5("Graphics and program created by Shin Lee '26"),
      h5("For submission for ASTRO 107 WA3"),
      
    )
  )
)


server <- function(input, output) {
  
  radiuscalculator <- reactive({
    (input$massquant * factor() * 2 * 6.67430 * (10^-11))/(3*10^8 * 10^3) 
  })
  
  radiustext <- reactive({
    paste("Mass is: ", input$massquant, masstext());
  })
  
  realradiustext <- reactive({
    paste("Radius is: ", radiuscalculator(), "Kilometers", " = ", aucalc(), "AUs")
  })
  
  aucalc <- reactive({
    radiuscalculator() / (6.684*10^9)
  })
  
  output$radiustext <-renderText({
    realradiustext()
  })
  
  output$caption <- renderText({
    radiustext()
  })
  
  masstext <- reactive({
    if(input$masstype == 'su'){
      "Suns"
    }else if(input$masstype == "et"){
      "Earths"
    }else{
      "Tonnes"
    }
  })
  
  factor <- reactive({
    if(input$masstype == 'su'){
      1.989*(10^30)
    }else if(input$masstype == "et"){
      5.9722*(10^24)
    }else{
      1000
    }
  })
  
  output$CirclePlot <- renderPlot({
    plot(1:100,type="n",axes="FALSE", ylab="", yaxt="n", xlab="", xaxt = "n")
    radius <- radiuscalculator() / (6.684*10^9)
    print(radius)
    draw.circle(50,50,radius,col="black")
  })
  
}




shinyApp(ui, server)

