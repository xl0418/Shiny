#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source('~/Dropbox/R/MMgit/MMM_sim.R', echo=TRUE)


library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Multiple-location Model"),
   fluidRow(
     column(12,
            "",
   fluidRow(
     column(2,
   numericInput("Mig",
                "Migration rate:",1 , min = 0, max = 10,step = 0.1),
   actionButton("goButton", "Simulate tree")
   # p("Click the button to simulate a phylogenetic tree under given parameters")
   ),
   column(2,
   numericInput("Age",
                "Age:", 10, min = 5, max = 100, step = 5)
  
   ),
   
     column(2,
   numericInput("Speciation",
                "Speciation rate:", 0.8, min = 0.5, max = 3,step = 0.1)
   ),
   
   column(2,
   numericInput("Extinction",
                "Extinction rate:", 0.4, min = 0, max = 1,step = 0.1)
   ),
   
     column(2,
   numericInput("Carrying",
                "Carrying capacity:", 20, min = 10, max = 100,step = 5)
   ),
   column(2,
              numericInput("n",
                           "Locations:", 4, min = 2, max = 10,step = 1)
   )
   ),
      
      # Show a plot of the generated distribution
   fluidRow(
        tabPanel("Plot",
                 column(10, plotOutput("lttplot"))
                 )
      ),
   fluidRow(
     tabPanel("Plot",
              column(10, plotOutput("tree"))
     )
   )
     )))
  
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  phylo <- eventReactive(input$goButton, {
    pars = c(input$Speciation,input$Extinction, input$Carrying)
    parsN = rep(0,input$n)
    parsN[1:2] = 1
    MMM_sim(n = input$n, parsN = parsN,pars = pars,M0 = input$Mig, age = input$Age) 
    
  })
  
 # phylo <- reactive({
 #   pars = c(input$Speciation,input$Extinction, input$Carrying)
 #   parsN = rep(0,input$n)
 #   parsN[1:2] = 1
 #   MMM_sim(n = input$n, parsN = parsN,pars = pars,M0 = input$Mig, age = input$Age) 
 #   })
  
   output$lttplot <- renderPlot({ 
     phylo = phylo()
     L = phylo$L
     phy = L2phylo(L)
     phy$tip.label = L[which(L[,4] == -1),5]
     plot(phy)
   },height = 400, width = 600)
   output$tree <- renderPlot({
     phylo = phylo()
     ltt.plot(phylo$tes,log="y")
   },height = 400, width = 600)
   
})

# Run the application 
shinyApp(ui = ui, server = server)

