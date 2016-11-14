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
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Mig",
                     "Migration rate:",
                     min = 0,
                     max = 10,
                     value = 1),
         sliderInput("Age",
                     "Age:",
                     min = 10,
                     max = 40,
                     value = 10),
         sliderInput("Speciation",
                     "Speciation rate:",
                     min = 0.5,
                     max = 3,
                     value = 0.8),
         sliderInput("Extinction",
                     "Extinction rate:",
                     min = 0,
                     max = 1,
                     value = 0.4),
         sliderInput("Carrying",
                     "Carrying capacity:",
                     min = 20,
                     max = 100,
                     value = 20)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # fluidRow(
        #   column(4,
        #          imageOutput("lttplot", height = 300),
        #          imageOutput("tree")
        #   )
        # )
        tabPanel("Plot",
                 fluidRow(
                   column(8, plotOutput("lttplot")),
                   column(12, plotOutput("tree"))
                 ))
      )
      )
  
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
 phylo <- reactive({
   pars = c(input$Speciation,input$Extinction, input$Carrying)
   MMM_sim(n = 4, parsN = c(1,1,0,0),pars = pars,M0 = input$Mig, age = input$Age) 
   })
  
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

