# Author: @ramnarasimhan

library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("shinySketch"),
  
  # input$numpts, input$boxend, input$boyend
  
  sidebarPanel(    
    p("Select a sketch from the gallery, or better yet", strong("Create your own!")),
    h5("Sketch Gallery"),
    wellPanel(      
      selectInput("opt_preset", "",
                  list(
                    "Preset 1: Underground Metro" = 1, 
                    "Preset 2: Crystal in Red & Blue" = 2, 
                    "Preset 3: Gossamer Threads" = 3,                 
                    "Preset 4: Maze Construction" = 4,
                    "Create Your Own Sketch" = 5
                  ),                 
                  selected=1)             
    ),
    
    #display only if Create Your Own
    conditionalPanel("input.opt_preset == '5' ",
                     
                     h5("Create Your Own Sketch"),                     
                     h5("Sketch Type"),
                     selectInput("opt.connect", "",
                                 list("Subway Map" = 1,
                                      "Crystal Lattice" = 2, 
                                      "Mazes" = 3, 
                                      "Pick up sticks" = 4),                 
                                 selected = 2),       
                     
                     h6("Canvas Size"),
                     radioButtons("boxend", "", 
                                  c( "10" = 10,
                                     "20" = 20,
                                     "50" = 50)),
                     
                     selectInput("numpts", "Number of Lines",
                                 choices = c(200, 500, 1000),
                                 selected = 1000),                                   
                     
                     h6("Colors"),    
                     radioButtons("opt.colors", "", 
                                  c("Red to Green" = 1, 
                                    "Red to Blue" = 2, 
                                    "Orange to Blue" = 3))
    ), #close conditionalPanel
    
    
    #submitButton("Render")
    wellPanel(
      p(strong("Options Selected:")), 
      textOutput("opts")
    ),
    
    p("Powered by",
      a("ggplot", href="http://ggplot2.org"), " and ",
      a("Shiny.", href="http://www.rstudio.com/shiny/"))
    
  ),
  
  
  mainPanel(
    plotOutput(outputId = "artPlot"),
    br()
    
  ) #end Main
  
  
  
))