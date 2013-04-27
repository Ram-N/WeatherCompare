library(shiny)
library(ggplot2)



#Calculations that are needed. But not reactive go here

# Objects in this file are shared across all sessions
source('functionsArt.R', local=TRUE)



#artPlot <- artZones(df, opt.segs)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {      
  
  output$opts <- renderText({  
    
    opt.pre <- as.numeric(input$opt_preset)
    
    if(opt.pre == 5) {
      c( "Sketch Type:", input$opt.connect, 
         "Canvas:", input$boxend,
         "Lines",input$numpts,
         "Color:", input$opt.colors 
      ) 
    }
    else {
      c("Rendering Gallery Image:", opt.pre)
    }
    
  })
  
  
  output$artPlot <- renderPlot({
    #your plotting code goes here
    df <- NULL
    xyz <- NULL
    opt.pre <- as.numeric(input$opt_preset)
    
    
    if(opt.pre == 5) {
      boxend <- as.numeric(input$boxend)
      numpts <- as.numeric(input$numpts)
      opt.connect <- as.numeric(input$opt.connect) #connect scheme
      opt.colors <- as.numeric(input$opt.colors)
    }
    else {
      pre <- getPresetParameters(opt.pre)
      
      opt.connect <- pre[[1]]
      boxend <- pre[[2]] 
      numpts <- pre[[3]]
      opt.colors <- pre[[4]]      
    }
    
    #get ready to plot
    xyz <- getParameters(numpts, boxend, boxend)
    combo <- convertToSegs(numpts, xyz[[1]], xyz[[2]], opt.connect)
    xs <- combo[[1]]; ys <- combo[[2]]
    xe <- combo[[3]]; ye <- combo[[4]]
    z <- xyz[[3]]
    z <- z[1:numpts] #drop the last one
    
    #make one data frame for ggplot
    df <- data.frame(xs, ys, xe, ye, z)
    
    #actual plotting
    artPlot <- artZones(df, opt.colors)
    print(artPlot)
    
  }, height=700)
  
})
