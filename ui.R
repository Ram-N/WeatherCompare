#'
#'  @author: @ramnarasimhan

library(shiny)

mmhtmlstring <- paste0("<div>For each colored bar, the left end is the mean of the Daily Minimum temperature. <br> The right end is the mean of the Daily Maximum temperature</div>")

shinyUI(pageWithSidebar(  
  # Application title
  headerPanel("Ambient Temp Compare"),
  
  sidebarPanel( 
    tags$head(
      tags$style(type="text/css", ".jslider { max-width: 200px; }"),
      tags$style(type='text/css', ".well { max-width: 310px; }")
    ),
    
    p("Compare Historical Temperatures Across Locations"),
    wellPanel(      
        h5("Locations to choose from"),
        checkboxInput(inputId = "ash", label = "Ashville, NC", value = FALSE),
        checkboxInput(inputId = "aus", label = "Austin, TX", value = FALSE),
        checkboxInput(inputId = "chs", label = "Charleston, NC", value = FALSE),
        checkboxInput(inputId = "pgv", label = "Greenville, NC", value = FALSE),
        checkboxInput(inputId = "iah", label = "Houston, TX", value = FALSE),
        checkboxInput(inputId = "jax", label = "Jacksonville, FL", value = FALSE),
        checkboxInput(inputId = "lax", label = "Los Angeles, CA", value = FALSE),
        checkboxInput(inputId = "mia", label = "Miami, FL", value = TRUE),
        checkboxInput(inputId = "mco", label = "Orlando, FL", value = FALSE),
        checkboxInput(inputId = "psp", label = "Palm Springs, CA", value = FALSE),
        checkboxInput(inputId = "rdu", label = "Raleigh, NC", value = TRUE),
        checkboxInput(inputId = "sat", label = "San Antonio, TX", value = FALSE),
        checkboxInput(inputId = "lee", label = "Shantiniketan, FL", value = FALSE),
        checkboxInput(inputId = "tpa", label = "Tampa, FL", value = FALSE)
              
        
    ),    
    
    
    wellPanel(
      conditionalPanel(
        condition = "input.graphtabs=='Histogram'",
        p(strong("Options Selected:")), 
        textOutput("opts")
        ),
      conditionalPanel(
        condition = "input.graphtabs=='Mean Temperature Chart'",
        textOutput("opt.mmm")
      )
    ),
    
    p("Powered by",
      a("ggplot", href="http://ggplot2.org"), " and ",
      a("Shiny.", href="http://www.rstudio.com/shiny/"))
    
    ), #close sidebarPanel
  
  
  mainPanel(
#    h2("Which City has the Ideal Temperature for me?"),
    h2("Temperature Comparisons across Cities"),
    tabsetPanel(id ="graphtabs",
                  
      tabPanel("Histogram", 
                 sliderInput(inputId = "opt.bin.width",
                             label = "Choose Temperature Band (degrees F)",
                             min = 1, max = 10, step = 1, value = 1), 
               plotOutput(outputId="TemperatureHistogram", width = "800px", height = "800px")
               )
      , tabPanel("Bar Plot", plotOutput("TempBarPlot", height="800px"))
      , tabPanel("Mean Temperature Table", plotOutput("MeanTempsTable", height="800px"))
      , tabPanel("Wiki-Style Chart", plotOutput("WikiChart", height="500px"))
      , tabPanel("Mean Temperature Chart", 
                 selectInput("opt.mmm", "",
                             list("Mean Temperature by Month" = "meanT",
                                  "Max Temperature by Month" = "maxT", 
                                  "Min Temperature by Month" = "minT"),                 
                             selected="meanT"), 
                 plotOutput("MeanTempsChart", height="500px")
                 )
      , tabPanel("Mean Max. & Min", HTML(mmhtmlstring), plotOutput("MeanMaxMinPlot", height="600px") )
      , tabPanel("Density Plot", plotOutput("TempKDE"))
      
    )#end tabsetPanel
  ) #end MainPanel
  
  
  
))