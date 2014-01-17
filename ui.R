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
    
    p("Compare Ambient Temperatures Across Cities"),
    wellPanel(      
        withTags(
          div(
            h4("US Cities"),
            div( class="row-fluid",
                 div( class="span5", "Atlanta, GA"),            
                 div(class='span3', checkboxInput(inputId = "atl12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "atl13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "Austin, TX"),            
                 div(class='span3', checkboxInput(inputId = "aus12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "aus13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "Honolulu, HI"),            
                 div(class='span3', checkboxInput(inputId = "hnl12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "hnl13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "Los Angeles, CA"),            
                 div(class='span3', checkboxInput(inputId = "lax12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "lax13", label = "2013", value=TRUE))),
            div( class="row-fluid",
               div( class="span5", "Miami, FL"),            
               div(class='span3', checkboxInput(inputId = "mia12", label = "2012", value=FALSE)),
               div(class='span3', checkboxInput(inputId = "mia13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "New York, NY"),            
                 div(class='span3', checkboxInput(inputId = "lga12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "lga13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "Portland, OR"),            
                 div(class='span3', checkboxInput(inputId = "pdx12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "pdx13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "Raleigh, NC"),            
                 div(class='span3', checkboxInput(inputId = "rdu12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "rdu13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "San Antonio, TX"),            
                 div(class='span3', checkboxInput(inputId = "sat12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "sat13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "San Francisco, CA"),            
                 div(class='span3', checkboxInput(inputId = "sfo12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "sfo13", label = "2013", value=TRUE))),
            h4("Outside the US"),
            div( class="row-fluid",
                 div( class="span5", "London, UK"),            
                 div(class='span3', checkboxInput(inputId = "lhr12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "lhr13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "Sydney, AUS"),            
                 div(class='span3', checkboxInput(inputId = "syd12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "syd13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "Brisbane, AUS"),            
                 div(class='span3', checkboxInput(inputId = "bne12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "bne13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "Singapore, SIN"),            
                 div(class='span3', checkboxInput(inputId = "sin12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "sin13", label = "2013", value=FALSE))),
            div( class="row-fluid",
                 div( class="span5", "Mumbai, IND"),            
                 div(class='span3', checkboxInput(inputId = "bom12", label = "2012", value=FALSE)),
                 div(class='span3', checkboxInput(inputId = "bom13", label = "2013", value=FALSE)))
            
          ))
        
    ),#ends wellpanel    
    
    
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
    h2("Temperature Comparisons across Cities"),
    tabsetPanel(id ="graphtabs",                  
      tabPanel("Histogram", 
                 sliderInput(inputId = "opt.bin.width",
                             label = "Choose Temperature Band (degrees F)",
                             min = 1, max = 10, step = 1, value = 2), 
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
#      , tabPanel("About", textOutput("TempKDE"))
      
    )#end tabsetPanel
  ) #end MainPanel
  
  
  
))