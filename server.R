library(shiny)
library(ggplot2)
library(plyr)


CitiesTempBarplot <- function (df, xfnt) {
  #Bin the temperatures into 10 degree buckets, using the "cut" funtion
  df$TempBucket <- cut(df$Temperature, breaks=brk, labels=label10s)
  p <- ggplot(data=df, aes(City, fill=TempBucket )) + geom_bar()  
  p <- p + scale_fill_manual(values=wx_range(11))
  p <- p + ylab("Number of Hours by Bucket")
  p <- p + labs(title = "Comparing Temperature Distributions Across Cities (2012) ")
  p <- p + theme( #eliminate background, gridlines, and chart border
    plot.background = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_rect("black")
    #,axis.text.y=element_blank()
    ,axis.ticks=element_blank()
    ,axis.title.x=element_blank()
    ,axis.text.x = element_text(colour="grey20",size=xfnt,angle=0,hjust=.5,vjust=.5,face="plain")
  )  
  
  return(p)  
}


MonthlyMaxMinOrMeanPlot <- function(df, str.column.to.plot) {
  p<- ggplot(data=df, aes_string(x="Month", y=str.column.to.plot, group="City", color="City"))  
  p<- p+geom_point(size=5)
  p<- p+geom_line(size=1, alpha=0.9)
  #p<- p+geom_linerange(size=2)  #ymin and ymax have to be specified for linerange
  p<- p + geom_hline(yintercept=c(50,60,70,80))
  p<- p + theme(panel.background = element_rect(fill= "transparent"))
  p<- p+xlab("Month 2012")
  p<- p+labs(title=paste(str.column.to.plot, " Temperatures For Cities, by Month"))
  return(p)
}


MeanMaxAndMinPlot <- function(df) {
  p <- ggplot(df) 
  p <- p + geom_linerange(aes(x=City, 
                            y = MeanDmin, ymin=MeanDmin, ymax=MeanDmax, 
                               color=City, size=3)) + coord_flip()
  p <- p + facet_grid(Month ~ .)
  p <- p + xlab("Mean of Daily_Minimum Temperature to Mean of Daily_Maximum Temperature") 
  p <- p + theme( #eliminate background, gridlines, and chart border
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_line(colour="blue", size=0.5)
    ,panel.grid.minor = element_line(colour="black", size=0.3)
    ,axis.ticks=element_blank()
    ,axis.title.y=element_blank()
    ,axis.text.x =element_text(colour="grey20",angle=0,hjust=.5,vjust=.5,face="plain")
    ,legend.position="none"
  )
  
  return(p)
}


compareCitiesHistograms <- function (df, opt.bin.width=1) {
  numcolors <- length(unique(df$Temperature)) #how many colors do we need?
  
  m <- ggplot(df, aes(x=Temperature, fill=factor(Temperature)))
  m <- m + geom_histogram(binwidth=opt.bin.width)
  m <- m + scale_fill_manual(values=wx_range(numcolors))
  m <- m + facet_grid(City ~ .)
  m <- m + ylab("Count of Hours")
  m <- m + labs(title = "Comparing Temperature Distributions Across Cities (2012)")
  m <- m + geom_vline(xintercept=c(75,100),
                      colour="#990000", linetype="dashed")
  m <- m + geom_vline(xintercept=c(32,50),
                      colour="blue", linetype="dashed")
  m <- m + theme(legend.position="none")
  
  return(m) #return the plot obj to the ShinyServer
}

##################
##################


#Calculations that are needed. But not reactive go here
dir <- "wxdata/"
flist <- list.files(dir)
flist <- paste0(dir, flist) #full path with no spaces
city.temp.list <- lapply(flist, read.csv, header=FALSE, stringsAsFactors=FALSE)
city.temp.df <- ldply(city.temp.list) #make it one giant data.frame
names(city.temp.df) <- c("City","Date","Hour","Temperature")

#unique(city.temp.df$City)
#str(city.temp.df)

cities <- unique(city.temp.df$City)
#print(cities)


#Needed for Temp Barplots
brk = c(seq(-10,100,10),1000)
label10s = c(as.character(seq(0,100,10)),">100")

#Color Schemes
wx_range<-colorRampPalette(c(rgb(0,0.5,1), rgb(1,0.35,0) )) #Cold to Hot



#End of Calculations that are needed. But not reactive go here


shinyServer(function(input, output) {      
  
  
  #values and dataframes available to all reactive functions inside Server.R
  city.compare.list <- reactive({
    cl <- NULL #cl is citiesList. We are going to store the indexes of each city checked in it.
    if (input$ash ==1 )    {cl <- c(cl,1)}
    if (input$aus ==1 )    {cl <- c(cl,2)}
    if (input$chs ==1 )    {cl <- c(cl,3)}
    if (input$pgv ==1 )    {cl <- c(cl,4)}
    if (input$iah ==1 )    {cl <- c(cl,5)}    
    if (input$jax ==1 )    {cl <- c(cl,6)}
    if (input$lax ==1 )    {cl <- c(cl,7)}
    if (input$mia ==1 )    {cl <- c(cl,8)}
    if (input$mco ==1 )    {cl <- c(cl,9)}
    if (input$psp ==1 )    {cl <- c(cl,10)}    
    if (input$rdu ==1 )    {cl <- c(cl,11)}
    if (input$sat ==1 )    {cl <- c(cl,12)}
    if (input$lee ==1 )    {cl <- c(cl,13)}
    if (input$tpa ==1 )    {cl <- c(cl,14)}    
    city.compare.list <-  cities[cl]    #this gets returned and can be used by other functions
  })
  
  num.cities <- reactive({
    #print(length(city.compare.list() ))
    length(city.compare.list())
  })
  
  xfontsize <- reactive({
    n <- num.cities()
    if (n <= 5) {fnt=20}
    if((n>5) & (n <= 9)) {fnt=15}
    if (n >= 10) {fnt=10}
    #print( paste( "N and font",n,fnt))
    return(fnt)
  })
  
  ## subset to cities of interest
  city.df <- reactive({
    subset(city.temp.df, City %in% city.compare.list() )
  })  
  
  #Summarize the data into Monthlies. 1 row = 1 month for 1 city
  summarized.df <- reactive({
    df <- city.df()
    df$Month <- months(as.Date(df$Date)) #Create a new column in df
    
    monthly.df <- ddply(df,.(City, Month), summarize,    
                      meanT= round(mean(Temperature),1) ,
                      maxT = round(max(Temperature),0) ,    
                      minT = round(min(Temperature),0) )
    
    # We need two ddply's to calculate the MEAN of the MAXimums
    # and the mean of the Mins
    # Step 1. we first compute daily Maxs and Daily Mins
    daily.max.min <- ddply(df, .(City, Month, Date), summarize, 
                Dmax = max(Temperature),
                Dmin = min(Temperature)
    )

    #Step 2. We now average the Dmax and Dmin values
    city.temp.Mean.of.Max.and.Min <- ddply(daily.max.min, .(City, Month), summarize, 
                                           MeanDmax=round(mean(Dmax), 1), 
                                           MeanDmin=round(mean(Dmin), 1) 
    )
    
    #attach these two columns to the main df and return it
    monthly.df$MeanDmax <- city.temp.Mean.of.Max.and.Min$MeanDmax
    monthly.df$MeanDmin <- city.temp.Mean.of.Max.and.Min$MeanDmin
    
    #Arrange the months in chronological order
    monthly.df <- within(monthly.df, Month <- factor(Month, levels=month.name))
    
    return(monthly.df) 
  })
  
  
  # end of computations
  
  #==============================================================================
  #Begin showing things on the screen
  
  output$opts <- renderText({  
    c("Temperature band Selected: ", input$opt.bin.width)
  })
  
  output$opt.mmm <- renderText({  
    if(input$opt.mmm == "meanT") {"Displaying Mean Temperatures by month"}
    else if(input$opt.mmm == "minT") {"Displaying Min Temperature by month"}
    else if(input$opt.mmm == "maxT") {"Displaying Max Temperature by month"}
    
  })
  
  #   # Show the first "n" observations
  #   output$view <- renderTable({
  #     head(datasetInput(), n = input$obs)
  #   })
  #   
  
  output$WikiChart <- renderPlot({    
    smalldf <- summarized.df()
    
    #ordered_month = factor(Month, levels=month.name)        
    p <- ggplot(data=smalldf, aes(x = factor(Month, levels=month.name),
                                  y=meanT,
                                  ymin=minT, ymax=maxT))
    p <- p + geom_crossbar(width=0.2, fill="red")
    p <- p + geom_text(data=smalldf, aes(y=maxT+5, label=maxT), color="red")
    p <- p + geom_text(data=smalldf, aes(y=minT-5, label=minT), color="blue")
    p <- p + facet_grid(City ~ .)
    #Plot Aesthetics
    p <- p + xlab("Month 2012") + ylab("")
    p <- p+labs(title="City Mean, Max & Min Temperatures, by Month")
    print(p)
  })  
  
  
  output$MeanTempsChart <- renderPlot({    
    smalldf <- summarized.df()
    mPlot <- MonthlyMaxMinOrMeanPlot(smalldf, input$opt.mmm) #meanT = 3, maxT=4, minT=5
    print(mPlot)    
  })  
  
  
  output$MeanTempsTable <- renderPlot({
    smalldf <- summarized.df()
    p<- ggplot(data=smalldf, aes(factor(Month, levels=month.name), City, color=meanT) ) 
    p<-p + geom_text(size=10, label=as.character(round(smalldf$meanT, 0)))
    p<- p + scale_color_gradient(low="blue", high="orange")
    p <- p + theme(panel.background = element_rect(fill= "transparent"))
    p<- p+xlab("Month")
    p<- p+labs(title="Mean Temperatures For Cities, by Month")
    print(p)
  })
  
  
  output$MeanMaxMinPlot <- renderPlot({
    monthly.df <- summarized.df()    
    MMbar <- MeanMaxAndMinPlot(monthly.df)
    print(MMbar)
  }, height=700)
  
  
  output$TempBarPlot <- renderPlot({
    Tbar <- CitiesTempBarplot(city.df(), xfontsize() )
    print(Tbar)
  }, height=600)
  
  
  # Plot multiple cities on one graph, as denisities
  output$TempKDE <- renderPlot({
    colorRange<-colorRampPalette(c(rgb(0,0,1), rgb(1,0.7,0) ))
    p<- ggplot(city.df(), aes(Temperature, color=City)) 
    p<- p + stat_density(position="identity",geom="line", size=2)
    p <- p + geom_vline(xintercept=c(75,100),
               colour="#990000", linetype="dashed")
    p <- p + geom_vline(xintercept=c(32,50),
                        colour="blue", linetype="dashed")
    p <- p + ylab("Density of Hours")
    p <- p + labs(title = "Comparing Temperature Distributions Across Cities (2012) ")
    print(p)    
  })
  
  
  output$TemperatureHistogram <- renderPlot({
    
    #actual plotting
    histPlot <- compareCitiesHistograms(city.df(), as.numeric(input$opt.bin.width))
    print(histPlot)
    
  }, height=600)
  
})
