library(ggplot2)

compareCitiesTempHistograms <- function (df, opt.bin.width=1) {
  numcolors <- length(unique(df$Temperature)) #how many colors do we need?
  redgreenrange<-colorRampPalette(c(rgb(1,0,0), rgb(0,0.7,0) ))
  temp.color.range<-colorRampPalette(c(rgb(0,0,1), rgb(0.7,0.3,0) ))
  
  m <- ggplot(df, aes(x=Temperature, fill=factor(Temperature)))
  m <- m + geom_histogram(binwidth=opt.bin.width)
  m <- m + scale_fill_manual(values=temp.color.range(numcolors))
  
  m <- m + facet_grid(City ~ .)
  m <- m + ylab("Count of Hours")
  m <- m + labs(title = "Comparing Temperature Distributions Across Cities (2012)")
  return(m) #return the plot obj to the ShinyServer
}


plotCitiesTempBarplot <- function (df, xfnt) {
  #Bin the temperatures into 10 degree buckets, using the "cut" funtion
  df$TempBucket <- cut(df$Temperature, breaks=brk, labels=label10s)
  p<- ggplot(data=df, aes(City, fill=TempBucket )) + geom_bar()  
  p<- p + scale_fill_manual(values=wx_range(11))
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



#The actual plotting happens here.
artZones <- function(df, opt.colors) {
  
  redgreenrange<-colorRampPalette(c(rgb(1,0,0), rgb(0,0.8,0) ))
  redbluerange<-colorRampPalette(c(rgb(1,0,0), rgb(0,0,1) ))
  orangebluerange<-colorRampPalette(c(rgb(1,0.6,0), rgb(0,0,1) ))

  
  numcolors <- length(unique(factor(df$z)))
  
  
  segs <- geom_segment(aes(x=xs,y=ys,xend=xe,yend=ye, color=factor(z)))
  p  <- p + segs
  p <- p + scale_color_manual(values=chosenColorRange(numcolors))  + guides(color=FALSE) #hide the legend
  
  p <- p +   #eliminate background, gridlines, and chart border
    theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,panel.background = element_rect("black")
    ) +theme(    #to remove axes
      #axis.text.y=element_blank()
      axis.ticks=element_blank()
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
    )  
  
  return(p)
}




getPresetParameters <- function(opt.pre) {
  if (opt.pre ==1 )    {params<-list(1,20,500,1)}
  if (opt.pre ==2 )    {params<-list(2,10,1000,2)}
  if (opt.pre ==3 )    {params<-list(4,10,1000,1)}
  if (opt.pre ==4 )    {params<- list(3,20,500,3)}
  return(params)
}


# getColors <- function(x,y) {
# #  zx <- as.integer((x-1)/(boxend/numzones)) #x zone
# #  zy <- as.integer((y-1)/(boyend/numzones)) #y zone
# #  z <- as.integer(numzones*zx + zy) #combine
#   z <- as.integer(y-1)
#   z  
# }


getParameters <- function(numpts, boxend, boyend) {
  x <- sample(boxend, numpts+1, replace=T)
  y <- sample(boyend, numpts+1, replace=T)
  #z <- getColors(x,y)
  z <- as.integer(y-1)
  return(list(x,y,z)  )  
}


convertToSegs <- function(numpts, x, y, opt.connect) {
  xs <-NULL; ys<- NULL; xe<-NULL; ye<-NULL
  
  for (i in 1:numpts) {
    
    xs[i] <- x[i] #starting x coord
    ys[i] <- y[i] #starting y coord
    
    
    #the opt.connects determine the length and the direction of the 
    #segements
    if(opt.connect == 4) { #pick up sticks, random length
      xe[i] <- x[i+1]
      ye[i] <- y[i+1]      
    }      
    
    if(opt.connect == 1) { #segments of max length 2 - subway map like lines
      choice <- c(-2,0,2)
      xe[i] <- x[i]+ sample(choice, 1)
      ye[i] <- y[i]+ sample(choice, 1)  
    }      
    
    if(opt.connect == 2) { #sparkle, more choices for the angle
      choice <- c(-2:2)
      xe[i] <- x[i]+ sample(choice, 1)
      ye[i] <- y[i]+ sample(choice, 1)  
    }      
    
    if(opt.connect == 3) { #weave & waft
      v.or.h <- sample(0:1,1)
      if(v.or.h) {
        xe[i] <- x[i]
        ye[i]<- y[i]+1
      }
      else {
        xe[i] <- x[i]+1    
        ye[i]<- y[i]
      }
    }
    
    
  }
  
  return(list(xs,ys,xe,ye))
}

### end of function definitions
