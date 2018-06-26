
xgetVar <- xgv <- function( df, variable )
{
  for( i in 1:length(lst))
  {
    if( i == 1 )
    {
      tab <- lst[[i]]
      inds <- which( tab$Type == "Track")
      tracks <- tab[inds,]
      df <- data.frame(  tracks$Item.Name, tracks[,variable] )
    }
    else
    {
      df <- rbind( df, data.frame( tracks$Item.Name, tracks[,variable] )  )
    }
  }
  
  
  
  return(df)
}



# concatenates volocity files, used in cases where image was cropped
catFiles <- function( files )
{
  for( i in 1:length(files))
  {
    tab <- read.csv(files[i], fileEncoding="latin1", na.strings="N/A")
    if( i == 1 )
    {
      lst <- list( tab )
    }
    else
    {
      lst[[i]] <- tab
    }
  }
  return(lst)
}












loader <- function(packVec, bioc = F ){	
  
  for( i in 1:length(packVec)){
    if(require( packVec[i], character.only = T) == F){	
      
      if( bioc == F )
      {
        install.packages( packVec[i] )
      }
      else
      {
        source("http://www.bioconductor.org/biocLite.R")
        biocLite( packVec[i], dependencies = TRUE, suppressUpdates = F, suppressAutoUpdate = F, ask = F )
      }
    }
    library(packVec[i], character.only = T)
  }
  
}







getVar <- gv <- function( df, variable, obj = F )
{
  if( obj == F)
  {
  inds <- which( df$Type == "Track")
  tracks <- df[inds,]
  df <- data.frame(  tracks$Item.Name, tracks[,variable] )
  colnames(df) <- c( "Population", variable )
  return(df)
  }
  else
  {
    inds <- which( df$Type == "Object")
    tracks <- df[inds,]
    df <- data.frame(  tracks$Item.Name, tracks[,variable] )
    colnames(df) <- c( "Population", variable )
    return(df)   
    
  }
}


bindLst <- function(lst)
{
  for( i in 1:length(lst))
  {
    if( i == 1)
    {
      df <- lst[[i]]
    }
    else
    {
      df <- rbind(df, lst[[i]])
    }
  }
  return(df)
}


plotIt <- function( df )
{
  loader( "ggplot2" )

  variable = colnames(df)[2]
  df[,2] <- as.numeric(as.character(df[,2]))
  
  p <- ggplot(df, aes(factor(Population), df[,2]))
  p <- p + geom_point(position = position_jitter(width = 0.2)) 
  p <- p + geom_boxplot(outlier.colour = NA, fill = NA)
  p <- p + labs(x = "", y = "", title = variable)
  #p <- p + facet_grid(Population ~ ., margins = T)
  p <- p +theme(axis.text.x = element_text(angle = 30, hjust = 1))
  p  

}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

makePlot <- function( lst, variable )
{
  df <- lapply( lst, gv, variable = variable)
  df <- bindLst( df )
  p <- plotIt( df )
  return(p)
}




circularPlot <- function( lst, variable )
{
  loader("ggplot2")
  
  df <- lapply( lst, gv, variable = variable)
  df <- bindLst( df )
  
  colnames(df) <- c("Population", "Angle")
  
  p <- ggplot(df, aes(x = Angle,y = ..density.., colour= factor(Population))) + geom_histogram(breaks = seq(0,360, 10), colour = "grey") + coord_polar(start = 0) + theme_minimal() 
  #p <- ggplot(df, aes(x = Angle, colour= factor(Population))) + geom_histogram(breaks = seq(0,360, 10), colour = "grey") + coord_polar(start = 0) + theme_minimal() 
  #p <- ggplot(df, aes(x = Angle, y = ..density.., colour= factor(Population))) + geom_histogram(breaks = seq(0,360, 10) ) + coord_polar(start = 0) + theme_minimal() 
  p <- p + scale_fill_brewer() + ylab("Density") + ggtitle("Bearing..degrees") 
  p <- p +  scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 360, 10), labels = seq(0, 360, 10))
  p <- p + facet_grid(Population ~ .)
  return(p)
}


zeroXYZ <- function( df )
{
  df1 <- df[  -which( is.na( df[,2] ) ), ]
  
  start <- df1[1,5]
  current <- df1[1,5]
  adjustX = 0 - df1$x[1]
  adjustY = 0 - df1$y[1]
  adjustZ = 0 - df1$z[1]
  
  for( i in 1:nrow( df1 ) )
  {
    current <- df1[i,5]
    if( start != current )
    {
      start <- current
      adjustX = 0 - df1$x[i]
      adjustY = 0 - df1$y[i]
      adjustZ = 0 - df1$z[i]
      df1$x[i] <- df1$x[i] + adjustX
      df1$y[i] <- df1$y[i] + adjustY
      df1$z[i] <- df1$z[i] + adjustZ
    }
    else
    {
      df1$x[i] <- df1$x[i] + adjustX
      df1$y[i] <- df1$y[i] + adjustY				
      df1$z[i] <- df1$z[i] + adjustZ
    }
  }
  
  return(df1)
}

plot3d <- function( lst )
{
  
  xx <- lapply(lst, gv, variable = "Rel..Centroid.X..µm.", obj = T)
  yy <- lapply(lst, gv, variable = "Rel..Centroid.Y..µm.", obj = T)
  zz <- lapply(lst, gv, variable = "Rel..Centroid.Z..µm.", obj = T)
  
  xx <- bindLst( xx )
  yy <- bindLst( yy )
  zz <- bindLst( zz )
  
  df <- data.frame( Population = xx[,1], x = xx[,2], y = yy[,2], z = zz[,2], id = 0  )
  
  lever=F
  ind <- 1
  for( i in 1:nrow(df))
  {	
    if ( is.na(sum(df[i,2:4])) )
    {
      df[i,2:4] <- NA
      df[i, 5] <- ind
      lever = T
    }
    else if( sum(df[i,2:4]) == 0     )
    {
      df[i,2:4] <- NA
      df[i, 5] <- ind + 1
      ind <- ind + 1
      lever = T
    }
    else if( lever == T )
    {
      df[i,2:4] <- NA
      df[i, 5] <- ind
      lever = F
    }
    else
    {
      df[i, 5] <- ind
    }
  }	
  
  df1 <- zeroXYZ ( df )
  
  th <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")
  fc <- facet_grid( . ~ Population )
  
  xy <- ggplot( df1, aes(x,y,  colour = factor(id) )) + geom_point() + geom_line() + th + fc
  zy <- ggplot( df1, aes(z,y,  colour = factor(id) )) + geom_point() + geom_line() + th + fc
  
   #multiplot( xy,  zy, cols=2 )

  
  #df <- bindLst( list(xx, yy, zz) ) 
  
  #p <- plotIt( df )
  return(list(xy, zy))
  
}


matrixPlot <- function(df)
{
  
  
  colnames(df) <- c("Population",  "Track_Velocity", "Displacement", "Meandering_Index", "Angle")
  
  scatterplotMatrix( ~Track_Velocity + Displacement + Meandering_Index | Population, data =df,
                     main="Matrix Plot")

  
}

draft <- function()
{
  
  table(cut(df[,2],breaks=seq(0, 360, 10),labels=paste(as.character(seq(0, 350, 10)),"%",sep="")))
  
}

tasting <- function()
{
  f1 <- "/Users/chrisbarilla/Desktop/UofR/Code/Volocity/GUI/testingGui/D7 RR MHC I all.csv"
  f2 <- "/Users/chrisbarilla/Desktop/UofR/Code/Volocity/Day 7 clean raw data/moddedOk.csv"
  f1 <- "/Users/chrisbarilla/Desktop/UofR/Code/Volocity/Day 7 clean raw data/D7 043013_02 a qc.csv"
  f2 <- "/Users/chrisbarilla/Desktop/UofR/Code/Volocity/Day 7 clean raw data/D7 042413_02 a qc.csv"
  
  files <- c(f1, f2)
  #tab <- read.csv(files[i], fileEncoding="latin1", skip=1, na.strings="N/A")
  
  #lst <- sapply( files, read.csv, fileEncoding="latin1", skip=0, na.strings="N/A"  )
  lst <- catFiles( files )
  
  
  df <- lapply( lst, gv, variable = "Meandering.Index.Cal.")
  df <- bindLst( df )
  p <- plotIt( df )
  
  p1 <- makePlot(lst, "Track.Velocity..µm.sec.")
  p2 <- makePlot(lst, "Displacement..µm.")
  p3 <- makePlot(lst, "Meandering.Index.Cal.")
  p4 <- makePlot(lst, "Angle..degrees.")
  
  df <- lapply( lst, gv, variable = "Bearing..degrees.")
  df <- bindLst( df )
  
  head(df)
  p5 <- circularPlot (df, "Bearing..degrees." )
  
  
  p5 <- circularPlot (lst, "Bearing..degrees." )
  
  p6 <- makePlot(lst, "Elevation..degrees.")
  
  lst <- list( p1, p2, p3, p4, p5, p6 )

  return(p)
}






