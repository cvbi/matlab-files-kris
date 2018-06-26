





#### PLots Plots plots

# concatenates volocity files, used in cases where image was cropped
catFiles <- function( files )
{
  for( i in 1:length(files))
  {
    tab <- read.csv(files[i], fileEncoding="latin1", na.strings="N/A", check.names = F)
    tab <- reid(tab)
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



getVar <- gv <- function( df, variable, obj = F )
{
  if( obj == F)
  {
	  inds <- which( df$Type == "Track")
	  tracks <- df[inds,]
	  df <- data.frame(  tracks$"Item Name", tracks[,variable] )
	  colnames(df) <- c( "Population", variable )
	  return(df)
  }
  else
  {
    inds <- which( df$Type == "Object") 

    tracks <- df[inds,]

    df <- data.frame(  tracks$"Item Name", tracks[,variable] )

    colnames(df) <- c( "Population", variable )
    return(df)   
    
  }
}


bindLst <- function(lst)
{
  for( i in 1:length(lst)){
    if( i == 1){
      df <- lst[[i]]
    }
    else{
      df <- rbind(df, lst[[i]])
    }
  }
  return(df)
}


plotIt <- function( df )
{
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
  xx <- lapply(lst, gv, variable = "Rel. Centroid X (µm)", obj = T)
  yy <- lapply(lst, gv, variable = "Rel. Centroid Y (µm)", obj = T)
  zz <- lapply(lst, gv, variable = "Rel. Centroid Z (µm)", obj = T)

  
  xx <- bindLst( xx )
  yy <- bindLst( yy )
  zz <- bindLst( zz )

  df <- data.frame( Population = xx[,1], x = xx[,2], y = yy[,2], z = zz[,2], id = 0  )
  for( i in 2:ncol(df))
  {
  	df[,i] <- as.numeric(as.character(df[,i]))
  }
  #print(df)

  
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
  
  return(list(xy, zy))
  
}


matrixPlot <- function(df)
{ 
  colnames(df) <- c("Population",  "Track_Velocity", "Displacement", "Meandering_Index", "Angle")
  
  for( i in 2:ncol(df) )
  {
    df[,i] <- as.numeric(df[,i])
  }
  
  scatterplotMatrix( ~Track_Velocity + Displacement + Meandering_Index | Population, data =df,
                     main="Matrix Plot")
}

draft <- function()
{
  
  table(cut(df[,2],breaks=seq(0, 360, 10),labels=paste(as.character(seq(0, 350, 10)),"%",sep="")))
  
}

getArrestCoeff <- function( df, coeff )
{
  mat <- goc( df, "Velocity (µm/sec)", object = T )   
  vec <- vector()
  
  # takes care leading zero
  for( i in 1:nrow(mat) )
  {
    ind <- which( as.numeric(as.character(mat[i,])) == 0 )[1]
    mat[i, ind] <- NA
    vec[i] <- mean( mat[i,] < coeff, na.rm = T ) * 100
  }
  dff <- data.frame( Arrest_Coefficient = vec, Population = df$"Item Name"[1]   )
}




makeAC <- function( lst )
{
  
  lstAC <- lapply( lst, getArrestCoeff, coeff = 0.02 )
  df <- bindLst( lstAC )
  df
  
  cuts <- cut(df[,1],breaks=seq(0, 100, 10),labels=paste(as.character(seq(0, 90, 10)),"%",sep=""), include.lowest = T)
  dff <- df
  #dff[,1] <- cuts
  
  #c <- ggplot(dff, aes(factor(Arrest_Coefficient), fill = Population) )
  #c <- ggplot(dff, aes(Arrest_Coefficient) )+ geom_bar(position="dodge") +  facet_grid(Population ~ .)
  #c + scale_x_continuous("", limits = c(0, 100), breaks = seq(0, 100, 10), labels = seq(0, 100, 10))
  #c
  
  c <- ggplot(dff, aes(Arrest_Coefficient) ) +geom_histogram(binwidth=10) + xlim(0,100)+  facet_grid(Population ~ .)
  c <- c + labs( y = "Count", x = "Arrest Coefficient (%)", title = "Arrest Coefficient")
  #c + geom_bar(position="dodge")
  c
  
  p <- ggplot(dff, aes(x=Arrest_Coefficient) ) + facet_grid(Population ~ .) + stat_ecdf() + xlim(0, 100)
  p <- p + labs( y = "Cumulative Density", x = "Arrest Coefficient (%)", title = "Arrest Coefficient")
  p 
  #table(cut(df[,1],breaks=seq(0, 100, 10),labels=paste(as.character(seq(0, 90, 10)),"%",sep="")))
 
  lst <- list( C = c, P = p) 
}












#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions



# erases objects from that do not meet the set timespan threshold, the threshold is inclusive 
deleteByTimespan <- dbt <- function( df, threshold )
{
  # finds which tracks have timespan less than threshold	
  tracks <- df[ df$Type == "Track", ]
  
  inds <- which( as.numeric(as.character(tracks$"Time Span")) < threshold )
  #print(inds)
  if( length(inds) != 0 ){
    ids <- tracks$"Track ID"[inds]
    #print("ids")
    #print(ids)
    inds2 <- which(df$"Track ID" %in% ids)
    if( length(inds2) != 0 ){
      df <- df[ -inds2, ]  
    }
  }
  return( df )
}


# erases objects and tracks associated with given trackIds
deleteByTrackID <- dbtid <- function( df, trackIds ) 
{
  inds <- which(df$"Track ID" %in%  trackIds)
  if( length(inds) != 0 )
  {
    df <- df[ -inds, ]
  }
  
  return( df )
}


translateName <- function( name )
{
  return(name)
}

# makes a matrix of the all the tracks for one measurement, the column names must match the wonky R names. just use colnames(objects) to find the name
# changed to use new track.ID
getObjectsColumn <- goc <- function ( df, columnName, object )
{
  
  
  if( object == T )
  {
    objects <- df[ df$Type == "Object", ]
    cols <- max( as.numeric(objects$Timepoint), na.rm=T )
    NameColumns <- unique(sort( as.numeric(as.character(objects$"Rel. Time (s)") )))
  }
  else
  {
    objects <- df[ df$Type == "Track", ] 
    cols <- 1
    NameColumns <- columnName
  }
  
  
  #objects <- lst[[1]]
  
  coln <- colnames(objects) == columnName
  
  mat <- data.frame(matrix( NA, nrow = length(unique( objects$"Track ID" )), ncol = cols ))
  
  row.names( mat ) <- unique( objects$"Track ID" )
  colnames ( mat ) <- NameColumns
  

  #colnames ( mat ) <- 1:max( objects$Timepoint )
  #colnames ( mat ) <- 0:max( objects$Timepoint )-1
  
  for( i in 1:nrow( objects ) )
  {
    #mat[ as.character( objects[i, 48] ), as.character( objects[i, 8] ) ]   <-  objects[i, coln]	
   if( object == T ){
       mat[ as.character( objects$"Track ID"[i] ), as.character( objects$"Rel. Time (s)"[i] ) ]   <-  as.character(objects[i, coln])
    }else{
      mat[ as.character( objects$"Track ID"[i] ), 1 ]   <-  objects[i, coln]
    }
  }
  #head(mat)
  
  return(mat) 
}


# removes NA tracks
# removes Tracks that have no ID, these are tracks only have an object at one time point
removeNas <- function( df )
{ 
	inds <- which( is.na(df$"Track ID") )
	if( length(inds) > 0 )
	{
		df <- df[-inds, ]
	}
	return(df)
}


# resets the track-id and id, only for objects dataframe, because dulicates can arise.
reid <- function( df )
{
  start <- df$"Track ID"[1]
  ind <- 1
  vec <- vector()
  
  for( i in 1:nrow(df) )
  {
    current <- df$"Track ID"[i]
    #print(current)
    if( current == start )
    {
      vec[i] <- ind
    }
    else
    {
      start <- current
      vec[i] <- ind + 1
      ind <- ind + 1
    }
  }	
  
  # in case this is a file that already contains a new.Id Column, removes the column 
  if( "New.Track.ID" %in% colnames(df) )
  {
  	ind <- which(colnames(df) %in% "New.Track.ID")
  	if( length(ind) != 0 ){
    	df <- df[,-ind]
  	}
  }
  
  df <- cbind( df, New.Track.ID = vec )
  
  return(df)
}


# concatenates volocity files, used in cases where image was cropped
catVol <- function( files )
{
  for( i in 1:length(files))
  {
    tab <- read.csv(files[i], fileEncoding="latin1", na.strings="N/A", check.names = F)
    if( "New.Track.ID" %in% colnames(tab) )
    {
      tab <- read.csv(files[i], fileEncoding="latin1", check.names = F)
    }
    
    
    if( i == 1 )
    {
      df <- tab
    }
    else
    {
      df <- rbind( df, tab )
    }
  }
  return(df)
}


main <- function( files, ids, timespan )
{
  df <- catVol( files )
  df <- removeNas( df )
  df <- reid( df )
  df <- dbtid( df, ids )
  df <- dbt( df, timespan )
  
  return(df)
}


### TESTing
### TESTing
### TESTing
### TESTing
### TESTing
### TESTing
### TESTing
### TESTing
### TESTing
### TESTing
### TESTing



tasting <- function()
{
  
  setwd("/Users/chrisbarilla/Desktop/UofR/Code/Volocity/Day 7 clean raw data/")
  fi <- list.files()
  fi
  f1 <- fi[8]
  f2 <- fi[11]
  files <- c(f1, f2)
  lst <- catFiles( files )

  
  lstAC <- lapply( lst, getArrestCoeff, coeff = 0.02 )
  df <- bindLst( lstAC )
  df
  
  cuts <- cut(df[,1],breaks=seq(0, 100, 10),labels=paste(as.character(seq(0, 90, 10)),"%",sep=""), include.lowest = T)
  dff <- df
  #dff[,1] <- cuts
  
  #c <- ggplot(dff, aes(factor(Arrest_Coefficient), fill = Population) )
  #c <- ggplot(dff, aes(Arrest_Coefficient) )+ geom_bar(position="dodge") +  facet_grid(Population ~ .)
  #c + scale_x_continuous("", limits = c(0, 100), breaks = seq(0, 100, 10), labels = seq(0, 100, 10))
  #c
  
  c <- ggplot(dff, aes(Arrest_Coefficient) ) +geom_histogram(binwidth=10) + xlim(0,100)+  facet_grid(Population ~ .)
  c <- c + labs( y = "Count", title = "Arrest Coefficient")
  #c + geom_bar(position="dodge")
  c
  
  p <- ggplot(dff, aes(x=Arrest_Coefficient) ) + facet_grid(Population ~ .) + stat_ecdf() + xlim(0, 100)
  p <- p + labs( y = "Cumulative Density", title = "Arrest Coefficient")
  p 
  #table(cut(df[,1],breaks=seq(0, 100, 10),labels=paste(as.character(seq(0, 90, 10)),"%",sep="")))
  
  

  
  
   
  mat <- goc( lst[[1]], "Velocity. (µm/sec)", object = T )
  mat <- goc( lst[[2]], "Velocity. (µm/sec)", object = T )
  mat  
  
  vf <- c("Track.Velocity..µm.sec.", "Displacement..µm.", "Meandering.Index.Cal.","Angle..degrees." )
  df <- lapply( lst, gv, variable = vf)
  df <- bindLst( df )
  lst2 <- list(df)
  
  #matrixPlot(lst2[[1]]) 
  
  df <- lst2[[1]]
  
  colnames(df) <- c("Population",  "Track_Velocity", "Displacement", "Meandering_Index", "Angle")
  
  for( i in 2:ncol(df) )
  {
    df[,i] <- as.numeric(df[,i])
  }
  
  scatterplotMatrix( ~Track_Velocity + Displacement + Meandering_Index | Population, data =df,
                     main="Matrix Plot")
  
  
  
  
  tab2 <- read.csv("/Users/cbarilla/Desktop/OutFile.csv", fileEncoding="latin1", na.strings="N/A", check.names = F)
  
  
  
  tab <- read.csv("/Users/cbarilla/Desktop/D9 041113_03 c4 qc.csv", fileEncoding="latin1",na.strings="N/A",  check.names = F)
  tab2 <- read.csv("/Users/cbarilla/Desktop/OutFile.csv", fileEncoding="latin1", check.names = F)
  
  factor(tab$`Velocity (µm/sec)`)
  factor(tab2$`Velocity (µm/sec)`)
  
  1 < tab$`Velocity (µm/sec)`[1]
  1 < tab2$`Velocity (µm/sec)`[1]
  
  is.factor(tab2$`Velocity (µm/sec)`[1])
  is.factor(tab$`Velocity (µm/sec)`[1])
  
  
  
  
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




