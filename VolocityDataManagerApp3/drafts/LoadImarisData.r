

vec1 <- c("Track Displacement Length", "Track Duration", "Track Area Mean", "Track Length",  "Track Speed Max", "Track Speed Mean", "Track Speed Min", "Track Speed StdDev", "Track Speed Variation", "Track Sphericity Mean", "Track Straightness", "Track Volume Mean" )
vec1 <- gsub(" ", "_", vec1)
#vec2 <- c( "Time", "Sphericity", "Speed", "Volume", "Area", "Ellipticity (prolate)", "Ellipticity (oblate)")
vec2 <- c( "Time", "Speed", "Volume", "Area" )
vec2 <- gsub(" ", "_", vec2)
vec4 <- c("Position")

features <- c(vec1, vec2,  vec4)

makeHash <- function(df)
{
  vec <- vector()
  for ( i in 1:nrow(df) ){
    vec[i] <- i;
    names(vec)[i] <- paste0( df$TrackID[i], df$Time_Point[i]);
  }
  return(vec)
}

getInd <- function( vec, key )
{
  if( key %in% names(vec) ){
    ind <- which( names(vec) %in% key )
    return( ind )
  } else{
    return(false)
  }
}



makeImsTable <- function( inFile )
{
  
  for( i in 1:length(features) )
  {
    if( sum( grepl(features[i], inFile[,1]) ) < 1 )
      stop("Need all stats csv files to make compile table.")
  }
  
  ind1 <- which( grepl("Area.csv", inFile[,1]) );
  rootname <- substr(inFile[ind1,1], 1, nchar(inFile[ind1,1])-8);
  #print(rootname)
  
  
  ind2 <- which( grepl("Track_Area", inFile[,1]) )[1]
  tab <- read.csv( inFile[ind2,4], skip=3, check.names = F   )  
  tracks <- data.frame( ID = tab$ID, TrackLabel = tab$"Default Labels" )
  tracks <- makeTracks( tracks, rootname, inFile )
  
  
  tab <- read.csv( inFile[ind1,4], skip=3, check.names = F  )  
  surfaces <- data.frame( TrackID = tab$TrackID, Time_Point = tab$Time, Label = tab$"Default Labels") #, ID = tab$ID )
  surfaces <- surfaces[ order(surfaces$TrackID, surfaces$Time_Point), ]
  hashSurf <- makeHash( surfaces )
  surfaces <- makeSurfaces( surfaces, rootname, inFile, hashSurf )
  

  df <- combineTabs( tracks, surfaces )
  df <- df[ order( df$TrackID, df$Time_Point, df$Type ),]  
  return( df )
}


combineTabs <- function( tracks, surfaces )
{
  tcols <- colnames(tracks)
  scols <- colnames(surfaces)
  
  
  surfaces2 <- cbind( Type = "Object", surfaces )
  for( i in 1:ncol(tracks) ){
    surfaces2 <- cbind(  surfaces2, NA )
  }
  colnames(surfaces2) <- c( "Type", colnames(surfaces), colnames(tracks)) 
  
  tracks2 <- tracks
  for( i in 1:ncol(surfaces) )
  {
    tracks2 <- cbind(  NA, tracks2 )
  }
  tracks2 <- cbind(  Type = "Track", tracks2 )
  colnames(tracks2) <- c( "Type", colnames(surfaces), colnames(tracks)) 
  
  
  tracks2$TrackID <- tracks2$ID 
  
  df <- rbind(surfaces2, tracks2)
  
  return(df)
  
}



makeSurfaces <- function( surfaces, rootname, inFile, hashSurf )
{
  # for surfaces
  for( i in 1:length(vec2) )
  {
    indTab <- which( grepl(paste0(rootname, vec2[i], ".csv"), inFile[,1]) )[1]
    tab <- read.csv( inFile[indTab,4], skip=3  ) 
    #tab <- read.csv( paste0(rootname, vec2[i], ".csv"), skip=3  )  
    surfaces <- cbind( surfaces, NA )
    colnames(surfaces)[ncol(surfaces)] <- paste0( vec2[i], "_(", tab$Unit[1], ")"  ) 
    
    for( j in 1:nrow(tab) )
    {
      ind <- getInd( hashSurf, paste0(tab$TrackID[j], tab$Time[j]) )
      if( ind != F )
      {
        surfaces[ ind, ncol(surfaces) ] <- tab$Value[j]
      }
    }
  }

  
  surfaces <- addPositions( surfaces, hashSurf, rootname, inFile )

  inds <- which(is.na(surfaces))
  if( length(inds) != 0){
    surfaces <- surfaces[-inds,]
  }
  return(surfaces)
}

makeTracks <- function( tracks, rootname, inFile )
{
  for( i in 1:length(vec1) )
  {
    indTab <- which( grepl(vec1[i], inFile[,1]) )[1]
    tab <- read.csv( inFile[indTab,4], skip=3  ) 
    if( nrow(tab) != nrow(tracks) )
    {
      stop("The Track files have different dimensions")
    }
    tracks <- cbind( tracks, NA )
    colnames(tracks)[ncol(tracks)] <- paste0( vec1[i], "_(", tab$Unit[1], ")"  ) 
    tracks[ , ncol(tracks)  ] <- tab$Value
  }
  
  tracks <- addPositionsTracks ( tracks, rootname, inFile )
  
  return(tracks)
}

addPositionsTracks <- function( tracks, rootname, inFile )
{
  indTab <- which( grepl("Track_Position_Start", inFile[,1]) )[1]
  
  tab <- read.csv(  inFile[indTab,4], skip=3  )  
  tracks <- cbind( tracks, NA, NA, NA ) 
  cols <- (length(tracks)-2):length(tracks)
  colnames(tracks)[cols] <- paste0( colnames(tab)[1:3], "_(", tab$Unit[1], ")"  ) 
  
  tracks[,cols] <-  tab[, 1:3]
  
  return(tracks)  
  
}

addPositions <- function( surfaces, hash, rootname, inFile )
{
  
  indTab <- which( grepl(paste0(rootname, "Position", ".csv"), inFile[,1]) )
  tab <- read.csv( inFile[indTab, 4], skip=3  )  
  surfaces <- cbind( surfaces, NA, NA, NA )
  cols <- (length(surfaces)-2):length(surfaces)
  colnames(surfaces)[cols] <- paste0( colnames(tab)[1:3], "_(", tab$Unit[1], ")"  ) 
  for( j in 1:nrow(tab) )
  {
    ind <- getInd( hash, paste0(tab$TrackID[j], tab$Time[j]) )
    if( ind != F )
    {
      surfaces[ ind, cols ] <- tab[j, 1:3]
    }
  }
  return(surfaces)
}



#### Functions for uploading dataset ^^^^^^^^^
#### Functions for uploading dataset ^^^^^^^^^
#### Functions for uploading dataset ^^^^^^^^^
#### Functions for uploading dataset ^^^^^^^^^
#### Functions for uploading dataset ^^^^^^^^^






#### Functions for Subsetting dataset \/\/\/\/\/\/\/\/\/
#### Functions for Subsetting dataset \/\/\/\/\/\/\/\/\/
#### Functions for Subsetting dataset \/\/\/\/\/\/\/\/\/
#### Functions for Subsetting dataset \/\/\/\/\/\/\/\/\/
#### Functions for Subsetting dataset \/\/\/\/\/\/\/\/\/
#### Functions for Subsetting dataset \/\/\/\/\/\/\/\/\/




# makes a matrix of the all the tracks for one measurement, the column names must match the wonky R names. just use colnames(objects) to find the name
# changed to use new track.ID
getObjectsColumn <- goc <- function ( df, columnName)
{
  object = T
  #cat("grepl(Track, ", columnName,")\n")
  #print(grepl("Track", columnName))
  if( grepl("Track", columnName) ){
    object = F
  }
  
  if( object == T )
  {
    objects <- df[ df$Type == "Object", ]
    cols <- max( as.numeric(objects$Time_Point), na.rm=T )
    NameColumns <- unique(sort( as.numeric(as.character(objects$"Time_(s)" )) ))
  }
  else
  {
    objects <- df[ df$Type == "Track", ] 
    cols <- 1
    NameColumns <- columnName
  }
  
  coln <- colnames(objects) == columnName
  

  mat <- data.frame(matrix( NA, nrow = length(unique( objects$TrackID )), ncol = cols ))
  row.names( mat ) <- unique( objects$TrackID )
  colnames ( mat ) <- NameColumns
  
  for( i in 1:nrow( objects ) )
  {
    if( object == T ){
      mat[ as.character( objects$TrackID[i] ), as.character( objects$"Time_(s)"[i] ) ]   <-  as.character(objects[i, coln])
    }else{
      mat[ as.character( objects$TrackID[i] ), 1 ]   <-  objects[i, coln]
    }
  }
  return(mat) 
}






