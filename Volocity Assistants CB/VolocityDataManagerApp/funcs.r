# erases objects from that do not meet the set timespan threshold, the threshold is inclusive 
deleteByTimespan <- function( df, threshold )
{
  # finds which tracks have timespan less than threshold	
  tracks <- df[ df$Type == "Track", ]
  
  inds <- which( tracks$Time.Span < threshold )
  ids <- tracks$New.Track.ID[inds]
  
  if( length(which(df$New.Track.ID %in% ids)) != 0 )
  {
    df <- df[ -which(df$New.Track.ID %in% ids), ]  
  }
  
  
  return( df )
}
dbt <- deleteByTimespan


# erases objects and tracks associated with given trackIds
deleteByTrackID <- function( df, trackIds ) 
{
  inds <- which(df$Track.ID %in%  trackIds)
  if( length(inds) != 0 )
  {
    df <- df[ -inds, ]
  }
  
  return( df )
}
dbtid <- deleteByTrackID


translateName <- function( name )
{
  choices <- c("Name","Type","ID","Track ID","Item Name","Timepoint","Abs. Time","Rel. Time (s)","Voxel Count","Volume (µm3)","Min (RXD1)","Max (RXD1)","Mean (RXD1)","Sum (RXD1)","Standard Deviation (RXD1)","Min (RXD2)","Max (RXD2)","Mean (RXD2)","Sum (RXD2)","Standard Deviation (RXD2)","Length","Length (µm)","Time Span","Track Velocity","Track Velocity (µm/sec)","Velocity","Velocity (µm/sec)","Distance","Distance (µm)","Displacement","Displacement (µm)","Delta^2","Delta^2 (µm2)","Displacement Rate","Displacement Rate (µm/sec)","Meandering Index","Rel. Centroid X","Rel. Centroid Y","Rel. Centroid Z","Rel. Centroid X (µm)","Rel. Centroid Y (µm)","Rel. Centroid Z (µm)","Angle (degrees)","Bearing (degrees)","Elevation (degrees)","Population","Meandering Index Cal.", "New Track ID")
  
  ind <- which( choices %in% name )
  
  colNames <- c("Name","Type","ID","Track.ID","Item.Name","Timepoint","Abs..Time","Rel..Time..s.","Voxel.Count","Volume..µm3.","Min..RXD1.","Max..RXD1.","Mean..RXD1.","Sum..RXD1.","Standard.Deviation..RXD1.","Min..RXD2.","Max..RXD2.","Mean..RXD2.","Sum..RXD2.","Standard.Deviation..RXD2.","Length","Length..µm.","Time.Span","Track.Velocity","Track.Velocity..µm.sec.","Velocity","Velocity..µm.sec.","Distance","Distance..µm.","Displacement","Displacement..µm.","Delta.2","Delta.2..µm2.","Displacement.Rate","Displacement.Rate..µm.sec.","Meandering.Index","Rel..Centroid.X","Rel..Centroid.Y","Rel..Centroid.Z","Rel..Centroid.X..µm.","Rel..Centroid.Y..µm.","Rel..Centroid.Z..µm.","Angle..degrees.","Bearing..degrees.","Elevation..degrees.","Population","Meandering.Index.Cal.","New.Track.ID" )
  
  return(colNames[ind])
}

# makes a matrix of the all the tracks for one measurement, the column names must match the wonky R names. just use colnames(objects) to find the name
# changed to use new track.ID
getObjectsColumn <- goc <- function ( df, columnName, object )
{
  
  tracks <- df[ df$Type == "Track", ]
  objects <- df[ df$Type == "Object", ]
  
  if( object == T )
  {
    objects <- df[ df$Type == "Object", ]
    cols <- max( as.numeric(objects$Timepoint), na.rm=T )
    NameColumns <- unique(sort( as.numeric(as.character(objects$Rel..Time..s.)) ))
  }
  else
  {
    objects <- df[ df$Type == "Track", ] 
    cols <- 1
    NameColumns <- columnName
  }
  
  
  #objects <- lst[[1]]
  
  coln <- colnames(objects) == columnName
  
  #print(coln)
  
  mat <- data.frame(matrix( NA, nrow = length(unique( objects$New.Track.ID )), ncol = cols ))
  
  row.names( mat ) <- unique( objects$New.Track.ID )
  colnames ( mat ) <- NameColumns
  
  #colnames ( mat ) <- 1:max( objects$Timepoint )
  #colnames ( mat ) <- 0:max( objects$Timepoint )-1
  
  for( i in 1:nrow( objects ) )
  {
    #mat[ as.character( objects[i, 48] ), as.character( objects[i, 8] ) ]   <-  objects[i, coln]	
   if( object == T ){
    #cat("id: ",as.character( objects$New.Track.ID[i] ), "\n")
    #cat("time: ", as.character( objects$Rel..Time..s.[i] ), "\n")
    #cat("value: ", as.character(objects[i, coln]), "\n")
      mat[ as.character( objects$New.Track.ID[i] ), as.character( objects$Rel..Time..s.[i] ) ]   <-  as.character(objects[i, coln])
    }else{
      mat[ as.character( objects$New.Track.ID[i] ), 1 ]   <-  objects[i, coln]
    }
  }
  #head(mat)
  
  return(mat) 
}




# divides the volicity file into track and object data frames
divideVolFile <- function( filename, selection = F )
{
  
  # reads in csv made by Kris, latin1 accounts for mu symbol, and "N/A" is recognized as NA in r 
  tab <- read.csv(filename, fileEncoding="latin1", skip=1, na.strings="N/A")
  
  # splits into two tables, one with objects and one with tracks
  inds <- which( tab$Type == "Track")
  objects <- tab[-inds,]
  tracks <- tab[inds,]
  
  
  if( selection == T )
  {
    # HARDCODE HARDCODE HARDCODE HARDCODE
    tracks$Abs..Time = NA
    
    inds <- which( is.na(objects[1,]) )
    objects <- objects[ ,-inds ]
    
    inds <- which( is.na(tracks[1,]) )
    tracks <- tracks[ ,-inds ]
  }
  
  lst <- list( objects = objects, tracks = tracks )
  return( lst )
  
}
dvf <- divideVolFile



# resets the track-id and id, only for objects dataframe, because dulicates can arise.
reid <- function( df )
{
  #removes NA tracks
  inds <- which( is.na(df$Track.ID) )
  if( length(inds) > 0 )
  {
    df <- df[-inds, ]
  }
  
  start <- df$Track.ID[1]
  ind <- 1
  vec <- vector()
  
  for( i in 1:nrow(df) )
  {
    current <- df$Track.ID[i]
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
  
  df <- cbind( df, New.Track.ID = vec )
  
  #tracks <- cbind( tracks, New.Track.ID = 1:nrow(tracks) )
  
  #lst <- list( objects = objects, tracks = tracks )
  #return( lst )
  
  return(df)
}


# concatenates volocity files, used in cases where image was cropped
catVol <- function( files )
{
  for( i in 1:length(files))
  {
    tab <- read.csv(files[i], fileEncoding="latin1", na.strings="N/A")
    
    if( i == 1 )
    {
      df <- tab
      #objects <- lst[[1]]
      #tracks <- lst[[2]]
    }
    else
    {
      df <- rbind( df, tab )
      #objects <- rbind( objects, lst[[1]] )
      #tracks <- rbind( tracks, lst[[2]] )
    }
  }
  
  df <- reid(df)
  
  return(df)
}


main <- function( files, ids, timespan )
{
  df <- catVol( files )
  
  df <- dbtid( df, ids )
  df <- dbt( df, timespan )
  
  return(df)
}


