#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions
#### table Manipulation functions#### table Manipulation functions



# erases objects from that do not meet the set timespan threshold, the threshold is inclusive 
deleteByTimespan <- dbt <- function( df, threshold )
{
  # find which track ids have timespan less than threshold	
  tracks <- df[ df$Type == "Track", ]
	inds <- which( as.numeric(as.character(tracks$"Time Span")) < threshold )
	ids <- tracks$"Track ID"[inds]
	
	
	if( threshold == 0){
		string <- ""
	}
	else{
		string <- paste0("0 tracks had less than ", threshold, " frames.\n")
	}	
	
	# erases any object or track that has track id in ids
	inds2 <- which( df$"Track ID" %in% ids )
	if( length(inds2) > 0 ){
		string <- paste0("Deleted ", length(inds), " tracks that had less than ", threshold, " frames.\n")
		df <- df[ -inds2, ] 
	}
	
	return( list(df, string) )
}


# erases objects and tracks associated with given trackIds
deleteByTrackID <- dbtid <- function( df, trackIds ) {
  inds <- which(df$"Track ID" %in%  trackIds)
	
	if( length(trackIds) == 1 ){
		if( trackIds == 0 ){
			string <- paste0(""  );
		}else{
			string <- paste0("Deleted ", length(inds), " rows based on supplied Track IDs.\n"  );
		}
	}	else{
	
		string <- paste0("Deleted ", length(inds), " rows based on supplied Track IDs.\n"  );
	}
	
	missed <- which( !trackIds %in%  df$"Track ID" )	
	if( length(missed) > 0 && trackIds[1] != 0 ){
		string <- paste0("The following Track ID(s) were not found in table: ", paste0(trackIds[missed], collapse=", "), "\n" );
	}

  if( length(inds) != 0 ){
    df <- df[ -inds, ]
  }
  return( list(df, string) )
}



getObjectsColumnMulti <- gocm <- function ( df, columnNames ){
    
    #testing
    #columnNames <- c("Track - Track Velocity (µm/min)", "Track - Displacement (µm)", "Track

	OutputText <- paste0( columnNames, collapse=", " )
	OutputText <- paste0( "Table: ", OutputText  )

	if( sum(grepl("Analysis - ", columnNames)) > 0 ){
		OutputText <- paste0( OutputText, "\n\nError: Cannot Include Analysis Parameters in Multiple Selection!" )
		return(list(NULL, OutputText))
	}

	if( sum(grepl("Object - ", columnNames)) > 0 ){
		OutputText <- paste0( OutputText, "\n\nWarning: Only track parameters can be selected multiple times!" )
	}
	

    
    tracks <- df[ df$Type == "Track", ] 
	for( i in 1:length(columnNames) ){

		colname=columnNames[i];
		
		if( grepl("Object - ", colname) ){
    		colname <- substr(colname,10,nchar(colname))
    	}
    	else{
    		colname <- substr(colname,9,nchar(colname))
    	}
		
		if( grepl("(µm/min)", colname) ){
    		colname2 <- gsub("µm/min", "µm/sec", colname)
    		values = tracks[,colname2];
    		values = as.numeric(as.character(values)) * 60
    	}
    	else{
    		values = tracks[,colname];
    	}
		if( i == 1 ){
			rowns<- tracks$"Track ID"
			df <- data.frame( "TrackIDs" = rowns, colname = values )
			colnames(df)[ncol(df)] <- colname
			#tracks2 <- cbind( "TrackIDs" = tracks$"Track ID", tracks2 )	
		}
		else{
			df <- cbind( df, colname = values)
			colnames(df)[ncol(df)] <- colname
		}	
	}

	return(list(df, OutputText))



    if( grepl("(µm/min)", columnNames) ){
    	columnName <- gsub("µm/min", "µm/sec", columnNames);
    	multiplier = T;
    }else{
    	multiplier = F;
    }
  	# if selecting track data

	#print(columnName)
	tracks <- df[ df$Type == "Track", ];
	coln <- which(colnames(tracks) %in% columnName);
	
	
	if( length(coln) == length(columnName) ){
		tracks2 <- tracks[,coln]
		tracks2 <- as.data.frame(tracks2)
		colnames(tracks2) <- colnames(tracks)[coln]
		#print(tracks2)
		row.names( tracks2 ) <- tracks$"Track ID"
		tracks2 <- cbind( "TrackIDs" = tracks$"Track ID", tracks2 )
		
		if( multiplier ){
			#print("as.numeric(as.character(tracks2[,2]))")
			#print(as.numeric(as.character(tracks2[,2])))
			#print("as.numeric(as.character(tracks2[,2])) * 60")
			#print(as.numeric(as.character(tracks2[,2])) * 60)
			
			tracks2[,2] <- as.numeric(as.character(tracks2[,2])) * 60
			columnName <- gsub("µm/sec", "µm/min", columnName)
			colnames(tracks2)[2] <- columnName
		}
		#print("here")
		return(tracks2)
	}
	
	else {
		cat( "Some columns not found in selection:", columnName,"\n");
		return(NULL)
	}	
	
}

# makes a matrix of the all the tracks for one measurement, the column names must match the wonky R names. just use colnames(objects) to find the name
# changed to use new track.ID
getObjectsColumn <- goc <- function ( df, columnName ){
    
    
    if( grepl("(µm/min)", columnName) ){
    	columnName <- gsub("µm/min", "µm/sec", columnName)
    	multiplier = T;
    }else{
    	multiplier = F;
    }
    
    
    if( grepl("Object - ", columnName) ){
    	columnName <- substr(columnName,10,nchar(columnName))
    	object = T
    }
    else{
    	columnName <- substr(columnName,9,nchar(columnName))
    	object = F
    }
    

  	# if selecting track data
	if( object == F ){
		#print(columnName)
		tracks <- df[ df$Type == "Track", ] 
		coln <- which(colnames(tracks) %in% columnName)
		if( length(coln) == length(columnName) ){
			tracks2 <- tracks[,coln]
			tracks2 <- as.data.frame(tracks2)
			colnames(tracks2) <- colnames(tracks)[coln]
			#print(tracks2)
			row.names( tracks2 ) <- tracks$"Track ID"
			tracks2 <- cbind( "TrackIDs" = tracks$"Track ID", tracks2 )
			if( multiplier ){
				#print("as.numeric(as.character(tracks2[,2]))")
				#print(as.numeric(as.character(tracks2[,2])))
				#print("as.numeric(as.character(tracks2[,2])) * 60")
				#print(as.numeric(as.character(tracks2[,2])) * 60)
				tracks2[,2] <- as.numeric(as.character(tracks2[,2])) * 60
				columnName <- gsub("µm/sec", "µm/min", columnName)
				colnames(tracks2)[2] <- columnName
			}
			#print("here")
			return(tracks2)
		}
		else {
			cat( "Some columns not found in selection:", columnName,"\n");
			return(NULL)
		}	
	}


	# testing
	#columnName <- "Velocity (µm/sec)"	
	

	objects <- df[ df$Type == "Object", ]
	NameColumns <- unique(sort( as.numeric(as.character(objects$"Rel. Time (s)") )))

	cols <- length(NameColumns)

	coln <- colnames(objects) == columnName

	mat <- data.frame(matrix( NA, nrow = length(unique( objects$"Track ID" )), ncol = cols ))

	row.names( mat ) <- unique( objects$"Track ID" )
	colnames ( mat ) <- NameColumns

	#colnames ( mat ) <- 1:max( objects$Timepoint )
	#colnames ( mat ) <- 0:max( objects$Timepoint )-1

	for( i in 1:nrow( objects ) )
	{
	
		if( multiplier ){
			#print("( as.numeric(as.character( objects[i, coln] ) ) * 60)")
			#print(( as.numeric(as.character( objects[i, coln] ) ) * 60) )
			mat[ as.character( objects$"Track ID"[i] ), as.character( objects$"Rel. Time (s)"[i] ) ]   <-  ( as.numeric(as.character( objects[i, coln] ) ) * 60)
		} else{				
			mat[ as.character( objects$"Track ID"[i] ), as.character( objects$"Rel. Time (s)"[i] ) ]   <-  as.character( objects[i, coln] )
		}
	}


	#mat <- removeLeadingZeros( mat )

	mat <- t(mat); 
	mat <- cbind( "Time in Seconds" = row.names(mat), mat );
	if( multiplier ){
		#mat[,1] <- round(as.numeric(as.character(mat[,1])) / 60,2)
		#colnames(mat)[1] <- "Time in Minutes"
	}	
	return(mat) 
}


# Not currently used, but it can be used to remove 0 values that really should be NA values
removeLeadingZeros <- function( mat ){

	matog <- mat
 	for( i in 1:nrow(mat) ){
 		for( j in 1:ncol(mat) ){
 			if( !is.na(mat[i,j]) ){
 				if( mat[i,j] == 0 )
 				{
 					mat[i,j] <- NA 
 					break()	
 				} else{
 					break()
 				}
 			} 
 		}
 	}
	return(mat)
}





# removes Tracks that have no Track ID, these are tracks that only have an object at one time point
# this feature is perculiar to Volocity. These Tracks should always be purged
removeNas <- function( df )
{ 
	inds <- which( is.na(df$"Track ID") )
	if( length(inds) > 0 )
	{
		df <- df[-inds, ]
	}
	return(df)
}



processTableColumns <- function( file ){
    df <- read.csv(file, fileEncoding="latin1", na.strings= c("N/A","<NA>"), check.names = F)
    df <- removeNas(df)
    cols <- colnames(df)
    newCols <- vector()
    tracks <- df[df$Type == "Track",]
    objects <- df[df$Type == "Object",]
    for( i in 1:length(cols) ){
    	objects <- objects[ order(objects[,cols[i]]), ]
    	tracks <- tracks[ order(tracks[,cols[i]]), ]
        objectValue <- objects[1,cols[i]]
        trackValue <- tracks[1,cols[i]]
        if( !is.na(objectValue) ){
            newCols <- c(newCols, paste0("Object - ", cols[i]) )
        }
        if( !is.na(trackValue) ){
            newCols <- c(newCols, paste0("Track - ", cols[i]) )
        }
    }
    
    
    if( "Object - Velocity (µm/sec)" %in% newCols ){
    	newCols <- c(newCols, "Object - Velocity (µm/min)", "Analysis - Arrest Coefficient")
    }
    
    if( "Track - Track Velocity (µm/sec)" %in% newCols ){
    	newCols <- c(newCols, "Track - Track Velocity (µm/min)")
    }
    
    if( "Object - Delta^2 (µm2)" %in% newCols ){
    	newCols <- c(newCols, "Analysis - MSD" )
    }
    
    if( "Object - Rel. Centroid X (µm)" %in% newCols && "Object - Rel. Centroid Y (µm)" %in% newCols && "Object - Rel. Centroid Z (µm)" %in% newCols ){
    	newCols <- c(newCols, "Analysis - Turning Angle" )
    }
    
    newCols <- sort(newCols);
    
    return(newCols)
}



makeDF <- function( inputfiles, inputids, inputtimespan, inputcolumn )
{


	if( is.null(inputfiles) ){
		return(NULL) 
	}
	if( is.null(inputcolumn) ){
		#print("null column")
		#return(NULL) 	
	}
  
  	
  	if(is.null(inputids)){ids = 0}
	else if( inputids == "" ){ids = 0}
	else{ ids = as.numeric(unlist(strsplit( inputids, ","))) }

	if(is.null(inputtimespan)){timespan = 0}
	else if( inputtimespan <= 0 ){timespan = 0}
	else{ timespan = as.numeric( inputtimespan ) };

	file <- inputfiles[,4]
	df <- read.csv(file, fileEncoding="latin1", na.strings= c("N/A","<NA>"), check.names = F)
	df <- removeNas( df )
	df <- dbtid( df, ids )
	string <- df[[2]]
	df <- df[[1]]
	df <- dbt( df, timespan )
	string <- paste0(df[[2]],string)
	df <- df[[1]]
	outputText <- paste0(string, "\nTable: Full Table");

	
	# for just getting objects
	if( !is.null(inputcolumn) ){
		
		if( length(inputcolumn) > 1 ){
			#check if erroneous "None" is in selected parameters
			#check if non-Track parameter was chosen:	
			df <- gocm( df, inputcolumn )
			outputText <- paste0(string, "\n", df[[2]])
			df <- df[[1]]
			return(list( df, outputText ))
		}
		else if( inputcolumn == "Analysis - MSD"){ 
			df <- makeMSD(df)
			outputText <- paste0(string, "\nTable: Analysis - MSD\nView graph in the Plot tab."	)		
			return( list( df[[1]], outputText, df[[2]] ) )
		}
		else if( inputcolumn == "Analysis - Arrest Coefficient"){ 
			df <- getArrestCoeff( df, 0.03333333)	
			outputText <- paste0(string, "\nTable: Analysis - Arrest Coefficient\nView graph in the Plot tab.")
			return( list( df[[1]], outputText, df[[2]] ) )
			
		}
		else if( inputcolumn == "Analysis - Turning Angle"){ 
			df <- getTurningAngle(df)
			outputText <- paste0(string, "\nTable: Analysis - Turning Angle\nView graph in the Plot tab.")
			return( list( df[[1]], outputText, df[[2]] ) )
		} 
		else{
			#print("here2")
			df <- goc( df, inputcolumn  );
			OutputText <- paste0(string, paste0( "\nTable: ", inputcolumn  ))
			return(list(df, OutputText))
		}
	}
	

  	return( list(df, outputText) )
}