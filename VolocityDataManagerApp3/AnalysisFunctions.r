
makeMSD <- function( df ){

    objects <- df[df$Type == "Object", ]
	NameColumns <- unique(sort( as.numeric(as.character(objects$"Rel. Time (s)") )))
	cols <- length(NameColumns)
	coln <- colnames(objects) == "Delta^2 (µm2)"
	mat <- data.frame(matrix( NA, nrow = length(unique( objects$"Track ID" )), ncol = cols ))
	row.names( mat ) <- unique( objects$"Track ID" )
	colnames ( mat ) <- NameColumns
	for( i in 1:nrow( objects ) ){
		mat[ as.character( objects$"Track ID"[i] ), as.character( objects$"Rel. Time (s)"[i] ) ]   <-  round(as.numeric(as.character( objects[i, coln] )), 3)
	}
    mat <- sapply(mat, as.numeric)
    # takes care leading zero
	for( i in 1:nrow(mat) ){
		ind <- which( mat[i,] == 0 )[1]
		mat[i, ind] <- NA
	}
	df <- mat
    
    # get the average inst. velocity at each time point across all objects
    n <- vector()
    for( j in 1:ncol(df) ){
        n[j] <- sum(!is.na(df[,j]))
    }
    
    means <- colSums(df, na.rm = T) / n    
    temp <- data.frame( "Time in Seconds" = as.numeric(names(means)), "Mean Squared Displacement" = means )

	temp[1,2] <- 0
    p <- ggplot( data=temp, aes(x=Time.in.Seconds, y = Mean.Squared.Displacement )) +
    	geom_point() + 
    	geom_line() +
        theme_classic() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs( x = "Time in Seconds", y = "Mean Squared Displacement (µm^2)", title = "Mean Square Displacment")
        
        
    colnames(temp)[2] <- "Mean Squared Displacement (µm^2)"
    return(list(temp, p))
}


getArrestCoeff <- function( df, threshold )
{
	objects <- df[df$Type == "Object", ]
	NameColumns <- unique(sort( as.numeric(as.character(objects$"Rel. Time (s)") )))
	cols <- length(NameColumns)
	coln <- colnames(objects) == "Velocity (µm/sec)"
	mat <- data.frame(matrix( NA, nrow = length(unique( objects$"Track ID" )), ncol = cols ))
	row.names( mat ) <- unique( objects$"Track ID" )
	colnames ( mat ) <- NameColumns
	for( i in 1:nrow( objects ) ){
		mat[ as.character( objects$"Track ID"[i] ), as.character( objects$"Rel. Time (s)"[i] ) ]   <-  round(as.numeric(as.character( objects[i, coln] )), 3)
	}
	vec <- vector()

	# takes care leading zero
	for( i in 1:nrow(mat) ){
		ind <- which( mat[i,] == 0 )[1]
		mat[i, ind] <- NA
		vec[i] <- mean( mat[i,] < threshold, na.rm = T ) * 100
		vec[i] <- round(vec[i], 3)
	}
	dff <- data.frame( TrackIDs = row.names(mat), Arrest_Coefficient = vec   )
	row.names(dff) <- row.names(mat)

	#get number of stops
	getStops <- function( matx,threshold ){
		
		inds <- which(is.na(matx))
		if( length(inds) > 0 ){
			matx <- matx[-inds]	
		}
		bool <- matx < threshold
		
		stops <- 0
		lever <- T
		for( i in 1:length(bool) ){
			if( bool[i] && lever ){
				stops = stops + 1
				lever = F
			} else if( !bool[i] && !lever ){
				lever <- T
			}	
		}
		return(stops)
	}

	#getStops(c(0.0001, 0.1, 0.00001, 0.1), 0.0333);getStops(c(0.1, 0.1, 0.00001, 0.1), 0.0333);getStops(c(0.0001, 0.1, 0.00001), 0.0333);getStops(c(0.1, 0.1, 0.00001), 0.0333);getStops(c(0.1, 0.1), 0.0333);
	
	stops <- vector()
	for( i in 1:nrow(mat) ){
		stops[i] <- getStops(mat[i,], threshold)
	}
	
	c <- ggplot(dff, aes(as.numeric(as.character(dff[,2]))) ) +
		stat_bin(binwidth=10) + xlim(-10,110) +
		theme_classic() + 
		labs( y = "Count", x = "Arrest Coefficient (%)", title = "Arrest Coefficient")

	dff <- cbind(dff, stops)
	colnames(dff)[2:3] <- c("Arrest_Coefficient (Percentage of Inst. Velocities < 2µm/min)", "Number of Discrete Stops")

	return(list(dff,c))
  
}



getCoords <- function( df, imaris ){
    df <- df[df$Type=="Object",]
    lst <- split(df, f=df$"Track ID")
    
    if( imaris ){
        cols <- c("Position.X_um","Position.Y_um","Position.Z_um","Time_s")
    } else{
        cols <- c("Rel. Centroid X (µm)", "Rel. Centroid Y (µm)", "Rel. Centroid Z (µm)", "Rel. Time (s)")
    }
    
    
    for( i in 1:length(lst) ){
        temp <- lst[[i]]
        temp <- temp[,cols]
        row.names(temp) <- temp[,4]
        temp <- temp[,-4]
        lst[[i]] <- temp
    }
    
    if( !imaris ){
        for( i in 1:length(lst) ){
            temp <- lst[[i]]
            for( row  in 2:nrow(temp)){
                temp[row,] <- temp[row,] + temp[row-1,]
            }
            temp[1,]<-NA
            lst[[i]]<-temp
        }
    }
    
    return(lst)
}


GetAngle <- function(a, b){
	if( nrow(a) == 0 || nrow(b) == 0 ){
		print("Error: Tracks with less frames than minimum.")
		return(NA)
	}
  crss <- sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) )
  turningAngle <- acos( as.numeric(crss) ) * 180/pi
  return( turningAngle )
}




getTurningAngle <- function( tab ){

	lst <- getCoords( tab, F )
	
	angleList <- list()
	for( i in 1:length(lst) ){
		vectors  <- lst[[i]][,1:3]
		len <- nrow(vectors);
		if( len > 1 ){
			steps <- vectors[1:len-1,] - vectors[2:len,] 
		}    
		angles <- vector()
		for( j in 2:(nrow(steps)-1)){
			angles[j] <- GetAngle(steps[j,],steps[j+1,]);
		}
		
		if( nrow(lst[[i]]) < 4 ){ 
			lst[[i]] <- cbind(lst[[i]], TurningAngle=c( rep(NA, nrow(lst[[i]]) ) ) ) 

		}else{
			lst[[i]] <- cbind(lst[[i]], TurningAngle=c(NA,NA,angles) ) 
		}
		angleList[[i]] <- angles
	}
	
	dff <- NULL
	for( i in 1:length(lst)){
		temp <- cbind( "Rel. Time (s)" = row.names(lst[[i]]), "Track ID" = names(lst)[i], lst[[i]] )
		if( i == 1 ){
			dff <- temp
		} else{
			dff <- rbind(dff,temp)
		}
	}


	dff <- cbind( Type = "Object", dff)

	colnames(dff)[2] <- "Rel. Time (s)"
	colnames(dff)[3] <- "Track ID"
	
	
	objects <- dff[dff$Type == "Object", ]
	NameColumns <- unique(sort( as.numeric(as.character(objects$"Rel. Time (s)") )))
	cols <- length(NameColumns)
	coln <- colnames(objects) == "TurningAngle"
	mat <- data.frame(matrix( NA, nrow = length(unique( objects$"Track ID" )), ncol = cols ))
	row.names( mat ) <- unique( objects$"Track ID" )
	colnames ( mat ) <- NameColumns
	for( i in 1:nrow( objects ) ){
		mat[ as.character( objects$"Track ID"[i] ), as.character( objects$"Rel. Time (s)"[i] ) ]   <-  round(as.numeric(as.character( objects[i, coln] )), 3)
	}

  
	angles <- as.data.frame( unlist(angleList) )
 	colnames(angles) <- "Turning Angle"
	p <- ggplot(angles, aes(angles[,1]) ) +
		geom_histogram(binwidth=10) + xlim(-10,190) +
		theme_classic() + 
		labs( y = "Count", x = "Angle in Degrees", title = "Turning Angle (Step Based)")

	mat <- t(mat)
	ndf <- cbind("Time in Seconds" = row.names(mat), mat )
  	
  	return(list( ndf, p))
	
}
