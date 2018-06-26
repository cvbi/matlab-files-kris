

file <- "/Users/cbarilla/Desktop/Comparing_VoloAndImaris/Code/VolocityData/Tracking_Count2_Simple_MHC_R_35.csv"
df <- read.csv(file, fileEncoding="latin1", na.strings= c("N/A","<NA>"), check.names = F)


processTableColumns <- function( file ){
    df <- read.csv(file, fileEncoding="latin1", na.strings= c("N/A","<NA>"), check.names = F)
    cols <- colnames(df)
    newCols <- vector()
    tracks <- df[df$Type == "Track",]
    objects <- df[df$Type == "Object",]
    for( i in 1:length(cols) ){
        objectValue <- objects[1,cols[i]]
        trackValue <- tracks[1,cols[i]]
        if( !is.na(objectValue) ){
            newCols <- c(newCols, paste0("Object - ", cols[i]) )
        }
        if( !is.na(trackValue) ){
            newCols <- c(newCols, paste0("Track - ", cols[i]) )
        }
    }
    return(newCols)
}

