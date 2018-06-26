	# Type name of export file
  	output$outputFile <- renderUI({
			textInput("outputFile", label = h5("Export Table"), value = "output.csv")
  	})
  	
  	# download button
  	output$downloadDataButton <- renderUI({
			downloadButton('downloadData', label = 'Download', class = NULL)			
  	})

	# type in ids to be deleted
  	output$ids <- renderUI({
		textInput("ids", label = h5("Track IDs to delete (separate IDs by commas)"), value = "")
  	})
  	
  	

  	
  	#Select type of data, object or track
  	output$type <- renderUI({
  			selectInput("type", "Tracks or Objects:", 
				choices = c("None", "Object", "Track"))	
  	})
  	
  	
  	
  	 # select the measurement item
	output$column <- renderUI({
		tab <- read.csv(input$files[,4], fileEncoding="latin1", na.strings= c("N/A","<NA>"), check.names = F)
		columnnames <- colnames(tab);
		
		if( !is.null(input$type) ){
			if( input$type == "Track" ){
				selectInput( "column", "Subset by Column(s):", multiple = T, choices = c("None",  sort(columnnames) ) );
			}
			else {
				selectInput( "column", "Subset by Column:", selected = "None", multiple = F, choices = c("None",  sort(columnnames) ) );			
			}
		}
	})
  	
  	
	# Render the Slider for frames threshold
  	output$timespan <- renderUI({
		tab <- read.csv(input$files[,4], fileEncoding="latin1", na.strings= c("N/A","<NA>"), check.names = F)
		columnnames <- colnames(tab);
		selectInput( "column", "Subset by Column:", choices = c("None",  columnnames) );
		val <- max( as.numeric(as.character(tab$"Time Span")), na.rm=T)
		#print("max time span")
		#print(val)
		sliderInput("timespan", "Minimum Frames in a Track:", 
			  min = 1, max = val + 1, value = 1, step= 1);
		 	
  	})