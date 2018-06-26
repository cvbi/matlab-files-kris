### Load dependent packages and also helper functions
### The arg passed from program icon is the directory with the function files
	
	args = commandArgs(trailingOnly=TRUE)
	dir <- args[1]
	source( paste0( dir, "GraphFunctions.r") )
	source( paste0( dir, "TableFunctions.r") )
	source( paste0( dir, "AnalysisFunctions.r") )



## load dependencies, base packages used to make graphs and gui
	
	list.of.packages <- pkgs <-  c("shiny", "markdown", "car", "ggplot2")
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
	if(length(new.packages)){install.packages(new.packages)}

	for( i in 1:length(pkgs))
	{
	  require(pkgs[i], character.only = T)
	  library(pkgs[i], character.only = T)
	}





##################### ui
##################### ui
##################### ui
##################### ui
##################### ui
##################### ui


ui <- navbarPage("Volocity Data Analysis",
	tabPanel("Table",
		sidebarLayout(
			sidebarPanel(
					
					fileInput('files', 'Upload Volocity File', multiple = F),
					
					uiOutput("tableButton"),

					uiOutput("ids"),					
					uiOutput("timespan"),
					uiOutput("column"),
					
					tags$hr(),
					uiOutput("outputFile"),
					uiOutput("downloadDataButton")
					
			),
			mainPanel(
					verbatimTextOutput("nText"),
					tableOutput('contents')#,
#					plotOutput('plot')				
					#dataTableOutput('contents')   
			)
		)
	),
	
	tabPanel("Plot",
				 mainPanel
				 (
					 verbatimTextOutput("nText2"),
					plotOutput('plot')				

				 )
	)
	
)





############################### Server
############################### Server
############################### Server
############################### Server
############################### Server
############################### Server
############################### Server
############################### Server
############################### Server


server <- function(input, output, session)  
{
	
	# increases the data storage capacity for loading data into rShiny
	options(shiny.maxRequestSize=30*1024^2) 
	
	
	# Type name of export file
  	output$outputFile <- renderUI({
		if( is.null(input$files) ){
            return(NULL)
		} else{
			textInput("outputFile", label = h5("Export Table "), value = "output.csv") }#(Remember to hit \"Render Table\" before downlaoding)"), value = "output.csv")		}  	
  	})
  	
  	# download button
  	output$downloadDataButton <- renderUI({
		if( is.null(input$files) ){
            return(NULL)
		} else{
			downloadButton('downloadData', label = 'Download', class = NULL)		
		}  	
  	})
  	

	# type in ids to be deleted
  	output$ids <- renderUI({
		if( is.null(input$files) ){
	      return(NULL)
		} else{
			textInput("ids", label = h5("Track IDs to delete (separate IDs by commas)"), value = "")
		}  	
  	})
  	
  	
  	output$tableButton <- renderUI({
		if( is.null(input$files) ){
	      return(NULL)
		} 
		else{
			actionButton("tableButton", "Render Table")
		}  	
  	})
  	
  	applied <- eventReactive(input$tableButton, {
      
		lst <- makeDF(  input$files, input$ids, input$timespan, input$column )
		cat("\nfiles: ", input$files[[4]] )
		cat("\nids: ", input$ids)
		cat("\ntimespan: ", input$timespan)
		cat("\nmeasurement: ", input$column, "\n\n")
		return(lst)
	
	})
  	
  	
  	
  	# select the measurement item
	output$column <- renderUI({
		if( is.null(input$files) ){
			return(NULL)
		} else{
		
			newCols <- processTableColumns( input$files[,4] )
	
			#selectInput( "column", "Get Subset Table:", selected = "None", multiple = T, choices = c("None", newCols) );			
			selectInput( "column", "Get Subset Table (Can select multiple track parameters):", selected = NULL, multiple = T, choices = newCols );			
			#selectInput( "column", "Get Subset Table:", selected = "None", multiple = F, choices = c("None", newCols) );			
		}
	})
  	
  	
  	


    	
  	
	# Render the Slider for frames threshold
  	output$timespan <- renderUI({
		if( is.null(input$files) ){
			return(NULL) 
		} else{
		
			textInput("timespan", label = h5("Minimum Frames in a Track:"), value = "")

			#tab <- read.csv(input$files[,4], fileEncoding="latin1", na.strings= c("N/A","<NA>"), check.names = F)
			#val <- max( as.numeric(as.character(tab$"Time Span")), na.rm=T)
			#sliderInput("timespan", "Minimum Frames in a Track:", 
             #     min = 1, max = val + 1, value = 1, step= 1);
		}  	
  	})
  
  
  	#output$nText <- renderText({ 
	#	text <- "Rendering Table..."
    #})
  
  
   	progress <- shiny::Progress$new()
    progress$set(message = "Rendering Table", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
  
  	#Render Table
	output$contents <- renderTable({
		
		output$nText <- renderText({ "" });
		
		if( !is.null(input$files) ){ 

			lst <- applied()

			if( !is.null(lst) ){
				df <- lst[[1]]
			}
			else{
				df <- NULL
			}
			
			if( length(lst) == 3 ){
				 output$plot <- renderPlot({ 
				 	lst[[3]]
				 })
			} else{
				output$plot <- renderPlot({ 
				 	NULL
				 })
			}
			
			if( length(lst) > 1 ){
				output$nText <- renderText({ 
					lst[[2]]
    	 		})
			}
			
						
			if( nrow(df) > 1000 ){
				#cols2 <- c("ID", "Track ID", "Type" ,"Timepoint", "Rel. Time (s)",  "Track Velocity (µm/sec)" , "Velocity (µm/sec)" , "Length (µm)", "Displacement (µm)" , "Meandering Index Cal.", "Delta^2 (µm2)", "Angle (degrees)", "Bearing (degrees)" ,"Elevation (degrees)", "Volume (µm3)" );
				cols2 <- c("ID", "Track ID", "Type" ,"Timepoint", "Rel. Time (s)", "Time Span" ); #"Track Velocity (µm/sec)" , "Velocity (µm/sec)" );#, "Length (µm)", "Displacement (µm)" , "Meandering Index Cal.", "Delta^2 (µm2)", "Angle (degrees)", "Bearing (degrees)" ,"Elevation (degrees)", "Volume (µm3)" );
				badInds <- which( !cols2 %in% colnames(df) )
				if( length(badInds) > 0 ){
					cat("these Cols not in Table: ", cols2[which( !cols2 %in% colnames(df) )], "\n" )
					cols2 <- cols2[-badInds]
				}
				if( length(cols2) > 0 ){
					df <- df[ , cols2];
					
					output$nText <- renderText({ 
						 "Table: Only a subset of columns is printed due to large table size" 
    	 			})
					
					#outputText <- "Only a subset of columns is printed due to large table size" 
				}
				
			}
			
			
			return(df)
		}
		else{
			return(NULL)
		}
	}
	,include.rownames=FALSE
	)
  

	# handles the download button
	output$downloadData <- downloadHandler(
		filename = function() {
			filename = as.character(input$outputFile)
		},
		content = function(file) {
			lst <- applied()
			#lst <- makeDF(  input$files, input$ids, input$timespan, input$column )
			if( !is.null(lst) ){
				df <- lst[[1]];
				write.csv( df, file, row.names=F, quote=F, fileEncoding = "latin1" )
			}
		}
	)
  
}

runApp(shinyApp(ui, server), launch.browser=TRUE)
