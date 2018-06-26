#source("funcs.r")
source("/Users/klambert/Desktop/VolocityDataManagerApp/allGraphFunctions.r")



pkgs <- c("shiny", "markdown", "car", "ggplot2")
for( i in 1:length(pkgs))
{
  require(pkgs[i], character.only = T)
  library(pkgs[i], character.only = T)
}


ui <- navbarPage("Volocity Data Analysis",
	tabPanel("Table",
		sidebarLayout(
		sidebarPanel(
				fileInput('files', 'Choose CSV File', multiple = T),
				#tags$hr(),
				textInput("ids", label = h5("Track ID's to delete"), value = "ids separated with comas"),
				textInput("timespan", label = h5("Time Point Threshold (inclusive)"), value = "Enter number..."),
				
				#selectInput("column", "Subset by Column:", 
				#	  choices = c("None", "Name","Type","ID","Track ID","Item Name","Timepoint","Abs. Time","Rel. Time (s)","Voxel Count","Volume (µm3)","Min (RXD1)","Max (RXD1)","Mean (RXD1)","Sum (RXD1)","Standard Deviation (RXD1)","Min (RXD2)","Max (RXD2)","Mean (RXD2)","Sum (RXD2)","Standard Deviation (RXD2)","Length","Length (µm)","Time Span","Track Velocity","Track Velocity (µm/sec)","Velocity","Velocity (µm/sec)","Distance","Distance (µm)","Displacement","Displacement (µm)","Delta^2","Delta^2 (µm2)","Displacement Rate","Displacement Rate (µm/sec)","Meandering Index","Rel. Centroid X","Rel. Centroid Y","Rel. Centroid Z","Rel. Centroid X (µm)","Rel. Centroid Y (µm)","Rel. Centroid Z (µm)","Angle (degrees)","Bearing (degrees)","Elevation (degrees)","Population","Meandering Index Cal.", "New Track ID")),
				uiOutput("ui"),	
				
				selectInput("type", "Tracks or Objects:", 
					  choices = c("None", "Object", "Track")),

				actionButton("tableButton", "Apply"),
				br(),br(),tags$hr(),

				textInput("outputFile", label = h5("Output File Name"), value = "Enter file name..."),

				downloadButton('downloadData', 'Download')
		),
		
		mainPanel(
				verbatimTextOutput("nText"),
      			tableOutput('contents')   
		)
		)
		),

	
	##PLOTS
	##PLOTS
	##PLOTS
	##PLOTS
	##PLOTS
	
	tabPanel("Plots",
	       sidebarLayout(
	         sidebarPanel
	         (
	         fileInput('files2', 'Choose CSV Files', multiple = T),
	         tags$hr(),
	         
	         selectInput("plot_type", "Plot Type",
            choices = c("Track_Measurements", "XYZ", "Matrix_Plot", "Arrest_Coefficient")
	         ),
	         
	         actionButton("goButton", "Apply"),
	         br(),br(),tags$hr(),
	         textInput("outputFile2", label = h5("Output File Name"), value = "Enter file name..."),
	         downloadButton('downloadData2', 'Download')
	         ),
	         mainPanel
	         (
	         verbatimTextOutput("nText2"),
	         plotOutput("plot1", width = 1500, height = 800)
	         #plotOutput("plot2", width = 1000, height = 500),
	         #plotOutput("plot3", width = 1000, height = 1000)
	         )
	       )
	)
)





### Server


server <- function(input, output, session)  
{
  options(shiny.maxRequestSize=30*1024^2);
  applied <- eventReactive(input$tableButton, {
      
    inFile <- input$files
    if (is.null(inFile))
      return(NULL)
    
    if( input$ids == "ids coma separated w/ no spaces..." || input$ids == "" ){ids = 0}
    else{ ids = as.numeric(unlist(strsplit( input$ids, ","))) }
    
    if( input$timespan == "Enter number..." || input$timespan == "" ){timespan = 0}
    else{ timespan = as.numeric( input$timespan ) }
    #print(paste0("ids: ", ids)); print(paste0("timespan: ", timespan))
    
    files <- inFile[,4]
    df <- main( files, ids, timespan  )
    

    columnNames <- colnames(df)

    if(!is.null(input$column)){ 
    if( input$column != "None" )
    {
      #print(input$column)
      #print(translateName(input$column))
      if( input$type == "None" ){return(NULL)}
      else if (input$type == "Object" ){ df <- goc( df, translateName(input$column), T  ) }
      else{ df <- goc( df, translateName(input$column), F ) }
    }
    }
    
    return( list(df, columnNames) )
    
  })
  
  output$ui <- renderUI({
    columnnames <- applied()[[2]];
    selectInput("column", "Subset by Column:",
                choices = c("None",  columnnames))#colnames(df)) )
  })
  
  
  output$contents <- renderTable({ 
    df <- applied()[[1]]; 
    #if( nrow( df ) > 1000 )
    #{
      #df <- head(df); 
      #output$nText <- renderText({ "This table is too large, so only first 6 rows are printed to screen. However the downloaded file will include whole table."  }); 
    #}
    df 
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      input$outputFile
    },
    content = function(file) {
      #write.csv(datasetInput(), file)
      df <- applied()[[1]]
      write.csv( df, file, row.names=F, quote=F, fileEncoding = "latin1" )
    })
  
  
  
  
  
  ## plots  ## plots
  ## plots  ## plots
  ## plots  ## plots
  ## plots  ## plots
  ## plots  ## plots
  ## plots  ## plots
  applied2 <- eventReactive(input$goButton, {
    
    inFile <- input$files2
    files <- inFile[,4]
    lst <- catFiles(files)
    
    plot_type = input$plot_type
    
    if( plot_type == "Track_Measurements"){
      p1 <- makePlot(lst, "Track Velocity (µm/sec)")
      p2 <- makePlot(lst, "Displacement (µm)")
      p3 <- makePlot(lst, "Meandering Index Cal.")
      p4 <- makePlot(lst, "Angle (degrees)")
      p5 <- circularPlot (lst, "Bearing (degrees)" )
      p6 <- makePlot(lst, "Elevation (degrees)")
      lst <- list( p1, p2, p3, p4, p5, p6 )
    } 
    if( plot_type == "XYZ"){
      p7 <- plot3d( lst )
      lst <- p7
    }
    if( plot_type == "Matrix_Plot"){    
      vf <- c("Track Velocity (µm/sec)", "Displacement (µm)", "Meandering Index Cal.","Angle (degrees)" )
      df <- lapply( lst, gv, variable = vf)
      df <- bindLst( df )
      lst <- list(df)
    }
    if( plot_type == "Arrest_Coefficient"){    
      lst <- makeAC( lst )
      lst[[3]] <- "filler"
    }
    
    #lst <- list( p1, p2, p3, p4, p5, p6, p7, df )
    lst
  })
  
  #writeMessage <- function( str )
  
  #output$nText <- renderText({applied()})
  #output$contents <- renderText({applied()})
  output$plot1 <- renderPlot({ 
    #plot_type = input$plot_type
    lst <- applied2(); 
    #if( plot_type == "Track_Measurements")
    if( length(lst) == 6 )
    {
      multiplot( lst[[1]], lst[[4]], lst[[2]], lst[[5]], lst[[3]], lst[[6]], cols=3 )
    } 
    #if( plot_type == "XYZ")
    if( length(lst) == 2 )
    {
      multiplot( lst[[1]],  lst[[2]], cols=2 )
      #multiplot( lst[[7]][[1]],  lst[[7]][[2]], cols=2 )
    }
    #if( plot_type == "Matrix_Plot")
    if( length(lst) == 1 )
    {    
      matrixPlot(lst[[1]]) 
      #matrixPlot(lst[[8]])  
    }
    if( length(lst) == 3 )
	{    
	multiplot( lst[[1]],  lst[[2]], cols=2 ) 
	}
    #multiplot( lst[[1]], lst[[4]], lst[[2]], lst[[5]], lst[[3]], lst[[6]], cols=3 )
  })
  
  
  
  
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      input$outputFile2
    },
    content = function(file) {
      #write.csv(datasetInput(), file)
      lst <- applied2(); 
      png(filename = file, height = 800, width = 1500  )
      #multiplot( lst[[1]], lst[[4]], lst[[2]], lst[[5]], lst[[3]], lst[[6]], cols=3 )
      if( length(lst) == 6 )
      {
        multiplot( lst[[1]], lst[[4]], lst[[2]], lst[[5]], lst[[3]], lst[[6]], cols=3 )
      } 
      #if( plot_type == "XYZ")
      if( length(lst) == 2 )
      {
        multiplot( lst[[1]],  lst[[2]], cols=2 )
        #multiplot( lst[[7]][[1]],  lst[[7]][[2]], cols=2 )
      }
      #if( plot_type == "Matrix_Plot")
      if( length(lst) == 1 )
      {    
        matrixPlot(lst[[1]]) 
        #matrixPlot(lst[[8]])  
      }
    if( length(lst) == 3 )
	{    
	multiplot( lst[[1]],  lst[[2]], cols=2 ) 
	}
      
      
      dev.off()
    })
}

#shinyApp(ui, server)
runApp(shinyApp(ui, server), launch.browser=TRUE)













