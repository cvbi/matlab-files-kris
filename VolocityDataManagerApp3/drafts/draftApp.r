ui <- fluidPage(
  fluidRow(
    column(3, #"sidebar"
    				
    				fileInput('files', 'Choose CSV File', multiple = T),
					#tags$hr(),
					textInput("ids", label = h5("Track IDs to delete (separate IDs by commas)"), value = ""),
					#textInput("timespan", label = h5("Time Point Threshold (inclusive)"), value = "Enter number..."),
					textInput("timespan", label = h5("Delete tracks if number of tracked time points is less than threshold"), value = ""),
					#textInput("timespan", label = h5("Delete tracks by number of tracked time points"), value = ""),
			
					uiOutput("ui"),	
			
					selectInput("type", "Tracks or Objects:", 
						  choices = c("None", "Object", "Track")),

					actionButton("tableButton", "Apply"),
					#br(),br(),
					tags$hr(),

					textInput("outputFile", label = h5("Output File Name"), value = ""),

					downloadButton('downloadData', 'Download')
    ),
    column(9, #"main"
    				verbatimTextOutput("nText"),
					tableOutput('contents') 
    )
  )
)
					


ui <- navbarPage("Volocity Data Analysis",
	tabPanel("Table",
		sidebarLayout(
			sidebarPanel(
					fileInput('files', 'Choose CSV File', multiple = T),
					#tags$hr(),
					textInput("ids", label = h5("Track IDs to delete (separate IDs by commas)"), value = ""),
					#textInput("timespan", label = h5("Time Point Threshold (inclusive)"), value = "Enter number..."),
					textInput("timespan", label = h5("Delete tracks if number of tracked time points is less than threshold"), value = ""),
					#textInput("timespan", label = h5("Delete tracks by number of tracked time points"), value = ""),
			
					uiOutput("ui"),	
			
					selectInput("type", "Tracks or Objects:", 
						  choices = c("None", "Object", "Track")),

					actionButton("tableButton", "Apply"),
					#br(),br(),
					tags$hr(),

					textInput("outputFile", label = h5("Output File Name"), value = ""),

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

