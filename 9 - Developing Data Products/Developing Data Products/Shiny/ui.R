library(shiny)

shinyUI(
	navbarPage("Simple Shiny Application",
		tabPanel("Analysis",
			fluidPage(
				titlePanel("The relationship between variables and miles per gallon (MPG)"),
				sidebarLayout(
					sidebarPanel(
						selectInput("variable", "Variable:",
							c("Number of cylinders" = "cyl",
							"Weight (lb/1000)" = "wt",
							"Transmission" = "am"
							)),

					checkboxInput("outliers", "Show BoxPlot's outliers", FALSE)
					),

					mainPanel(
						h3(textOutput("caption")),

						tabsetPanel(type = "tabs", 
							tabPanel("BoxPlot", plotOutput("mpgBoxPlot")),
							tabPanel("Regression model", 
								 plotOutput("mpgPlot"),
								 verbatimTextOutput("fit")
							)
						)
					)
				)
			)
		),
		
		tabPanel("Source Code",
			h2("Motor Trend Data"),
			hr(),
			p("This project is an upgrade of the final project of Regression Models Course of the Johns Hopkins University in partnership with Coursera. And aims to demonstrate the relationship between Miles Per Gallon with other variables, with the intent to answer the following questions:"),
			p("1) Is an automatic or manual transmission better for MPG?"),
			p("2) Quantify the MPG difference between automatic and manual transmissions?"),
			p("The data used in this study are from the mtcars dataset and is available in the RStudio. These data were extracted from Motor Trend Magazine US in 1974. It is composed of data such as fuel consumption and 10 aspects that comprise information about design and performace of 32 car models (model 1973-74)."),
			p("Link for the Final Project of Regression Models Course GitHub"),
			a("https://github.com/cardosop/Data-Science-Johns-Hopkins/tree/master/Regression-Models")
		),
		
		tabPanel("Github repository",
			hr(),
			a("https://github.com/cardosop/Data-Science-Johns-Hopkins/tree/master/Developing%20Data%20Products")
		)
	)
)

