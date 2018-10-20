library(shiny)
library(datasets)

cars <- mtcars
cars$am <- factor(cars$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
	fText <- reactive({
	paste("mpg ~", input$variable)
	})
	fPoint <- reactive({
		paste("mpg ~", "as.integer(", input$variable, ")")
	})
	fit <- reactive({
		lm(as.formula(fPoint()), data=cars)
	})
	output$caption <- renderText({
		fText()
	})
	output$mpgBoxPlot <- renderPlot({
		boxplot(as.formula(fText()), data = cars, outline = input$outliers)
	})

	output$fit <- renderPrint({
		summary(fit())
	})

	output$mpgPlot <- renderPlot({
		with(cars, {
			plot(as.formula(fPoint()))
			abline(fit(), col=2)
		})
	})

})