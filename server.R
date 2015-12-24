# TODO
# github
# portfolio https://jpsauve.shinyapps.io/climate-poverty
    # 50% explained by pure geography, over which man cannot do anything!
    # interpretability, not accuracy: lm
    # mention github in portfolio page
# bokeh
# more visually pleasing

library(shiny)
library(arm)
library(ggplot2)
library(car)

source('business-logic.R')
source('CoefficientPlot.R')

df = read.data()

shinyServer(function(input, output) {
    result = reactive({
        set.seed(1)
        # guard against missing input data
        if(length(input$predictors) == 0) {
            predictors = "Annual_temp"
        } else {
            predictors = input$predictors
        }
        if(length(input$regions) == 0) {
            regions = "Africa"
        } else {
            regions = input$regions
        }
        data = filter.data.by.region(df, regions)
        formula = get.formula(predictors)
        list(get.model(formula, df=data), data, regions, predictors)
    })
    output$plot1 <- renderPlot({
        res = result()
        fit = res[[1]]
        df = res[[2]]
        predictors = res[[4]]
        if(length(predictors) > 1) {
            # with many predictors, provide added-variable plots
            avPlots(fit$finalModel, 
                    main='Partial Regression Plots', 
                    labels=df[complete.cases(df),]$Country.Code, 
                    id.n=20,
                    col=df$Region,
                    layout = c(3,1))
        } else {
            # with one predictor: normal scatter plot with prediction interval
            pred = predictors[1]
            ggplot(df, aes_string(pred, 'log(mean.gdp)')) +
                geom_point(aes(colour = df$Region)) +
                geom_text(aes(label=Country.Code)) +
                geom_smooth(method=lm) +
                ggtitle(paste('log GDP versus', get.predictor.label(pred))) +
                xlab(get.predictor.label(pred)) +
                ylab('log GDP')
        }
    }, width=450, height = 400)
    output$std.coefficients.plot <- renderPlot({
        # a coefficient plot
        res = result()
        fit = res[[1]]
        df = res[[2]]
        CoefficientPlot(list(fit$finalModel), 
                        keep.intercept = FALSE,
                        modelnames = c('Regression Coefficients'))
    }, width=450, height = 200)
    output$var.expl = renderTable({
        # a table of percentage of variation explained by the  predictors
        res = result()
        fit = res[[1]]
        df = res[[2]]
        percent.explained(fit$finalModel)[,'PctExp',drop=FALSE]
    })
    output$regions = renderText({
        # display the regions that were selected
        res = result()
        regions = res[[3]]
        paste(regions, collapse=',')
    })
})
