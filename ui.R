library(shiny)

source('business-logic.R')

width1 = 8
width2 = 12 - width1

shinyUI(fluidPage(
    titlePanel('Modeling poverty with predictors outside human control'),
    fluidRow(
        column(12,
            p('This application investigates predictors of poverty (or wealth)
that are outside human control.
Think about it: how many cold and poor countries do you know?
How many hot and rich countries do you know?
Intuitively, a correlation appears to exist between temperature and poverty (or wealth, measured with per capita GDP).'),
            p('There are 2 inputs you can manipulate: the predictors used to predict GDP and the geographic region.'),
            p('Temperature predictor: how much can temperature explain GDP?'),
            p('Natural resources predictor: how much can natural resources explain GDP?'),
            p('Landlocked predictor: how much can GDP be explained by whether a country is landlocked?'),
            p('The output shows partial regression plots, if there are many
              predictors, or a standard scatter plot if there is only 1 predictor.
              In all plots, wealth (log of per capita GDP) is on the y axis.'),
            p('The output also shows a coefficient plot with confidence intervals.'),
            p('Finally, on the right, the amount of variation explained is shown. 
              Surprise! 50% of variation in wealth is explained by factors that humans cannot control!')
        )
    ),
    fluidRow(
        column(6,
               checkboxGroupInput('predictors', 
                                  label = 'Choose the predictors', 
                                  choices = get.possible.predictors(),
                                  selected = get.possible.predictors(),
                                  inline = TRUE
               )
        ),
        column(6,
            checkboxGroupInput('regions', 
                           label = 'Choose the regions', 
                           choices = get.region.names(),
                           selected = get.region.names(),
                           inline = TRUE
            )
        )
    ),
    fluidRow(
        column(width1,
               plotOutput("plot1"),
               plotOutput("std.coefficients.plot")
        ),
        column(width2,
            fluidRow(
               column(width2,
                  h5('Chosen Regions:'),
                  textOutput('regions')
               )
            ),
            fluidRow(
               column(width2,
                  h5('Variation Explained:'),
                  tableOutput('var.expl')
               )
            ),
            fluidRow(
                column(width2,
                       plotOutput('var.exp.plot')
                )
            )
        )
    )
))
