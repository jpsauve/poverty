library(shiny)

source('business-logic.R')

shinyUI(fluidPage(
    titlePanel('Modeling poverty with predictors outside human control'),
    fluidRow(
        column(12,
            p('This application investigates predictors of poverty
              that are outside human control. 
              The application includes temperature, natural resources and whether a country is landlocked.')
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
        column(8,
               plotOutput("plot1"),
               plotOutput("std.coefficients.plot")
        ),
        column(4,
            fluidRow(
               column(4,
                  h5('Chosen Regions:'),
                  textOutput('regions')
               )
            ),
            fluidRow(
               column(4,
                  h5('Variation Explained:'),
                  tableOutput('var.expl')
               )
            )
        )
    )
))
