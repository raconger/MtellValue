library(shiny)
shinyUI(fluidPage(
        fluidRow(column(6,h1("Mtell - An Inquiry into Value")),
                 column(2,actionButton(inputId = "go", label = "Simulate")),
                 column(4,sliderInput('runs', 'Number of runs',value = 1000, min = 500, max = 10000, step = 500))
        ),
        fluidRow(
          column(4,
                h3('Failure Details'),
                numericInput('mtbf', 'Mean time between failures (days)', value = 200),
                sliderInput('rate.std', 'MTBF relative standard deviation',value = 0.05, min = 0.00, max = .5, step = 0.05),
                numericInput('mttr', 'Mean time to repair (days)', value = 3),
                sliderInput('mttr.std', 'MTTR relative standard deviation',value = 0.1, min = 0.00, max = .5, step = 0.05)),
          column(4,
                h3('Financial Details'),
                numericInput('cost', 'Cost to repair ($k)', value = 100),
                sliderInput('cost.std', 'Cost relative standard deviation',value = 0.05, min = 0.00, max = .5, step = 0.05),
                numericInput('rev', 'Lost revenue per hour down ($k)', value = 100),
                sliderInput('prod.std', 'Revenue relative standard deviation',value = 0.1, min = 0.00, max = .5, step = 0.05)),
          column(4,
                h3('Impact Details'),
                sliderInput('detect', 'Probability of detection', value = 0.8, min = 0.00, max = 1, step = 0.05),
                sliderInput('impact.cost', 'Relative cost reduction',value = 0.5, min = 0.00, max = 1, step = 0.05),
                sliderInput('impact.mttr', 'Relative MTTR reduction',value = 0.2, min = 0.00, max = 1, step = 0.05))
        )
         ,
          fluidRow(
            column(3,h3('Yearly Cost Savings'),htmlOutput("text_output")),
            column(4,plotOutput('plot3')),
            column(4,plotOutput('plot2'))
            
          )
)
)
