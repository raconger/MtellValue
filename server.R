library(shiny)
library(zoo)
library(ggplot2)
library(reshape2)
library(plyr)


shinyServer( 
        function(input, output) {
          
          
            days <- 365.25*5 # 5 Year
            mtbf.in <- eventReactive(input$go, {input$mtbf})
            rate.std.in <- eventReactive(input$go, {input$rate.std})
            mttr.in <- eventReactive(input$go, {input$mttr})
            mttr.std.in <- eventReactive(input$go, {input$mttr.std})
            cost.in <- eventReactive(input$go, {input$cost})
            cost.std.in <- eventReactive(input$go, {input$cost.std})
            rev.in <- eventReactive(input$go, {input$rev}) 
            rev.std.in <- eventReactive(input$go, {input$prod.std})
            detect.in <- eventReactive(input$go, {input$detect})
            impact.cost.in <- eventReactive(input$go, {input$impact.cost})
            impact.mttr.in <- eventReactive(input$go, {input$impact.mttr})
            runs <- eventReactive(input$go, {input$runs})
            
            
            # Establish the two functions
            run.base <- function(){
              
              mtbf.in <- mtbf.in()
              rate.std.in <- rate.std.in()
              mttr.in <- mttr.in()
              mttr.std.in <- mttr.std.in()
              cost.in <- cost.in()
              cost.std.in <- cost.std.in()
              rev.in <- rev.in() 
              rev.std.in <- rev.std.in()
              
              location <- log(mtbf.in^2 / sqrt((mtbf.in*rate.std.in)^2 + mtbf.in^2))
              shape <- sqrt(log(1 + ((mtbf.in*rate.std.in)^2 / mtbf.in^2)))
              
              mtbf = rlnorm(1, mean = location, sd = shape)
              duration = rnorm(1, mean = mttr.in, sd = mttr.in*mttr.std.in)
              cost = rnorm(1, mean = cost.in, sd = cost.in*cost.std.in)
              production = rnorm(1, rev.in, sd = rev.in*rev.std.in)
              
              events <- floor(days/mtbf)
              yearlycost <- (events*cost + events*duration*production)/5
              return(yearlycost)
            }
            
            run.mtell <- function(){
              
              mtbf.in <- mtbf.in()
              rate.std.in <- rate.std.in()
              mttr.in <- mttr.in()
              mttr.std.in <- mttr.std.in()
              cost.in <- cost.in()
              cost.std.in <- cost.std.in()
              rev.in <- rev.in() 
              rev.std.in <- rev.std.in()
              detect.in <- detect.in()
              impact.cost.in <- impact.cost.in()
              impact.mttr.in <- impact.mttr.in()
              
              location <- log(mtbf.in^2 / sqrt((mtbf.in*rate.std.in)^2 + mtbf.in^2))
              shape <- sqrt(log(1 + ((mtbf.in*rate.std.in)^2 / mtbf.in^2)))
              
              mtbf = rlnorm(1, mean = location, sd = shape)
              duration = rnorm(1, mean = mttr.in, sd = mttr.in*mttr.std.in)
              cost = rnorm(1, mean = cost.in, sd = cost.in*cost.std.in)
              production = rnorm(1, rev.in, sd = rev.in*rev.std.in)
              
              catch = rbinom(1,1,input$detect)
              costreduce = rnorm(1, mean = impact.cost.in, sd = impact.cost.in*.1)
              mttrreduce = rnorm(1, mean = impact.mttr.in, sd = impact.mttr.in*.1)
              
              events <- floor(days/mtbf)
              yearlycost <- ((events*cost + events*duration*production)-catch*(costreduce*events*cost + mttrreduce*events*duration*production))/5
              return(yearlycost)
            }
            

            
            monte_carlo_base <- eventReactive(input$go, {
            
              runs <- runs()
              replicate(runs,run.base())
            })  
              
            monte_carlo_mtell <- eventReactive(input$go, {
              
              runs <- runs()
              replicate(runs,run.mtell())
            })

            df <- eventReactive(input$go, {
              
              monte_carlo_base <- monte_carlo_base()
              monte_carlo_mtell <- monte_carlo_mtell()
              
              # Take the delta and data frame it
              delta <- monte_carlo_base-monte_carlo_mtell
              df <- data.frame(monte_carlo_base,monte_carlo_mtell,delta)
              melt(data = df, measure.vars = c("monte_carlo_base","monte_carlo_mtell"))
            })
              
            means <- eventReactive(input$go, {  
              
              df <- df()
              ddply(df, "variable", summarise, grp.mean=mean(value))
              
            })

              
          output$plot2 <- renderPlot({
            df <- df()
            
            ggplot(df, aes(delta)) + stat_ecdf(geom = "step", pad = FALSE) +
              geom_vline(data=df, aes(xintercept=mean(df$delta)),
                         linetype="dashed")+
              labs(title = "Cumulative Distribution of Cost Savings")+
              xlab("Yearly Cost, $K")+
              ylab(NULL) 
            })
              
          output$plot3 <- renderPlot({
            df <- df()
            means <- means()
            
            ggplot(df, aes(x=value, color=variable, fill=variable)) +
            geom_density(position="identity", alpha=0.2) +
            geom_vline(data=means, aes(xintercept=grp.mean, color=variable),
                         linetype="dashed")+
            labs(title = "Distribution of Yearly Cost", subtitle= "(Blue = Mtell, Red = Without)")+
              xlab("Yearly Cost, $K")+
              ylab(NULL) + 
              theme(legend.position = "none")
            
            })
          
          output$text_output <- renderUI({
            
            df <- df()
            means <- means()
            
            str1 <- paste("Savings, on average: <b>$", round(mean(df$delta),0), "</b> thousand dollars.")
            str2 <- paste("Savings, 80% probability: <b>$", round(quantile(df$delta,.2),0), "</b> thousand dollars.")
            
            HTML(paste(str1,str2, sep = '<br/>'))
          })
        }
)


          
          
        
