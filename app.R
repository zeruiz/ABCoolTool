library(shiny)
library(ggplot2)

# Define UI for dataset viewer app ----
  ui <- navbarPage("Simple A/B Testing Helper",
                   tabPanel("Significance Test",
                            # App title ----
                            fluidRow(
                                     h4("Step 1: input summary metrics for two groups"),
                                     column(5, wellPanel(
                                            numericInput("p1", "Conversion Rate for Renter:", 0.5, min = 0, max = 1), 
                                            numericInput("n1", "Visitors:", 50)
                                      )
                                     ),
                                     column(5, offset = 1, wellPanel(
                                            numericInput("p2", "Conversion Rate for Renter+OTG:", 0.6, min = 0, max = 1),
                                            numericInput("n2", "Visitors:", 60)
                                      )
                                     )
                            ),
                            fluidRow(    
                                     h4("Step 2: set criteria for testing"),
                                     column(5, h4("Select confidence (1-alpha)"),
                                       sliderInput("alpha1", "How important is it that you do not erroneously report a difference when, 
                                                    in reality, the variations are the same? \n This is the statistical confidence. The higher you set the statistical confidence, the less likely the statistical results will return a false 
                                                    difference.",
                                                   min = 0.5, max = 1, value = 0.95)
                                     ),
                                     column(5, h4("Select  1-tail vs. 2-tail"), offset = 1,
                                       radioButtons("tail1", "Is your hypothesis directional (e.g., you expect the conversion rate to 
                                           increase) or non-directional (e.g., you expect the challenger to be different,
                                           but you have no evidence to support the challenger as better)?", 
                                                    c("My hypothesis is directional and I want to test if B is BETTER than A (1-tailed)" = "tail1",
                                                      "My hypothesis is non-directional and I want to test if B is DIFFERENT than A (2-tailed)" = "tail2"))
                                     )
                            ),
                            hr(),
                            fluidRow(h4("Output: result and suggestion"),
                              column(4, h4("p-value:"),
                                     verbatimTextOutput("pval"),
                                     h4("Conclusion"),
                                     verbatimTextOutput("conclusion")),
                              column(7, offset = 1, plotOutput("plot"))
                            )
                    ),
                   tabPanel("Visitor/Runtime for Conversion",
                            fluidRow(
                              h4("Step 1: input values from dashboard for the conversion rate"),
                              column(5, wellPanel(
                                numericInput("po", "Baseline renter conversion rate:", 0.6, min = 0, max = 1),
                                numericInput("r", "Estimated lift with OTG:", 0.05, min = -5, max = 5),
                                HTML("<div style=\"color:red;\">e.g., Given baseline as 60% and lift as 5%, then estimated conversion to be 65%.</div>")
                              )
                              ),
                              column(5, offset = 1, wellPanel(
                                     numericInput("vol", "How many visitors in total do you get, on average, over the course of 30 days to start a quote?",
                                                  2000, min=0),
                                     numericInput("prop", "What percent of visitors do you expect to complete a quote? (you can find it in the Power BI Certainly Market dashboard)",
                                                  0.5, min=0, max=1)),
                              )
                              ),
                            fluidRow(
                              h4("Step 2: set the criteria for estimation"),
                              column(5, h4("Select confidence (1-alpha)"),
                                     sliderInput("alpha2", "How important is it that you do not erroneously report a difference when, 
                                                    in reality, the variations are the same? \n This is the statistical confidence. The higher you set the statistical confidence, the less likely the statistical results will return a false 
                                                    difference.",
                                                 min = 0.5, max = 1, value = 0.95)
                              ),
                              column(5, h4("Select  1-tail vs. 2-tail"), offset = 1,
                                     radioButtons("tail2", "Is your hypothesis directional (e.g., you expect the conversion rate to 
                                           increase) or non-directional (e.g., you expect the challenger to be different,
                                           but you have no evidence to support the challenger as better)?", 
                                                  c("My hypothesis is directional and I want to test if B is BETTER than A (1-tailed)" = "one.sided",
                                                    "My hypothesis is non-directional and I want to test if B is DIFFERENT than A (2-tailed)" = "two.sided"))
                              )
                            ),
                            fluidRow(
                              h4("Optional: set the power for the estimation"),
                              column(5, h4("Select power"),
                                     sliderInput("beta", "How important is it that you do not erroneously report NO difference when, in reality, 
                                                 there is a difference between the variations? The higher you set the statistical power, 
                                                 the greater your likelihood of detecting a real difference if one exists and the less likely you will 
                                                 return a false “no difference”. ",
                                                 min = 0.5, max = 1, value = 0.8)
                              )
                            ),
                   hr(),
                   fluidRow(h4("Output: estimated number of visitors and runtime"),
                     column(4, h4("For each group, the number of visitors should be at least:"),
                            verbatimTextOutput("calc_results")
                     ),
                     column(4, h4("The total number of visitors should be at least:"),
                            verbatimTextOutput("total_results")
                     ),
                     column(4, h4("Runtime (days) based on the lift % desired:"),
                            verbatimTextOutput("run_time")
                     )
                   )
                   ),
                   tabPanel("More",
                       column(10,  "I hypothsize that OTG", selectInput("metric", 
                                                             label = "choose",
                                                             choices = list("conversion", "web visitors")) 
                       )
                     
                   )
                   
  )


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
      output$plot <- renderPlot({
        p1 <- input$p1
        p2 <- input$p2
        n1 <- input$n1
        n2 <- input$n2
        a <- 1- input$alpha1
        d <- p2 - p1
        s <- p1*(1-p1)/n1 + p2*(1-p2)/n2
        x <- seq(-5,5,length=200)
        y <- dnorm(x)
        test_statistic <- d/sqrt(s)
        df <- data.frame(x=x, y=y)
        if (input$tail1 == "tail1"){
          if (d > 0){
            areax <- seq(-qnorm(a), 5, length.out = 200)
          }else{
            areax <- seq(-5, qnorm(a), length.out = 200)
          }
          area <- data.frame(x = areax, ymin = 0, ymax = dnorm(areax))
          ggplot(data = df, aes(x=x,y=y))+geom_line()+ 
            geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax))+
            geom_vline(xintercept = test_statistic, color = "red")
        }else{
          areaxl <- seq(-5, qnorm(a/2), length.out = 200)
          areaxr <- seq(-qnorm(a/2), 5, length.out = 200)
          areal <- data.frame(x = areaxl, ymin = 0, ymax = dnorm(areaxl))
          arear <- data.frame(x = areaxr, ymin = 0, ymax = dnorm(areaxr))
          ggplot(data = df, aes(x=x,y=y))+geom_line()+ 
            geom_ribbon(data = areal, mapping = aes(x = x, ymin = ymin, ymax = ymax))+
            geom_ribbon(data = arear, mapping = aes(x = x, ymin = ymin, ymax = ymax))+
            geom_vline(xintercept = test_statistic, color = "red")
        }
        })
      
      output$pval <- reactive({
        p1 <- input$p1
        p2 <- input$p2
        n1 <- input$n1
        n2 <- input$n2
        d <- p2 - p1
        s <- p1*(1-p1)/n1 + p2*(1-p2)/n2
        if (input$tail1 == "tail1"){
          return(1-pnorm(d/sqrt(s)))
        }else{
          return(2*(1-pnorm(d/sqrt(s))))
        }
      })
      
      output$conclusion <- reactive({
        p1 <- input$p1
        p2 <- input$p2
        n1 <- input$n1
        n2 <- input$n2
        d <- p2 - p1
        s <- p1*(1-p1)/n1 + p2*(1-p2)/n2
        if (input$tail1 == "tail1"){
          p <- 1-pnorm(d/sqrt(s))
        }else{
          p <- 2*(1-pnorm(d/sqrt(s)))
        }
        if (p < 1-input$alpha1){
          return("Reject the null hypothesis that two groups have the same rate.")
        }else{
          return("No enough evidence to say that two groups have different rates.")
        }
      })
      
      output$calc_results <- reactive({
        tryCatch(
          {
            # Calculate the result
            calc_n <- power.prop.test(p1 = input$po,
                                      p2 = input$po + input$r,
                                      sig.level = 1-input$alpha2,
                                      power = input$beta,
                                      alternative = input$tail2)
            
            # Extract the sample size from the returned list and return it
            return(round(calc_n$n))
          },
          error = function(e){
            return("N/A")
          }
        )
      })
      
      output$total_results <- reactive({
        tryCatch(
          {
        calc_n <- power.prop.test(p1 = input$po,
                                  p2 = input$po  + input$r,
                                  sig.level = 1-input$alpha2,
                                  power = input$beta,
                                  alternative = input$tail2)
        return(round(calc_n$n)*2)
          },
        error = function(e){
          return("N/A")
        }
        )
      })
      
      output$run_time <- reactive({
        tryCatch(
          {
        calc_n <- power.prop.test(p1 = input$po,
                                  p2 = (input$po) * (1 + input$r),
                                  sig.level = 1-input$alpha2,
                                  power = input$beta,
                                  alternative = input$tail2)
        days <- round(calc_n$n)*2/input$vol*input$prop*30
        return(days)
          },
        error = function(e){
          return("N/A")
        }
        )
      })
      
}

shinyApp(ui = ui, server = server)

