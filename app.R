#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)

run_sim <- function(sample_size, mean, sd, homogeneity){
  # simulates an evaluation cycle
  
  # first generate the samples of ability from the distribution (with error)  
  x <- rnorm(sample_size, mean, sd)
  
  # now convert to rubric scores
  cutpoints <- c(1.5, 2.5, 3.5, 4.5) + runif(4, -homogeneity, homogeneity)
  cutpoints <- c(-Inf,cutpoints, Inf)
  scores    <- cut(x,cutpoints, labels = 1:5)
  
  return(list(x= x, rubric_avg = mean(as.integer(scores))))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Pre-Post and Sample Size"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("sample_size",
                  "Number of samples in each group:",
                  min = 1,
                  max = 100,
                  value = 10,
                  step = 1),
      
      sliderInput("mean_ability1",
                  "Average Ability before:",
                  min = 1,
                  max = 5,
                  value = 3,
                  step = .1),
      
      sliderInput("mean_ability2",
                  "Average Ability after:",
                  min = 1,
                  max = 5,
                  value = 3.4,
                  step = .1),
      
      sliderInput("sd_ability",
                  "Ability standard deviation:",
                  min = 0,
                  max = 3,
                  value = .4,
                  step = .1),
      
      sliderInput("error",
                  "Measurement error:",
                  min = 0,
                  max = 3,
                  value = .6,
                  step = .1),
      actionButton("simulate",
                   "Run Simulation")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("historyPlot")

    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    req(input$sample_size, input$mean_ability1, input$mean_ability1, input$sd_ability, input$error)
    
    # standard error
    SE <- (input$sd_ability + input$error) / sqrt(input$sample_size)
    
    # probability that Post > Pre
    prob_diff <- pnorm(0, 
                       mean = input$mean_ability2 - input$mean_ability1, 
                       SE*sqrt(2), 
                       lower.tail = FALSE)
    
    # generate bins based on input$bins from ui.R
    pdf <- data.frame( x  = seq(1,5,.01))
    pdf$Pre <- dnorm(pdf$x, mean = input$mean_ability1, sd = SE)
    pdf$Post <- dnorm(pdf$x, mean = input$mean_ability2, sd = SE)
    pdf <- pdf %>% 
      gather(Var, Value, -x)
    
    ggplot(pdf, aes(x = x, y = Value, group = Var, color = Var, fill = Var)) +
      geom_area(alpha = .3, position = "identity") +
      theme_minimal() +
      theme(legend.text=element_text(size=20),
      axis.text=element_text(size=20),
      axis.title=element_text(size=20),
      plot.title = element_text(size=22)) +
      xlab("Average Score") +
      ylab("") 
    
    #  ggtitle(paste0("Probability Post > Pre = ",round(prob_diff,2)))
    
  })
  
  sim_results  <- eventReactive(input$simulate, {
    
    # run simulation 1000 times and summarize results
    output <- data.frame(Time = 1:1000,
                         Pre  = NA,
                         Post = NA,
                         stringsAsFactors = FALSE)
    
    SE <- (input$sd_ability + input$error) / sqrt(input$sample_size) 
    
    output$Pre <- rnorm(1000, input$mean_ability1, SE )
    output$Post <- rnorm(1000, input$mean_ability2, SE)
    output$Difference <- output$Post - output$Pre
    
    return(output)     
  })
  
  output$historyPlot <- renderPlot({
    
    # get sim results
    mysim <- sim_results()
    
    # plot the goal posts
    lower_bound <- input$mean_ability2 - input$mean_ability1 - input$sd_ability/2
    upper_bound <- input$mean_ability2 - input$mean_ability1 + input$sd_ability/2
    
    
    ggplot(mysim, aes(x = Difference)) +
      geom_histogram(color = "white",alpha = .5) +
      geom_rect(xmin=lower_bound, xmax=upper_bound, ymin=0, ymax=Inf, fill= "blue", alpha = .004) +
      geom_vline(xintercept = 0, color = "red", size = 2) +
      geom_vline(xintercept = input$mean_ability2 - input$mean_ability1, color = "blue", size = 2) +
      theme_minimal() +
      theme(axis.title=element_text(size=20)) +
      xlab("Simulated Difference")

  })
}




# Run the application 
shinyApp(ui = ui, server = server)

# notes: from my validity work with DASL writing data, a good regression model with
# time and grades as predictors. that gives R^2 = .3. As a measure of reliability
# that's the signal variance divided by total variance, or T^2/(E^2 + T^2)
# We also know that the SD of the scores is 1, so T + E = 1.
# The first equation gives T^2 = .3T^2 + .3E^2, so .7T^2 = .3E^2, or 7/3 T^2 = E^2. 
# taking square roots gives 1.5 T = E, or 1.5 T - E = 0. Adding that to T + E = 1
# gives 2.5 T = 1, so T = .4 and E = .6.
# check: .16 / (.16 + .36 ) = .16 / .52 = .3
