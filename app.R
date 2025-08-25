library(shiny)
library(tidyverse)

# UI!!!
ui <- fluidPage(
  titlePanel("Probability Simulator for a Coin Flip!"),
  # adjusts to screensizes
  sidebarLayout(
    sidebarPanel(#makes a side panel for these inputs
      numericInput("trials", "Number of Trials (n):", value = 100, min = 1),
      numericInput("runs", "Number of Runs (R, for Batch Sim):", value = 100, min = 1),
      actionButton("resample", "Resample")
    ),
    
    mainPanel(
      tabsetPanel(# tabset means that it can switch tabs between single and batch simulation
        tabPanel("Single Simulation",
                 plotOutput("histPlot"),
                 plotOutput("cumulativePlot"),
                 tableOutput("freqTable")),
        
        tabPanel("Batch Simulation",
                 plotOutput("batchPlot"),
                 tableOutput("batchSummary"))
      )
    )
  )
)

# SERVER !!!
server <- function(input, output) {
  
  # Single run results (automatic on load + resample) 
  results <- eventReactive(input$resample, {
    sample(c("Heads", "Tails"), input$trials, replace = TRUE) #flips a coin n times
  }, ignoreNULL = FALSE)  #ignoreNULL means that it runs when app loads, not just after pressing
  
  # Simple Histogram plot!
  output$histPlot <- renderPlot({
    # <-- Change: convert result to factor with both levels -->
    df <- data.frame(result = factor(results(), levels = c("Heads","Tails")))
    theory <- data.frame(result = c("Heads","Tails"), prob = c(0.5,0.5))
    
    ggplot(df, aes(x = result)) +
      geom_bar(fill = "steelblue", alpha = 0.7) +
      geom_point(data = theory, aes(x = result, y = prob*input$trials), #theory is expected probabilities, x and y are real data
                 color = "red", size = 3) +
      geom_line(data = theory, aes(x = result, y = prob*input$trials, group = 1),
                color = "red", linetype = "dashed") +
      scale_x_discrete(limits = c("Heads","Tails")) +
      coord_cartesian(ylim = c(0, input$trials)) +
      labs(x = "Outcome", y = "Frequency",
           title = "Observed vs Theoretical Probabilities of a Coin Flip") +
      theme_minimal()
  }) #the coord cartesian adjusts the y axis without removing data
  
  # Cumulative Coin Flip plot!!!
  output$cumulativePlot <- renderPlot({
    outcomes <- results()
    n <- length(outcomes)
    
    cumulative <- cumsum(outcomes == "Heads") / seq_along(outcomes) #this calculates the heds/tails proportion of heads
    #for each ongoing in the figure
    df <- data.frame(trial = 1:n,
                     cumulative = cumulative,
                     theoretical = rep(0.5, n)) #holds all necessary info
    
    ggplot(df, aes(x = trial)) +
      geom_line(aes(y = cumulative), color = "steelblue") +
      geom_line(aes(y = theoretical), color = "red", linetype = "dashed") +
      coord_cartesian(xlim = c(1, n), ylim = c(0, 1)) +
      labs(x = "Trial", y = "Cumulative Proportion of Heads",
           title = "Law of Large Numbers") +
      theme_minimal()
  })
  
  # Single run: frequency table
  output$freqTable <- renderTable({
    # Convert to factor with both levels
    out <- factor(results(), levels = c("Heads","Tails"))
    tab <- as.data.frame(table(out))
    names(tab) <- c("Outcome","Count")
    tab$Proportion <- tab$Count / length(out)
    tab$Expected <- 0.5 * length(out)
    tab
  })
  
  
  # ----- Batch simulations (auto on load + resample) -----
  batch_sims <- eventReactive(input$resample, {
    n <- input$trials #n amount of sample in "study"
    R <- input$runs #repeats r times
    m <- replicate(R, sample(c("Heads","Tails"), n, TRUE) == "Heads")# makes it replicate r times
    tibble(run = 1:R,
           p_hat = colMeans(m))
  }, ignoreNULL = FALSE)# makes both load automatically when loading in 
  
  # batch plot
  output$batchPlot <- renderPlot({
    dat <- batch_sims()
    n <- input$trials
    R <- input$runs
    p <- 0.5
    sd_theory <- sqrt(p*(1-p)/n)
    
    x_seq <- seq(0, 1, length.out = 200)
    max_density <- max(dnorm(x_seq, mean = p, sd = sd_theory))
    
    ggplot(dat, aes(x = p_hat)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.7, fill = "steelblue") +
      stat_function(fun = dnorm, args = list(mean = p, sd = sd_theory),
                    color = "black", linetype = "dotted", size = 1) +
      geom_vline(xintercept = p, linetype = "dashed", color = "red") +
      coord_cartesian(xlim = c(0,1), ylim = c(0, max_density*1.1)) +
      labs(x = "Sample proportion of Heads (p̂)", y = "Density",
           title = paste0("Sampling Distribution of p̂ (n=", n, ", runs=", R, ")")) +
      theme_minimal()
  })
  
  # summary table
  output$batchSummary <- renderTable({
    dat <- batch_sims()
    n <- input$trials
    R <- input$runs
    p <- 0.5
    tibble(
      `Sample Size (n)` = n,
      `Runs (Number of Simulations)` = R,
      `Sample Mean` = mean(dat$p_hat),
      `Sample Standard Deviation` = sd(dat$p_hat),
      `Theoretical Standard Deviation` = sqrt(p*(1-p)/n)
    )
  })
}
# launch app!
shinyApp(ui = ui, server = server)

#library(rsconnect)
#rsconnect::setAccountInfo(name='ryanarm',
#                          token='FD94A163278EAAA6EA4488A87E9028C2',
#                          secret='NPC9GjnH6MlWvzFRpH/CCEqw2DqQ/FlfCkCwVHgx')

#rsconnect::deployApp('C:/Users/Owner/OneDrive - Westminster College/Desktop/Python Stuff/Probability-App')
