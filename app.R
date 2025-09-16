library(shiny)

ui <- fluidPage( #fluid page makes it adjust to the screen width
  titlePanel("Common Statistical Distributions Learning Tool"),
  
  # load MathJax globally
  withMathJax(),
  
  # add JS helper so MathJax re-typesets dynamic UI
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('mathjax-typeset', function(message) {
      if (window.MathJax) {
        if (MathJax.typesetPromise) {
          MathJax.typesetPromise([document.getElementById(message.id)])
        } else if (MathJax.Hub && MathJax.Hub.Queue) {
          MathJax.Hub.Queue(['Typeset', MathJax.Hub, document.getElementById(message.id)]);
        }
      }
    });
  "))), # this whole piece tells the app that it can use LaTeX for the math formulas (PDFs written out)
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose distribution:",
                  choices = c("Normal", "Exponential", "Binomial",
                              "Geometric", "Poisson", "Negative Binomial",
                              "Gamma")), # picks distribution
      uiOutput("paramUI"), # this is the placeholder that is filled in by specific distributions
      sliderInput("n", "Sample size:", min = 0, max = 10000, value = 10000, step = 100)
    ), # controls the number of samples, minimum, maximum, step size, etc
    
    mainPanel( #this sets all the plots
      tabsetPanel(
        tabPanel("Facts", uiOutput("facts")),
        tabPanel("PDF / PMF", plotOutput("pdfPlot")),
        tabPanel("CDF", plotOutput("cdfPlot")),
        tabPanel("Samples", plotOutput("samplePlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamic parameter inputs
  output$paramUI <- renderUI({
    switch(input$dist,
           "Binomial" = tagList( #sets the ui for each of the distribitions
             numericInput("size", "Number of trials (n):", 10, min = 1),
             sliderInput("prob", "Probability (p):", 0, 1, 0.5)
           ),
           "Negative Binomial" = tagList(
             numericInput("r", "Number of successes (r):", 5, min = 1),
             sliderInput("prob", "Probability (p):", 0, 1, 0.5)
           ),
           "Geometric" = sliderInput("prob", "Probability (p):", 0, 1, 0.5),
           "Poisson" = numericInput("lambda", "Rate (λ):", 2, min = 0.01),
           "Exponential" = numericInput("rate", "Rate (λ):", 1, min = 0.01),
           "Normal" = tagList(
             numericInput("mean", "Mean (μ):", 0),
             numericInput("sd", "SD (σ):", 1, min = 0.1)
           ),
           "Gamma" = tagList(
             numericInput("shape", "Shape (α):", 2, min = 0.01),
             numericInput("rate", "Rate (β):", 1, min = 0.01)
           )
    )
  })
  
  # Distribution facts (description + formulas)
  output$facts <- renderUI({
    desc <- switch(input$dist,
                   "Normal" = list( #writes out all text for all of the distributions
                     "The normal distribution is a continuous probability distribution with a bell-shaped curve. It is defined by: the mean (μ), which determines the center, and the standard deviation (σ), which measures how spread out the values are. Most observations lie close to the mean, with probabilities decreasing smoothly as values move further away. Many phenomena, such as test scores, and measurement errors, are well-approximated by a normal distribution. Almost all parametric statistical tests assume a normal distribution. Probability Density Function (PDF):",
                     "$$f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} e^{-\\tfrac{(x-\\mu)^2}{2\\sigma^2}}$$"),
                   "Exponential" = list(
                     "The exponential distribution is a continuous probability distribution that describes the amount of time until an event occurs, such as the lifetime of a device or the time until the next customer arrives. It is defined by a single parameter, the rate (λ), which determines how quickly events happen. Probability Density Function (PDF):",
                     "$$f(x) = \\lambda e^{-\\lambda x}, \\quad x \\geq 0$$"),
                   "Binomial" = list(
                     "The binomial distribution is a discrete probability distribution that describes the number of successes in a fixed number of independent trials, where each trial has the same probability of success (p). It is defined by two parameters: the number of trials (n) and the probability of success (p). Examples include modeling the number of heads in coin flips or the number of defective items in a batch. Probability Density Function (PDF):",
                     "$$P(X=k) = {n \\choose k} p^k (1-p)^{n-k}, \\quad k=0,1,\\ldots,n$$"),
                   "Geometric" = list(
                     "The geometric distribution is a discrete probability distribution that describes the number of trials needed to get the first success in repeated independent trials, each with probability of success (p). It has one parameter, p, and is useful for modeling scenarios such as the number of coin flips until the first head or the number of customers approached before making a sale. Probability Density Function (PDF):",
                     "$$P(X=x) = (1-p)^{x-1} p, \\quad x=1,2,\\ldots$$"),
                   "Poisson" = list(
                     "The Poisson distribution is a discrete probability distribution that describes the number of events occurring within a fixed interval of time or space, when events happen independently and at a constant average rate (λ). It is often used to model counts, such as the number of phone calls received in an hour or the number of misprints per page in a book. Probability Density Function (PDF):",
                     "$$P(X=k) = \\frac{\\lambda^k e^{-\\lambda}}{k!}, \\quad k=0,1,\\ldots$$"),
                   "Negative Binomial" = list(
                     "The negative binomial distribution is a discrete probability distribution that describes the number of failures observed before achieving a specified number of successes (r) in repeated independent trials, each with probability of success (p). It generalizes the geometric distribution (which corresponds to the case r = 1) and is useful for modeling overdispersed count data, such as the number of accidents before reaching a safety target or the number of sales calls required to close several deals. Probability Density Function (PDF):",
                     "$$P(X=x) = {x+r-1 \\choose r-1} (1-p)^x p^r, \\quad x=0,1,\\ldots$$"),
                   "Gamma" = list(
                     "The gamma distribution is a continuous probability distribution that generalizes the exponential distribution. It is defined by two parameters: a shape parameter (α) and a rate parameter (β). The gamma distribution is often used to model waiting times or lifetimes when the process involves multiple stages, such as the total time for several tasks to be completed or the lifespan of systems with multiple components. Probability Density Function (PDF):",
                     "$$f(x) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x}, \\quad x > 0$$")
    )
    
    withMathJax( #this turns any $$ ... $$ math to latex format and formats the rest
      tags$div(
        id = "facts",
        h3(input$dist),
        p(desc[[1]]),
        HTML(desc[[2]])
      )
    )
  })
  
  

  observeEvent(input$dist, {
    session$sendCustomMessage("mathjax-typeset", list(id = "facts"))
  }, ignoreInit = FALSE) #changes when distribution changes
  
  # Samples
  samples <- reactive({
    req(input$n)
    switch(input$dist,
           "Binomial" = { req(input$size, input$prob); rbinom(input$n, size = input$size, prob = input$prob) },
           "Negative Binomial" = { req(input$r, input$prob); rnbinom(input$n, size = input$r, prob = input$prob) },
           "Geometric" = { req(input$prob); rgeom(input$n, prob = input$prob) + 1 },
           "Poisson" = { req(input$lambda); rpois(input$n, lambda = input$lambda) },
           "Exponential" = { req(input$rate); rexp(input$n, rate = input$rate) },
           "Normal" = { req(input$mean, input$sd); rnorm(input$n, mean = input$mean, sd = input$sd) },
           "Gamma" = { req(input$shape, input$rate); rgamma(input$n, shape = input$shape, rate = input$rate) }
    )
  }) #specific inputs for the click and drag for each distribution
  
  # PDF / PMF plots
  output$pdfPlot <- renderPlot({
    s <- samples(); req(s)
    x <- seq(min(s), max(s), length.out = 200)
    if (input$dist == "Binomial") {
      k <- 0:input$size
      plot(k, dbinom(k, input$size, input$prob), type="h", lwd=2, main="Binomial PMF")
    } else if (input$dist == "Negative Binomial") {
      k <- 0:max(s)
      plot(k, dnbinom(k, size=input$r, prob=input$prob), type="h", lwd=2, main="Negative Binomial PMF")
    } else if (input$dist == "Geometric") {
      k <- 1:max(s)
      plot(k, dgeom(k-1, prob=input$prob), type="h", lwd=2, main="Geometric PMF")
    } else if (input$dist == "Poisson") {
      k <- 0:max(s)
      plot(k, dpois(k, input$lambda), type="h", lwd=2, main="Poisson PMF")
    } else if (input$dist == "Exponential") {
      plot(x, dexp(x, input$rate), type="l", lwd=2, main="Exponential PDF")
    } else if (input$dist == "Normal") {
      plot(x, dnorm(x, input$mean, input$sd), type="l", lwd=2, main="Normal PDF")
    } else if (input$dist == "Gamma") {
      plot(x, dgamma(x, shape=input$shape, rate=input$rate), type="l", lwd=2, main="Gamma PDF")
    }
  })
  
  # CDF plots
  output$cdfPlot <- renderPlot({
    s <- samples(); req(s)
    x <- seq(min(s), max(s), length.out = 200)
    if (input$dist == "Binomial") {
      k <- 0:input$size
      plot(k, pbinom(k, input$size, input$prob), type="s", lwd=2, main="Binomial CDF")
    } else if (input$dist == "Negative Binomial") {
      k <- 0:max(s)
      plot(k, pnbinom(k, size=input$r, prob=input$prob), type="s", lwd=2, main="Negative Binomial CDF")
    } else if (input$dist == "Geometric") {
      k <- 1:max(s)
      plot(k, pgeom(k-1, prob=input$prob), type="s", lwd=2, main="Geometric CDF")
    } else if (input$dist == "Poisson") {
      k <- 0:max(s)
      plot(k, ppois(k, input$lambda), type="s", lwd=2, main="Poisson CDF")
    } else if (input$dist == "Exponential") {
      plot(x, pexp(x, input$rate), type="l", lwd=2, main="Exponential CDF")
    } else if (input$dist == "Normal") {
      plot(x, pnorm(x, input$mean, input$sd), type="l", lwd=2, main="Normal CDF")
    } else if (input$dist == "Gamma") {
      plot(x, pgamma(x, shape=input$shape, rate=input$rate), type="l", lwd=2, main="Gamma CDF")
    }
  })
  
  # Histogram of samples
  output$samplePlot <- renderPlot({
    s <- samples(); req(s)
    hist(s, breaks = 20, col = "gray", border = "white",
         main = "Random Samples", xlab = "Value")
  })
}

shinyApp(ui, server)