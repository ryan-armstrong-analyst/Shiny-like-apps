library(shiny)
library(pwr)

ui <- fluidPage(
  ## enable MathJax for LaTeX rendering of mathematical terms
  withMathJax(), 
  
  titlePanel("Two-Sample T-Test: Power, Sample Size, Effect Size, Significance Value Learning Tool"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter any two of: effect size (Cohen's d), sample size per group, or desired power, plus alpha. The app will solve for the missing value."),
      
      selectInput("solveFor", "Solve for:",
                  choices = c("Sample size per group" = "n",
                              "Power" = "power",
                              "Effect size (Cohen's d)" = "d")),
      
      conditionalPanel("input.solveFor != 'd'",
                       numericInput("d", "Effect size (Cohen's d):", value = 0.5, min = 0.01, step = 0.01),
                       helpText(HTML(paste0(
                         "<b>Cohen's <i>d</i> (two-sample):</b><br/>",
                         "$$d = \\frac{\\mu_1 - \\mu_2}{s_p}$$",
                         "where \\(\\mu_1, \\mu_2\\) are the group means and \\(s_p\\) is the pooled standard deviation:<br/>",
                         "$$s_p = \\sqrt{\\dfrac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1 + n_2 - 2}}$$",
                         "<br/>Cohen’s d expresses how far apart two group means are in terms of standard deviations, making the difference independent of the original measurement scale.<br/>",
                         "<br/><b>Conventional benchmarks:</b>",
                         "<ul>",
                         "<li>Small: \\(d \\approx 0.2\\)</li>",
                         "<li>Medium: \\(d \\approx 0.5\\)</li>",
                         "<li>Large: \\(d \\approx 0.8\\)</li>",
                         "</ul>"
                       )))
      ),
      
      conditionalPanel("input.solveFor != 'n'",
                       numericInput("n", "Sample size per group:", value = 50, min = 2, step = 1)),
      
      conditionalPanel("input.solveFor != 'power'",
                       numericInput("power", "Desired power:", value = 0.8, min = 0.01, max = 0.99, step = 0.01),
                       helpText(HTML(paste0(
                         "<b>Power (1 − β):</b> The probability of correctly rejecting the null hypothesis when the alternative is true.<br/>", #defines power
                         "<i>Functionally:</i> Power is the chance your study will detect a true effect of the specified size." #Defines power
                       )))
      ),
      
      numericInput("alpha", "Significance level (α):", value = 0.05, min = 0.0001, max = 0.2, step = 0.001), #sets alpha thresholds
      helpText(HTML(paste0(
        "<b>Alpha (α):</b> The probability of rejecting the null hypothesis when it is actually true (Type I error).<br/>", #defines alpha and functionality
        "<i>Functionally:</i> Alpha sets the threshold for how much evidence is required before you declare a result statistically significant."
      )))
    ),
    
    mainPanel(
      h3("Result"),
      verbatimTextOutput("result"),
      br(),
      helpText(HTML(paste0(
        "<b>Note:</b> Cohen's <i>d</i> is defined as<br/>",
        "$$d = \\frac{\\mu_1 - \\mu_2}{s_p},$$",
        "where \\(\\mu_1, \\mu_2\\) are the group means and \\(s_p\\) is the pooled standard deviation:<br/>",
        "$$s_p = \\sqrt{\\dfrac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1 + n_2 - 2}}$$",
        "<br/>Cohen’s d expresses how far apart two group means are in terms of standard deviations, making the difference independent of the original measurement scale.<br/>",
        "<br/><b>Conventional benchmarks:</b>",
        "<ul>",
        "<li>Small: \\(d \\approx 0.2\\)</li>",
        "<li>Medium: \\(d \\approx 0.5\\)</li>", #explains Cohen's D and gives terms to explain differences. Also actually writes out notated formula
        "<li>Large: \\(d \\approx 0.8\\)</li>",
        "</ul>"
      )))
    )
  )
)

server <- function(input, output, session) {
  output$result <- renderPrint({
    tryCatch({
      if (input$solveFor == "n") {
        res <- pwr.t.test(d = input$d, sig.level = input$alpha, power = input$power, type = "two.sample")
        cat("Required sample size per group:", ceiling(res$n), "\n") # uses power of a t.test for each in a simple if else
      } else if (input$solveFor == "power") {
        res <- pwr.t.test(d = input$d, sig.level = input$alpha, n = input$n, type = "two.sample")
        cat("Achieved power with n =", input$n, "per group:", round(res$power, 3), "\n")
      } else {
        res <- pwr.t.test(n = input$n, sig.level = input$alpha, power = input$power, type = "two.sample")
        cat("Required effect size (Cohen's d):", round(res$d, 3), "\n")
      }
    }, error = function(e) {
      cat("Error:", e$message, "\n")
    })
  })
}

shinyApp(ui, server)
