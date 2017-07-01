library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Distributions"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Continuous type", tabName = "continuous", icon = icon("dashboard")),
                menuItem("Discrete type", tabName = "discrete", icon = icon("dashboard"))
                )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "continuous",
      fluidRow(
      column(width = 4, selectInput("distribution", label = "Step 1: Choose a distribution",
                  choices = c("Normal", "Unifrom", "Exponential"),
                  selected = "Normal", multiple = FALSE)),
      column(width = 8, sliderInput("number", label = "Step 2: Set sample size", min = 30, max = 1000, 
                      value = 100))
    ),
    fluidRow(
      column(width = 12, plotOutput("plot1"))
    )),
    tabItem(tabName = "discrete",
      fluidRow(
      column(width = 4, selectInput("distribution2", label = "Step 1: Choose a distribution",
                                    choices = c("Binomail(n=50, p=0.1)", "Binomail(n=50, p=0.3)",
                                                "Binomail(n=50, p=0.5)",
                                                "Poisson(lambda=1)", "Poisson(lambda=10)"),
                                    selected = "Binomail(n=50, p=0.5)", multiple = FALSE)),
      column(width = 8, sliderInput("number2", label = "Step 2: Set sample size", min = 100, max = 1000, 
                                    value = 300))
    ),
    fluidRow(
      column(width = 12, plotOutput("plot2"))
    )))
    
  ))


server <- function(input, output)
{the_data <- reactive({
  if(input$distribution == "Normal") d <- rnorm(input$number)
  if(input$distribution == "Unifrom") d <- runif(input$number)  
  if(input$distribution == "Exponential") d <- rexp(input$number) 
  return(d)
 })

the_data2 <- reactive({
  if(input$distribution2 == "Binomail(n=50, p=0.1)") d <- rbinom(input$number2, size = 50, 0.1)
  if(input$distribution2 == "Binomail(n=50, p=0.3)") d <- rbinom(input$number2, size = 50, 0.3)
  if(input$distribution2 == "Binomail(n=50, p=0.5)") d <- rbinom(input$number2, size = 50, 0.5)
  if(input$distribution2 == "Poisson(lambda=1)") d <- rpois(input$number2, 1)
  if(input$distribution2 == "Poisson(lambda=10)") d <- rpois(input$number2, 10)
  the_df <- as.data.frame(table(d))
  names(the_df) <- c("x", "Frequency")
  return(the_df)
})


output$plot1 <- renderPlot({hist(the_data(), xlab = "x", main = "")})
output$plot2 <- renderPlot({plot(the_data2()$x, the_data2()$Frequency, type="h", 
                                 xlab = "x", ylab="Frequency")})
  
}

shinyApp(ui = ui, server = server)