# Packages & options
library(shiny)
library(gstat)
library(sp)
library(lattice)
library(gridExtra)
options(warn = F)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Having fun with goestatistics"),
  
  # Instructions
  h3("Instructions:"),
  p("1. Choose variogram parameters"),
  p("2. Hit Run! button and see what it looks like"),
  p("3. Repeat"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Model:", c("Spherical" = "Sph","Gaussian" = "Gau", "Exponential" = "Exp")),
      sliderInput("range", "Range:", min = 0.1, max = 100, value = 0.1, step = 5),
      sliderInput("nugget", "Nugget:Sill", min = 0, max = 100, value = 100, step = 5),
      actionButton("run", label = "Run!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Define prediction grid
  x <- y <- seq(0, 100, by = 5)
  pred_grid <- expand.grid(x = x, y = y)
  pred_grid$sim1 <- rnorm(length(x)**2)
  coordinates(pred_grid) <- ~ x + y
  gridded(pred_grid) <- T
  
  # Set variogram model from input
  vm <- reactive({
    vgm(100-input$nugget, model = input$model, nugget = input$nugget, range = input$range)
  })
  
  output$plot <- renderPlot({
    v <- variogramLine(object = vm(), maxdist = 100, n = 100)
    p1 <- xyplot(gamma ~ dist, v, type = "n", ylim = c(0, 105), xlab = "Distance", ylab = "Semivariance")
    p2 <- spplot(pred_grid, zcol = "sim1")
    grid.arrange(p1, p2, ncol = 2)
  })
  
  # Get predictions
  observeEvent(input$run, {
    
    # Get predictions
    g <- gstat(id  = 'sim1', formula = sim1 ~ 1, model = vm(), data = pred_grid)
    preds <- predict(g, newdata = pred_grid, nsim = 1, beta = 0)
    print("termino")
    
    output$plot <- renderPlot({
      
      v <- variogramLine(object = vm(), maxdist = 100, n = 100)
      p1 <- xyplot(gamma ~ dist, v, type = "l", ylim = c(0, 105), xlab = "Distance", ylab = "Semivariance")
      p2 <- spplot(preds, zcol = "sim1")
      grid.arrange(p1, p2, ncol = 2)
      
    })
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)