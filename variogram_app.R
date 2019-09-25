#----
# Shiny application for exploring variogram properties
#----

# To do:
# add support for trend
# add options for block kriging



set.seed(1)

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
  titlePanel("Having fun with geostatistics"),
  
  # Instructions
  h3("Instructions:"),
  p("1. Choose variogram parameters (model, range, nugget:sill)"),
  p("1. Define grid resolution from 1 to 5 (small grid size results in better images but longer simulation times)"),
  p("3. Check for block kriging, otherwise punctual kriging is used"),
  p("4. Hit Run! button and see what happens"),
  p("5. Repeat"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Model:", c("Spherical" = "Sph","Gaussian" = "Gau", "Exponential" = "Exp")),
      sliderInput("range", "Range:", min = 0.1, max = 100, value = 30, step = 5),
      sliderInput("nugget", "Nugget:Sill", min = 0, max = 100, value = 10, step = 5),
      sliderInput("res", "Grid resolution:", min = 1, max = 5, value = 4, step = 1),
      checkboxInput("block", "Block kriging", value = T),
      actionButton("run", label = "Run!"),
      actionButton("stop", label = "Stop!")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      textOutput("text")

    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pred_grid <- reactive({
    # Define prediction grid
    x <- y <- seq(0, 100, length.out = 100/input$res)
    pred_grid <- expand.grid(x = x, y = y)
    pred_grid$sim1 <- rnorm(length(x)**2)
    coordinates(pred_grid) <- ~ x + y
    gridded(pred_grid) <- T
    pred_grid
  })
  
  # Set variogram model from input
  vm <- reactive({
    if (input$model == "Exp") {
      range <- input$range/3
    } else if (input$model == "Gau") {
      range <- input$range/sqrt(3)
    } else {
      range <- input$range
    }
    vgm(100-input$nugget, model = input$model, nugget = input$nugget, range = range)
  })
  
  output$plot <- renderPlot({
    v <- variogramLine(object = vm(), maxdist = 100, n = 100)
    p1 <- xyplot(gamma ~ dist, v, type = "l", ylim = c(0, 105), xlab = "Distance", ylab = "Semivariance")
    p2 <- spplot(pred_grid(), zcol = "sim1", col.regions = "transparent")
    grid.arrange(p1, p2, ncol = 2)
  })
  
  # Get predictions
  observeEvent(input$run, {
    
    progress <- showNotification("Conditional Gaussian simulation is in progress (it may take a while)", duration = NULL)
    
    # Get predictions
    g <- gstat(id  = 'sim1', formula = sim1 ~ 1, model = vm(), data = pred_grid(), maxdist = 50)
    if(input$block) {
      block <- c(2,2) 
    } else {
      block <- NULL
    }
    preds <- predict(g, newdata = pred_grid(), nsim = 1, beta = 0, block = block)
    removeNotification(progress)
    showNotification("Contitional Gaussian simulation has finished")
    
    output$plot <- renderPlot({
      
      v <- variogramLine(object = vm(), maxdist = 100, n = 100)
      p1 <- xyplot(gamma ~ dist, v, type = "l", ylim = c(0, 105), xlab = "Distance", ylab = "Semivariance")
      p2 <- spplot(preds, zcol = "sim1")
      grid.arrange(p1, p2, ncol = 2)
      
    })
  })
  
  observeEvent(input$stop, {
    
    stopApp()
  })

}


# Run the application
shinyApp(ui = ui, server = server)
