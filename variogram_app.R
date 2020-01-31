#----
# Shiny application for exploring variogram properties
#----

# To do:
# add support for trend
# add options for block kriging
# add support for anisotropy
# add support for other popular covariance modelsno, 


# Packages & options
library(shiny)
library(gstat)
library(sp)
library(lattice)
library(gridExtra)
library(parallel)
options(warn = F)

# Messages

es <- c(
  title = "Jugando con geoestadística",
  instructions_title = "Instrucciones:",
  instructions1 = "1. Elegir los parámetros del variogram (nugget, sill, range)",
  instructions2 = "2. Definir la resolución de la grilla entre 1 y 5 pixeles (una grilla mas fina produce mejores imágenes pero demora más)",
  instructions3 = "3. Elegir si hacer block kriging o punctual kriging",
  instructions4 = "4. Hacer click en Correr! y ver que pasa",
  instructions5 = "5. Repetir",
  run = "Correr!",
  model = "Modelo"
)


# Define UI for setting the variogram parameters and grid options
ui <- fluidPage(
  
  # Application title
  titlePanel("Spatial Structure simulator"),
  
  # Instructions
  h3("Instructions:"),
  p("1. Choose variogram parameters (model, range, nugget:sill)"),
  p("1. Define grid resolution from 1 to 10 (small grid sizes result in better images but take more time to run)"),
#  p("3. Check if block kriging, otherwise punctual kriging is used"),
  p("4. Hit Run! button and see what happens"),
  p("5. Repeat"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Model:", c("Spherical" = "Sph","Gaussian" = "Gau", "Exponential" = "Exp")),
      sliderInput("range", "Range:", min = 0.1, max = 100, value = 30, step = 5),
      sliderInput("nugget", "Nugget:Sill", min = 0, max = 100, value = 10, step = 5),
      sliderInput("res", "Grid resolution:", min = 1, max = 5, value = 4, step = 1),
 #     checkboxInput("block", "Block kriging", value = T),
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

# Define server
server <- function(input, output) {
  
  # Define simulation grid
  x_sim <- y_sim <- seq(0, 200, by = 10)
  sim_grid <- expand.grid(x = x_sim, y = y_sim)
  gridded(sim_grid) <- ~ x + y

  # Define prediction grid
  pred_grid <- reactive({
    
    set.seed(1)
    x_pred <- y_pred <- seq(0, 200, length.out = 200/input$res)
    pred_grid <- expand.grid(x = x_pred, y = y_pred)
    pred_grid$z <- 0
    gridded(pred_grid) <- ~ x + y
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
    vgm(1-input$nugget/100, model = input$model, nugget = input$nugget/100, range = range)
    
  })
  
  output$plot <- renderPlot({
    
    v <- variogramLine(object = vm(), maxdist = 100, n = 100)
    p1 <- xyplot(gamma ~ dist, v, type = "l", ylim = c(0, 1.05), xlab = "Distance", ylab = "Semivariance")
    p2 <- spplot(pred_grid(), zcol = "z", col.regions = "transparent", scales = list(draw = T))
    grid.arrange(p1, p2, ncol = 2)
  
  })
  
  # Get predictions
  observeEvent(input$run, {
    
    set.seed(1)
    
    
    progress <- showNotification("Unconditional Gaussian simulation is in progress (it may take a while)", duration = NULL)
    
    # Simulating on coarser grid
    g <- gstat(formula = z ~ 1, model = vm(), dummy = T, data = sim_grid, beta = 0)
    sims <- predict(g, newdata = sim_grid, nsim = 1)
    print(names(sims))
    removeNotification(progress)
    showNotification("Unconditional Gaussian simulation has finished")

    # Kriging simulated values on user defined grid
    progress <- showNotification("Ordinary kriging is in progress (it may take a while)", duration = NULL)
  
    # if(input$block) {
    #   block <- c(2,2) 
    # } else {
    #   block <- NULL
    # }

    block <- NULL    

    g <- gstat(id = "sim1", formula = sim1 ~ 1, model = vm(), data = sims)
    preds <- predict(g, newdata = pred_grid(), block = block)
    removeNotification(progress)
    showNotification("Interpolation has finished")
    
    output$plot <- renderPlot({
      
      v <- variogramLine(object = vm(), maxdist = 100, n = 100)
      p1 <- xyplot(gamma ~ dist, v, type = "l", ylim = c(0, 1.05), xlab = "Distance", ylab = "Semivariance")
      p2 <- spplot(preds, zcol = "sim1.pred", scales = list(draw = T))
      names(preds)
      grid.arrange(p1, p2, ncol = 2)
      
    })
  })
  
  observeEvent(input$stop, {
    
    stopApp()

  })

}


# Run the application
shinyApp(ui = ui, server = server)
