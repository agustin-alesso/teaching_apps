#----
# Shiny application for exploring variogram properties
# Author: Agustin Alesso
#----

# To do:
# add options for block kriging
# add support for anisotropy
# add support for other popular covariance models,
# add translation
# add trend profile
# add new tab for sampling and variogram from sample.
 


# Packages & options
library(shiny)
library(gstat)
library(sp)
library(lattice)
library(gridExtra)
library(parallel)
library(shinythemes)
options(warn = F)


# Messages
lang <- "es"
es <- list(
  title = "Simulador de estructura espacial",
  tab1_title = "Campos aleatorios",
  instructions_title = "Instrucciones:",
  instructions1 = "Elegir los parámetros del variograma",
  instructions2 = "Definir la resolución de la grilla entre 1 y 5 pixeles (una grilla mas fina produce mejores imágenes pero demora más)",
  instructions3 = "Elegir si hacer block kriging o punctual kriging",
  instructions4 = "Hacer click en Correr! y ver que pasa",
  instructions5 = "Repetir",

  run = "Correr!",
  stop = "Parar!",
  model = "Modelo:",
  trend = "Tendencia:",
  range = "Rango:",
  nugget = "Efecto pepita (%):",
  grid_res = "Resolución de grilla: ",
  seed = "Usar semilla fija?",
  no_trend = "constante",
  lin_trend = "lineal",
  exp = "Exponencial",
  sph = "Esférico",
  gau = "Gaussiano"
)

en <- list(
  app_title = "Spatial structure simulator",
  instructions_title = "Instructions:",
  instructions1 = "Choose variogram parameters",
  instructions2 = "Define grid resolution from 1 to 5 pixels",
  #instructions3 = "Elegir si hacer block kriging o punctual kriging",
  instructions3 = "Hit run! and see what happens",
  instructions4 = "Repeat",
  
  run = "Run!",
  stop = "Stop!",
  model = "Model:",
  trend = "Trend:",
  range = "Range:",
  nugget = "Nugget effect (%):",
  grid_res = "Grid resolution:",
  seed = "Use fixed seed?"
  
  
)

txts <- if(lang == 'es') {
  es
} else {
  en
}
  

trend_ch <- c(0,1)
names(trend_ch) <- c(paste(txts$no_trend, "(~ 1)"), paste(txts$lin_trend, "(~ Y)"))

mod_ch <- c("Sph", "Gau", "Exp")
names(mod_ch) <- c(txts$sph, txts$gau, txts$exp)


# Define UI for setting the variogram parameters and grid options
ui <- navbarPage(
  
  title = txts$title,
  theme = shinytheme('cerulean'),
  footer = list(
    #hr(style="height:2px;border-width:10;color:gray;background-color:gray"),
    hr(style = "border-top: 3px solid gray; border-bottom: none; border-left: none; border-right: none"),
    p(icon("github"), a(href="http://www.github.com/agustin-alesso", "by Agustin Alesso "), style="text-align: right")
  ),
  
  # tags$script(HTML("var header = $('.navbar> .container-fluid');
  #                      header.append('<div style=\"float:right\"><h3>Company name text here</h3></div>');
  #                      console.log(header)")),
  
  tabPanel(

    # Application title
    title = txts$tab1_title,
    
    # Instructions
    h3(txts$instructions_title),
    p(paste("1.", txts$instructions1)),
    p(paste("2.", txts$instructions2)),
    p(paste("3.", txts$instructions4)),
    p(paste("4.", txts$instructions5)),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "trend", label = txts$trend, 
          #choices = c("No trend (~ 1)" = 0, "Linear (~ Y)" = 1, "Quadratic (~ Y + Y^2)" = 2)),
          choices = trend_ch
        ),
        selectInput(
          inputId = "model", label = txts$model,
          choices = mod_ch
        ),
        sliderInput(
          inputId = "range", label = txts$range,
          min = 0.1, max = 100, value = 30, step = 5
        ),
        sliderInput(
          inputId = "nugget", label = txts$nugget,
          min = 0, max = 100, value = 10, step = 5
        ),
        sliderInput(
          inputId = "res", label = txts$grid_res,
          min = 1, max = 5, value = 4, step = 1
        ),
   #     checkboxInput("block", "Block kriging", value = T),
        checkboxInput(inputId = "seed1", txts$seed, value = T),
        actionButton(inputId = "run_rf", label = txts$run),
        actionButton(inputId = "stop", label = txts$stop)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot1"),
        #textOutput("text")
  
      )
    )
  )
  
  # tabPanel(
  #   # Application title
  #   title = "Sampling",
  #   
  #   # Instructions
  #   h3("Instructions:"),
  #   p("1. Choose the number of samples to collect"),
  #   p("2. Choose if to use fixed or random seed for simulation."),
  #   p("3. Hit Run! button and see what happens"),
  #   p("4. Repeat"),
  #   
  #   # Sidebar with a slider input for number of bins
  #   sidebarLayout(
  #     sidebarPanel(
  #       sliderInput(
  #         inputId = "sample_size", label = "Number of sampling points: ", 
  #         min = 20, max = 300, value = 150, step = 5
  #       ),
  #       checkboxInput(inputId = "seed2", "Use a fixed seed?", value = T),
  #       actionButton(inputId = "run_sample", label = "Run!")
  #       #actionButton(inputId = "stop", label = "Stop!")
  #     ),
  #     mainPanel(
  #       plotOutput('plot2')
  #     )
  #   ),
  # 
  # )
  
)

# Define server
server <- function(input, output, session) {
  
  # Define simulation grid 10x10
  x_sim <- y_sim <- seq(0, 200, by = 10)
  sim_grid <- expand.grid(x = x_sim, y = y_sim)
  gridded(sim_grid) <- ~ x + y

  # User's defined prediction grid
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
  
  # Variogram plot    
  p1 <- reactive({
    v <- variogramLine(object = vm(), maxdist = 100, n = 100)
    xyplot(gamma ~ dist, v, type = "l", ylim = c(0, 1.05), xlim  = c(0,100), xlab = "Distance", ylab = "Semivariance")
  })

  # Empty prediction plot
  p2 <- reactive({
    spplot(pred_grid(), zcol = "z", col.regions = "transparent", scales = list(draw = T))
  })
    
  plot1 <- reactive({
    grid.arrange(p1(), p2(), ncol = 2)
  })
  
  output$plot1 <- renderPlot({
    
    plot1()
  
  })
  
  output$plot2 <- renderPlot({
  
      p2()
  
  })
  
  # Get predictions
  observeEvent(input$run_rf, {
    
    if (input$seed1) set.seed(1)
    
    progress <- showNotification("Unconditional Gaussian simulation is in progress (it may take a while)", duration = NULL)
    
    # Simulating on coarser grid 10x10
    if(input$trend == 0) {
      mod_fm <- as.formula('z ~ 1')
      betas <- 0
    } else if (input$trend == 1) {
      mod_fm <- as.formula('z ~ y')
      betas <- c(0, 0.01)
    } else if (input$trend == 2) {
      mod_fm <- as.formula('z ~ y + I(y^2)')
      betas <- c(0.5, -0.03, 0.0015)
    }

    g <- gstat(formula = mod_fm, model = vm(), dummy = T, data = sim_grid, beta = betas)
    sims <- predict(g, newdata = sim_grid, nsim = 1)
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
    
    # adapt formula
    mod_fm2 <- formula(paste("sim1 ~ ", as.character(mod_fm)[3]))
    g <- gstat(id = "sim1", formula = mod_fm2, model = vm(), data = sims, beta = betas)
    
    preds <- predict(g, newdata = pred_grid(), block = block)
      
    # Add nois from nugget component
    n <- nrow(pred_grid())
    sill <- var(sims$sim1)
    preds$sim1.pred <- preds$sim1.pred + rnorm(n, mean = 0, sd = sqrt(sill)*input$nugget/100)
    pred_grid2 <- preds
    
    removeNotification(progress)
    showNotification("Interpolation has finished")
    
    # Update variogram plot
    p1 <- reactive({
      v <- variogramLine(object = vm(), maxdist = 100, n = 100)
      xyplot(gamma ~ dist, v, type = "l", ylim = c(0, 1.05), xlim  = c(0,100), xlab = "Distance", ylab = "Semivariance")
    })
    
    # Update prediction plot
    p2 <- spplot(pred_grid2, zcol = "sim1.pred", scales = list(draw = T))

    plot1 <- reactive({
      grid.arrange(p1(), p2, ncol = 2)
    })
    
    output$plot1 <- renderPlot({
      plot1()
    })
    
    # Pass prediction plot to panel 2
    output$plot2 <- renderPlot({
      
      p2()
      
    })
    

  })
  
  # HOW TO SEE UPDATED PREDICTION GRID WITHIN OBSERVEVENT PANEL 2
  
  # Get sampling points
  observeEvent(input$run_sample, {
    
    if (input$seed2) set.seed(1)
    
    sample_pts <- spsample(pred_grid(), n = input$sample_size, type = 'random')
    
    p3 <- spplot(pred_grid(), scales = list(draw = T),
            sp.layout = list("sp.points", sample_pts, col = "black"))
    
    output$plot2 <- renderPlot({
      
      p3
      
    })
  })
  
  observeEvent(input$stop, {
    
    stopApp()

  })

}


# Run the application
shinyApp(ui = ui, server = server)
