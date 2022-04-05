#----
# Shiny application for exploring CLT concept
# Author: Agustin Alesso
#----

library(shiny)
library(ggplot2)

shinyApp(

  ui = fluidPage(

    titlePanel("Distribución de la media muestral"),

    fluidRow(
      column(
        width = 6,
        h3("Población original: de números del 1 al 20"),
        plotOutput("distPlot"),
        h4("Cada histograma representa la distribución de 500 muestras de tamaño:"),
        sliderInput("n", label = "", min = 1, max = 100, value = 30, step = 10)
      )
    )
  ),

  server = function(input, output) {

    output$distPlot <- renderPlot({

      xbars <- replicate(n = 500, expr = {
        mean(sample(1:20, size = input$n, replace =T))
      })

      lbs <- paste("media = ", round(mean(xbars), 2), "\n", "desvio = ", round(sd(xbars), 2))

      ggplot(data.frame(xbars = xbars)) +
        aes(x = xbars) +
        geom_histogram(binwidth = 1) +
        labs(
          #title = expression(paste("Distribución muestral de ", bar(X))),
          x = expression(bar(X)),
          y = "Frecuencia"
        ) +
        lims(x = c(0, 20), y = c(0,300)) +
        theme_classic(base_size = 14) +
        annotate("text", label = lbs, x = 0, y = 250, hjust = 0, size = 6)
    })
  })