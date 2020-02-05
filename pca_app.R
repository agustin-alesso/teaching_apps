#----
# title: Application to demosntrate the relationship between correlation matrix
#        and the biplot graph
#----

# 1. Define a correlation matrix for 5 variables
# 2. Generate multivariate random sample
# 3. Compute summary statistics (vector of means and var-cov matrix)
# 3. Run eigen decomposition
# 4. Biplot
#


library(shiny)
library(MASS)
library(Matrix)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Principal Components Simulator | CPSC 543", 
               windowTitle = "PCA simulator | CPSC543"),
    
    # Instructions
    h3("Instructions:"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("1. Define the vector of population means"),
            fluidRow(
                column(2, withMathJax(uiOutput('mu1'))),
                column(2, withMathJax(uiOutput('mu2'))),
                column(2, withMathJax(uiOutput('mu3'))),
                column(2, withMathJax(uiOutput('mu4')))
            ),
            fluidRow(
                column(2, numericInput(inputId = "mu1", label = NULL, value = 0, step = 0.05)),
                column(2, numericInput(inputId = "mu2", label = NULL, value = 0, step = 0.05)),
                column(2, numericInput(inputId = "mu3", label = NULL, value = 0, step = 0.05)),
                column(2, numericInput(inputId = "mu4", label = NULL, value = 0, step = 0.05))
            ),
            p("2. Define the vector of population standard deviations"),
            fluidRow(
                column(2, withMathJax(uiOutput('sigma1'))),
                column(2, withMathJax(uiOutput('sigma2'))),
                column(2, withMathJax(uiOutput('sigma3'))),
                column(2, withMathJax(uiOutput('sigma4')))
                
            ),
            fluidRow(
                column(2, numericInput(inputId = "sigma1", label = NULL, value = 1, min = 0.1, step = 0.05)),
                column(2, numericInput(inputId = "sigma2", label = NULL, value = 1, min = 0.1, step = 0.05)),
                column(2, numericInput(inputId = "sigma3", label = NULL, value = 1, min = 0.1, step = 0.05)),
                column(2, numericInput(inputId = "sigma4", label = NULL, value = 1, min = 0.1, step = 0.05))
            ),
            p("3. Define the population correlation matrix (upper triangle only)"),
            fluidRow(
                column(3, offset = 1, "Y2"),
                column(3, "Y3"),
                column(3, "Y4")
            ),
            fluidRow(
                column(1, "Y1"),
                column(3, numericInput(inputId = "r12", label = NULL, value = 0, step = 0.05, min = -1, max = 1)),
                column(3, numericInput(inputId = "r13", label = NULL, value = 0, step = 0.05, min = -1, max = 1)),
                column(3, numericInput(inputId = "r14", label = NULL, value = 0, step = 0.05, min = -1, max = 1))
            ),
            fluidRow(
                column(1, "Y2"),
                column(3, offset = 3, numericInput(inputId = "r23", label = NULL, value = 0, step = 0.05, min = -1, max = 1)),
                column(3, numericInput(inputId = "r24", label = NULL, value = 0, step = 0.05, min = -1, max = 1))
            ),
            fluidRow(
                column(1, "Y3"),
                column(3, offset = 6, numericInput(inputId = "r34", label = NULL, value = 0, step = 0.05, min = -1, max = 1))
            ),
            p("4. Choose the sample size to be drawn from the multivariate normal distribution"),
            numericInput(inputId = "n",  label = NULL, value = 30),
            
            h5("More options"),
            checkboxInput(inputId = "cormat", label = "Use correlation matrix for PCA", value = T),
            checkboxInput(inputId = "popmats", label = "Show population matrices", value = F),
            checkboxInput(inputId = "fixed_axis", label = "Use fixed limits in biplots (-0.5, 0.5)", value = T)
            #numericInput(inputId = "scale",  label = "Scale biplot", value = 1, min = 0, max = 1, step = 0.1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            conditionalPanel(
                condition = "input.popmats == true",
                h5("Population variance-covariance matrix"),
                verbatimTextOutput('pop_vcov'),
                h5("Population Correlation matrix"),
                verbatimTextOutput('pop_r')
            ),
            h5("Sample variance-covariance matrix"),
            verbatimTextOutput('sample_vcov'),
            h5("Sample correlation matrix"),
            verbatimTextOutput('sample_cor'),
            h5("Eigenvalues"),
            verbatimTextOutput('eigenval'),
            h5("Biplot"),
            plotOutput("biplot"),
            h5("Principal components summary"),
            verbatimTextOutput('pca_summary'),
            h5("Loadings"),
            verbatimTextOutput('loadings')
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Get pop mean vector
    mu <- reactive(c(input$mu1, input$mu2, input$mu3, input$mu4))
    
    R <- reactive({
        # Create positive definite R matrix
        rij <- c(input$r12, input$r13, input$r23, input$r14, input$r24, input$r34)
        # rij <- jitter(rij, factor = 0.0001)
        R <- matrix(1, ncol = 4, nrow = 4)
        R[upper.tri(R)] <- rij
        R <- t(R)
        R[upper.tri(R)] <- rij
    
        if (det(R) > 0) {
            showNotification("Positive definite matrix", duration = 15)
        } else {    
            showNotification("Non-positive definite matrix, using the nearest PD matrix", duration = 15, type = "warning")
            R <- as.matrix(nearPD(R, corr = T)$mat)
        }
        
        colnames(R) <- rownames(R) <- paste0("Y", 1:4)
        R
        
    })
    
    
    Sigma <- reactive({
        
        # Get pop sd vector
        D <- diag(c(input$sigma1, input$sigma2, input$sigma3, input$sigma4))
        
        # Get sigma matrix
        Sigma <- D %*% R() %*% D
        Sigma <- as.matrix(Sigma)
        colnames(Sigma) <- rownames(Sigma) <- paste0("Y", 1:4)
        Sigma
    })
    
    output$pop_r <- renderPrint(R())
    output$pop_vcov <- renderPrint(Sigma())
    
    # Simulate data
    data <- reactive({
        Y <- mvrnorm(n = input$n, mu = mu(), Sigma = Sigma(), empirical = F)
        Y <- data.frame(Y)
        colnames(Y) <- paste0("Y", 1:4)
        Y
    })
    output$sample_cor <- renderPrint(cor(data()))
    output$sample_vcov <- renderPrint(var(data()))
    
    # PCA analysis
    
    pca <- reactive({
        if(input$cormat) {
            prcomp(data(), center = T, scale = T, retx = T)
        } else {
            prcomp(data(), center = F, scale = F, retx = T)
        }
    })
    output$pca_summary <- renderPrint(summary(pca()))
    output$eigenval <- renderPrint(pca()$sdev**2)
    output$loadings <- renderPrint(pca()$rotation)
    
    # Create biplot
    output$biplot <- renderPlot({
        
        pts_labs <- rep(".", input$n)
        lims <- c(-0.5, 0.5)
        
        # Plot PCs
        par(mfcol = c(1,2))
        if (input$fixed_axis) {
            biplot(pca(), choices = c(1, 2), xlabs = pts_labs, cex = c(1.5, 1),
                   xlim = lims, ylim = lims)
            biplot(pca(), choices = c(1, 3), xlabs = pts_labs, cex = c(1.5, 1),
                   xlim = lims, ylim = lims)
        } else {
        biplot(pca(), choices = c(1, 2), xlabs = pts_labs, cex = c(1.5, 1))
        biplot(pca(), choices = c(1, 3), xlabs = pts_labs, cex = c(1.5, 1))
        }
        
    }) 
    
    # Mu labels
    output$mu1 <- renderUI({
        return(HTML(paste0("<p>", '$$\\mu_1$$',"</p>")))
    })
    output$mu2 <- renderUI({
        return(HTML(paste0("<p>", '$$\\mu_2$$',"</p>")))
    })
    output$mu3 <- renderUI({
        return(HTML(paste0("<p>", '$$\\mu_3$$',"</p>")))
    })
    output$mu4 <- renderUI({
        return(HTML(paste0("<p>", '$$\\mu_4$$',"</p>")))
    })
    
    # Sigma labels
    output$sigma1 <- renderUI({
        return(HTML(paste0("<p>", '$$\\sigma_1$$',"</p>")))
    })
    output$sigma2 <- renderUI({
        return(HTML(paste0("<p>", '$$\\sigma_2$$',"</p>")))
    })
    output$sigma3 <- renderUI({
        return(HTML(paste0("<p>", '$$\\sigma_3$$',"</p>")))
    })
    output$sigma4 <- renderUI({
        return(HTML(paste0("<p>", '$$\\sigma_4$$',"</p>")))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
