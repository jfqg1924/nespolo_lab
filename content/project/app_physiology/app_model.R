# Shiny app Physiological model

# This app is to run the model changing the parameters we define 


# Library to use
library(shiny)

# Defines the user interface
ui <- fluidPage(
  titlePanel("Physiological model predicting Nespolo et al (in prep)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("L0", "Value of L0:", value = 3.48),
      numericInput("L", "Value of L:", value = -0.37),
      numericInput("T0", "Value of T0:", value = 0.6),
      numericInput("T1", "Value of T1:", value = -0.07),
      numericInput("E0", "Value of E0:", value = 2.41),
      numericInput("E", "Value of E:", value = -0.08),
      numericInput("n", "Value of n:", value = 3),
      numericInput("i", "Value of i:", value = 0.25),
      numericInput("M0", "Value of M0:", value = 10),
      numericInput("R", "Value of R:", value = 0.34),
      numericInput("C0", "Value of C0:", value = -5),
      numericInput("C1", "Value of C1:", value = 5),
      numericInput("C2", "Value of C2:", value = 25),
      numericInput("fat1", "Value of fat1:", value = 20),
      numericInput("clim", "Value of clim:", value = 0),
      actionButton("calcular", "Calcular")
    ),
    mainPanel(
      verbatimTextOutput("resultado"),
      plotOutput("grafico1"),  # Primer gráfico
      verbatimTextOutput("resultado_surv"),  # Resultado calculado por 'surv' en texto
      plotOutput("grafico2")  # Segundo gráfico

    )
  )
)

# Defines the DEEh function
DEEh <- function(x, L0, L, T0, T1, E0, E, n, i, M0, R, C0, C1, C2, fat1, clim) {
  p_torp = 1 / (1 + exp(-(L0 + L * x)))
  TMR = M0 * (T0 + T1 * x)
  MR = M0 * (E0 + E * x + R * n)
  DEEh1 = p_torp * TMR + (1 - p_torp) * MR
  
  p_torp2 = 1 / (1 + exp(-(L0 + L * x)))
  TMR2 = M0 * i
  MR2 = M0 * (E0 + E * x + R * n)
  DEEh2 = p_torp2 * TMR2 + (1 - p_torp2) * MR2
  
  out <- ifelse(x <= 5, DEEh1, DEEh2)
  return(out)
}


# Define the survival function (surv) with the same parameters
surv <- function(x, L0, L, T0, T1, E0, E, n, i, M0, R, C0, C1, C2, fat1, clim) {
  DEEh_result <- DEEh(x, L0, L, T0, T1, E0, E, n, i, M0, R, C0, C1, C2, fat1, clim)
  surv_result <- 39.7 * fat1 / DEEh_result
  
  return(surv_result)
}


# Defines the server
server <- function(input, output) {
  observeEvent(input$calcular, {
    # Realiza los cálculos basados en los valores ingresados
    resultado <- input$L0 + input$L + input$T0 + input$T1 + input$E0 + input$E +
      input$n + input$i + input$M0 + input$R + input$C0 + input$C1 + input$C2 +
      input$fat1 + input$clim
    
    # Muestra el resultado en la aplicación
    output$resultado <- renderText({
      paste("The result of energy consumption is:", resultado ,"(kJ/d)")
    })
    
    # Crea el gráfico basado en los valores ingresados
    x <- seq(-10, 20, by = 0.1)
    resultado_grafico1 <- DEEh(
      x, input$L0, input$L, input$T0, input$T1, input$E0, input$E,
      input$n, input$i, input$M0, input$R, input$C0, input$C1, input$C2,
      input$fat1, input$clim
    )
    
    output$grafico1 <- renderPlot({
      plot(x, resultado_grafico1, type = "l", 
           xlab = "Ambient temperature (ºC)", ylab = "Energy Consumption (DEE) (kJ/d)", 
           ylim = c(0, 50))
    })
    
    
    # Crea el segundo gráfico utilizando la función de supervivencia
    x2 <- seq(-10, 20, by = 0.1)
    resultado_grafico2 <- surv(
      x2, input$L0, input$L, input$T0, input$T1, input$E0, input$E,
      input$n, input$i, input$M0, input$R, input$C0, input$C1, input$C2,
      input$fat1, input$clim
    )
    
    output$grafico2 <- renderPlot({
      plot(x2, resultado_grafico2, type = "l", xlab = "Ambient temperature (ºC)", 
           ylab = "Survival (days)", xlim = c(-10, 20), ylim = c(0, 300))
    })
    # Muestra el resultado de 'surv' en texto en la aplicación antes de la segunda gráfica
    output$resultado_surv <- renderText({
      paste("Mean Supervivence:", round(mean(resultado_grafico2), 0), "(days)")
    })
  })
}

# Create Shiny app
shinyApp(ui, server)





