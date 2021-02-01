ui <- fluidPage(
  
  titlePanel("Estatística - Atividade 1"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("N", h4("População"), value = "211"),
      numericInput("n", h4("Amostra"), value = "15"),
      radioButtons("plano", h4("Plano de Amostragem"), 
                         choices = list("Aleatória Simples" = 1, 
                                        "Sistemática" = 2, 
                                        "Estratificada" = 3),
                         selected = 1),
      conditionalPanel(
        condition = "input.plano == 3",
        numericInput("Nb", h4("Variável"), value = "30"),
      )
    ),

    mainPanel(
      htmlOutput("header"),
      textOutput("amostra")
    )
  )
)

server <- function(input, output) {

  output$header <- renderText({
    if (input$plano == 1)
      paste("<h3>Amostragem Aleatória Simples</h3>")
    else if (input$plano == 2)
      paste("<h3>Amostragem Sistemática</h3>")
    else
      paste("<h3>Amostragem Estratificada</h3>")
  })

  output$amostra <- renderText({
    if (input$plano == 1) {
      amostra <- sample(input$N, input$n, replace = F)
      amostra <- sort(amostra)
      paste(amostra)
    }
    else if (input$plano == 2) {
      k <- round(input$N/input$n)
      amostra <- sample(1:k,1)
      amostra <- amostra + (0:(input$n-1))*k
      amostra <- sort(amostra)
      paste(amostra)
    }
    else {
      Na <- input$N - input$Nb
      na <- round((input$n * Na) / input$N)
      nb <- round((input$n * input$Nb) / input$N)
      amostra_A <- sample(1:Na, na, replace = F)
      amostra_B <- sample(Na:input$N, nb, replace = F)
      amostra_A <- sort(amostra_A)
      amostra_B <- sort(amostra_B)
      paste(list(amostra_A), list(amostra_B), sep="\n")
    }
  })

}

shinyApp(ui = ui, server = server)