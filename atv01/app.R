simples <- function(N, n) {
  header <- "<h3>Amostragem Aleatória Simples</h3>"
  amostra <- sample(N, n, replace = F)
  amostra <- sort(amostra)
  HTML(header, toString(amostra))
}

sistematica <- function(N, n) {
  header <- "<h3>Amostragem Sistemática</h3>"
  k <- round(N/n)
  amostra <- sample(1:k,1)
  amostra <- amostra + (0:(n-1))*k
  amostra <- sort(amostra)
  HTML(header, toString(amostra))
}

estratificada <- function(N, n, var) {
  Na <- N - var
  na <- round((n * Na) / N)
  nb <- round((n * var) / N)
  amostra_A <- sort(sample(1:Na, na, replace = F))
  amostra_B <- sort(sample(Na:N, nb, replace = F))
  header <- "<h3>Amostragem Estratificada</h3>"
  str1 <- paste("<p>","<b>Amostra A: </b>",toString(amostra_A),"</p>")
  str2 <- paste("<p>","<b>Amostra B: </b>",toString(amostra_B),"</p>")
  HTML(header, str1, str2)
}


ui <- fluidPage(
  
  titlePanel("Estatística - Atividade 1"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("N", h4("População"), value = "211"),
      numericInput("n", h4("Amostra"), value = "15"),
      radioButtons(
        "plano",
        h4("Plano de Amostragem"),
        choices = list(
          "Aleatória Simples" = 1, 
          "Sistemática" = 2, 
          "Estratificada" = 3),
        selected = 1
        ),
      conditionalPanel(
        condition = "input.plano == 3",
        numericInput("var", h4("Variável"), value = "30"),
      )
    ),

    mainPanel(
      htmlOutput("amostra")
    )
  )
)

server <- function(input, output) {

  output$amostra <- renderUI({
    if (input$plano == 1)
      simples(input$N, input$n)
    else if (input$plano == 2)
      sistematica(input$N, input$n)
    else
      estratificada(input$N, input$n, input$var)
  })

}

shinyApp(ui = ui, server = server)