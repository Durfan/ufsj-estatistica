library(shiny)

ui <- fluidPage(
  titlePanel("Estatística - Atividade 1"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("N", h4("População"), value = "1"),
      numericInput("n", h4("Amostra"), value = "1"),
      radioButtons("plano", h4("Plano de Amostragem"), 
                         choices = list("Aleatória Simples" = 1, 
                                        "Sistemática" = 2, 
                                        "Estratificada" = 3),
                         selected = 1)
    ),
    mainPanel(
      textOutput("amostra_plano")
    )
  )
)

server <- function(input, output) {
  output$amostra_plano <- renderText({
    paste(sample(input$N, input$n, replace = FALSE))
  })

}

shinyApp(ui = ui, server = server)