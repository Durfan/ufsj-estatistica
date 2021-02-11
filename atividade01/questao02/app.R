reamostragem <- function(vec, B) {
  vec <- as.numeric(unlist(strsplit(vec,",")))
  columns <- length(vec)+3
  mat_vec <- matrix(nrow = B, ncol = columns)
  for(i in 1:B) {
    new_vec <- sample(vec, replace = T)
    new_vec <- c(new_vec, mean(new_vec))
    new_vec <- c(new_vec, median(new_vec))
    new_vec <- c(new_vec, sd(new_vec))
    mat_vec[i,] <- new_vec
  }
  row_foo <- c(
    rep(' ',length(vec)),
    'mean','median','sd'
    )
  colnames(mat_vec) <- row_foo
  mat_vec
}

b_results <- function(mat) {
  b_mean <- mean(mat[,ncol(mat) - 2])
  b_mediam <- median(mat[,ncol(mat) - 1])
  b_sd <- sd(mat[,ncol(mat)])
  tags$div(
    tags$table(
      tags$tr(
        tags$td(style = 'text-align: right;padding-right: 5px;',
                'B médias ='),
        tags$td(b_mean)
      ),
      tags$tr(
        tags$td(style = 'text-align: right;padding-right: 5px;',
                'B medianas ='),
        tags$td(b_mediam)
      ),
      tags$tr(
        tags$td(style = 'text-align: right;padding-right: 5px;',
                'B desvios-padrões ='),
        tags$td(b_sd)
      )
    )
  )
}

ui <- fluidPage(
  
  fluidRow(
    img(
      src='logo_estatistica.png',
      align='left',
      style='margin:10px 10px 20px 10px;'),
    
    titlePanel(
      'Questão 2',
      windowTitle = 'Atividade 1 - Questão 2'
    ),
    p('Média das B médias, das B medianas e dos B desvios-padrões.')
  ),
  
  sidebarLayout(
    sidebarPanel(
      textInput('vec', h4('Vetor'), "16,145,23,42,10"),
      helpText('(separado por vírgulas)'),
      numericInput('B', h4('Reamostragem'), value = '6'),
    ),
    
    mainPanel(
      uiOutput('reamostragem'),
      htmlOutput('result')
    )
  ),
  fluidRow(
    tags$small('Pablo Cecilio Oliveira 172050108'),
    tags$br(),
    tags$small(
      icon('code-branch'),
      a('github.com/Durfan/ufsj-estatistica',
        href='https://github.com/Durfan/ufsj-estatistica')
    ),
    style='margin:10px 10px 20px 10px;position: fixed;bottom: 0;'
  )
)

server <- function(input, output) {
  
  dataInput <- reactive({reamostragem(input$vec, input$B)})
  
  output$reamostragem <- renderTable({dataInput()},
    hover = T, spacing = 'xs',
    rownames = T, colnames = T
  )
  
  output$result <- renderUI({b_results(dataInput())})
  
}

shinyApp(ui = ui, server = server)