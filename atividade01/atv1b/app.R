# Deploy feito em https://pcecilio.shinyapps.io/atv1b/

reamostragem <- function(vec, B) {
  vec <- as.numeric(unlist(strsplit(vec,",")))
  cols <- length(vec)+3
  mat_vec <- matrix(nrow = B, ncol = cols)
  for(i in 1:B) {
    new_vec <- sample(vec, replace = T)
    ins_vec <- c(new_vec, mean(new_vec))
    ins_vec <- c(ins_vec, median(new_vec))
    ins_vec <- c(ins_vec, sd(new_vec))
    mat_vec[i,] <- ins_vec
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
        tags$td(
          style = 'text-align: right;padding-right: 5px;',
          'B médias =',
          icon('square', style = 'color:blue;')
        ),
        tags$td(b_mean)
      ),
      tags$tr(
        tags$td(
          style = 'text-align: right;padding-right: 5px;',
          'B medianas =',
          icon('square', style = 'color:red;')
        ),
        tags$td(b_mediam)
      ),
      tags$tr(
        tags$td(
          style = 'text-align: right;padding-right: 5px;',
          'B desvios-padrões =',
          icon('square', style = 'color:green;')
        ),
        tags$td(b_sd)
      )
    )
  )
}

matbox <- function(mat) {
  b_mean <- mean(mat[,ncol(mat) - 2])
  b_mediam <- median(mat[,ncol(mat) - 1])
  b_sd <- sd(mat[,ncol(mat)])
  data <- mat[,(ncol(mat)-2):ncol(mat)]
  boxplot.matrix(data, use.cols = F)
  abline(h=b_mean, col='blue')
  abline(h=b_mediam, col='red')
  abline(h=b_sd, col='green')
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
      textInput('vec', 'Vetor', "16,145.4,23.2,42,10"),
      helpText('(separado por vírgulas)'),
      numericInput('B', 'Reamostragem', value = '6')
    ),
    
    mainPanel(
      uiOutput('reamostragem'),
      htmlOutput('result'),
      plotOutput('plothis')
    )
  ),
  fluidRow(
    tags$small('Pablo Cecilio Oliveira 172050108'),
    tags$br(),
    tags$small(
      icon('code-branch'),
      a(
        'github.com/Durfan/ufsj-estatistica',
        href='https://github.com/Durfan/ufsj-estatistica'
        )
    ),
    style='margin:10px 10px 20px 10px'
  )
)

server <- function(input, output) {
  
  dataInput <- reactive({
    reamostragem(input$vec, input$B)
  })
  
  output$reamostragem <- renderTable({
    dataset <- dataInput()
    formatC(dataset)
    }, hover = T, spacing = 'xs',
    rownames = T, colnames = T
  )
  
  output$result <- renderUI({
    b_results(dataInput())
  })
  
  output$plothis <- renderPlot({
    matbox(dataInput())
  })
  
}

shinyApp(ui = ui, server = server)