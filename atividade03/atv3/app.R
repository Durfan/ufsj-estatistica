# Deploy feito em https://pcecilio.shinyapps.io/atv3/
source('helper.R', local = TRUE)

ui <- fluidPage(
  
  fluidRow(
    img(src='logo_ufsj.png',
      align='left',
      style='margin:25px 10px 30px 15px;'),
    titlePanel('Estatística e Probabilidade',
      windowTitle = 'Estatística e Probabilidade'
    ),
    h4('Atividade III / Simulador de IC', style='margin-top:-10px;')
  ),
  
  fluidRow(
    column(3,
      numericInput('n', 'Tamanho da amostra',
        width='150px', value = 45),
      numericInput('media', 'Média da amostra',
        width='150px', value = 22),
      numericInput('dp', 'SD da amostra',
        width='150px', value = 2),
      selectInput('iter', 'Iterações',
        choices = list(
          '10' = 10, '50' = 50, '100' = 100,
          '250' = 250, '500' = 500, '1000' = 1000),
        width='150px', selected = 100),
      radioButtons('alpha', HTML('&alpha; alpha'),
        choices = list(
          'conf 0.90' = 0.90,
          'conf 0.95' = 0.95,
          'conf 0.99' = 0.99),
        selected = 0.95),
      actionButton("refresh" ,"Atualizar", icon("refresh"),
        class = "btn btn-primary"),
      tags$div(
        tags$small('Pablo Cecilio Oliveira 172050108'),
        tags$br(),
        tags$small(
          icon('github'),
          a('Durfan/ufsj-estatistica',
            href='https://github.com/Durfan/ufsj-estatistica')
        ), style='margin-top: 30px;'
      )
    ),
    column(9, align='center',
      tabsetPanel(type = 'tabs',
        tabPanel('Plot',
          plotOutput('ic', height = '580px')),
        tabPanel('Sobre', htmlOutput('sobre'))
      )
    )
  )
)

server <- function(input, output, session) {
  
  global <- reactiveValues(refresh = FALSE)
  
  observe({
    if (input$refresh)
      isolate(global$refresh <- TRUE)
  })
  
  results <- reactive({
    if (global$refresh)
      isolate(global$refresh <- FALSE)
    data.process(input$n,input$media,input$dp,input$iter,input$alpha)
  })

  output$ic <- renderPlot({
    results <- results()
    plot.graph(input$media,input$dp,input$iter,results)
  })
  
  output$sobre <- renderUI({
    p('Desenvolva um programa de simulação para verificar
      que o intervalo para a média normal com variância desconhecida
      baseado na distribuição t possui, de fato, 100(1−α)% de confiança.
      Esta verificação pode ser feita simulando muitas vezes diferentes
      amostras da distribuição normal, construindo o IC associado e
      verificando se o intervalo realmente contém o valor verdadeiro
      da média: se contiver, soma 1, se não contiver soma 0 e,
      a partir da soma final, calcula-se a porcentagem de inclusão.',
      style = "margin-top: 20px;text-align: left;")
  })
}

shinyApp(ui = ui, server = server)