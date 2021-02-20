# Deploy feito em https://pcecilio.shinyapps.io/atvividade01/

source('helper.R', local = TRUE)

ui <- navbarPage(
  title = div(
    icon('chart-line'),
    'Atividade 1'
  ),
  tabPanel('Questão 1',

    fluidRow(
      p('Elementos da população a serem incluídos na amostra.'),
      style = 'padding-left: 20px;padding-bottom: 10px;'
    ),

    sidebarLayout(
      sidebarPanel(
        numericInput('N', 'População',value = 211),
        sliderInput('n', "Amostra",
          min = 0, max = 211, value = 15, ticks = F),
        radioButtons(
          'plano',
          'Plano de Amostragem',
          choices = list(
            'Aleatória Simples' = 1, 
            'Sistemática' = 2, 
            'Estratificada' = 3
          ),
         selected = 1
        ),
        conditionalPanel(
          condition = 'input.plano == 3',
          textInput('e', 'Estratos', '148,63'),
          helpText('Elementos da população para cada estrato.
                   (Separados por vírgulas)')
        )
      ),
     
      mainPanel(
        htmlOutput('amostra')
      )
    )
  ),

  tabPanel('Questão 2',
           
    fluidRow(
      p('Média das B médias, das B medianas
        e dos B desvios-padrões.'),
      style = 'padding-left: 20px;padding-bottom: 10px;'
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
    )
  ),

  windowTitle = 'UFSJ Estatística - Atividade 1',
  footer = tags$footer(
    tags$small('Pablo Cecilio Oliveira 172050108'),
    tags$br(),
    tags$small(
      icon('code-branch'),
      a('github.com/Durfan/ufsj-estatistica',
        href='https://github.com/Durfan/ufsj-estatistica')
    ),
    style='padding-left: 20px;padding-bottom: 10px;'
  )
)

server <- function(input, output, session) {
  
  observe({
    updateSliderInput(session, 'n', max = input$N)
  })
  
  dataInput <- reactive({
    reamostragem(input$vec, input$B)
  })
  
  output$amostra <- renderUI({
    if (input$plano == 1)
      simples(input$N, input$n)
    else if (input$plano == 2)
      sistematica(input$N, input$n)
    else if (input$plano == 3)
      estratificada(input$N, input$n, input$e)
    else
      HTML('COMO?????')
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