# Deploy feito em https://pcecilio.shinyapps.io/atv2b/
if (!require('fdth')) install.packages('fdth')
library(fdth)
source('helper.R', local = TRUE)

ui <- fluidPage(
  
  fluidRow(
    img(src='logo_ufsj.png',
      align='left',
      style='margin:25px 10px 30px 15px;'),
    titlePanel('Estatística e Probabilidade',
      windowTitle = 'Estatística e Probabilidade'
    ),
    h4('Atividade II / Análise Descritiva de dados',
       style='margin-top:-10px;')
  ),
  
  fluidRow(
    column(3,
      textAreaInput('data', 'Vetor de Dados',
        width='200px', rows='5', value = example),
      actionButton('reset', 'Limpar', style = 'margin-bottom: 30px;'),
      helpText('○ Para números decimais utilize a vírgula.'),
      helpText('○ Os valores no vetor são separados por ponto e vírgula.'),
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
        tabPanel('Analise',
          fluidRow(
            htmlOutput('dataInfo'),
            htmlOutput('tableInfo'),
            tableOutput('dataTable'),
          style = 'text-align: left; margin-left:20px; margin-bottom: 0px;'),
          fluidRow(
            plotOutput('plotThis', width = 'auto')
          )
        ),
        tabPanel('Sobre',
          tags$p('Implemente a seguinte descrição narrativa:',
            style = "margin-top: 20px;text-align: left;"),
          tags$ul(
            tags$li('Usuário informa um vetor de dados;'),
            tags$li('O programa identifica o tipo de variável e exibe ao usuário;'),
            tags$li('O programa realiza uma análise descritiva dos dados, 
            exibindo ao usuário tabelas e gráficos adequados
            para o tipo de variável;'),
            tags$li('O programa retorna as principais medidas-resumo.'),
            style = 'text-align: left;')
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateTextInput(session, 'data', value = '')
  })
  
  dataType <- reactive({
    suppressWarnings(getDataType(input$data))
  })
  
  data <- reactive({
    if (is.null(dataType()))
      return(NULL)
    
    if (dataType() == 'Continuas' | dataType() == 'Discretas'){
      data <- gsub(",", ".", input$data)
      data <- as.numeric(unlist(strsplit(data,";")))
      data
    } else {
      data <- unlist(strsplit(input$data,";"))
      data
    }

  })
  
  output$dataInfo <- renderUI({
    if (is.null(dataType()))
      return(tags$h4('Aguardando dados...'))

    data <- data()
    tags$div(
      tags$h3('Vetor de Variaveis', dataType()),
      tags$p(
        if (dataType() == 'Qualitativas'){
          data <- table(data)
          tags$div(
            tags$span('M', tags$sub('e'), ' = ', mean(data)),
            tags$span('M', tags$sub('d'), ' = ', median(data),
              style="margin-left: 15px;"),
            tags$span('s = ', sd(data), style="margin-left: 15px;")
        )}
        else {
          tags$div(
            tags$span('M', tags$sub('e'), ' = ', mean(data)),
            tags$span('M', tags$sub('d'), ' = ', median(data),
              style="margin-left: 15px;"),
            tags$span('s = ', sd(data), style="margin-left: 15px;")
        )}
      )
    )
  })
  
  output$tableInfo <- renderUI({
    if (is.null(dataType()))
      return()

    if (dataType() == 'Continuas'){
      title <- tags$h4('Tabela de Distribuição de Frequencia')
    } else
      title <- tags$h4('Tabela de Frequencia')
    tags$div(title,style = 'margin-top: 20px;')
  })
  
  output$dataTable <- renderTable({
    if (is.null(dataType()))
      return()

    data <- data()
    if (dataType() == 'Continuas'){
      data <- fdt(data())
      table <- summary(data)
    } else
      table <- table(data)
    table
  })

  output$plotThis <- renderPlot({
    if (is.null(dataType()))
      return()
    
    if (dataType() == 'Continuas'){
      data <- fdt(data())
      plot(data, main = 'Histograma Plot')
    } else if (dataType() == 'Discretas'){
      data <- table(data())
      barplot(data, main = 'Bar Plot')
    } else if (dataType() == 'Qualitativas'){
      data <- table(data())
      pie(data, main = 'Setores Plot')
    }
  })
  
}

shinyApp(ui = ui, server = server)