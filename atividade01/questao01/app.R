UPictogram <-function(N, amostra, var=NULL) {
  # Ugly but Works
  estratificado <- !is.null(var)
  pic <- c('\U25A1','\U25A0','\U2606','\U2605')
  graph <- rep(pic[1], N)
  if (estratificado)
    graph[(N-var):N] <- pic[3]
  for(i in 1:length(amostra)) {
    idx = amostra[i]
    if (estratificado && idx > (N-var))
      graph[idx] <- pic[4]
    else
      graph[idx] <- pic[2]
  }
  graph
}

simples <- function(N, n) {
  amostra <- sample(N, n, replace = F)
  amostra <- sort(amostra)
  tags$div(
    h3('Amostragem Aleatória Simples'),
    p(toString(amostra)),
    p(HTML(UPictogram(N,amostra)))
  )
}

sistematica <- function(N, n) {
  k <- round(N/n)
  amostra <- sample(1:k,1)
  amostra <- amostra + (0:(n-1))*k
  amostra <- amostra[amostra <= N] # well, its a fix
  amostra <- sort(amostra)
  eq <- sprintf(
    '$$k = \\frac{N}{n} = \\frac{%d}{%d} \\approx %d$$',N,n,k)
  tags$div(
    h3('Amostragem Sistemática'),
    withMathJax(eq),
    hr(),
    p(toString(amostra)),
    p(HTML(UPictogram(N,amostra)))
  )
}

estratificada <- function(N, n, var) {
  var <- round((N/100) * var)
  Na <- N - var
  na <- round((n * Na) / N)
  nb <- round((n * var) / N)
  eq1 <-sprintf('$$N_{A} = %d, \\; N_{B} = %d$$',Na,var)
  eq2 <-sprintf('$$n_{A} = \\frac{nN_{A}}{N} = 
                \\frac{%d \\cdot %d}{%d} 
                \\approx %d$$',n,Na,N,na)
  eq3 <-sprintf('$$n_{B} = \\frac{nN_{B}}{N} = 
                \\frac{%d \\cdot %d}{%d} 
                \\approx %d$$',n,var,N,nb)
  amostra_A <- sort(sample(1:Na, na, replace = F))
  amostra_B <- sort(sample(Na:N, nb, replace = F))
  amostra <- c(amostra_A,amostra_B)
  tags$div(
    h3('Amostragem Estratificada'),
    withMathJax(eq1),
    withMathJax(eq2),
    withMathJax(eq3),
    hr(),
    h4('Estrato A'),
    p(toString(amostra_A)),
    h4('Estrato B'),
    p(toString(amostra_B)),
    p(HTML(UPictogram(N,amostra,var)))
  )
}


ui <- fluidPage(

  fluidRow(
    img(
      src='logo_estatistica.png',
      align='left',
      style='margin:10px 10px 20px 10px;'),
    
    titlePanel(
      'Questão 1',
      windowTitle = 'Atividade 1 - Questão 1'
      ),
    p('Elementos da população a serem incluídos na amostra.')
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput('N', h4('População'), value = '211'),
      numericInput('n', h4('Amostra'), value = '15'),
      radioButtons(
        'plano',
        h4('Plano de Amostragem'),
        choices = list(
          'Aleatória Simples' = 1, 
          'Sistemática' = 2, 
          'Estratificada' = 3),
        selected = 1
        ),
      conditionalPanel(
        condition = 'input.plano == 3',
        numericInput('var', h4('Variável Estratificadora'), value = '30'),
        helpText('Porcentagem sobre a populção para um estrato.')
      )
    ),

    mainPanel(
      htmlOutput('amostra')
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
    style='margin:10px 10px 20px 10px;'
  )
)

server <- function(input, output) {

  output$amostra <- renderUI({
    if (input$plano == 1)
      simples(input$N, input$n)
    else if (input$plano == 2)
      sistematica(input$N, input$n)
    else if (input$plano == 3)
      estratificada(input$N, input$n, input$var)
    else
      HTML('COMO?????')
  })

}

shinyApp(ui = ui, server = server)