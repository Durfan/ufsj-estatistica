# Deploy feito em https://pcecilio.shinyapps.io/atv1a/

UPictogram <- function(N, amostra) {
  # Ugly but Works
  pic <- c('\U25A1','\U25A0')
  graph <- rep(pic[1], N)
  for(i in 1:length(amostra)) {
    idx = amostra[i]
    graph[idx] <- pic[2]
  }
  graph
}

UEPictogram <- function(N, amostras, pos_idx) {
  # Ugly but Works
  pic <- c('\U25A1','\U25A0','\U2606','\U2605')
  graph <- rep(pic[1], N)
  for(i in 1:length(pos_idx)) {
    first <- pos_idx[[i]][1]
    last <- pos_idx[[i]][2]
    if (i%%2 == 0)
      graph[first:last] <- pic[3]
  }
  amostra <- unlist(amostras)
  for(i in 1:length(amostra)) {
    idx = amostra[i]
    if (graph[idx] == pic[1])
      graph[idx] <- pic[2]
    else
      graph[idx] <- pic[4]
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
  amostra <- amostra[amostra <= N] # bom, its a fix
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

estratificada <- function(N, n, e) {
  Nx <- as.numeric(unlist(strsplit(e,",")))
  size <- length(Nx)
  if (sum(Nx) != N) {
    error <- tags$div(
      p('A soma dos estratos não é igual a população.',
        style="color:red"))
    return(error)
  }
  
  nx_estrat <- c()
  for(i in 1:size)
    nx_estrat <- c(nx_estrat, round((n * Nx[i]) / N))
  
  amostras <- list()
  start <- 0
  pos_idx <- list()
  for(i in 1:size) {
    end <- start + Nx[i]
    pos_idx[[i]] <- c((start+1),end)
    sample <- sample((start+1):end, nx_estrat[i], replace = F)
    sample <- sort(sample)
    amostras[[i]] <- sample
    start <- end
  }
  #pos_idx <<- pos_idx

  eq <- sprintf('$$n_{i} = \\frac{nN_{i}}{N}$$')

  tags$div(
    h3('Amostragem Estratificada'),
    withMathJax(eq),
    p(HTML(
      paste0('N = {',toString(Nx),'}','<br>'),
      paste0('n = {',toString(nx_estrat),'}','<br>'))),
    hr(),
    HTML(
      paste('<p>',sapply(amostras, toString),'</p>')),
    p(HTML(UEPictogram(N,amostras,pos_idx)))
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
      numericInput('N', 'População', value = '211'),
      sliderInput('n', "Amostra",
        min = 0, max = 211, value = 15, ticks = F),
      radioButtons(
        'plano',
        'Plano de Amostragem',
        choices = list(
          'Aleatória Simples' = 1, 
          'Sistemática' = 2, 
          'Estratificada' = 3),
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
    style='margin:10px 10px 20px 10px;'
  )
)

server <- function(input, output, session) {
  
  observe({
    updateSliderInput(session, 'n', max = input$N)
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

}

shinyApp(ui = ui, server = server)