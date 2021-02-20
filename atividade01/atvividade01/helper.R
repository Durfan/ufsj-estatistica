# Deploy feito em https://pcecilio.shinyapps.io/atvividade01/

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