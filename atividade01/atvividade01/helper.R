# Deploy feito em https://pcecilio.shinyapps.io/atvividade01/

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
          'B médias ='
        ),
        tags$td(b_mean)
      ),
      tags$tr(
        tags$td(
          style = 'text-align: right;padding-right: 5px;',
          'B medianas ='
        ),
        tags$td(b_mediam)
      ),
      tags$tr(
        tags$td(
          style = 'text-align: right;padding-right: 5px;',
          'B desvios-padrões ='
        ),
        tags$td(b_sd)
      )
    )
  )
}