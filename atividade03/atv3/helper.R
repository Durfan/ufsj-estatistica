intervalo.conf <- function(amostra, n, alpha) {
  media <- mean(amostra)
  variancia <- var(amostra)
  quantis <- qt(c((1-alpha)/2, 1 - (1-alpha)/2), df = n-1)
  ic <- media + quantis * sqrt(variancia/n)
  return(ic)
}

data.process <- function(n, m, dp, iter, alpha) {
  soma <- 0
  alpha <- as.double(alpha)
  graph.data <- list()
  for (i in 1:iter) {
    amostra <- rnorm(n, mean = m, sd = dp)
    #teste <- t.test(amostra, conf.level=alpha)
    #intervalo <- teste$conf.int
    intervalo <- intervalo.conf(amostra, n, alpha)
    graph.data[[i]] <- intervalo
    if (m > intervalo[1] & m < intervalo[2])
      soma <- soma +1
  }
  processed <- list(soma = soma, graphdata = graph.data)
  return(processed)
}

plot.graph <- function(me, dp, iter, data) {
  interval <- data$graphdata
  iter <- as.integer(iter)
  inclusao <- (data$soma/iter) * 100 
  title <- paste('Soma: ', data$soma, '(', inclusao, '%)')
  plot(NULL, main = title,
    xlim = c(me-dp,me+dp), ylim = c(0,iter),
    xlab = (expression(mu)), ylab = 'Iterações')
  abline(v = me)

  for (i in 1:iter) {
    point <- interval[[i]]
    if (me > point[1] & me < point[2]) {
      if (iter <= 100)
        lines(c(point[1], point[2]), c(i,i))
      points(mean(point), i, pch = 20)
    }
    else {
      if (iter <= 100)
        lines(c(point[1], point[2]), c(i,i), col='red' )
      points(mean(point), i, pch = 20, col='red')
    }
  }
}
