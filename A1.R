rm(list=ls())
N <- readline(prompt="Tamanho da população: ")
N <- as.numeric(unlist(N))
n <- as.integer(readline(prompt="Tamanho da amostra: "))
n <- as.numeric(unlist(n))

?sample
k = sample(N, n, replace = FALSE)
k