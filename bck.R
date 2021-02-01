rm(list=ls())
N <- readline(prompt="Tamanho da população: ")
N <- as.numeric(unlist(N))
n <- as.integer(readline(prompt="Tamanho da amostra: "))
n <- as.numeric(unlist(n))

# Simples
k = sample(N, n, replace = FALSE)
k <- sort(k); k

#
k <- round(N/n); k
ua1 <- sample(1:k,1); ua1
ua1 + (0:(n-1))*k