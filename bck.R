rm(list=ls())
N <- readline(prompt="Tamanho da população: ")
N <- as.numeric(unlist(N))
n <- as.integer(readline(prompt="Tamanho da amostra: "))
n <- as.numeric(unlist(n))

?sample
k = sample(N, n, replace = FALSE)
k <- sort(k)
k


#This R code selects a systematic sample of size n from a  
# population of size N. 
# The values of N and n must be provided 
sys.sample = function(N,n){
  k = ceiling(N/n)
  #ceiling(x) rounds to the nearest integer that’s larger than x. 
  #This means ceiling (2.1) = 3 
  r = sample(1:k, 1)
  sys.samp = seq(r, r + k*(n-1), k)
  cat("The selected systematic sample is: \"", sys.samp, "\"\n")
  # Note: the last command "\"\n" prints the result in a new line
}
# To select a systematic sample, type the following command
# providing the values of N and n
sys.sample(N,n)