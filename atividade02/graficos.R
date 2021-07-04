library(ggplot2)
library(reshape2)
library(VGAM)
library(evd)

rm(list = ls())

# Distribuição geométrica
x <- 0:10
df_g <- data.frame(
  x = x,
  p2 = dgeom(x, prob = .2),
  p5 = dgeom(x, prob = .5),
  p8 = dgeom(x, prob = .8))
df_g <- melt(df_g, id.vars='x')
ggplot(df_g, aes(x=factor(x), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  theme(legend.position = c(0.9, 0.8)) +
  labs(x = "x", y = "P(X = x)") +
  scale_fill_manual(
    values=c("#00AFBB", "#E7B800", "#FC4E07"),
    labels = c("p = 0,2", "p = 0,5", "p = 0,8"),
    name="Probabilidade")

# Distribuição Hipergeométrica
x <- 0:60
df_hg <- data.frame(
  x = x,
  k70 = dhyper(x, m = 300, n = 200, k = 70),
  k60 = dhyper(x, m = 200, n = 300, k = 60),
  k50 = dhyper(x, m = 100, n = 400, k = 50))
df_hg <- melt(df_hg, id.vars='x')
ggplot(df_hg, aes(x=as.numeric(x), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  theme(legend.position = c(0.845, 0.86), legend.title = element_blank()) +
  labs(x = "k", y = "PMF") +
  scale_x_continuous(limits = c(0,60), breaks=seq(0,60,10)) +
  scale_fill_manual(
    values=c("#00AFBB", "#E7B800", "#FC4E07"),
    labels = c("n = 200, k = 70, M = 300", 
               "n = 300, k = 60, M = 200", 
               "n = 400, k = 50, M = 100"))

# Distribuição multinomial
X <- t(as.matrix(expand.grid(0:3, 0:3))); X <- X[, colSums(X) <= 3]
X <- rbind(X, 3:3 - colSums(X)); dimnames(X) <- list(letters[1:3], NULL)
multi <- round(apply(X, 2, function(x) dmultinom(x, prob = c(1,2,5))), 3)
df_multi <- as.data.frame(multi)
ggplot(df_multi,aes(x = as.numeric(row.names(df_multi)), y = multi)) +
  geom_bar(stat = 'identity', fill = "#00AFBB") +
  scale_x_continuous(breaks=seq(0,10,1)) +
  annotate("text", x = 9, y = .28,
           label = "paste(\" a 0 1 2 3 0 1 2 0 1 0 \")", parse = T) +
  annotate("text", x = 9, y = .26,
           label = "paste(\" b 0 0 0 0 1 1 1 2 2 3 \")", parse = T) +
  annotate("text", x = 9, y = .24,
           label = "paste(\" c 3 2 1 0 2 1 0 1 0 0 \")", parse = T) +
  labs(x = NULL, y = "dmultinom()")

# Distribuição Binomial Negativa
x <- 0:100
df_bn <- data.frame(
  x = x,
  k10 = dnbinom(x, size = 10, prob = .5),
  k40 = dnbinom(x, size = 40, prob = .5),
  k70 = dnbinom(x, size = 70, prob = .5))
df_bn <- melt(df_bn, id.vars='x')
ggplot(df_bn, aes(x=as.numeric(x), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  theme(legend.position = c(0.92, 0.83)) +
  labs(x = "k", y = "Probabilidade") +
  scale_fill_manual(
    values=c("#00AFBB", "#E7B800", "#FC4E07"),
    labels = c("k = 10", "k = 40", "k = 70"),
    name="p = 0,5")

# Pareto Discreta (ou Zeta)
x <- 1:5
df_zt <- data.frame(
  x = x,
  s2 = dzeta(x, shape = 2),
  s3 = dzeta(x, shape = 3),
  s4 = dzeta(x, shape = 4),
  s5 = dzeta(x, shape = 5))
df_zt <- melt(df_zt, id.vars='x')
ggplot(df_zt, aes(x=as.numeric(x), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  theme(legend.position = c(0.93, 0.84), legend.title = element_blank()) +
  labs(x = NULL, y = "Probabilidade") +
  scale_fill_discrete(labels = c(
    "s = 2",
    "s = 3",
    "s = 4",
    "s = 5"))

# Distribuição Exponencial
x <- seq(0, 5, by = .1)
df_exp <- data.frame(
  x = x,
  c1 = dexp(x, rate = .5),
  c2 = dexp(x, rate = 1),
  c3 = dexp(x, rate = 1.5))
df_exp <- melt(df_exp, id.vars='x')
ggplot(df_exp, aes(x=as.numeric(x), y=value, fill=variable)) +
  geom_line(aes(color = variable, linetype = variable)) +
  theme(legend.position = c(0.9, 0.75), legend.title = element_blank()) +
  labs(x = NULL, y = "Função Densidade de Probabilidade") +
  scale_color_discrete(labels = c(
    expression(paste(lambda," = 0.5")),
    expression(paste(lambda," =  1  ")),
    expression(paste(lambda," = 1.5")))) +
  scale_linetype_discrete(labels = c(
    expression(paste(lambda," = 0.5")),
    expression(paste(lambda," =  1  ")),
    expression(paste(lambda," = 1.5"))))

# Distribuição de Cauchy
x <- seq(-5, 5, by = .1)
df_ct <- data.frame(
  x = x,
  c1 = dcauchy(x, location = 0, scale = .5),
  c2 = dcauchy(x, location = 0, scale = 1),
  c3 = dcauchy(x, location = 0, scale = 2),
  c4 = dcauchy(x, location = -2, scale = 2))
df_ct <- melt(df_ct, id.vars='x')
ggplot(df_ct, aes(x=as.numeric(x), y=value, fill=variable)) +
  geom_line(aes(color = variable, linetype = variable)) +
  theme(legend.position = c(0.9, 0.84), legend.title = element_blank()) +
  labs(x = NULL, y = "Função Densidade de Probabilidade") +
  scale_color_discrete(labels = c(
    expression(paste(alpha," = 0  ",beta," = 0.5")),
    expression(paste(alpha," = 0  ",beta," = 1   ")),
    expression(paste(alpha," = 0  ",beta," = 2   ")),
    expression(paste(alpha," = 0  ",beta," = -2 ")))) +
  scale_linetype_discrete(labels = c(
    expression(paste(alpha," = 0  ",beta," = 0.5")),
    expression(paste(alpha," = 0  ",beta," = 1   ")),
    expression(paste(alpha," = 0  ",beta," = 2   ")),
    expression(paste(alpha," = 0  ",beta," = -2 "))))

#distribuição Weibull
x <- seq(0, 6, by = .01)
df_wb <- data.frame(
  x = x,
  c1 = dweibull(x, 2, .5),
  c2 = dweibull(x, 2, 1.5),
  c3 = dweibull(x, 2, 3))
df_wb <- melt(df_wb, id.vars='x')
ggplot(df_wb, aes(x=as.numeric(x), y=value, fill=variable)) +
  geom_line(aes(color = variable, linetype = variable)) +
  theme(legend.position = c(0.89, 0.86), legend.title = element_blank()) +
  labs(x = NULL, y = "Função Densidade de Probabilidade") +
  scale_color_discrete(labels = c(
    expression(paste(alpha," = 2  ",beta," = 0.5")),
    expression(paste(alpha," = 2  ",beta," = 1.5")),
    expression(paste(alpha," = 2  ",beta," = 3   ")))) +
  scale_linetype_discrete(labels = c(
    expression(paste(alpha," = 2  ",beta," = 0.5")),
    expression(paste(alpha," = 2  ",beta," = 1.5")),
    expression(paste(alpha," = 2  ",beta," = 3   "))))

# Distribuição Gumbel
x <- seq(-4, 24, by = .1)
df_gb <- data.frame(
  x = x,
  c1 = dgumbel(x, .5, 2),
  c2 = dgumbel(x, 1.5, 3),
  c3 = dgumbel(x, 3, 4))
df_gb <- melt(df_gb, id.vars='x')
ggplot(df_gb, aes(x=as.numeric(x), y=value, fill=variable)) +
  geom_line(aes(color = variable, linetype = variable)) +
  theme(legend.position = c(0.89, 0.86), legend.title = element_blank()) +
  labs(x = NULL, y = "Função Densidade de Probabilidade") +
  scale_color_discrete(labels = c(
    expression(paste(mu," = 0.5  ",sigma," = 2")),
    expression(paste(mu," = 1.5  ",sigma," = 3")),
    expression(paste(mu," = 3.0  ",sigma," = 4")))) +
  scale_linetype_discrete(labels = c(
    expression(paste(mu," = 0.5  ",sigma," = 2")),
    expression(paste(mu," = 1.5  ",sigma," = 3")),
    expression(paste(mu," = 3.0  ",sigma," = 4"))))

# Distribuição de Rayleigh
x <- seq(0, 15, by = .1)
df_ry <- data.frame(
  x = x,
  c1 = drayleigh(x, 1),
  c2 = drayleigh(x, 2),
  c3 = drayleigh(x, 3),
  c4 = drayleigh(x, 4),
  c5 = drayleigh(x, 5))
df_ry <- melt(df_ry, id.vars='x')
ggplot(df_ry, aes(x=as.numeric(x), y=value, fill=variable)) +
  geom_line(aes(color = variable, linetype = variable)) +
  theme(legend.position = c(0.91, 0.81), legend.title = element_blank()) +
  labs(x = NULL, y = "Função Densidade de Probabilidade") +
  scale_color_discrete(labels = c(
    "Rayleigh 1",
    "Rayleigh 2",
    "Rayleigh 3",
    "Rayleigh 4",
    "Rayleigh 5")) +
  scale_linetype_discrete(labels = c(
    "Rayleigh 1",
    "Rayleigh 2",
    "Rayleigh 3",
    "Rayleigh 4",
    "Rayleigh 5"))

# Distribuição de Rice
x <- seq(0, 15, by = .1)
df_rc <- data.frame(
  x = x,
  r1 = drice(x, 0.5, 1),
  r2 = drice(x, 1, 1),
  r3 = drice(x, 5, 3),
  r4 = drice(x, 10, 5))
df_rc <- melt(df_rc, id.vars='x')
ggplot(df_rc, aes(x=as.numeric(x), y=value, fill=variable)) +
  geom_line(aes(color = variable, linetype = variable)) +
  theme(legend.position = c(0.9, 0.84), legend.title = element_blank()) +
  labs(x = NULL, y = "Função Densidade de Probabilidade") +
  scale_color_discrete(labels = c(
    "Rice (0.5 1)",
    "Rice (1 1)",
    "Rice (5 3)",
    "Rice (10 5)")) +
  scale_linetype_discrete(labels = c(
    "Rice (0.5 1)",
    "Rice (1 1)",
    "Rice (5 3)",
    "Rice (10 5)"))


