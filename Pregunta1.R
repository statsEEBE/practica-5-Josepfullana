#Ej 1

  #a
mu <- 96.3
sigma <- 5.7

curve(dnorm(x, mean = mu, sd = sigma), xlim=c(80,120))

rnorm(1,mu, sigma)
pnorm(90, mu, sigma)
  #b
#set.seed(123)
#muestra aleatoria de tamaño 4
Y <- function(i)(sum(rnorm(4,mu, sigma)))
Y(1)
Y10000 <- sapply(1:10000,Y)
hist(Y10000)
mean(Y10000)

4*mu
#varianza suma muestral
4*sigma^2
var(Y1000)

###
hist(Y10000)

  #c)
1-pnorm(103, mu, sigma)

  #d)
Xbar <- function(i)(mean(rnorm(4,mu, sigma)))
Xbar10000 <- sapply(1:10000,Xbar)
mean(Xbar10000<98)

pnorm(98, mu, sigma/sqrt(4))

  #e) usando 32 y no 98

Ssq <- function(i)(var(rnorm(100,mu, sigma))) #hacemos la grafica de todas las varianzas posibles
Ssq100000 <- sapply(1:10000,Ssq)
hist(Ssq100000)
mean(Ssq100000>32) #estimacion
#exactitud-> teorema de normalizacion, usamos la formula de F(x) de esta conversion
1-pchisq((100-1)*32/sigma^2, 100-1)
hist(Ssq100000*(100-1)/sigma^2, prob=TRUE)
curve(dchisq(x, 100-1), add= TRUE, col='red')
