# Lista 1 - MAE 514

library(ggplot2)
library(gridExtra)

# Exercício 3
# item a

# função taxa de falha
alpha <- function(t){
  l <- vector()
  for(i in 1:length(t)){
    if(t[i]>=0 & t[i]<=5){
      l[i]=0.07
    }
    else {
      l[i]=0.14
    }
  }
  return(l)
}

t <- data.frame(t=seq(0,10,0.1))

ggplot(t, aes(x=t)) + 
  stat_function(fun = alpha) +
  scale_y_continuous(limits = c(0,0.15)) +
  scale_x_continuous(breaks = seq(0,10)) +
  labs(y = expression(alpha(t)), 
       x = "t",
       title = "Função taxa de falha") 

# item b

Alpha <- vector()
for(i in 1:length(t$t)){
  Alpha[i]=integrate(alpha,0,t$t[i])$value
}

S <- data.frame(S=exp(-Alpha),t=t$t)

ggplot(S, aes(x=t,y=S)) + 
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,10),breaks = seq(0,10)) +
  labs(y = "S(t)", 
       x = "t",
       title = "Função de Sobrevivência")

ggplot(S, aes(x=t,y=S)) + 
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,10),breaks = seq(0,10)) +
  labs(y = "S(t)", 
       x = "t",
       title = "Fun??o de Sobreviv?ncia")+
  geom_abline(intercept = 0.5, slope = 0, color = "red", lty=2)+
  geom_vline(xintercept = 7.45, color = "red",lty=2)

# Exercício 4
# item a

# função taxa de sobrevivência
S_4 <- function(t,l,a){
  S <- vector()
  for(i in 1:length(t)){
    S[i] <- 1/(1+l*t[i]^a)
  }
  return(S)
}

S_4(50,0.001,1.5)
S_4(100,0.001,1.5)
S_4(150,0.001,1.5)

# item b

S_4(100,0.001,1.5)

t <- seq(0,1000,0.1)
S <-S_4(t,0.001,1.5)

dad <- data.frame(t,S)
ggplot(dad, aes(x=t,y=S)) + 
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1000)) +
  labs(y = "S(t)", 
       x = "t",
       title = "Fun??o de Sobreviv?ncia")+
  geom_abline(intercept = 0.5, slope = 0, color = "red", lty=2)+
  geom_vline(xintercept = 100, color = "red",lty=2)

# item c

# função taxa de taxa de falha
alpha_4 <- function(t,l,a){
  alfa <- vector()
  for(i in 1:length(t)){
    alfa[i] <- (a*l*t[i]^(a-1))/(1+l*t[i]^a)
  }
  return(alfa)
}

t1 <- data.frame(t1=seq(0,200,0.1))
ggplot(t1, aes(x=t1)) + 
  stat_function(fun = function(y) alpha_4(y,0.001,1.5)) +
  geom_vline(xintercept = 63,colour='red') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = bquote(alpha(t)), 
       x = "t",
       title = "Função de taxa de falha") 

# item d

R <- function(t){
  R <- vector()
  for(i in 1:length(t)){
    R[i] <- 1/(1+0.001*t[i]^1.5)
  }
  return(R)
}
integrate(R,0,Inf)$value

# Exercício 5

# função taxa de sobrevivência
S_5 <- function(t,l,p){
  S <- vector()
  for(i in 1:length(t)){
    S[i] <- exp(-l*t[i]^p)
  }
  return(S)
}

plot1 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) S_5(y,0.5,0.1),colour='blue') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "S(t)", 
       x = "t",
       title = bquote(lambda == 0.5 ~ 'e' ~ rho == 0.1)) 

plot2 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) S_5(y,0.5,0.5),colour='blue') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "S(t)", 
       x = "t",
       title = bquote(lambda == 0.5 ~ 'e' ~ rho == 0.5)) 

plot3 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) S_5(y,0.5,1),colour='blue') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "S(t)", 
       x = "t",
       title = bquote(lambda == 0.5 ~ 'e' ~ rho == 1)) 

plot4 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) S_5(y,0.1,0.1),colour='blue') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "S(t)", 
       x = "t",
       title = bquote(lambda == 0.1 ~ 'e' ~ rho == 0.1)) 

plot5 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) S_5(y,0.1,0.5),colour='blue') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "S(t)", 
       x = "t",
       title = bquote(lambda == 0.1 ~ 'e' ~ rho == 0.5)) 

plot6 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) S_5(y,0.1,1),colour='blue') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "S(t)", 
       x = "t",
       title = bquote(lambda == 0.1 ~ 'e' ~ rho == 1)) 

grid.arrange(plot1, plot2, plot3,
             plot4, plot5, plot6, ncol=3,nrow=2)

# função taxa de falha
alpha_5 <- function(t,l,p){
  alpha <- vector()
  for(i in 1:length(t)){
    alpha[i] <- (exp(-l*t[i]^p)*l*p*t[i]^(-1+p))/exp(-l*t[i]^p)
  }
  return(alpha)
}

plot1 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) alpha_5(y,0.5,0.1),colour='red') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = bquote(alpha(t)), 
       x = "t",
       title = bquote(lambda == 0.5 ~ 'e' ~ rho == 0.1)) 

plot2 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) alpha_5(y,0.5,0.5),colour='red') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = bquote(alpha(t)), 
       x = "t",
       title = bquote(lambda == 0.5 ~ 'e' ~ rho == 0.5)) 

plot3 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) alpha_5(y,0.5,1),colour='red') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = bquote(alpha(t)), 
       x = "t",
       title = bquote(lambda == 0.5 ~ 'e' ~ rho == 1)) 

plot4 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) alpha_5(y,0.1,0.1),colour='red') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = bquote(alpha(t)), 
       x = "t",
       title = bquote(lambda == 0.1 ~ 'e' ~ rho == 0.1)) 

plot5 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) alpha_5(y,0.1,0.5),colour='red') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = bquote(alpha(t)), 
       x = "t",
       title = bquote(lambda == 0.1 ~ 'e' ~ rho == 0.5)) 

plot6 <- ggplot(t, aes(x=t)) + 
  stat_function(fun = function(y) alpha_5(y,0.1,1),colour='red') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = bquote(alpha(t)), 
       x = "t",
       title = bquote(lambda == 0.1 ~ 'e' ~ rho == 1)) 

grid.arrange(plot1, plot2, plot3,
             plot4, plot5, plot6, ncol=3,nrow=2)

# Exercício 6
# item a
#                                        rho         1/lambda
set.seed(4)
rweibull <- data.frame(W = rweibull(100,shape = 1, scale = 2))
ggplot(rweibull, aes(y=W)) + 
  geom_boxplot()+
  labs(y = "rweibull",
       title = "Box plot rweibull")

ggplot(rweibull, aes(x = W)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(0, 10, by = 0.4), 
                 colour = "black", 
                 fill = "white")+
  stat_function(fun = dweibull,
                args = list(shape = 1, scale = 2))

dat <- data.frame(t=knots(ecdf(rweibull$W)),
                  S=1-ecdf(rweibull$W)(knots(ecdf(rweibull$W))))

ggplot(dat,aes(x=t, y=S)) +
  geom_step() +
  stat_function(fun = function(y) S_5(y,0.5,1),colour='blue') +
  scale_x_continuous(breaks = seq(0,10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "S(t)", 
       x = "t",
       title = 'Função de sobrevivência') 

# item b

# QQplot
ggplot(rweibull, aes(sample = W)) +
  stat_qq(distribution = qweibull,dparams = list(shape = 1, scale = 2)) +
  stat_qq_line(distribution = qweibull, dparams = list(shape = 1, scale = 2))+
  ggtitle("QQ-Plot Weibull")

# item c

# substraindo da média e dividindo pelo desvio padrão
rweibull2 <- data.frame(W_pad=(rweibull$W-mean(rweibull$W))/sd(rweibull$W))

ggplot(rweibull2, aes(sample = W_pad)) + 
  stat_qq(distribution = qnorm) + 
  stat_qq_line(distribution = qnorm) + 
  ggtitle("QQ-Plot Normal")

# item d

#n=40
set.seed(4)
rweibull <- data.frame(W = rweibull(40,shape = 1, scale = 2))

ggplot(rweibull, aes(sample = W)) +
  stat_qq(distribution = qweibull,dparams = list(shape = 1, scale = 2)) +
  stat_qq_line(distribution = qweibull, dparams = list(shape = 1, scale = 2))+
  ggtitle("QQ-Plot Weibull")

# substraindo da média e dividindo pelo desvio padrão
rweibull2 <- data.frame(W_pad=(rweibull$W-mean(rweibull$W))/sd(rweibull$W))

ggplot(rweibull2, aes(sample = W_pad)) + 
  stat_qq(distribution = qnorm) + 
  stat_qq_line(distribution = qnorm) + 
  ggtitle("QQ-Plot Normal")

#n=300
set.seed(4)
rweibull <- data.frame(W = rweibull(300,shape = 1, scale = 2))

ggplot(rweibull, aes(sample = W)) +
  stat_qq(distribution = qweibull,dparams = list(shape = 1, scale = 2)) +
  stat_qq_line(distribution = qweibull, dparams = list(shape = 1, scale = 2))+
  ggtitle("QQ-Plot Weibull")

# substraindo da média e dividindo pelo desvio padrão
rweibull2 <- data.frame(W_pad=(rweibull$W-mean(rweibull$W))/sd(rweibull$W))

ggplot(rweibull2, aes(sample = W_pad)) + 
  stat_qq(distribution = qnorm) + 
  stat_qq_line(distribution = qnorm) + 
  ggtitle("QQ-Plot Normal")

#n=1200
set.seed(4)
rweibull <- data.frame(W = rweibull(1200,shape = 1, scale = 2))

ggplot(rweibull, aes(sample = W)) +
  stat_qq(distribution = qweibull,dparams = list(shape = 1, scale = 2)) +
  stat_qq_line(distribution = qweibull, dparams = list(shape = 1, scale = 2))+
  ggtitle("QQ-Plot Weibull")

# substraindo da média e dividindo pelo desvio padrão
rweibull2 <- data.frame(W_pad=(rweibull$W-mean(rweibull$W))/sd(rweibull$W))

ggplot(rweibull2, aes(sample = W_pad)) + 
  stat_qq(distribution = qnorm) + 
  stat_qq_line(distribution = qnorm) + 
  ggtitle("QQ-Plot Normal")