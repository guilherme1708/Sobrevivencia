# Lista 3 - MAE0514

# local de trabalho 
setwd("~/Área de Trabalho/Lista 3")

# Pacotes
library(ggplot2)
library(asaur)
library(survival)
library(survminer)
library(sqldf)
library(mice)
library(KMsurv)
library(mice)


# Exercício 1

# item a
Tempo <- c(0.3,5.9,20.8,28.0,1.7,73.6,7.2,2.1,6.4,2.5,2.3,0.3,0.4,65.4,64.9,0.6,23.0,42.6,48.0,6.9,2.1,43.6,42.6,12.0,0.8)
Censura <- c(1,1,1,0,1,0,1,1,1,1,1,1,1,0,0,1,1,0,0,1,1,0,1,0,1)

library(survival)
library(survminer)
Ex1 <- data.frame(Tempo,Censura)

KM1 <- survfit(Surv(Ex1$Tempo, Ex1$Censura)~1)


# Tabela com estimativas de Kaplan-Meier
knitr::kable(surv_summary(KM1),col.names = c("Tempo","nº em risco","nº de eventos",
                                             "censura","sobreviv.","desv.pad sobrev.",
                                             "IC(95%) sup.","IC(95%) inf."))

# Grafico Kaplan-Meier
ggsurvplot(KM1, data = Ex1,palette = c('blue'),
           ggtheme=theme_gray(), legend = 'none') + 
  labs(x="Tempo (em anos)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

x <- (18-12)/((20.8-12)/(0.396-0.44))+0.44

#log(0.65)/log(0.41)

#log(0.4831582)

# a =5% b= 80%
d <- data.frame()
d[1,1] = 4*(1.96+0.8416)^2/(-0.7274111)^2

# a = 8% b = 80%
d[1,2] = 4*(1.75+0.8416)^2/(-0.7274111)^2

# a =5% b= 85%
d[2,1] = 4*(1.96+1.0364)^2/(-0.7274111)^2

# a = 8% b = 85%
d[2,2] = 4*(1.75+1.0364)^2/(-0.7274111)^2

# a =5% b= 90%
d[3,1] = 4*(1.96+1.2816)^2/(-0.7274111)^2

# a = 8% b = 90%
d[3,2] = 4*(1.75+1.2816)^2/(-0.7274111)^2

a= 2

S30 <- (30-28)/((42.6-28)/(0.3017143-0.3520000))+0.3520000
S42 <- (42-28)/((42.6-28)/(0.3017143-0.3520000))+0.3520000
S54 <- 0.3017143
ppad <- 1-(S30+4*S42+ S54)/6

S30N <- S30^-log(ppad)
S42N <- S42^-log(ppad)
S54N <- S54^-log(ppad)

pnovo <- 1-(S30N+4*S42N+S54N)/6

pf <- (ppad+pnovo)/2

n24 <- d/pf
colnames(n24) <- c("5%","8%")
rownames(n24) <- c("80%","85%","90%")
knitr::kable(n24)

# item b
a= 4.5

S0 <- 1
S27<- 0.3520000
S54 <- 0.3017143
ppad <- 1-(S0+S27+4*S54)/6

S0N <- S0^-log(ppad)
S27N <- S27^-log(ppad)
S54N <- S54^-log(ppad)

ppad <- 1-(S0N+S27N+4*S54N)/6

pf <- (ppad+pnovo)/2

n54 <- d/pf
colnames(n54) <- c("5%","8%")
rownames(n54) <- c("80%","85%","90%")
knitr::kable(n54)

# Exercício 3

data <- read.csv("HOD_NHL.csv",header = T,sep=';')

# item a

ekm_ex3 <- survfit(Surv(Time, D_R)~ Graft,data = data)

# Grafico Kaplan-Meier
ggsurvplot(ekm_ex3, data = data, palette = c('deeppink','blue'),conf.int = T,
           ggtheme=theme_gray()) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

# categorizando variável Karnofsky
data$escore <- sapply(data$Karnofsky,
                          function(x){
                            if (x < 80) x = '<80'
                            else x = '>=80'
                          })
data$escore <- as.factor(data$escore)

ekm_ex3_esc <- survfit(Surv(Time, D_R)~ escore,data = data)

# Grafico Kaplan-Meier
ggsurvplot(ekm_ex3_esc, data = data, palette = c('deeppink','blue'),conf.int = T,
           ggtheme=theme_gray()) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

# item b

Modelo.wei <- survreg(Surv(Time, D_R)~ escore+Graft, dist='weibull',data = data)

summary(Modelo.wei)

df <- data.frame(Variaveis=c("Intercepto","escore>=80","Graft"), estimativa=c(exp(-Modelo.wei$coefficients[1]/Modelo.wei$scale),exp(-Modelo.wei$coefficients[2]/Modelo.wei$scale),exp(-Modelo.wei$coefficients[3]/Modelo.wei$scale)))

knitr::kable(df,row.names = FALSE)

# item d

knitr::kable(summary(Modelo.wei)$table)


# Exercício 4

# item a

# Modelo log-logistico
Modelo.ll <- survreg(Surv(Time, D_R)~ escore+Graft, dist='loglogistic',data = data)

summary(Modelo.ll)

# item b

exp(Modelo.ll$coefficient[2]+Modelo.ll$coefficient[3])

# Exercício 5

# item a

data <- read.csv("HOD_NHL.csv",header = T,sep=';')
data$Graft <- as.factor(data$Graft)
data$escore <- sapply(data$Karnofsky,
                      function(x){
                        if (x < 80) x = '<80'
                        else x = '>=80'
                      })
data$escore <- as.factor(data$escore)

Modelo.wei <- survreg(Surv(Time, D_R)~ escore+Graft, dist='weibull',data = data)
Modelo.llog <- survreg(Surv(Time, D_R)~ escore+Graft, dist='loglogistic',data = data)

beta <- as.vector(-Modelo.wei$coef/Modelo.wei$scale)
gama <- 1/Modelo.wei$scale

dataG1 <- data[data$Graft==1,]
dataG2 <- data[data$Graft==2,]

# Weibull
a <- exp(beta[1])*dataG1$Time^(gama)
A <- log(a)
plot(log(dataG1$Time),A,ylab="taxa de falha acumulada",xlab="log(Tempo)",main="Graft=1")
abline(log(exp(beta[1])),gama,lty=2)

b <- exp(beta[1]+beta[3])*dataG2$Time^(gama)
B <- log(b)
plot(log(dataG2$Time),B,ylab="taxa de falha acumulada",xlab="log(Tempo)",main="Graft=2")
abline(log(exp(beta[1]+beta[3])),gama,lty=2)

# item b

#log-logistico

beta <- Modelo.llog$coefficients
gama <- 1/Modelo.llog$scale

a <- log(1+exp(-beta[1]*gama)*dataG1$Time^gama)
A <- log(exp(a)-1)
plot(log(dataG1$Time),A,ylab="taxa de falha acumulada",xlab="log(Tempo)",main="Graft=1")

b <- log(1+exp(-(beta[1]+beta[3])*gama)*dataG2$Time^gama)
B <- log(exp(b)-1)
plot(log(dataG2$Time),B,ylab="taxa de falha acumulada",xlab="log(Tempo)",main="Graft=2")

# Exercício 6

v2 <- ifelse(data$Graft==2,1,0)
v3 <- ifelse(data$escore==">=80",1,0)

xb_wei<- Modelo.wei$coef[1]+Modelo.wei$coef[2]*v2+Modelo.wei$coef[3]*v3
res_wei<- (log(data$Time)-xb_wei)/Modelo.wei$scale

xb_llog<- Modelo.llog$coef[1]+Modelo.llog$coef[2]*v2+Modelo.llog$coef[3]*v3
res_llog<- (log(data$Time)-xb_llog)/Modelo.llog$scale

resid_wei<-exp(res_wei)
resid_llog<-exp(res_llog)

coxsnell_wei<- (data$Time^(1/Modelo.wei$scale))*exp(-xb_wei/Modelo.wei$scale)
coxsnell_llog<- log(1+(data$Time^(1/Modelo.llog$scale))*exp(-xb_llog/Modelo.llog$scale))

coxsnell_wei

coxsnell_llog

# RESÍDUOS DEVIANCE

m_wei<- data$D_R- coxsnell_wei
m_llog<- data$D_R- coxsnell_llog
deviance_wei <-  sqrt(-2*(m_wei+data$D_R*log(data$D_R-m_wei)))*ifelse(m_wei<0,-1,1)
deviance_llog <-  sqrt(-2*(m_llog+data$D_R*log(data$D_R-m_llog)))*ifelse(m_llog<0,-1,1)

deviance_wei

deviance_llog

#WEIBULL
# Curva de Kaplan-Meier
KM_wei <-  survfit(Surv(coxsnell_wei, data$D_R)~1)
TFAcum_KM_wei <- -log(KM_wei$surv)
# Estimador de Nelson_Aalen
Surv_Aa_wei <- survfit(coxph(Surv(coxsnell_wei, data$D_R)~1))
TFAcum_Aa_wei <- -log(Surv_Aa_wei$surv)
#GrÃ¡fico
plot(KM_wei$time,TFAcum_KM_wei, col="dark red", pch=16, main="Gráfico da função de risco acumulada - Weibull", xlab="Tempo", ylab="-log(S(t))", cex=0.8 )
points(Surv_Aa_wei$time,TFAcum_Aa_wei, col="navy blue", pch=16, cex=0.8)
abline(0,1,lty=2)

#LOG-LOGÍSTICA
# Curva de Kaplan-Meier
KM_llog <-  survfit(Surv(coxsnell_llog, data$D_R)~1)
TFAcum_KM_llog <- -log(KM_llog$surv)
# Estimador de Nelson_Aalen
Surv_Aa_llog <- survfit(coxph(Surv(coxsnell_llog, data$D_R)~1))
TFAcum_Aa_llog <- -log(Surv_Aa_llog$surv)
#Gráfico

plot(KM_llog$time,TFAcum_KM_llog, col="dark red", pch=16, main="Gráfico da função de risco acumulada - log-logístico", xlab="tempo", ylab="-log(S(t))", cex=0.8 )
points(Surv_Aa_wei$time,TFAcum_Aa_llog, col="navy blue", pch=16, cex=0.8)
abline(0,1,lty=2)

plot(data$Time, deviance_wei, pch=16, col="orange", main="Resíduos deviance - modelo Weibull",xlab="tempo",ylab="Deviance")

plot(data$Time, deviance_llog, pch=16, col="tomato1", main="Resíduos deviance - modelo log-logístico",xlab="tempo",ylab="Deviance")

# CRITÉRIOS DE AKAIKE E BIC (Klein e Moeschberger)

AIC_wei<- -2*Modelo.wei$loglik[2]+2*4
AIC_llog<- -2*Modelo.llog$loglik[2]+2*4
n<- length(data$Time)
BIC_wei <- Modelo.wei$loglik[2]-(3/2)*log(n)
BIC_llog <- Modelo.llog$loglik[2]-(3/2)*log(n)


