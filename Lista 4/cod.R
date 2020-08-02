# Lista 3 - MAE0514

# local de trabalho 
setwd("~/Área de Trabalho/Lista 4")

# Pacotes
library(ggplot2)
library(survival)
library(survminer)
library(KMsurv)


# Exercício 2

data <- data.frame(trat = c(rep("Sem_trat",10),rep("Radicao",10),
                            rep("Radiacao_BPA",10)),
                   delta = c(rep(1,19),0,rep(1,8),0,0),
                   tempo = c(20,21,23,24,24,26,26,27,28,30,26,28,29,29,30,
                             30,31,31,32,35,31,32,34,35,36,38,38,39,42,42))


# item a

KM2 <- survfit(Surv(tempo, delta)~trat,data = data)

# Grafico Kaplan-Meier
ggsurvplot(KM2, data = data,conf.int = T,legend.title = "Tratamento",
           legend.labs = c("Radiação + BPA", "Radiação","Nenhum"),
           ggtheme=theme_gray()) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

# teste log-rank
knitr::kable(surv_pvalue(KM2, method = c("1"),data=data)[,1:3])

# item b

data$Z1 <- sapply(data$trat,
                  function(x){
                    if (x == 'Radicao') x = '1'
                    else x = '0'
                  })
data$Z1 <- as.factor(data$Z1)

data$Z2 <- sapply(data$trat,
                  function(x){
                    if (x == 'Radiacao_BPA') x = '1'
                    else x = '0'
                  })
data$Z2 <- as.factor(data$Z2)

attach(data)

# Efron
fit <- coxph(Surv(tempo,delta)~Z1+Z2,ties = "efron",data=data)
summary(fit)

# Breslow
fit2 <- coxph(Surv(tempo,delta)~Z1+Z2,ties = "breslow")
summary(fit2)

# cox/exato
fit3 <- coxph(Surv(tempo,delta)~Z1+Z2,ties = "exact")
summary(fit3)

# item e
l <- -2*logLik(coxph(Surv(tempo,delta)~Z2,ties = "breslow"))+2*logLik(fit2)

1-pchisq(l,1) # p-valor
detach(data)

# Exercicio 3

data3 <- data.frame(Tumor = c(rep("grande",15),rep("moderado",20)),
                    delta = c(rep(1,5),rep(0,4),1,rep(0,5),
                              rep(1,6),0,0,rep(1,9),0,1,0),
                    Tempo = c(28,89,175,195,309,377,393,421,447,462,709,744,770,
                              1106,1206,34,88,137,199,280,291,299,300,308,351,358,
                              369,370,371,375,382,392,429,451,1119))

# item a
# dummy tumor
data3$Tumor <- ifelse(data3$Tumor=="grande",1,0)

# Exercicio 4

# item a

KM4 <- survfit(Surv(Tempo, delta)~Tumor,data = data3)

# Grafico Kaplan-Meier
ggsurvplot(KM4, data = data3,conf.int = T,legend.title = "Tumor",risk.table = T,
           legend.labs = c("Moderado", "Grande"),
           ggtheme=theme_gray()) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

# item b

# teste Log-rank
knitr::kable(surv_pvalue(KM4, method = c("1"))[,1:3],digits = 3)

# teste Familia Fleming-Harrington
knitr::kable(surv_pvalue(KM4, method = c("FH_p=1_q=1"))[,1:3])

# item c

fit3 <- coxph(Surv(Tempo, delta)~Tumor,data = data3)
summary(fit3)

# Exercicio 5

data5 <- read.csv('Lista2_Hodgkins.csv',sep=';',dec=',')

data5$id <- NULL

# item a
fit5 <- coxph(Surv(survivaltime,dead)~sex+stage+factor(hist),
              data = data5, ties="breslow")
summary(fit5)

# martingal
plot(data5$age,resid(fit5), xlab='Idade', 
     ylab='Residuo martingal', pch=16, col="steelblue3" )
smooth <- lowess(data5$age,resid(fit5),iter=0)
lines(smooth)

# item b

fit6 <- coxph(Surv(survivaltime,dead)~age+sex+stage+factor(hist),
              data = data5, ties="breslow")
summary(fit6)

# item c

fit7 <- coxph(Surv(survivaltime,dead)~age+stage,data = data5, ties="breslow")
knitr::kable(anova(fit6, test="Chisq"),digits = 3)

summary(fit7)

# item d

## Cox-Snell residual
coxsnell <- fit6$residuals*data5$dead
x <- coxsnell[coxsnell!=0]
data5 <- data.frame(data5,x)

Surv_Aa <- survfit(coxph(Surv(x, dead)~1,data = data5))

plot(Surv_Aa$surv,x, col="dark red", pch=16, main="Resíduos Cox-Senell", 
     xlab="Resíduos de Cox-Snell", ylab=expression(hat(A)(r[es])),
     cex=0.8,ylim=c(-0.5,1) )
abline(0,1,lty=2)

# item e

plot(fit7$linear.predictors,resid(fit7,type='deviance'),
     xlab='Preditores lineares', ylab='Residuo deviance',
     pch=16, col="royalblue3")


# item f

scho <- cox.zph(fit6,transform='identity')
par(mfrow=c(2,3))
plot(scho) 

# tabela
knitr::kable(scho$table,digits = 3)



