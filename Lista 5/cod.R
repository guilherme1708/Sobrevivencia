# local de trabalho 
setwd("~/Área de Trabalho/Lista 5")

# Pacotes
library(ggplot2)
library(survival)
library(survminer)
library(KMsurv)
library(cmprsk)
library(readr)

# Exercício 1

# Dados
BMT_DataOBITO <- read_delim("BMT_DataOBITO.csv",";", 
                            escape_double = FALSE, trim_ws = TRUE)
BMT_DataRELAPSED <- read_delim("BMT_DataRELAPSED.csv",";", 
                               escape_double = FALSE, trim_ws = TRUE)

# item b

# modelo inicial
modelo1.cox <- coxph(Surv(Tinicio, Tfim, Delta2)~A+C+Z1+Z3+Z7+Z8+Z10,
                     data=BMT_DataRELAPSED)
summary(modelo1.cox)

knitr::kable(drop1(modelo1.cox, test="Chisq"),digits = 3)

# modelo final
modelo2.cox <- coxph(Surv(Tinicio, Tfim, Delta2)~Z8, data=BMT_DataRELAPSED)
summary(modelo2.cox)

drop1(modelo2.cox, test="Chisq")

# resíduos de Schoenfeld
zph <- cox.zph(modelo2.cox)
plot(zph)
abline(h=modelo2.cox$coefficients,col='red')
knitr::kable(zph$table,digits = 3)

# item c
# modelo inicial
modelo3.cox <- coxph(Surv(Tinicio, Tfim, Delta1)~.-Id, data=BMT_DataOBITO)
summary(modelo3.cox)

drop1(modelo3.cox, test="Chisq")

# modelo final
modelo4.cox <- coxph(Surv(Tinicio, Tfim, Delta1)~Z8, data=BMT_DataOBITO)
summary(modelo4.cox)

knitr::kable(drop1(modelo3.cox, test="Chisq"),digits = 3)

# resíduos de Schoenfeld
zph2 <- cox.zph(modelo4.cox)
plot(zph2)
abline(h=modelo4.cox$coefficients,col='red')
knitr::kable(zph2$table,digits = 3)

# Exercício 2

# dados
litter_Data <- read.csv("litter-data.csv")

# Kaplan Meier
KM <- survfit(Surv(Tempo,Delta)~ Tratamento,data = litter_Data)

# Grafico Kaplan-Meier
ggsurvplot(KM, data =litter_Data, linetype = 1,  risk.table = TRUE,
           xlab="Tempo (em semanas)",
           ylab=expression(hat(S(t))), risk.table.title="Em risco",
           legend.title ="Tratamento",ggtheme=theme_gray(),
           legend.labs=c("Placebo", "Droga"), pval=TRUE)

# item b

# modelo de cox ignorando ninhada
cox_fit1 <- coxph(Surv(Tempo, Delta)~ Tratamento,data=litter_Data)
summary(cox_fit1)

# Residuos de Schoenfeld
zph_km <- cox.zph(cox_fit1,transform='km')

plot(zph_km)
abline(h=cox_fit1$coefficients,col='red')

# tabela do testeS
knitr::kable(zph_km$table,digits = 3)

# item c

# modelo de cox estratificado por ninhada
cox_strat_fit2 <-coxph(Surv(Tempo, Delta)~ Tratamento+strata(Ninhada),
                       data = litter_Data)
summary(cox_strat_fit2)

# Residuos de Schoenfeld
zph_km2 <- cox.zph(cox_strat_fit2,transform='km')

plot(zph_km2)
abline(h=cox_strat_fit2$coefficients,col='red')

# tabela do teste
knitr::kable(zph_km2$table,digits = 3)

# Exercício 3

# Dados
lin <- c(158, 192, 193, 194, 195, 202, 212, 215, 229, 230, 237,
         240, 244, 247, 259, 300, 301, 337, 415, 444, 485, 496,
         529, 537, 624, 707, 800)
sar <-  c(430, 590, 606, 638, 655, 679, 691, 693, 696,
          747, 752, 760, 778, 821, 986)
out <- c(136, 246, 255, 376, 421, 565, 616, 617, 652, 655, 658,
         660, 662, 675, 681, 734, 736, 737, 757, 769, 777, 801,
         807, 825, 855, 857, 864, 868, 870, 873, 882, 895, 910,
         934, 942, 1015, 1019)

causa <- factor(c(rep('Linfoma',27),rep('Sarcoma',15),rep('Outras_Causas',37)))
tempo <- c(lin,sar,out)
data3 <- data.frame(tempo,causa)

# item a

# estimativas das curvas de incidência acumulada
CIF <- cuminc(tempo,factor(causa), cencode = 0)

# Estimativas de km globais
KM3 <- survfit(Surv(tempo)~1,data = data3)

# dados para 1-km e cuvras de incidência acumulada
df3d <- data.frame(KM3$time,1-KM3$surv,
                   t(timepoints(CIF, times = data3$tempo)$est))

colnames(df3d) <- c("time","F","Linfoma","Outras_Causas","Sarcoma")

# cores
colors <- c("Linfoma" = "Blue",
            "Sarcoma" = "Red",
            "Outras Causas" = "Green")

# curvas das estimativas das curvas de incidência acumulada
ggplot(data = df3d) +  
  geom_step(aes(x = time, y = Linfoma,color = "Linfoma")) +
  geom_step(aes(x = time, y = Sarcoma,color = "Sarcoma")) +
  geom_step(aes(x = time, y = Outras_Causas,color = "Outras Causas")) +
  scale_x_continuous(breaks = c(200,300,400,600,800,1000)) +
  scale_color_manual(values = colors) +
  labs(x="Idade ao morrer (dias)",
       y="Probabilidade do evento",
       title = "Estimativas das curvas de incidência acumulada",
       color = "Evento") 

# item b
knitr::kable(data.frame(time = c(200,300,400,600,800,1000),
  t(timepoints(CIF, times = c(200,300,400,600,800,1000))$est)),
  digits = 3,row.names = F,col.names = c("time","Linfoma","Outras Causas","Sarcoma"))

# item c

# Grafico Kaplan-Meier global
ggsurvplot(KM3, data = data3, linetype = 1, xlab="Idade ao morrer (dias)",legend='none',
           title='Curva de Kaplan-Meier Global',
           ggtheme=theme_gray())

# tabela com as estimavivas de km para o t=200,300,400,600,800 e 1000
table <- summary(KM3,time=c(200,300,400,600,800,1000))
table <- data.frame(table$time,table$surv,1-table$surv)
colnames(table) <- c("time", "km","1-km")
knitr::kable(table,digits = 3)

# item d
linf <- ifelse(causa=='Linfoma',1,0)
sarc <- ifelse(causa=='Sarcoma',1,0)
oc <- ifelse(causa=='Outras_Causas',1,0)

# Km evento linfoma
KM4 <- survfit(Surv(tempo,linf)~1,data = data3)

# Grafico Kaplan-Meier linfoma
plot1 <- ggsurvplot(KM4,legend='none',
                    title='Kaplan-Meier Linfoma tímico',
                    ggtheme=theme_gray()) +
                    labs(y='Probabilidade do evento',
                         x='Idade ao morrer (dias)')

# Km evento sarcoma
KM5 <- survfit(Surv(tempo,sarc)~1,data = data3)

# Grafico Kaplan-Meier sarcoma
plot2 <- ggsurvplot(KM5, legend='none',
                    title='Kaplan-Meier Sarcoma de células reticulares',
                    ggtheme=theme_gray()) +
                    labs(y='Probabilidade do evento',
                         x='Idade ao morrer (dias)')

# Km evento outras causas
KM6 <- survfit(Surv(tempo,oc)~1,data = data3)

# Grafico Kaplan-Meier outras causas
plot3 <- ggsurvplot(KM6,legend='none', 
                    title='Kaplan-Meier Outras causas',
                    ggtheme=theme_gray()) + 
                    labs(y='Probabilidade do evento',
                         x='Idade ao morrer (dias)')

# PLot das 3 curvas
arrange_ggsurvplots(list(plot1, plot2, plot3), 
                    print = TRUE,ncol = 2, nrow = 2)

# cores
colors <- c("1-Kaplan-Meier" = "Black",
            "Linfoma" = "Blue",
            "Sarcoma" = "Red",
            "Outras Causas" = "Green")

# Gráficos das estimativas de 1-Kaplan-Meier e incidência acumulada
ggplot(data = df3d) +  
  geom_step(aes(x = time, y = F,color = "1-Kaplan-Meier")) +
  geom_step(aes(x = time, y = Linfoma,color = "Linfoma")) +
  geom_step(aes(x = time, y = Sarcoma,color = "Sarcoma")) +
  geom_step(aes(x = time, y = Outras_Causas,color = "Outras Causas")) +
  scale_x_continuous(breaks = c(200,300,400,600,800,1000)) +
  scale_color_manual(values = colors) +
  labs(x="Idade ao morrer (dias)",
       y="Probabilidade do evento",
       title = "Estimativas de 1-Kaplan-Meier e incidência acumulada",
       color = "Curvas estimadas") 