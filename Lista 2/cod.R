# set 
setwd("~/Área de Trabalho/Lista 2")

# Lista 2 - MAE 514

library(ggplot2)
library(asaur)
library(survival)
library(survminer)
library(sqldf)
library(KMsurv)

# Exercício 2

# item a

# Carregando dados
dados_HIV <- read.table('Lista2_HIV.txt',header = F)

# Atribuindo nomes as variáveis
names(dados_HIV) <- c("Paciente", "Idade_entrada", "Idade_morte_censura","Falha")

# Criando variável tempo
dados_HIV$Tempo <- (dados_HIV$Idade_morte_cen - dados_HIV$Idade_entr)

# item b
dados_HIV$Faixa <- sapply(dados_HIV$Tempo,
                          function(x){
                            if (x <= 60) x = 'Faixa_1'
                            else if (x > 60 & x <= 120)  x = 'Faixa_2'
                            else if (x > 120 & x <= 240) x = 'Faixa_3'
                            else if (x > 240 & x <= 360) x = 'Faixa_4'
                            else if (x > 360 & x <= 480) x = 'Faixa_5'
                            else x = 'Faixa_6'
                          })

cont_HIV <- sqldf('SELECT Faixa,
                          count(Faixa),
                          sum(Falha),
                          count(Faixa)-sum(Falha)
                          FROM dados_HIV GROUP BY Faixa')

# Dados para contrução do estimador tábua de vida
intervalos <- c(0,60,120,240,360,480,600)
nindiv <- 45
ncens <- c(0,1,5,12,3,4)
neventos <- c(5,3,6,4,2,0)
HIV.tv <- lifetab(intervalos,nindiv,ncens,neventos)

# Tabela com estimativas de tábua de vida
HIV.tv_imp <- round(HIV.tv[,c("nsubs","nlost","nrisk","nevent","surv","se.surv")],3)
names(HIV.tv_imp) <- c("nº individuos","nº cens","nº individuos em risco",
                       "nº de eventos","sobrevivência","desv.pad sobrevivência")
knitr::kable(caption = "Tabela com estimativas Tábua de Vida", HIV.tv_imp)

# Gráfico do estimador tábua de vida
dt <- data.frame(x=intervalos[1:6],y= HIV.tv[,5])

ggplot() +
  geom_step(aes(x=x,y=y),data=dt) + 
  labs(title = "Estimativas Tábua de Vida",
       y = expression(hat(S(t))),
       x = "Tempo (em meses)")

# item c
ekm_ex2 <- survfit(Surv(dados_HIV$Tempo, dados_HIV$Falha)~1)

# Tabela com estimativas de Kaplan-Meier
knitr::kable(surv_summary(ekm_ex2),col.names = c("Tempo","nº em risco","nº de eventos",
                                                 "censura","sobreviv.","desv.pad sobrev.",
                                                 "IC(95%) sup.","IC(95%) inf."))

# Grafico Kaplan-Meier
ggsurvplot(ekm_ex2, data = dados_HIV,palette = c('blue'),
           ggtheme=theme_gray(), legend = 'none') + 
      labs(x="Tempo (em meses)",
           y=expression(hat(S(t))),
           title = "Estimativas de Kaplan-Meier") 
  
# item d

m <- 1 # numero de grupos
df <- data.frame(time = c(rep(0, m), ekm_ex2$time),
                 surv = c(rep(1, m), ekm_ex2$surv))

colors <- c("Tábua de vida" = "Black", "Kaplan-Meier" = "Blue")

ggplot(data = df) +  
  geom_step(aes(x = time, y = surv,color = "Kaplan-Meier")) +
  scale_y_continuous(limits = c(0,1)) +
  geom_step(aes(x=x,y=y,color = "Tábua de vida"),data=dt) +
  scale_x_continuous(breaks = seq(0,500,100)) +
  scale_color_manual(values = colors) +
  labs(x="Tempo (em meses)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier e Tábua de Vida",
       color = "Curvas estimadas") 


# Exercício 4

# dados exercício 4
df_ex4 <- data.frame(tempos=c(0.19,0.78,0.96,1.31,2.78,3.16,4.67,4.85,6.5,7.35,
                          8.27,12.07,32.52,33.91,36.71),
                    falhas=rep(1,15))
  

# função que estima a função de sobrevivência via KM
S_km <- function(tempos,falhas,n){
  
  k <- length(tempos)
  km_matrix <- matrix(0,k+1,4)
  
  km_matrix[1,] <- c(0,0,n,1)
  
  km_matrix[1:k+1,1] <- tempos
  km_matrix[1:k+1,2] <- falhas
  
  for (i in 3:k){
    km_matrix[2,3] <- km_matrix[1,3]
    km_matrix[i,3] <- km_matrix[i-1,3] - km_matrix[i,2]
  }

  km_matrix[k+1,3] <- km_matrix[k-1,3] - km_matrix[k,2] 
  
  for(j in 1:k+1){
    km_matrix[j,4] <- round((1-km_matrix[j,2]/km_matrix[j,3])*km_matrix[j-1,4],2)
  }
  
  df <- data.frame(km_matrix)
  colnames(df) <- c(expression(t[j]),expression(d[j]),expression(n[j]),"Surv")
  return(df)
} 

S <- S_km(df_ex4[,1],df_ex4[,2],25)

# Gráfico do estimador tábua de vida
dt_ex4 <- data.frame(x=S[,1],y= S[,4])

ggplot() +
  geom_step(aes(x=x,y=y),data=dt_ex4) + 
  ylim(c(.3,1)) +
  geom_point(shape=3,aes(x=36.71,y=0.4)) +
  labs(x="Tempo (em minutos)",
      y=expression(hat(S(t))),
      title = "Estimativas de Kaplan-Meier") 

# item b

# (32.52-12.07)/(0.48-0.52)=(med-12.07)/(0.5-0.52)
med <- (32.52-12.07)/(0.48-0.52)*(0.5-0.52) + 12.07
med

# item c

# (2.78-1.31)/(0.80-0.84)=(2-1.31)/(x-0.84)
x <- (0.80-0.84)/(2.78-1.31)*(2-1.31)+0.84
x

# Exercício 5

# Carregando os dados
data_Hodgkins <- readxl::read_excel("Lista2_Hodgkins.xlsx")

# item a

ekm_ex5 <- survfit(Surv(survivaltime, dead)~ sex,data = data_Hodgkins)

# Grafico Kaplan-Meier
ggsurvplot(ekm_ex5, data = data_Hodgkins, palette = c('deeppink','blue'),conf.int = T,
           ggtheme=theme_gray(), legend.labs = c("Feminino", "Masculino")) + 
  labs(x="Tempo (em meses)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

# teste log-rank
surv_pvalue(ekm_ex5, method = c("1"))[,1:3]

# teste Tarone-Ware
surv_pvalue(ekm_ex5, method = c("sqrtN"))[,1:3]

# item b

# criando faixas de idade
data_Hodgkins$Faixa_idade <- sapply(data_Hodgkins$age,
                          function(x){
                            if (x < 25) x = 'Faixa_1'
                            else if (x >= 25 & x < 38)  x = 'Faixa_2'
                            else if (x >= 38 & x < 53) x = 'Faixa_3'
                            else x = 'Faixa_4'
                          })

# Kaplan-Meier por faixa de idade
ekm_ex5_age <- survfit(Surv(survivaltime, dead)~ Faixa_idade,data = data_Hodgkins)

ggsurvplot(ekm_ex5_age, data = data_Hodgkins,conf.int = F,
           ggtheme=theme_gray(), legend.labs = c("Faixa 1", "Faixa 2", 
                                                 "Faixa 3", "Faixa 4")) + 
  labs(x="Tempo (em meses)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

# teste log-rank

surv_pvalue(ekm_ex5_age, method = c("1"))[,1:3]

# teste Tarone-Ware
surv_pvalue(ekm_ex5_age, method = c("sqrtN"))[,1:3]

# item c

# Para histologia
ekm_ex5_hist <- survfit(Surv(survivaltime, dead)~ hist,data = data_Hodgkins)

# Grafico Kaplan-Meier
ggsurvplot(ekm_ex5_hist, data = data_Hodgkins,conf.int = F,
           ggtheme=theme_gray(), legend.labs = c("Esclerose nodular",
                                                 "Misto celular","Depleção de linfócitos")) + 
  labs(x="Tempo (em meses)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

# teste log-rank
surv_pvalue(ekm_ex5_hist, method = c("1"))[,1:3]

# teste Tarone-Ware
surv_pvalue(ekm_ex5_hist, method = c("sqrtN"))[,1:3]

# Para doença
ekm_ex5_est_doe <- survfit(Surv(survivaltime, dead)~ stage,data = data_Hodgkins)

# Grafico Kaplan-Meier
ggsurvplot(ekm_ex5_est_doe, data = data_Hodgkins,conf.int = F,
           ggtheme=theme_gray(), legend.labs = c("Inicial","Avançado")) + 
  labs(x="Tempo (em meses)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

# teste log-rank
surv_pvalue(ekm_ex5_est_doe, method = c("1"))[,1:3]

# teste Tarone-Ware
surv_pvalue(ekm_ex5_est_doe, method = c("sqrtN"))[,1:3]

# Exercício 6

# Carregando dados
data_PS <- asaur::pharmacoSmoking

# item a
ekm_ex6 <- survfit(Surv(ttr, relapse)~ grp,data = data_PS)

# Grafico Kaplan-Meier
ggsurvplot(ekm_ex6, data = data_PS, palette = c('deeppink','blue'),conf.int = T,
           ggtheme=theme_gray(), legend.labs = c("Combination", "PatchOnly")) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

# item b

# teste log-rank
surv_pvalue(ekm_ex6, method = c("1"))[,1:3]

# teste Tarone-Ware
surv_pvalue(ekm_ex6, method = c("sqrtN"))[,1:3]

# teste Familia Fleming-Harrington
surv_pvalue(ekm_ex6, method = c("FH_p=1_q=1"))[,1:3]

# item c

ekm_ex6_2 <- survfit(Surv(ttr, relapse)~ employment,data = data_PS)

ggsurvplot(ekm_ex6_2, data = data_PS, palette = c('deeppink','blue',"green"),conf.int = F,
           ggtheme=theme_gray(), legend.labs = c("ft", "other","pt")) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Estimativas de Kaplan-Meier") 

# teste log-rank
surv_pvalue(ekm_ex6_2, method = c("1"))[,1:3]