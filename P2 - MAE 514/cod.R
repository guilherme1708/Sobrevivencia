# Prova 2 - MAE0514

# local de trabalho 
setwd("~/Área de Trabalho/P2 - MAE 514")

# Pacotes
library(ggplot2)
library(survival)
library(gridExtra)
library(survminer)
library(KMsurv)
library(cmprsk)
library(Amelia)

# Leitura dos dados
data <- read.csv("Dados-eind3-prova2.csv",header = T)

# Removendo a primeira coluna (desnecessária)
data$id <- NULL

# transformando estadiamento em fator
data$grade <- as.factor(data$grade)

attach(data)

# item a
missmap(data,col = c('red','black'),main = "Gráfico 1: Heatmap missing")

# categorização das variáveis

# summary(age)
# summary(nodes)
# summary(size)
data$age_cat <- as.factor(findInterval(age,c(quantile(age)[2],quantile(age)[4])))
data$nodes_cat <- as.factor(findInterval(nodes,c(quantile(nodes)[3],quantile(nodes)[4])))
data$size_cat <- as.factor(findInterval(size,c(quantile(size)[2],quantile(size)[4])))

# Taxa de óbitos
ggplot(data, aes(x=factor(censdead),fill=factor(censdead)))+ 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent,breaks = c(seq(0,0.75,0.05))) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette="Set1") +
  labs(y = "Pacientes (%)",
       x="Óbito",
       title="Gráfico 2: Taxa de óbtos")

# Óbito vs. Estadiamento
ggplot(data, aes(x= grade,  group=factor(censdead))) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Pacientes (%)", 
       x="Estadiamento",
       fill=c("Estadiamento"),
       title="Gráfico 3: Óbito vs. Estadiamento") +
  facet_grid(~factor(censdead)) +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.79))

# Histograma idade  vs. óbito
ggplot(data, aes(x=age,  color=factor(censdead),fill=factor(censdead))) + 
  geom_histogram(color="black", fill="white")+
  facet_grid(factor(censdead) ~ .) + 
  labs(x="Idade (em anos)",
       y="# de pacientes",
       title="Gráfico 4: Histograma Idade vs. óbito")

# Histograma Número de linfonodos comprometidos vs. óbito
ggplot(data, aes(x=nodes,  color=factor(censdead),fill=factor(censdead))) + 
  geom_histogram(color="black", fill="white")+
  facet_grid(factor(censdead) ~ .) + 
  labs(x="Número de linfonodos comprometidos",
       y="# de pacientes",
       title="Gráfico 5: Número de linfonodos comprometidos vs. óbito")

# Histograma tamanho do tumor vs. óbito
ggplot(data, aes(x=size,  color=factor(censdead),fill=factor(censdead))) + 
  geom_histogram(color="black", fill="white")+
  facet_grid(factor(censdead) ~ .) + 
  labs(x="Tamanho do tumor (em mm)",
       y="# de pacientes",
       title="Gráfico 6: Histograma tamanho do tumor vs. óbito")

ekm_meno <- survfit(Surv(survtime, censdead)~ menopause,data = data)

# Grafico Kaplan-Meier Menopausa
ggsurvplot(ekm_meno, data = data,conf.int = T,palette="Set1",pval = T,risk.table = T,
           ggtheme=theme_gray(),
           legend.labs=c("Não","Sim")) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Gráfico 7: Estimativas de Kaplan-Meier (Menopausa)") 

knitr::kable(surv_pvalue(ekm_meno,data, method = c("1"))[,1:3],digits = 3)

ekm_horm <- survfit(Surv(survtime, censdead)~ hormone,data = data)

# Grafico Kaplan-Meier Hormonio
ggsurvplot(ekm_horm, data = data,conf.int = T,palette="Set1",pval = T,risk.table = T,
           ggtheme=theme_gray(),
           legend.labs=c("Não","Sim")) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Gráfico 8: Estimativas de Kaplan-Meier (Terapia Hormonal)") 

knitr::kable(surv_pvalue(ekm_horm,data, method = c("1"))[,1:3],digits = 3)

ekm_est <- survfit(Surv(survtime, censdead)~ grade,data = data)

# Grafico Kaplan-Meier Estadiamento
ggsurvplot(ekm_est, data = data,conf.int = T,palette="Set1",pval = T,risk.table = T,
           ggtheme=theme_gray(),
           legend.labs=c("1","2","3")) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Gráfico 9: Estimativas de Kaplan-Meier (Estadiamento)") 

knitr::kable(surv_pvalue(ekm_est,data, method = c("1"))[,1:3],digits = 3)

ekm_age_cat <- survfit(Surv(survtime, censdead)~age_cat,data = data)

# Grafico Kaplan-Meier Idade categorizada
ggsurvplot(ekm_age_cat, data = data,conf.int = T,palette="Set1",pval = T,risk.table = T,
           ggtheme=theme_gray(),
           legend.labs=c("< 46","de 46 a 61",">= 61")) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Gráfico 10: Estimativas de Kaplan-Meier (Idade)") 

knitr::kable(surv_pvalue(ekm_age_cat,data, method = c("1"))[,1:3],digits = 3)

ekm_nodes_cat <- survfit(Surv(survtime, censdead)~nodes_cat,data = data)

# Grafico Kaplan-Meier numero de Linfonodos
ggsurvplot(ekm_nodes_cat, data = data,conf.int = T,palette="Set1",pval = T,risk.table = T,
           ggtheme=theme_gray(),
           legend.labs=c("< 3","de 3 a 7",">= 7")) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Gráfico 11: Estimativas de Kaplan-Meier (Linfonodos)") 

knitr::kable(surv_pvalue(ekm_nodes_cat,data, method = c("1"))[,1:3],digits = 3)

ekm_size_cat <- survfit(Surv(survtime, censdead)~size_cat,data = data)

# Grafico Kaplan-Meier tamanho tumor
ggsurvplot(ekm_size_cat, data = data,conf.int = T,palette="Set1",pval = T,risk.table = T,
           ggtheme=theme_gray(),
           legend.labs=c("< 20","de 20 a 35",">= 35")) + 
  labs(x="Tempo (em dias)",
       y=expression(hat(S(t))),
       title = "Gráfico 12: Estimativas de Kaplan-Meier (Tamanho do tumor)") 

knitr::kable(surv_pvalue(ekm_size_cat,data, method = c("1"))[,1:3],digits = 4)

# item b

# removendo as variáveis categorizadas
data$age_cat <- NULL
data$nodes_cat <- NULL
data$size_cat <- NULL

# modeo inicial
mod.ini <- coxph(Surv(survtime, censdead)~.,ties="breslow",data = data)
summary(mod.ini)

knitr::kable(drop1(mod.ini, test="Chisq"),digits = 3)

#mod1 <- coxph(Surv(survtime, censdead)~menopause+hormone+size+grade+nodes,data=data)
#drop1(mod1, test="Chisq")
#mod2 <- coxph(Surv(survtime, censdead)~hormone+size+grade+nodes,data=data)
#drop1(mod2, test="Chisq")
#mod3 <- coxph(Surv(survtime, censdead)~size+grade+nodes,data=data)
#drop1(mod3, test="Chisq")

mod.fim <- coxph(Surv(survtime, censdead)~size + grade + nodes,ties="breslow",
                 data = data)
summary(mod.fim)

# item d

# Nelson-Aalen 
H_0 <- basehaz(mod.fim,centered=F)
S_0 <- exp(-H_0$hazard)

# indivíduo 1
ind1 <- c(53,1,0,15,0,0,1)

# perfil 1
p1 <- S_0^(exp(ind1[1]*mod.ini$coefficients[1]+ind1[2]*mod.ini$coefficients[2]+
              ind1[3]*mod.ini$coefficients[3]+ind1[4]*mod.ini$coefficients[4]+
              ind1[5]*mod.ini$coefficients[5]+ind1[6]*mod.ini$coefficients[6]+
              ind1[7]*mod.ini$coefficients[7]))

# indivíduo 2
ind2 <- c(53,1,0,30,0,0,1)

# perfil 2
p2 <- S_0^(exp(ind2[1]*mod.ini$coefficients[1]+ind2[2]*mod.ini$coefficients[2]+
              ind2[3]*mod.ini$coefficients[3]+ind2[4]*mod.ini$coefficients[4]+
              ind2[5]*mod.ini$coefficients[5]+ind2[6]*mod.ini$coefficients[6]+
              ind2[7]*mod.ini$coefficients[7]))

# indivíduo 3
ind3 <- c(53,1,0,30,0,0,5)

# perfil 3
p3 <- S_0^(exp(ind3[1]*mod.ini$coefficients[1]+ind3[2]*mod.ini$coefficients[2]+
              ind3[3]*mod.ini$coefficients[3]+ind3[4]*mod.ini$coefficients[4]+
              ind3[5]*mod.ini$coefficients[5]+ind3[6]*mod.ini$coefficients[6]+
              ind3[7]*mod.ini$coefficients[7]))

# indivíduo 4
ind4 <- c(53,1,0,30,1,0,5)

# perfil 4
p4 <- S_0^(exp(ind4[1]*mod.ini$coefficients[1]+ind4[2]*mod.ini$coefficients[2]+
              ind4[3]*mod.ini$coefficients[3]+ind4[4]*mod.ini$coefficients[4]+
              ind4[5]*mod.ini$coefficients[5]+ind4[6]*mod.ini$coefficients[6]+
              ind4[7]*mod.ini$coefficients[7]))

# indivíduo 5
ind5 <- c(53,1,0,30,0,1,5)

# perfil 5
p5 <- S_0^(exp(ind5[1]*mod.ini$coefficients[1]+ind5[2]*mod.ini$coefficients[2]+
              ind5[3]*mod.ini$coefficients[3]+ind5[4]*mod.ini$coefficients[4]+
              ind5[5]*mod.ini$coefficients[5]+ind5[6]*mod.ini$coefficients[6]+
              ind5[7]*mod.ini$coefficients[7]))

pf_time <- data.frame(time=H_0$time,p1,p2,p3,p4,p5)

col <- c('Perfil 1' ='red','Perfil 2' ='black','Perfil 3' ='blue','Perfil 4' ='green','Perfil 5' ='brown')

pt1 <- ggplot(pf_time,aes(x=time)) +
  geom_step(aes(y=p1,colour="Perfil 1")) +
  geom_step(aes(y=p2,colour="Perfil 2")) +
  geom_step(aes(y=p3,colour="Perfil 3")) +
  geom_step(aes(y=p4,colour="Perfil 4")) +
  geom_step(aes(y=p5,colour="Perfil 5")) + 
  scale_colour_manual(name="Perfil",values=col) + 
  labs(x="Tempo (dias)",
       y=expression(hat(S(t))),
       title="Gráfico 13: Função de sobrevivência")

# para função de taxa de falha acumulda

p1 <- S_0*(exp(ind1[1]*mod.ini$coefficients[1]+ind1[2]*mod.ini$coefficients[2]+
              ind1[3]*mod.ini$coefficients[3]+ind1[4]*mod.ini$coefficients[4]+
              ind1[5]*mod.ini$coefficients[5]+ind1[6]*mod.ini$coefficients[6]+
              ind1[7]*mod.ini$coefficients[7]))


p2 <- S_0*(exp(ind2[1]*mod.ini$coefficients[1]+ind2[2]*mod.ini$coefficients[2]+
              ind2[3]*mod.ini$coefficients[3]+ind2[4]*mod.ini$coefficients[4]+
              ind2[5]*mod.ini$coefficients[5]+ind2[6]*mod.ini$coefficients[6]+
              ind2[7]*mod.ini$coefficients[7]))


p3 <- S_0*(exp(ind3[1]*mod.ini$coefficients[1]+ind3[2]*mod.ini$coefficients[2]+
              ind3[3]*mod.ini$coefficients[3]+ind3[4]*mod.ini$coefficients[4]+
              ind3[5]*mod.ini$coefficients[5]+ind3[6]*mod.ini$coefficients[6]+
              ind3[7]*mod.ini$coefficients[7]))


p4 <- S_0*(exp(ind4[1]*mod.ini$coefficients[1]+ind4[2]*mod.ini$coefficients[2]+
              ind4[3]*mod.ini$coefficients[3]+ind4[4]*mod.ini$coefficients[4]+
              ind4[5]*mod.ini$coefficients[5]+ind4[6]*mod.ini$coefficients[6]+
              ind4[7]*mod.ini$coefficients[7]))


p5 <- S_0*(exp(ind5[1]*mod.ini$coefficients[1]+ind5[2]*mod.ini$coefficients[2]+
              ind5[3]*mod.ini$coefficients[3]+ind5[4]*mod.ini$coefficients[4]+
              ind5[5]*mod.ini$coefficients[5]+ind5[6]*mod.ini$coefficients[6]+
              ind5[7]*mod.ini$coefficients[7]))

pf_time2 <- data.frame(time=H_0$time,cum1=cumsum(p1),cum2=cumsum(p2),cum3=cumsum(p3),cum4=cumsum(p4),cum5=cumsum(p5))

pt2 <- ggplot(pf_time2,aes(x=time)) +
  geom_step(aes(y=cum1,colour="Perfil 1")) +
  geom_step(aes(y=cum2,colour="Perfil 2")) +
  geom_step(aes(y=cum3,colour="Perfil 3")) +
  geom_step(aes(y=cum4,colour="Perfil 4")) +
  geom_step(aes(y=cum5,colour="Perfil 5")) + 
  scale_colour_manual(name="Perfil",values=col) + 
  labs(x="Tempo (dias)",
       y=expression(hat(A(t))),
       title="Gráfico 14: Função de taxa de falha acumulada")

grid.arrange(pt1, pt2, ncol=2,nrow=1)

# item e

# Resíduo deviance
dev <- data.frame(pred_lin=mod.fim$linear.predictors,
                  res_dev=resid(mod.fim,type='deviance'))

ggplot(dev, aes(x=pred_lin,y=res_dev))+
         geom_point() +
         scale_y_continuous(breaks = seq(-2,3,1)) +
         annotate(geom = "point", x = dev[56,1], y = dev[56,2], colour='red',
                  size = 2) +
         geom_abline(slope = 0) +
         labs(x="Preditor linear",
              y="Residuo deviance",
              title = "Gráfico 13: Resíduo Deviance x preditor linear") 

# Resíduos de cox-snell
data$resid_mart <- residuals(mod.fim, type = "martingale")
data$resid_coxsnell <- -(data$resid_mart - data$censdead)

# Modelo com os Resíduos de cox-snell 
fit_coxsnell <- coxph(formula = Surv(resid_coxsnell, censdead) ~ 1,data = data)

# Nelson-Aalen 
df_base_haz <- basehaz(fit_coxsnell, centered = FALSE)

ggplot(data = df_base_haz, mapping = aes(x = time, y = hazard)) +
  geom_step() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Resíduos de Cox-Snell",
       y = expression(hat(A)(r[es])),
       title = "Resíduos de Cox-Snell ")

# Resíduo martingal
mod.0 <- coxph(Surv(survtime, censdead)~ 1)
data$resid_mart <- resid(mod.0)

# martingal tamanho do tumor
plot1 <- ggplot(data = data, mapping = aes(x = size, y = resid_mart)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Tamanho do tumor",
       y = "Resíduo martingal",
       title = "Resíduo Martingal")

# martingal do estadiamento
plot2 <- ggplot(data = data, mapping = aes(x = grade, y = resid_mart)) +
  geom_boxplot() +
  labs(x = "Esatiamento do tumor",
       y = "Resíduo Martingal",
       title = "Gráfico xx: Resíduo Martingal x Esatiamento do tumor")

# martingal numero de linfonodos
plot3 <- ggplot(data = data, mapping = aes(x = nodes, y = resid_mart)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Número de linfonodos comprometidos",
       y = "Resíduo Martingal",
       title = "Gráfico xx: Resíduo Martingal x número de linfonodos")

# martingal numero de linfonodos trsnformado em log
plot4 <- ggplot(data = data, mapping = aes(x = log(nodes), y = resid_mart)) +
  geom_point() +
  geom_smooth() +
  labs(x = "log do Número de linfonodos comprometidos",
       y = "Resíduo Martingal",
       title = "Gráfico xx: Resíduo Martingal x log do número de linfonodos")

grid.arrange(plot1, plot2, ncol=2,nrow=1)
grid.arrange(plot3, plot4, ncol=2,nrow=1)

# Resíduo escore
res_escore <- data.frame(resid(mod.fim,type='dfbeta'),
                         index=seq(1,dim(data)[1]))

p1 <- ggplot(data = res_escore, mapping = aes(x = index, y = ind1)) +
  geom_point() +
  annotate(geom = "point", x = c(392,78),
           y = c(1.160555e-03,-0.0016507054), colour='red',size = 2) +
  labs(x = "Index",
       y = "Influencia para o tamanho do tumor",
       title = "Gráfico xx:Resíduo Escore x índices")

p2 <- ggplot(data = res_escore, mapping = aes(x = index, y = ind4)) +
  geom_point() +
  annotate(geom = "point", x = c(684,638),
           y = c(-0.0059678128,-0.0028668434), colour='red',size = 2) +
  labs(x = "Index",
       y = "Influencia para número de linfonodos",
       title = "Gráfico xx: Resíduo Escore x índices")

p3 <- ggplot(data = res_escore, mapping = aes(x = index, y = ind3)) +
  geom_point() +
  annotate(geom = "point", x = c(439,79,131,474,68,155),
           y = c(-0.161467776,-0.159754032,-0.159516346,
                 -0.157515569,-0.150190617,-0.146015240),
           colour='red',size = 2) +
  labs(x = "Index",
       y = "Influencia para o estadiamento=3",
       title = "Gráfico xx:Resíduo Escore x índices")

p4 <- ggplot(data = res_escore, mapping = aes(x = index, y = ind2)) +
  geom_point() +
  annotate(geom = "point", x = c(439,79,131,474,68,155),
           y = c(-0.161467776,-0.159754032,-0.159516346,
                 -0.157515569,-0.150190617,-0.146015240),
           colour='red',size = 2) +
  labs(x = "Index",
       y = "Influencia para o estadiamento=2",
       title = "Gráfico xx:Resíduo Escore x índices")

grid.arrange(p1, p2, ncol=2,nrow=1)

grid.arrange(p3, p4, ncol=2,nrow=1)

# Resíduo de Schoenfeld
res_scho <- cox.zph(mod.fim)

# gráfico dos resíduos
plot(res_scho)

# teste de hipotese rp
knitr::kable(res_scho$table,digits=3) 