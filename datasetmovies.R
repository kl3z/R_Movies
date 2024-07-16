library(forecast)
library(GGally)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(car)
library(caret)
library(ResourceSelection)
library(lattice)
library(plm)
library(lmtest)
library(pROC)
library(DescTools)
library(ROCR)
library(forecast)
library(ConfusionTableR)
library(scatterplot3d)
library(gridExtra)
library(grid)
library(caret)
library(MLmetrics)
library(stargazer)
library(car)
library(ggthemes)
library(dplyr)
library(scatterplot)
library(gplots)

#install.packages("scatterplot3d")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("skimr")
#install.packages("GGally")
#install.packages("grid")
#install.packages("gtable")
#install.packages("pROC")
#install.packages("ROCR")

load("MoviesDB.RData")
View(MoviesDB)

str(MoviesDB)
skimr::skim(MoviesDB)

#vamos então fazer a divisão do nosso subconjunto do dataset tendo em conta o numero do nosso grupo
N <- 8
tamanho_dataset <- 100-N
#vou establecer uma semente para que os resultados sejam sempre os mesmos na criação do nosso dataset
set.seed(42)
subconjunto_movies <- MoviesDB[sample(nrow(MoviesDB), tamanho_dataset),]
subconjunto_movies
skimr::skim(subconjunto_movies)
#vamos explorar então o nosso subconjunto
summary(subconjunto_movies)
str(subconjunto_movies)
subconjunto_movies$runtime_type

#se Popularity maior ou igual a 50 então é popular e é atribuido o valor 1, caso contrário é classificado como
#não popular atribuido o valor 0
subconjunto_movies$popbinom <- ifelse(subconjunto_movies$popularity>=50,"Popular","Não popular")
subconjunto_movies$popbinomnum <- ifelse(subconjunto_movies$popularity>=50,1,0)
ggpairs(subconjunto_movies[2:10], 
        aes(color = subconjunto_movies$popbinom, alpha = 0.5))
#distribuição das observações
a <- table(subconjunto_movies$popbinom)
a <- as.data.frame(a)
colnames(a) <- c("popbinom","Frequências")
a <- tableGrob(a)
grid.arrange(a,nrow = 1, ncol = 1)

proporcoes2 <- table(subconjunto_movies$popbinom)
dados_grafico2 <- data.frame(
  popbinom = names(proporcoes2),
  value = as.numeric(proporcoes2),
  value2= paste(round(prop.table(as.numeric(proporcoes2)),4)*100,"%\n" , proporcoes2)
)
ggplot(dados_grafico2, aes(x = "", y = value, fill = popbinom)) +
  geom_col(color = "black") + geom_text(aes(label = value2), vjust = -0.1, color="White",
                                        position = position_stack(vjust = 0.5)) +labs(title = "Gráfico circular da variável dependente popbinom" 
                                                                                  )+ coord_polar(theta = "y") + theme_void()+theme(plot.title = element_text(hjust = 0.5, colour = "Dark blue"), 
                                                                                                                                                      plot.subtitle = element_text(hjust = 0.5, color = "Blue"))

#As duas variáveis que parecem fazer mais sentido utilizar são a variável vote_count e revenue,
#isto porque conceptualmente parecem as que fazem mais sentido, são inclusive as que têm maior 
#coeficiente de correlação com a nossa variável alvo

ggpairs(subconjunto_movies,columns = c(3,4,7), aes(color = subconjunto_movies$popbinom, alpha = 0.5))

#Vamos entao construir o nosso modelo de acordo com as variáveis escolhidas
MRL <- glm(subconjunto_movies$popbinomnum ~ subconjunto_movies$vote_count+subconjunto_movies$revenue, family=binomial)
MRL
#Equaçao do MRLog: y^= -7.360201 + 0.003411VC + 0.001874R
#π = 1/( 1+exp(- (-7.360201 + 0.003411VC + 0.002426R))
#que pode ser simplificado como π = 1/( 1+exp(7.360201 - 0.003411VC - 0.001874R)) 
        
#contudo Variáveis altamente correlacionadas entre si podem causar problemas de multicolinearidade,
# que podem afetar negativamente a estabilidade e a interpretação dos coeficientes do modelo.

vif(MRL)

# os nossos valores do vif são relativamente baixos (aproximadamente 1.08 para ambos)
#o que indica que não apresenta problemas significativos de multicolinearidade entre as variáveis 
#vote_count e reveneu no nosso modelo.
#Dando a entender assim que as variáveis vote_count e reveneu não são altamente colineares.

#vamos agora interpretar os coeficientes das nossas variáveis

exp(coefficients(MRL))
#Vamos então agora fazer a interpretação dos nossos coeficientes do modelo
#ß0 - intercepção - Na interpretação do nosso modelo da função logística podemos afirmar que o nosso ß0, é -7.681072, 
#e o valor da Exp(ß0^) é de aproximadamente 0.0005 que representa o valor do logaritmo quando as variáveis “vote_count” e “revenue” são 0

# A exponencial dos restantes coeficientes, 1.0034170262 (VC), 1.0018753134 (R)
#ß1^ - têm um valor de 1.0034170262 que significa O que significa que a cada aumento de um voto em “vote_count” 
#a probabilidade de ser classificado como popular aumenta aproximadamente 1.003, o que traduz um aumento 
#de probabilidade das chances de sucesso de aproximadamente 0.3%.

#ß2^ - tem um valor 1.0018753134 O que se traduz em ao aumentar um milhão de euros  em “revenue”, e a variável “vote_count” 
#se mantiver inalterada as chances de ser classificado como popular são iguais a 1.002 das chances originais, 
#ou seja, aumentam 0.2%.

#TH de Wald:

#H0: beta_i = 0
#H1: beta_i != 0
summary(MRL)

#para ß0 p-value = 0.000104 < alfa rejeitamos H0 pois existe evidência estatistica para afirmar que a probabilidade
#de o filme ser classificado como popular quando as “revenue”, e  “vote_count” são 0 é extremamente baixo
#para vote_count p-value = 0.000638 < alfa rejeitamos H0 poisexiste evidência estatistica para afirmar que um aumento
#no numero de votos está associado a um aumento significativo na probabilidade de um filme ser classificado como popular
#para revenue p-value = 0.604455 > alfa Não rejeitamos H0 pois existe evidência estatistica para afirmar que a variável 
#revenue não têm um efeito significativo na probabilidade de um filme ser classifciado como popular.

#comparação modelo Nulo
MRLNulo = glm(subconjunto_movies$popbinomnum ~ 1, family = binomial)
summary(MRLNulo)
1/( 1+exp(-  (-1.3460) ) )
#Equação: pi^ = 1/( 1+exp(-  (-1.3460) ) ) = 0.2065251

#TH ao rácio de verosimilhanças

# H0: beta_i = 0, as variáveis preditoras “revenue” e “vote_count”,não melhoram o ajuste em comparação do modelo nulo   
# H1: beta_i != 0, as variáveis preditoras “revenue” e “vote_count”,não melhoram o ajuste em comparação do modelo nulo 
alfa=0.05
lrtest(MRL, MRLNulo)

#como p-value = 3.763e-15 < alfa logo Rejeita-se H0, i.e.
# existe ev. est. para concluir que algum parâmetro é não nulo, e 
# portanto, o Modelo Logístico obtido é preferível ao modelo nulo.

summary(MRL)$deviance
summary(MRL)$null.deviance

c <- cbind(round(summary(MRL)$deviance,4),round(summary(MRL)$null.deviance,4))
c
colnames(c) <- c("Residual deviance","Null deviance")
c <- tableGrob(c)
grid.arrange(c,nrow = 1, ncol = 1)
# Null deviance: 93.713
# Residual deviance: 27.286
# O Modelo que considera os dois preditores é preferível ao modelo nulo, pois apresenta uma
# "deviance" muito mais baixa.
#Teste de Hosmer e Lemeshow
#H0 : O modelo ajusta-se aos dados
#H1 : O modelo n˜ao se ajusta aos dados
alfa=0.05
hoslem.test(subconjunto_movies$popbinomnum, MRL$fitted.values, g=10)

#como p-value = 0.9422> alpha(usual) => Não rejeitar H0, i.e.
# não havendo "provas" em contrário, assume-se que o MRL ajusta-se
# bem aos dados observados.

#vamos então agora calcular o nosso pseudo-R2

PseudoR2(MRL, c("CoxSnell", "Nagelkerke", "McFadden"))

# Pseudo-R2 Cox-Snell: 0.5142376
# Pseudo-R2 de Nagelkerke: 0.8048691
# Pseudo-R2 de McFadden: 0.7088348 

PseudoR2(MRLNulo, c("CoxSnell", "Nagelkerke", "McFadden"))

# Pseudo-R2 Cox-Snell: 0
# Pseudo-R2 de Nagelkerke: 0
# Pseudo-R2 de McFadden: 0

MRL1=glm(subconjunto_movies$popbinomnum ~ subconjunto_movies$vote_count, family = binomial)
PseudoR2(MRL1, c("CoxSnell", "Nagelkerke", "McFadden"))

# Pseudo-R2 Cox-Snell: 0.5127392
# Pseudo-R2 de Nagelkerke: 0.8025240
# Pseudo-R2 de McFadden: 0.7058114

MRL2=glm(subconjunto_movies$popbinomnum ~ subconjunto_movies$revenue, family = binomial)
PseudoR2(MRL2, c("CoxSnell", "Nagelkerke", "McFadden"))

# Pseudo-R2 Cox-Snell: 0.3303158
# Pseudo-R2 de Nagelkerke: 0.5170003
# Pseudo-R2 de McFadden: 0.3936185

d1 <- c(round(PseudoR2(MRL, c("CoxSnell", "Nagelkerke", "McFadden"))[1],4),
       round(PseudoR2(MRL, c("CoxSnell", "Nagelkerke", "McFadden"))[2],4),
       round(PseudoR2(MRL, c("CoxSnell", "Nagelkerke", "McFadden"))[3],4))
d2 <- c(round(PseudoR2(MRL1, c("CoxSnell", "Nagelkerke", "McFadden"))[1],4),
        round(PseudoR2(MRL1, c("CoxSnell", "Nagelkerke", "McFadden"))[2],4),
        round(PseudoR2(MRL1, c("CoxSnell", "Nagelkerke", "McFadden"))[3],4))
d3 <- c(round(PseudoR2(MRL2, c("CoxSnell", "Nagelkerke", "McFadden"))[1],4),
       round(PseudoR2(MRL2, c("CoxSnell", "Nagelkerke", "McFadden"))[2],4),
       round(PseudoR2(MRL2, c("CoxSnell", "Nagelkerke", "McFadden"))[3],4))
d4 <- c(round(PseudoR2(MRLNulo, c("CoxSnell", "Nagelkerke", "McFadden"))[1],4),
        round(PseudoR2(MRLNulo, c("CoxSnell", "Nagelkerke", "McFadden"))[2],4),
        round(PseudoR2(MRLNulo, c("CoxSnell", "Nagelkerke", "McFadden"))[3],4))

d <- rbind(d1,d2,d3,d4)
colnames(d) <- c("CoxSnell", "Nagelkerke", "McFadden")
rownames(d) <- c("MRL com 2 preditores", "MRL com o preditore vote count", "MRL com o preditore revenue", "MRL sem preditores")
d <- tableGrob(d)
grid.arrange(d,nrow = 1, ncol = 1)

# O Modelo logístico que considera os dois preditores apresenta melhores Pseudo-R2.

#curva de ROC
roccurve = roc(subconjunto_movies$popbinomnum ~ MRL$fitted)
plot(roccurve, lwd = 2, xlab = "", ylab = "")
auc(roccurve)
# Area under the curve: 0.9805 => O Modelo tem excelentes capacidades classificadoras.

which.min(abs(roccurve$sensitivities-roccurve$specificities))
roccurve$thresholds[68]
roccurve$sensitivities[68]
roccurve$specificities[68]
par(bty = "n")

# capacidades classificadoras.
# O ponto de corte ótimo é: pc = 0.1511254
# o que origina uma sensibilidade de 89.47% e uma especificidade de 89.04%.


#método de yuoden - método  que maximiza a precisão/acurácia

coords(roccurve, "best", ret = "threshold")

j <- roccurve$sensitivities + roccurve$specificities - 1
j_index <- which.max(j)
j_index
roccurve$thresholds[j_index]
roccurve$sens[j_index]
roccurve$spec[j_index]

# capacidades classificadoras.
# O ponto de corte ótimo é: pc = 0.07967575
# o que origina uma sensibilidade de 86.30% e uma especificidade de 100%.

#Método da intercepção das retas da sensibilidade e especificidade

diff_sens_spec <- roccurve$sensitivities - roccurve$specificities

crossings <- which(diff_sens_spec[-length(diff_sens_spec)] * diff_sens_spec[-1] < 0)

if (length(crossings) > 0) {
  crossing <- crossings[1]

  x1 <- roccurve$thresholds[crossing]
  x2 <- roccurve$thresholds[crossing + 1]
  y1 <- diff_sens_spec[crossing]
  y2 <- diff_sens_spec[crossing + 1]

  intersection <- x1 - y1 * (x2 - x1) / (y2 - y1)
  
  plot(roccurve$thresholds, roccurve$sensitivities, col = "green", type = "l",
       xlab = "Ponto de Corte", ylab = "")
  lines(roccurve$thresholds, roccurve$specificities, col = "blue")
  legend(x = 0.8, y = 0.9,
         legend = c("Sensibilidade", "Especificidade"),
         col = c("green", "blue"),
         lty = c(1, 1))
  points(intersection, roccurve$sensitivities[which.min(abs(roccurve$thresholds - intersection))], col = "red", pch = 19)
  text(intersection, roccurve$sensitivities[which.min(abs(roccurve$thresholds - intersection))],
       labels = paste("Interseção: ", round(intersection, 4)), pos = 4, col = "red")
  print(paste("Ponto de interseção:", intersection))
} else {
  print("Não foi encontrado ponto de interseção.")
}

which(roccurve$thresholds == intersection)
#como não existe vamos ver o mais póximo que no nosso caso corresponde com o anterior 
# como era expetável

# capacidades classificadoras.
# O ponto de corte ótimo é: pc = 0.160164881519047
# o que origina uma sensibilidade de 89% e uma especificidade de 89.5%.


# método do ponto mais proximo do canto superior esquerdo mais afastado da diagonal

distancia <- sqrt((1 - sens)^2 + (1 - spec)^2)
distancia
which.min(distancia)
roccurve$thresholds[72]
roccurve$sensitivities[72]
roccurve$specificities[72]

# capacidades classificadoras.
# O ponto de corte ótimo é: pc = 0.3283568
# o que origina uma sensibilidade de 89.47% e uma especificidade de 94.52%.


# gráfico com os 4 pontos

plot(roccurve, lwd = 2, xlab = "", ylab = "", ylim=c(0,1.1), xlim=c(1.11,0))
title(main = "Curva ROC",line = 2.5, col.main = "blue")
mtext("Especificidade", side = 1, line = 3, col = "blue") 
mtext("Sensibilidade", side = 2, line = 3, col = "blue")
points(roccurve$specificities[68], roccurve$sensitivities[68], col = "red", pch = 19)
text(roccurve$specificities[68]+0.20, roccurve$sensitivities[68]+0.07, 
     labels = paste("(PC:", round(roccurve$thresholds[68], 2), "Sens:", round(roccurve$sensitivities[68], 2), "Espe:", round(roccurve$specificities[68], 2),")"), 
     col = "red", pos = 1, offset = 1)
points(roccurve$specificities[72], roccurve$sensitivities[72], col = "darkgreen", pch = 19)
text(roccurve$specificities[72]+0.21, roccurve$sensitivities[72]+0.02, 
     labels = paste("(PC:", round(roccurve$thresholds[72], 2), "Sens:", round(roccurve$sensitivities[72], 2), "Espe:", round(roccurve$specificities[72], 2),")"), 
     col = "darkgreen", pos = 1, offset = 1)
points(roccurve$specificities[64], roccurve$sensitivities[64], col = "darkorange", pch = 19)
text(roccurve$specificities[j_index]+0.2, roccurve$sensitivities[j_index]+0.04, 
     labels = paste("(PC:", round(roccurve$thresholds[j_index], 2), "Sens:", round(roccurve$sensitivities[j_index], 2), "Espe:", round(roccurve$specificities[72], 2),")"), 
     col = "darkorange", pos = 1, offset = 1)
polygon(c(0, roccurve$specificities, 0), c(0,roccurve$sensitivities,1), col = rgb(0, 0, 1, 0.2), border = NA)
text(0.7, .6, labels = paste("AUC =", round(auc(roccurve), 3)), col = "darkblue")
legend(x = 0.05, y = 0.3,
       legend = c("1º Critério", "2º Critério", "3º Critério"),
       col = c("red","darkgreen","darkorange"),
       lty = c(1, 1))


#matriz de confusão para o pc- 0.1511254

pc = 0.1511254

probpop = MRL$fitted.values
probpop
Classificação = ifelse( probpop >= pc, 1, 0 ) 
Classificação
table(subconjunto_movies$popbinomnum, Classificação)
cm2 <- confusionMatrix(factor(Classificação), factor(subconjunto_movies$popbinomnum ))
cm2
TP <- cm2$table[2,2]
TN <- cm2$table[1,1]
FN <- cm2$table[1,2]
FP <- cm2$table[2,1]

TP1 <- TP
TN1 <- TN 
FN1 <- FN
FP1 <- FP
draw_confusion_matrix <- function(cm2) {
  
  total <- sum(cm2$table)
  total
  res <- as.numeric(cm2$table)
  res
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
    layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Matriz de confusão com pc = 0.1511254', cex.main=2)
  classes = c("Não Popular", "Popular")
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Previsto', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  text(195, 400, res[1], cex=1.6, font=2, col='Black')
  text(195, 335, res[2], cex=1.6, font=2, col='Black')
  text(295, 400, res[3], cex=1.6, font=2, col='Black')
  text(295, 335, res[4], cex=1.6, font=2, col='Black')
  cm2$byClass[1]
  
  plot(c(100, 0), c(100,0), type = "n", xlab="", ylab="", main = "DETALHES", xaxt='n', yaxt='n')
  text(5, 85, "Sensibilidade", cex=2, font=4)
  text(5, 60, round(TP/(TP+FN), 3), cex=4)
  text(25, 85, "Especificidade", cex=2, font=2)
  text(25, 60, round(TN/(TN+FP), 3), cex=4)
  text(42, 85, "Precisão", cex=2, font=2)
  text(42, 60, round(TP/(TP+FP), 3), cex=4)
  text(59, 85, "Accuracy", cex=2, font=2)
  text(59, 60, round((TN+TP)/(TP+TN+FN+FP), 3), cex=4)
  text(77, 85, "F1 Score", cex=2, font=2)
  text(77, 60, round(2*(((TP/(TP+FP))*(TP/(TP+FN)))/((TP/(TP+FP))+(TP/(TP+FN)))), 3), cex=4)
  text(95, 85, "Kappa", cex=2, font=2)
  text(95, 60, round((2*(TP*TN-FN*FP)/((TP+FP)*(FP+TN)+(TP+FN)*(FN+TN))), 3), cex=4)
}

draw_confusion_matrix(cm2)


#matrix de confusão com o PC = 0.160164881519047 - método da intercepção das retas

pc <- 0.160164881519047
probpop = MRL$fitted.values
probpop
Classificação = ifelse( probpop >= pc, 1, 0 ) 
Classificação
table(subconjunto_movies$popbinomnum, Classificação)
cm2 <- confusionMatrix(factor(Classificação), factor(subconjunto_movies$popbinomnum ))
cm2
TP <- cm2$table[2,2]
TN <- cm2$table[1,1]
FN <- cm2$table[1,2]
FP <- cm2$table[2,1]
draw_confusion_matrix <- function(cm2) {
  
  total <- sum(cm2$table)
  total
  res <- as.numeric(cm2$table)
  res
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Matriz de confusão com pc = 0.160164881519047', cex.main=2)
  classes = c("Não Popular", "Popular")
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Previsto', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  text(195, 400, res[1], cex=1.6, font=2, col='Black')
  text(195, 335, res[2], cex=1.6, font=2, col='Black')
  text(295, 400, res[3], cex=1.6, font=2, col='Black')
  text(295, 335, res[4], cex=1.6, font=2, col='Black')
  cm2$byClass[1]
  
  plot(c(100, 0), c(100,0), type = "n", xlab="", ylab="", main = "DETALHES", xaxt='n', yaxt='n')
  text(5, 85, "Sensibilidade", cex=2, font=4)
  text(5, 60, round(TP/(TP+FN), 3), cex=4)
  text(25, 85, "Especificidade", cex=2, font=2)
  text(25, 60, round(TN/(TN+FP), 3), cex=4)
  text(42, 85, "Precisão", cex=2, font=2)
  text(42, 60, round(TP/(TP+FP), 3), cex=4)
  text(59, 85, "Accuracy", cex=2, font=2)
  text(59, 60, round((TN+TP)/(TP+TN+FN+FP), 3), cex=4)
  text(77, 85, "F1 Score", cex=2, font=2)
  text(77, 60, round(2*(((TP/(TP+FP))*(TP/(TP+FN)))/((TP/(TP+FP))+(TP/(TP+FN)))), 3), cex=4)
  text(95, 85, "Kappa", cex=2, font=2)
  text(95, 60, round((2*(TP*TN-FN*FP)/((TP+FP)*(FP+TN)+(TP+FN)*(FN+TN))), 3), cex=4)
}

draw_confusion_matrix(cm2)

#matrix de confusão com o PC = 0.07967575 - método de youden
set.seed(42)
pc <- 0.07967575
probpop = MRL$fitted.values
probpop
Classificação = ifelse( probpop >= pc, 1, 0 ) 
Classificação
table(subconjunto_movies$popbinomnum, Classificação)
cm2 <- confusionMatrix(factor(Classificação), factor(subconjunto_movies$popbinomnum ))
cm2
TP <- cm2$table[2,2]
TN <- cm2$table[1,1]
FN <- cm2$table[1,2]
FP <- cm2$table[2,1]
TP2 <- TP
TN2 <- TN 
FN2 <- FN
FP2 <- FP
draw_confusion_matrix <- function(cm2) {
  
  total <- sum(cm2$table)
  total
  res <- as.numeric(cm2$table)
  res
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Matriz de confusão com pc = 0.07967575', cex.main=2)
  classes = c("Não Popular", "Popular")
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Previsto', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  text(195, 400, res[1], cex=1.6, font=2, col='Black')
  text(195, 335, res[2], cex=1.6, font=2, col='Black')
  text(295, 400, res[3], cex=1.6, font=2, col='Black')
  text(295, 335, res[4], cex=1.6, font=2, col='Black')
  cm2$byClass[1]
  
  plot(c(100, 0), c(100,0), type = "n", xlab="", ylab="", main = "DETALHES", xaxt='n', yaxt='n')
  text(5, 85, "Sensibilidade", cex=2, font=4)
  text(5, 60, round(TP/(TP+FN), 3), cex=4)
  text(25, 85, "Especificidade", cex=2, font=2)
  text(25, 60, round(TN/(TN+FP), 3), cex=4)
  text(42, 85, "Precisão", cex=2, font=2)
  text(42, 60, round(TP/(TP+FP), 3), cex=4)
  text(59, 85, "Accuracy", cex=2, font=2)
  text(59, 60, round((TN+TP)/(TP+TN+FN+FP), 3), cex=4)
  text(77, 85, "F1 Score", cex=2, font=2)
  text(77, 60, round(2*(((TP/(TP+FP))*(TP/(TP+FN)))/((TP/(TP+FP))+(TP/(TP+FN)))), 3), cex=4)
  text(95, 85, "Kappa", cex=2, font=2)
  text(95, 60, round((2*(TP*TN-FN*FP)/((TP+FP)*(FP+TN)+(TP+FN)*(FN+TN))), 3), cex=4)
}

draw_confusion_matrix(cm2)

# MC para a menor distancia para o canto superior esquerdo e distancia da diagonal
pc = 0.3283568
probpop = MRL$fitted.values
probpop
Classificação = ifelse( probpop >= pc, 1, 0 ) 
Classificação
table(subconjunto_movies$popbinomnum, Classificação)
cm2 <- confusionMatrix(factor(Classificação), factor(subconjunto_movies$popbinomnum ))
cm2
TP <- cm2$table[2,2]
TN <- cm2$table[1,1]
FN <- cm2$table[1,2]
FP <- cm2$table[2,1]
TP3 <- TP
TN3 <- TN 
FN3 <- FN
FP3 <- FP
draw_confusion_matrix <- function(cm2) {
  
  total <- sum(cm2$table)
  total
  res <- as.numeric(cm2$table)
  res
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Matriz de confusão com pc = 0.3283568', cex.main=2)
  classes = c("Não Popular", "Popular")
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Previsto', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  text(195, 400, res[1], cex=1.6, font=2, col='Black')
  text(195, 335, res[2], cex=1.6, font=2, col='Black')
  text(295, 400, res[3], cex=1.6, font=2, col='Black')
  text(295, 335, res[4], cex=1.6, font=2, col='Black')
  cm2$byClass[1]
  
  plot(c(100, 0), c(100,0), type = "n", xlab="", ylab="", main = "DETALHES", xaxt='n', yaxt='n')
  text(5, 85, "Sensibilidade", cex=2, font=4)
  text(5, 60, round(TP/(TP+FN), 3), cex=4)
  text(25, 85, "Especificidade", cex=2, font=2)
  text(25, 60, round(TN/(TN+FP), 3), cex=4)
  text(42, 85, "Precisão", cex=2, font=2)
  text(42, 60, round(TP/(TP+FP), 3), cex=4)
  text(59, 85, "Accuracy", cex=2, font=2)
  text(59, 60, round((TN+TP)/(TP+TN+FN+FP), 3), cex=4)
  text(77, 85, "F1 Score", cex=2, font=2)
  text(77, 60, round(2*(((TP/(TP+FP))*(TP/(TP+FN)))/((TP/(TP+FP))+(TP/(TP+FN)))), 3), cex=4)
  text(95, 85, "Kappa", cex=2, font=2)
  text(95, 60, round((2*(TP*TN-FN*FP)/((TP+FP)*(FP+TN)+(TP+FN)*(FN+TN))), 3), cex=4)
}

draw_confusion_matrix(cm2)

#tabela comparativa

d <- rbind(round(TP1/(TP1+FN1), 3),round(TN1/(TN1+FP1), 3),
           round(TP1/(TP1+FP1), 3),
           round((TN1+TP1)/(TP1+TN1+FN1+FP1), 3),
           round(2*(((TP1/(TP1+FP1))*(TP1/(TP1+FN1)))/((TP1/(TP1+FP1))+(TP1/(TP1+FN1)))), 3),
           round((2*(TP1*TN1-FN1*FP1)/((TP1+FP1)*(FP1+TN1)+(TP1+FN1)*(FN1+TN1))), 3))
d1 <- rbind(round(TP2/(TP2+FN2), 3),round(TN2/(TN2+FP2), 3),
           round(TP2/(TP2+FP2), 3),
           round((TN2+TP2)/(TP2+TN2+FN2+FP2), 3),
           round(2*(((TP2/(TP2+FP2))*(TP2/(TP2+FN2)))/((TP2/(TP2+FP2))+(TP2/(TP2+FN2)))), 3),
           round((2*(TP2*TN2-FN2*FP2)/((TP2+FP2)*(FP2+TN2)+(TP2+FN2)*(FN2+TN2))), 3))
d2 <- rbind(round(TP3/(TP3+FN3), 3),round(TN3/(TN3+FP3), 3),
            round(TP3/(TP3+FP3), 3),
            round((TN3+TP3)/(TP3+TN3+FN3+FP3), 3),
            round(2*(((TP3/(TP3+FP3))*(TP3/(TP3+FN3)))/((TP3/(TP3+FP3))+(TP3/(TP3+FN3)))), 3),
            round((2*(TP3*TN3-FN3*FP3)/((TP3+FP3)*(FP3+TN3)+(TP3+FN3)*(FN3+TN3))), 3))

d <- cbind(d,d1,d2)
colnames(d) <- c("PC=0.1511254", "PC=0.07967575 ", "PC=0.3283568")
rownames(d) <- c("Sensibilidade", "Especificidade", "Precisão", "Accuracy", "F1 Score", "Kappa")
d <- tableGrob(d)
grid.arrange(d,nrow = 1, ncol = 1)

