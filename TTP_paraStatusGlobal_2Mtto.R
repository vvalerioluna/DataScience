#Programa para TTP


setwd("C:/Users/USER/OneDrive - Anheuser-Busch InBev/TTP/") #esta es la ruta de trabajo de acuerdo a donde estan las bds
# "C:\Users\USER\OneDrive - Anheuser-Busch InBev\TTP\10Book1R.xlsx" Esta es la dirección original de Windows, pero debe modificarse la "\" por "/" para que R la pueda leer

rm(list = ls()) #Borrar todos lo dataframe del entorno
### librerias -----
options(scipen = 999999999) #convertir valores de notación cientifica a valores enteros
#Para trabajar con los datos
library(tidyr)
library(readxl)
library(dplyr)
library(tidyverse)

#para graficos
library(ggplot2)
library(ggpubr)
library(rpart.plot)

library(GGally)#grafico de correlacion mejorado
library(gridExtra)
library(skimr)
library(PerformanceAnalytics) 
library(psych)
library(tseries) 
library(vars)

#Para arboles decision
library(tree)
library(randomForest)
library(rpart)  # Esta es la libreria fue la que finalmente se uso
library(caret)
library(lattice)
library(rattle)
#Otros paquetes
library(tidymodels)
library(DataExplorer)
library(univariateML)

library(doParallel)
library(modelgrid)
library(recipes)
library(rsample)
library(parsnip)
library(workflows)
library(lubridate)
library(lpSolve)
library(easystats)

#archivos----
Base <- read_excel("C:/Users/USER/OneDrive - Anheuser-Busch InBev/TTP/10Book1R2.xlsx", 
                   sheet = "10Book1R2")

Base$Fecha <- as.Date(Base$Fecha)

Base <- Base %>% rename(
  "TTP_Cal"="TTP Cal","FTE_Tur"="FTE*Tur")


# Modelo 1: Base con toda la muestra####


#Condicion importante las variables tienen que se mayor o igual a cero y no debe haber texto
BaseTTP <- Base %>% select(SKAP,AVCOT,
                           #SKU, se retira SKU con base a la sesión con Olga y Javier Huertas Alvarado
                           Fail,Lim,Mtto,
                           #Vol,
                           MTTR,MTBF,
                           TTP_Cal,
                           #Hrs,
                           "FTE_Tur")


#Vistas de Correlograma
Correlograma <- cor(BaseTTP2) #Matriz de valores del correlograma
#Vista 1
corrplot(Correlograma,method="pie",type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 100)


library(PerformanceAnalytics)

chart.Correlation(BaseTTP, histogram = TRUE, method = "pearson")
library(psych)
#Correlograma de todas la variables en forma de matriz (con colores)
pairs.panels(BaseTTP,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes

#Indentificar si existen predictores altamente correlacionados(sin colores, no en PPT)
plot_correlation(
  data = BaseTTP,
  type = "continuous",
  title = "Matriz de correlación variables continuas",
  theme_config = list(legend.position = "none",
                      plot.title = element_text(size = 16, face = "bold"),
                      axis.title = element_blank(),
                      axis.text.x = element_text(angle = -45, hjust = +0.1)
  )
)

#Vista 2 correlograma (Los pasteles, no en PPT)
ggpairs(BaseTTP,#Data frame
        lower = list(continuous = "smooth"),#Panel inferior
        diag = list(continuous = "barDiag"), axisLabels = "none" #Diagonal
) 

# Modelo Arbol TOTAL (diap 16)
arb <- rpart(TTP_Cal ~., 
             data=BaseTTP #base de datos
             , method="anova" #comparar las varianzas entre las medias y ver efecto medible sobre una variable dependiente.
)
#Plot del arbol
prp(arb, main="",
    nn=F, #mostrar el numero de nodos
    fallen.leaves = T,#colocar las hojas en la parte inferior
    branch.lty = 3 ,#dibujar ramas con lineas punteadas
    split.prefix = "es ",  # poner "x" antes del texto
    split.suffix = "?",    # poner "?" despues del texto
    split.box.col = "lightblue",   # color de la caja
    split.border.col = "darkgray", # color del borde
    split.round = 0.5             # redondeo de esquinas
)
rpart.rules(arb, style = "tall")#condiciones del árbol
summary(arb)
#Con base al arbol se busca cual varaible es mas importante
importance <- arb$variable.importance  
importance <- round(100*importance/sum(importance), 1)
importance[importance >= 1]#aqui se puede ver de mayor a menor importancia 


#Modelo de regresion lineal 
lin <- lm(TTP_Cal ~ ., data=BaseTTP)
summary(lin)

#simulacion lin vs arboles
#segmentacion de Bds
data <- initial_split(BaseTTP,strata = TTP_Cal, prop = 0.75)#se divide la base 75 u 80%
BaseTTP_train <- training(data)#base de entrenamiento
BaseTTP_test  <- testing(data)#base prueba

lin2 <- lm(TTP_Cal ~ ., data=BaseTTP_train)
arb2 <- rpart(TTP_Cal ~., 
              data=BaseTTP_train #base de datos
)
pred_lin2 <- predict(object=lin2, newdata=BaseTTP_test)
pred_arb2 <- predict(object=arb2, newdata=BaseTTP_test)
cor(BaseTTP_test$TTP_Cal, pred_lin2)#correlacion modelo lineal
cor(BaseTTP_test$TTP_Cal, pred_arb2)#correlacion modelo arbol

ecm1 <- mean((BaseTTP_test$TTP_Cal - pred_lin2)^2)
ecm2 <- mean((BaseTTP_test$TTP_Cal - pred_arb2)^2)
result <- rbind(ecm1,ecm2)

par(mfrow=c(1, 2))
plot(x=pred_lin2, y=BaseTTP_test$TTP_Cal, main="ML")
abline(a=0, b=1, lty="dashed", col="blue")
plot(x=pred_arb2, y=BaseTTP_test$TTP_Cal, main="Arbol")
abline(a=0, b=1, lty="dashed", col="blue")
summary(lin2)
summary(arb2)

cor(BaseTTP2$TTP_Cal, pred_lin2)#correlacion modelo Random Forest
cor(BaseTTP2$TTP_Cal, pred_arb2)#correlacion modelo arbol

par(mfrow=c(1, 1))

prp(arb2, main="",
    nn=F, #mostrar el numero de nodos
    fallen.leaves = T,#colocar las hojas en la parte inferior
    branch.lty = 3 ,#dibujar ramas con lineas punteadas
    split.prefix = "es ",  # poner "x" antes del texto
    split.suffix = "?",    # poner "?" despues del texto
    split.box.col = "lightblue",   # color de la caja
    split.border.col = "darkgray", # color del borde
    split.round = 0.5             # redondeo de esquinas
)

rpart.rules(arb2, style = "tall")#condiciones del árbol
summary(arb2)


##Arima (diapositiva 21)
#data2 <- initial_split(Base,strata = TTP, prop = 0.75)
#Base_train <- training(data2)
#Base_test  <- testing(data2)
Base_trainAr <- Base[(1:861),c(-1,-3,-11,-13,-14,-16,-17,-18,-19)]
Base_testAr <- Base[(862:1152),c(-1,-3,-11,-13,-14,-16,-17,-18,-19)]

Base %>%
  group_by(Fecha) %>%
  summarize(TTP_Cal) %>%
  ggplot(aes(Fecha, TTP_Cal)) + geom_line(col =  "#8dc8e8") + geom_smooth(method ="lm", se=FALSE, col = "#00263e") +
  labs(title = "Evolución y Tendencia", x= "Fecha", y="Valor")

library(tseries)
adf.test(Base_trainAr$TTP_Cal, k = 1)#la serie debe ser  estacionaria
#rechazamos la hipótesis nula, por lo que se trata de una serie estacionaria.
-20.624==0.01

ggplot(Base_trainAr, aes(x = Fecha, y = TTP_Cal)) + geom_line(col = "#8dc8e8") +
  geom_smooth(method = "lm", se=FALSE, col = "#00263e") + 
  labs(title = "Evolución y tendencia TTP", x="", y = "Cantidad de TTP")

library(vars)
#Se calcula el vector autoregresivo: cada variable es una función lineal de sus propios valores pasados y el resto de valores pasados de otras variables
x <- Base_trainAr[, colnames(Base_trainAr) %in% c("TTP_Cal","SKAP","AVCOT","Fail","Lim","Mtto","MTTR","MTBF","FTE_Tur")]

VARselect(x, lag.max = 10, type = "both")  # cuatro citerios: Akaike Information Criteria, Hannan-Quinn, Schwartz y Akaike’s Final Predicton Error.    
var = VAR(x, p=10)#se usara el criterio AIC, puesto que la varianza de los errores que produce suelen ser menores
summary(var)

prediccion <- predict(var, 
                      n.ahead = 291, #El número de pasos por delante para los que se requiere prediccion
                      ci = 0.95, 
                      dumvar = NULL)


y_MARIMA <- prediccion$fcst$TTP_Cal [,1]
y <- cbind(Base_testAr, prediccion = y_MARIMA)

cols <- c("Prediccion"="#00263e","Real"="#8dc8e8")
ggplot(y, aes( x = Fecha)) + 
  geom_line(aes(y=prediccion, col = "Prediccion"), size=2) + 
  geom_line(aes(y=TTP_Cal, col = "Real"), size=2) +
  labs(title = "TTP_Cal reales vs TTP_Cal Estimado", x = "") +
  scale_colour_manual(name="",values=cols) +
  theme(legend.position = "bottom")
#RMSE (error cuadratico) calculado manualmente
y %>%
  mutate(ER = (TTP_Cal-prediccion)^2) %>%
  summarize(RMSE = sqrt(sum(ER)/291))      


###Random Forest(diapositiva 20, 22)

BaseTTP_train2 <- Base_trainAr[,c(-1)]
BaseTTP_test2 <- Base_testAr[,c(-1)]
skim(BaseTTP_train2)

#Out-Of-Bag Error (OOB) es un método para medir el error de predicción de bosques aleatorios
tuneRF(x = BaseTTP_train2[,-which(names(BaseTTP_train2) == "TTP_Cal")],#variables predictoras
       y = BaseTTP_train2$TTP_Cal,#variable a predecir
       tepFactor = 1.5, #iteraciones
       improve = 1e-5,#La mejora (relativa) en el error OOB
       ntreeTry = 1000)#numero de arboles
#el menor valor es mtry = 

rf <- randomForest(TTP_Cal ~., data=BaseTTP_train2, 
                   ntree=500, mtry=4)

importancia_pred <- rf$importance %>%
  enframe(name = "predictor", value = "importancia")

ggplot(
  data = importancia_pred,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

## Al quitar FTE, vemos que variable sería la más importante (diapositiva 20)
BaseTTP_train3 <- BaseTTP_train2[,c(-9)]

rf3 <- randomForest(TTP_Cal ~., data=BaseTTP_train3, 
                   ntree=500, mtry=4)

importancia_pred3 <- rf3$importance %>%
  enframe(name = "predictor", value = "importancia")

ggplot(
  data = importancia_pred3,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
##

test_predictors <- BaseTTP_test2[,-which(names(BaseTTP_test2) == "TTP_Cal")]
prediccion <- predict(rf, test_predictors)

y_rf <- prediccion
#despues de entrenar el modelo y predecir el valor para los próximos datos, analizaremos cómo se ha comportado el modelo, mediante un RMSE

test <- cbind(BaseTTP_test2, prediccion)
test %>%
  mutate(ER = (TTP_Cal-prediccion)^2) %>%
  summarize(RMSE = sqrt(sum(ER)/291)) #y muestra un X%


resultados <- as.data.frame(cbind(MARIMA = y_MARIMA,  Random_Forest = y_rf, Valor = BaseTTP_test2$TTP_Cal))
resultados$Fecha <- Base$Fecha[862:1152]

cols <- c("MARIMA"="#40a3d8","Random Forest"="#1e6b95", "Valores" = "#00263e")

ggplot(resultados, aes(x = Fecha)) +
  geom_line(aes(y = MARIMA, col = "ARIMA"), size = 1) + 
  geom_line(aes(y = Random_Forest, col = "Random Forest"), size = 1) +
  geom_line(aes(y = Valor, col = "Valores"), size =1, alpha = 0.8) +
  labs(title = "Comparación del Ajuste de los Modelos", x = "", y="TTP_Cal")+
  scale_colour_manual(name="",values=cols) +
  theme(legend.position = "bottom") 
#resultados del pronostico
#write.csv(resultados,"resultados.csv",row.names=F)

#Predicciones
pred_rf <- predict(object=rf, newdata=Base)
pred_arb <- predict(object=arb, newdata=Base)
#pred_arima <- predict(object=var, newdata=Base)

par(mfrow=c(1, 2)) #diapositiva 22
plot(x=pred_rf, y=BaseTTP$TTP_Cal, main="Random Forest")
abline(a=0, b=1, lty="dashed", col="blue")
plot(x=pred_arb, y=BaseTTP$TTP_Cal, main="Arbol")
abline(a=0, b=1, lty="dashed", col="blue")

cor(BaseTTP$TTP_Cal, pred_rf)#correlacion modelo Random Forest
cor(BaseTTP$TTP_Cal, pred_arb)#correlacion modelo arbol

# Modelo 2: solo Oversold ####

Base2 <- Base %>% filter(Oversold==1)
names(Base2)

#Condicion importante las variables tienen que se mayor o igual a cero y no debe haber texto
BaseTTP2 <- Base2[,c(-1,-2,-3,-11,-13,-14,-16,-17,-18,-19)] #esta base es la que se necesita para la correlacion


#Vistas de Correlograma
Correlograma2 <- cor(BaseTTP2)#Matriz de valores del correlograma
#Vista 1
corrplot(Correlograma2,method="pie",type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 100)

library(psych)
#Correlograma de todas la variables en forma de matriz (con colores)
pairs.panels(BaseTTP2,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes
#@@@Aqui vamos


# Modelo Arbol TOTAL
arbm2 <- rpart(TTP_Cal ~., 
             data=BaseTTP2 #base de datos
             , method="anova" #comparar las varianzas entre las medias y ver efecto medible sobre una variable dependiente.
)
#Plot del arbol
prp(arbm2, main="",
    nn=F, #mostrar el numero de nodos
    fallen.leaves = T,#colocar las hojas en la parte inferior
    branch.lty = 3 ,#dibujar ramas con lineas punteadas
    split.prefix = "es ",  # poner "x" antes del texto
    split.suffix = "?",    # poner "?" despues del texto
    split.box.col = "lightblue",   # color de la caja
    split.border.col = "darkgray", # color del borde
    split.round = 0.5             # redondeo de esquinas
)

rpart.rules(arbm2, style = "tall")#condiciones del árbol
summary(arbm2)
#Con base al arbol se busca cual varaible es mas importante
importancem2 <- arbm2$variable.importance  
importancem2 <- round(100*importancem2/sum(importancem2), 1)
importancem2[importancem2 >= 1]#aqui se puede ver de mayor a menor importancia 

#Modelo de regresion lineal
linm2 <- lm(TTP_Cal ~ ., data=BaseTTP2)
summary(linm2)

#simulacion lin vs arboles
#segmentacion de Bds
datam2 <- initial_split(BaseTTP2,strata = TTP_Cal, prop = 0.75)#se divide la base 75 u 80%
BaseTTP_trainm2 <- training(datam2)#base de entrenamiento
BaseTTP_testm2  <- testing(datam2)#base prueba

lin2m2 <- lm(TTP_Cal ~ ., data=BaseTTP_trainm2)
arb2m2 <- rpart(TTP_Cal ~., 
              data=BaseTTP_trainm2 #base de datos
)

pred_lin2m2 <- predict(object=lin2m2, newdata=BaseTTP_testm2)
pred_arb2m2 <- predict(object=arb2m2, newdata=BaseTTP_testm2)
cor(BaseTTP_testm2$TTP_Cal, pred_lin2m2)#correlacion modelo lineal
cor(BaseTTP_testm2$TTP_Cal, pred_arb2m2)#correlacion modelo arbol

ecm1m2 <- mean((BaseTTP_testm2$TTP_Cal - pred_lin2m2)^2)
ecm2m2 <- mean((BaseTTP_testm2$TTP_Cal - pred_arb2m2)^2)
resultm2 <- rbind(ecm1m2,ecm2m2)


par(mfrow=c(1, 2))
plot(x=pred_lin2m2, y=BaseTTP_testm2$TTP_Cal, main="ML")
abline(a=0, b=1, lty="dashed", col="blue")
plot(x=pred_arb2m2, y=BaseTTP_testm2$TTP_Cal, main="Arbol")
abline(a=0, b=1, lty="dashed", col="blue")
summary(lin2m2)
summary(arb2m2)

##Arima

Base_trainArm2 <- Base2[(1:96),c(-1,-3,-11,-13,-14,-16,-17,-18,-19)]
Base_testArm2 <- Base2[(97:130),c(-1,-3,-11,-13,-14,-16,-17,-18,-19)]
library(tseries)
adf.test(Base_trainArm2$TTP_Cal, k = 1)

library(vars)
xm2 <- Base_trainArm2[, colnames(Base_trainArm2) %in% c("TTP_Cal","SKAP","AVCOT","Fail","Lim","Mtto","MTTR","MTBF","FTE_Tur")]

VARselect(xm2, lag.max = 10, type = "both")  # cuatro citerios: Akaike Information Criteria, Hannan-Quinn, Schwartz y Akaike’s Final Predicton Error.    
varm2 = VAR(xm2, p=10)#se usara el criterio AIC, puesto que la varianza de los errores que produce suelen ser menores
summary(varm2)


prediccionm2 <- predict(varm2, 
                      n.ahead = 34, #El número de pasos por delante para los que se requiere prediccion
                      ci = 0.95, 
                      dumvar = NULL)

y_MARIMAm2 <- prediccionm2$fcst$TTP_Cal [,1]
ym2 <- cbind(Base_testArm2, prediccionm2 = y_MARIMAm2)


ym2 %>%
  mutate(ER = (TTP_Cal-prediccionm2)^2) %>%
  summarize(RMSE = sqrt(sum(ER)/34)) 


###Random Forest

BaseTTP_train2m2 <- Base_trainArm2[,c(-1)]
BaseTTP_test2m2 <- Base_testArm2[,c(-1)]

#Out-Of-Bag Error (OOB) es un método para medir el error de predicción de bosques aleatorios
tuneRF(x = BaseTTP_train2m2[,-which(names(BaseTTP_train2m2) == "TTP_Cal")],#variables predictoras
       y = BaseTTP_train2m2$TTP_Cal,#variable a predecir
       tepFactor = 1.5, #iteraciones
       improve = 1e-5,#La mejora (relativa) en el error OOB
       ntreeTry = 1000)#numero de arboles

rfm2 <- randomForest(TTP_Cal ~., data=BaseTTP_train2m2, 
                   ntree=500, mtry=4)

importancia_predm2 <- rfm2$importance %>%
  enframe(name = "predictor", value = "importancia")


ggplot(
  data = importancia_predm2,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")


## Al quitar FTE, vemos que variable sería la más importante
BaseTTP_train3m2 <- BaseTTP_train2m2[,c(-9)]

tuneRF(x = BaseTTP_train3m2[,-which(names(BaseTTP_train3m2) == "TTP_Cal")],#variables predictoras
       y = BaseTTP_train3m2$TTP_Cal,#variable a predecir
       tepFactor = 1.5, #iteraciones
       improve = 1e-5,#La mejora (relativa) en el error OOB
       ntreeTry = 1000)#numero de arboles

rf3m2 <- randomForest(TTP_Cal ~., data=BaseTTP_train3m2, 
                    ntree=500, mtry=2)

importancia_pred3m2 <- rf3m2$importance %>%
  enframe(name = "predictor", value = "importancia")


ggplot(
  data = importancia_pred3m2,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
##

test_predictorsm2 <- BaseTTP_test2m2[,-which(names(BaseTTP_test2m2) == "TTP_Cal")]
prediccionm2 <- predict(rfm2, test_predictorsm2)
y_rfm2 <- prediccionm2

testm2 <- cbind(BaseTTP_test2m2, prediccionm2)
testm2 %>%
  mutate(ER = (TTP_Cal-prediccionm2)^2) %>%
  summarize(RMSE = sqrt(sum(ER)/34)) #y muestra un X%

resultadosm2 <- as.data.frame(cbind(MARIMA = y_MARIMAm2,  Random_Forest = y_rfm2, Valor = BaseTTP_test2m2$TTP_Cal))
resultadosm2$Fecha <- Base2$Fecha[97:130]


ggplot(resultadosm2, aes(x = Fecha)) +
  geom_line(aes(y = MARIMA, col = "ARIMA"), size = 1) + 
  geom_line(aes(y = Random_Forest, col = "Random Forest"), size = 1) +
  geom_line(aes(y = Valor, col = "Valores"), size =1, alpha = 0.8) +
  labs(title = "Comparación del Ajuste de los Modelos", x = "", y="TTP_Cal")+
  scale_colour_manual(name="",values=cols) +
  theme(legend.position = "bottom") 


#Predicciones
pred_rfm2 <- predict(object=rfm2, newdata=Base2)
pred_arbm2 <- predict(object=arb2m2, newdata=Base2)

par(mfrow=c(1, 2))
plot(x=pred_rfm2, y=BaseTTP2$TTP_Cal, main="Random Forest")
abline(a=0, b=1, lty="dashed", col="blue")
plot(x=pred_arbm2, y=BaseTTP2$TTP_Cal, main="Arbol")
abline(a=0, b=1, lty="dashed", col="blue")

cor(BaseTTP2$TTP_Cal, pred_rfm2)#correlacion modelo Random Forest
cor(BaseTTP2$TTP_Cal, pred_arbm2)#correlacion modelo arbol



#### Modelo 4: Mtto solo oversold::::::-----


Base2 <- Base %>% filter(Cluster=="Bottle C1" & Oversold==1 )
names(Base2)

#Condicion importante las variables tienen que se mayor o igual a cero y no debe haber texto
BaseTTP2 <- Base2[,c(-1,-2,-3,-4,-11,-13,-14,-16,-17,-18,-19)] #esta base es la que se necesita para la correlacion
BaseTTP2 <- BaseTTP2[ ,c(-9)]
#BaseTTP2 <- BaseTTP2 %>% select(Mtto,Fail,AVCOT,MTBF,MTTR,Lim,TTP_Cal,FTE_Tur)
BaseTTP2 <- BaseTTP2[,c(4,2,1,6,5,3,7,8)]
#BaseTTP2 <- BaseTTP2[ ,c(-8)]#eliminar FTE

#library(PerformanceAnalytics) Se utilizo esta libreria porque no me funciono pairs

chart.Correlation(BaseTTP2,ellipses = TRUE,histogram = TRUE, method = "pearson")




pairs(BaseTTP2,                     # Data frame de variables
      labels = colnames(BaseTTP2),  # Nombres de las variables
      pch = 21,                 # Símbolo pch
      lower.panel = panel.smooth,# Curvas de regresión suavizadas
      lm = T,
      bg = rainbow(2),  # Color de fondo del símbolo (pch 21 a 25)
      col = rainbow(2), # Color de borde del símbolo
      main = "Matriz de correlación Mexico Plant",            # Título del gráfico
      row1attop = T,         # Si FALSE, cambia la dirección de la diagonal
      gap = 1,                  # Distancia entre subplots
      cex.labels = NULL,        # Tamaño del texto de la diagonal
      font.labels = 1)          # Estilo de fuente del texto de la diagonal


#Vistas de Correlograma
Correlograma2 <- cor(BaseTTP2)#Matriz de valores del correlograma
#Vista 1
library(corrplot)
corrplot(Correlograma2,method="pie",type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 100)

library(psych)
#Correlograma de todas la variables en forma de matriz (con colores)
pairs.panels(BaseTTP2,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = T,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 2,           # Símbolo pch
             lm = T,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = T,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = F,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = F)          # Si TRUE, añade intervalos de confianza a los ajustes
#@@@Aqui vamos


# Modelo Arbol TOTAL
arbm2 <- rpart(Mtto ~., 
               data=BaseTTP2 #base de datos
               , method="anova" #comparar las varianzas entre las medias y ver efecto medible sobre una variable dependiente.
)
#Plot del arbol
prp(arbm2, main="",
    nn=F, #mostrar el numero de nodos
    fallen.leaves = T,#colocar las hojas en la parte inferior
    branch.lty = 3 ,#dibujar ramas con lineas punteadas
    split.prefix = "es ",  # poner "x" antes del texto
    split.suffix = "?",    # poner "?" despues del texto
    split.box.col = "lightblue",   # color de la caja
    split.border.col = "darkgray", # color del borde
    split.round = 0.5             # redondeo de esquinas
)

rpart.rules(arbm2, style = "tall")#condiciones del árbol
summary(arbm2)
#Con base al arbol se busca cual varaible es mas importante
importancem2 <- arbm2$variable.importance  
importancem2 <- round(100*importancem2/sum(importancem2), 1)
importancem2[importancem2 >= 1]#aqui se puede ver de mayor a menor importancia 

#Modelo de regresion lineal
linm2 <- lm(Mtto ~ ., data=BaseTTP2)
summary(linm2)

#simulacion lin vs arboles
#segmentacion de Bds
datam2 <- initial_split(BaseTTP2,strata = Mtto, prop = 0.75)#se divide la base 75 u 80%
BaseTTP_trainm2 <- training(datam2)#base de entrenamiento
BaseTTP_testm2  <- testing(datam2)#base prueba

lin2m2 <- lm(Mtto ~ ., data=BaseTTP_trainm2)
arb2m2 <- rpart(Mtto ~., 
                data=BaseTTP_trainm2 #base de datos
)

pred_lin2m2 <- predict(object=lin2m2, newdata=BaseTTP_testm2)
pred_arb2m2 <- predict(object=arb2m2, newdata=BaseTTP_testm2)
cor(BaseTTP_testm2$Mtto, pred_lin2m2)#correlacion modelo lineal
cor(BaseTTP_testm2$Mtto, pred_arb2m2)#correlacion modelo arbol

ecm1m2 <- mean((BaseTTP_testm2$Mtto - pred_lin2m2)^2)
ecm2m2 <- mean((BaseTTP_testm2$Mtto - pred_arb2m2)^2)
resultm2 <- rbind(ecm1m2,ecm2m2)


par(mfrow=c(1, 2))
plot(x=pred_lin2m2, y=BaseTTP_testm2$Mtto, main="ML")
abline(a=0, b=1, lty="dashed", col="blue")
plot(x=pred_arb2m2, y=BaseTTP_testm2$Mtto, main="Arbol")
abline(a=0, b=1, lty="dashed", col="blue")
summary(lin2m2)
summary(arb2m2)

##Arima

Base_trainArm2 <- Base2[(1:96),c(-1,-3,-11,-13,-14,-16,-17,-18,-19)]
Base_testArm2 <- Base2[(97:130),c(-1,-3,-11,-13,-14,-16,-17,-18,-19)]
library(tseries)
adf.test(Base_trainArm2$Mtto, k = 1)

library(vars)
xm2 <- Base_trainArm2[, colnames(Base_trainArm2) %in% c("TTP_Cal","AVCOT","Fail","Lim","Mtto","MTTR","MTBF","FTE_Tur")]

VARselect(xm2, lag.max = 10, type = "both")  # cuatro citerios: Akaike Information Criteria, Hannan-Quinn, Schwartz y Akaike’s Final Predicton Error.    
varm2 = VAR(xm2, p=10)#se usara el criterio AIC, puesto que la varianza de los errores que produce suelen ser menores
summary(varm2)


prediccionm2 <- predict(varm2, 
                        n.ahead = 34, #El número de pasos por delante para los que se requiere prediccion
                        ci = 0.95, 
                        dumvar = NULL)

y_MARIMAm2 <- prediccionm2$fcst$Mtto [,1]
ym2 <- cbind(Base_testArm2, prediccionm2 = y_MARIMAm2)


ym2 %>%
  mutate(ER = (Mtto-prediccionm2)^2) %>%
  summarize(RMSE = sqrt(sum(ER)/34)) 


###Random Forest

BaseTTP_train2m2 <- Base_trainArm2[,c(-1,-2,-11)]
BaseTTP_test2m2 <- Base_testArm2[,c(-1,-2,-11)]

#Out-Of-Bag Error (OOB) es un método para medir el error de predicción de bosques aleatorios
tuneRF(x = BaseTTP_train2m2[,-which(names(BaseTTP_train2m2) == "Mtto")],#variables predictoras
       y = BaseTTP_train2m2$Mtto,#variable a predecir
       tepFactor = 1.5, #iteraciones
       improve = 1e-5,#La mejora (relativa) en el error OOB
       ntreeTry = 1000)#numero de arboles

rfm2 <- randomForest(Mtto ~., data=BaseTTP_train2m2, 
                     ntree=500, mtry=2)

importancia_predm2 <- rfm2$importance %>%
  enframe(name = "predictor", value = "importancia")


ggplot(
  data = importancia_predm2,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")


## Al quitar FTE, vemos que variable sería la más importante
BaseTTP_train3m2 <- BaseTTP_train2m2[,c(-8)]

tuneRF(x = BaseTTP_train3m2[,-which(names(BaseTTP_train3m2) == "Mtto")],#variables predictoras
       y = BaseTTP_train3m2$Mtto,#variable a predecir
       tepFactor = 1.5, #iteraciones
       improve = 1e-5,#La mejora (relativa) en el error OOB
       ntreeTry = 1000)#numero de arboles

rf3m2 <- randomForest(Mtto ~., data=BaseTTP_train3m2, 
                      ntree=500, mtry=4)

importancia_pred3m2 <- rf3m2$importance %>%
  enframe(name = "predictor", value = "importancia")


ggplot(
  data = importancia_pred3m2,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
##

test_predictorsm2 <- BaseTTP_test2m2[,-which(names(BaseTTP_test2m2) == "Mtto")]
prediccionm2 <- predict(rfm2, test_predictorsm2)
y_rfm2 <- prediccionm2

testm2 <- cbind(BaseTTP_test2m2, prediccionm2)
testm2 %>%
  mutate(ER = (Mtto-prediccionm2)^2) %>%
  summarize(RMSE = sqrt(sum(ER)/34)) #y muestra un X%

resultadosm2 <- as.data.frame(cbind(MARIMA = y_MARIMAm2,  Random_Forest = y_rfm2, Valor = BaseTTP_test2m2$Mtto))
resultadosm2$Fecha <- Base2$Fecha[97:130]

ggplot(resultadosm2, aes(x = Fecha)) +
  #geom_line(aes(y = MARIMA, col = "ARIMA"), size = 1) + 
  geom_line(aes(y = Random_Forest, col = "Random Forest"), size = 1) +
  geom_line(aes(y = Valor, col = "Valores"), size =1, alpha = 0.8) +
  labs(title = "Comparación del Ajuste de los Modelos", x = "", y="Mtto")+
  scale_colour_manual(name="",values=cols) +
  theme(legend.position = "bottom") 


#Predicciones
pred_rfm2 <- predict(object=rfm2, newdata=Base2)
pred_arbm2 <- predict(object=arb2m2, newdata=Base2)

par(mfrow=c(1, 2))
plot(x=pred_rfm2, y=BaseTTP2$Mtto, main="Random Forest")
abline(a=0, b=1, lty="dashed", col="blue")
plot(x=pred_arbm2, y=BaseTTP2$Mtto, main="Arbol")
abline(a=0, b=1, lty="dashed", col="blue")

cor(BaseTTP2$Mtto, pred_rfm2)#correlacion modelo Random Forest
cor(BaseTTP2$Mtto, pred_arbm2)#correlacion modelo arbol

par(mfrow=c(1, 1))

prp(arb2m2, main="",
    nn=F, #mostrar el numero de nodos
    fallen.leaves = T,#colocar las hojas en la parte inferior
    branch.lty = 3 ,#dibujar ramas con lineas punteadas
    split.prefix = "es ",  # poner "x" antes del texto
    split.suffix = "?",    # poner "?" despues del texto
    split.box.col = "lightblue",   # color de la caja
    split.border.col = "darkgray", # color del borde
    split.round = 0.5             # redondeo de esquinas
)

rpart.rules(arb2m2, style = "tall")#condiciones del árbol
summary(arb2m2)









# Modelo 3: solo Oversold >10% SKAP----

Base3 <- Base %>% filter(SKAP>0.1)
Base3 <- Base3 %>% filter(Oversold==1)
names(Base2)

BaseTTP3 <- Base3[,c(-1,-2,-3,-11,-13,-14,-16,-17,-18,-19)]

#Vistas de Correlograma
Correlograma3 <- cor(BaseTTP3)
#Vista 1
corrplot(Correlograma3,method="pie",type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 100)

pairs.panels(BaseTTP3,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes


# Modelo 2: solo Oversold (Cluster)####

Base2 <- Base %>% filter(Variante==1) #*Variante corresponde a las lineas de un cluster que esta contenido dentro de Oversold
Base2 <- Base %>% filter(Cluster=="Bottle C1")
#Base2 <- Base %>% filter(Cluster=="Bottle C10")
#Base2 <- Base %>% filter(Cluster=="Bottle C3")
#Base2 <- Base %>% filter(Cluster=="Bottle C7")
#Base2 <- Base %>% filter(Cluster=="Bottle C8")
#Base2 <- Base %>% filter(Cluster=="Can C1")
#Base2 <- Base %>% filter(Cluster=="Can C11")
#Base2 <- Base %>% filter(Cluster=="Can C1")
#Base2 <- Base %>% filter(Cluster=="Can C9")
#Base2 <- Base %>% filter(Cluster=="PET C1")

names(Base2) #muestra los nombres del data frame

BaseTTP2 <- Base2[,c(-1,-2,-3,-11,-13,-14,-16,-17,-18,-19,-20)]


#Vistas de Correlograma
Correlograma2 <- cor(BaseTTP2) #Generar la matriz del Correlograma
#Vista 1-Genera un primer grafico de correlacion 
corrplot(Correlograma2,method="pie",type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 100)

library(psych)

pairs.panels(BaseTTP2,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes


# Modelo Arbol TOTAL
arbm2 <- rpart(TTP_Cal ~., 
               data=BaseTTP2 #base de datos
               , method="anova" #comparar las varianzas entre las medias y ver efecto medible sobre una variable dependiente.
)
#Plot del arbol
prp(arbm2, main="",
    nn=F, #mostrar el numero de nodos
    fallen.leaves = T,#colocar las hojas en la parte inferior
    branch.lty = 3 ,#dibujar ramas con lineas punteadas
    split.prefix = "es ",  # poner "x" antes del texto
    split.suffix = "?",    # poner "?" despues del texto
    split.box.col = "lightblue",   # color de la caja
    split.border.col = "darkgray", # color del borde
    split.round = 0.5             # redondeo de esquinas
)

rpart.rules(arbm2, style = "tall")#condiciones del árbol
summary(arbm2)
#Con base al arbol se busca cual varaible es mas importante
importancem2 <- arbm2$variable.importance  
importancem2 <- round(100*importancem2/sum(importancem2), 1)
importancem2[importancem2 >= 1]#aqui se puede ver de mayor a menor importancia 

#Modelo de regresion lineal
linm2 <- lm(TTP_Cal ~ ., data=BaseTTP2)
summary(linm2)

#simulacion lin vs arboles
#segmentacion de Bds
datam2 <- initial_split(BaseTTP2,strata = TTP_Cal, prop = 0.75)#se divide la base 75 u 80%
BaseTTP_trainm2 <- training(datam2)#base de entrenamiento
BaseTTP_testm2  <- testing(datam2)#base prueba

lin2m2 <- lm(Mtto ~ ., data=BaseTTP_trainm2)
arb2m2 <- rpart(Mtto ~., 
                data=BaseTTP_trainm2 #base de datos
)

pred_lin2m2 <- predict(object=lin2m2, newdata=BaseTTP_testm2)
pred_arb2m2 <- predict(object=arb2m2, newdata=BaseTTP_testm2)
cor(BaseTTP_testm2$Mtto, pred_lin2m2)#correlacion modelo lineal
cor(BaseTTP_testm2$Mtto, pred_arb2m2)#correlacion modelo arbol

ecm1m2 <- mean((BaseTTP_testm2$Mtto - pred_lin2m2)^2)
ecm2m2 <- mean((BaseTTP_testm2$Mtto - pred_arb2m2)^2)
resultm2 <- rbind(ecm1m2,ecm2m2)


par(mfrow=c(1, 2))
plot(x=pred_lin2m2, y=BaseTTP_testm2$Mtto, main="ML")
abline(a=0, b=1, lty="dashed", col="blue")
plot(x=pred_arb2m2, y=BaseTTP_testm2$Mtto, main="Arbol")
abline(a=0, b=1, lty="dashed", col="blue")
summary(lin2m2)
summary(arb2m2)

##Arima

Base_trainArm2 <- Base2[(1:52),c(-1,-3,-11,-13,-14,-16,-17,-18,-19,-20)]
Base_testArm2 <- Base2[(53:70),c(-1,-3,-11,-13,-14,-16,-17,-18,-19,-20)]

adf.test(Base_trainArm2$TTP_Cal, k = 1)

xm2 <- Base_trainArm2[, colnames(Base_trainArm2) %in% c("TTP_Cal","SKAP","AVCOT","Fail","Lim","Mtto","MTTR","MTBF","FTE_Tur")]

VARselect(xm2, lag.max = 10, type = "both")  # cuatro citerios: Akaike Information Criteria, Hannan-Quinn, Schwartz y Akaike’s Final Predicton Error.    
varm2 = VAR(xm2, p=5)#se usara el criterio AIC, puesto que la varianza de los errores que produce suelen ser menores
summary(varm2)


prediccionm2 <- predict(varm2, 
                        n.ahead = 19, #El número de pasos por delante para los que se requiere prediccion
                        ci = 0.95, 
                        dumvar = NULL)

y_MARIMAm2 <- prediccionm2$fcst$TTP_Cal [,1]
ym2 <- cbind(Base_testArm2, prediccionm2 = y_MARIMAm2)


ym2 %>%
  mutate(ER = (TTP_Cal-prediccionm2)^2) %>%
  summarize(RMSE = sqrt(sum(ER)/19)) 


###Random Forest

Base_trainArm2 <- Base2[(1:52),c(-1,-3,-11,-13,-14,-16,-17,-18,-19,-20)]
Base_testArm2 <- Base2[(53:70),c(-1,-3,-11,-13,-14,-16,-17,-18,-19,-20)]

BaseTTP_train2m2 <- Base_trainArm2[,c(-1)]
BaseTTP_test2m2 <- Base_testArm2[,c(-1)]

#Out-Of-Bag Error (OOB) es un método para medir el error de predicción de bosques aleatorios
tuneRF(x = BaseTTP_train2m2[,-which(names(BaseTTP_train2m2) == "TTP_Cal")],#variables predictoras
       y = BaseTTP_train2m2$TTP_Cal,#variable a predecir
       tepFactor = 1.5, #iteraciones
       improve = 1e-5,#La mejora (relativa) en el error OOB
       ntreeTry = 1000)#numero de arboles

rfm2 <- randomForest(TTP_Cal ~., data=BaseTTP_train2m2, 
                     ntree=500, mtry=4)

importancia_predm2 <- rfm2$importance %>%
  enframe(name = "predictor", value = "importancia")


ggplot(
  data = importancia_predm2,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")


## Al quitar FTE, vemos que variable sería la más importante
BaseTTP_train3m2 <- BaseTTP_train2m2[,c(-9)]

tuneRF(x = BaseTTP_train3m2[,-which(names(BaseTTP_train3m2) == "TTP_Cal")],#variables predictoras
       y = BaseTTP_train3m2$TTP_Cal,#variable a predecir
       tepFactor = 1.5, #iteraciones
       improve = 1e-5,#La mejora (relativa) en el error OOB
       ntreeTry = 1000)#numero de arboles

rf3m2 <- randomForest(TTP_Cal ~., data=BaseTTP_train3m2, 
                      ntree=500, mtry=4)

importancia_pred3m2 <- rf3m2$importance %>%
  enframe(name = "predictor", value = "importancia")


ggplot(
  data = importancia_pred3m2,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor", title = "Importancia predictores") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
##

test_predictorsm2 <- BaseTTP_test2m2[,-which(names(BaseTTP_test2m2) == "TTP_Cal")]
prediccionm2 <- predict(rfm2, test_predictorsm2)
y_rfm2 <- prediccionm2

testm2 <- cbind(BaseTTP_test2m2, prediccionm2)
testm2 %>%
  mutate(ER = (TTP_Cal-prediccionm2)^2) %>%
  summarize(RMSE = sqrt(sum(ER)/34)) #y muestra un X%

resultadosm2 <- as.data.frame(cbind(MARIMA = y_MARIMAm2,  Random_Forest = y_rfm2, Valor = BaseTTP_test2m2$TTP_Cal))
resultadosm2$Fecha <- Base2$Fecha[51:69]


ggplot(resultadosm2, aes(x = Fecha)) +
  geom_line(aes(y = MARIMA, col = "ARIMA"), size = 1) + 
  geom_line(aes(y = Random_Forest, col = "Random Forest"), size = 1) +
  geom_line(aes(y = Valor, col = "Valores"), size =1, alpha = 0.8) +
  labs(title = "Comparación del Ajuste de los Modelos", x = "", y="TTP_Cal")+
  scale_colour_manual(name="",values=cols) +
  theme(legend.position = "bottom") 


#Predicciones
pred_rfm2 <- predict(object=rfm2, newdata=Base2)
pred_arbm2 <- predict(object=arb2m2, newdata=Base2)

par(mfrow=c(1, 2))
plot(x=pred_rfm2, y=BaseTTP2$Mtto, main="Random Forest")
abline(a=0, b=1, lty="dashed", col="blue")
plot(x=pred_arbm2, y=BaseTTP2$Mtto, main="Arbol")
abline(a=0, b=1, lty="dashed", col="blue")

cor(BaseTTP2$Mtto, pred_rfm2)#correlacion modelo Random Forest
cor(BaseTTP2$Mtto, pred_arbm2)#correlacion modelo arbol

par(mfrow=c(1, 1))

prp(arb2m2, main="",
    nn=F, #mostrar el numero de nodos
    fallen.leaves = T,#colocar las hojas en la parte inferior
    branch.lty = 3 ,#dibujar ramas con lineas punteadas
    split.prefix = "es ",  # poner "x" antes del texto
    split.suffix = "?",    # poner "?" despues del texto
    split.box.col = "lightblue",   # color de la caja
    split.border.col = "darkgray", # color del borde
    split.round = 0.5             # redondeo de esquinas
)

rpart.rules(arb2m2, style = "tall")#condiciones del árbol
summary(arb2m2)


# Modelo 3: solo Oversold >10% SKAP ----

Base3 <- Base %>% filter(SKAP>0.1)
Base3 <- Base3 %>% filter(Oversold==1)
names(Base2)

BaseTTP3 <- Base3[,c(-1,-2,-3,-11,-13,-14,-16,-17,-18,-19)]

#Vistas de Correlograma
Correlograma3 <- cor(BaseTTP3)
#Vista 1
corrplot(Correlograma3,method="pie",type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 100)

pairs.panels(BaseTTP3,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes


#
#Simulacion ----

TTP_AS <- c(
  9.17703475387401,
  6.7588850613947,
  6.7588850613947,
  6.7588850613947,
  4.41219070399388,
  6.71173362003541,
  4.76329406386173,
  5.93607032526633,
  5.75003267400701,
  5.01968877031288,
  9.09596717541621,
  4.76329406386173,
  5.75003267400701,
  8.54670228009271,
  6.56063052087581,
  6.56063052087581,
  6.56063052087581,
  4.32588230885348,
  6.41479762324857,
  4.9232825547875,
  5.90271810001323,
  5.34027211397366,
  5.34027211397366,
  8.49563783623987,
  4.49246195256475,
  4.9232825547875,
  8.60819082394368,
  6.61542337450331,
  6.61542337450331,
  6.61542337450331,
  4.15821223067142,
  6.15248323807306,
  4.6205333081987,
  6.17085074064339,
  5.06669630150588,
  5.06669630150588,
  8.04776511060132,
  4.57827422891826,
  4.6205333081987,
  8.6821827135568,
  6.52560076324199,
  6.52560076324199,
  6.52560076324199,
  4.17343986537056,
  6.40363596271601,
  6.13443565211099,
  4.59872976227134,
  5.07830901432295,
  5.07830901432295,
  7.99702562123542,
  4.51781881881303,
  4.59872976227134,
  2.49637965050705,
  6.58310390814341,
  8.71932382229014,
  6.58310390814341,
  6.58310390814341,
  4.2243082427555,
  5.99610949468638,
  6.4959799331121,
  4.65679294086562,
  7.90454017157823,
  4.8766080546196,
  4.8766080546196,
  4.44893876829409,
  4.65679294086562,
  2.52262118665481,
  8.71766539463912,
  6.63391266733424,
  6.63391266733424,
  6.63391266733424,
  6.51789297274291,
  6.03655415229596,
  4.17739043953012,
  4.61956540944313,
  7.60066211969812,
  4.74324451390391,
  4.74324451390391,
  4.49034997764107,
  4.61956540944313,
  2.52429986045322,
  8.78084701000515,
  6.61796322999356,
  6.61796322999356,
  6.61796322999356,
  4.63032143557387,
  6.56540288719952,
  6.22929485024594,
  4.63032143557387,
  4.06089989120196,
  7.56506816818491,
  4.80599229446293,
  4.80599229446293,
  4.47706426086782,
  2.59620018491992,
  8.69258577851417,
  6.61475254219296,
  6.61475254219296,
  6.61475254219296,
  4.63345633345102,
  6.62083793792414,
  6.20890136168418,
  4.63345633345102,
  3.99049241681368,
  7.58944428682705,
  4.8839544637649,
  4.8839544637649,
  4.4681787143819,
  2.6484379628928)


SKAP_AS <- c(
  0.414737321428571,
  0.408213277777778,
  0.37844946875,
  0.352255737704918,
  0.2888459,
  0.225328068965517,
  0.204629,
  0.131375542857143,
  0.106793027027027,
  0.0445415,
  0.0431061607142857,
  0.0316706571428571,
  0.00375815555555556,
  0.493726107142857,
  0.408213277777778,
  0.377286671875,
  0.350292852459016,
  0.290141833333333,
  0.245510647058824,
  0.204629,
  0.131375542857143,
  0.113810760869565,
  0.109515162162162,
  0.0506058245614035,
  0.0383185483870968,
  0.0316706571428571,
  0.493726107142857,
  0.408213277777778,
  0.377286671875,
  0.350292852459016,
  0.290141833333333,
  0.245510647058824,
  0.204629,
  0.131375542857143,
  0.113810760869565,
  0.109515162162162,
  0.0506058245614035,
  0.0383185483870968,
  0.0316706571428571,
  0.4343499375,
  0.408213277777778,
  0.377286671875,
  0.356131066666667,
  0.290141833333333,
  0.251790694444444,
  0.247560424242424,
  0.204629,
  0.123739710526316,
  0.101437181818182,
  0.0702667142857143,
  0.0354589552238806,
  0.0316706571428571,
  0.0261893666666667,
  0.408213277777778,
  0.397119942857143,
  0.376379203125,
  0.350833916666667,
  0.290141833333333,
  0.257628363636364,
  0.251790694444444,
  0.204629,
  0.139913757281553,
  0.123739710526316,
  0.101437181818182,
  0.0362389552238806,
  0.0316706571428571,
  0.0261893666666667,
  0.408336885714286,
  0.408213277777778,
  0.376379203125,
  0.350833916666667,
  0.302145685714286,
  0.25737676744186,
  0.242499555555556,
  0.204629,
  0.14669559223301,
  0.114740810810811,
  0.101437181818182,
  0.0362389552238806,
  0.0246327333333333,
  0.010719375,
  0.5193,
  0.4402,
  0.3824,
  0.3568,
  0.3261,
  0.3021,
  0.2675,
  0.2032,
  0.1587,
  0.1467,
  0.1237,
  0.1014,
  0.0433,
  0.0262,
  0.519288542857143,
  0.440234555555556,
  0.382353476190476,
  0.356780254237288,
  0.326137322033898,
  0.302145685714286,
  0.267455317073171,
  0.203164444444444,
  0.158731554216867,
  0.14669559223301,
  0.123739710526316,
  0.101437181818182,
  0.0433461515151515,
  0.0261893666666667)
COT_AS <- c(
  11.38666667,
  19.42027778,
  6.708333333,
  9.765,
  5.758333333,
  9.212777778,
  0,
  34.06694444,
  6.915277778,
  25.84777778,
  19.48888889,
  0,
  3.3,
  22.92722222,
  14.47833333,
  5.725277778,
  12.82805556,
  7.416666667,
  7.406666667,
  0,
  21.06361111,
  1,
  3.5,
  14.37944444,
  6.963333333,
  0,
  25.614167,
  17.503333,
  4.785,
  18.119444,
  6.416667,
  10.075833,
  0,
  26.918889,
  3.6358,
  4.7833,
  19.726111,
  0,
  0,
  18.79166667,
  15.82138889,
  1.478333333,
  13.49666667,
  6.294166667,
  24.74722222,
  7.34,
  0,
  3.166666667,
  6.518611111,
  19.43638889,
  0,
  0,
  2.991944444,
  9.641111111,
  24.545,
  3.738888889,
  14.77527778,
  6.810833333,
  6.158888889,
  27.94638889,
  0,
  14.76194444,
  3.591944444,
  1.088333333,
  0,
  0,
  3.825277778,
  16.18555556,
  14.23916667,
  8.0375,
  14.92888889,
  34.04944444,
  10.25277778,
  9.328333333,
  0,
  16.14972222,
  4.013611111,
  7.5975,
  1,
  0,
  2.865277778,
  19.44333333,
  9.129166667,
  6.334444444,
  19.36888889,
  0,
  44.08361111,
  3.716666667,
  0,
  9.632777778,
  12.17,
  0,
  3.8775,
  8.319444444,
  4.2375,
  10.85555556,
  31.15194444,
  6.556111111,
  11.95027778,
  0,
  29.28416667,
  5.378888889,
  0,
  8.997777778,
  11.74694444,
  3.104444444,
  4.668055556,
  6.9,
  2.965)

Fail_AS <- c(
  14.98083333,
  27.41972222,
  10.02361111,
  26.16944444,
  59.40555556,
  7.818611111,
  15.43027778,
  94.80777778,
  54.82,
  26.825,
  11.15888889,
  15.07916667,
  71.07472222,
  30.42722222,
  15.66388889,
  9.8611111,
  25.30138889,
  44.45,
  8.767777778,
  47.78722222,
  61.1875,
  33.43444444,
  28.71555556,
  9.854722222,
  23.60472222,
  17.205,
  14.709444,
  20.639444,
  2.806389,
  32.845833,
  28.4625,
  4.381944,
  51.049167,
  91.336111,
  46.760278,
  41.785278,
  12.384722,
  6.7275,
  30.064444,
  19.175,
  21.27638889,
  6.156944444,
  22.33777778,
  22.34333333,
  74.61722222,
  10.29944444,
  36.42944444,
  45.93444444,
  58.40472222,
  10.60444444,
  1.014444444,
  41.86027778,
  47.98277778,
  18.75,
  31.58166667,
  9.362777778,
  46.33,
  23.66527778,
  5.153055556,
  81.44444444,
  36.65111111,
  9.360833333,
  48.07638889,
  44.86194444,
  9.906944444,
  65.22555556,
  80.66694444,
  16.51944444,
  13.02388889,
  9.374166667,
  35.43722222,
  58.47861111,
  5.720833333,
  37.86083333,
  40.93805556,
  13.23472222,
  50.93694444,
  42.47083333,
  9.831944444,
  21.955,
  38.60666667,
  17.8825,
  24.77555556,
  18.59888889,
  31.45472222,
  33.05194444,
  48.62805556,
  0.750833333,
  42.92138889,
  25.26,
  10.76972222,
  46.54416667,
  40.18944444,
  11.7925,
  35.61277778,
  13.85083333,
  30.75944444,
  17.88111111,
  19.95666667,
  31.68472222,
  61.91833333,
  6.680555556,
  40.90611111,
  29.57194444,
  15.42861111,
  43.35972222,
  54.41444444,
  18.2025,
  34.13055556)

Lim_AS<-c(
  43.47194444,
  10.45833333,
  8.368333333,
  29.90472222,
  25.86666667,
  25.17833333,
  7.421111111,
  61.79833334,
  83.63805556,
  28.57277778,
  38.52083333,
  7.494166667,
  49.327222219,
  29.82722222,
  9.103611111,
  6.468333333,
  24.47194444,
  23.65,
  20.62944444,
  22.91111111,
  46.83944445,
  48.50138889,
  79.60722222,
  42.58333334,
  36.58388889,
  11.86083333,
  20.748889,
  8.2375,
  7.751389,
  32.868056,
  27.458333,
  15.598611,
  20.453889,
  43.363333,
  30.593056,
  85.401667,
  29.810556,
  9.202778,
  13.8225,
  35.26888889,
  6.588333333,
  8.135833333,
  29.92027778,
  30.365,
  34.00277778,
  27.83638889,
  19.297222223,
  111.26083333,
  50.49805556,
  42.8038888899999,
  3.757222222,
  15.78388889,
  30.1652777799999,
  9.803888889,
  32.522222223,
  7.342222222,
  24.36277778,
  24.51666667,
  19.81805556,
  45.73638889,
  22.008333337,
  38.1924999999999,
  90.32027778,
  26.39527778,
  21.5341666699999,
  17.561111113,
  30.4825,
  27.278333334,
  11.22305556,
  9.9275,
  29.64666667,
  51.20916667,
  18.48583333,
  23.3533333299999,
  20.523055554,
  42.28138889,
  81.9475,
  38.14361111,
  27.76416667,
  12.87194444,
  29.7344444459999,
  29.178888888,
  7.671388889,
  12.11555556,
  27.95611111,
  14.45083333,
  45.12111111,
  17.66666667,
  20.4780555599999,
  31.7505555499999,
  30.641666667,
  63.04944445,
  31.429166666,
  15.479722227,
  11.24277778,
  26.53388889,
  10.66083333,
  11.21555556,
  28.68388889,
  13.949166666,
  45.2525,
  17.15833333,
  16.105277777,
  26.6325,
  23.9844444429999,
  70.32388889,
  38.61444444,
  21.9899999969999,
  15.80305556)

Mtto_AS<-c(
  24.15777778,
  21.6175,
  12.29361111,
  32.20305556,
  42.5,
  16,
  4,
  48,
  51.91805556,
  20.81666667,
  32.87361111,
  1.962777778,
  47.41583333,
  30.11694444,
  17.71527778,
  11.92416667,
  26.89944444,
  26.11666667,
  19.84083333,
  8.395277778,
  20.33333333,
  38.8,
  34.99083333,
  30.61055556,
  28.72111111,
  8.87444444,
  25.054722,
  24.5325,
  9.467778,
  43.009444,
  22,
  8,
  8,
  47.916667,
  56.299444,
  58.378333,
  41.945833,
  5,
  9.601944,
  23.87472222,
  19.17944444,
  16.77083333,
  40.14861111,
  28.16666667,
  24,
  16,
  8.596111111,
  56.98666667,
  40.09611111,
  42.97166667,
  2,
  7.861666667,
  44.475,
  34.98638889,
  18.88,
  10.24833333,
  118.8552778,
  19.5,
  14,
  29.69361111,
  8,
  44.10222222,
  63.18111111,
  48.90111111,
  9.009722222,
  8.700833333,
  15.74305556,
  18.75111111,
  25.95666667,
  24.50222222,
  42.36638889,
  35,
  12,
  20.66666667,
  10.64638889,
  47.10666667,
  36.30972222,
  37.54388889,
  12.5,
  6.228888889,
  36,
  21.06666667,
  24.31638889,
  15.35111111,
  29.95722222,
  6.173611111,
  49.31666667,
  91.92416667,
  8.065555556,
  23,
  46.56888889,
  55.7625,
  44.02277778,
  6,
  0,
  18.24944444,
  27.22277778,
  13.0675,
  33.70777778,
  4.995555556,
  24,
  8,
  15.10333333,
  20.5,
  54.25194444,
  42.44222222,
  41.975,
  3.033333333,
  5)

FTE_AS<-c(
  27.6818181818182,
  43.1666666666667,
  109.166666666667,
  112.166666666667,
  44.3181818181818,
  39,
  107.333333333333,
  33,
  76.125,
  55.5,
  25.4210526315789,
  57.3333333333333,
  29.75,
  27.6818181818182,
  43.1666666666667,
  109.166666666667,
  112.166666666667,
  44.3181818181818,
  39,
  107.333333333333,
  33,
  29.75,
  76.125,
  25.4210526315789,
  55.5,
  57.3333333333333,
  27.6818181818182,
  43.1666666666667,
  109.166666666667,
  112.166666666667,
  44.3181818181818,
  39,
  107.333333333333,
  33,
  29.75,
  76.125,
  25.4210526315789,
  55.5,
  57.3333333333333,
  27.6818181818182,
  43.1666666666667,
  109.166666666667,
  112.166666666667,
  44.3181818181818,
  33,
  39,
  107.333333333333,
  76.125,
  29.75,
  25.4210526315789,
  55.5,
  57.3333333333333,
  52,
  43.1666666666667,
  27.6818181818182,
  109.166666666667,
  112.166666666667,
  44.3181818181818,
  39,
  33,
  107.333333333333,
  25.4210526315789,
  76.125,
  29.75,
  55.5,
  57.3333333333333,
  52,
  27.6818181818182,
  43.1666666666667,
  109.166666666667,
  112.166666666667,
  33,
  39,
  44.3181818181818,
  107.333333333333,
  25.4210526315789,
  76.125,
  29.75,
  55.5,
  57.3333333333333,
  52,
  27.6818181818182,
  43.1666666666667,
  109.166666666667,
  112.166666666667,
  57.3333333333333,
  33,
  39,
  107.333333333333,
  44.3181818181818,
  25.4210526315789,
  76.125,
  29.75,
  55.5,
  52,
  27.6818181818182,
  43.1666666666667,
  109.166666666667,
  112.166666666667,
  57.3333333333333,
  33,
  39,
  107.333333333333,
  44.3181818181818,
  25.4210526315789,
  76.125,
  29.75,
  55.5,
  52)
TTPCal_AS<-c(
  5.91567435466215,
  4.14511849545398,
  1.29202500369367,
  0.996409787662369,
  3.64342786104218,
  2.9892517921147,
  0.482801778200761,
  3.02809995112414,
  1.59689519572011,
  2.336518744551,
  2.34857600347292,
  0.470565022505626,
  7.97808078069938,
  5.84016893619517,
  3.4371720394374,
  1.10655670665213,
  0.997134472511144,
  4.18138898168498,
  2.84577926587302,
  1.44917016415262,
  3.95884875541126,
  8.39649029611845,
  1.64684860035968,
  3.08867994860988,
  2.18476319176319,
  0.793851079734219,
  5.61566594099264,
  3.84049703512268,
  1.31602719034721,
  1.11958516033169,
  4.30894230934657,
  3.26605241935484,
  1.20885181827289,
  3.454011485826,
  7.59045744104093,
  1.47429976870244,
  3.20452253500746,
  0.692911508282476,
  1.00364127906977,
  5.89345684957125,
  4.07979589446589,
  1.43395929770992,
  1.09579684992571,
  4.13426801709402,
  3.41958333333333,
  2.84871296296296,
  1.12052442546584,
  1.58770488961868,
  7.8895606442577,
  2.76224257533931,
  0.275856356356356,
  1.15197819767442,
  2.25990918803419,
  3.37226483061402,
  5.66962441557639,
  1.5093086727407,
  0.849991180558884,
  3.96713400496278,
  1.95831093189964,
  3.58577956989247,
  1.15798211781206,
  3.06058724593157,
  1.32617451489309,
  7.72350609921388,
  1.80053036907876,
  1.22954538634659,
  2.37055417700579,
  5.64910311850027,
  3.92560544079794,
  1.67880561832061,
  1.02415158989599,
  3.08645580808081,
  1.92465491452991,
  3.96792135384615,
  1.14640304347826,
  3.11189038129745,
  1.62522550629447,
  7.00018356676004,
  2.24298373373373,
  1.12630854651163,
  2.73266128205128,
  5.97707402713774,
  2.98336388591356,
  1.61503532134942,
  1.06912537027273,
  1.25592603150788,
  3.27472262952102,
  0.624553694513372,
  1.13179775596073,
  4.20471511662531,
  2.88666242013402,
  1.2747866765542,
  7.17836242884251,
  2.40686888501405,
  2.06848188585608,
  6.43897935492699,
  3.36658261987794,
  1.44936964786998,
  1.03932093179313,
  1.08706980495124,
  3.74931329423265,
  1.83565098566308,
  1.13937715888599,
  4.21279455748553,
  2.43454519802311,
  1.21754353161361,
  8.04413047799765,
  2.69380751719461,
  1.51137303556658)
#Coeficientes de la simulacion
X <- matrix(c(TTPCal_AS,COT_AS,Fail_AS,Lim_AS,Mtto_AS,FTE_AS,SKAP_AS),nrow=length(SKAP_AS))

b <- solve(t(X)%*%X)%*%t(X)%*%TTP_AS
TTP_AS1=t(b)%*%t(X) 
TTP_AS1t=t(TTP_AS1)

mod.AS1=data.frame(X,TTP_AS1t)

npk.aov <- aov(TTP_AS1t ~ . , mod.AS1)
s_of_s <- as.matrix(anova(npk.aov)["Sum Sq"]) # Sum of Squares
d_of_f <- as.matrix(anova(npk.aov)["Df"])
m_of_s <- as.matrix((s_of_s / d_of_f)) # Mean of squares
var_comp <- c()

for (i in 1:7){
  aux <- (m_of_s[i] - m_of_s[8])/7
  var_comp <- c(var_comp,aux)}

Perc_var <- (var_comp / sum(var_comp))*100


barplot(Perc_var, main="Variabilidad en la respuesta de cada variable", horiz=FALSE,
        names.arg=c("TTPCal","COT","Fail","Lim","Mtto","FTE","SKAP"),xlab = "Variables", ylab = "% Variabilidad explicada", col =
          c("darkblue","red", "purple"))


mtree <- 131# Optimo
mtry <- 11 # Optimo

#Radom forest simulacion
rf.regre.as <- randomForest(TTP_AS1t ~ ., data=mod.AS1,ntree=mtree, mtry=mtry)

pred_TTP_AS <- predict(rf.regre.as, newdata = mod.AS1)

write.csv(as.data.frame(pred_TTP_AS),"pred_TTP_AS.csv",row.names = F)
mod.AS1 <-data.frame(X, pred_TTP_AS)

npk.aov1 <- aov(pred_TTP_AS  ~ . , mod.AS1)

s_of_s1 <- as.matrix(anova(npk.aov1)["Sum Sq"]) # Sum of Squares
d_of_f1 <- as.matrix(anova(npk.aov1)["Df"])
m_of_s1 <- as.matrix((s_of_s1 / d_of_f1)) # Mean of squares
var_comp1 <- c()

for (i in 1:7){
  aux <- (m_of_s1[i] - m_of_s1[8])/7
  var_comp1 <- c(var_comp1,aux)}

Perc_var1 <- (var_comp1 / sum(var_comp1))*100

#grafico de pronostico
plot(mod.AS1$pred_TTP_AS, main = "Ajuste de la Predicción",
     col = "#00263e", xlim = c(1, 109))
lines(ts(TTP_AS), col = "red", lty = 2, lwd = 1.5)
legend("bottomleft", lty=1, col=c("#00263e","red"),
       legend=c("Predicción","Valor Real"))
