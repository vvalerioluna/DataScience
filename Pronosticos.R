library(rvest)
library(nflfastR)
library(stringr)
library(reactable)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(crosstalk)
library(d3scatter)
devtools::install_github("jcheng5/d3scatter")
devtools::install_github("rstudio/leaflet")

shared_iris <- SharedData$new(iris)
d3scatter(shared_iris, ~Petal.Length, ~Petal.Width, ~Species)

## Pronosticos Producccion ----

Mexico_Beer_CAN_Zac <- read_excel("C:/Users/USER/OneDrive - Anheuser-Busch InBev/BusinessCase/BDS/PronosticosProd.xlsx", 
                              sheet = "MX-MXApan-Beer-CAN-L2-473")


#Produccion

MX_BEER_CAN_ZAC <- ts(Mexico_Beer_CAN_Zac[,2],start = c(2021,1),frequency=12)

ts_plot(Mexico_Beer_CAN_Zac,
        title = "Producción en KHL 2021-2023",
        Ytitle = "Unidades en KHL",
        Xtitle = "Source: MAZ", 
        slider = TRUE)

ts_decompose(MX_BEER_CAN_ZAC)

ts_seasonal(MX_BEER_CAN_ZAC, type = "normal")#normal box all

#Componente estacional sin tendencia
ts_seasonal(MX_BEER_CAN_ZAC - decompose(MX_BEER_CAN_ZAC)$trend, 
            type = "normal", 
            title = "Seasonal Plot - Producción en Volumen (sin tendencia)")



#PRONOSTICO PRODUCCION

ProdForecast <- ts_to_prophet(MX_BEER_CAN_ZAC) 
head(ProdForecast)

ProdForecast$flag <- ifelse(year(ProdForecast$ds) >= 2021, 1, 0)
h1 <- 12 
h2 <- 60 
Prod_split <- ts_split(MX_BEER_CAN_ZAC, sample.out = h1)
#Bases de train y test
Prodtrain <- Prod_split$train
Prodtest <- Prod_split$test

Prodtrain_df <- ProdForecast[1:(nrow(ProdForecast) - h1), ]
Prodtest_df <- ProdForecast[(nrow(ProdForecast) - h1 + 1):nrow(ProdForecast), ]


set.seed(1234)
library(forecast)
library(plotly)
Prodmd1 <- auto.arima(Prodtrain, 
                      stepwise = FALSE, 
                      approximation = FALSE,
                      D = 1)

Prodfc1 <- forecast(Prodmd1, h = h1)

#write.csv(ProdForecast,"BDS/MX_BEER_CAN_ZAC.csv",row.names = T)
#write.csv(Prodfc1,"BDS/ProdfcMX_BEER_NRB.csv",row.names = T)


accuracy(Prodfc1, Prodtest)

#Modelo SARIMA
#MX_BEER_CAN_ZAC <- MX_BEER_CAN_ZAC_L1_355

test_forecast(forecast.obj = Prodfc1, actual = MX_BEER_CAN_ZAC, test = Prodtest) %>% 
  layout(legend = list(x = 0.6, y = 0.95))

check_res(Prodmd1)
fc_finalProd <- forecast(Prodmd1, h = 12)
plot_forecast(fc_finalProd) %>% 
  layout(legend = list(x = 0.6, y = 0.95))


#Modelo 2 ETS Training
md2 <- ets(train, opt.crit = "mse")
fc2 <- forecast(md2, h = h1)
accuracy(fc2, test)
test_forecast(forecast.obj = fc2, actual = MX_BEER_CAN_ZAC, test = test) %>% 
  layout(legend = list(x = 0.6, y = 0.95))

#Modelo 3 TSLM Training
md3 <- tslm(train ~ season + trend)
fc3 <- forecast(md3, h = h1)
accuracy(fc3, test)
test_forecast(forecast.obj = fc3, actual = MX_BEER_CAN_ZAC, test = test) %>% 
  layout(legend = list(x = 0.6, y = 0.95))

#Modelo 4 TSLM con Flag -Training-
md3a <- tslm(train ~ season + trend + flag, data = train_df)
fc3a <- forecast(md3a, h = h1, newdata = test_df)
accuracy(fc3a, test)

test_forecast(forecast.obj = fc3a, actual = MX_BEER_CAN_ZAC, test = test) %>% 
  layout(legend = list(x = 0.6, y = 0.95))

#Modelo 5 TSLM Training -Polinómico-

md3b <- tslm(train ~ season + trend + I(trend ^ 2))
fc3b <- forecast(md3b, h = h1)
accuracy(fc3b, test)
test_forecast(forecast.obj = fc3b, actual = MX_BEER_CAN_ZAC, test = test)  %>% 
  layout(legend = list(x = 0.6, y = 0.95))


methods <- list(ets1 = list(method = "ets",
                            method_arg = list(opt.crit = "mse"),
                            notes = "ETS model with opt.crit = mse"),
                ets2 = list(method = "ets",
                            method_arg = list(opt.crit = "amse"),
                            notes = "ETS model with opt.crit = amse"),
                arima1 = list(method = "arima",
                              method_arg = list(order = c(1,0,0)),
                              notes = "ARIMA(1,0,0)"),
                Sarima = list(method = "arima",
                              method_arg = list(order = c(1,0,0),
                                                seasonal = list(order = c(1,1,0))),
                              notes = "SARIMA(1,0,0)(1,1,0)"),
                tslm = list(method = "tslm",
                            method_arg = list(formula = input ~ trend + season),
                            notes = "tslm model with trend and seasonal components"))

md <- train_model(input = MX_BEER_CAN_ZAC,
                  methods = methods,
                  train_method = list(partitions = 3, 
                                      sample.out = 12, 
                                      space = 3),
                  horizon = 12,
                  error = "MAPE")

## Pronosticos Ventas----

library(readxl)
Ventas_MAZ_1_11 <- read_excel("BDS/PronosticosVtas.xlsx", 
                              sheet = "Mexico-Beer-NRB-330", col_types = c("date", 
                                                                       "numeric"))

library(readxl)
quadrant_chart2 <- read_excel("BDS/quadrant_chart2.xlsx", 
                              sheet = "Ctry_Catg_pack", col_types = c("text", 
                                                                      "numeric", "numeric", "skip", "skip", 
                                                                      "skip", "skip"))

shared_iris <- SharedData$new(quadrant_chart2)
d3scatter(shared_iris, ~`Var %Vtas`, ~`Var %Prod`)
bscols(widths = c(1,NA,NA),
       list(
         filter_checkbox("Country_Categoria_Pack", "Pack", shared_iris, ~Country_Categoria_Pack, inline = T),
         filter_select("auto", "Vtas>=0", shared_iris, ~ifelse(`Var %Vtas` >= 0, "Yes", "No")
         #filter_slider("hp", "Horsepower", shared_mtcars, ~hp, width = "100%"),
                       )
       ),
       d3scatter(shared_iris, ~`Var %Vtas`, ~`Var %Prod`, ~factor(Country_Categoria_Pack), width="100%", height=250)
       #,
       #d3scatter(shared_mtcars, ~hp, ~qsec, ~factor(Country_Categoria_Pack), width="100%", height=250)
)



library(TSstudio)
Ventas_MAZ_1_11ts <- ts(Ventas_MAZ_1_11[,3],start = c(2021,1),frequency=12)

ts_plot(Ventas_MAZ_1_11ts,
        title = "Ventas  2021-2023",
        Ytitle = "KHL",
        Xtitle = "(1+11) MAZ ", 
        slider = TRUE)

ts_decompose(Ventas_MAZ_1_11ts)
ts_seasonal(Ventas_MAZ_1_11ts, type = "normal")
ts_seasonal(Ventas_MAZ_1_11ts - decompose(Ventas_MAZ_1_11ts)$trend, 
            type = "normal", 
            title = "Seasonal Plot - Ventas en Volumen (sin tendencia)")
ts_cor(Ventas_MAZ_1_11ts)

Vol_df <- ts_to_prophet(Ventas_MAZ_1_11ts) 
head(Vol_df)

library(lubridate)
Vol_df$flag <- ifelse(year(Vol_df$ds) >= 2023, 1, 0)
h1 <- 12 
h2 <- 60 
Vol_split <- ts_split(Ventas_MAZ_1_11ts, sample.out = h1)
train <- Vol_split$train
test <- Vol_split$test
train_df <- Vol_df[1:(nrow(Vol_df) - h1), ]
test_df <- Vol_df[(nrow(Vol_df) - h1 + 1):nrow(Vol_df), ]
set.seed(1234)
library(forecast)
library(plotly)

md1 <- auto.arima(train, 
                  stepwise = FALSE, 
                  approximation = FALSE,
                  D = 1)
#Modelo tradicional
#Traninig

fc1 <- forecast(md1, h = h1)

#write.csv(ProdForecast,"BDS/MX_BEER_CAN_ZAC.csv",row.names = T)
#write.csv(fc1,"BDS/VentasForecastMaz.csv",row.names = T)

accuracy(fc1, test)
#Sarima
test_forecast(forecast.obj = fc1, actual = Ventas_MAZ_1_11ts, test = test) %>% 
  layout(legend = list(x = 0.6, y = 0.95))

#Modelo 2 ETS Training

md2 <- ets(train, opt.crit = "mse")
fc2 <- forecast(md2, h = h1)
accuracy(fc2, test)

test_forecast(forecast.obj = fc2, actual = Ventas_MAZ_1_11ts, test = test) %>% 
  layout(legend = list(x = 0.6, y = 0.95))

#Modelo 3 TSLM Training
md3 <- tslm(train ~ season + trend)
fc3 <- forecast(md3, h = h1)
accuracy(fc3, test)

test_forecast(forecast.obj = fc3, actual = Ventas_MAZ_1_11ts, test = test) %>% 
  layout(legend = list(x = 0.6, y = 0.95))

#Modelo 4 TSLM con Bandera -Entrenamiento-
md3a <- tslm(train ~ season + trend + flag, data = train_df)
fc3a <- forecast(md3a, h = h1, newdata = test_df)
accuracy(fc3a, test)

test_forecast(forecast.obj = fc3a, actual = Ventas_MAZ_1_11ts, test = test) %>% 
  layout(legend = list(x = 0.6, y = 0.95))

#Modelo 5 TSLM Formación -Polinómico-

md3b <- tslm(train ~ season + trend + I(trend ^ 2))
fc3b <- forecast(md3b, h = h1)
accuracy(fc3b, test)

test_forecast(forecast.obj = fc3b, actual = Ventas_MAZ_1_11ts, test = test)  %>% 
  layout(legend = list(x = 0.6, y = 0.95))


check_res(md1)
check_res(md2)
check_res(md3)
check_res(md3a)
check_res(md3b)

md_final <- auto.arima(Ventas_MAZ_1_11ts, 
                       stepwise = FALSE, 
                       approximation = FALSE,
                       D = 1)
fc_final <- forecast(md_final, h = 12)
plot_forecast(fc_final) %>% 
  layout(legend = list(x = 0.6, y = 0.95))


#ML
library(readxl)
PronosticosVtas <- read_excel("BDS/PronosticosVtas.xlsx", 
                              sheet = "ML_Prod")

PronosticosVtas$Fecha <- as.Date(PronosticosVtas$Fecha)
names(PronosticosVtas)
#PronosticosVtas <-PronosticosVtas %>% rename(BGT_VTAS=`BGT Vtas`,VTAS210=`2+10 Vtas`,BGT_PROD=`BGT Prod`,PROD210=`2+10 Prod`)
PronosticosVtas <-PronosticosVtas %>% rename(PROD210=`Prod Domestic`)
set.seed(100) 

PronosticosVtasMl <- mutate(PronosticosVtas,Clase=ifelse(Fecha>="2023-01-01","Test","Train"))

train = PronosticosVtasMl[PronosticosVtasMl$Clase == 'Train',]

test = PronosticosVtasMl[PronosticosVtasMl$Clase == 'Test',]

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

#RF
set.seed(100)

library(randomForest)
names(train)

 

#rf = randomForest(VTAS210 ~ BGT_VTAS + BGT_PROD  + PROD210 + Fecha, data = train)
rf = randomForest(Vtas_210 ~ Prod_Domestic, data = train)


print(rf)


predictions = predict(rf, newdata = train)
mape(train$Vtas_210, predictions)

predictions = predict(rf, newdata = test)
mape(test$Vtas_210, predictions) 

varImpPlot(rf)

set.seed(100)
rf_revised = randomForest(PROD210 ~ + BGT_PROD  + VTAS210, data = train)
print(rf_revised) 


predictions = predict(rf_revised, newdata = train)
mape(train$PROD210, predictions) 


predictions = predict(rf_revised, newdata = test)
mape(test$PROD210, predictions) 

importance(rf_revised) 
varImpPlot(rf_revised) 

#Out-Of-Bag Error (OOB) es un método para medir el error de predicción de bosques aleatorios
tuneRF(x = train[,-which(names(train2) == "Vtas_210")],#variables predictoras
       y = train$Vtas_210,#variable a predecir
       tepFactor = 1.5, #iteraciones
       improve = 1e-5,#La mejora (relativa) en el error OOB
       ntreeTry = 1000)#numero de arboles

train2 <- train %>% select(2:19)

rfm2 <- randomForest(Vtas_210 ~., data=train2, 
                     ntree=500, mtry=12)
names(train)
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


test_predictorsm2 <- test[,-which(names(test) == "PROD210")]
prediccionm2 <- predict(rfm2, test_predictorsm2)
y_rfm2 <- prediccionm2

Dom <- PronosticosVtas %>% select(3,4,5)
Dom2 <- PronosticosVtas %>% select(2,3,4,5)

Exp <- PronosticosVtas %>% select(6:19)

pairs.panels(Dom2,
             smooth = F,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = T,     # Si TRUE, añade histogramas y curvas de densidad
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
  data = Exp,
  type = "continuous",
  title = "Matriz de correlación",
  theme_config = list(legend.position = "none",
                      plot.title = element_text(size = 16, face = "bold"),
                      axis.title = element_blank(),
                      axis.text.x = element_text(angle = -45, hjust = +0.1)
  )
)

#Vistas de Correlograma
Correlograma <- cor(Exp)
#Vista 1
corrplot(Correlograma,method="pie",type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 100)

ggpairs(Exp,#Data frame
        lower = list(continuous = "smooth"),#Panel inferior
        diag = list(continuous = "barDiag"), axisLabels = "none" #Diagonal
) 


#Backtesting
methods <- list(ets1 = list(method = "ets",
                            method_arg = list(opt.crit = "mse"),
                            notes = "ETS model with opt.crit = mse"),
                ets2 = list(method = "ets",
                            method_arg = list(opt.crit = "amse"),
                            notes = "ETS model with opt.crit = amse"),
                arima1 = list(method = "arima",
                              method_arg = list(order = c(1,0,0)),
                              notes = "ARIMA(1,0,0)"),
                Sarima = list(method = "arima",
                              method_arg = list(order = c(1,0,0),
                                                seasonal = list(order = c(1,1,0))),
                              notes = "SARIMA(1,0,0)(1,1,0)"),
                tslm = list(method = "tslm",
                            method_arg = list(formula = input ~ trend + season),
                            notes = "tslm model with trend and seasonal components"))


md <- train_model(input = Ventas_MAZ_1_11ts,
                  methods = methods,
                  train_method = list(partitions = 2, 
                                      sample.out = 12, 
                                      space = 6),
                  horizon = 12,
                  error = "MAPE")

library(TSstudio)

library(forecast)
md5 <- ts_backtesting(ts.obj = Ventas_MAZ_1_11ts,
                      periods = 6, 
                      error = "MAPE",
                      window_size = h1,
                      h = h2,
                      a.arg = list(stepwise = FALSE, 
                                   approximation = FALSE,
                                   D = 1),
                      e.arg = list(opt.crit = "mse"),
                      n.arg = list(P = 2, 
                                   p =1,
                                   repeats = 100),
                      h.arg = list(errorMethod = "RMSE",
                                   verbos = FALSE))


###Analisis multivariado Ventas-Prod-Inflacion MX----

library(tseries)
library(vars)
library(highcharter)
library(dplyr)
library(ggfortify)
library(plotly)

Ventas_MX_2_10 <- read_excel("BDS/PronosticosVtas.xlsx", 
                              sheet = "MX", col_types = c("date","numeric","numeric","numeric"))as.Date()
Ventas_MX_2_10$ds <- as.Date(Ventas_MX_2_10$ds)

class(Ventas_MX_2_10)

Ventas_MX_2_10ts <- ts(Ventas_MX_2_10[,-1],start = c(2021,1),frequency=12)

autoplot(Ventas_MX_2_10ts)

hchart(Ventas_MX_2_10ts)

pp.test(Ventas_MX_2_10ts[,1])#Produccion
pp.test(diff(Ventas_MX_2_10ts[,1]))#Diferencias Produccion

pp.test(Ventas_MX_2_10ts[,2])#Venta
pp.test(diff(Ventas_MX_2_10ts[,2]))#

pp.test(Ventas_MX_2_10ts[,3])#Inflacion
pp.test(diff(Ventas_MX_2_10ts[,3]))#

#Se piensa que la Inflacion tiene relacion con la vtas
po.test(cbind(Ventas_MX_2_10ts[,3],Ventas_MX_2_10ts[,2]))

reg <- lm(Ventas_MX_2_10ts[,2]~Ventas_MX_2_10ts[,3])
summary(reg)

residuos = resid(reg)
plot(resid(reg),t="l")
arma11 <- forecast::auto.arima(residuos)
arma11
tsdiag(arma11)


library(vars)
var.1 <- VAR(Ventas_MX_2_10ts, 2, type = "none") # Estimamos el modelo
var.1

var.aic <- VAR(Ventas_MX_2_10ts, type = "none", lag.max = 5, ic = "AIC")
summary(var.aic)

causality(var.aic,"Ventas.2.10")

ir.1 <- irf(var.1, impulse = "Producción.2.10", response = "Inflacion", n.ahead = 10, ortho = T)
plot(ir.1)

ir.2 <- irf(var.1, impulse = "Ventas.2.10", response = "Inflacion", n.ahead = 10, ortho = FALSE)
plot(ir.2)

ir.3 <- irf(var.1, impulse = "Ventas.2.10", response = "Producción.2.10", n.ahead = 10, ortho = FALSE)
plot(ir.3)

ir.4 <- irf(var.1, impulse = "Producción.2.10", response = "Ventas.2.10", n.ahead = 10, ortho = FALSE)
plot(ir.4)

sm <- summary(var.1)
sm$covres #Sigma

sm$corres #Rho

t(chol(sm$covres))

#Tasas de crecimiento
TasaCrec <- diff(log(Ventas_MX_2_10ts))
ggplot2::autoplot(TasaCrec,main = "Series como tasas de crecimiento")

library(timsac)
decomp(TasaCrec, trade=TRUE)
VARselect(TasaCrec, type = "none", lag.max = 5)


# Representación gráfica de la tendencia
plot(TasaCrec, main="Tendencia")


modVar <- VAR(TasaCrec, 2, type = "none")
summary(modVar)

#MODELOS ML 

library(modeltime)


fit_arima <- modeltime::arima_reg() %>%
  set_engine('auto_arima') %>%
  fit(`Ventas 2+10` ~ ds, training(Ventas_MAZ_1_11))




