rm(list = ls()) 
setwd("C:/Users/USER/Documents/VictorV/Proyectos/BusinessCase/")
options(scipen = 999999999) 
#librerias
library(explore)
#install.packages("explore",dependencies = T)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggpubr)
#para analisis exploratorio
library(skimr)
library(DataExplorer)
library(GGally)
library(ggpubr)#para graficar variables cualitativas

#ANALISIS
library(ExPanDaR)
library("dataMaid")

#install.packages("qcc", dependencies = T)
library(qcc)#Pareto
#install.packages("TSstudio",dependencies = T)
library(TSstudio)
library(scales)
#para graficos de burbujas
library(faraway)
library(RColorBrewer)

#Graficos Dinamicos
library(gapminder)
library(dplyr)
library(gifski)

AnalisisBusinessCase5_R <- read_excel("C:/Users/USER/OneDrive - Anheuser-Busch InBev/BusinessCase/AnalisisBusinessCase5_R.xlsx", 
                                      sheet = "Rtendencias", col_types = c("text", 
                                                                           "text", "text", "text", "text", "text", 
                                                                           "text", "numeric", "numeric"))

AnalisisBusinessCase5_R$Fecha <-as.Date(AnalisisBusinessCase5_R$Fecha) 

unique(AnalisisBusinessCase5_R$Size)

AnalisisBusinessCase5_R <-AnalisisBusinessCase5_R
VentasPanel <- AnalisisBusinessCase5_R %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,VOL_HL_Loaded)%>%
  group_by(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size) %>% 
  summarise_at(vars(VOL_HL_Loaded),list(sum)) 
unique(VentasPanel$Fecha)
VentasPanel$KHL <- VentasPanel$VOL_HL_Loaded/1000

#Filtros 

#VentasPanel <- na.omit(VentasPanel)
unique(VentasPanel$Category)
#Country	Category	Pack	Size
#Mexico	BEER	CAN	355

MXBEERCAN355 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="CAN"&Size=="355")
unique(MXBEERCAN355$Fecha)

MXBEERCAN355 <- MXBEERCAN355 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
    group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX CAN
MXBEERCAN355TS <- ts(MXBEERCAN355[,2],start = c(2021,1),frequency=12)

ts_plot(MXBEERCAN355TS,
        title = "Ventas en HL 2021-2022",
        Ytitle = "Unidades en miles(K) HL",
        Xtitle = "Source: Mexico CAN 355", 
        slider = TRUE)

ts_decompose(MXBEERCAN355TS)

ts_seasonal(MXBEERCAN355TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERCAN355TS - decompose(MXBEERCAN355TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen (sin tendencia)")

#Mexico	BEER	RB	355
MXBEERRB355 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="RB"&Size=="355")

MXBEERRB355 <- MXBEERRB355 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX CAN
MXBEERRB355TS <- ts(MXBEERRB355[,2],start = c(2021,1),frequency=12)

ts_plot(MXBEERRB355TS,
        title = "Ventas en HL 2021-2022",
        Ytitle = "Unidades en miles(K) HL",
        Xtitle = "Source: Mexico RB 355", 
        slider = TRUE)

ts_decompose(MXBEERRB355TS)

ts_seasonal(MXBEERRB355TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERRB355TS - decompose(MXBEERRB355TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen RB 355 (sin tendencia)")

#Mexico	BEER	RB	325
MXBEERRB325 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="RB"&Size=="325")

MXBEERRB325 <- MXBEERRB325 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX CAN
MXBEERRB325TS <- ts(MXBEERRB325[,2],start = c(2021,1),frequency=12)

ts_plot(MXBEERRB325TS,
        title = "Ventas en HL 2021-2022",
        Ytitle = "Unidades en miles(K) HL",
        Xtitle = "Source: Mexico RB 325", 
        slider = TRUE)

ts_decompose(MXBEERRB325TS)

ts_seasonal(MXBEERRB325TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERRB325TS - decompose(MXBEERRB325TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen RB 325 (sin tendencia)")

#Mexico	BEER	RB	1200
MXBEERRB1200 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="RB"&Size=="1200")

MXBEERRB1200 <- MXBEERRB1200 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX CAN
MXBEERRB1200TS <- ts(MXBEERRB1200[,2],start = c(2021,1),frequency=12)

ts_plot(MXBEERRB1200TS,
        title = "Ventas en HL 2021-2022",
        Ytitle = "Unidades en miles(K) HL",
        Xtitle = "Source: Mexico RB 1200", 
        slider = TRUE)

ts_decompose(MXBEERRB1200TS)

ts_seasonal(MXBEERRB1200TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERRB1200TS - decompose(MXBEERRB1200TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen RB 1200 (sin tendencia)")

#Mexico	BEER	RB	1200
MXBEERRB1200 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="RB"&Size=="1200")

MXBEERRB1200 <- MXBEERRB1200 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX CAN
MXBEERRB1200TS <- ts(MXBEERRB1200[,2],start = c(2021,1),frequency=12)

ts_plot(MXBEERRB1200TS,
        title = "Ventas en HL 2021-2022",
        Ytitle = "Unidades en miles(K) HL",
        Xtitle = "Source: Mexico RB 1200", 
        slider = TRUE)

ts_decompose(MXBEERRB1200TS)

ts_seasonal(MXBEERRB1200TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERRB1200TS - decompose(MXBEERRB1200TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen RB 1200 (sin tendencia)")

#Mexico	BEER	RB	210
MXBEERRB210 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="RB"&Size=="210")

MXBEERRB210 <- MXBEERRB210 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX CAN
MXBEERRB210TS <- ts(MXBEERRB210[,2],start = c(2021,1),frequency=12)

ts_plot(MXBEERRB210TS,
        title = "Ventas en HL 2021-2022",
        Ytitle = "Unidades en miles(K) HL",
        Xtitle = "Source: Mexico CAN 210", 
        slider = TRUE)

ts_decompose(MXBEERRB210TS)

ts_seasonal(MXBEERRB210TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERRB210TS - decompose(MXBEERRB210TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen RB 210 (sin tendencia)")
#Mexico	BEER	CAN	210
MXBEERCAN210 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="CAN"&Size=="210")

MXBEERCAN210 <- MXBEERCAN210 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX CAN
MXBEERCAN210TS <- ts(MXBEERCAN210[,2],start = c(2021,1),frequency=12)


ts_seasonal(MXBEERCAN210TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERCAN210TS - decompose(MXBEERCAN210TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen CAN 210 (sin tendencia)")


#Mexico	BEER	NRB	355
MXBEERNRB355 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="NRB"&Size=="355")

MXBEERNRB355 <- MXBEERNRB355 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX 
MXBEERNRB355TS <- ts(MXBEERNRB355[,2],start = c(2021,1),frequency=12)

#ts_plot(MXBEERNRB355TS,
        title = "Ventas en HL 2021-2022",
        Ytitle = "Unidades en miles(K) HL",
        Xtitle = "Source: Mexico ", 
        slider = TRUE)

#ts_decompose(MXBEERNRB355TS)

ts_seasonal(MXBEERNRB355TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERNRB355TS - decompose(MXBEERNRB355TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen NRB 355 (sin tendencia)")

#Mexico	BEER	NRB	325
MXBEERNRB325 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="NRB"&Size=="325")

MXBEERNRB325 <- MXBEERNRB325 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX 
MXBEERNRB325TS <- ts(MXBEERNRB325[,2],start = c(2021,1),frequency=12)

#ts_plot(MXBEERNRB355TS,
#title = "Ventas en HL 2021-2022",
#Ytitle = "Unidades en miles(K) HL",
#Xtitle = "Source: Mexico ", 
#slider = TRUE)

#ts_decompose(MXBEERNRB355TS)

ts_seasonal(MXBEERNRB325TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERNRB325TS - decompose(MXBEERNRB325TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen NRB 325 (sin tendencia)")

#Mexico	BEER	NRB	355
MXBEERNRB355 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="NRB"&Size=="355")

MXBEERNRB355 <- MXBEERNRB355 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX 
MXBEERNRB355TS <- ts(MXBEERNRB355[,2],start = c(2021,1),frequency=12)

#ts_plot(MXBEERNRB355TS,
title = "Ventas en HL 2021-2022",
Ytitle = "Unidades en miles(K) HL",
Xtitle = "Source: Mexico ", 
slider = TRUE)

#ts_decompose(MXBEERNRB355TS)

ts_seasonal(MXBEERNRB355TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERNRB355TS - decompose(MXBEERNRB355TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen NRB 355 (sin tendencia)")

#Mexico	BEER	CAN	473
MXBEERCAN473 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="CAN"&Size=="473")

MXBEERCAN473 <- MXBEERCAN473 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX 
MXBEERCAN473TS <- ts(MXBEERCAN473[,2],start = c(2021,1),frequency=12)

ts_seasonal(MXBEERCAN473TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERCAN473TS - decompose(MXBEERCAN473TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen CAN 473 (sin tendencia)")

#Mexico	BEER	CAN	330
MXBEERCAN330 <- VentasPanel %>%  filter(Country=="Mexico" & Category=="BEER"&Pack=="CAN"&Size=="330")

MXBEERCAN330 <- MXBEERCAN330 %>% select(Fecha,Country,SKU_ID,SKU_Description,Category,Brand,Pack,Size,KHL)%>%
  group_by(Fecha) %>% 
  summarise_at(vars(KHL),list(sum)) 


#Serie de tiempo para analisis MX 
MXBEERCAN330TS <- ts(MXBEERCAN330[,2],start = c(2021,1),frequency=12)

ts_seasonal(MXBEERCAN330TS, type = "all")

#Componente estacional sin tendencia
ts_seasonal(MXBEERCAN330TS - decompose(MXBEERCAN330TS)$trend, 
            type = "all", 
            title = "Seasonal Plot - Ventas en Volumen CAN 330 (sin tendencia)")

