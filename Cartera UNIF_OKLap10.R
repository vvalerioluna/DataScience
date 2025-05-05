####### PROGRAMA PARA GENERAR LA CARTERA UNIFICADA B2B EN R 3.6.1 Ver.10

#### Instalacion Paquetes
#install.packages("DBI",dependencies = TRUE)
#install.packages("odbc",dependencies=TRUE)
#install.packages("devtools",dependencies = TRUE)
#devtools::install_github("rstats-db/odbc")
#install.packages("RODM",dependencies = TRUE)
#install.packages("ROracle",dependencies=TRUE)
#install.packages("tidyverse",dependencies = TRUE)
#install.packages('htmlwidgets', 'rpivotTable',dependencies = TRUE)
#install.packages('rpivotTable',dependencies = TRUE)

#### Carga de Paquetes
#setwd("D:/BD_B2B") #enrutado a carpeta de BDs
#setwd("D:/Users/MRT15076/OneDrive - Telefonica/4_CarteraUnificada") #enrutado a carpeta de BDs
setwd("D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/04_Abr21") #enrutado a carpeta de BDs
library(readxl) #leer excel
library(readr) #leer txt
library(tidyverse)
library(dplyr,warn.conflicts = FALSE)
#library(rpivotTable)
#library(RODBC))
#library(RODM)#library(DBI)

#library(DBI)
#library(rJava)
#library(RJDBC)
#library(shiny)
#library(miniUI)

### Variables

Perimetro<- read_csv("1_Perimetro12042021.csv",locale = locale(encoding = "ISO-8859-1", asciify = TRUE), trim_ws = TRUE)
SaldosSCL <- read_delim("SCL12Abril21Ok.txt", "|", escape_double = FALSE, trim_ws = TRUE)
Castigo <- read_delim("Castigo_Contable1Abr21.txt","|", escape_double = FALSE, trim_ws = TRUE)





## Rutas Archivos de Entrada
#Perimetro

Perimetro <- read_delim("//10.225.211.58/Finanzas/Credito_y_Cobranza/POSPAGO/Cobranza/Actualizacion BD b2b (cobranza)/2020_Cartera_B2B.txt", 
                        "\t", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1", 
                                                                     asciify = TRUE), trim_ws = TRUE)
RutaP <- paste('D:/BD_B2B/4_CarteraUnificada/2021/1_Ene21/','1_Perimetro',as.character(Sys.Date(),"%d%m%Y"),'.csv',sep = "")
write.csv(Perimetro,file=RutaP,row.names = FALSE)





##Rutas Archivos de Salida



### PROCESO

## Seleccionar columnas del perimetro para cruce
Perimetro <- Perimetro %>% select(1,6,8,2:5,7,19)
names(Perimetro)[1] <- "PERIMETRO" #A partir de Mar20 se cambia el nombre con Acuerdo de Post-Venta
names(Perimetro)[3] <- "SUBSEGMENTO" #A partir de Mar20 se cambia el nombre con Acuerdo de Post-Venta

#### PROCESAMIENTO BDs
#### CARTERA SCL
SaldosSCL <- read_delim("SCL12Abril21Ok.txt", "|", escape_double = FALSE, trim_ws = TRUE)
str(SaldosSCL)
SaldosSCL %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
Parque <- Perimetro %>% select(COD_CLIENTE)
SaldosSCL <- left_join(Parque,SaldosSCL,by=c("COD_CLIENTE"))
SaldosSCL <- left_join(Parque,SaldosSCL,by=NULL)

CifraCtrlSCL1 <- SaldosSCL %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
SaldosSCL %>% filter(COD_CLIENTE==95358134) %>%  summarise(SALDO=sum(SALDO,na.rm = TRUE))
SaldosSCL %>% filter(COD_CLIENTE==68821285) %>%  summarise(SALDO=sum(SALDO,na.rm = TRUE))#Cliente en SCL,ONIX
SaldosSCL %>% filter(COD_CLIENTE==3513938) %>%  summarise(SALDO=sum(SALDO,na.rm = TRUE))#Cliente en SCL,ONIX
SaldosSCL %>% filter(COD_CLIENTE==22656910) %>%  summarise(SALDO=sum(SALDO,na.rm = TRUE))
SaldosSCL %>% filter(COD_CLIENTE==22739434) %>%  summarise(SALDO=sum(SALDO,na.rm = TRUE))#Cliente Nuevo por RFC
### Se cargan los archivos de cierre de Fonseca para LIB
##CASTIGO Mes
Castigo <- read_delim("Castigo_Contable1Abr21.txt","|", escape_double = FALSE, trim_ws = TRUE)
Castigo <- unite(Castigo,NRO_DOCUMENTO_FISCAL,c(3:4),sep="     ",remove = TRUE)
Castigo <-Castigo %>%  select(3,7) %>% group_by(NRO_DOCUMENTO_FISCAL) %>% summarise(Castigo=sum(SALDO))
Castigo <- as_tibble(Castigo[!duplicated(Castigo$NRO_DOCUMENTO_FISCAL), ])
head(Castigo)

SaldosSCL <- left_join(SaldosSCL,Castigo,by=NULL)
CifraCtrlSCL2 <- SaldosSCL %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
rm(Castigo)
#Check de cifras Ctrl
ifelse(sum(CifraCtrlSCL1-CifraCtrlSCL2)==0,"CIFRAS_OK","DIFERENCIA_REVISAR")


##PENA Mes, esta base es a mes vencido
Pena <- read_delim("PENA_CONVENCIONAL_SCL1Abr21.txt","|",escape_double = FALSE,trim_ws = TRUE)
#Pena <- read_delim("PENA_CONVENCIONAL_SCL.txt","\t",escape_double = FALSE,trim_ws = TRUE)

names(Pena)
Pena <- unite(Pena,NRO_DOCUMENTO_FISCAL,c(9:10),sep="     ",remove = TRUE)
Pena <- Pena %>% select(9,3)
names(Pena)[2]="PENA"
Pena <- as_tibble(Pena[!duplicated(Pena$NRO_DOCUMENTO_FISCAL), ])
SaldosSCL <- left_join(SaldosSCL,Pena,by=NULL)
CifraCtrlSCL3 <- SaldosSCL %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
rm(Pena)
#Check de cifras Ctrl
ifelse(sum(CifraCtrlSCL1-CifraCtrlSCL3)==0,"CIFRAS_OK","DIFERENCIA_REVISAR")


### TERCERA Acumulada (es la unica que se acumula, Pena y castigo solo la del mes como indica Fonseca)

T3era <- read_delim("Terceras_a_mas_Jul17_Oct20.txt", 
                                         "|", escape_double = FALSE, trim_ws = TRUE)
T3era <- T3era %>% select(3,7)
names(T3era)[2]="TERCERA"
#OctNov2020
T3eraOct20 <- read_excel("Terceras_a_mas_Oct20_20201130_C.xlsx", 
                         sheet = "Terceras_a_mas_Oct20_20201130_C")

T3eraOct20 <- T3eraOct20 %>% select(NRO_DOCUMENTO_FISCAL,NUM_FACT_MORA)
names(T3eraOct20)[2]="TERCERA"
T3era <- full_join(T3era,T3eraOct20,by=NULL)
T3era <- as.tibble(T3era[!duplicated(T3era$NRO_DOCUMENTO_FISCAL), ])
rm(T3eraOct20)

#Dic2020
T3eraDic20 <- read_excel("Terceras_a_mas_Dic20_20201231_C.xlsx", 
                         sheet = "Terceras_a_mas_Dic20_20201231_C")

T3eraDic20 <- T3eraDic20 %>% select(NRO_DOCUMENTO_FISCAL,NUM_FACT_MORA)
names(T3eraDic20)[2]="TERCERA"
T3era <- full_join(T3era,T3eraDic20,by=NULL)
T3era <- as.tibble(T3era[!duplicated(T3era$NRO_DOCUMENTO_FISCAL), ])
rm(T3eraDic20)


#Ene2020
T3eraEne20 <- read_excel("Terceras_a_mas_Ene21_20210131_C.xlsx", 
                         sheet = "Terceras_a_mas_Ene21_20210131_C")

T3eraEne20 <- T3eraEne20 %>% select(NRO_DOCUMENTO_FISCAL,NUM_FACT_MORA)
names(T3eraEne20)[2]="TERCERA"
T3era <- full_join(T3era,T3eraEne20,by=NULL)
T3era <- as.tibble(T3era[!duplicated(T3era$NRO_DOCUMENTO_FISCAL), ])
rm(T3eraEne20)

#Feb2020
T3eraFeb20 <- read_excel("Terceras_a_mas_Feb21_20210228_C.xlsx", 
                         sheet = "Terceras_a_mas_Feb21_20210228_C")

T3eraFeb20 <- T3eraFeb20 %>% select(NRO_DOCUMENTO_FISCAL,NUM_FACT_MORA)
names(T3eraFeb20)[2]="TERCERA"
T3era <- full_join(T3era,T3eraFeb20,by=NULL)
T3era <- as.tibble(T3era[!duplicated(T3era$NRO_DOCUMENTO_FISCAL), ])
rm(T3eraFeb20)

#Mar2020
T3eraMar20 <- read_excel("Terceras_a_mas_Mar21_20210331_C.xlsx", 
                         sheet = "Terceras_a_mas_Mar21_20210331_C")

T3eraMar20 <- T3eraMar20 %>% select(NRO_DOCUMENTO_FISCAL,NUM_FACT_MORA)
names(T3eraMar20)[2]="TERCERA"
T3era <- full_join(T3era,T3eraMar20,by=NULL)
T3era <- as.tibble(T3era[!duplicated(T3era$NRO_DOCUMENTO_FISCAL), ])
rm(T3eraMar20)


##
SaldosSCL <- left_join(SaldosSCL,T3era,by=NULL)
CifraCtrlSCL4 <- SaldosSCL %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
rm(T3era)
#Check de cifras Ctrl
ifelse(sum(CifraCtrlSCL1-CifraCtrlSCL4)==0,"CIFRAS_OK","DIFERENCIA_REVISAR")
#Solo clientes con Saldo
#SaldosSCL <- SaldosSCL %>% filter(SALDO!=is.na(SALDO))

# COLUMNA PARA LIB
SaldosSCL <-mutate(SaldosSCL,Final=ifelse(Castigo!=0,"CASTIGO",is.na()))
SaldosSCL <-mutate(SaldosSCL,Final=ifelse(is.na(Final)& PENA!=is.na(PENA),"PENA",Final))
SaldosSCL <-mutate(SaldosSCL,Final=ifelse(is.na(Final)& TERCERA!=is.na(TERCERA),"TERCERA",Final))
SaldosSCL <-mutate(SaldosSCL,Final=ifelse(is.na(Final),EQ_SERV,Final))
levels(as.factor(SaldosSCL$Final))
SaldosSCL %>% select(1,15,20:23) %>% filter(COD_CLIENTE==7003328)
SaldosSCL %>% select(1,10,15,20:23) %>% filter(COD_CLIENTE==84533881)

#Check con Pame diferencias en adeudo lib
SaldosSCL %>% select(1,10,15,20:23) %>% filter(COD_CLIENTE==9754156)
SaldosSCL %>% select(1,10,15,20:23) %>% filter(COD_CLIENTE==87259571)
SaldosSCL %>% select(1,10,15,20:23) %>% filter(COD_CLIENTE==73245999)
#CC_BANCOMER_CARTERA_SCL <- left_join(CC_BANCOMER_CARTERA_SCL,SaldosSCL,by="COD_CLIENTE")#Clientes BBVA Sandy
#write.csv(CC_BANCOMER_CARTERA_SCL,"BBVASAndy.csv",row.names=F)



rm(CifraCtrlSCL2,CifraCtrlSCL3,CifraCtrlSCL4)
###

#Salida de los saldos por factura
SaldosSCL_F <- SaldosSCL %>% filter(SALDO!=is.na(SALDO))
#para sacarlo por años > 360

SaldosSCL_F <- SaldosSCL_F %>% filter(CAJON=='D_181_210' | CAJON=='D_211_240' | CAJON=='D_241_360'| CAJON=='MAS_360')
#
BaseSCLYear <- paste('D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/04_Abr21/','BaseSCLYear',as.character(Sys.Date(),"%d%m%Y"),'.csv',sep = "")
write.csv(SaldosSCL_F,BaseSCLYear,row.names = F)
rm(SaldosSCL_F)
###FIN BASE SCL PARA CARTERA Y NET


#### NET con valores totales
NET_SCL<-mutate(SaldosSCL,CORRIENTE_T=ifelse(CAJON=="CORRIENTE",SALDO,0))
NET_SCL<-mutate(NET_SCL,'1.30_T'=ifelse(CAJON=="D_01_30",SALDO,0))
NET_SCL<-mutate(NET_SCL,'31.60_T'=ifelse(CAJON=="D_31_60",SALDO,0))
NET_SCL<-mutate(NET_SCL,'61.90_T'=ifelse(CAJON=="D_61_90",SALDO,0))
NET_SCL<-mutate(NET_SCL,'91.120_T'=ifelse(CAJON=="D_91_120",SALDO,0))
NET_SCL<-mutate(NET_SCL,'121.150_T'=ifelse(CAJON=="D_121_150",SALDO,0))
NET_SCL<-mutate(NET_SCL,'151.180_T'=ifelse(CAJON=="D_151_180",SALDO,0))
NET_SCL<-mutate(NET_SCL,'181.210_T'=ifelse(CAJON=="D_181_210",SALDO,0))
NET_SCL<-mutate(NET_SCL,'211.240_T'=ifelse(CAJON=="D_211_240",SALDO,0))
NET_SCL<-mutate(NET_SCL,'241.360_T'=ifelse(CAJON=="D_241_360",SALDO,0))
NET_SCL<-mutate(NET_SCL,MAS_360_T=ifelse(CAJON=="MAS_360",SALDO,0))
str(NET_SCL)

NET_SCL<-mutate(NET_SCL,'TOTAL_ADEUDO_T'=rowSums(select(NET_SCL,24:34)))
NET_SCL<-mutate(NET_SCL,'TOTAL_VENCIDO_T'=rowSums(select(NET_SCL,25:34)))
NET_SCL<-mutate(NET_SCL,'1.90_T'=rowSums(select(NET_SCL,25:27)))
NET_SCL<-mutate(NET_SCL,'91.360_T'=rowSums(select(NET_SCL,28:33)))

CFRCTRLA <- NET_SCL %>% summarise(SALDO=sum(TOTAL_ADEUDO_T,na.rm = TRUE))
#Check de cifras Ctrl
ifelse(sum(CifraCtrlSCL1-CFRCTRLA)==0,"CIFRAS_OK","DIFERENCIA_REVISAR")

## Segmento Equipo Totales
NET_SCL<-mutate(NET_SCL,CTE_EQ_T=ifelse(CAJON=="CORRIENTE" & EQ_SERV=="EQUIPO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'1.30_EQ_T'=ifelse(CAJON=="D_01_30" & EQ_SERV=="EQUIPO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'31.60_EQ_T'=ifelse(CAJON=="D_31_60" & EQ_SERV=="EQUIPO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'61.90_EQ_T'=ifelse(CAJON=="D_61_90" & EQ_SERV=="EQUIPO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'91.120_EQ_T'=ifelse(CAJON=="D_91_120" & EQ_SERV=="EQUIPO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'121.150_EQ_T'=ifelse(CAJON=="D_121_150" & EQ_SERV=="EQUIPO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'151.180_EQ_T'=ifelse(CAJON=="D_151_180" & EQ_SERV=="EQUIPO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'181.210_EQ_T'=ifelse(CAJON=="D_181_210" & EQ_SERV=="EQUIPO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'211.240_EQ_T'=ifelse(CAJON=="D_211_240"& EQ_SERV=="EQUIPO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'241.360_EQ_T'=ifelse(CAJON=="D_241_360" & EQ_SERV=="EQUIPO",SALDO,0))
NET_SCL<-mutate(NET_SCL,MAS_360_EQ_T=ifelse(CAJON=="MAS_360" & EQ_SERV=="EQUIPO",SALDO,0))
str(NET_SCL)

NET_SCL<-mutate(NET_SCL,'TOTAL_ADE_EQ_T'=rowSums(select(NET_SCL,39:49)))
NET_SCL<-mutate(NET_SCL,'1.90_EQ_T'=rowSums(select(NET_SCL,40:42)))
NET_SCL<-mutate(NET_SCL,'91.360_EQ_T'=rowSums(select(NET_SCL,43:48)))

#######Segmento Servicio Totales

NET_SCL<-mutate(NET_SCL,CTE_SV_T=ifelse(CAJON=="CORRIENTE" & EQ_SERV=="SERVICIO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'1.30_SV_T'=ifelse(CAJON=="D_01_30" & EQ_SERV=="SERVICIO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'31.60_SV_T'=ifelse(CAJON=="D_31_60" & EQ_SERV=="SERVICIO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'61.90_SV_T'=ifelse(CAJON=="D_61_90" & EQ_SERV=="SERVICIO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'91.120_SV_T'=ifelse(CAJON=="D_91_120" & EQ_SERV=="SERVICIO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'121.150_SV_T'=ifelse(CAJON=="D_121_150" & EQ_SERV=="SERVICIO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'151.180_SV_T'=ifelse(CAJON=="D_151_180" & EQ_SERV=="SERVICIO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'181.210_SV_T'=ifelse(CAJON=="D_181_210" & EQ_SERV=="SERVICIO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'211.240_SV_T'=ifelse(CAJON=="D_211_240"& EQ_SERV=="SERVICIO",SALDO,0))
NET_SCL<-mutate(NET_SCL,'241.360_SV_T'=ifelse(CAJON=="D_241_360" & EQ_SERV=="SERVICIO",SALDO,0))
NET_SCL<-mutate(NET_SCL,MAS_360_SV_T=ifelse(CAJON=="MAS_360" & EQ_SERV=="SERVICIO",SALDO,0))
str(NET_SCL)

NET_SCL<-mutate(NET_SCL,'TOTAL_ADE_SV_T'=rowSums(select(NET_SCL,53:63)))
NET_SCL<-mutate(NET_SCL,'1.90_SV_T'=rowSums(select(NET_SCL,54:56)))
NET_SCL<-mutate(NET_SCL,'91.360_SV_T'=rowSums(select(NET_SCL,57:62)))

#FIN ENCADOJANDO SCL TOTAL
NET_SCL <- NET_SCL %>% select(1,24:66,20)
NET_SCL %>% summarise(SUM=sum(TOTAL_ADEUDO_T,na.rm = TRUE))
#Sumarizado SCL
NET_SCL <- NET_SCL %>% group_by(COD_CLIENTE) %>% 
  summarise(CORRIENTE_T=sum(CORRIENTE_T),
            '1.30_T'=sum(`1.30_T`),
            '31.60_T'=sum(`31.60_T`),
            '61.90_T'=sum(`61.90_T`),
            '91.120_T'=sum(`91.120_T`),
            '121.150_T'=sum(`121.150_T`),
            '151.180_T'=sum(`151.180_T`),
            '181.210_T'=sum(`181.210_T`),
            '211.240_T'=sum(`211.240_T`),
            '241.360_T'=sum(`241.360_T`),
            MAS_360_T=sum(MAS_360_T),
            TOTAL_ADEUDO_T=sum(TOTAL_ADEUDO_T),
            TOTAL_VENCIDO_T=sum(TOTAL_VENCIDO_T),
            '1.90_T'=sum(`1.90_T`,na.rm = TRUE),
            '91.360_T'=sum(`91.360_T`),
            CTE_EQ_T=sum(CTE_EQ_T),
            '1.30_EQ_T'=sum(`1.30_EQ_T`),
            '31.60_EQ_T'=sum(`31.60_EQ_T`),
            '61.90_EQ_T'=sum(`61.90_EQ_T`),
            '91.120_EQ_T'=sum(`91.120_EQ_T`),
            '121.150_EQ_T'=sum(`121.150_EQ_T`),
            '151.180_EQ_T'=sum(`151.180_EQ_T`),
            '181.210_EQ_T'=sum(`181.210_EQ_T`),
            '211.240_EQ_T'=sum(`211.240_EQ_T`),
            '241.360_EQ_T'=sum(`241.360_EQ_T`),
            MAS_360_EQ_T=sum(MAS_360_EQ_T),
            TOTAL_ADE_EQ_T=sum(TOTAL_ADE_EQ_T),
            '1.90_EQ_T'=sum(`1.90_EQ_T`),
            '91.360_EQ_T'=sum(`91.360_EQ_T`),
            CTE_SV_T=sum(CTE_SV_T),
            '1.30_SV_T'=sum(`1.30_SV_T`),
            '31.60_SV_T'=sum(`31.60_SV_T`),
            '61.90_SV_T'=sum(`61.90_SV_T`),
            '91.120_SV_T'=sum(`91.120_SV_T`),
            '121.150_SV_T'=sum(`121.150_SV_T`),
            '151.180_SV_T'=sum(`151.180_SV_T`),
            '181.210_SV_T'=sum(`181.210_SV_T`),
            '211.240_SV_T'=sum(`211.240_SV_T`),
            '241.360_SV_T'=sum(`241.360_SV_T`),
            MAS_360_SV_T=sum(MAS_360_SV_T),
            TOTAL_ADE_SV_T=sum(TOTAL_ADE_SV_T),
            '1.90_SV_T'=sum(`1.90_SV_T`),
            '91.360_SV_T'=sum(`91.360_SV_T`),
            Castigo=sum(Castigo))


##@@@ SCL LIB
SCL_LIB <- SaldosSCL %>% filter(Final!="CASTIGO" & Final!="PENA" & Final!="TERCERA")
CFRCTRLIB <- SCL_LIB %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
SCL_LIB %>% select(COD_CLIENTE,SALDO) %>%  filter(COD_CLIENTE==68249773) %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
##ACOMODO CAJONES LIB
SCL_LIB<-mutate(SCL_LIB,CORRIENTE_LIB=ifelse(CAJON=="CORRIENTE",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'1.30_LIB'=ifelse(CAJON=="D_01_30",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'31.60_LIB'=ifelse(CAJON=="D_31_60",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'61.90_LIB'=ifelse(CAJON=="D_61_90",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'91.120_LIB'=ifelse(CAJON=="D_91_120",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'121.150_LIB'=ifelse(CAJON=="D_121_150",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'151.180_LIB'=ifelse(CAJON=="D_151_180",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'181.210_LIB'=ifelse(CAJON=="D_181_210",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'211.240_LIB'=ifelse(CAJON=="D_211_240",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'241.360_LIB'=ifelse(CAJON=="D_241_360",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,MAS_360_LIB=ifelse(CAJON=="MAS_360",SALDO,0))
str(SCL_LIB)

SCL_LIB <- SCL_LIB %>% select(1,24:34,15,17,19)
SCL_LIB<-mutate(SCL_LIB,'TOTAL_ADEUDO_LIB'=rowSums(select(SCL_LIB,2:12)))
SCL_LIB<-mutate(SCL_LIB,'1.90_LIB'=rowSums(select(SCL_LIB,3:5)))
SCL_LIB<-mutate(SCL_LIB,'91.360_LIB'=rowSums(select(SCL_LIB,6:11)))

## Segmento Equipo LIB
SCL_LIB<-mutate(SCL_LIB,CTE_EQ_LIB=ifelse(CAJON=="CORRIENTE" & EQ_SERV=="EQUIPO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'1.30_EQ_LIB'=ifelse(CAJON=="D_01_30" & EQ_SERV=="EQUIPO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'31.60_EQ_LIB'=ifelse(CAJON=="D_31_60" & EQ_SERV=="EQUIPO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'61.90_EQ_LIB'=ifelse(CAJON=="D_61_90" & EQ_SERV=="EQUIPO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'91.120_EQ_LIB'=ifelse(CAJON=="D_91_120" & EQ_SERV=="EQUIPO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'121.150_EQ_LIB'=ifelse(CAJON=="D_121_150" & EQ_SERV=="EQUIPO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'151.180_EQ_LIB'=ifelse(CAJON=="D_151_180" & EQ_SERV=="EQUIPO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'181.210_EQ_LIB'=ifelse(CAJON=="D_181_210" & EQ_SERV=="EQUIPO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'211.240_EQ_LIB'=ifelse(CAJON=="D_211_240"& EQ_SERV=="EQUIPO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'241.360_EQ_LIB'=ifelse(CAJON=="D_241_360" & EQ_SERV=="EQUIPO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,MAS_360_EQ_LIB=ifelse(CAJON=="MAS_360" & EQ_SERV=="EQUIPO",SALDO,0))
str(SCL_LIB)

SCL_LIB<-mutate(SCL_LIB,'TOTAL_ADE_EQ_LIB'=rowSums(select(SCL_LIB,19:29)))
SCL_LIB<-mutate(SCL_LIB,'1.90_EQ_LIB'=rowSums(select(SCL_LIB,20:22)))
SCL_LIB<-mutate(SCL_LIB,'91.360_EQ_LIB'=rowSums(select(SCL_LIB,23:28)))

#######Segmento Servicio LIB

SCL_LIB<-mutate(SCL_LIB,CTE_SV_LIB=ifelse(CAJON=="CORRIENTE" & EQ_SERV=="SERVICIO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'1.30_SV_LIB'=ifelse(CAJON=="D_01_30" & EQ_SERV=="SERVICIO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'31.60_SV_LIB'=ifelse(CAJON=="D_31_60" & EQ_SERV=="SERVICIO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'61.90_SV_LIB'=ifelse(CAJON=="D_61_90" & EQ_SERV=="SERVICIO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'91.120_SV_LIB'=ifelse(CAJON=="D_91_120" & EQ_SERV=="SERVICIO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'121.150_SV_LIB'=ifelse(CAJON=="D_121_150" & EQ_SERV=="SERVICIO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'151.180_SV_LIB'=ifelse(CAJON=="D_151_180" & EQ_SERV=="SERVICIO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'181.210_SV_LIB'=ifelse(CAJON=="D_181_210" & EQ_SERV=="SERVICIO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'211.240_SV_LIB'=ifelse(CAJON=="D_211_240"& EQ_SERV=="SERVICIO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,'241.360_SV_LIB'=ifelse(CAJON=="D_241_360" & EQ_SERV=="SERVICIO",SALDO,0))
SCL_LIB<-mutate(SCL_LIB,MAS_360_SV_LIB=ifelse(CAJON=="MAS_360" & EQ_SERV=="SERVICIO",SALDO,0))
str(SCL_LIB)

SCL_LIB<-mutate(SCL_LIB,'TOTAL_ADE_SV_LIB'=rowSums(select(SCL_LIB,33:43)))
SCL_LIB<-mutate(SCL_LIB,'1.90_SV_LIB'=rowSums(select(SCL_LIB,34:36)))
SCL_LIB<-mutate(SCL_LIB,'91.360_SV_LIB'=rowSums(select(SCL_LIB,37:42)))

SCL_LIB <- SCL_LIB %>% select(1:12,16:46)
SCL_LIB %>% summarise(SUM=sum(`MAS_360_SV_LIB`,na.rm = TRUE))

SCL_LIB <- SCL_LIB %>% group_by(COD_CLIENTE) %>% 
  summarise(CORRIENTE_LIB=sum(CORRIENTE_LIB),
            '1.30_LIB'=sum(`1.30_LIB`),
            '31.60_LIB'=sum(`31.60_LIB`),
            '61.90_LIB'=sum(`61.90_LIB`),
            '91.120_LIB'=sum(`91.120_LIB`),
            '121.150_LIB'=sum(`121.150_LIB`),
            '151.180_LIB'=sum(`151.180_LIB`),
            '181.210_LIB'=sum(`181.210_LIB`),
            '211.240_LIB'=sum(`211.240_LIB`),
            '241.360_LIB'=sum(`241.360_LIB`),
            MAS_360_LIB=sum(MAS_360_LIB),
            TOTAL_ADEUDO_LIB=sum(TOTAL_ADEUDO_LIB),
            '1.90_LIB'=sum(`1.90_LIB`),
            '91.360_LIB'=sum(`91.360_LIB`),
            CTE_EQ_LIB=sum(CTE_EQ_LIB),
            '1.30_EQ_LIB'=sum(`1.30_EQ_LIB`),
            '31.60_EQ_LIB'=sum(`31.60_EQ_LIB`),
            '61.90_EQ_LIB'=sum(`61.90_EQ_LIB`),
            '91.120_EQ_LIB'=sum(`91.120_EQ_LIB`),
            '121.150_EQ_LIB'=sum(`121.150_EQ_LIB`),
            '151.180_EQ_LIB'=sum(`151.180_EQ_LIB`),
            '181.210_EQ_LIB'=sum(`181.210_EQ_LIB`),
            '211.240_EQ_LIB'=sum(`211.240_EQ_LIB`),
            '241.360_EQ_LIB'=sum(`241.360_EQ_LIB`),
            MAS_360_EQ_LIB=sum(MAS_360_EQ_LIB),
            TOTAL_ADE_EQ_LIB=sum(TOTAL_ADE_EQ_LIB),
            '1.90_EQ_LIB'=sum(`1.90_EQ_LIB`),
            '91.360_EQ_LIB'=sum(`91.360_EQ_LIB`),
            CTE_SV_LIB=sum(CTE_SV_LIB),
            '1.30_SV_LIB'=sum(`1.30_SV_LIB`),
            '31.60_SV_LIB'=sum(`31.60_SV_LIB`),
            '61.90_SV_LIB'=sum(`61.90_SV_LIB`),
            '91.120_SV_LIB'=sum(`91.120_SV_LIB`),
            '121.150_SV_LIB'=sum(`121.150_SV_LIB`),
            '151.180_SV_LIB'=sum(`151.180_SV_LIB`),
            '181.210_SV_LIB'=sum(`181.210_SV_LIB`),
            '211.240_SV_LIB'=sum(`211.240_SV_LIB`),
            '241.360_SV_LIB'=sum(`241.360_SV_LIB`),
            MAS_360_SV_LIB=sum(MAS_360_SV_LIB),
            TOTAL_ADE_SV_LIB=sum(TOTAL_ADE_SV_LIB),
            '1.90_SV_LIB'=sum(`1.90_SV_LIB`),
            '91.360_SV_LIB'=sum(`91.360_SV_LIB`))
NET_SCL <- left_join(NET_SCL,SCL_LIB,by=NULL)

#Categoria,Ciclo, DIAS, CAJON SCL

NETDatos <- SaldosSCL %>% select(COD_CLIENTE,CAT_LARGA,CICLO_FACTURACION,DIAS,CAJON)%>% group_by(COD_CLIENTE)%>% arrange(desc(DIAS),COD_CLIENTE)
NETDatos <- NETDatos %>% filter(DIAS!=0)
NETDatos <- as.tibble(NETDatos[!duplicated(NETDatos$COD_CLIENTE), ])
names(NETDatos)[3]="CICLO"
NET_SCL <- left_join(NET_SCL,NETDatos,by=NULL)

NET_SCL %>% summarise(SALDO=sum(TOTAL_ADEUDO_T,na.rm = TRUE))
rm(CFRCTRLA,CFRCTRLIB,CifraCtrlSCL1,SaldosSCL,NETDatos,SCL_LIB,BaseSCLYear)


### Integracion Comporta SCL
#Comporta
Comporta <- read_delim("05_Final_Comportamiento_AVR_Cs_MAR21.txt", 
                       "|", escape_double = FALSE, col_types = cols(Avg_Fact = col_skip(), 
                                                                    NUM_FACTS = col_skip(), RFC = col_skip(), 
                                                                    Ult_Fec = col_skip()), trim_ws = TRUE)
NET_SCL <- left_join(NET_SCL,Comporta,by=NULL)
rm(Comporta)

# Inmunes
#Se intregra las Prorrogas e Inmunes
Prorrogas <- read_delim("Prorrogas12Abr21.txt","|",escape_double = FALSE,trim_ws = TRUE)
Prorrogas <- as.tibble(Prorrogas[!duplicated(Prorrogas$COD_CLIENTE), ])
Inmunes <- read_delim("Inmunes12Abr21.txt","|",escape_double = FALSE,trim_ws = TRUE)
Inmunes <- as.tibble(Inmunes[!duplicated(Inmunes$COD_CLIENTE), ])

NET_SCL <- left_join(NET_SCL,Prorrogas,by=NULL)
NET_SCL <- left_join(NET_SCL,Inmunes,by=NULL)
rm(Inmunes,Prorrogas)

### Lineas
Lineas <- read_delim("Lineas12Abr21.txt", "|", 
                     escape_double = FALSE, trim_ws = TRUE)
as.vector(levels(as.factor(Lineas$COD_SITUACION)))

Lineas <- Lineas %>% mutate(AAA=ifelse(COD_SITUACION=="AAA",DNS,0))
Lineas <- Lineas %>% mutate(ABP=ifelse(COD_SITUACION=="ABP",DNS,0))
Lineas <- Lineas %>% mutate(AIP=ifelse(COD_SITUACION=="AIP",DNS,0))
Lineas <- Lineas %>% mutate(AOP=ifelse(COD_SITUACION=="AOP",DNS,0))
Lineas <- Lineas %>% mutate(ATP=ifelse(COD_SITUACION=="ATP",DNS,0))
Lineas <- Lineas %>% mutate(BAA=ifelse(COD_SITUACION=="BAA",DNS,0))
Lineas <- Lineas %>% mutate(BAP=ifelse(COD_SITUACION=="BAP",DNS,0))
Lineas <- Lineas %>% mutate(CPP=ifelse(COD_SITUACION=="CPP",DNS,0))
Lineas <- Lineas %>% mutate(CSP=ifelse(COD_SITUACION=="CSP",DNS,0))
Lineas <- Lineas %>% mutate(RTP=ifelse(COD_SITUACION=="RTP",DNS,0))
Lineas <- Lineas %>% mutate(SAA=ifelse(COD_SITUACION=="SAA",DNS,0))
Lineas <- Lineas %>% mutate(STP=ifelse(COD_SITUACION=="STP",DNS,0))
Lineas <- Lineas %>% mutate(TVP=ifelse(COD_SITUACION=="TVP",DNS,0))
Lineas[3] <- NULL
Lineas[2] <- NULL
Lineas <- Lineas %>% group_by(COD_CLIENTE) %>% summarise(AAA=sum(AAA),
                                                         ABP=sum(ABP),
                                                         AIP=sum(AIP),
                                                         AOP=sum(AOP),
                                                         ATP=sum(ATP),
                                                         BAA=sum(BAA),
                                                         BAP=sum(BAP),
                                                         CPP=sum(CPP),
                                                         CSP=sum(CSP),
                                                         RTP=sum(RTP),
                                                         SAA=sum(SAA),
                                                         STP=sum(STP),
                                                         TVP=sum(TVP))

Lineas <- as_tibble(Lineas[!duplicated(Lineas$COD_CLIENTE), ])
#write.csv(Lineas,"LineasPameCierreEne.csv",row.names = FALSE)

NET_SCL <-  left_join(NET_SCL,Lineas,by=NULL)
rm(Lineas)

NET_SCL <- NET_SCL %>% mutate(ID_FUENTE=ifelse(COD_CLIENTE>=0,"SCL","NA"))

NET_SCL <- inner_join(Perimetro,NET_SCL,by=NULL)

str(NET_SCL)
#write.csv(NET_SCL,"5_May20/NET_SCL18May.csv",row.names=FALSE)
NET_SCL %>% summarise(SALDO=sum(TOTAL_ADEUDO_LIB,na.rm = TRUE))

###ONIX

## Parque Onix Conversion
OnixBDConv <- read_delim("Onix_conv13Abr21.txt","|",
                         escape_double = FALSE,
                         col_types = cols(ACCT_CODE = col_number(),
                                          TP_ACCT_KEY = col_number()), trim_ws = TRUE)
OnixBDConv <- OnixBDConv %>% select(1,2)
names(OnixBDConv)[1]="COD_CLIENTE"
Perimetro_ONIX <- inner_join(Perimetro,OnixBDConv,by=NULL)


#ONIX Saldos
ONIX <- read_delim("Onix_saldos13Abr21.txt", 
                   "|", escape_double = FALSE, trim_ws = TRUE)
names(ONIX)[1]="ACCT_CODE"
ONIX %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
Parque_Onix <- Perimetro_ONIX %>% select(ACCT_CODE)
NET_ONIX <- left_join(Parque_Onix,ONIX,by=NULL)
NET_ONIX %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
names(NET_ONIX)[10]="EQ_SERV"

### ENCAJONADO ONIX
NET_ONIX<-mutate(NET_ONIX,CORRIENTE_T=ifelse(CAJON=="CORRIENTE",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'1.30_T'=ifelse(CAJON=="D_01_30",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'31.60_T'=ifelse(CAJON=="D_31_60",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'61.90_T'=ifelse(CAJON=="D_61_90",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'91.120_T'=ifelse(CAJON=="D_91_120",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'121.150_T'=ifelse(CAJON=="D_121_150",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'151.180_T'=ifelse(CAJON=="D_151_180",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'181.210_T'=ifelse(CAJON=="D_181_210",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'211.240_T'=ifelse(CAJON=="D_211_240",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'241.360_T'=ifelse(CAJON=="D_241_360",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,MAS_360_T=ifelse(CAJON=="MAS_360",SALDO,0))
str(NET_ONIX)

NET_ONIX<-mutate(NET_ONIX,'TOTAL_ADEUDO_T'=rowSums(select(NET_ONIX,19:29)))
NET_ONIX<-mutate(NET_ONIX,'TOTAL_VENCIDO_T'=rowSums(select(NET_ONIX,20:29)))
NET_ONIX<-mutate(NET_ONIX,'1.90_T'=rowSums(select(NET_ONIX,20:22)))
NET_ONIX<-mutate(NET_ONIX,'91.360_T'=rowSums(select(NET_ONIX,23:28)))


## Segmento Equipo ONIX
NET_ONIX<-mutate(NET_ONIX,CTE_EQ_T=ifelse(CAJON=="CORRIENTE" & EQ_SERV=="EQUIPO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'1.30_EQ_T'=ifelse(CAJON=="D_01_30" & EQ_SERV=="EQUIPO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'31.60_EQ_T'=ifelse(CAJON=="D_31_60" & EQ_SERV=="EQUIPO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'61.90_EQ_T'=ifelse(CAJON=="D_61_90" & EQ_SERV=="EQUIPO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'91.120_EQ_T'=ifelse(CAJON=="D_91_120" & EQ_SERV=="EQUIPO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'121.150_EQ_T'=ifelse(CAJON=="D_121_150" & EQ_SERV=="EQUIPO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'151.180_EQ_T'=ifelse(CAJON=="D_151_180" & EQ_SERV=="EQUIPO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'181.210_EQ_T'=ifelse(CAJON=="D_181_210" & EQ_SERV=="EQUIPO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'211.240_EQ_T'=ifelse(CAJON=="D_211_240"& EQ_SERV=="EQUIPO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'241.360_EQ_T'=ifelse(CAJON=="D_241_360" & EQ_SERV=="EQUIPO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,MAS_360_EQ_T=ifelse(CAJON=="MAS_360" & EQ_SERV=="EQUIPO",SALDO,0))
str(NET_ONIX)

NET_ONIX<-mutate(NET_ONIX,'TOTAL_ADE_EQ_T'=rowSums(select(NET_ONIX,34:44)))
NET_ONIX<-mutate(NET_ONIX,'1.90_EQ_T'=rowSums(select(NET_ONIX,35:37)))
NET_ONIX<-mutate(NET_ONIX,'91.360_EQ_T'=rowSums(select(NET_ONIX,38:43)))

#######Segmento Servicio ONIX

NET_ONIX<-mutate(NET_ONIX,CTE_SV_T=ifelse(CAJON=="CORRIENTE" & EQ_SERV=="SERVICIO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'1.30_SV_T'=ifelse(CAJON=="D_01_30" & EQ_SERV=="SERVICIO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'31.60_SV_T'=ifelse(CAJON=="D_31_60" & EQ_SERV=="SERVICIO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'61.90_SV_T'=ifelse(CAJON=="D_61_90" & EQ_SERV=="SERVICIO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'91.120_SV_T'=ifelse(CAJON=="D_91_120" & EQ_SERV=="SERVICIO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'121.150_SV_T'=ifelse(CAJON=="D_121_150" & EQ_SERV=="SERVICIO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'151.180_SV_T'=ifelse(CAJON=="D_151_180" & EQ_SERV=="SERVICIO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'181.210_SV_T'=ifelse(CAJON=="D_181_210" & EQ_SERV=="SERVICIO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'211.240_SV_T'=ifelse(CAJON=="D_211_240"& EQ_SERV=="SERVICIO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,'241.360_SV_T'=ifelse(CAJON=="D_241_360" & EQ_SERV=="SERVICIO",SALDO,0))
NET_ONIX<-mutate(NET_ONIX,MAS_360_SV_T=ifelse(CAJON=="MAS_360" & EQ_SERV=="SERVICIO",SALDO,0))
str(NET_ONIX)

NET_ONIX<-mutate(NET_ONIX,'TOTAL_ADE_SV_T'=rowSums(select(NET_ONIX,48:58)))
NET_ONIX<-mutate(NET_ONIX,'1.90_SV_T'=rowSums(select(NET_ONIX,49:51)))
NET_ONIX<-mutate(NET_ONIX,'91.360_SV_T'=rowSums(select(NET_ONIX,52:57)))

##FIN CAJONES ONIX

#Sumarizado ONIX TOTALES
NET_ONIX <- NET_ONIX %>% select(1,19:61) %>% group_by(ACCT_CODE) %>% 
  summarise(CORRIENTE_T=sum(CORRIENTE_T),
            '1.30_T'=sum(`1.30_T`),
            '31.60_T'=sum(`31.60_T`),
            '61.90_T'=sum(`61.90_T`),
            '91.120_T'=sum(`91.120_T`),
            '121.150_T'=sum(`121.150_T`),
            '151.180_T'=sum(`151.180_T`),
            '181.210_T'=sum(`181.210_T`),
            '211.240_T'=sum(`211.240_T`),
            '241.360_T'=sum(`241.360_T`),
            MAS_360_T=sum(MAS_360_T),
            TOTAL_ADEUDO_T=sum(TOTAL_ADEUDO_T),
            TOTAL_VENCIDO_T=sum(TOTAL_VENCIDO_T),
            '1.90_T'=sum(`1.90_T`),
            '91.360_T'=sum(`91.360_T`),
            CTE_EQ_T=sum(CTE_EQ_T),
            '1.30_EQ_T'=sum(`1.30_EQ_T`),
            '31.60_EQ_T'=sum(`31.60_EQ_T`),
            '61.90_EQ_T'=sum(`61.90_EQ_T`),
            '91.120_EQ_T'=sum(`91.120_EQ_T`),
            '121.150_EQ_T'=sum(`121.150_EQ_T`),
            '151.180_EQ_T'=sum(`151.180_EQ_T`),
            '181.210_EQ_T'=sum(`181.210_EQ_T`),
            '211.240_EQ_T'=sum(`211.240_EQ_T`),
            '241.360_EQ_T'=sum(`241.360_EQ_T`),
            MAS_360_EQ_T=sum(MAS_360_EQ_T),
            TOTAL_ADE_EQ_T=sum(TOTAL_ADE_EQ_T),
            '1.90_EQ_T'=sum(`1.90_EQ_T`),
            '91.360_EQ_T'=sum(`91.360_EQ_T`),
            CTE_SV_T=sum(CTE_SV_T),
            '1.30_SV_T'=sum(`1.30_SV_T`),
            '31.60_SV_T'=sum(`31.60_SV_T`),
            '61.90_SV_T'=sum(`61.90_SV_T`),
            '91.120_SV_T'=sum(`91.120_SV_T`),
            '121.150_SV_T'=sum(`121.150_SV_T`),
            '151.180_SV_T'=sum(`151.180_SV_T`),
            '181.210_SV_T'=sum(`181.210_SV_T`),
            '211.240_SV_T'=sum(`211.240_SV_T`),
            '241.360_SV_T'=sum(`241.360_SV_T`),
            MAS_360_SV_T=sum(MAS_360_SV_T),
            TOTAL_ADE_SV_T=sum(TOTAL_ADE_SV_T),
            '1.90_SV_T'=sum(`1.90_SV_T`),
            '91.360_SV_T'=sum(`91.360_SV_T`))

NET_ONIX <- NET_ONIX %>% filter(TOTAL_ADEUDO_T!=0)
NET_ONIX <- NET_ONIX %>% mutate(ID_FUENTE=ifelse(ACCT_CODE>=0,"ONIX","NA"))
NET_ONIX %>% summarise(SALDO=sum(TOTAL_ADEUDO_T,na.rm = TRUE))


#Categoria,Ciclo, DIAS, CAJON SCL NET_ONIX
str(ONIX)
OnixDatos <- ONIX %>% select(ACCT_CODE,DIA_MORA,CAJON,CICLO) %>% arrange(desc(DIA_MORA),ACCT_CODE) 
OnixDatos <- as.tibble(OnixDatos[!duplicated(OnixDatos$ACCT_CODE), ])
names(OnixDatos)[2]="DIAS"

NET_ONIX <- inner_join(Perimetro_ONIX,NET_ONIX,by=NULL)
## FIN ONIX TOTAL
###ONIX LIB
ONIX_LIB <- inner_join(Parque_Onix,ONIX,by=NULL)
ONIX_LIB <- ONIX_LIB %>% filter(LIQUIDACION=="OTRO")
ONIX_LIB %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))

##ACOMODO CAJONES LIB
ONIX_LIB<-mutate(ONIX_LIB,CORRIENTE_LIB=ifelse(CAJON=="CORRIENTE",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'1.30_LIB'=ifelse(CAJON=="D_01_30",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'31.60_LIB'=ifelse(CAJON=="D_31_60",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'61.90_LIB'=ifelse(CAJON=="D_61_90",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'91.120_LIB'=ifelse(CAJON=="D_91_120",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'121.150_LIB'=ifelse(CAJON=="D_121_150",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'151.180_LIB'=ifelse(CAJON=="D_151_180",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'181.210_LIB'=ifelse(CAJON=="D_181_210",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'211.240_LIB'=ifelse(CAJON=="D_211_240",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'241.360_LIB'=ifelse(CAJON=="D_241_360",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,MAS_360_LIB=ifelse(CAJON=="MAS_360",SALDO,0))
str(ONIX_LIB)

ONIX_LIB <- ONIX_LIB %>% select(1,8,10,18:29)
ONIX_LIB<-mutate(ONIX_LIB,'TOTAL_ADEUDO_LIB'=rowSums(select(ONIX_LIB,5:15)))
ONIX_LIB<-mutate(ONIX_LIB,'1.90_LIB'=rowSums(select(ONIX_LIB,6:8)))
ONIX_LIB<-mutate(ONIX_LIB,'91.360_LIB'=rowSums(select(ONIX_LIB,9:14)))
names(ONIX_LIB)[3]="EQ_SERV"

## Segmento Equipo LIB
ONIX_LIB<-mutate(ONIX_LIB,CTE_EQ_LIB=ifelse(CAJON=="CORRIENTE" & EQ_SERV=="EQUIPO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'1.30_EQ_LIB'=ifelse(CAJON=="D_01_30" & EQ_SERV=="EQUIPO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'31.60_EQ_LIB'=ifelse(CAJON=="D_31_60" & EQ_SERV=="EQUIPO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'61.90_EQ_LIB'=ifelse(CAJON=="D_61_90" & EQ_SERV=="EQUIPO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'91.120_EQ_LIB'=ifelse(CAJON=="D_91_120" & EQ_SERV=="EQUIPO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'121.150_EQ_LIB'=ifelse(CAJON=="D_121_150" & EQ_SERV=="EQUIPO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'151.180_EQ_LIB'=ifelse(CAJON=="D_151_180" & EQ_SERV=="EQUIPO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'181.210_EQ_LIB'=ifelse(CAJON=="D_181_210" & EQ_SERV=="EQUIPO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'211.240_EQ_LIB'=ifelse(CAJON=="D_211_240"& EQ_SERV=="EQUIPO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'241.360_EQ_LIB'=ifelse(CAJON=="D_241_360" & EQ_SERV=="EQUIPO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,MAS_360_EQ_LIB=ifelse(CAJON=="MAS_360" & EQ_SERV=="EQUIPO",SALDO,0))
str(ONIX_LIB)

ONIX_LIB<-mutate(ONIX_LIB,'TOTAL_ADE_EQ_LIB'=rowSums(select(ONIX_LIB,19:29)))
ONIX_LIB<-mutate(ONIX_LIB,'1.90_EQ_LIB'=rowSums(select(ONIX_LIB,20:22)))
ONIX_LIB<-mutate(ONIX_LIB,'91.360_EQ_LIB'=rowSums(select(ONIX_LIB,23:28)))

#######Segmento Servicio LIB

ONIX_LIB<-mutate(ONIX_LIB,CTE_SV_LIB=ifelse(CAJON=="CORRIENTE" & EQ_SERV=="SERVICIO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'1.30_SV_LIB'=ifelse(CAJON=="D_01_30" & EQ_SERV=="SERVICIO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'31.60_SV_LIB'=ifelse(CAJON=="D_31_60" & EQ_SERV=="SERVICIO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'61.90_SV_LIB'=ifelse(CAJON=="D_61_90" & EQ_SERV=="SERVICIO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'91.120_SV_LIB'=ifelse(CAJON=="D_91_120" & EQ_SERV=="SERVICIO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'121.150_SV_LIB'=ifelse(CAJON=="D_121_150" & EQ_SERV=="SERVICIO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'151.180_SV_LIB'=ifelse(CAJON=="D_151_180" & EQ_SERV=="SERVICIO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'181.210_SV_LIB'=ifelse(CAJON=="D_181_210" & EQ_SERV=="SERVICIO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'211.240_SV_LIB'=ifelse(CAJON=="D_211_240"& EQ_SERV=="SERVICIO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,'241.360_SV_LIB'=ifelse(CAJON=="D_241_360" & EQ_SERV=="SERVICIO",SALDO,0))
ONIX_LIB<-mutate(ONIX_LIB,MAS_360_SV_LIB=ifelse(CAJON=="MAS_360" & EQ_SERV=="SERVICIO",SALDO,0))
str(ONIX_LIB)

ONIX_LIB<-mutate(ONIX_LIB,'TOTAL_ADE_SV_LIB'=rowSums(select(ONIX_LIB,33:43)))
ONIX_LIB<-mutate(ONIX_LIB,'1.90_SV_LIB'=rowSums(select(ONIX_LIB,34:36)))
ONIX_LIB<-mutate(ONIX_LIB,'91.360_SV_LIB'=rowSums(select(ONIX_LIB,37:42)))

ONIX_LIB <- ONIX_LIB %>% select(1,5:46)

ONIX_LIB %>% summarise(SUM=sum(`MAS_360_SV_LIB`,na.rm = TRUE))

ONIX_LIB <- ONIX_LIB %>% group_by(ACCT_CODE) %>% 
  summarise(CORRIENTE_LIB=sum(CORRIENTE_LIB),
            '1.30_LIB'=sum(`1.30_LIB`),
            '31.60_LIB'=sum(`31.60_LIB`),
            '61.90_LIB'=sum(`61.90_LIB`),
            '91.120_LIB'=sum(`91.120_LIB`),
            '121.150_LIB'=sum(`121.150_LIB`),
            '151.180_LIB'=sum(`151.180_LIB`),
            '181.210_LIB'=sum(`181.210_LIB`),
            '211.240_LIB'=sum(`211.240_LIB`),
            '241.360_LIB'=sum(`241.360_LIB`),
            MAS_360_LIB=sum(MAS_360_LIB),
            TOTAL_ADEUDO_LIB=sum(TOTAL_ADEUDO_LIB),
            '1.90_LIB'=sum(`1.90_LIB`),
            '91.360_LIB'=sum(`91.360_LIB`),
            CTE_EQ_LIB=sum(CTE_EQ_LIB),
            '1.30_EQ_LIB'=sum(`1.30_EQ_LIB`),
            '31.60_EQ_LIB'=sum(`31.60_EQ_LIB`),
            '61.90_EQ_LIB'=sum(`61.90_EQ_LIB`),
            '91.120_EQ_LIB'=sum(`91.120_EQ_LIB`),
            '121.150_EQ_LIB'=sum(`121.150_EQ_LIB`),
            '151.180_EQ_LIB'=sum(`151.180_EQ_LIB`),
            '181.210_EQ_LIB'=sum(`181.210_EQ_LIB`),
            '211.240_EQ_LIB'=sum(`211.240_EQ_LIB`),
            '241.360_EQ_LIB'=sum(`241.360_EQ_LIB`),
            MAS_360_EQ_LIB=sum(MAS_360_EQ_LIB),
            TOTAL_ADE_EQ_LIB=sum(TOTAL_ADE_EQ_LIB),
            '1.90_EQ_LIB'=sum(`1.90_EQ_LIB`),
            '91.360_EQ_LIB'=sum(`91.360_EQ_LIB`),
            CTE_SV_LIB=sum(CTE_SV_LIB),
            '1.30_SV_LIB'=sum(`1.30_SV_LIB`),
            '31.60_SV_LIB'=sum(`31.60_SV_LIB`),
            '61.90_SV_LIB'=sum(`61.90_SV_LIB`),
            '91.120_SV_LIB'=sum(`91.120_SV_LIB`),
            '121.150_SV_LIB'=sum(`121.150_SV_LIB`),
            '151.180_SV_LIB'=sum(`151.180_SV_LIB`),
            '181.210_SV_LIB'=sum(`181.210_SV_LIB`),
            '211.240_SV_LIB'=sum(`211.240_SV_LIB`),
            '241.360_SV_LIB'=sum(`241.360_SV_LIB`),
            MAS_360_SV_LIB=sum(MAS_360_SV_LIB),
            TOTAL_ADE_SV_LIB=sum(TOTAL_ADE_SV_LIB),
            '1.90_SV_LIB'=sum(`1.90_SV_LIB`),
            '91.360_SV_LIB'=sum(`91.360_SV_LIB`))

NET_ONIX <- left_join(NET_ONIX,ONIX_LIB,by=NULL)
NET_ONIX <- left_join(NET_ONIX,OnixDatos,by="ACCT_CODE")
rm(ONIX,ONIX_LIB,OnixDatos,Parque)
str(NET_ONIX)
rm(OnixBDConv,Parque_Onix,Perimetro_ONIX)


### Carga BD SAP_HORACIO
SALDOS_SAP <- read_excel("CARTERA 12 ABRIL 2021.xlsx", 
                         sheet = "SAP_SALDOS")
names(SALDOS_SAP)[1] <- "COD_CLIENTE"
names(SALDOS_SAP)[2] <- "SUBSIDIARIA"

PerimetroSAP <- Perimetro %>% select(COD_CLIENTE,HOLDING)
SALDOS_SAP <- left_join(SALDOS_SAP,PerimetroSAP,by=NULL)


SALDOS_SAP <- mutate(SALDOS_SAP,HOLDING=ifelse(is.na(HOLDING),SUBSIDIARIA,HOLDING))

SALDOS_SAP <- mutate(SALDOS_SAP,COD_CUENTA=SALDOS_SAP$COD_CLIENTE)
SALDOS_SAP <- mutate(SALDOS_SAP,ID_FUENTE=ifelse(COD_CLIENTE>=1,"SAP",0))

SALDOS_SAP <- SALDOS_SAP %>% select(COD_CLIENTE,COD_CUENTA,HOLDING,SUBSIDIARIA,ID_FUENTE,3:13)

###  
DATOS_SAP <- read_excel("CARTERA 12 ABRIL 2021.xlsx", 
                                    sheet = "SAP_DATOS")
DATOS_SAP <- DATOS_SAP %>% select(Cuenta,Días,Cajón,`Nº ident.fis.1`) %>% arrange(desc(Días))
DATOS_SAP <- as.tibble(DATOS_SAP[!duplicated(DATOS_SAP$Cuenta), ])
names(DATOS_SAP)[1]="COD_CLIENTE"
names(DATOS_SAP)[2]="DIAS"
names(DATOS_SAP)[3]="CAJON"
names(DATOS_SAP)[4]="RFC"

SALDOS_SAP <- left_join(SALDOS_SAP,DATOS_SAP,by=c("COD_CLIENTE"))
SALDOS_SAP <- SALDOS_SAP %>% select(1:5,RFC,DIAS,CAJON,6:16)
rm(DATOS_SAP)
names(SALDOS_SAP)[9]="CORRIENTE_T"
names(SALDOS_SAP)[10]='1.30_T'
names(SALDOS_SAP)[11]='31.60_T'
names(SALDOS_SAP)[12]='61.90_T'
names(SALDOS_SAP)[13]='91.120_T'
names(SALDOS_SAP)[14]='121.150_T'
names(SALDOS_SAP)[15]='151.180_T'
names(SALDOS_SAP)[16]='181.210_T'
names(SALDOS_SAP)[17]='211.240_T'
names(SALDOS_SAP)[18]='241.360_T'
names(SALDOS_SAP)[19]="MAS_360_T"

SALDOS_SAP<-mutate(SALDOS_SAP,'TOTAL_ADEUDO_T'=rowSums(select(SALDOS_SAP,9:19),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'TOTAL_VENCIDO_T'=rowSums(select(SALDOS_SAP,10:19),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'1.90_T'=rowSums(select(SALDOS_SAP,10:12),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'91.360_T'=rowSums(select(SALDOS_SAP,13:18),na.rm = TRUE))

## Segmento Equipo 
SALDOS_SAP<-mutate(SALDOS_SAP,CTE_EQ_T=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'1.30_EQ_T'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'31.60_EQ_T'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'61.90_EQ_T'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'91.120_EQ_T'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'121.150_EQ_T'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'151.180_EQ_T'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'181.210_EQ_T'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'211.240_EQ_T'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'241.360_EQ_T'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,MAS_360_EQ_T=0)
str(SALDOS_SAP)

SALDOS_SAP<-mutate(SALDOS_SAP,'TOTAL_ADE_EQ_T'=rowSums(select(SALDOS_SAP,24:34),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'1.90_EQ_T'=rowSums(select(SALDOS_SAP,25:27),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'91.360_EQ_T'=rowSums(select(SALDOS_SAP,28:33),na.rm = TRUE))

## Segmento Servicio
SALDOS_SAP<-mutate(SALDOS_SAP,CTE_SV_T=SALDOS_SAP$CORRIENTE_T)
SALDOS_SAP<-mutate(SALDOS_SAP,'1.30_SV_T'=SALDOS_SAP$`1.30_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'31.60_SV_T'=SALDOS_SAP$`31.60_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'61.90_SV_T'=SALDOS_SAP$`61.90_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'91.120_SV_T'=SALDOS_SAP$`91.120_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'121.150_SV_T'=SALDOS_SAP$`121.150_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'151.180_SV_T'=SALDOS_SAP$`151.180_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'181.210_SV_T'=SALDOS_SAP$`181.210_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'211.240_SV_T'=SALDOS_SAP$`211.240_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'241.360_SV_T'=SALDOS_SAP$`241.360_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,MAS_360_SV_T=SALDOS_SAP$MAS_360_T)
str(SALDOS_SAP)

SALDOS_SAP<-mutate(SALDOS_SAP,'TOTAL_ADE_SV_T'=rowSums(select(SALDOS_SAP,38:48),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'1.90_SV_T'=rowSums(select(SALDOS_SAP,39:41),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'91.360_SV_T'=rowSums(select(SALDOS_SAP,42:47),na.rm = TRUE))

## ACOMODO CAJONES LIB
SALDOS_SAP<-mutate(SALDOS_SAP,CORRIENTE_LIB=SALDOS_SAP$CORRIENTE_T)
SALDOS_SAP<-mutate(SALDOS_SAP,'1.30_LIB'=SALDOS_SAP$`1.30_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'31.60_LIB'=SALDOS_SAP$`31.60_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'61.90_LIB'=SALDOS_SAP$`61.90_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'91.120_LIB'=SALDOS_SAP$`91.120_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'121.150_LIB'=SALDOS_SAP$`121.150_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'151.180_LIB'=SALDOS_SAP$`151.180_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'181.210_LIB'=SALDOS_SAP$`181.210_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'211.240_LIB'=SALDOS_SAP$`211.240_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'241.360_LIB'=SALDOS_SAP$`241.360_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,MAS_360_LIB=SALDOS_SAP$MAS_360_T)
str(SALDOS_SAP)

SALDOS_SAP<-mutate(SALDOS_SAP,'TOTAL_ADEUDO_LIB'=rowSums(select(SALDOS_SAP,52:62),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'1.90_LIB'=rowSums(select(SALDOS_SAP,53:55),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'91.360_LIB'=rowSums(select(SALDOS_SAP,56:61),na.rm = TRUE))

## Segmento Equipo LIB
SALDOS_SAP<-mutate(SALDOS_SAP,CTE_EQ_LIB=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'1.30_EQ_LIB'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'31.60_EQ_LIB'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'61.90_EQ_LIB'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'91.120_EQ_LIB'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'121.150_EQ_LIB'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'151.180_EQ_LIB'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'181.210_EQ_LIB'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'211.240_EQ_LIB'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,'241.360_EQ_LIB'=0)
SALDOS_SAP<-mutate(SALDOS_SAP,MAS_360_EQ_LIB=0)
str(SALDOS_SAP)

SALDOS_SAP<-mutate(SALDOS_SAP,'TOTAL_ADE_EQ_LIB'=rowSums(select(SALDOS_SAP,66:76),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'1.90_EQ_LIB'=rowSums(select(SALDOS_SAP,67:69),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'91.360_EQ_LIB'=rowSums(select(SALDOS_SAP,70:75),na.rm = TRUE))

##Segmento Servicio LIB

SALDOS_SAP<-mutate(SALDOS_SAP,CTE_SV_LIB=SALDOS_SAP$CORRIENTE_T)
SALDOS_SAP<-mutate(SALDOS_SAP,'1.30_SV_LIB'=SALDOS_SAP$`1.30_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'31.60_SV_LIB'=SALDOS_SAP$`31.60_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'61.90_SV_LIB'=SALDOS_SAP$`61.90_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'91.120_SV_LIB'=SALDOS_SAP$`91.120_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'121.150_SV_LIB'=SALDOS_SAP$`121.150_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'151.180_SV_LIB'=SALDOS_SAP$`151.180_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'181.210_SV_LIB'=SALDOS_SAP$`181.210_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'211.240_SV_LIB'=SALDOS_SAP$`211.240_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,'241.360_SV_LIB'=SALDOS_SAP$`241.360_T`)
SALDOS_SAP<-mutate(SALDOS_SAP,MAS_360_SV_LIB=SALDOS_SAP$MAS_360_T)
str(SALDOS_SAP)

SALDOS_SAP<-mutate(SALDOS_SAP,'TOTAL_ADE_SV_LIB'=rowSums(select(SALDOS_SAP,80:90),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'1.90_SV_LIB'=rowSums(select(SALDOS_SAP,81:83),na.rm = TRUE))
SALDOS_SAP<-mutate(SALDOS_SAP,'91.360_SV_LIB'=rowSums(select(SALDOS_SAP,84:89),na.rm = TRUE))
## Fin SAP

#Eliminar SAP de SCL
SAP_R <-SALDOS_SAP %>% select(COD_CLIENTE,ID_FUENTE) 
names(SAP_R)[2] <- "ID"
NET_SCL <- left_join(NET_SCL,SAP_R,by=NULL)
NET_SCL <- NET_SCL %>% filter(is.na(NET_SCL$ID))
NET_SCL <- NET_SCL %>% select(1:118)
#write.csv(NET_SCL,"10_Oct/NET_SCL21Oct.csv",row.names=FALSE)

#############################FIN TOTALES Y LIB

##CARTERAS
CARTERA_SCL <-  NET_SCL %>% select(PERIMETRO,SUBSEGMENTO,IOT,COD_CLIENTE,COD_CUENTA,HOLDING,SUBSIDIARIA,ID_FUENTE,RFC,10:95)
CARTERA_SCL$COD_CLIENTE <-as.character(CARTERA_SCL$COD_CLIENTE)
CARTERA_SAP <- SALDOS_SAP %>% select(1:6,9:93)
CARTERA_SAP$COD_CLIENTE <-as.character(CARTERA_SAP$COD_CLIENTE)
Perimetro_SAP <- Perimetro %>% select(1,3,4,9)
Perimetro_SAP$COD_CLIENTE <- as.character(Perimetro_SAP$COD_CLIENTE)
CARTERA_SAP <- left_join(CARTERA_SAP,Perimetro_SAP,by="COD_CLIENTE")
CARTERA_ONIX <- NET_ONIX %>%select(PERIMETRO,SUBSEGMENTO,IOT,COD_CLIENTE,COD_CUENTA,HOLDING,SUBSIDIARIA,ID_FUENTE,RFC,11:53,55:96)
CARTERA_ONIX$COD_CLIENTE <- as.character(CARTERA_ONIX$COD_CLIENTE)
CARTERA <- full_join(CARTERA_SCL,CARTERA_SAP,by=NULL)
CARTERA <- full_join(CARTERA,CARTERA_ONIX,by=NULL)
CARTERA$COD_CUENTA <-as.character(CARTERA$COD_CUENTA)
rm(CARTERA_ONIX,CARTERA_SAP,CARTERA_SCL,Perimetro_SAP)

## NET
str(NET_SCL)
NET_SCL$COD_CLIENTE <-as.character(NET_SCL$COD_CLIENTE)
NET_SCL$COD_CUENTA <-as.character(NET_SCL$COD_CUENTA)
NET_ONIX$COD_CLIENTE <-as.character(NET_ONIX$COD_CLIENTE)
NET_ONIX$COD_CUENTA <-as.character(NET_ONIX$COD_CUENTA)
SALDOS_SAP$COD_CLIENTE <-as.character(SALDOS_SAP$COD_CLIENTE)
SALDOS_SAP$COD_CUENTA <-as.character(SALDOS_SAP$COD_CUENTA)

NET <- NET_SCL %>% select(PERIMETRO,SUBSEGMENTO,IOT,CAT_LARGA,COD_CLIENTE,CICLO,COD_CUENTA,Comporta,HOLDING,SUBSIDIARIA,TIPO_CLIENTE,ID_FUENTE,RFC,PERIMETRO,DIAS,CAJON,10:22,Castigo,FEC_DEUDVENC,DIA_PRORROGA,FEC_PRORROGA,INMUNE,105:117,25:52,53:95)
NET <- full_join(NET,NET_ONIX,by=NULL)
NET <- full_join(NET,SALDOS_SAP,by=NULL)
NET <- NET %>% select(1:116)

##### SALIDAS

SalCARTERA <- paste('','16_Cartera',as.character(Sys.Date(),"%d%m%Y"),'.csv',sep = "")
SalNET <- paste('','15_NETFLOW',as.character(Sys.Date(),"%d%m%Y"),'.csv',sep = "")
write.csv(CARTERA,file=SalCARTERA,na="0",row.names = FALSE)
write.csv(NET,file=SalNET,na="0",row.names = FALSE)
rm(list=ls())

###########################FIN PROGRAMA

#Prototipo para diferencias
library(readr)
CarteraActual <- read_csv("16_Cartera14042021.csv",locale = locale(encoding = "ISO-8859-1", asciify = TRUE))
CarteraActual <- unite(CarteraActual,ID_DIF,c(4,8),sep="-",remove = FALSE)

CarteraAnterior <- read_csv("16_Cartera06042021.csv",locale = locale(encoding = "ISO-8859-1", asciify = TRUE))
CarteraAnterior <- unite(CarteraAnterior,ID_DIF,c(4,8),sep="-",remove = FALSE)
CarteraAnterior <- CarteraAnterior %>% select(4,55:96)
Cartera_DIF <- left_join(CarteraActual,CarteraAnterior,by="ID_DIF")

SalCARTERA_DIF <- paste('','16_CarteraDIF',as.character(Sys.Date(),"%d%m%Y"),'.csv',sep = "")
write.csv(Cartera_DIF,file=SalCARTERA_DIF,na="0",row.names = FALSE)


table(as.factor(Cartera_DIF$ID_FUENTE))


#Check de Catergorias SCL

CategoriasFull <- read_delim("categ12abril21.txt", 
                             "|", escape_double = FALSE, trim_ws = TRUE)

NET_CAT <- left_join(Perimetro,CategoriasFull,by=c("COD_CLIENTE"))
SalNET <- paste('','15_NETFLOWCAT',as.character(Sys.Date(),"%d%m%Y"),'.csv',sep = "")
write.csv(NET_CAT,file=SalNET,na="0",row.names = FALSE)