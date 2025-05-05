##### 
#1. CARTERA SCL BD ORACLE (DESCARGAR el 1 día de mes por la mañana)

### 2. Parque (Perimetro), Cartera Sergio y Carga Consulta SCL
Perimetro <- read_delim("Z:/POSPAGO/Cobranza/Actualizacion BD b2b (cobranza)/2019_Cartera_B2B.txt",
                        "\t", escape_double = FALSE, trim_ws = TRUE)
##3. Carga Cartera inicial Archivo Sergio
CarteraUnificada <- read_excel("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/CarteraUnificada.xlsx")
names(CarteraUnificada)[1]="SEGMENTO_ANTE"
names(CarteraUnificada)[2]="SEGMENTO"
## 4. Join Archivo Sergio vs Perimetro Post-Venta
CarteraUnificada <- full_join(CarteraUnificada,Perimetro,by=c("COD_CLIENTE","COD_CLIENTE"))
CarteraUnificada <- tbl_df(CarteraUnificada[!duplicated(CarteraUnificada$COD_CLIENTE),])


ClientesSCL_ONIX <- read_delim("Salidas/ClientesSCL_ONIX.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE)
ClientesSCL_ONIX %>% filter(COD_CLIENTE==16539792)

Libro3 <- read_excel("Salidas/Libro3.xlsx")
Lib <- left_join(Libro3,ClientesSCL_ONIX,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Lib,file = "Salidas/Onix.csv",row.names=FALSE)

### 5.Carga de BD SALDOS
SAP <- read_excel("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/7_Jul/SALDOS DATA, FULL PRICE Y M2M AL 16 DE JULIO.xlsx", 
                  sheet = "BASECRUCE")
names(SAP)[2]="HOLDING"

CarteraSCL <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/7_Jul/CarteraUniSCL1_8_19.txt", 
                            "|", escape_double = FALSE, trim_ws = TRUE)
CarteraSCL <- read_delim("Victor/10_CobranzaPymes/Co_cartera14Ago.txt","|", escape_double = FALSE, trim_ws = TRUE)
str(CarteraSCL)
levels(as.factor(CarteraSCL$CAJONM))



##6.Cruce COD_CLIENTE; NUM_ABONADO; PENALIZACION
Pena <- read_delim(file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/7_Jul/PENAL1_8_19.txt","|",escape_double = FALSE,trim_ws = TRUE)
Pena <- unite(Pena,NRO_DOCUMENTO_FISCAL,c(9:10),sep="     ",remove = TRUE)
Pena <- Pena %>% select(9,4)
str(Pena)

#Join Tercera a mas
Abr <- read_delim(file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/4Abr19/Tercera_a_mas_20190430_CAbril.txt","|",escape_double = FALSE,trim_ws = TRUE)
May <- read_delim(file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/5May19/Tercera_a_mas_May19_20190531_C.txt","|",escape_double = FALSE,trim_ws = TRUE)
Jun <- read_delim(file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/6Jun19/Tercera_a_mas_Jun19_20190630_C.txt","|",escape_double = FALSE,trim_ws = TRUE)
Jul <- read_delim(file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/7Jul19/Tercera_a_mas_Jul19_20190731_C.txt","|",escape_double = FALSE,trim_ws = TRUE)
a <- full_join(Abr,May,by=NULL)
b <- full_join(a,Jun,by=NULL)
T3era <- full_join(b,Jul,by=NULL)
rm(Abr,May,Jun,Jul,a,b)

T3era <- T3era %>% select(NRO_DOCUMENTO_FISCAL,NUM_FACT_MORA,CVE_CLIENTE,ID_CLIENTE,ID_FUENTE)

str(T3era)
T3era%>% filter(NRO_DOCUMENTO_FISCAL=="CHSFC     763116")

##Segmentacion de la BD TOTAL

CarteraSCL <-  select(CarteraSCL,1:10,13:18)
names(CarteraSCL)[1]="COD_CLIENTE"

CarteraSCL <- Cartera22Ago

#Agrupacion y sumarizado
## Segmento Totales
CarteraSCL<-mutate(CarteraSCL,CORRIENTE_T=ifelse(CAJON=="CORRIENTE",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'1.30_T'=ifelse(CAJON=="D_01_30",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'31.60_T'=ifelse(CAJON=="D_31_60",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'61.90_T'=ifelse(CAJON=="D_61_90",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'91.120_T'=ifelse(CAJON=="D_91_120",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'121.150_T'=ifelse(CAJON=="D_121_150",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'151.180_T'=ifelse(CAJON=="D_151_180",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'181.210_T'=ifelse(CAJON=="D_181_210",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'211.240_T'=ifelse(CAJON=="D_211_240",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'241.360_T'=ifelse(CAJON=="D_241_360",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,MAS_360_T=ifelse(CAJON=="MAS_360",SALDO,0))

str(CarteraSCL)
CarteraSCL<-mutate(CarteraSCL,'TOTAL_ADEUDO_T'=rowSums(select(CarteraSCL,17:27)))
CarteraSCL<-mutate(CarteraSCL,'1.90_T'=rowSums(select(CarteraSCL,18:20)))
CarteraSCL<-mutate(CarteraSCL,'91.360_T'=rowSums(select(CarteraSCL,21:26)))

## Segmento Equipo
CarteraSCL<-mutate(CarteraSCL,CTE_EQ=ifelse(CAJON=="CORRIENTE" & EQ_SERV=="EQUIPO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'1.30_EQ'=ifelse(CAJON=="D_01_30" & EQ_SERV=="EQUIPO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'31.60_EQ'=ifelse(CAJON=="D_31_60" & EQ_SERV=="EQUIPO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'61.90_EQ'=ifelse(CAJON=="D_61_90" & EQ_SERV=="EQUIPO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'91.120_EQ'=ifelse(CAJON=="D_91_120" & EQ_SERV=="EQUIPO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'121.150_EQ'=ifelse(CAJON=="D_121_150" & EQ_SERV=="EQUIPO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'151.180_EQ'=ifelse(CAJON=="D_151_180" & EQ_SERV=="EQUIPO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'181.210_EQ'=ifelse(CAJON=="D_181_210" & EQ_SERV=="EQUIPO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'211.240_EQ'=ifelse(CAJON=="D_211_240"& EQ_SERV=="EQUIPO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'241.360_EQ'=ifelse(CAJON=="D_241_360" & EQ_SERV=="EQUIPO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,MAS_360_EQ=ifelse(CAJON=="MAS_360" & EQ_SERV=="EQUIPO",SALDO,0))

str(CarteraSCL)
CarteraSCL<-mutate(CarteraSCL,'TOTAL_ADE_EQ'=rowSums(select(CarteraSCL,31:41)))
CarteraSCL<-mutate(CarteraSCL,'1.90_EQ'=rowSums(select(CarteraSCL,32:34)))
CarteraSCL<-mutate(CarteraSCL,'91.360_EQ'=rowSums(select(CarteraSCL,35:40)))

#######Segmento Servicio

CarteraSCL<-mutate(CarteraSCL,CTE_SV=ifelse(CAJON=="CORRIENTE" & EQ_SERV=="SERVICIO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'1.30_SV'=ifelse(CAJON=="D_01_30" & EQ_SERV=="SERVICIO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'31.60_SV'=ifelse(CAJON=="D_31_60" & EQ_SERV=="SERVICIO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'61.90_SV'=ifelse(CAJON=="D_61_90" & EQ_SERV=="SERVICIO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'91.120_SV'=ifelse(CAJON=="D_91_120" & EQ_SERV=="SERVICIO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'121.150_SV'=ifelse(CAJON=="D_121_150" & EQ_SERV=="SERVICIO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'151.180_SV'=ifelse(CAJON=="D_151_180" & EQ_SERV=="SERVICIO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'181.210_SV'=ifelse(CAJON=="D_181_210" & EQ_SERV=="SERVICIO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'211.240_SV'=ifelse(CAJON=="D_211_240"& EQ_SERV=="SERVICIO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,'241.360_SV'=ifelse(CAJON=="D_241_360" & EQ_SERV=="SERVICIO",SALDO,0))
CarteraSCL<-mutate(CarteraSCL,MAS_360_SV=ifelse(CAJON=="MAS_360" & EQ_SERV=="SERVICIO",SALDO,0))

str(CarteraSCL)
CarteraSCL<-mutate(CarteraSCL,'TOTAL_ADE_SV'=rowSums(select(CarteraSCL,45:55)))
CarteraSCL<-mutate(CarteraSCL,'1.90_SV'=rowSums(select(CarteraSCL,46:48)))
CarteraSCL<-mutate(CarteraSCL,'91.360_SV'=rowSums(select(CarteraSCL,49:54)))

##Segmento Castigo
str(CarteraSCL)
CarteraSCL<-mutate(CarteraSCL,CASTIGO_EQ=ifelse(EQ_SERV=="EQUIPO",CASTIGO,0))
CarteraSCL<-mutate(CarteraSCL,CASTIGO_SV=ifelse(EQ_SERV=="SERVICIO",CASTIGO,0))
CarteraSCL<-mutate(CarteraSCL,'TOTAL_CASTIGO'=rowSums(select(CarteraSCL,59:60)))


## Segmento Totales solo EQ y SV (SIN PENA, TERCERA A MAS,CASTIGO)
CarteraSCL<-mutate(CarteraSCL,CORRIENTE_LIB=ifelse(CAJON=="CORRIENTE",0,0))
CarteraSCL<-mutate(CarteraSCL,'1.30_LIB'=ifelse(CAJON=="D_01_30",0,0))
CarteraSCL<-mutate(CarteraSCL,'31.60_LIB'=ifelse(CAJON=="D_31_60",0,0))
CarteraSCL<-mutate(CarteraSCL,'61.90_LIB'=ifelse(CAJON=="D_61_90",0,0))
CarteraSCL<-mutate(CarteraSCL,'91.120_LIB'=ifelse(CAJON=="D_91_120",0,0))
CarteraSCL<-mutate(CarteraSCL,'121.150_LIB'=ifelse(CAJON=="D_121_150",0,0))
CarteraSCL<-mutate(CarteraSCL,'151.180_LIB'=ifelse(CAJON=="D_151_180",0,0))
CarteraSCL<-mutate(CarteraSCL,'181.210_LIB'=ifelse(CAJON=="D_181_210",0,0))
CarteraSCL<-mutate(CarteraSCL,'211.240_LIB'=ifelse(CAJON=="D_211_240",0,0))
CarteraSCL<-mutate(CarteraSCL,'241.360_LIB'=ifelse(CAJON=="D_241_360",0,0))
CarteraSCL<-mutate(CarteraSCL,MAS_360_LIB=ifelse(CAJON=="MAS_360",0,0))

str(CarteraSCL)
CarteraSCL<-mutate(CarteraSCL,'TOTAL_ADEUDO_LIB'=ifelse(CAJON=="MAS_360",0,0))
CarteraSCL<-mutate(CarteraSCL,'1.90_LIB'=ifelse(CAJON=="MAS_360",0,0))
CarteraSCL<-mutate(CarteraSCL,'91.360_LIB'=ifelse(CAJON=="MAS_360",0,0))

CarteraSCL <- unite(CarteraSCL,NRO_DOCUMENTO_FISCAL,c(6:7),sep="     ",remove = TRUE)



CarteraSCL$PREF_PLAZA <- NULL
CarteraSCL$COD_TIPDOCUM <- NULL
CarteraSCL$NUM_FOLIO <- NULL
CarteraSCL$CAJON <- NULL
CarteraSCL$CAJONM <- NULL
CarteraSCL$EQ_SERV <- NULL 
str(CarteraSCL)

## Join Cartera vs SCL

Cartera1Ago19SCL <- left_join(CarteraUnificada,CarteraSCL,by=c("COD_CLIENTE","COD_CLIENTE"))
str(Cartera1Ago19SCL)
##Sumarizado
CarteraSCL<-select(CarteraSCL,everything())%>%
  group_by(COD_CLIENTE,NOMBRE,CUENTA,DES_CUENTA,DES_CATEGORIA) %>% 
    summarise(SALDO=sum(SALDO),
            MAXD_MORA=max(DIAS),
            CORRIENTE_T=sum(CORRIENTE_T),
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
            '1.90_T'=sum(`1.90_T`),
            '91.360_T'=sum(`91.360_T`),
            CTE_EQ=sum(CTE_EQ),
            '1.30_EQ'=sum(`1.30_EQ`),
            '31.60_EQ'=sum(`31.60_EQ`),
            '61.90_EQ'=sum(`61.90_EQ`),
            '91.120_EQ'=sum(`91.120_EQ`),
            '121.150_EQ'=sum(`121.150_EQ`),
            '151.180_EQ'=sum(`151.180_EQ`),
            '181.210_EQ'=sum(`181.210_EQ`),
            '211.240_EQ'=sum(`211.240_EQ`),
            '241.360_EQ'=sum(`241.360_EQ`),
            MAS_360_EQ=sum(MAS_360_EQ),
            TOTAL_ADE_EQ=sum(TOTAL_ADE_EQ),
            '1.90_EQ'=sum(`1.90_EQ`),
            '91.360_EQ'=sum(`91.360_EQ`),
            CTE_SV=sum(CTE_SV),
            '1.30_SV'=sum(`1.30_SV`),
            '31.60_SV'=sum(`31.60_SV`),
            '61.90_SV'=sum(`61.90_SV`),
            '91.120_SV'=sum(`91.120_SV`),
            '121.150_SV'=sum(`121.150_SV`),
            '151.180_SV'=sum(`151.180_SV`),
            '181.210_SV'=sum(`181.210_SV`),
            '211.240_SV'=sum(`211.240_SV`),
            '241.360_SV'=sum(`241.360_SV`),
            MAS_360_SV=sum(MAS_360_SV),
            TOTAL_ADE_SV=sum(TOTAL_ADE_SV),
            '1.90_SV'=sum(`1.90_SV`),
            '91.360_SV'=sum(`91.360_SV`),
            CASTIGO_EQ=sum(CASTIGO_EQ),
            CASTIGO_SV=sum(CASTIGO_SV),
            TOTAL_CASTIGO=sum(TOTAL_CASTIGO),
            CORRIENTE_LIB=sum(CORRIENTE_LIB),
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
            '91.360_LIB'=sum(`91.360_LIB`))

write.csv(CarteraSCL,"Victor/4_CarteraUnificada/CarterasUnifSCLSAP/SCLTOTAL.csv",row.names=FALSE)

#la cargo nuevamente para hacer cruces, esto es para los clientes faltantes de cartera
SCLTOTAL <- read_csv("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/SCLTOTAL.csv")



write.csv(Cartera1Ago19SCL,"Victor/4_CarteraUnificada/CarterasUnifSCLSAP/7_Jul/CarteraSCL1Ago19Detalle.csv",row.names=FALSE)

## fin agrupado
CarteraSCL %>% filter(COD_CLIENTE==90040491)



































































                                 
### Transformación Cartera SCL a Agrupados por Cajon Total, Servicio y Equipo
head(CarteraUniSCL)
CarteraUniSCL <- mutate(CarteraUniSCL,CATEG=ifelse(COD_TIPDOCUM==1,"EQUIPO","SERVICIO"))
CarteraUniSCL <- mutate(CarteraUniSCL,CASTIGO=ifelse(COD_TIPDOCUM==39,"CASTIGO",0))
head(CarteraUniSCL)
levels(as.factor(CarteraUniSCL$CAJON))
###
CarteraUniSCL2 <- mutate(CarteraUniSCL,CORRIENTE=ifelse(CAJON=="CORRIENTE",SALDO,0))
CarteraUniSCL2<-mutate(CarteraUniSCL2,D_1_A_30=ifelse(CAJON=="D_1_A_30",SALDO,0))
CarteraUniSCL2<-mutate(CarteraUniSCL2,D_31_A_60=ifelse(CAJON=="D_31_A_60",SALDO,0))
CarteraUniSCL2<-mutate(CarteraUniSCL2,D_61_A_90=ifelse(CAJON=="D_61_A_90",SALDO,0))
CarteraUniSCL2<-mutate(CarteraUniSCL2,D_91_A_120=ifelse(CAJON=="D_91_A_120",SALDO,0))
CarteraUniSCL2<-mutate(CarteraUniSCL2,D_121_A_150=ifelse(CAJON=="D_121_A_150",SALDO,0))
CarteraUniSCL2<-mutate(CarteraUniSCL2,D_151_A_180=ifelse(CAJON=="D_151_A_180",SALDO,0))
CarteraUniSCL2<-mutate(CarteraUniSCL2,D_181_A_210=ifelse(CAJON=="D_181_A_210",SALDO,0))
CarteraUniSCL2<-mutate(CarteraUniSCL2,D_211_A_240=ifelse(CAJON=="D_211_A_240",SALDO,0))
CarteraUniSCL2<-mutate(CarteraUniSCL2,D_241_A_360=ifelse(CAJON=="D_241_A_360",SALDO,0))
CarteraUniSCL2<-mutate(CarteraUniSCL2,MAS_360=ifelse(CAJON=="MAS_360",SALDO,0))
str(CarteraUniSCL2)

names(CarteraUniSCL2)[4]="DEBE"
names(CarteraUniSCL2)[5]="HABER"
##
CarteraUnificada2 <- left_join(CarteraUnificada,CarteraUniSCL2,by=c("COD_CLIENTE","COD_CLIENTE"))
Penalizacion <- read_csv("Salidas/Penalizacion.csv")
Penalizacion <- Penalizacion %>% select(3,4)
CarteraUnificada2 <- left_join(CarteraUnificada2,Penalizacion,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(CarteraUnificada2,file = "Victor/4_CarteraUnificada/CarterasUnifSCLSAP/7_Jul/CruceCartera.csv",row.names = FALSE)

library(readxl)
XX <- read_excel("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/7_Jul/XX.xlsx")


DoctosFolio <-
  tbl(SCL,"CO_CARTERA") %>% select(COD_CLIENTE,NUM_FOLIO,IMPORTE_DEBE,IMPORTE_HABER,COD_TIPDOCUM,FEC_VENCIMIE) %>%
  group_by(COD_CLIENTE,NUM_FOLIO,COD_TIPDOCUM,FEC_VENCIMIE) %>% 
  summarise(DEBE=sum(IMPORTE_DEBE,na.rm = TRUE),HABER=sum(IMPORTE_HABER,na.rm = TRUE)) %>% 
  mutate(IMPORTE=(DEBE-HABER)) %>% 
  arrange(COD_CLIENTE,FEC_VENCIMIE)



CarteraUniSCL2 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/7_Jul/CarteraUniSCL2.txt", 
                             "|", escape_double = FALSE, trim_ws = TRUE)

Folios <- inner_join(XX,CarteraUniSCL2,by=c("COD_CLIENTE","COD_CLIENTE"),copy=TRUE)

write.csv(Folios,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/7_Jul/Folios.csv",row.names = FALSE)

##
Doctos<-select(Doctos,1,everything())%>% group_by(COD_CLIENTE) %>%
  summarise(CORRIENTE=sum(CORRIENTE),
            D_1_A_30=sum(D_1_A_30),
            D_31_A_60=sum(D_31_A_60),
            D_61_A_90=sum(D_61_A_90),
            D_91_A_120=sum(D_91_A_120),
            D_121_A_150=sum(D_121_A_150),
            D_151_A_180=sum(D_151_A_180),
            D_181_A_210=sum(D_181_A_210),
            D_211_A_240=sum(D_211_A_240),
            D_241_A_360=sum(D_241_A_360),
            MAS_360=sum(MAS_360),TOTAL_VENCIDO=sum(TOTAL_VENCIDO))
head(Doctos)
Doctos %>% filter(COD_CLIENTE==2724123)## tiene que estar solo un renglon
head(Saldos2)

####FIN








Cartera <- left_join(CarteraUnificada,CarteraUniSCL,by=c("COD_CLIENTE","COD_CLIENTE"))
Cartera <- left_join(Cartera,SAP,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Cartera,file = "Victor/4_CarteraUnificada/CarterasUnifSCLSAP/CarteraSCL16jul19.csv",row.names =FALSE)


#### BDs Oracle
### CO_CARTERA

DoctosR <-
  tbl(SCL,"CO_CARTERA") %>% select(COD_CLIENTE,IMPORTE_DEBE,IMPORTE_HABER,COD_TIPDOCUM,FEC_VENCIMIE) %>%
  group_by(COD_CLIENTE,COD_TIPDOCUM,FEC_VENCIMIE) %>% 
  summarise(DEBE=sum(IMPORTE_DEBE,na.rm = TRUE),HABER=sum(IMPORTE_HABER,na.rm = TRUE)) %>% 
  mutate(IMPORTE=(DEBE-HABER)) %>% filter(IMPORTE!=0 ) %>% 
  arrange(COD_CLIENTE,FEC_VENCIMIE)




TotalDoctosR <-
  tbl(SCL,"CO_CARTERA") %>% select(COD_CLIENTE,IMPORTE_DEBE,IMPORTE_HABER,COD_TIPDOCUM,FEC_VENCIMIE) %>%
  group_by(COD_CLIENTE,COD_TIPDOCUM,FEC_VENCIMIE) %>% 
  summarise(DEBE=sum(IMPORTE_DEBE,na.rm = TRUE),HABER=sum(IMPORTE_HABER,na.rm = TRUE)) %>% 
  mutate(IMPORTE=(DEBE-HABER)) %>% filter(IMPORTE!=0) %>% 
  arrange(COD_CLIENTE,FEC_VENCIMIE)%>% as.data.frame()

tail(TotalDoctosR2)
TotalDoctosR2 <-
  TotalDoctosR %>% mutate(Dias=(Sys.Date() - as.Date(FEC_VENCIMIE))) %>% 
  mutate(IMPORTE=(DEBE-HABER)) %>% 
  mutate(CAJON=ifelse(Dias<=0,"Corriente",
                      ifelse((Dias >=1) & (Dias <=30),"1_A_30",
                             ifelse((Dias>=31)&(Dias<=60),"31_A_60",
                                    ifelse((Dias>=61)&(Dias<=90),"61_A_90",
                                           ifelse((Dias>=91)&(Dias<=120),"91_A_120",
                                                  ifelse((Dias>=121)&(Dias<=150),"121_A_150",
                                                         ifelse((Dias>=151)&(Dias<=180),"151_A_180",
                                                                ifelse((Dias>=181)&(Dias<=210),"181_A_210",
                                                                       ifelse((Dias>=211)&(Dias<=240),"211_A_240",
                                                                              ifelse((Dias>=241)&(Dias<=360),"241_A_360","MAS_360"))))))))))) %>% 
  mutate(CORRIENTE=ifelse(CAJON=="Corriente",IMPORTE,0)) %>% 
  mutate(D_1_A_30=ifelse(CAJON=="1_A_30",IMPORTE,0)) %>% 
  mutate(D_31_A_60=ifelse(CAJON=="31_A_60",IMPORTE,0)) %>% 
  mutate(D_61_A_90=ifelse(CAJON=="61_A_90",IMPORTE,0)) %>% 
  mutate(D_91_A_120=ifelse(CAJON=="91_A_120",IMPORTE,0)) %>% 
  mutate(D_121_A_150=ifelse(CAJON=="121_A_150",IMPORTE,0)) %>% 
  mutate(D_151_A_180=ifelse(CAJON=="151_A_180",IMPORTE,0)) %>% 
  mutate(D_181_A_210=ifelse(CAJON=="181_A_210",IMPORTE,0)) %>% 
  mutate(D_211_A_240=ifelse(CAJON=="211_A_240",IMPORTE,0)) %>% 
  mutate(D_241_A_360=ifelse(CAJON=="241_A_360",IMPORTE,0)) %>% 
  mutate(MAS_360=ifelse(CAJON=="MAS_360",IMPORTE,0)) %>% 
  mutate(Castigo=ifelse(COD_TIPDOCUM==39,IMPORTE,0))

TotalDoctosR2$FEC_VENCIMIE <- NULL
TotalDoctosR2$CAJON <- NULL
TotalDoctosR2$DEBE <- NULL
TotalDoctosR2$HABER <- NULL
TotalDoctosR2$IMPORTE <- NULL
TotalDoctosR2 <-  mutate(TotalDoctosR2,Total_Adeudo=rowSums(select(TotalDoctosR2,4:14)))
TotalDoctosR2 <-  mutate(TotalDoctosR2,Total_Vencido=rowSums(select(TotalDoctosR2,5:14)))
TotalDoctosR2$COD_TIPDOCUM <- NULL
head(TotalDoctosR2)
#Agrupado y Sumarizado
TotalDoctosR3 <- TotalDoctosR2 %>% select(everything())%>% 
  group_by(COD_CLIENTE) %>%
  summarise(Dias_Mora=sum(Dias,na.rm = TRUE),
            CORRIENTE=sum(CORRIENTE),
            D_1_A_30=sum(D_1_A_30),
            D_31_A_60=sum(D_31_A_60),
            D_61_A_90=sum(D_61_A_90),
            D_91_A_120=sum(D_91_A_120),
            D_121_A_150=sum(D_121_A_150),
            D_151_A_180=sum(D_151_A_180),
            D_181_A_210=sum(D_181_A_210),
            D_211_A_240=sum(D_211_A_240),
            D_241_A_360=sum(D_241_A_360),
            MAS_360=sum(MAS_360),
            Total_Adeudo=sum(Total_Adeudo),
            Total_Vencido=sum(Total_Vencido))

Cartera2$Dias <- NULL
TotalDoctosR2 %>% filter(COD_CLIENTE==2724123)## tiene que estar solo un renglon
head(Cartera2)

write.csv(TotalDoctosR,file="Salidas/TotalDoctosR.csv",row.names = FALSE)
# %>% as.data.frame()
#  filter(COD_CLIENTE==3221766)
### Join Perimetro Cartera  
Cartera <- left_join(local(Perimetro),DoctosR,by=c("COD_CLIENTE","COD_CLIENTE"),copy=TRUE)

Cartera <- left_join(local(Perimetro),DoctosR,by=c("COD_CLIENTE","COD_CLIENTE"),copy=TRUE)
Cartera <- TotalDoctosR
###


###
### Creacion de Cajones
Cartera2 <-
Cartera %>% mutate(Dias=(Sys.Date() - as.Date(FEC_VENCIMIE))) %>% 
mutate(IMPORTE=(DEBE-HABER)) %>% 
mutate(CAJON=ifelse(Dias<=0,"Corriente",
                    ifelse((Dias >=1) & (Dias <=30),"1_A_30",
                           ifelse((Dias>=31)&(Dias<=60),"31_A_60",
                                  ifelse((Dias>=61)&(Dias<=90),"61_A_90",
                                         ifelse((Dias>=91)&(Dias<=120),"91_A_120",
                                                ifelse((Dias>=121)&(Dias<=150),"121_A_150",
                                                       ifelse((Dias>=151)&(Dias<=180),"151_A_180",
                                                              ifelse((Dias>=181)&(Dias<=210),"181_A_210",
                                                                     ifelse((Dias>=211)&(Dias<=240),"211_A_240",
                                                                            ifelse((Dias>=241)&(Dias<=360),"241_A_360","MAS_360"))))))))))) %>% 
mutate(CORRIENTE=ifelse(CAJON=="Corriente",IMPORTE,0)) %>% 
mutate(D_1_A_30=ifelse(CAJON=="1_A_30",IMPORTE,0)) %>% 
mutate(D_31_A_60=ifelse(CAJON=="31_A_60",IMPORTE,0)) %>% 
mutate(D_61_A_90=ifelse(CAJON=="61_A_90",IMPORTE,0)) %>% 
mutate(D_91_A_120=ifelse(CAJON=="91_A_120",IMPORTE,0)) %>% 
mutate(D_121_A_150=ifelse(CAJON=="121_A_150",IMPORTE,0)) %>% 
mutate(D_151_A_180=ifelse(CAJON=="151_A_180",IMPORTE,0)) %>% 
mutate(D_181_A_210=ifelse(CAJON=="181_A_210",IMPORTE,0)) %>% 
mutate(D_211_A_240=ifelse(CAJON=="211_A_240",IMPORTE,0)) %>% 
mutate(D_241_A_360=ifelse(CAJON=="241_A_360",IMPORTE,0)) %>% 
mutate(MAS_360=ifelse(CAJON=="MAS_360",IMPORTE,0)) %>% 
mutate(Castigo=ifelse(COD_TIPDOCUM==39,IMPORTE,0))

Cartera2 <-  mutate(Cartera2,Total_Adeudo=rowSums(select(Cartera2,15:25)))
Cartera2 <-  mutate(Cartera2,Total_Vencido=rowSums(select(Cartera2,16:25)))

Cartera2$DEBE <- NULL
Cartera2$HABER <- NULL
Cartera2$CAJON <- NULL
Cartera2$IMPORTE <- NULL
Cartera2$COD_TIPDOCUM <- NULL


#Agrupado y Sumarizado
Cartera2 <- Cartera2 %>% select(everything()) %>% 
  group_by(SEGMENTO,COD_CLIENTE,COD_CUENTA,HOLDING,SUBSIDIARIA,TIPO_CLIENTE,RFC,Dias) %>%
  summarise(Dias_Mora=sum(Dias,na.rm = TRUE),
            CORRIENTE=sum(CORRIENTE),
            D_1_A_30=sum(D_1_A_30),
            D_31_A_60=sum(D_31_A_60),
            D_61_A_90=sum(D_61_A_90),
            D_91_A_120=sum(D_91_A_120),
            D_121_A_150=sum(D_121_A_150),
            D_151_A_180=sum(D_151_A_180),
            D_181_A_210=sum(D_181_A_210),
            D_211_A_240=sum(D_211_A_240),
            D_241_A_360=sum(D_241_A_360),
            MAS_360=sum(MAS_360),
            Total_Adeudo=sum(Total_Adeudo),
            Total_Vencido=sum(Total_Vencido))
Cartera2$Dias <- NULL
Cartera2 %>% filter(COD_CLIENTE==2724123)## tiene que estar solo un renglon
head(Cartera2)





CarteraSaldos <- "
select cod_cliente,importe_debe,importe_haber,(importe_debe - importe_haber) importe, cod_tipdocum
from co_cartera
where cod_cliente = 95700641
"

Saldos <- dbGetQuery(jdbcConnection,CarteraSaldos)

Saldos %>% summarise(IMPORTE=sum(IMPORTE,na.rm = TRUE))






#####
#Cartera Historica
####
#Estadisticas Cartera
CarteraAgo <- read_delim("Salidas/Cartera_2SepCierre.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)

CarteraAgo <- unite(CarteraAgo,NRO_DOCUMENTO_FISCAL,c(6:7),sep="     ",remove = TRUE)
#
Categorias <- read_excel("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/8_Ago/Cartera_CierreAgo2_1.xlsx", 
                                   sheet = "Tablas")
CarteraAgo <- left_join(CarteraAgo,Categorias,by = c("COD_TIPDOCUM","COD_TIPDOCUM")) 

#Cuadro resumen de la cartera total por Docto
CarteraAgo %>% group_by(as.factor(COD_TIPDOCUM),as.factor(DES_TIPDOCUM)) %>% summarise(Saldo=sum(SALDO,na.rm=TRUE))
sum(CarteraAgo$SALDO)# Cifra total de la cartera para cuadre
rbind(head(CarteraAgo),tail(CarteraAgo))

#Cruce con el Perimetro Ago19
CarteraUnificada <- read_excel("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/CarteraUnificada.xlsx")
CarteraUnificada <- CarteraUnificada %>% select(4,1,2,10) %>% filter(ID_FUENTE=="SCL")
CarteraAgo <- left_join(CarteraAgo,CarteraUnificada,by = c("COD_CLIENTE","COD_CLIENTE")) 

Res <- CarteraAgo %>% select(SEGMENTO,PERIMETRO,EQ_SERV,DES_CATEGORIA,SALDO) %>% 
  group_by(SEGMENTO,PERIMETRO,EQ_SERV,DES_CATEGORIA) %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))

Res2 <- CarteraAgo %>% select(SEGMENTO,PERIMETRO,EQ_SERV,DES_CATEGORIA,SALDO) %>% 
  group_by(SEGMENTO,PERIMETRO,DES_CATEGORIA) %>% summarise(Saldo=sum(SALDO,na.rm = TRUE))



#Grafico
CarteraAgoGrap <- CarteraAgo %>% select(SEGMENTO,PERIMETRO,EQ_SERV,SALDO)

Grap1 <- ggplot(Res2,aes( x = reorder(SEGMENTO,Saldo),y = Saldo, 
                      fill = PERIMETRO))+ geom_bar( stat = "identity")
+
  scale_fill_manual( name = "Región",
                     labels = c( "Norte de África", "África Subsahariana" ),
                     values = c( "#00AFBB", "#E7B800" ) ) +
  ylim( 0, 100 ) +
  labs( title = "Ranking del índice EPI",
        subtitle = "(índice 0 - 100)") +
  coord_flip( ) 



#
##3. Carga Cartera inicial Archivo Sergio
CarteraUnificada <- read_excel("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/CarteraUnificada.xlsx")
#names(CarteraUnificada)[1]="SEGMENTO_ANTE"
#names(CarteraUnificada)[2]="SEGMENTO"
str(CarteraUnificada)
levels(as.factor(CarteraUnificada$ID_FUENTE))
CarteraUnificada <- CarteraUnificada %>% filter(ID_FUENTE=="SCL")

#Cartera 5Sep19
Cartera5Sep19 <- read_delim("Salidas/Cartera10Sep.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)

Cartera5Sep19 <- unite(Cartera5Sep19,NRO_DOCUMENTO_FISCAL,c(6:7),sep="     ",remove = TRUE)

head(Cartera5Sep19)
max(as.Date(Cartera5Sep19$FEC_VENCIMIE,format='%d/%m/%y'))
Cartera5Sep19 %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
Cartera5Sep19 <- Cartera5Sep19 %>% select(everything()) %>% filter(as.Date(FEC_VENCIMIE,format='%d/%m/%y') <="2019/09/05")
Cartera5Sep19 %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))

Cartera5Sep19 <- Cartera5Sep19 %>%
  select(COD_CLIENTE,NOMBRE,COD_TIPDOCUM,NRO_DOCUMENTO_FISCAL,DEBE,HABER,SALDO,DES_CATEGORIA,DIAS,CAJON,CAJONM,EQ_SERV)


#Join Sep19

Cartera5Sep19 <- left_join(Cartera5Sep19,Castigo_Ago19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera5Sep19 %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
Cartera5Sep19 <- left_join(Cartera5Sep19,Pena_Ago19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera5Sep19 %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
Cartera5Sep19 <- left_join(Cartera5Sep19,T3era,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera5Sep19 %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))
names(Cartera5Sep19)[1]="COD_CLIENTE"

Cartera5Sep19 <- left_join(CarteraUnificada,Cartera5Sep19,by=c("COD_CLIENTE","COD_CLIENTE"))
Cartera5Sep19 %>% summarise(SALDO=sum(SALDO,na.rm = TRUE))


head(Cartera5Sep19)
write.csv(Cartera5Sep19,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/9_Sep/5Sep19_2.csv",row.names = FALSE)






library(readr)

#Cartera Ago19
CarteraAgo <- read_delim("Salidas/Cartera_2SepCierre.txt", 
                           "|", escape_double = FALSE, trim_ws = TRUE)

CarteraAgo <- unite(CarteraAgo,NRO_DOCUMENTO_FISCAL,c(6:7),sep="     ",remove = TRUE)
head(CarteraAgo)


CarteraAgo <- CarteraAgo %>%
  select(COD_CLIENTE,NOMBRE,COD_TIPDOCUM,NRO_DOCUMENTO_FISCAL,DEBE,HABER,SALDO,DES_CATEGORIA,DIAS,CAJON,CAJONM,EQ_SERV)

#PENA Ago19
Pena_Ago19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/8Ago19/Pena_Convencional_SCL.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)
names(Pena_Ago19)
Pena_Ago19 <- unite(Pena_Ago19,NRO_DOCUMENTO_FISCAL,c(9:10),sep="     ",remove = TRUE)
Pena_Ago19 <- Pena_Ago19 %>% select(9,3)
names(Pena_Ago19)[2]="PENA"
Pena_Ago19 <- tbl_df(Pena_Ago19[!duplicated(Pena_Ago19$NRO_DOCUMENTO_FISCAL), ])


#Join Tercera a mas
AcumTercera_Jul_Feb <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/AcumTercera_Jul_Feb.txt", 
                                  "|", escape_double = FALSE, trim_ws = TRUE)
AcumTercera_Jul_Feb <- AcumTercera_Jul_Feb %>% select(1,3,6)

Abr <- read_delim(file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/4Abr19/Tercera_a_mas_20190430_CAbril.txt","|",escape_double = FALSE,trim_ws = TRUE)
May <- read_delim(file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/5May19/Tercera_a_mas_May19_20190531_C.txt","|",escape_double = FALSE,trim_ws = TRUE)
Jun <- read_delim(file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/6Jun19/Tercera_a_mas_Jun19_20190630_C.txt","|",escape_double = FALSE,trim_ws = TRUE)
Jul <- read_delim(file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/7Jul19/Tercera_a_mas_Jul19_20190731_C.txt","|",escape_double = FALSE,trim_ws = TRUE)
a <- full_join(Abr,May,by=NULL)
b <- full_join(a,Jun,by=NULL)
c <- full_join(b,AcumTercera_Jul_Feb,by=NULL)
T3era <- full_join(c,Jul,by=NULL)
rm(Abr,May,Jun,Jul,a,b,c,AcumTercera_Jul_Feb)
head(T3era)
T3era <- T3era %>% select(NRO_DOCUMENTO_FISCAL,NUM_FACT_MORA,CVE_CLIENTE,ID_CLIENTE,ID_FUENTE)
T3era <- T3era %>% select(NRO_DOCUMENTO_FISCAL,NUM_FACT_MORA)
#names(T3era)[2]="Tercera"
T3era <- tbl_df(T3era[!duplicated(T3era$NRO_DOCUMENTO_FISCAL), ])

Tercera_a_mas_Ago19_20190831_C <- read_excel("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/8Ago19/Tercera_a_mas_Ago19_20190831_C.xlsx", 
                                             sheet = "Tercera_a_mas_Ago19_20190831_C")
Tercera_a_mas_Ago19_20190831_C <- Tercera_a_mas_Ago19_20190831_C %>% select(5,15)
T3era <- full_join(T3era,Tercera_a_mas_Ago19_20190831_C,by=NULL)
T3era <- tbl_df(T3era[!duplicated(T3era$NRO_DOCUMENTO_FISCAL), ])
rm(Tercera_a_mas_Ago19_20190831_C)

####CASTIGO Ago19
Castigo_Ago19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/8Ago19/Castigo_Contable.txt", 
                            "|", escape_double = FALSE, trim_ws = TRUE)
Castigo_Ago19 <- unite(Castigo_Ago19,NRO_DOCUMENTO_FISCAL,c(3:4),sep="     ",remove = TRUE)
Castigo_Ago19 <-Castigo_Ago19 %>%  select(1,3,7) %>% group_by(COD_CLIENTE,NRO_DOCUMENTO_FISCAL) %>% summarise(Castigo=sum(SALDO))
head(Castigo_Ago19)


#Join Ago19

Cartera_Ago <- left_join(CarteraAgo,Castigo_Ago19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_Ago <- left_join(Cartera_Ago,Pena_Ago19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_Ago <- left_join(Cartera_Ago,T3era,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
names(Cartera_Ago)[1]="COD_CLIENTE"
Cartera_Ago <- left_join(CarteraUnificada,Cartera_Ago,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Cartera_Ago,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/8_Ago/Cartera_CierreAgo2_1.csv",row.names = FALSE)
names(Cartera_Ago)


####---

#Cartera Jul19 SCL
CarteraJul <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/7_Jul/CarteraUniSCL1_8_19.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)

CarteraJul <- unite(CarteraJul,NRO_DOCUMENTO_FISCAL,c(6:7),sep="     ",remove = TRUE)
head(CarteraJul)
names(CarteraJul)[1]="COD_CLIENTE"

CarteraJul <- CarteraJul %>%
  select(COD_CLIENTE,NOMBRE,COD_TIPDOCUM,NRO_DOCUMENTO_FISCAL,DEBE,HABER,SALDO,DES_CATEGORIA,DIAS,CAJON,CAJONM,EQ_SERV)


####CASTIGO Jul19
Castigo_Jul19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/7Jul19/Castigo_Contable.txt", 
                            "|", escape_double = FALSE, trim_ws = TRUE)
Castigo_Jul19 <- unite(Castigo_Jul19,NRO_DOCUMENTO_FISCAL,c(3:4),sep="     ",remove = TRUE)
Castigo_Jul19 <-Castigo_Jul19 %>%  select(1,3,7) %>% group_by(COD_CLIENTE,NRO_DOCUMENTO_FISCAL) %>% summarise(Castigo=sum(SALDO))
head(Castigo_Jul19)


#Join Jul19

CarteraJul <- left_join(CarteraJul,Castigo_Jul19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
CarteraJul <- left_join(CarteraJul,Pena_Jul19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
CarteraJul <- left_join(CarteraJul,T3era,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
names(CarteraJul)[1]="COD_CLIENTE"
CarteraJul <- left_join(CarteraUnificada,CarteraJul,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(CarteraJul,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/7_Jul/Cartera_CierreJul2_1.csv",row.names = FALSE)
names(Cartera_Jul)

###--


  
#Cartera Jul19
Cartera_BI_Jul19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/7Jul19/Cartera_BI_Jul19.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE)
levels(as.factor(Cartera_BI_Jul19$ID_TIPO_DOCUMENTO))
levels(as.factor(Cartera_BI_Jul19$ID_FUENTE))
names(Cartera_BI_Jul19)[4]="COD_CLIENTE"
Cartera_BI_Jul19 <- Cartera_BI_Jul19 %>% select(2,4:9,13,15:17,12) %>% filter(ID_FUENTE=="SC")
names(Cartera_BI_Jul19)[1]="ID"
Cartera_BI_Jul19 <-Cartera_BI_Jul19 %>%  mutate(EQ_SERV=ifelse(ID==1,"EQUIPO","SERVICIO"))
Cartera_BI_Jul19$ID <- NULL
Cartera_BI_Jul19$ID_MODULO <- NULL
Cartera_BI_Jul19$ID_FUENTE <- NULL


#PENA JUL porque es acumulado para cruzar con Dic18
Pena_Jul19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/7Jul19/Pena_Convencional_SCL.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)
names(Pena_Jul19)
Pena_Jul19 <- unite(Pena_Jul19,NRO_DOCUMENTO_FISCAL,c(9:10),sep="     ",remove = TRUE)
Pena_Jul19 <- Pena_Jul19 %>% select(9,3)
names(Pena_Jul19)[2]="PENA"





#Cierre Cartera Jun19
Cartera_BI_Jun19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/6Jun19/Cartera_BIJun19.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE)
levels(as.factor(Cartera_BI_Jun19$ID_TIPO_DOCUMENTO))
levels(as.factor(Cartera_BI_Jun19$ID_FUENTE))
names(Cartera_BI_Jun19)[4]="COD_CLIENTE"
Cartera_BI_Jun19 <- Cartera_BI_Jun19 %>% select(2,4:9,13,15:17,12) %>% filter(ID_FUENTE=="SC")
names(Cartera_BI_Jun19)[1]="ID"
Cartera_BI_Jun19 <-Cartera_BI_Jun19 %>%  mutate(EQ_SERV=ifelse(ID==1,"EQUIPO","SERVICIO"))
Cartera_BI_Jun19$ID <- NULL
Cartera_BI_Jun19$ID_MODULO <- NULL
Cartera_BI_Jun19$ID_FUENTE <- NULL



##CARTERA MAYO19
Cartera_BI_May19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/5May19/Cartera_BI_May19.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE)
levels(as.factor(Cartera_BI_May19$ID_TIPO_DOCUMENTO))
levels(as.factor(Cartera_BI_May19$ID_FUENTE))
names(Cartera_BI_May19)[4]="COD_CLIENTE"
Cartera_BI_May19 <- Cartera_BI_May19 %>% select(2,4:9,13,15:17,12) %>% filter(ID_FUENTE=="SC")
names(Cartera_BI_May19)[1]="ID"
Cartera_BI_May19 <-Cartera_BI_May19 %>%  mutate(EQ_SERV=ifelse(ID==1,"EQUIPO","SERVICIO"))
Cartera_BI_May19$ID <- NULL
Cartera_BI_May19$ID_MODULO <- NULL
Cartera_BI_May19$ID_FUENTE <- NULL




####CASTIGO Jul19
Castigo_Jul19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/7Jul19/Castigo_Contable.txt", 
                            "|", escape_double = FALSE, trim_ws = TRUE)
Castigo_Jul19 <- unite(Castigo_Jul19,NRO_DOCUMENTO_FISCAL,c(3:4),sep="     ",remove = TRUE)
Castigo_Jul19 <-Castigo_Jul19 %>%  select(1,3,7) %>% group_by(COD_CLIENTE,NRO_DOCUMENTO_FISCAL) %>% summarise(Castigo=sum(SALDO))
head(Castigo_Jul19)


####CASTIGO MAY19
Castigo_May19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/5May19/Castigo_Contable.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE)

Pena_May19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/5May19/Pena_Convencional_SCL.txt", 
                            "|", escape_double = FALSE, trim_ws = TRUE)

#CASTIGO
Castigo_May19 <- unite(Castigo_May19,NRO_DOCUMENTO_FISCAL,c(3:4),sep="     ",remove = TRUE)
Castigo_May19 <-Castigo_May19 %>%  select(1,3,7) %>% group_by(COD_CLIENTE,NRO_DOCUMENTO_FISCAL) %>% summarise(Castigo=sum(SALDO))

head(Castigo_May19)


##PENA MAYO19
names(Pena_May19)
Pena_May19 <- unite(Pena_May19,NRO_DOCUMENTO_FISCAL,c(9:10),sep="     ",remove = TRUE)
Pena_May19 <- Pena_May19 %>% select(9,3)
names(Pena_May19)[2]="PENA"


#CARTERA ABRIL19
Cartera_BI_Abr19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/4Abr19/Cartera_BI_Abr19.txt", 
                               "|", escape_double = FALSE, col_types = cols(ID_CLIENTE_UNIVERSAL = col_number()), 
                               trim_ws = TRUE)
levels(as.factor(Cartera_BI_Abr19$ID_TIPO_DOCUMENTO))
levels(as.factor(Cartera_BI_Abr19$ID_FUENTE))
names(Cartera_BI_Abr19)[4]="COD_CLIENTE"
Cartera_BI_Abr19 <- Cartera_BI_Abr19 %>% select(2,4:9,13,15:17,12) %>% filter(ID_FUENTE=="SC")
names(Cartera_BI_Abr19)[1]="ID"
Cartera_BI_Abr19 <-Cartera_BI_Abr19 %>%  mutate(EQ_SERV=ifelse(ID==1,"EQUIPO","SERVICIO"))
Cartera_BI_Abr19$ID <- NULL
Cartera_BI_Abr19$ID_MODULO <- NULL
Cartera_BI_Abr19$ID_FUENTE <- NULL


####CASTIGO ABRIL19 
Castigo_Abr19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/4Abr19/Castigo_Contable.txt", 
                            "|", escape_double = FALSE, trim_ws = TRUE)

Pena_Abr19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/4Abr19/Pena_Convencional_SCL.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)
#CASTIGO
Castigo_Abr19 <- unite(Castigo_Abr19,NRO_DOCUMENTO_FISCAL,c(3:4),sep="     ",remove = TRUE)
Castigo_Abr19 <-Castigo_Abr19 %>%  select(1,3,7) %>% group_by(COD_CLIENTE,NRO_DOCUMENTO_FISCAL) %>% summarise(Castigo=sum(SALDO))
head(Castigo_Abr19)

##PENA Abr19
names(Pena_Abr19)
Pena_Abr19 <- unite(Pena_Abr19,NRO_DOCUMENTO_FISCAL,c(9:10),sep="     ",remove = TRUE)
Pena_Abr19 <- Pena_Abr19 %>% select(9,3)
names(Pena_Abr19)[2]="PENA"



#CARTERA MARZO19
Cartera_BI_Mar19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/3Mar19/Cartera_BI_Mar19.txt", 
                               "|", escape_double = FALSE, col_types = cols(ID_CLIENTE_UNIVERSAL = col_number()), 
                               trim_ws = TRUE)
levels(as.factor(Cartera_BI_Mar19$ID_TIPO_DOCUMENTO))
levels(as.factor(Cartera_BI_Mar19$ID_FUENTE))
names(Cartera_BI_Mar19)[4]="COD_CLIENTE"
Cartera_BI_Mar19 <- Cartera_BI_Mar19 %>% select(2,4:9,13,15:17,12) %>% filter(ID_FUENTE=="SC")
names(Cartera_BI_Mar19)[1]="ID"
Cartera_BI_Mar19 <-Cartera_BI_Mar19 %>%  mutate(EQ_SERV=ifelse(ID==1,"EQUIPO","SERVICIO"))
Cartera_BI_Mar19$ID <- NULL
Cartera_BI_Mar19$ID_MODULO <- NULL
Cartera_BI_Mar19$ID_FUENTE <- NULL


####CASTIGO Mar19 
Castigo_Mar19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/3Mar19/Castigo_Contable.txt", 
                            "|", escape_double = FALSE, trim_ws = TRUE)

Pena_Mar19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/3Mar19/Pena_Miscelanea.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)
#CASTIGO
Castigo_Mar19 <- unite(Castigo_Mar19,NRO_DOCUMENTO_FISCAL,c(3:4),sep="     ",remove = TRUE)
Castigo_Mar19 <-Castigo_Mar19 %>%  select(1,3,7) %>% group_by(COD_CLIENTE,NRO_DOCUMENTO_FISCAL) %>% summarise(Castigo=sum(SALDO))
head(Castigo_Mar19)

##PENA Mar19
names(Pena_Mar19)
Pena_Mar19 <- unite(Pena_Mar19,NRO_DOCUMENTO_FISCAL,c(9:10),sep="     ",remove = TRUE)
Pena_Mar19 <- Pena_Mar19 %>% select(9,3)
names(Pena_Mar19)[2]="PENA"


##CARTERA FEBRERO19
Cartera_BI_Feb19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/2Feb19/Cartera_BI_Feb19.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE)
levels(as.factor(Cartera_BI_Feb19$ID_TIPO_DOCUMENTO))
levels(as.factor(Cartera_BI_Feb19$ID_FUENTE))
names(Cartera_BI_Feb19)[4]="COD_CLIENTE"
Cartera_BI_Feb19 <- Cartera_BI_Feb19 %>% select(2,4:9,13,15:17,12) %>% filter(ID_FUENTE=="SC")
names(Cartera_BI_Feb19)[1]="ID"
Cartera_BI_Feb19 <-Cartera_BI_Feb19 %>%  mutate(EQ_SERV=ifelse(ID==1,"EQUIPO","SERVICIO"))
Cartera_BI_Feb19$ID <- NULL
Cartera_BI_Feb19$ID_MODULO <- NULL
Cartera_BI_Feb19$ID_FUENTE <- NULL

####CASTIGO FEB19
Castigo_Feb19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/2Feb19/Castigo_Contable.txt", 
                            "|", escape_double = FALSE, trim_ws = TRUE)

Pena_Feb19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/2Feb19/Pena_Convencional_SCL.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)

#CASTIGO
Castigo_Feb19 <- unite(Castigo_Feb19,NRO_DOCUMENTO_FISCAL,c(3:4),sep="     ",remove = TRUE)
Castigo_Feb19 <-Castigo_Feb19 %>%  select(1,3,7) %>% group_by(COD_CLIENTE,NRO_DOCUMENTO_FISCAL) %>% summarise(Castigo=sum(SALDO))

head(Castigo_Feb19)


##PENA Feb19
names(Pena_Feb19)
Pena_Feb19 <- unite(Pena_Feb19,NRO_DOCUMENTO_FISCAL,c(9:10),sep="     ",remove = TRUE)
Pena_Feb19 <- Pena_Feb19 %>% select(9,3)
names(Pena_Feb19)[2]="PENA"


##CARTERA ENERO19
Cartera_BI_Ene19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/1Ene19/Cartera_BI_Ene19.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE)
levels(as.factor(Cartera_BI_Ene19$ID_TIPO_DOCUMENTO))
levels(as.factor(Cartera_BI_Ene19$ID_FUENTE))
names(Cartera_BI_Ene19)[4]="COD_CLIENTE"
Cartera_BI_Ene19 <- Cartera_BI_Ene19 %>% select(2,4:9,13,15:17,12) %>% filter(ID_FUENTE=="SC")
names(Cartera_BI_Ene19)[1]="ID"
Cartera_BI_Ene19 <-Cartera_BI_Ene19 %>%  mutate(EQ_SERV=ifelse(ID==1,"EQUIPO","SERVICIO"))
Cartera_BI_Ene19$ID <- NULL
Cartera_BI_Ene19$ID_MODULO <- NULL
Cartera_BI_Ene19$ID_FUENTE <- NULL

####CASTIGO Ene19
Castigo_Ene19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/1Ene19/Castigo_Contable.txt", 
                            "|", escape_double = FALSE, trim_ws = TRUE)

Pena_Ene19 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/1Ene19/Pena_Convencional_SCL.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)

#CASTIGO
Castigo_Ene19 <- unite(Castigo_Ene19,NRO_DOCUMENTO_FISCAL,c(3:4),sep="     ",remove = TRUE)
Castigo_Ene19 <-Castigo_Ene19 %>%  select(1,3,7) %>% group_by(COD_CLIENTE,NRO_DOCUMENTO_FISCAL) %>% summarise(Castigo=sum(SALDO))
head(Castigo_Ene19)


##PENA Ene19
names(Pena_Ene19)
Pena_Ene19 <- unite(Pena_Ene19,NRO_DOCUMENTO_FISCAL,c(9:10),sep="     ",remove = TRUE)
Pena_Ene19 <- Pena_Ene19 %>% select(9,3)
names(Pena_Ene19)[2]="PENA"

##Cartera Dic19

X1Cartera_Doc_2_69_8_25_Dic18 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/AUDITORIADic18/1Cartera_Doc_2_69_8_25_Dic18.txt", 
                                            "|", escape_double = FALSE, trim_ws = TRUE)
X2Cartera_Contado_Dic18 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/AUDITORIADic18/2Cartera_Contado_Dic18.txt", 
                                      "|", escape_double = FALSE, trim_ws = TRUE)
X3Cartera_Miscelanea_Dic18 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/AUDITORIADic18/3Cartera_Miscelanea_Dic18.txt", 
                                         "|", escape_double = FALSE, trim_ws = TRUE)
X4Cartera_ND_Dic18 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/AUDITORIADic18/4Cartera_ND_Dic18.txt", 
                                 "|", escape_double = FALSE, trim_ws = TRUE)
Cartera_BI_Dic18 <- full_join(X1Cartera_Doc_2_69_8_25_Dic18,X2Cartera_Contado_Dic18,by=NULL)
Cartera_BI_Dic18 <- full_join(Cartera_BI_Dic18,X3Cartera_Miscelanea_Dic18,by=NULL)
Cartera_BI_Dic18 <- full_join(Cartera_BI_Dic18,X4Cartera_ND_Dic18,by=NULL)
rm(X1Cartera_Doc_2_69_8_25_Dic18,X2Cartera_Contado_Dic18,X3Cartera_Miscelanea_Dic18,X4Cartera_ND_Dic18) 
str(Cartera_BI_Dic18)  
levels(as.factor(Cartera_BI_Dic18$ID_TIPO_DOCUMENTO))
levels(as.factor(Cartera_BI_Dic18$ID_FUENTE))
names(Cartera_BI_Dic18)[3]="COD_CLIENTE"
str(Cartera_BI_Dic18)
Cartera_BI_Dic18 <- Cartera_BI_Dic18 %>% select(2:12,20)
names(Cartera_BI_Dic18)[1]="ID"
Cartera_BI_Dic18 <-Cartera_BI_Dic18 %>%  mutate(EQ_SERV=ifelse(ID==1,"EQUIPO","SERVICIO"))

Cartera_BI_Dic18$ID <- NULL
Cartera_BI_Dic18$ID_MODULO <- NULL
Cartera_BI_Dic18$ID_FUENTE <- NULL

####CASTIGO Dic19
Castigo_Dic18 <- read_delim("Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/AUDITORIADic18/5Castgo_Contable.txt", 
                            "|", escape_double = FALSE, trim_ws = TRUE)

#CASTIGO
Castigo_Dic18 <- unite(Castigo_Dic18,NRO_DOCUMENTO_FISCAL,c(3:4),sep="     ",remove = TRUE)
Castigo_Dic18 <-Castigo_Dic18 %>%  select(1,3,7) %>% group_by(COD_CLIENTE,NRO_DOCUMENTO_FISCAL) %>% summarise(Castigo=sum(SALDO))
head(Castigo_Dic18)





#Join Jul19

Cartera_BI_Jul19 <- left_join(Cartera_BI_Jul19,Castigo_Jul19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_BI_Jul19 <- left_join(Cartera_BI_Jul19,Pena_Jul19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_BI_Jul19 <- left_join(Cartera_BI_Jul19,T3era,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
names(Cartera_BI_Jul19)[1]="COD_CLIENTE"
Cartera_BI_Jul19$FCH_VENCIMIENTO <- as.Date(Cartera_BI_Jul19$FCH_VENCIMIENTO,"%d/%m/%y")
Cartera_BI_Jul19 <- left_join(CarteraUnificada,Cartera_BI_Jul19,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Cartera_BI_Jul19,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/7Jul19/CarteraJul19.txt",row.names = FALSE)
names(Cartera_BI_Jul19)





#1er join MAYO19
Cartera_BI <- left_join(Cartera_BI_May19,Castigo_May19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_BI <- left_join(Cartera_BI,Pena_May19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_BI <- left_join(Cartera_BI,T3era,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
names(Cartera_BI)[1]="COD_CLIENTE"
Cartera_BI <- left_join(CarteraUnificada,Cartera_BI,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Cartera_BI,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/5May19/Cartera_BI_May19.csv",row.names = FALSE)

#2do join ABR19
str(Cartera_BI_Abr19)
Cartera_Abr19 <- left_join(Cartera_BI_Abr19,Castigo_Abr19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_Abr19 <- left_join(Cartera_Abr19,Pena_Abr19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_Abr19 <- left_join(Cartera_Abr19,T3era,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
names(Cartera_Abr19)[1]="COD_CLIENTE"
Cartera_Abr19 <- left_join(CarteraUnificada,Cartera_Abr19,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Cartera_Abr19,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/4Abr19/Cartera_Abr19.csv",row.names = FALSE)


#2do join MAR19
str(Cartera_BI_Mar19)
Cartera_Mar19 <- left_join(Cartera_BI_Mar19,Castigo_Mar19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_Mar19 <- left_join(Cartera_Mar19,Pena_Mar19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_Mar19 <- left_join(Cartera_Mar19,T3era,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
names(Cartera_Mar19)[1]="COD_CLIENTE"
Cartera_Mar19 <- left_join(CarteraUnificada,Cartera_Mar19,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Cartera_Mar19,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/3Mar19/Cartera_Mar19.csv",row.names = FALSE)


#3ro join Feb19
str(Cartera_BI_Feb19)
Cartera_Feb19 <- left_join(Cartera_BI_Feb19,Castigo_Feb19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_Feb19 <- left_join(Cartera_Feb19,Pena_Feb19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_Feb19 <- left_join(Cartera_Feb19,T3era,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
names(Cartera_Feb19)[1]="COD_CLIENTE"
Cartera_Feb19 <- left_join(CarteraUnificada,Cartera_Feb19,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Cartera_Feb19,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/2Feb19/Cartera_Feb19.csv",row.names = FALSE)


#4to join Ene19
str(Cartera_BI_Ene19)
Cartera_Ene19 <- left_join(Cartera_BI_Ene19,Castigo_Ene19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_Ene19 <- left_join(Cartera_Ene19,Pena_Ene19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_Ene19 <- left_join(Cartera_Ene19,T3era,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
names(Cartera_Ene19)[1]="COD_CLIENTE"
Cartera_Ene19 <- left_join(CarteraUnificada,Cartera_Ene19,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Cartera_Ene19,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/1Ene19/Cartera_Ene19.csv",row.names = FALSE)


#5to join Dic18
Cartera_BI <- left_join(Cartera_BI_Dic18,Castigo_Dic18,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_BI <- left_join(Cartera_BI,Pena_Jul19,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
Cartera_BI <- left_join(Cartera_BI,T3era,by=c("NRO_DOCUMENTO_FISCAL","NRO_DOCUMENTO_FISCAL"))
names(Cartera_BI)[1]="COD_CLIENTE"
Cartera_BI <- left_join(CarteraUnificada,Cartera_BI,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Cartera_BI,file="Victor/4_CarteraUnificada/CarterasUnifSCLSAP/Dic_18a/AUDITORIADic18/Cartera_BI_Dic18.csv",row.names = FALSE)





#####
##### CARTERA SCL ARCHIVOS
### Librerias
library(tidyverse)
library(readr)
library(xlsx)
library(readxl)
library(readxl)
library(dbplyr)

install.packages("ggpubr",dependencies=TRUE)

###100% clientes
SCL <- tbl_df(read.delim("D:/Users/MRT15076/Documents/Salidas/TotalClientes_Saldos.txt",header = TRUE,sep="|"))
Saldos2 <- tbl_df(read.delim("D:/Users/MRT15076/Documents/Salidas/TotalClientes_SaldosPositivo_negativo.txt",header = TRUE,sep="|"))

Doctos <- tbl_df(read.delim("D:/Users/MRT15076/Documents/Salidas/TotalClientes_SaldosCajon2.txt",header = TRUE,sep="|"))
### Acomodar Cajones "Doctos" 

head(Doctos)
Doctos$IMPORTE_DEBE <- NULL
Doctos$IMPORTE_HABER <- NULL
Doctos <- mutate(Doctos,CORRIENTE=ifelse(CAJON=="CORRIENTE",IMPORTE,0))
Doctos<-mutate(Doctos,D_1_A_30=ifelse(CAJON=="D_1_A_30",IMPORTE,0))
Doctos<-mutate(Doctos,D_31_A_60=ifelse(CAJON=="D_31_A_60",IMPORTE,0))
Doctos<-mutate(Doctos,D_61_A_90=ifelse(CAJON=="D_61_A_90",IMPORTE,0))
Doctos<-mutate(Doctos,D_91_A_120=ifelse(CAJON=="D_91_A_120",IMPORTE,0))
Doctos<-mutate(Doctos,D_121_A_150=ifelse(CAJON=="D_121_A_150",IMPORTE,0))
Doctos<-mutate(Doctos,D_151_A_180=ifelse(CAJON=="D_151_A_180",IMPORTE,0))
Doctos<-mutate(Doctos,D_181_A_210=ifelse(CAJON=="D_181_A_210",IMPORTE,0))
Doctos<-mutate(Doctos,D_211_A_240=ifelse(CAJON=="D_211_A_240",IMPORTE,0))
Doctos<-mutate(Doctos,D_241_A_360=ifelse(CAJON=="D_241_A_360",IMPORTE,0))
Doctos<-mutate(Doctos,MAS_360=ifelse(CAJON=="MAS_360",IMPORTE,0))
Doctos <- mutate(Doctos,TOTAL_VENCIDO=rowSums(select(Doctos,3:10)))
Doctos$CAJON <- NULL
Doctos$IMPORTE <- NULL
Doctos$COD_TIPDOCUM <- NULL
Doctos$DIAS <- NULL
#DoctosNames <- c("COD_CLIENTE","TOTAL_ADEUDO","COD_TIPDOCUM","DIAS","CORRIENTE","D_1_A_30","D_31_A_60","D_61_A_90","D_91_A_120","D_121_A_150","D_151_A_180","D_181_A_210","D_211_A_240","D_241_A_360","MAS_360","TOTAL_VENCIDO")
#colnames(Doctos) <- DoctosNames
head(Doctos)
#Agrupado y Sumarizado

Doctos<-select(Doctos,1,everything())%>% group_by(COD_CLIENTE) %>%
  summarise(CORRIENTE=sum(CORRIENTE),
            D_1_A_30=sum(D_1_A_30),
            D_31_A_60=sum(D_31_A_60),
            D_61_A_90=sum(D_61_A_90),
            D_91_A_120=sum(D_91_A_120),
            D_121_A_150=sum(D_121_A_150),
            D_151_A_180=sum(D_151_A_180),
            D_181_A_210=sum(D_181_A_210),
            D_211_A_240=sum(D_211_A_240),
            D_241_A_360=sum(D_241_A_360),
            MAS_360=sum(MAS_360),TOTAL_VENCIDO=sum(TOTAL_VENCIDO))
head(Doctos)
Doctos %>% filter(COD_CLIENTE==2724123)## tiene que estar solo un renglon
head(Saldos2)


Libro1 <- read_excel("Salidas/Libro1.xlsx")
H <- left_join(Libro1,Doctos,by=c("COD_CLIENTE","COD_CLIENTE"))
Castigos1<-select(Castigos,1,3,4)%>% group_by(COD_CLIENTE) %>%
  summarise(IMPORTE_CASTIGO=sum(IMPORTE_CASTIGO),
            SALDO_CASTIGO=sum(SALDO_CASTIGO))

H <- left_join(H,Castigos1,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(H,file="Salidas/G.csv",row.names = FALSE)





Gobierno <- read_csv("Salidas/Gobierno.csv")
head(Gobierno)
H <- left_join(Saldos2,Perimetro,by=c("COD_CLIENTE","COD_CLIENTE"))
HA %>% select (1:3,TOTAL_VENCIDO) %>% filter(COD_CLIENTE==2700063)
HA <- left_join(H,Doctos,by=c("COD_CLIENTE","COD_CLIENTE"))
head(HA)
Gob <- left_join(Gob,Perimetro,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(Gob,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Gob.csv",row.names = FALSE)
head(Gob)
SCL <- ClientesOk %>% select(1:4,7,8,11:12)
SCL <- SCL %>% select(1:2)
SCL <- tbl_df(SCL[!duplicated(SCL$NOM_CLIENTE), ])
HA <- tbl_df(HA[!duplicated(HA$COD_CLIENTE), ])

Perimetro %>% filter(COD_CLIENTE==90758416)

SCL %>% filter(COD_CLIENTE==3292382)
Saldos %>% summarise(Saldo=sum(SALDO,na.rm = TRUE),Castigo=sum(CASTIGO,na.rm = TRUE))
head(Saldos)
RFCs <- read_delim("Salidas/RFCs.txt", ";",escape_double = FALSE, trim_ws = TRUE)
RFC1 <- RFCs[1:1000000,]
RFC2 <- RFCs[1000001:2000000,]
RFC3 <- RFCs[2000001:3000000,]
RFC4 <- RFCs[3000001:4000000,]
RFC5 <- RFCs[4000001:4404652,]
write.csv(RFC1,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/RFC1.csv",row.names = FALSE)
write.csv(RFC2,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/RFC2.csv",row.names = FALSE)
write.csv(RFC3,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/RFC3.csv",row.names = FALSE)
write.csv(RFC4,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/RFC4.csv",row.names = FALSE)
write.csv(RFC5,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/RFC5.csv",row.names = FALSE)


SAP21May19 <- read_excel("Victor/4_CarteraUnificada/Horacio/Cartera21May19.xlsx")
str(SAP21May19)
SAP21May192 <- SAP21May19 %>% select(4:5,14,Cartera,Ejecutivo) %>% group_by(COD_CLIENTE,NOM_CLIENTE,Cartera,Ejecutivo) %>% summarise(SALDO=sum(`Importe en ML`)) 
TotalSCLSAP <- full_join(ClientesOk2,SAP21May192,by=c("COD_CLIENTE","COD_CLIENTE")) 
head(TotalSCLSAP)
write.csv(TotalSCLSAP,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/TotalSCLSAP.csv",row.names = FALSE)

ClientesOk %>% filter(COD_CLIENTE=="83644835")
ClientesOk2 <- ClientesOk %>% select(1:4,6:8,11:12)%>% 
  group_by(COD_CLIENTE,NOM_CLIENTE,COD_CUENTA,DES_CUENTA,COD_CATEGORIA,DES_CATEGORIA,COD_SITUACION) %>% 
  summarise(Saldo=sum(Saldo,na.rm = TRUE),Castigo=sum(CASTIGO,na.rm = TRUE))
write.csv(ClientesOk2,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/ClientesOk2.csv",row.names = FALSE)
##Quitar Duplicados
ClientesOk2 <- tbl_df(ClientesOk2[!duplicated(ClientesOk2$COD_CLIENTE), ])
head(ClientesOk2)


str(Saldos)
#Cruce
ClientesOk <- left_join(Saldos,Perimetro,by=c("COD_CLIENTE","COD_CLIENTE"))
head(ClientesOk)
##Revisión por Holding

CarteraEjecutivos <- read_excel("Victor/4_CarteraUnificada/CarteraTotal.xlsx", 
                           sheet = "TOTAL_CLIENTES")
CarteraEjecutivos <- CarteraEjecutivos %>% select(COD_CLIENTE,SISTEMA,COD_CUENTA,HOLDING,Ejecutivo)

ClientesOk <- left_join(ClientesOk,CarteraEjecutivos,by=c("COD_CLIENTE","COD_CLIENTE"))
ClientesOk %>% filter(Ejecutivo=="Beatriz Anzaldo")

Saldos <- read.delim("D:/Users/MRT15076/Documents/Salidas/SaldosTotalClientes.txt",header = TRUE,sep="|")
Saldos <- Saldos %>% select(1:3) %>% group_by(COD_CLIENTE) %>%
  summarise(Saldo=sum(Saldo,na.rm = TRUE),Castigo=sum(CASTIGO,na.rm = TRUE))
Saldos <- tbl_df(Saldos[!duplicated(Saldos$COD_CLIENTE), ])
head(Saldos) 
ClientesOkFinal <- left_join(ClientesOk,Saldos,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(ClientesOkFinal,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/ClientesOkFinal.csv",row.names = FALSE)
ClientesOkFinal <- read.csv("D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/ClientesOkFinal.csv",header = TRUE,sep = ",")
head(ClientesOkFinal)
ClientesOkFinal <- ClientesOkFinal %>% arrange(COD_CUENTA.x)
#Ananiendo el estatus
C100 <- tbl_df(C100[!duplicated(ClientesOkFinal$COD_CLIENTE), ])
C100 <- C100 %>% select(COD_CLIENTE,COD_SITUACION)
#ClientesOkFinal %>% filter(Saldo!=0,Castigo!=0)
ClientesOkFinal <- left_join(ClientesOkFinal,C100,by=c("COD_CLIENTE","COD_CLIENTE"))
str(ClientesOkFinal)
Doctos %>% filter(COD_CLIENTE==3104352)
ClientesOkFinal <- tbl_df(ClientesOkFinal[!duplicated(ClientesOkFinal$COD_CLIENTE), ])

Comp <- Saldos %>% arrange(COD_CLIENTE)
ClientesOkFinal <-Comp 
head(Comp)
H1 <- HA[1:1000000,]
H2 <- HA[1000001:1819917,]
Base3 <- ClientesOkFinal[2000001:3000000,]
Base4 <- ClientesOkFinal[3000001:4000000,]
Base5 <- ClientesOkFinal[4000001:4918503,]
tail(Base5)
Base6 <- ClientesOkFinal[5000001:6000000,]
Base7 <- ClientesOkFinal[6000001:6521246,]
write.csv(H1,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/H1A.csv",row.names = FALSE)
write.csv(H2,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/H2A.csv",row.names = FALSE)
write.csv(Base3,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Base3.csv",row.names = FALSE)
write.csv(Base4,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Base4.csv",row.names = FALSE)
write.csv(Base5,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Base5.csv",row.names = FALSE)
write.csv(Base6,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Base6.csv",row.names = FALSE)
write.csv(Base7,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Base7.csv",row.names = FALSE)

## Clientes en SAP y SCL de qlikview
Clientes <- read_csv("Salidas/Clientes.csv")
head(Clientes)
Clientes %>% filter(COD_CLIENTE==10000461)
Clientes <- Clientes %>% select(COD_CLIENTE)

Base1 <- Clientes[2:1000000,]
Base1 <- mutate(Base1,Base=ifelse(COD_CLIENTE>0,"BASE1",0))

Base2 <- Clientes[1000001:2000000,]
Base2 <- mutate(Base2,Base=ifelse(COD_CLIENTE>0,"BASE2",0))

Base3 <- Clientes[2000001:3000000,]
Base3 <- mutate(Base3,Base=ifelse(COD_CLIENTE>0,"BASE3",0))

Base4 <- Clientes[3000001:4000000,]
Base4 <- mutate(Base4,Base=ifelse(COD_CLIENTE>0,"BASE4",0))

Base5 <- Clientes[4000001:4909390,]
Base5 <- mutate(Base5,Base=ifelse(COD_CLIENTE>0,"BASE5",0))

write.csv(Base1,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Base1H.csv",row.names = FALSE)
write.csv(Base2,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Base2H.csv",row.names = FALSE)
write.csv(Base3,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Base3H.csv",row.names = FALSE)
write.csv(Base4,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Base4H.csv",row.names = FALSE)
write.csv(Base5,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Base5H.csv",row.names = FALSE)


###
ClientesOk %>% filter(COD_CLIENTE==95302624)
SonoraLalo <- ClientesOk %>% filter(COD_CUENTA.x==15611566)
?tbl_vars
write.csv(SonoraLalo,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Sonora.csv",row.names = FALSE)
  select(COD_CLIENTE,COD_CUENTA.x,NOM_CLIENTE,COD_SITUACION,COD_ESTADO,COD_CUENTA.y,HOLDING,SUBSIDIARIA,TIPO_CLIENTE,RFC) %>% group_by(COD_CLIENTE)
head(ClientesOk)


##Revisión Miguel
ClientesOk %>% filter(COD_CLIENTE==10879963)
SonoraLalo <- ClientesOk %>% filter(COD_CUENTA.x==3159891)
Saldos %>% filter(COD_CLIENTE==70000000)


##Frecuencias
str(ClientesOk)
Freq <- table(ClientesOk$COD_CLIENTE,ClientesOk$COD_SITUACION,useNA = "ifany")

##Tabla de frecuencia cruzada

TablaCruzada <- table(ClientesOk$COD_CLIENTE,ClientesOk$COD_SITUACION,useNA = "ifany")
prop.table(table(ClientesOk$COD_SITUACION))


### Carga de Archivos Extraccion ORACLE y catalogos
Cliente <- read.delim("D:/Users/MRT15076/Documents/Salidas/INFOCLIENTE.txt",header = TRUE,sep="|")
Prorrogas <- read.delim("D:/Users/MRT15076/Documents/Salidas/prorrogas.txt",header = TRUE,sep="|")
DNS<- read.delim("D:/Users/MRT15076/Documents/Salidas/DNS.txt",header = TRUE,sep="|")
Doctos <- read.delim("D:/Users/MRT15076/Documents/Salidas/DOCTOS.txt",header = TRUE,sep="|")
Doctos <- read.delim("D:/Users/MRT15076/Documents/Salidas/TotalClientes_SaldosCajon2.txt",header = TRUE,sep="|")
Castigos <- read.delim("D:/Users/MRT15076/Documents/Salidas/castigos.txt",header=TRUE,sep="|")
Castigos <- select(Castigos,1,3,5,6)
catalogo<-read_delim("D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Arturo/CATALOGO_TIPDOCUMENT.txt", delim = "|")
#Parque (Perimetro)
Perimetro <- read_delim("Z:/POSPAGO/Cobranza/Actualizacion BD b2b (cobranza)/2019_Cartera_B2B.txt",
                        "\t", escape_double = FALSE, trim_ws = TRUE)
head(Perimetro)
Perimetro<- select(Perimetro,2:7)


### Renombrar columnas
colnames(Cliente) <- c("COD_CLIENTE","COD_CUENTA","HOLDING","SUBSIDIARIA","SEGMENTO","DES_CUENTA","COD_CATEGORIA","CICLO","NUM_IDENT","METODO_PAGO")
colnames(Prorrogas) <- c("COD_CLIENTE","FEC_PRORROGA","DIA_PRORROGA","INMUNE","VAL")
colnames(DNS) <- c("COD_CLIENTE","COD_SITUACION","NUM_ABONADO")



###Acomodar DNS
head(DNS)
levels(DNS$COD_SITUACION)
DNS <- mutate(DNS,AAA=ifelse(COD_SITUACION=="AAA",NUM_ABONADO,0))
DNS <- mutate(DNS,AIP=ifelse(COD_SITUACION=="AIP",NUM_ABONADO,0))
DNS <- mutate(DNS,ATP=ifelse(COD_SITUACION=="ATP",NUM_ABONADO,0))
DNS <- mutate(DNS,BAA=ifelse(COD_SITUACION=="BAA",NUM_ABONADO,0))
DNS <- mutate(DNS,BAP=ifelse(COD_SITUACION=="BAP",NUM_ABONADO,0))
DNS <- mutate(DNS,RTP=ifelse(COD_SITUACION=="RTP",NUM_ABONADO,0))
DNS <- mutate(DNS,SAA=ifelse(COD_SITUACION=="SAA",NUM_ABONADO,0))
DNS <- mutate(DNS,STP=ifelse(COD_SITUACION=="STP",NUM_ABONADO,0))
DNS <- mutate(DNS,TVP=ifelse(COD_SITUACION=="TVP",NUM_ABONADO,0))
DNS$COD_SITUACION <- NULL
DNS$NUM_ABONADO <- NULL
DNS <- mutate(DNS,TOTAL_DNS=rowSums(select(DNS,2:10)))
DNS <- DNS %>% group_by(COD_CLIENTE) %>%
  summarise(AAA=sum(AAA),AIP=sum(AIP),ATP=sum(ATP),BAA=sum(BAA),BAP=sum(BAP),RTP=sum(RTP),SAA=sum(SAA),STP=sum(STP),TVP=sum(TVP),TOTAL_DNS=sum(TOTAL_DNS))
head(DNS)


###Cruce Catalogo
catalogo<-select(catalogo,COD_TIPDOCUM,DES_TIPDOCUM)
Doctos <- left_join(Doctos,catalogo,by=c("COD_TIPDOCUM","COD_TIPDOCUM"))
Doctos <- select(Doctos,1,3:13,2,14)
head(Doctos)
##Castigos
Castigos <- select(Castigos,everything()) %>% group_by(COD_CLIENTE,FEC_CASTIGO) %>% 
  summarise(CASTIGO=sum(IMPORTE_CASTIGO),SALDO_CASTIGO=sum(SALDO_CASTIGO))
head(Castigos)


###Armado del LayOut Cartera
##Clietes vs Doctos
Cartera <- left_join(Cliente,Doctos,by=c("COD_CLIENTE","COD_CLIENTE"))
##Cartera vs Castigos
Cartera <- left_join(Cartera,Castigos,by=c("COD_CLIENTE","COD_CLIENTE"))
##Cartera vs Prorrogas
Cartera <- left_join(Cartera,Prorrogas,by=c("COD_CLIENTE","COD_CLIENTE"))
##Cartera vs DNS
Cartera <- left_join(Cartera,DNS,by=c("COD_CLIENTE","COD_CLIENTE"))
head(Cartera)
NET0605 %>% select (4,36:38) %>% filter(COD_CLIENTE==66229296)

###Comparación entre NET vs Cartera.

NET1006 <- read_excel("D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Arturo/NETFLOW_1006.xlsx")

Cartera2 <- Cartera %>% select(1,12:21,23,25,26,31:39)
Net2 <- NET1006 %>% select(4,12,23:33,37,38,43:52)
str(Cartera2)
str(Net2)
Comp <- full_join(SCL,NET1006,by=c("COD_CLIENTE","COD_CLIENTE"))
Comp %>% filter(COD_CLIENTE==66229296)
write.csv(Comp,file="D:/Users/MRT15076/Documents/Victor/4_CarteraUnificada/Comp0605.csv",row.names = FALSE)

head(Comp)


#### Cartera Mora
###Carga de archivos
CARTERA_0804 <- read_delim("Victor/NETFLOWS/Brallan/CARTERA_0804.txt",
                           "|", escape_double = FALSE, trim_ws = TRUE)

Cartera <- CARTERA_0804 %>% select(COD_CLIENTE,COD_TIPDOCUM,CAJON,IMPORTE)%>%
  group_by(COD_CLIENTE,COD_TIPDOCUM,CAJON)%>% 
  summarise(Importe=sum(IMPORTE))

catalogo<-read_delim("D:/Users/MRT15076/Documents/Victor/NETFLOWS/Arturo/CATALOGO_TIPDOCUMENT.txt", delim = "|")
catalogo <- select(catalogo,COD_TIPDOCUM,DES_TIPDOCUM)
#Join cartera vs catalogo para obtener el documento
Cart <- left_join(Cartera,catalogo,by=c("COD_TIPDOCUM","COD_TIPDOCUM"))
Cart <- as.data.frame(Cart)

Cart %>% select(COD_CLIENTE,DES_TIPDOCUM,Importe) %>% 
filter(COD_CLIENTE=="3364584")%>%
  summarise(Importe=sum(Importe))



Total <- C100 %>%select(COD_CLIENTE,COD_CUENTA,NOM_CLIENTE,COD_CICLO,COD_SITUACION,COD_ESTADO) %>% 
  group_by(COD_CLIENTE,COD_CUENTA,COD_CICLO,COD_SITUACION,COD_CICLO)


M7Miguel <- read_csv("D:/Users/MRT15076/Desktop/M7Miguel.csv")
str(NET2405)
NET <- NET2405 %>% select(COD_CLIENTE,25:37,39:40)
M7Miguel_SALDOS <- inner_join(M7Miguel,NET,by=c("COD_CLIENTE","COD_CLIENTE"))
write.csv(M7Miguel_SALDOS,"D:/Users/MRT15076/Desktop/M7Miguel_SALDOS.csv",row.names = FALSE)


library(readr)
Blue_Label <- read_csv("Salidas/Blue Label.txt", 
                       col_names = TRUE)
colnames(Blue_Label) <- c("COD_CLIENTE")
Doctos <-
  tbl(SCL,"CO_CARTERA")
MikeBlueLabel <- inner_join(local(Blue_Label),Doctos,by=c("COD_CLIENTE","COD_CLIENTE"),copy=TRUE)


Sergio <-
  tbl(SCL,"FA_HISTDOCU") %>% select(COD_CLIENTE,NUM_FOLIO)

TotalCartera <- read_csv("D:/Users/MRT15076/Documents/Salidas/Libro3.csv")

Libro3_1 <- left_join(local(TotalCartera),Sergio,by=c("COD_CLIENTE","COD_CLIENTE"),copy=TRUE)
Libro3_1 <- tbl_df(Libro3_1[!duplicated(Libro3_1$COD_CLIENTE), ])
write.csv(Libro3_1,file="Salidas/Libro3_1.csv",row.names = FALSE)

head(Libro3_1)