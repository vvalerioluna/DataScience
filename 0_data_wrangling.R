library(readxl)
library(dplyr)
library(stringr)
library(tidyverse)


## Definimos mes
mes<-9

## Leemos data base (packaging, people y ZBS)
data_marco<-read_xlsx("C:/Users/USER/OneDrive - Anheuser-Busch InBev/TTP/Clustering/MAZ_TTP_BASE_MODEL.xlsx",sheet = "BASE")


## Leemos base de global (NOTA: hay que quitarle manualmente la primera linea)
global<-read_xlsx("./2021 Extraccion Anual BOP's.xlsx",sheet = "Extraction",
                  col_types = c("text","numeric","numeric","text","text","text","text","text","text","text","text","text","text","text",
                                "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

#Ajustado para validar

global<-read_xlsx("C:/Users/USER/OneDrive - Anheuser-Busch InBev/TTP/Clustering/2022 Extraccion Anual BOP's.xlsx",sheet = "Extraction_lines",skip = 1,
                  col_types = c("text","numeric","numeric","text","text","text","text","text","text","text","text","text","text","text",
                                "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

# ## PARA CALCULAR MODA DE TIPO DE FORMATO DE LA DATA DE P360
# base<-read_xlsx("./Reporte_BD_2020-05-11_20.53.11.xlsx",sheet = "BD PRODUCCION")

# mode <- function(codes){
#   which.max(tabulate(as.integer(codes)))
# }
# 
# base_group<-base %>% group_by(ID) %>% summarise(mode=mode(`TIPO FORMATO`))

unique(global$`N° MONTH`)
unique(global$`LOCATION TYPE`)

## CALCULO DE CONTAINER LOSSES (%) YTD DE LA BASE DE GLOBAL
# PB-R0010 es cerveza, PB-R0030 es soft y PB-R0050 es agua.
kpis_total_packed<-c("PB-R0010","PB-R0030","PB-R0050","PC-R0010","PC-R0030","PC-R0050",
                         "PK-R0010","PK-R0030","PK-R0050","PS-R0010","PS-R0030","PS-R0050",
                         "PP-R0010","PP-R0030","PP-R0050")

####Kpi-Volumen
lines_total_packed<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="LINE",`KPI Code` %in% c(kpis_total_packed)) %>% 
  group_by(`Line Name`) %>% summarise(total_packed_ytd=sum(AC...22))


# Para GLY por linea
lines_gly<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="LINE",`KPI Code` =="PG-K0812") %>% 
  group_by(`Line Name`) %>% summarise(gly_ytd=first(AC...22))

# Para LEF por linea
lines_lef<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="LINE",`KPI Code` =="PG-K1312") %>% 
  group_by(`Line Name`) %>% summarise(lef_ytd=first(AC...22))

# Para TT por linea
lines_tt<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="LINE",`KPI Code` =="GE-R0060") %>% 
  group_by(`Line Name`) %>% summarise(tt_ytd=first(AC...22))

# Para ST por linea
lines_st<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="LINE",`KPI Code` =="PG-R0080") %>% 
  group_by(`Line Name`) %>% summarise(st_ytd=first(AC...22))

# Para EPT por linea
lines_ept<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="LINE",`KPI Code` =="PG-R0060") %>% 
  group_by(`Line Name`) %>% summarise(ept_ytd=first(AC...22))

# Para LET por linea
lines_let<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="LINE",`KPI Code` =="PG-R0090") %>% 
  group_by(`Line Name`) %>% summarise(let_ytd=first(AC...22))

# Para avcot por linea
lines_avcot<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="LINE",`KPI Code` =="PG-K0977") %>% 
  group_by(`Line Name`) %>% summarise(avcot_ytd=first(AC...22))


# Consolidamos todas 
lines_data<-lines_total_packed %>% left_join(lines_gly) %>% left_join(lines_lef) %>% left_join(lines_tt) %>% 
  left_join(lines_st) %>% left_join(lines_ept) %>% left_join(lines_let) %>% 
  left_join(lines_avcot) %>% mutate(line_name_espacios=str_replace_all(`Line Name`," ",""))


#unimos base con nivel linea y nivel planta
base_final<-data_marco %>% left_join(lines_data,by=c("Line Name ESPACIOS_APAN"="line_name_espacios")) 


############ DATA P360
# Para actualizar Container Size de P360

pac<-read_xlsx("C:/Users/USER/OneDrive - Anheuser-Busch InBev/TTP/Clustering/Reporte_BD_2021-03-15_19.24.10.xlsx"
,sheet="BD PRODUCCION")


pac_id<-pac %>% mutate(ID=paste0(PLANTA,LÍNEA)) %>% filter(FECHA>='2021-01-01')

pac_group<-pac_id %>% group_by(ID,`FORMATO VOLUMEN`) %>% summarise(tot_prod=sum(`PRODUCCIÓN LIQUIDA REAL (HL)`)) %>% 
  top_n(1)


base_clean<-base_final %>% left_join(pac_group,by=c("id_p360"="ID")) %>% 
  mutate(`FORMATO VOLUMEN`=if_else(`FORMATO VOLUMEN` > 100000 | is.na(`FORMATO VOLUMEN`),
                                   container_size_ant,`FORMATO VOLUMEN`)) %>% 
  mutate("NST YTD [hours]"=tt_ytd-st_ytd,"TTP per line"=
           if_else(is.na(total_packed_ytd/((st_ytd+`NST YTD [hours]` )*
                                             `Total FTEs`)) |
                                             is.nan(total_packed_ytd/((st_ytd+`NST YTD [hours]` )*
                                              `Total FTEs`)) |is.infinite(total_packed_ytd/((st_ytd+`NST YTD [hours]` )
                                            *`Total FTEs`))  ,0,
   total_packed_ytd/((st_ytd+`NST YTD [hours]` )*`Total FTEs`))) %>% 
  rename("GLY YTD [%]"=gly_ytd,"LEF YTD [%]"=lef_ytd,"TT YTD (hours)"=tt_ytd,"ST YTD (hours)"=st_ytd,
         "EPT YTD (hours)"=ept_ytd,"LET YTD (hours)"=let_ytd,"AVCOT YTD [hours]"=avcot_ytd,
         "Total Packed Khl ACT YTD"=total_packed_ytd,"Line Name"=`Line Name.x`,"Container Size"=`FORMATO VOLUMEN`)
namesbase_clean

base_clean <- base_clean %>% select(-c(tot_prod,container_size_ant,`Line Name.y`))



#Agregamos ceros en lugar de NA

base_clean$Depalletizer[which(is.na(base_clean$Depalletizer))]<-0
base_clean$Palletizer[which(is.na(base_clean$Palletizer))]<-0
base_clean$Unpacker[which(is.na(base_clean$Unpacker))]<-0
base_clean$Packer[which(is.na(base_clean$Packer))]<-0
base_clean$`Container washer`[which(is.na(base_clean$`Container washer`))]<-0
base_clean$`Crate washer`[which(is.na(base_clean$`Crate washer`))]<-0
base_clean$`Bottle Inspector (BVI)`[which(is.na(base_clean$`Bottle Inspector (BVI)`))]<-0
base_clean$Pasteurizer[which(is.na(base_clean$Pasteurizer))]<-0
base_clean$Blowing[which(is.na(base_clean$Blowing))]<-0
base_clean$Fillers[which(is.na(base_clean$Fillers))]<-0
base_clean$Labellers[which(is.na(base_clean$Labellers))]<-0

base_clean$Depalletizer[which(base_clean$Depalletizer=="NA")]<-0
base_clean$Palletizer[which(base_clean$Palletizer=="NA")]<-0
base_clean$Unpacker[which(base_clean$Unpacker=="NA")]<-0
base_clean$Packer[which(base_clean$Packer=="NA")]<-0
base_clean$`Container washer`[which(base_clean$`Container washer`=="NA")]<-0
base_clean$`Crate washer`[which(base_clean$`Crate washer`=="NA")]<-0
base_clean$`Bottle Inspector (BVI)`[which(base_clean$`Bottle Inspector (BVI)`=="NA")]<-0
base_clean$Pasteurizer[which(base_clean$Pasteurizer=="NA")]<-0
base_clean$Blowing[which(base_clean$Blowing=="NA")]<-0
base_clean$Fillers[which(base_clean$Fillers=="NA")]<-0
base_clean$Labellers[which(base_clean$Labellers=="NA")]<-0

write.csv(base_clean,"MAZ_TTP.csv",row.names = FALSE)

################################################# CLUSTERING ###########################################
#---------------------------------- BOTTLE

clust_bot <- base_clean %>% filter(Container=="Bottle") %>% filter(Validation=="OK") %>% 
  select(`Line ID`,`GLY YTD [%]`,`LEF YTD [%]`,`TTP per line`,`Total Packed Khl ACT YTD`,`NST YTD [hours]`,
         `AVCOT YTD [hours]`,
         `Container Size`,Model,
         `Nominal Speed [units/h]`,`Total FTEs`,`Qty FTEs x shift`,
         `Total Big Machines`, `ZBB Budget (USD)`,`Pasteurizer Discrete`, 
         `Autonomous Maturity Level Discrete`,`Line Layout Discrete`,
         `Line Automation Level Discrete`)

#Por tipo de contenedor (aquellas lineas ej botella); el de validación (las que estan ok); se queda solo lo necesario para el cluster
#
clust_clean<-clust_bot%>% select(-c(`Line ID`)) %>% as.data.frame() 
row.names(clust_clean)<-clust_bot$`Line ID`
clust_clean$`Container Size`<-as.numeric(clust_clean$`Container Size`)
clust_clean$`ZBB Budget (USD)`<-as.numeric(clust_clean$`ZBB Budget (USD)`)
#clust_clean_std<-scale(clust_clean)

d_bot<-dist(clust_clean %>% scale(),method = "euclidean")
h_bot<-hclust(d_bot,method = "complete") #cluster jerarquico,con base a las variables. todo esto con base a lineas envasados

# plot(h_bot,cex=0.6)
# rect.hclust(h_bot, k = 14,border=2:15)

bottles<-cbind(h_bot$labels,cutree(h_bot,k=14))#el valor 14 se determino con los involucrados para llegar a un match con la operacon

bottle<-as.data.frame(cutree(h_bot,k=14))
colnames(bottle)<-"cluster"




#-------------------------------- CAN

clust_can <- base_clean %>% filter(Container=="Can")  %>% filter(Validation=="OK") %>% 
  select(`Line ID`,`GLY YTD [%]`,`LEF YTD [%]`,`TTP per line`,`Total Packed Khl ACT YTD`,`NST YTD [hours]`,
         `AVCOT YTD [hours]`,
         `Container Size`,Model,
         `Nominal Speed [units/h]`,`Total FTEs`,`Qty FTEs x shift`,
         `Total Big Machines`, `ZBB Budget (USD)`,`Pasteurizer Discrete`, 
         `Autonomous Maturity Level Discrete`,`Line Layout Discrete`,
         `Line Automation Level Discrete`)

clust_clean<-clust_can%>% select(-c(`Line ID`)) %>% as.data.frame() 
row.names(clust_clean)<-clust_can$`Line ID`
clust_clean$`Container Size`<-as.numeric(clust_clean$`Container Size`)
clust_clean$`ZBB Budget (USD)`<-as.numeric(clust_clean$`ZBB Budget (USD)`)
#clust_clean_std<-scale(clust_clean) %>% na.omit()

d_can<-dist(clust_clean%>% scale(),method = "euclidean")
h_can<-hclust(d_can,method = "complete")

# plot(h_can,cex=0.6)
# rect.hclust(h_can, k=6,border=2:7)

can<-as.data.frame(cutree(h_can,k=6))
colnames(can)<-"cluster"



#--------------------------------- PET

clust_pet <- base_clean %>% filter(Container=="PET") %>% filter(Validation=="OK") %>% 
  select(`Line ID`,`GLY YTD [%]`,`LEF YTD [%]`,`TTP per line`,`Total Packed Khl ACT YTD`,`NST YTD [hours]`,
         `AVCOT YTD [hours]`,
         `Container Size`,Model,
         `Nominal Speed [units/h]`,`Total FTEs`,`Qty FTEs x shift`,
         `Total Big Machines`, `ZBB Budget (USD)`,`Pasteurizer Discrete`, 
         `Autonomous Maturity Level Discrete`,`Line Layout Discrete`,
         `Line Automation Level Discrete`)

clust_clean<-clust_pet%>% select(-c(`Line ID`)) %>% as.data.frame() 
row.names(clust_clean)<-clust_pet$`Line ID`
clust_clean$`Container Size`<-as.numeric(clust_clean$`Container Size`)
clust_clean$`ZBB Budget (USD)`<-as.numeric(clust_clean$`ZBB Budget (USD)`)
#clust_clean_std<-scale(clust_clean) %>% na.omit()

d_pet<-dist(clust_clean%>% scale(),method = "euclidean")
h_pet<-hclust(d_pet,method = "complete")

# plot(h_pet,cex=0.6)
# rect.hclust(h_pet, k=7,border=2:8)


pet<-as.data.frame(cutree(h_pet,k=7))
colnames(pet)<-"cluster"


#---------------------------------- KEG
clust_keg <- base_clean %>% filter(Container=="KEG") %>% filter(Validation=="OK") %>% 
  select(`Line ID`,`GLY YTD [%]`,`LEF YTD [%]`,`TTP per line`,`Total Packed Khl ACT YTD`,`NST YTD [hours]`,
         `AVCOT YTD [hours]`,
         `Container Size`,Model,
         `Nominal Speed [units/h]`,`Total FTEs`,`Qty FTEs x shift`,
         `Total Big Machines`, `ZBB Budget (USD)`,`Pasteurizer Discrete`, `Autonomous Maturity Level Discrete`,`Line Layout Discrete`,
         `Line Automation Level Discrete`)

clust_clean<-clust_keg%>% select(-c(`Line ID`)) %>% as.data.frame() 
row.names(clust_clean)<-clust_keg$`Line ID`
clust_clean$`Container Size`<-as.numeric(clust_clean$`Container Size`)
clust_clean$`ZBB Budget (USD)`<-as.numeric(clust_clean$`ZBB Budget (USD)`)
#clust_clean_std<-scale(clust_clean) %>% na.omit()

d_keg<-dist(clust_clean%>% scale(),method = "euclidean")
h_keg<-hclust(d_keg,method = "complete")

#plot(h_keg,cex=0.6)
#rect.hclust(h_keg, k=2,border=2:3)

keg<-as.data.frame(cutree(h_keg,k=2))
colnames(keg)<-"cluster"


#------------------------------- Data Frame Final
df_clusters<-rbind(bottle,can,pet,keg)
#write.csv(df_clusters,"clusters_lineas.csv")

########################################### CALCULO FTE OPPORTUNITIES ##############################

#Nota: 

"Line ID"<-row.names(df_clusters)
clusters<-cbind(`Line ID`,cluster=df_clusters$cluster) %>% as.data.frame()
clusters$cluster<-as.numeric(clusters$cluster)

base_validated<-base_clean %>% filter(Validation=="OK" ) %>% left_join(clusters) %>% 
  mutate(cluster_id=paste0(Container," C",cluster)) %>% mutate(cluster=if_else(`Line ID`=="UIO LINEA 5",1,cluster),
                                                               cluster_id=if_else(`Line ID`=="UIO LINEA 5","Bag C1",cluster_id))

# se toma el percentil 83 
avg_clust<-base_validated %>% group_by(cluster_id) %>%
  summarise("Avg. Qty FTEs x Shifts in Cluster"=quantile(`Qty FTEs x shift`,0.83)) %>% ungroup()


base_validated_clust<-base_validated %>% left_join(avg_clust) %>% 
  mutate("FTEs Opportunity"=if_else(`Qty FTEs x shift`<= `Avg. Qty FTEs x Shifts in Cluster`,0,
                                    floor((`Qty FTEs x shift`-`Avg. Qty FTEs x Shifts in Cluster`)*`Shifts x day`)))

# quedo una diferencia de 1 fte en la linea Santo Domingo - Bottling Line 6, pues el floor() en lugar de 5 pone 


write.csv(base_validated_clust,"MAZ_CLUSTERS.csv",row.names = FALSE)

#################################################################################################################
#################################################################################################################
####################################### CALCULO AVCOT Y TTP A NIVEL PLANTA ######################################
#################################################################################################################
#################################################################################################################

# Para avcot por planta
plant_avcot<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="COUNTRY_GROUP",`KPI Code` =="PG-K0977") %>% 
  group_by(`Country Group Name`) %>% summarise(avcot_plant_ytd=first(AC...22)) %>% 
  filter(`Country Group Name`!="BU Mexico")

avcot_mex<-global %>%  filter(`N° MONTH`==mes,`LOCATION TYPE`=="PLANT_GROUP",
                              `Plant Group Name`  %in% c("Mexico_Dom_PG_Name","Mexico_EXP_PG_Name"),
                              `KPI Code` =="PG-K0977") %>%
  group_by(`Plant Group Name`) %>% 
  summarise(avcot_plant_ytd=first(AC...22))
names(avcot_mex)[1]<-"Country Group Name"

plant_avcot_final<-rbind(plant_avcot,avcot_mex)

# Para TTP por planta
plant_ttp<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="COUNTRY",`KPI Code` =="PG-K0412") %>% 
  group_by(`Country Group Name`) %>% summarise(ttp_plant_ytd=first(AC...22)) %>% 
  filter(`Country Group Name`!="BU Mexico")

ttp_mex<-global %>%  filter(`N° MONTH`==mes,`LOCATION TYPE`=="PLANT_GROUP",
                            `Plant Group Name`  %in% c("Mexico_Dom_PG_Name","Mexico_EXP_PG_Name"),
                            `KPI Code` =="PG-K0412") %>%
  group_by(`Plant Group Name`) %>% 
  summarise(ttp_plant_ytd=first(AC...22))
names(ttp_mex)[1]<-"Country Group Name"

plant_ttp_final<-rbind(plant_ttp,ttp_mex)


#################### TOTAL PRODUCTION VOLUME Y ATTENDED HOURS POR PLANTA PARA TTP #####################

# Para TTP por planta
plant_tpv<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="PLANT",`KPI Code` =="PG-K0411") %>% 
  group_by(`Plant Name`) %>% summarise(tpv_plant_ytd=first(AC...22)) 

plant_att_hrs<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="PLANT",`KPI Code` =="PG-S0410") %>% 
  group_by(`Plant Name`) %>% summarise(hrs_plant_ytd=first(AC...22)) 

plant_reempacados<-global %>% filter(`N° MONTH`==mes,`LOCATION TYPE`=="PLANT",`KPI Code` =="PG-K0340") %>% 
  group_by(`Plant Name`) %>% summarise(reempacados=first(AC...22)) 

plant_ttp<-plant_tpv %>% left_join(plant_att_hrs,by="Plant Name") %>% 
  left_join(plant_reempacados,by="Plant Name") %>% 
  rename("plant"=`Plant Name`) %>% 
  mutate(plant=case_when(plant=="Mexico Apan"~"Apan",
                         plant=="GUADALAJARA"~"Guadalajara",
                         plant=="MAZATLAN"~"Mazatlan",
                         plant=="TORREON"~"Torreon",
                         plant=="ZACATECAS"~"Zacatecas",
                         plant=="Bridgetown"~"Barbados Beer Banks",
                         TRUE~plant))

write.csv(plant_ttp,"MAZ_PLANT_BEEROMETER.csv",row.names = FALSE)

