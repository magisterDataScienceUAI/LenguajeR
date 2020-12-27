library(tidyverse)
library(readxl)
library(dplyr)

path <- file.path("C:/Users/Felipe/Desktop/COMPARTIR MAC/Magister/R/Taller 2/DEF_2017.csv")
path2 <- file.path("C:/Users/Felipe/Desktop/COMPARTIR MAC/Magister/R/Taller 2/Proyeccion Chile.xlsx")

DEF <- read.csv(path, skip = 1, head = FALSE, sep=";")
#Cambiaremos el nombre de las Columnas de V1 a el nombre original en el archuivo CSV
columnas <- c("DIA_NAC","MES_NAC","ANO1_NAC","ANO2_NAC","SEXO","EST_CIVIL","EDAD_TIPO","EDAD_CANT","CURSO_INS","NIVEL_INS","ACTIV","OCUPA","CATEG","DIA_DEF","MES_DEF","ANO_DEF","LOCAL_DEF","COMUNA","URBA_RURAL"
              ,"DIAG1","DIAG2","AT_MEDICA","CAL_MEDICO","FUND_CAUSA","PESO","GESTACION","NUTRITIVO","EDAD_M","EST_CIV_MA","HIJ_VIVOS","HIJ_FALL","HIJ_MORT","HIJ_TOTAL","PARTO_ABOR","DIA_PARTO","MES_PARTO"
              ,"ANO_PARTO","ACTIV_MA","OCUPA_MA","CATEG_MA","CURSO_MA","NIVEL_MA","EDAD_PADRE","ACTIV_PA","OCUPA_PA","CATEG_PA","CURSO_PA","NIVEL_PA","REG_RES","SERV_RES")

names(DEF) <- columnas
PROY = read_excel(path2)
###################FIN LECTURA EXCEL####################

#lEEREMOS SOLAMENTE LOS DATOS SOLICITADOS

dataF = PROY %>%
  dplyr::select(ID_Region:División,'2018':'2025')

dataF = dataF[dataF$División =='Total', c("2018","2019","2020","2021","2022","2023","2024","2025")]
head(dataF)


dataGat= gather(dataF, key = Año, value = Pob, )

dataGat =aggregate(dataGat$Pob, by=list(Año=dataGat$Año), FUN=sum)
columna = c("Año","Pob")
names(dataGat)=columna
#Ahora debemos obtener la tasa de defunción, que sería el total de fallecidos 2017 / población estimada 2017
#total de muertos en 2017:
t_muertos_2017=nrow(DEF)
#Ahora la población estimada para el 2017
PROY_2017 = sum(PROY[PROY$División =='Total', c("2017")])
tasaE = t_muertos_2017/PROY_2017
DEF_E = dataGat$Pob*tasaE

dataGat <- cbind(dataGat[, c(1, 2)], DEF_E)

