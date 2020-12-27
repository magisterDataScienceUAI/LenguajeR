library(tidyverse)
library(readxl)
library(dplyr)

path <- file.path("C:/Users/Felipe/Desktop/COMPARTIR MAC/Magister/R/Taller 2/DEF_2017.csv")

DEF <- read.csv(path, skip = 1, head = FALSE, sep=";")
#Cambiaremos el nombre de las Columnas de V1 a el nombre original en el archuivo CSV
columnas <- c("DIA_NAC","MES_NAC","ANO1_NAC","ANO2_NAC","SEXO","EST_CIVIL","EDAD_TIPO","EDAD_CANT","CURSO_INS","NIVEL_INS","ACTIV","OCUPA","CATEG","DIA_DEF","MES_DEF","ANO_DEF","LOCAL_DEF","COMUNA","URBA_RURAL"
              ,"DIAG1","DIAG2","AT_MEDICA","CAL_MEDICO","FUND_CAUSA","PESO","GESTACION","NUTRITIVO","EDAD_M","EST_CIV_MA","HIJ_VIVOS","HIJ_FALL","HIJ_MORT","HIJ_TOTAL","PARTO_ABOR","DIA_PARTO","MES_PARTO"
              ,"ANO_PARTO","ACTIV_MA","OCUPA_MA","CATEG_MA","CURSO_MA","NIVEL_MA","EDAD_PADRE","ACTIV_PA","OCUPA_PA","CATEG_PA","CURSO_PA","NIVEL_PA","REG_RES","SERV_RES")

names(DEF) <- columnas

###################FIN LECTURA EXCEL####################

#Función 

num_def_fecha<-function(fecha){
       # Debe recibir la fecha en formato: DÍA-MES-AÑO, se debe entregar el número de personas fallecidas que habrían desde el inicio del año de la fecha ingresada hasta la fecha, EJ:
      # 11-11-2020, desde 01-01-2020 a 11-11-2020.  
  if(is.character(fecha)) {
      d <- try( as.Date( fecha, format= "%d-%m-%Y" ) )
      if( class( d ) == "try-error" || is.na( d ) ) {
        print( "Debe ingresar una fecha correcta!") 
      }else{
         if(lubridate::year(d)==2017){
           print("#################### La fecha cumple los requisito #######################")
           #Ahora empieza el trabajo, seleccionaremos solamente las columnas a trabajar:
           dataF = DEF %>%
             dplyr::select(DIA_DEF,MES_DEF,ANO_DEF,)
           #sapply(dataF, function(x) sum(is.na(x)))
           #dataF$DIA_DEF[dataF$DIA_DEF == 0] <- "NA"
            total_0=sum(dataF$DIA_DEF == 0)
            total_espacios=sum(dataF$DIA_DEF == "")
            total_0_mes=sum(dataF$MES_DEF == 0)
            total_espacios_mes=sum(dataF$MES_DEF == "")
            filasNa <- apply(dataF, 1, function(x){any(is.na(x))})
            filasNulas <- apply(dataF, 1, function(x){any(is.null(x))}) 
            filasNa=sum(row.has.na)
            filasNulas=sum(filasNulas)
            
           if (total_0 > 0 | total_espacios > 0 | total_0_mes > 0 | total_espacios_mes > 0 | filasNulas> 0 | filasNa >0){
             print("Hay valores con 0")
           }else{
             print("Se han eliminados los valores nulos o en valor 0")
            vector=paste(dataF$DIA_DEF,"-",dataF$MES_DEF,"-",dataF$ANO_DEF)
            vector=gsub(" ", "",vector)
            vector=as.Date( vector, format= "%d-%m-%Y" )
            vector=sort(vector)
            cont=0
            for (i in 1:length(vector)) {
               if(vector[i] <= d){
                 cont=cont+1
               }else{
                 print(vector[i])
                 print("No es menor")
                 break
               }
              
            }
            
            print("El total de fallecidos a la fecha ingresada es de: ")
            print(cont)
            
             #dataN= dataF[paste(dataF$DIA_DEF,dataF$MES_DEF, dataF$ANO_DEF, sep="-")]
  
         }
           # head(dataF,100)  
         }else{
           print("Lo sentimos, solamente tenemos el año 2017 para dar respuestas")
         }
        
      }
       
  } else {
    print("Lo sentimos, debe ingresar una fecha con el formato:  día-mes-año 11-11-2020") }

}

# Estos son datos de prueba:

num_def_fecha(fecha="a")
num_def_fecha(fecha=2)
num_def_fecha(fecha="11-11-2020")
num_def_fecha(fecha="11-13-2020")
num_def_fecha(fecha="11-11-2017")