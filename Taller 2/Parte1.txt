library(readr)
library(readxl)
#al ver el enunciado es igual al taller 1 es por esta razón que solamente se cambia el nombre de las columnas

path <- file.path("C:/Users/Felipe/Documents/DEF_2017.csv")
path2 <- file.path("C:/Users/Felipe/Desktop/COMPARTIR MAC/Magister/R/Taller 2/Proyeccion Chile.xlsx")

DEF <- read_delim(path, ";", escape_double = FALSE, trim_ws = TRUE)
PROY <- read_excel(path2)

PROY = PROY[, c("ID_Region","Region","División","2017")]
colnames(PROY)[4] = "Población"
PROY_T = PROY[PROY$División =="Total",c("ID_Region", "Region", "Población")]

# DEF: Número de defunciones por región
t1 = data.frame(table(DEF$REG_RES))
colnames(t1) = c("ID_Region","DEF")

# HAB: Número de habitantes por región lo entrega la proyección Se une con el número de defunciones!
tabla_final = merge(PROY_T, t1, by.x = "ID_Region", by.y ="ID_Region")


# REG: Región (Escrito como: “Región de Tarapacá”, ...)
tabla_final$Region = paste("Región de",tabla_final$Region, sep=" ")


# TASA_MORT: Tasa de mortalidad regional por cada 1000 habitantes.
tabla_final$TASA_MORT = 1000*tabla_final[,4]/tabla_final[,3]

# TASA_MASC: Tasa de masculinidad regional de fallecidos por cada 1000 mujeres.
HOMBRES_FALLECIDOS = data.frame(table(DEF$REG_RES[DEF$SEXO==1]))
colnames(HOMBRES_FALLECIDOS) = c("ID_Region", "Total_Hombres")
MUJERES_FALLECIDAS = data.frame(table(DEF$REG_RES[DEF$SEXO==2]))
colnames(MUJERES_FALLECIDAS) = c("ID_Region", "Total_Mujeres")

t3 = merge(HOMBRES_FALLECIDOS, MUJERES_FALLECIDAS)
t3$TASA_MASC = 1000*t3$Total_Hombres/t3$Total_Mujeres
t3 = t3[,c("ID_Region","TASA_MASC")]

tabla_final = merge(tabla_final, t3, by.x = "ID_Region", by.y ="ID_Region")


# EDAD_PROM: Edad promedio de defunción por región.
## Hay que considerar que edad en meses, días o minutos es 0 años.
edad = DEF$EDAD_CANT
edad[DEF$EDAD_TIPO>=2] = 0
edad[DEF$EDAD_CANT==999] = NA

t4 = aggregate(edad~DEF$REG_RES, FUN=mean) 
colnames(t4) = c("ID_Region","EDAD_PROM")
tabla_final = merge(tabla_final, t4, by.x = "ID_Region", by.y ="ID_Region")



# PASIST: Porcentaje de defunciones en Hospital o Clínica, por región.
ind_HC = 100*(DEF$LOCAL_DEF==1)
t5= aggregate(ind_HC~DEF$REG_RES, FUN=mean) 
colnames(t5) = c("ID_Region","PASIST")
tabla_final = merge(tabla_final, t5, by.x = "ID_Region", by.y ="ID_Region")


# PDEFC: Porcentaje de defunciones asociadas a una causa de muerte que a usted le interese (Ver archivo CODIGO CAUSA DE MUERTE)
letra=substr(DEF$DIAG1, 1,1)

## Por ejemplo: C -> 
enfermedad = 100*(letra=="C")
t6 = aggregate(enfermedad~DEF$REG_RES, FUN=mean)
colnames(t6) = c("ID_Region","PDEFC")
tabla_final = merge(tabla_final, t6, by.x = "ID_Region", by.y ="ID_Region")
columnas= c("REG","HAB","DEF","TASA_MORT","TASA_MASC","EDAD_PROM","PASIST","PDEFC")
names(tabla_final)=columnas