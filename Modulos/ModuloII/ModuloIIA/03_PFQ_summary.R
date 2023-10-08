
library(data.table)

data_base
colnames(data_base)

data_base$T_fuente             #
data_base$Ph                   #
data_base$pH_mV
data_base$Eh
data_base$ORP_mv               #
data_base$CE_uS_cm             #
data_base$TDS_mg_L             #
data_base$Salin_PSU            #
data_base$R_Kohm_cm            #
data_base$OD_mgL
data_base$OD_porSat

summary(data_base)
summary(data_base$Ph)

data_base %>% select(contains("Codigo"), c("Temporada"), c("Ph"), c("Eh"), c("ORP_mv"), c("CE_uS_cm"), 
                     c("TDS_mg_L"), c("Salin_PSU"), c("R_Kohm_cm"), c("OD_mgL"))


##############################################################################################################
attach(data_base)
data_base


estadisticos <- function(col){
  
  norm_test <- shapiro.test(col)
  value <- c(round(length(col),3),round(sum(is.na(col))),round(min(col, na.rm=TRUE),3),
             round(max(col,na.rm=TRUE),3),
             round(quantile(col, 0.05,na.rm=TRUE),3),
             round(quantile(col, 0.25,na.rm=TRUE),3), round(mean(col,na.rm=TRUE),3), round(median(col,na.rm=TRUE),3),
             round(mean(col,trim = 0.10,na.rm=TRUE),3),
             round(quantile(col, 0.75,na.rm=TRUE),3), 
             round(quantile(col, 0.95,na.rm=TRUE),3),
             round(IQR(col,na.rm=TRUE),3),
             round(mad(col,na.rm=TRUE),3),
             round(sd(col,na.rm=TRUE),3),round(skew(col,na.rm=TRUE),3), round(kurtosi(col,na.rm=TRUE),3), 
             round((sd(col,na.rm=TRUE)/mean(col,na.rm=TRUE))*100,3),
             norm_test$statistic, norm_test$p.value)
}

statistic <- c("N","Nulos","Minimo", "Maximo","P5 (5%)","Q1 (25%)","Media Aritmetica","Mediana",
               "Trimmed mean (10%)","Q3 (75%)","P95 (95%)", "RIQ","MAD","Sd","As","K","CV",
               "Shapiro statistic", "Shapiro p-valor")

T2PRO <- sapply(data_base %>% select(T_fuente, Ph, CE_uS_cm, TDS_mg_L, Salin_PSU, R_Kohm_cm, OD_mgL), 
                estadisticos) 

rownames(T2PRO) <- statistic
View(T2PRO)



##############################################################################################################


statistic <- c("N","Nulos","Minimo", "Maximo","P5 (5%)","Q1 (25%)","Media Aritmetica","Mediana",
               "Trimmed mean (10%)","Q3 (75%)","P95 (95%)", "RIQ","MAD","Sd","As","K","CV",
               "Shapiro statistic", "Shapiro p-valor")
T2PRO <- sapply(data_base %>% select(T_fuente, Ph, CE_uS_cm, TDS_mg_L, Salin_PSU, R_Kohm_cm, OD_mgL) %>%
                  filter(Temporada=="Avenida"), estadisticos) 

rownames(T2PRO) <- statistic
View(T2PRO)

##############################################################################################################

data_base




#03 BD NORMAL SIN "<"
#04 BD NORMAL SIN "<", sin columnas de calculo de Balance iónico
#05 corrigiendo TDS
#06 BD CON "<" DE PRUEBA









