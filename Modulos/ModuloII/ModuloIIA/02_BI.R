#### Cationes ##########################################################################################
#ROSS

# Calcio
data_base$Ca_valdis <- as.numeric(str_remove(data_base$Ca_dis, pattern = "<"))
data_base$Ca_valdis_cen <- ifelse(str_detect(data_base$Ca_dis, pattern = "<"), TRUE, FALSE)
data_base$Ca_valdis_cen
data_base[ ,c("Ca_valdis", "Ca_valdis_cen")]

attach(data_base)
Ca.ros <- cenros(Ca_valdis, Ca_valdis_cen)
Ca.ros <- as.data.frame(Ca.ros)
id <- which(Ca.ros$censored) 
Ca.ros[id, ]

val0 <- unique(grep("<", data_base$Ca_dis, value=TRUE))
id <- which(data_base$Ca_dis==val0)
data_base$Ca_com <- data_base$Ca_dis

for (i in 1:length(id)){
  replace1 <- Ca.ros$modeled[i]
  data_base[id, ]$Ca_com[i] <- replace1
}

data_base[order(data_base$Ca_dis),][ , c("Codigo","Ca_com","Ca_dis")][16:40,]

####

# Magnesio
data_base$Mg_valdis <- as.numeric(str_remove(data_base$Mg_dis, pattern = "<"))
data_base$Mg_valdis_cen <- ifelse(str_detect(data_base$Mg_dis, pattern = "<"), TRUE, FALSE)
data_base$Mg_valdis_cen
data_base[ ,c("Mg_valdis", "Mg_valdis_cen")]

attach(data_base)
Mg.ros <- cenros(Mg_valdis, Mg_valdis_cen)
Mg.ros <- as.data.frame(Mg.ros)
id <- which(Mg.ros$censored) 
Mg.ros[id, ]

val0 <- unique(grep("<", data_base$Mg_dis, value=TRUE))
id <- which(data_base$Mg_dis==val0)
data_base$Mg_com <- data_base$Mg_dis

for (i in 1:length(id)){
  replace1 <- Mg.ros$modeled[i]
  data_base[id, ]$Mg_com[i] <- replace1
}

data_base[order(data_base$Mg_dis),][ , c("Codigo","Mg_com","Mg_dis")]

####

# Sodio
data_base$Na_valdis <- as.numeric(str_remove(data_base$Na_dis, pattern = "<"))
data_base$Na_valdis_cen <- ifelse(str_detect(data_base$Na_dis, pattern = "<"), TRUE, FALSE)
data_base$Na_valdis_cen
data_base[ ,c("Na_valdis", "Na_valdis_cen")]

attach(data_base)
Na.ros <- cenros(Na_valdis, Na_valdis_cen)
Na.ros <- as.data.frame(Na.ros)
id <- which(Na.ros$censored) 
Na.ros[id, ]

val0 <- unique(grep("<", data_base$Na_dis, value=TRUE))
id <- which(data_base$Na_dis==val0)
data_base$Na_com <- data_base$Na_dis

for (i in 1:length(id)){
  replace1 <- Na.ros$modeled[i]
  data_base[id, ]$Na_com[i] <- replace1
}

data_base[order(data_base$Na_dis),][ , c("Codigo","Na_com","Na_dis")]

####

# Potasio **K**
data_base$K_valdis <- as.numeric(str_remove(data_base$K_dis, pattern = "<"))
data_base$K_valdis_cen <- ifelse(str_detect(data_base$K_dis, pattern = "<"), TRUE, FALSE)
data_base$K_valdis_cen
data_base[ ,c("K_valdis", "K_valdis_cen")]

attach(data_base)
K.ros <- cenros(K_valdis, K_valdis_cen)
K.ros <- as.data.frame(K.ros)
id <- which(K.ros$censored) 
K.ros[id, ]

val0 <- unique(grep("<", data_base$K_dis, value=TRUE))
id <- which(data_base$K_dis==val0)
data_base$K_com <- data_base$K_dis

for (i in 1:length(id)){
  replace1 <- K.ros$modeled[i]
  data_base[id, ]$K_com[i] <- replace1
}

data_base[order(data_base$K_dis),][ , c("Codigo","K_com","K_dis")]

#### Aniones ##########################################################################################

# Cloruro
data_base$Cl_dis <- data_base$Cloruros
data_base$Cl_valdis <- as.numeric(str_remove(data_base$Cl_dis, pattern = "<"))
data_base$Cl_valdis_cen <- ifelse(str_detect(data_base$Cl_dis, pattern = "<"), TRUE, FALSE)
data_base$Cl_valdis_cen
data_base[ ,c("Cl_valdis", "Cl_valdis_cen")]

attach(data_base)
Cl.ros <- cenros(Cl_valdis, Cl_valdis_cen)
Cl.ros <- as.data.frame(Cl.ros)
id <- which(Cl.ros$censored) 
Cl.ros[id, ]

val0 <- unique(grep("<", data_base$Cl_dis, value=TRUE))
id <- which(data_base$Cl_dis==val0)
data_base$Cl_com <- data_base$Cl_dis

for (i in 1:length(id)){
  replace1 <- Cl.ros$modeled[i]
  data_base[id, ]$Cl_com[i] <- replace1
}

data_base[order(data_base$Cl_dis),][ , c("Codigo","Cl_com","Cl_dis")]

####

# Sulfatos
data_base$SO4_dis <- data_base$Sulfatos
data_base$SO4_valdis <- as.numeric(str_remove(data_base$SO4_dis, pattern = "<"))
data_base$SO4_valdis_cen <- ifelse(str_detect(data_base$SO4_dis, pattern = "<"), TRUE, FALSE)
data_base$SO4_valdis_cen
data_base[ ,c("SO4_valdis", "SO4_valdis_cen")]

attach(data_base)
SO4.ros <- cenros(SO4_valdis, SO4_valdis_cen)
SO4.ros <- as.data.frame(SO4.ros)
id <- which(SO4.ros$censored) 
SO4.ros[id, ]

val0 <- unique(grep("<", data_base$SO4_dis, value=TRUE))
id <- which(data_base$SO4_dis==val0)
data_base$SO4_com <- data_base$SO4_dis

for (i in 1:length(id)){
  replace1 <- SO4.ros$modeled[i]
  data_base[id, ]$SO4_com[i] <- replace1
}

data_base[order(data_base$SO4_dis),][ , c("Codigo","SO4_com","SO4_dis")]

####

# Carbonatos
data_base$CO3_dis <- data_base$Carb_
data_base$CO3_valdis <- as.numeric(str_remove(data_base$CO3_dis, pattern = "<"))
data_base$CO3_valdis_cen <- ifelse(str_detect(data_base$CO3_dis, pattern = "<"), TRUE, FALSE)
data_base$CO3_valdis_cen
data_base[ ,c("CO3_valdis", "CO3_valdis_cen")]

attach(data_base)
CO3.ros <- cenros(CO3_valdis, CO3_valdis_cen)
CO3.ros <- as.data.frame(CO3.ros)
id <- which(CO3.ros$censored) 
CO3.ros[id, ]

val0 <- unique(grep("<", data_base$CO3_dis, value=TRUE))
id <- which(data_base$CO3_dis==val0)
data_base$CO3_com <- data_base$CO3_dis

for (i in 1:length(id)){
  replace1 <- CO3.ros$modeled[i]
  data_base[id, ]$CO3_com[i] <- replace1
}

data_base[order(data_base$CO3_dis),][ , c("Codigo","CO3_com","CO3_dis")]

####

# Bicarbonatos
data_base$HCO3_dis <- data_base$Bicarb_
data_base$HCO3_valdis <- as.numeric(str_remove(data_base$HCO3_dis, pattern = "<"))
data_base$HCO3_valdis_cen <- ifelse(str_detect(data_base$HCO3_dis, pattern = "<"), TRUE, FALSE)
data_base$HCO3_valdis_cen
data_base[ ,c("HCO3_valdis", "HCO3_valdis_cen")]

attach(data_base)
HCO3.ros <- cenros(HCO3_valdis, HCO3_valdis_cen)
HCO3.ros <- as.data.frame(HCO3.ros)
id <- which(HCO3.ros$censored) 
HCO3.ros[id, ]

val0 <- unique(grep("<", data_base$HCO3_dis, value=TRUE))
id <- which(data_base$HCO3_dis==val0)
data_base$HCO3_com <- data_base$HCO3_dis

for (i in 1:length(id)){
  replace1 <- HCO3.ros$modeled[i]
  data_base[id, ]$HCO3_com[i] <- replace1
}

data_base[order(data_base$HCO3_dis),][ , c("Codigo","HCO3_com","HCO3_dis")]


colnames(data_base)

#### Balance ##########################################################################################

colnames(data_base)
#data_base <- data_base %>% select(contains("_com"), c("Codigo"),c("Temporada"))
data_base

data_base$Ca_com <- as.numeric(data_base$Ca_com)
data_base$Mg_com <- as.numeric(data_base$Mg_com)
data_base$Na_com <- as.numeric(data_base$Na_com)
data_base$K_com <- as.numeric(data_base$K_com)

data_base$Cl_com <- as.numeric(data_base$Cl_com)
data_base$SO4_com <- as.numeric(data_base$SO4_com)
data_base$CO3_com <- as.numeric(data_base$CO3_com)
data_base$HCO3_com <- as.numeric(data_base$HCO3_com)
str(data_base)

data_base$Ca_meql  <- (data_base$Ca_com*2)/40.08
data_base$Mg_meql  <- (data_base$Mg_com*2)/24.32
data_base$Na_meql  <- (data_base$Na_com*1)/23.00
data_base$K_meql   <- (data_base$K_com*1)/39.10

data_base$Cl_meql  <- (data_base$Cl_com*1)/35.56
data_base$SO4_meql <- (data_base$SO4_com*2)/96.06
data_base$CO3_meql<-(data_base$CO3_com*2)/60.008
data_base$HCO3_meql<-(data_base$HCO3_com*1)/61.0168

data_base$Tot_Cat <- rowSums(data_base[ ,c("Ca_meql","Mg_meql","Na_meql","K_meql")])
data_base$Tot_Ani <- rowSums(data_base[ ,c("Cl_meql","SO4_meql","CO3_meql", "HCO3_meql")])
data_base$BI_perc <- 100*(data_base$Tot_Cat-data_base$Tot_Ani)/(data_base$Tot_Cat+data_base$Tot_Ani)
data_base$BI_perc
data_base %>% select(contains("_meql"), c("BI_perc"), c("Codigo"), c("Temporada"))

#para ver ciertas columnas de un codigo en especifico
subset(data_base %>% select(contains("_meql"), c("BI_perc"), c("Codigo"), c("Temporada")), Codigo == "1394-22-SW-053A")


#SOLO PARA VER Y para ordenar y ver solo lo creado y el balance
data_base1 <- data_base %>% select(contains("_meql"), c("BI_perc"), c("Codigo"), c("Temporada"))
data_base1 <- data_base1[order(-data_base1$BI_perc),]
data_base1[1:11,]
data_base1 <- data_base1[order(data_base1$BI_perc),]
data_base1[1:11,]
subset(data_base1, Codigo == "1394-22-GW-013")     #para buscar un codigo especifico


ggplot(data_base, aes(x=BI_perc, y=BI_perc, shape="Error_ionico", label=Codigo))+
  geom_point()+
  geom_text(aes(label=ifelse(BI_perc>12, as.character(Codigo),"")),hjust="inward", vjust="inward")



data_base
colnames(data_base)

#### oUTLIERS/GRAFICOS ##########################################################################################
#### Avenida ####
Error_avenida <- data_base %>% filter(Temporada=="Avenida")
Error_avenida <- Error_avenida[order(Error_avenida$BI_perc),]
ErrorAvenidaMenor <- Error_avenida %>% filter(BI_perc < -10) %>% select(Codigo, BI_perc) %>%
  arrange(desc(BI_perc))
ErrorAvenidaMenor
write.table(x= ErrorAvenidaMenor, file = "ErrorAvenidaMenor.csv", fileEncoding = "utf-8")

ErrorAvenidaMayor <- Error_avenida %>% filter(BI_perc > 10) %>% select(Codigo, BI_perc) %>%
  arrange(desc(BI_perc))
ErrorAvenidaMayor
write.table(x= ErrorAvenidaMayor, file = "ErrorAvenidaMayor.csv", fileEncoding = "utf-8")

####avenida graficos####

par(mfrow=c(1,2))
qq <- ggplot(Error_avenida, aes(sample=BI_perc))+
  geom_qq_line(line.p = c(0.25, 0.75), col = "steelblue")+
  stat_qq()+
  xlab("Cuantiles Teoricos Normales") + ylab("Valores de Error Reales")+
  labs(caption = "Basado en Data del Laboratorio de INGEMMET")+
  scale_colour_manual()
##$$$$$$$$$$$$$$$$$$$$$$$$$$$$
dh <- ggplot(Error_avenida, aes(x=BI_perc)) +
  geom_histogram(aes(y = stat(density)), color="black",fill="steelblue") +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(0), sd = sd(Error_avenida$BI_perc)), 
    lwd = 2, 
    col = 'red')+
  geom_vline(xintercept = c(-10,10), linetype="longdash",color="blue",size=1.5)+
  annotate("text",x=6,y=0.12,label="Normal Distribution",color="red", fontface=2,
           size=6)+
  geom_segment(x = 6.4, y = 0.115, xend = 2.2, yend = 0.09,
               arrow = arrow(length = unit(0.5, "cm")))+
  annotate("text",x=-7.5,y=0.13,label="Limite Inferior",color="black", fontface=2,
           size=4)+
  annotate("text",x=+7.5,y=0.13,label="Limite Superior",color="black", fontface=2,
           size=4)
##$$$$$$$$$$$$$$$$$$$$$$$$$$$$
figure_av <- ggarrange(dh, qq,
                       labels = c("Distribucion de Errores", "QQplot-Errores"),
                       ncol = 2, nrow = 1)
figure_av
plotly::ggplotly(qq)


#### Estiaje ####
Error_estiaje <- data_base %>% filter(Temporada=="Estiaje")
Error_estiaje <- Error_estiaje[order(Error_estiaje$BI_perc),]
ErrorEstiajeMenor <- Error_estiaje %>% filter(BI_perc < -10) %>% select(Codigo, BI_perc) %>%
  arrange(desc(BI_perc))
ErrorEstiajeMenor
write.table(x= ErrorEstiajeMenor, file = "ErrorEstiajeMenor.csv", fileEncoding = "utf-8")

ErrorEstiajeMayor <- Error_estiaje %>% filter(BI_perc > 10) %>% select(Codigo, BI_perc) %>%
  arrange(desc(BI_perc))
ErrorEstiajeMayor
write.table(x= ErrorEstiajeMayor, file = "ErrorEstiajeMayor.csv", fileEncoding = "utf-8")

####estiaje graficos####

par(mfrow=c(1,2))
qq <- ggplot(Error_estiaje, aes(sample=BI_perc))+
  geom_qq_line(line.p = c(0.25, 0.75), col = "steelblue")+
  stat_qq()+
  xlab("Cuantiles Teoricos Normales") + ylab("Valores de Error Reales")+
  labs(caption = "Basado en Data del Laboratorio de INGEMMET")+
  scale_colour_manual()
##$$$$$$$$$$$$$$$$$$$$$$$$$$$$
dh <- ggplot(Error_estiaje, aes(x=BI_perc)) +
  geom_histogram(aes(y = stat(density)), color="black",fill="steelblue") +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(0), sd = sd(Error_estiaje$BI_perc)), 
    lwd = 2, 
    col = 'red')+
  geom_vline(xintercept = c(-10,10), linetype="longdash",color="blue",size=1.5)+
  annotate("text",x=6,y=0.12,label="Normal Distribution",color="red", fontface=2,
           size=6)+
  geom_segment(x = 6.4, y = 0.115, xend = 2.2, yend = 0.09,
               arrow = arrow(length = unit(0.5, "cm")))+
  annotate("text",x=-7.5,y=0.13,label="Limite Inferior",color="black", fontface=2,
           size=4)+
  annotate("text",x=+7.5,y=0.13,label="Limite Superior",color="black", fontface=2,
           size=4)
##$$$$$$$$$$$$$$$$$$$$$$$$$$$$
figure_es <- ggarrange(dh, qq,
                       labels = c("Distribucion de Errores", "QQplot-Errores"),
                       ncol = 2, nrow = 1)
figure_es
plotly::ggplotly(qq)



##############################################################################################

data_base
colnames(data_base)

colnames(data_base %>% select(contains("_com")))
data_base




#03 BD NORMAL SIN "<"
#04 BD NORMAL SIN "<", sin columnas de calculo de Balance iónico
#05 corrigiendo TDS
#06 BD CON "<" DE PRUEBA




