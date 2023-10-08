#### Detectar valores debajo del L.D - ECA - Completar ####

#Verificar si superan ECA tot:
#seleccionar los valores que tienen ECA segun DL:

# seleccionar subcuenca:
data_base_grande <- data_base %>% filter(Subcuenca=="Grande")

# data_base_grande <- data_base %>% filter(Subcuenca=="Antauta")
# data_base_grande <- data_base %>% filter(Subcuenca=="San Gabán")

# seleccionar disueltos: 
disueltos <- ECA %>% select(Elemento)
disueltos <- as.data.frame(t(disueltos))
names(disueltos) <- (ECA %>% select(Elemento))$Elemento
a <- sub(pattern = "_tot", replacement = "_dis", x = colnames(disueltos))

data_eca <- data_base_grande %>% select("Temporada", any_of(a))

data_eca2 <- data_eca |>
  pivot_longer(!1, values_transform=list(value=as.character)) |>
  group_by(across(1:2)) |>
  summarize(
    non_detected = sum(str_detect(value, "^<")),
    completed = nrow(data_base) - non_detected,
    .groups="drop")

data_ros <- data_eca2 %>% 
  filter(completed >= nrow(data_eca)/2)%>%
  arrange(desc(completed))

#ros
data_ros_com <- data_ros %>% filter(non_detected==0)

data_ros_com  <- data_ros_com %>% 
  mutate(detectados_per = (completed/(non_detected+completed))*100)


data_ros_run <- data_ros %>% filter(non_detected!=0)

data_ros_run  <- data_ros_run %>% 
  mutate(detectados_per = (completed/(non_detected+completed))*100)


## GENERANDO TABLA COMO SE DESEA:
# Porcentaje de valor de medición

PVM <- rbind(data_ros_com, data_ros_run)

PVM <- PVM %>% select(Temporada, name, detectados_per) %>%
  pivot_wider(names_from = "Temporada", 
              values_from = "detectados_per")

write.csv(PVM, file = "PVM.csv")

# Analizamos los que estan completos con ECAS totales:
data_base2 <- data_base
cols <- data_ros_com$name
data_base2[cols] <- lapply(data_base2[cols], as.numeric) 

data_ros_com$name

id <- which(data_base2$Mg_tot >= ECA[ECA$Elemento == 'Mg_tot', ]$ECAD1D2)
length(data_base2[id, ]$Mg_tot)

data_base2[id, ]$Codigo

#noros
data_noros <- data_eca2 %>% filter(completed < nrow(data_eca)/2)
data_noros$name

id <- which(str_detect(data_base2$Li_tot, pattern = "<")==FALSE)
id <- which(as.numeric(data_base2[id, ]$Li_tot)>= ECA[ECA$Elemento == 'Li_tot', ]$ECAD1D2)
length(id)

# Aplicando Ros:
# Proceso semiautomatico
data_base2 <- data_base_grande
columnas <- data_ros_run$name
library(NADA)
df <- data_base2 %>%
  group_nest(Temporada) %>% 
  mutate(data = map(data, 
                    ~ .x %>%
                      mutate(across(.cols = columnas, # Selecting columns with few imputations for examplification
                                    .fns = ~ ros(as.numeric(str_remove(.x, "<")),
                                                 str_detect(.x, "<"),
                                                 forwardT = "log",
                                                 reverseT = "exp")%>%
                                      as.data.frame() %>%
                                      arrange(sort(as.numeric(str_remove(.x, "<")), index.return = TRUE)$ix) %>% 
                                      pull(modeled))))) %>%
  unnest(cols = c(data)) #%>% 
#select(Temporada, c(5,71:74)) 

#Revisando ECAS después de ROS:

data_ros_run$name

id <- which(df$Ni_tot >= ECA[ECA$Elemento == 'Ni_tot', ]$ECAA1)
length(df[id, ]$Ni_tot)

#Then Classsified by Ranges all:

identificados <- data_ros_run$name
vdf <- Tumbes2 %>%
  mutate(across(.cols = c(identificados), 
                .fns = ~ cut(.x, breaks = quantile(.x, probs = seq(0, 1, 0.25)), 
                             labels=c("Bajo","Moderado","Alto","Muy Alto"))))%>%   
  rename_with(~gsub("tot", "rango", .x, fixed = TRUE), .cols = c(identificados))

vdf <- cbind(Tumbes2, vdf%>%select(contains("rango")))

write.csv(x = vdf, file = "vdf.csv")

# Agruparlo y Procesarlo (falta terminar)
vdf <- Tumbes2 %>%
  group_nest(Temporada) %>% 
  mutate(data = map(data, 
                    ~ .x %>%
                      mutate(across(.cols = c(identificados), # Selecting columns with few imputations for examplification
                                    .fns = ~ cut(.x, breaks = quantile(.x, probs = seq(0, 1, 0.25)), 
                                                 labels=c("Bajo","Moderado","Alto","Muy Alto"))%>%
                                      as.data.frame())))) %>%
  unnest(cols = c(data)) %>%   
  rename_with(~gsub("tot", "rango", .x, fixed = TRUE), .cols = c(identificados))

#verificar que valores tienen LD otro casos:

elementos <- colnames(data_base[ ,70:180]) #solo elementos quimicos
data_total <- data_base %>% select("Cuenca","Temporada", any_of(elementos))

data_total |>
  pivot_longer(!1:2, values_transform=list(value=as.character)) |>
  group_by(across(1:3)) |>
  summarize(
    non_detected=sum(str_detect(value, "^<")),
    completed=nrow(data_total) - non_detected,
    .groups="drop")

b <- a %>% filter(grepl('_dis|_tot', name))


write.csv(below_dl, file="belowld.csv")


#histogramas

histog <- df %>% select(any_of(data_ros_com$name), any_of(data_ros_run$name))
histog <- as.data.frame(histog)

par(mfrow=c(2,2))
for (i in 9:10){
  hist(histog[ ,i], main = names(histog)[i])
  par(new = TRUE)
  plot(density(histog[ ,i]), main= "", axes = FALSE, col="red")
  par(new = TRUE)
  boxplot(histog[ ,i], horizontal = TRUE, axes = FALSE, col="lightgreen")
}

#sumario

es <- sapply(histog[ ,1:ncol(histog)], estadisticos) # 2:9 O -1 funciona igual.
Nombres <- c("n","Vacios","Minimo","LI","Q1","Mediana","Media","Media Cortada","Q3","Maximo","LS",
             "IQR","MAD", "Sd", "As","k", "CV")
rownames(es) <- Nombres
f <- data.frame(es)
f
write.csv(x = f, file = "elementos1.csv")

## Data supera ECA:

name_sup_eca <- c("Al_tot","B_tot","Ba_tot","Mn_tot","U_tot","Fe_tot","Cd_tot","Pb_tot")

data_sup_eca <- df %>% select(any_of(name_sup_eca))


#### Revisar outliers

check_outlier <- function(v, coef=1.5){
  quantiles <- quantile(v,probs=c(0.25,0.75))
  IQR <- quantiles[2]-quantiles[1]
  res <- v < (quantiles[1]-coef*IQR)|v > (quantiles[2]+coef*IQR)
  return(res)
}

check_outlier(df$Al_tot)

#### Al_tot (Al_tot)

Al_tot <- ggplot(df, aes(x=factor("Aluminio"), y=Al_tot))+
  geom_boxplot(outlier.colour="red", #outlier.shape = 2,outlier.size = 3,
               notch=FALSE, width=0.4, fill=rgb(0,128,128,max=255))+
  scale_x_discrete(limits=c("Aluminio"))+
  
  stat_summary(fun=mean, geom="point", shape=23, fill= "black",size=4)+
  geom_jitter(data = filter(df, Al_tot>0.9), 
              aes(color=Cuenca, shape=Cuenca),position=position_jitter(seed = 1),size=3)+
  geom_text_repel(data = filter(df, Al_tot>0.9), aes(label=Codigo),vjust = 0,nudge_y = 0.05)+
  #geom_label_repel(aes(label = Cod_Corto), position=position_jitter(seed = 1))+
  geom_hline(yintercept = 0.9)+
  #scale_shape_manual(values = c(17,8,16,18))+
  scale_color_manual(values =rainbow(67))+
  labs(title="",x="",y= "Al (mg/l)")

theme(legend.position =  "none")

Al_tot

#### En caso querer observarlo como arreglo

grid.arrange(Mg_01,Mg_01,Mg_01,Mg_01,Mg_01,Mg_01,nrow=2)
ggplotly()

# Plot separate ggplot figures in a loop.
name_sup_eca <- c("Al_tot","B_tot","Ba_tot","Mn_tot","U_tot","Fe_tot","Cd_tot","Pb_tot")
ECA_box <- ECA[c(1,5,4,10,16,9,6,14), c("ECAA1")]

# Make plots.
plot_list = list()
for (i in 1:length(name_sup_eca)){
  p = ggplot(df, aes_string(x=factor(name_sup_eca[i]), y=name_sup_eca[i])) +
    geom_boxplot(outlier.colour="red", #outlier.shape = 2,outlier.size = 3,
                 notch=FALSE, width=0.4, fill=rgb(0,128,128,max=255))+
    scale_x_discrete(limits=c(name_sup_eca[i]))+
    stat_summary(fun=mean, geom="point", shape=23, fill= "black",size=4)+
    # geom_jitter(
    #   data =     filter(df, 
    #                     .data[[name_sup_eca[[i]]]] >= ECA_box[[i]]
    #   ),
    #   aes(color=Cuenca, shape=Cuenca), size=2)+
    geom_label_repel(data = filter(df,
                                   .data[[name_sup_eca[[i]]]] >= ECA_box[[i]]),
                     aes(label=Codigo), size=3)+
    geom_hline(yintercept = ECA_box[i])
  plot_list[[i]] = p
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in 1:length(name_sup_eca)) {
  file_name = paste("elemento_plot_", i, ".tiff", sep="")
  tiff(file_name)
  print(plot_list[[i]])
  dev.off()
}

# Another option: create pdf where each page is a separate plot.
pdf("plots.pdf")
for (i in 1:length(name_sup_eca)) {
  print(plot_list[[i]])
}
dev.off()


#### ECAS ####

data_ECAS <- data_base
data_ECAS$Mn_tot <- as.numeric(data_ECAS$Mn_tot)
data_ECAS <- data_ECAS %>% filter(!is.na(Mn_tot)) 

range(data_ECAS$Mn_tot, na.rm = TRUE)

ggplot(data_ECAS, aes(x=Codigo, y=Mn_tot))+
  geom_point(aes(colour = factor(Cuenca), shape= factor(Tip_fuente), size=6))+
  geom_hline(aes(yintercept = 0.25,linetype="ECA Cat.1 A-2"),colour="#ff0000" ,size=1)+
  geom_hline(aes(yintercept = 0.25,linetype="ECA Cat.3 D-1"),colour="#00ff00",size=1)+
  geom_hline(aes(yintercept = 0.50,linetype="ECA Cat.3 D-2"),colour="#ff00ff",size=1)+
  scale_linetype_manual(name="Ecas", values =c(1,1,1),
                        guide=guide_legend(override.aes = list(color=c(rgb(255,0,0,max=255),
                                                                       "#00ff00",
                                                                       "#ff00ff"))))+
  theme(axis.text.x = element_text(face = "italic", color = "black",
                                   angle = 90, vjust = 0.5, hjust=1, size=8),
        axis.title.y = element_text(face = "italic",color = "black",
                                    size = 15, angle = 90))+
  scale_y_continuous(name="Mg (mg/l)", limits=c(0, 3.5), breaks = seq(0,3.5, by=0.5))+
  scale_shape_manual(values = c(17,8,16,18,18))+
  scale_color_manual(values =rainbow(5))+
  theme(legend.position = "none")
# scale_x_discrete(name ="",limits=c("SW-16","SW-08","SW-09","SW-10","SW-05","SW-06",
#                                   "SW-04","SW-03","SW-14","SW-15","SW-02","SW-01",
#                                   "SW-18","SW-19"))+

# Graphic identifier:

plot(data_ECAS$Mg_tot, data_ECAS$Mn_tot,
     main="Gráfico de Puntos desde la zona Alta hasta la desembocadura del Río Tumbes",
     xlab = "Mg_tot", ylab = "Mn_tot (mg/l)", col="black", pch=18,  bg="blue", cex=1)

identify(data_ECAS$Mg_tot, data_ECAS$Mn_tot, 
         labels=row.names(data_ECAS)) 
coords <- locator(type="l")
identify(data_ECAS$Mg_tot, data_ECAS$Mn_tot,
         labels = c(data_ECAS$Mg_tot, data_ECAS$Mn_tot))

#### Comparar ECAS (antiguo metodo) ####

# Proceso Largo semiautomatico : 

# Count how many values are below the detection limit:
library(tidyverse); library(NADA); library(DT)

#Filter to detect some B.D
datatable(data_base[ ,c("Zn_dis", "Zn_tot")], filter = "top") 

#Elements that I need to complete
elementos <- colnames(data_base[ ,70:180])
#Variables that I need to complete
data_total <- data_base %>% select("Cuenca","Temporada", any_of(elementos))

# First Block
# Filter accord to Cuenca, Subcuenca or Microcuenca (or spatial coherence)
Tumbes <- data_base %>% filter(Cuenca=="Tumbes")
str(Tumbes)

# Second Block
#After one run, start here with Avenida:
z <- Tumbes[Tumbes$Temporada=="Avenida", ] #Do the same for Estiaje!
#Preparing Data for Analysis:
val0 <- unique(grep("<", z$Zn_dis, value = TRUE))
z$var0 <- z$Zn_dis
z$ND_var0 <- rep(0, length(z$var0))
indcero0 <- which(z$var0==val0)
z$var0[indcero0] <- substr(val0,2,nchar(val0))
z$var0 <- as.numeric(z$var0)
z$ND_var0[indcero0] <- 1
z$ND_var0 <- as.logical(z$ND_var0)
#Sort the Data Acorder to Study:
indna0 <- is.na(z$var0)
yn0 <- z$var0[which(indna0==FALSE)]
cyn0 <- z$ND_var0[which(indna0==FALSE)]
yn0 <- sort(yn0,index.return=TRUE)
cyn0 <- cyn0[yn0$ix]
#Apply the ROS (REGRESSION IN ORDER STATISTICS)
elemento <- ros(yn0$x,cyn0,forwardT = "log", reverseT = "exp")
elemento <- as.data.frame(elemento)
id <- which(elemento$censored==TRUE)
elemento <-  elemento[id, ]

# Third Block

#DO IT WITH CARE!!!! (only the first time)
#Create new column of completed values.
Tumbes$Zn_com <- Tumbes$Zn_dis #omit this step after first time run

#Loop to input values:
id <- which(Tumbes$Zn_com==val0)
for(i in 1:length(id)){
  replace <- elemento$modeled[i]
  Tumbes[id, ]$Zn_com[i] <-  replace
}
Tumbes$Zn_com <- as.numeric(Tumbes$Zn_com)

# Testing if works
#After run in Avenida and Estiaje:
knitr::kable(Tumbes[ ,c("Zn_dis","Zn_com")])  
write.csv(x = Tumbes, file = "Tumbes.csv") 




