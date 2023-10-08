
attach(data_base)
data_base
colnames(data_base)
#_com (corregidos por ROSS),      _tot (analisis por totales)
data_base[order(data_base$Ca_tot),][ , c("Codigo","Ca_tot","Ca_com")]



#############################################################################################################
#### Elementos Mayoritarios ####
mayoritarios <- c("Ca_tot", "Mg_tot", "Na_tot", "K_tot", 
                  "Sulfatos", "Bicarb_", "Carb_", "Cloruros")

mayor <- data_base %>% select(any_of(mayoritarios))
mayor$Ca_tot <- as.numeric(mayor$Ca_tot)
mayor$Mg_tot <- as.numeric(mayor$Mg_tot)
mayor$Na_tot <- as.numeric(mayor$Na_tot)
mayor$K_tot <- as.numeric(mayor$K_tot)
mayor

# Hidrotipos #
hidrotipos <- data_base %>% select(contains("_meql"), c("Temporada"), c("Codigo"))
colnames(hidrotipos)
hidrotipos

#to_remove <- c("Total_meq_l_c", "Total_meq_l_a")
#hidrotipos <- hidrotipos[ , !(names(hidrotipos) %in% to_remove)]
#hidrotipos

#rename(Calcica = Ca_meql,   Magnesica = Mg_meql,     Sodica = Na_meql,  Potasica = K_meql,
#Bicarbonatada = HCO3_meql,  Carbonatada = CO3_meql, 
#Sulfatada = SO4_meql,       Clorurada = Cl_meql)


colnames(hidrotipos) = c("Calcica", "Magnesica", "Sodica", "Potasica", 
                         "Clorurada", "Sulfatada", "Carbonatada", "Bicarbonatada",   
                         "Temporada", "Codigo")
hidrotipos


a <- colnames(hidrotipos[ ,c("Bicarbonatada","Carbonatada","Sulfatada","Clorurada")])[apply(hidrotipos[ ,
                                  c("Bicarbonatada","Carbonatada","Sulfatada","Clorurada")],1,which.max)]

c <- colnames(hidrotipos[ ,c("Calcica","Magnesica","Sodica","Potasica")])[apply(hidrotipos[ ,
                                  c("Calcica","Magnesica","Sodica","Potasica")],1,which.max)]



df <- data.frame(a, c)
data_base$hidrotype <- paste0(df$a,"-", df$c) #Agregando la columna de hidrotipos a la data



#############################################################################################################
#Pye de Hidrotipos:
library(lessR)
cols <-  hcl.colors(length(levels(as.factor(data_base$hidrotype))), "BluYl")
PieChart(hidrotype, data = data_base, hole = 0,
         fill = cols,
         labels_cex = 0.8,
         radius = 0.5,
         values_size = 1,
         values_color = "black", lwd=3,
         main = "HIDROTIPOS")



#############################################################################################################
# Diagrama de Gibss:
library(scales)
data_gibss <- data_base
data_gibss$Ca_meql <- as.numeric(data_gibss$Ca_meql)
data_gibss$Na_meql <- as.numeric(data_gibss$Na_meql)
data_gibss$K_meql <- as.numeric(data_gibss$K_meql)

# data_gibss$Na_meql <- data_gibss$Na_tot*0.0435
# data_gibss$K_meql  <- data_gibss$K_tot*0.0256
# data_gibss$Cl_meql <- data_gibss$Cloruros*0.0282
# data_gibss$SO4_meql<- data_gibss$Sulfatos*0.0208

data_gibss <- data_gibss %>%
  mutate(Na_K_Ca = (Na_meql+K_meql)/(Na_meql+K_meql+Ca_meql)) %>%
  mutate(Cl_HCO3 = (Cl_meql)/(Cl_meql+HCO3_meql))

a <- ggplot(data = data_gibss)+
  geom_point(aes(x = Na_K_Ca, y = TDS_mg_L, shape = Cuenca, color = Cla_fuente))+
  scale_x_continuous(name = "Na+K/Na+K+Ca", breaks = seq(0,1, by=0.2))+
  scale_y_continuous(name = "Total Dissolved Salts (ppm)",trans=log_trans(), breaks = c(1,10,100,1000,10000,100000))+
  expand_limits(y = c(0, 100000))+
  annotate("text", x = 0.2, y = 1000, label = "Rock Dominance")+
  annotate("text", x = 0.8, y = 10000, label = "Sea Water")+
  annotate("text", x = 0.8, y = 20, label = "Precipitation Dominance")+
  annotate("segment", x = 0, xend = 1, y = 110, yend = 3,  colour = "blue")+
  annotate("segment", x = 0, xend = 0.9, y = 600, yend = 100000,  colour = "blue")+
  annotate("segment", x = 1, xend = 0.5, y = 8000, yend = 300,  colour = "blue")+
  annotate("segment", x = 0.5, xend = 1, y = 300, yend = 30,  colour = "blue")+
  annotation_logticks(sides = "lr")  +
  theme_bw() 
a

b <- ggplot(data = data_gibss)+
  geom_point(aes(x = Cl_HCO3, y = TDS_mg_L, shape = Cuenca, color = Cla_fuente))+
  scale_x_continuous(name = "Cl/Cl+HCO3", breaks = seq(0,1, by=0.2))+
  scale_y_continuous(name = "Total Dissolved Salts (ppm)",trans=log_trans(), breaks = c(1,10,100,1000,10000,100000))+
  expand_limits(y = c(0, 100000))+
  annotate("text", x = 0.2, y = 1000, label = "Rock Dominance")+
  annotate("text", x = 0.8, y = 10000, label = "Sea Water")+
  annotate("text", x = 0.8, y = 20, label = "Precipitation Dominance")+
  annotate("segment", x = 0, xend = 1, y = 110, yend = 3,  colour = "blue")+
  annotate("segment", x = 0, xend = 0.9, y = 600, yend = 100000,  colour = "blue")+
  annotate("segment", x = 1, xend = 0.5, y = 8000, yend = 300,  colour = "blue")+
  annotate("segment", x = 0.5, xend = 1, y = 300, yend = 30,  colour = "blue")+
  annotation_logticks(sides = "lr")  +
  theme_bw() 
b
ggplotly(b)



#############################################################################################################
# Flujos: 

data_scatter <- data_gibss %>%
  mutate(Cl_SO4 = Cl_meql+SO4_meql) %>%
  mutate(Na_K = Na_meql+K_meql)

c <- ggplot(data = data_scatter)+
  geom_point(aes(x = Cl_SO4, y = Na_K , shape = Tip_fuente, color = Cla_fuente))+
  scale_x_continuous(name = "Cl+SO4 (meq/l)",trans=log_trans(), 
                     breaks = c(0.1,1,10,100, 1000))+
  scale_y_continuous(name = "Na+K (meq/l)",trans=log_trans(), 
                     breaks = c(0.1,1,10,100, 1000))+
  expand_limits(y = c(0, 100), x=c(0,100))+
  annotation_logticks(sides = "lr")+
  annotate("text", x = 0.1, y = 1, label = "Flujo Local")+
  annotate("text", x = 5, y = 5, label = "Flujo Intermedio")+
  annotate("text", x = 30, y = 30, label = "Flujo Regional")+
  annotate("segment", x = 1, xend = 100, y = 100, yend = 1,  colour = "blue")+
  annotate("segment", x = 0.1, xend = 100, y = 100, yend = 0.1,  colour = "blue")+
  theme_bw() 

ggplotly(c)



#############################################################################################################
## Diagrama de Piper ###

data_piper <- data_base %>% select("Codigo","Nom_Comp","Cota","Tip_fuente",
                                   "Ca_dis","Mg_dis","Na_dis","K_dis",
                                   "Cloruros","Sulfatos","Carb_","Bicarb_",
                                   "Uso_fuente","T_fuente","Ph","CE_uS_cm",
                                   "TDS_mg_L", "Salin_PSU","OD_mgL",
                                   "Cuenca","Este", "Norte")
data_piper
colnames(data_piper)


summary(data_piper)
colnames(data_piper) = c("Codigo", "Nom_Comp", "Cota", "Tip_fuente", 
                         "Ca", "Mg", "Na", "K",   
                         "Cl","SO4","CO3","HCO3",
                         "Uso_fuente", "T_fuente", 
                         "Ph", "CE_uS_cm", "TDS_mg_L","Salin_PSU", "OD_mgL",
                         "Cuenca", "Este", "Norte")

data_piper

# data_piper$K_com <- as.numeric(replace(data_piper$K_com, data_piper$K_com=="<0.2", 0.1))
# data_piper$SO4 <- as.numeric(replace(data_piper$SO4, data_piper$SO4=="<0.1", 0.05))
# data_piper$Cl <- as.numeric(replace(data_piper$Cl, data_piper$Cl=="<0.2", 0.1))
# data_piper$CO3 <- as.numeric(replace(data_piper$CO3, data_piper$CO3=="<1", 0.5))
data_piper$colors <- factor(data_piper$Cuenca, levels = unique(data_piper$Cuenca))
cols <- c(rgb(255,255,0,maxColorValue = 255),
          rgb(50,74,178,maxColorValue = 255),
          rgb(0,176,242,maxColorValue = 255),
          rgb(0,176,242,maxColorValue = 255),
          rgb(0,176,242,maxColorValue = 255))

toPercent <- function (data_input) {
  totalCations <- rowSums(data_input[ ,c("Ca","Mg","Na","K")], na.rm=TRUE)
  data_input$Ca <- 100 * (data_input$Ca/totalCations)
  data_input$Mg <- 100 * (data_input$Mg/totalCations)
  data_input$Na <- 100 * (data_input$Na/totalCations)
  data_input$K <- 100 * (data_input$K/totalCations)
  totalAnions <- rowSums(data_input[ ,c("Cl","SO4","CO3","HCO3")], na.rm=TRUE)
  data_input$Cl <- 100 * (data_input$Cl/totalAnions)
  data_input$SO4 <- 100 * (data_input$SO4/totalAnions)
  data_input$CO3 <- 100 * (data_input$CO3/totalAnions)
  data_input$HCO3 <- 100 * (data_input$HCO3/totalAnions)
  return(data_input)
}
transform_piper_data <- function(toPercent){
  Codigo<-toPercent$Codigo
  y1 <- toPercent$Mg * 0.86603
  x1 <- 100*(1-(toPercent$Ca/100) - (toPercent$Mg/200))
  y2 <- toPercent$SO4 * 0.86603
  x2 <-120+(100*toPercent$Cl/100 + 0.5 * 100*toPercent$SO4/100)
  new_point <- function(x1, x2, y1, y2, grad=1.73206){
    b1 <- y1-(grad*x1)
    b2 <- y2-(-grad*x2)
    M <- matrix(c(grad, -grad, -1,-1), ncol=2)
    intercepts <- as.matrix(c(b1,b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(x=t_mat[1,1], y=t_mat[2,1])
  }
  np_list <- lapply(1:length(x1), function(i) new_point(x1[i], x2[i], y1[i], y2[i]))
  npoints <- do.call("rbind",np_list)
  data.frame(observation=Codigo,x=c(x1, x2, npoints$x), y=c(y=y1, y2, npoints$y))
}
ggplot_piper <- function(piper.data,output = c("ggplot","plotly")) {
  grid1p1 <<- data.frame(x1 = c(20,40,60,80), x2= c(10,20,30,40),y1 = c(0,0,0,0), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid1p2 <<- data.frame(x1 = c(20,40,60,80), x2= c(60,70,80,90),y1 = c(0,0,0,0), y2 = c(69.2824, 51.9618,34.6412,17.3206))
  grid1p3 <<- data.frame(x1 = c(10,20,30,40), x2= c(90,80,70,60),y1 = c(17.3206,34.6412,51.9618, 69.2824), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid2p1 <<- grid1p1
  grid2p1$x1 <- grid2p1$x1+120
  grid2p1$x2 <- grid2p1$x2+120
  grid2p2 <<- grid1p2
  grid2p2$x1 <- grid2p2$x1+120
  grid2p2$x2 <- grid2p2$x2+120
  grid2p3 <<- grid1p3
  grid2p3$x1 <- grid2p3$x1+120
  grid2p3$x2 <- grid2p3$x2+120
  grid3p1 <<- data.frame(x1=c(100,90, 80, 70),y1=c(34.6412, 51.9618, 69.2824, 86.603), x2=c(150, 140, 130, 120), y2=c(121.2442,138.5648,155.8854,173.2060))
  grid3p2 <<- data.frame(x1=c(70, 80, 90, 100),y1=c(121.2442,138.5648,155.8854,173.2060), x2=c(120, 130, 140, 150), y2=c(34.6412, 51.9618, 69.2824, 86.603))
  
  p <- ggplot2::ggplot() +
    ## left hand ternary plot
    ggplot2::geom_segment(ggplot2::aes(x=0,y=0, xend=100, yend=0)) +
    ggplot2::geom_segment(ggplot2::aes(x=0,y=0, xend=50, yend=86.603)) +
    ggplot2::geom_segment(ggplot2::aes(x=50,y=86.603, xend=100, yend=0)) +
    ## right hand ternary plot
    ggplot2::geom_segment(ggplot2::aes(x=120,y=0, xend=220, yend=0)) +
    ggplot2::geom_segment(ggplot2::aes(x=120,y=0, xend=170, yend=86.603)) +
    ggplot2::geom_segment(ggplot2::aes(x=170,y=86.603, xend=220, yend=0)) +
    ## Upper diamond
    ggplot2::geom_segment(ggplot2::aes(x=110,y=190.5266, xend=60, yend=103.9236)) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=190.5266, xend=160, yend=103.9236)) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=17.3206, xend=160, yend=103.9236)) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=17.3206, xend=60, yend=103.9236)) +
    ## Add grid lines to the plots
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p1, linetype = "dashed", size = 0.25, colour = "grey80") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p2, linetype = "dashed", size = 0.25, colour = "grey70") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p3, linetype = "dashed", size = 0.25, colour = "grey60") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p2, linetype = "dashed", size = 0.25, colour = "grey40") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p3, linetype = "dashed", size = 0.25, colour = "grey30") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p1, linetype = "dashed", size = 0.25, colour = "grey20") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p2, linetype = "dashed", size = 0.25, colour = "grey10") +
    ### Labels and grid values
    
    ggplot2::geom_text(ggplot2::aes(c(20,40,60,80),c(-5,-5,-5,-5), label=c(80, 60, 40, 20)), size=3) +
    ggplot2::geom_text(ggplot2::aes(c(35,25,15,5),grid1p2$y2, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::coord_equal(ratio=1) +  
    ggplot2::geom_text(ggplot2::aes(c(215,205,195,185),grid2p3$y2, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(c(140,160,180,200),c(-5,-5,-5,-5), label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p1$x1-5,grid3p1$y1, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p1$x2+5,grid3p1$y2, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p2$x1-5,grid3p2$y1, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p2$x2+5,grid3p2$y2, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
                   legend.title = element_blank()) +
    ggplot2::geom_point(ggplot2::aes(x,y, colour=factor(Cuenca),
                                     shape = Tip_fuente, 
                                     text = paste(observation,
                                                  '</br></br>Cuenca: ',Cuenca,
                                                  '</br></br>CE: ',CE_uS_cm,
                                                  '</br></br>Uso:',Tip_fuente,
                                                  '</br></br>Ca: ',Ca,
                                                  '</br></br>K: ',K,
                                                  '</br></br>Na: ',Na,
                                                  '</br></br>Cl: ',Cl,
                                                  '</br></br>SO4 : ',SO4,
                                                  '</br></br>HCO3: ',HCO3,
                                                  '</br></br>CO3: ',CO3
                                     )), data=piper.data)+
    ggplot2::scale_color_manual(values=c(rgb(0,176,242,maxColorValue = 255),
                                         rgb(50,74,178,maxColorValue = 255),
                                         rgb(255,255,0,maxColorValue = 255),
                                         rgb(255,255,0,maxColorValue = 255),
                                         rgb(255,255,0,maxColorValue = 255)))
  
  if (output == "ggplot"){
    p <- p + 
      ggplot2::geom_text(ggplot2::aes(17,50, label="Mg^'2+'"), angle=60, size=4, parse=TRUE) +  
      ggplot2::geom_text(ggplot2::aes(77.5,50, label="Na^'+'~+~K^'+'"), angle=-60, size=4,parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(50,-10, label="Ca^'2+'"), size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(170,-10, label="Cl^'-'"), size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(205,50, label="SO[4]^'-'"), angle=-60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(142,50, label="Alkalinity~as~{HCO[3]^'-'"), angle=60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(72.5,150, label="SO[4]^'-'~+~Cl^'-'"), angle=60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(147.5,150, label="Ca^'2+'~+~Mg^'2+'"), angle=-60, size=4, parse=TRUE)
  }
  
  
  
  
  if (output == "plotly"){
    #this fixes an issue that plotly can't render geom_text() with the  angle option set properly
    p <- plotly::ggplotly(p,
                          tooltip = c("text")
    )
    p <- p  %>% plotly::layout(
      annotations=list(text=c("Mg<sup>2+</sup>",
                              "Na<sup>+</sup> + K<sup>+</sup>",
                              "Ca<sup>2+</sup>",
                              "Cl<sup>-</sup>",
                              "SO<sub>4</sub><sup>-</sup>",
                              "Alkalinity as HCO<sub>3</sub><sup>-</sup>",
                              "SO<sub>4</sub><sup>-2</sup> + Cl<sup>-</sup>",
                              "Ca<sup>2+</sup> + Mg<sup>2+</sup>"),
                       x = c(17,77.5,50,170,205,142.5,72.5,147.5),
                       y = c(50,50,-10,-10,50,50,150,150),
                       textangle = c(-60,60,0,0,60,-60,-60,60),
                       "showarrow"=F, font=list(size = 12, color = "black")
      ))
    }
  
  return(p)
}


# Generando Piper:

dt <- toPercent(data_piper)
dt1 <- transform_piper_data(dt)

piper_data <- merge(dt1,  dt[,c("Codigo", "Nom_Comp", "Cota", "Tip_fuente", 
                                "Ca", "Mg", "Na", "K",   
                                "Cl","SO4","CO3","HCO3",
                                "Uso_fuente", "T_fuente", 
                                "Ph", "CE_uS_cm", "TDS_mg_L", "OD_mgL",
                                "Cuenca", "Este", "Norte")],
                    by.x = "observation",
                    by.y = "Codigo")


## Piper ggplot:

ggplot_piper(piper.data = piper_data, output = "plotly")
ggplot_piper(piper.data = piper_data, output = "plot")


#############################################################################################################
## Schöller ## (Tambien puede ser con trazas o minoritarios)

scholler_barkaloff <- data_base %>% filter(Cuenca == "Intercuenca 13951") %>%
  select(c("HCO3_meql", "SO4_meql","Cl_meql","Ca_meql","Mg_meql","Na_meql","K_meql","Codigo"))

sc <- scholler_barkaloff %>%
  pivot_longer(c("HCO3_meql","SO4_meql","Cl_meql","Ca_meql","Mg_meql","Na_meql","K_meql"))%>%
  ggplot(aes(x = name, y = value, color = Codigo, group = Codigo)) +
  geom_point(size = 2)+
  geom_line()+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme(legend.position = "none")
sc
ggplotly(sc)



#############################################################################################################

data_base




#03 BD NORMAL SIN "<"
#04 BD NORMAL SIN "<", sin columnas de calculo de Balance iónico
#05 corrigiendo TDS
#06 BD CON "<" DE PRUEBA






