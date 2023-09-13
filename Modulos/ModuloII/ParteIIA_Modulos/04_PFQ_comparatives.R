
#### Parametros fisicoquimicos ##########################################################################################

data_base
colnames(data_base)


#### Temperatura ##############################################################################################
data_base$T_fuente

ggplot(data_base, aes(x=Codigo, y = T_fuente))+
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(face = "italic", color = "black",
                                   angle = 90, vjust = 0.5, hjust=1, size=8),
        axis.title.y = element_text(face = "italic",color = "black",
                                    size = 15, angle = 90))



#### Temperatura ##############################################################################################
# Temporada - Clase - Tipo - Temperatura

a <- data_base %>% drop_na(T_fuente) %>% 
  ggplot(aes(x = Codigo, y = T_fuente, group=1, # group = Cla_fuente,
  color= Cla_fuente))+
  geom_point()+
  #geom_line()+
  facet_grid(rows =vars(Tip_fuente), cols = vars(Temporada))+
  theme(axis.text.x = element_text(face = "italic", color = "black",
                                   angle = 90, vjust = 0.5, hjust=1, size=5),
        axis.title.y = element_text(face = "italic",color = "black",
                                    size = 15, angle = 90))
a
ggplotly(a)



#### pH vs T_fuente ##########################################################################################
data_base$Ph    

plot <- ggplot(data_base, aes(x = T_fuente, y = Ph, color = Cla_fuente,
                              shape = Tip_fuente,
                              text =    paste("Codigo", Codigo,
                                              "</br>TipFuente",Tip_fuente,
                                              "</br>ClaseFuente:",Cla_fuente,
                                              "</br>AspGeologico",Asp_geologico,
                                              "</br>ActAntropica:",Act_Antropica,
                                              "</br>OD",OD_mgL,
                                              "</br>CE (uS/cm):",CE_uS_cm
                              ))) + geom_point(size=4)+
  geom_text(data = filter(data_base, Ph>8.5 | Ph<6.5), aes(label=Codigo),vjust = 0,nudge_y = 0.05)+
  geom_hline(yintercept = 6.5, colour = "red")+ #ECA A1
  geom_hline(yintercept = 8.5, colour = "red")

plot
plotly::ggplotly(plot)



#### Conductividad Electrica vs T_fuente ##########################################################################################
data_base$CE_uS_cm     

plot <- ggplot(data_base, aes(x = T_fuente, y = CE_uS_cm, color = Cla_fuente,
                              shape = Tip_fuente,
                              text =    paste("Codigo", Codigo,
                                              "</br>TipFuente",Tip_fuente,
                                              "</br>ClaseFuente:",Cla_fuente,
                                              "</br>AspGeologico",Asp_geologico,
                                              "</br>ActAntropica:",Act_Antropica,
                                              "</br>OD",OD_mgL,
                                              "</br>CE (uS/cm):",CE_uS_cm,
                                              "</br>Realizado (uS/cm):",Realizado
                              ))) + geom_point(size=4)+
  geom_text_repel(data = filter(data_base, CE_uS_cm>1500), 
                  aes(label=Cod_Corto),vjust = 0, nudge_y = 0.18, point.padding = 0.5)+
  geom_hline(yintercept = 1500, colour = "red") #ECA A1

plot
plotly::ggplotly(plot)



#### pH vs Ce ##################################################################################################
plot <- ggplot(data_base, aes(x = Ph, y = CE_uS_cm, color = Lito_Predom,
                              shape = Cuenca,
                              text =    paste("Codigo", Codigo,
                                              "</br>TipFuente",Tip_fuente,
                                              "</br>ClaseFuente:",Cla_fuente,
                                              "</br>AspGeologico",Asp_geologico,
                                              "</br>ActAntropica:",Act_Antropica,
                                              "</br>OD",OD_mgL,
                                              "</br>CE (uS/cm):",CE_uS_cm
                              ))) + geom_point(size=4) #+
#geom_text_repel(data = filter(data_base, TDS_mg_L>1000), 
#                aes(label=Cod_Corto),vjust = 0,nudge_y = 0.05, point.padding = 0.5)+
#geom_hline(yintercept = 1000, colour = "red") #ECA A1

plot
plotly::ggplotly(plot)



#### TDS vs T_fuente ##########################################################################################
data_base$TDS_mg_L

plot <- ggplot(data_base, aes(x = T_fuente, y = TDS_mg_L, color = Cla_fuente,
                              shape = Tip_fuente,
                              text =    paste("Codigo", Codigo,
                                              "</br>TipFuente",Tip_fuente,
                                              "</br>ClaseFuente:",Cla_fuente,
                                              "</br>AspGeologico",Asp_geologico,
                                              "</br>ActAntropica:",Act_Antropica,
                                              "</br>OD",OD_mgL,
                                              "</br>CE (uS/cm):",CE_uS_cm
                              ))) + geom_point(size=4)+
  geom_text_repel(data = filter(data_base, TDS_mg_L>1000), 
                  aes(label=Cod_Corto),vjust = 0,nudge_y = 0.05, point.padding = 0.5)+
  geom_hline(yintercept = 1000, colour = "red") #ECA A1

plot
plotly::ggplotly(plot)



#### OD vs T_fuente ##########################################################################################
data_base$OD_mgL

plot <- ggplot(data_base, aes(x = T_fuente, y = OD_mgL, color = Cla_fuente,
                              shape = Tip_fuente,
                              text =    paste("Codigo", Codigo,
                                              "</br>TipFuente",Tip_fuente,
                                              "</br>ClaseFuente:",Cla_fuente,
                                              "</br>AspGeologico",Asp_geologico,
                                              "</br>ActAntropica:",Act_Antropica,
                                              "</br>OD",OD_mgL,
                                              "</br>CE (uS/cm):",CE_uS_cm,
                                              "</br>Realizado (uS/cm):",Realizado
                              ))) + geom_point(size=4)+
  geom_text_repel(data = filter(data_base, OD_mgL<6), 
                  aes(label=Cod_Corto),vjust = 0, nudge_y = 0.05, point.padding = 0.5)+
  geom_hline(yintercept = 6, colour = "red") #ECA A1

plot
plotly::ggplotly(plot)



#### Turbidez vs T_fuente ##########################################################################################
data_base$Turbidez

plot <- ggplot(data_base, aes(x = T_fuente, y = Turbidez, color = Cla_fuente,
                              shape = Tip_fuente,
                              text =    paste("Codigo", Codigo,
                                              "</br>TipFuente",Tip_fuente,
                                              "</br>ClaseFuente:",Cla_fuente,
                                              "</br>AspGeologico",Asp_geologico,
                                              "</br>ActAntropica:",Act_Antropica,
                                              "</br>OD",OD_mgL,
                                              "</br>CE (uS/cm):",CE_uS_cm,
                                              "</br>Realizado (uS/cm):",Realizado
                              ))) + geom_point(size=4)+
  geom_text_repel(data = filter(data_base, Turbidez>5), 
                  aes(label=Cod_Corto),vjust = 0, nudge_y = 0.05, point.padding = 0.5)+
  geom_hline(yintercept = 5, colour = "red") #ECA A1

plot
plotly::ggplotly(plot)



#### TDS vs CE ###############################################################################################
data_base$TDS_mg_L
data_base$CE_uS_cm

plot <- ggplot(data= data_base)+
  geom_point(aes(x=TDS_mg_L, y=CE_uS_cm, color =Cla_fuente, size = 4,
                 text =    paste("Codigo", Codigo,
                                 "</br>TipFuente",Tip_fuente,
                                 "</br>ClaseFuente:",Cla_fuente,
                                 "</br>AspGeologico",Asp_geologico,
                                 "</br>ActAntropica:",Act_Antropica,
                                 "</br>OD",OD_mgL,
                                 "</br>CE (uS/cm):",CE_uS_cm,
                                 "</br>Realizado (uS/cm):",Realizado
                 )))

plot
ggplotly(plot)



#### pH vs CE ################################################################################################
data_base$Ph
data_base$CE_uS_cm

coeff <- max(data_base$CE_uS_cm)/max(data_base$Ph)

plot <- ggplot(data= data_base, aes(x=Codigo))+
  geom_point(aes(y=Ph), size =2, color = "red")+
  geom_point(aes(y=CE_uS_cm/coeff), size=2, color ="blue")+
  scale_y_continuous(
    
    # Features of the first axis
    name = "pH (adimensional)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Conductividad Electrica (uS/cm)")
  ) + 
  
  theme(
    axis.title.y = element_text(color = "red", size=15),
    axis.title.y.right = element_text(color = "blue", size=15),
    axis.text.x = element_text(face = "italic", color = "black",
                               angle = 90, vjust = 0.5, hjust=1, size=8)
  ) +
  
  ggtitle("Ph y CE")
plot



#### pH vs Eh ################################################################################################
data_base$Ph
data_base$Eh

plot <- ggplot(data= data_base)+
  geom_point(aes(x=Ph, y=Eh/100, color =Cla_fuente, size=4,
                 text =    paste("Codigo", Codigo,
                                 "</br>TipFuente",Tip_fuente,
                                 "</br>ClaseFuente:",Cla_fuente,
                                 "</br>AspGeologico",Asp_geologico,
                                 "</br>ActAntropica:",Act_Antropica,
                                 "</br>OD",OD_mgL,
                                 "</br>CE (uS/cm):",CE_uS_cm,
                                 "</br>Realizado (uS/cm):",Realizado
                 )))
plot
ggplotly(plot)



###############################################################################################################

data_base




#03 BD NORMAL SIN "<"
#04 BD NORMAL SIN "<", sin columnas de calculo de Balance iónico
#05 corrigiendo TDS
#06 BD CON "<" DE PRUEBA




