#### Caso 4 ----

# Conexion a API de NASA y analisis de datos:
library(nasapower)

ag_d <- get_power(
  community = "ag",
  lonlat = c(-77.01, -9.30),
  pars = c("RH2M", "T2M", "PRECTOTCORR"),
  dates = c("2021-01-01","2021-12-31"),
  temporal_api = "hourly"
)

ag_d <- ag_d %>% rename(precipitacion = PRECTOTCORR,
                        temperatura = T2M,
                        humedad_relativa = RH2M)

estadisticas <- ag_d %>%
  group_by(YEAR, MO, DY) %>% 
  summarise(count =n(), 
            minimo = min(precipitacion, na.rm=TRUE),
            media = mean(precipitacion, na.rm = TRUE),
            maximo = max(precipitacion, na.rm = TRUE),
            acumulado_diario = sum(precipitacion, na.rm = TRUE))
estadisticas$time <- make_datetime(year = estadisticas$YEAR,
                                   month = estadisticas$MO,
                                   day = estadisticas$DY)
estadisticas %>%
  ggplot()+
  geom_line(aes(x=time, y=acumulado_diario))+
  geom_point(aes(x=time , y=acumulado_diario))

ag_d$time <- make_datetime(year = ag_d$YEAR,
                           month = ag_d$MO,
                           day = ag_d$DY,
                           hour = ag_d$HR)

dateRange <- c(as.Date("2021-04-01"),as.Date("2021-04-20"))
ag_d %>%
  filter(time >= dateRange[1] & time <= dateRange[2]) %>%
  ggplot()+
  geom_line(aes(x=time, y=precipitacion))+
  geom_point(aes(x=time, y=precipitacion))

ag_d %>%
  filter(time >= dateRange[1] & time <= dateRange[2]) %>%
  ggplot()+
  geom_line(aes(x=time, y=humedad_relativa))+
  geom_point(aes(x=time, y=humedad_relativa))


#### Caso 5 ----

# Analisis de data GNNS_CORS

mgal <- read.table(file = "data_compilada/MGAL_mo_Data_GNSS_CORS.txt", header=TRUE)
mgal$YYYYMMDD <- ymd(mgal$YYYYMMDD)

a <- plot_ly(mgal, type = 'scatter', mode = 'markers')%>%
  add_trace(x = ~YYYYMMDD, y = ~dN)%>%
  layout(showlegend = F)
a <- a %>%
  layout(showlegend = F, title='Series de Tiempo Desplazamiento - Norte',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=1, label="1m", step="month", stepmode="backward"),
                          list(count=6, label="6m", step="month", stepmode="backward"),
                          list(count=1, label="YTD", step="year", stepmode="todate"),
                          list(count=1, label="1y", step="year", stepmode="backward"),
                          list(step="all")
                        ))))
b <- plot_ly(mgal, type = 'scatter', mode = 'markers')%>%
  add_trace(x = ~YYYYMMDD, y = ~dE)%>%
  layout(showlegend = F)
b <- b %>%
  layout(showlegend = F, title='Series de Tiempo Desplazamiento - Este',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=1, label="1m", step="month", stepmode="backward"),
                          list(count=6, label="6m", step="month", stepmode="backward"),
                          list(count=1, label="YTD", step="year", stepmode="todate"),
                          list(count=1, label="1y", step="year", stepmode="backward"),
                          list(step="all")
                        ))))

c <- plot_ly(mgal, type = 'scatter', mode = 'markers')%>%
  add_trace(x = ~YYYYMMDD, y = ~dU)%>%
  layout(showlegend = F)
c <- c %>%
  layout(showlegend = F, title='Series de Tiempo Desplazamiento - Vertical',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=1, label="1m", step="month", stepmode="backward"),
                          list(count=6, label="6m", step="month", stepmode="backward"),
                          list(count=1, label="YTD", step="year", stepmode="todate"),
                          list(count=1, label="1y", step="year", stepmode="backward"),
                          list(step="all")
                        ))))


subplot(a, b, c, nrows=1)

#### Caso 6 ----

SRM <- read_excel(path = "data_compilada/SRM.xlsx", col_names = TRUE)
str(SRM)
SRM$Tiempo <- mdy_hms(SRM$Tiempo)
colnames(SRM)


SRU <- read_excel(path = "data_compilada/SRU.xlsx", col_names = TRUE)
str(SRU)
SRU$Tiempo <- mdy_hms(SRU$Tiempo)
colnames(SRU)

STT <- read_excel(path = "data_compilada/STT.xlsx", col_names = TRUE)
str(STT)
STT$Tiempo <- mdy_hms(STT$Tiempo )
colnames(SRM)

Water <- rbind(SRM, SRU, STT)
str(Water)
Water$Tiempo <- ymd_hms(Water$Tiempo)

Water2 <- Water %>% filter(CE_datalogger_uS_cm<mean(Water$CE_datalogger_uS_cm))

m <- Water2 %>%
  ggplot(aes(x=Tiempo, y = CE_datalogger_uS_cm))+
  geom_area(fill = "#69b3a2", alpha = 0.5)+
  geom_line(aes(y= 100*Q_m3_s), colour = "red")+
  geom_line(color = "#69ba32")+
  theme_ipsum()
ggplotly(m)

n <- Water2 %>% ggplot(aes(x=Tiempo, y = CE_datalogger_uS_cm))+
  geom_point(aes(color=Nombre), size = 0.5)
ggplotly(n)

o <- Water2 %>% ggplot(aes(x = Tiempo, y =Nivel_comp_m))+
  geom_point(aes(color = Nombre), size = 0.5)+
  geom_hline(yintercept = mean(Water$Ca_plus_Mg_mg.l))
ggplotly(o)


data01 <- Water2[ ,c("Este","Norte")]
data01 <- data01[ ,order(c(names(data01)))]
sputm <- SpatialPoints(data01, proj4string = CRS("+proj=utm +zone=18 +south +datum=WGS84")) ; str(sputm)
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo <- as.data.frame(spgeo)
colnames(spgeo) <- c("long","lat")
prueba <- cbind(Water2, spgeo)
str(prueba)
prueba$Nombre <- as.factor(prueba$Nombre); prueba$Codigo <- as.factor(prueba$Codigo); prueba$Tiempo <- ymd_hms(prueba$Tiempo)
str(prueba)

leaflet(prueba) %>%
  addTiles(group = "Street View") %>%
  addCircleMarkers()


