con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "DESKTOP-0MGEA6E", 
                      Database = "Hydro_Yanacocha", 
                      Trusted_Connection = "True")

con@ptr
con@quote
con@info
con@encoding

nombredatabase <- "Estaciones"
database <- dbReadTable(con, nombredatabase)
head(database, 3)
tail(database, 3)
database <- database %>% mutate_if(is.character,as.factor)
View(database)
str(database)

database <- database %>% select(-contains("SSMA_TimeStamp"))
str(database)
database <- database %>% select(-contains("Ignorar"))
summary(database)

ggplot(data=database)+
  geom_point(aes(x=EsteLocal, y=NorteLocal, size=NivelFreatico))

# database_2 <- database[ ,-c(13,14)] ## otra forma de eleminar los datos
database_2 <- database # para no malograr procesamiento de la bd original
database_2 <- database_2 %>% filter(!is.na(NorteWGS84), !is.na(EsteWGS84))
Alonso <- database_2[ ,c("NorteWGS84","EsteWGS84")]
Alonso <- Alonso[ ,order(c(names(Alonso)))]
sputm  <- SpatialPoints(Alonso, proj4string=CRS("+proj=utm +zone=17 +south +datum=WGS84"))
spgeo  <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo  <- as.data.frame(spgeo)
colnames(spgeo) <- c("lng","lat")
ggplot(data=spgeo)+
  geom_point(aes(x=lng, y=lat))

df_final <- cbind(database_2, spgeo)
head(df_final)

mapview(df_final, xcol = "lng", ycol = "lat", crs = 4326, zcol="NivelFreatico")

df_final2 <- st_as_sf(df_final, coords = c("lng", "lat"),
                     remove = FALSE, crs = 32717, agr = "constant")
mapview(df_final2)

#### Generacion de produccion simulada ####

ui <- fluidPage(
  numericInput("nrows", "Enter the number of rows to display:", 5),
  tableOutput("tbl")
)

server <- function(input, output, session) {
  output$tbl <- renderTable({
    conn <- DBI::dbConnect(odbc::odbc(), 
                           Driver = "SQL Server", 
                           Server = "DESKTOP-0MGEA6E", 
                           Database = "Hydro_Yanacocha", 
                           Trusted_Connection = "True")
    on.exit(dbDisconnect(conn), add = TRUE)
    dbGetQuery(conn,
               "SELECT * FROM Alteraciones", n = input$nrows)
  })
}

shinyApp(ui,server)

#### Insert a new value:
# SET IDENTITY_INSERT dbo.Alteraciones ON
# INSERT INTO dbo.Alteraciones
# (IDAlteracion, NAlteracion, SAlteracion)
# VALUES
# (55, 'Silice margosa', 'SMP');
# SET IDENTITY_INSERT dbo.Alteraciones OFF




