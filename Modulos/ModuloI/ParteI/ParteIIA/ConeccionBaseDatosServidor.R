####Conexión a Base de Datos####

# Revisar que odbc drivers estan instalados y disponibles

(odbcListDrivers()[[1]])

#### Para nuestro caso usaremos MySQL ####
# Servidor con MySQL version 

mycon <- dbConnect(RMySQL::MySQL(),
                 dbname = "branext_sabado",
                 host = "dallas166.arvixeshared.com",
                 port = 3306,
                 user = "branext_users",
                 password = "g?Y_3s3FUYB6")

mycon <- dbConnect(RMySQL::MySQL(),
                   dbname = "branext_test",
                   host = "dallas166.arvixeshared.com",
                   port = 3306,
                   user = "branext_alonso",
                   password = "@lonso12345")

mycon

# Luego que nos conectamos a la base de datos realizaremos eligiremos
# una tabla para conectarla y realizar el análisis:

#### Para tener en cuenta (no lo desarrollaré, porque no quiero modificar tablas del servidor)####

# Leyendo data desde
df <- DBI::dbGetQuery(con,"Select Country, count(*) as n  from  Customers group by Country")
df <- DBI::dbReadTable(con, 'Customers')

# Escribiendo a la base de datos
DBI::dbWriteTable(con, 'df', df)
# Eliminando data
DBI::dbExecute(con, "Delete from df")
# Agregando ata
DBI::dbAppendTable(con,'df',df)
# Removiendo una tabla
DBI::dbRemoveTable(con,'df')
# Ejecutando procedimientos de almacenamiento
DBI::dbCallProc(conn,'Ten Most Expensive Products')
DBI::dbExecute(con, 'EXEC [Ten Most Expensive Products]')
# Cuando se desea resultados de salida desde tus procedimientos de guardados
DBI::dbGetQuery(con, 'EXEC [Ten Most Expensive Products]')
# Cuando tienens un proveido de parámetros para procedimientos de almancenamiento
# en este ejemplo proveemos el inicio y fin de la fecha.
DBI::dbGetQuery(con, "EXEC  [Employee Sales by Country]
        @Beginning_Date = '01/01/1995',
        @Ending_Date = '01/01/2010'")


#### Tidyverse y GGplot ####

# Conexion a base de datos:

sdf <- DBI::dbReadTable(mycon, "gq_colca_corregido2")

sdf <- readxl::read_xlsx(path="Modulos/ModuloI/ParteI/ParteIIA/GQ_Colca_corregido2.xlsx", col_names = TRUE)

#### Analisis exploratorio de datos (AED) basico #### 

View(sdf)
str(sdf)
character_vals <- lapply(sdf,class)=="character"
sdf[ ,character_vals] <- lapply(sdf[ ,character_vals], as.factor)
colnames(sdf)
str(sdf)
head(sdf)
tail(sdf)
summary(sdf)

#### Cobre:

hist(sdf$Cu_ppm)
plot(density(sdf$Cu_ppm))
boxplot(sdf$Cu_ppm)
plot(ecdf(sdf$Cu_ppm))

#### Oro:

hist(sdf$Au_ppb_com)
plot(density(sdf$Au_ppb_com))
boxplot(sdf$Au_ppb_com)
plot(ecdf(sdf$Au_ppb_com))

## Bivariante Plot:

ggplotly(ggplot(data=sdf, 
                aes(x=Cu_ppm, y=Au_ppb_com, color= ROCA))+
  geom_point(size=5))

sdf %>% dplyr::filter(Au_ppb_com > 2500)

##### Analisis Multivariable Basico #### 

# Cluterizacion de Variables continuas:

df2 <- sdf[ ,9:ncol(sdf)]
pairs(df2[ ,1:10])


# Compute PCA with ncp = 2
res.pca <- PCA(df2, ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE)


# Dendograma:

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)


res.hcpc$desc.var$quanti
data_final <- res.hcpc$data.clust
View(data_final)

data_final %>%
  filter(clust =="3")

ggplotly(ggplot(data=data_final, aes(x=Au_ppb_com, y =Cu_ppm, shape=clust, color=clust))+
  geom_point(size=5))

###  CLUSTER ALGORITMOS

df2 <- sdf[ ,9:ncol(sdf)]
pairs(df2[ ,1:10])


#K-Means:

kmedias <- scale(df2)
head(kmedias, n = 3)
fviz_nbclust(kmedias, kmeans, method = "wss") #El Codo
fviz_nbclust(kmedias, kmeans, method = "silhouette") #Silhouette
fviz_nbclust(kmedias, kmeans, method = "gap_stat") #gapstat
km.res <- kmeans(kmedias, 8, nstart = 25)
fviz_cluster(km.res, data = kmedias)
km.res <- kmeans(kmedias, 3, nstart = 25)
fviz_cluster(km.res, data = kmedias)


#### DEMO Real Time Table ####

df <- DBI::dbReadTable(mycon, "gq_colca_corregido2")

ui <- fluidPage(
  numericInput("nrows", "Enter the number of rows to display:", 5),
  tableOutput("tbl")
)

server <- function(input, output, session) {
  output$tbl <- renderTable({
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "branext_test",
      host = "dallas166.arvixeshared.com",
      port = 3306,
      username = "branext_alonso",
      password = "@lonso12345")
    on.exit(dbDisconnect(conn), add = TRUE)
    dbGetQuery(conn,
               "SELECT * FROM gq_colca_corregido2", n = input$nrows)
  })
}

shinyApp(ui,server)




