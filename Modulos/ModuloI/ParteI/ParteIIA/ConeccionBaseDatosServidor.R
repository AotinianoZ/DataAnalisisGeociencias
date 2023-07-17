####Conexión a Base de Datos####

# Revisar que odbc drivers estan instalados y disponibles

(odbcListDrivers()[[1]])

#### Para nuestro caso usaremos MySQL ####
# Servidor con MySQL version 

mycon <- dbConnect(RMySQL::MySQL(),
                 dbname = "branextc_Geochemical",
                 host = "dallas166.arvixeshared.com",
                 port = 3306,
                 user = "branextc",
                 password = "gdhB0a2X38")

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

df <- DBI::dbReadTable(mycon, "intrusivo")

sdf <- readxl::read_xlsx(path="ParteI/ParteIIA/GQ_Colca_corregido2.xlsx", col_names = TRUE)

#### Analisis exploratorio de datos (AED) basico #### 

View(df)
str(df)
character_vals <- lapply(df,class)=="character"
df[ ,character_vals] <- lapply(df[ ,character_vals],as.factor)
colnames(df)
str(df)
head(df)
tail(df)
summary(df)

#### Cobre:

hist(df$Cu_ppm)
plot(density(df$Cu_ppm))
boxplot(df$Cu_ppm)
plot(ecdf(df$Cu_ppm))

#### Oro:

hist(df$Au_ppb_com)
plot(density(df$Au_ppb_com))
boxplot(df$Au_ppb_com)
plot(ecdf(df$Au_ppb_com))


## Bivariante Plot:

ggplotly(ggplot(data=df, 
                aes(x=Cu_ppm, y=Au_ppb_com, color= ROCA))+
  geom_point(size=5))

##### Analisis Multivariable Basico #### 

# Cluterizacion de Variables continuas:

df2 <- df[ ,9:ncol(df)]
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

df2 <- df[ ,9:ncol(df)]
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

df <- DBI::dbReadTable(mycon, "DIP_Strong")

ui <- fluidPage(
  numericInput("nrows", "Enter the number of rows to display:", 5),
  tableOutput("tbl")
)

server <- function(input, output, session) {
  output$tbl <- renderTable({
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "branextc_Geochemical",
      host = "dallas166.arvixeshared.com",
      port = 3306,
      username = "branextc",
      password = "gdhB0a2X38")
    on.exit(dbDisconnect(conn), add = TRUE)
    dbGetQuery(conn,
               "SELECT * FROM DIP_Strong", n = input$nrows)
  })
}

shinyApp(ui,server)




