# Usando ruta desde web:

ruta <- 'https://drive.google.com/file/d/17mFf5HxtAXJFx2Z2OL8Th-2TIopvGq20/view?usp=sharing'
ruta
readcsvfile <- read.table(file = ruta, header = TRUE, sep = ",")

write.csv(readcsvfile, file = "data_agua.csv", col.names = TRUE)

save(readcsvfile, file = "tabla_agua.Rdata")

load(file = "tabla_agua.Rdata")


# Usando ruta local:

path <- 'D:/9.CursoCIP/2.MaterialClase/2.ModuloI/ParteII/B/data_compilada/demo_data.csv'
path
readcsvfile <- read.table(file = path, header = TRUE, sep = ",")


# Usando funcion file.choose():

document <- read.table(file = file.choose(), header = TRUE, sep = ",")

# Guardar todo lo avanzado en R:

save.image(file = "Connection.RData")

# Comentar sobre formatos y encodificadores ... 


load(file = "Connection.RData")
