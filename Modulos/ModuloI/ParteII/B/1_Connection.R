# Usando ruta desde web:

ruta <- 'https://drive.google.com/file/d/17mFf5HxtAXJFx2Z2OL8Th-2TIopvGq20/view?usp=sharing'
ruta

ruta2 <- "aozweb.com/MaterialCurso/demo_data.csv"
ruta2

ruta3 <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data'
ruta3

readcsvfile <- read.csv(file = ruta, header = TRUE, sep = ",")
readcsvfile <- read.table(file = ruta2, header = TRUE, sep = ",")

readcsvfile <- read.table(file = ruta3, header = TRUE, sep = ",")

# Usando ruta local:

path <- 'D:/9.CursoCIP/2.MaterialClase/2.ModuloI/ParteII/B/data_compilada/demo_data.csv'
path
readcsvfile <- read.table(file = path, header = TRUE, sep = ",")


# Usando funcion file.choose():

document <- read.table(file = file.choose(), header = TRUE, sep = ",")

write.csv(document, file = "data_agua.csv")
save(document, file = "tabla_agua.Rdata")
load(file = "tabla_agua.Rdata")

a <- "Picón"

# Guardar todo lo avanzado en R:

save.image(file = "Connection.RData")

# Comentar sobre formatos y encodificadores ... 


load(file = "Connection.RData")
