#### Proceso Generalizado de ML

# 1. Configuraciones del proyecto e instalacion de paquetes
# 2. Base de Datos y estructuras de datos a usar.
# 3. Carga y analisis de Datos (Estadisticas Descriptiva e Inferencial)
# 4. Procesamiento y Tratamiento
# 5. Evaluacion de las metricas
# 6. Fase de Modelado
# 7. Optimizacion del Forecasting

# Modelaremos los valores de CE acorde a 6 valores numéricos referentes:

var_estudio <- c("Ph","T_fuente","TDS_mg_L","Salin_PSU", "R_Kohm_cm","OD_mgL","CE_uS_cm" )
data_ml <- data_base %>% select(any_of(var_estudio))
str(data_ml)

# target CE_uS_cm
# Conocer nuestros datos
head(data_ml, n=20)
# Datos Nulos:
library(Amelia)
missmap(data_ml,col=c("black","grey"),legend = FALSE)
# Dimension del dataset
dim(data_ml)
#Tipos de Dato
sapply(data_ml, class)
data_ml$TDS_mg_L <- as.numeric(data_ml$TDS_mg_L)
#Distribucion de Clase (En caso el target sea una clase)
library(caret)
data(PimaIndiansDiabetes)
y<-PimaIndiansDiabetes$diabetes
cbind(freq = table(y), percentage = prop.table(table(y)*100))
table(y)
prop.table(table(y))
#Resumen de datos
colnames(data_ml) <- c("ph","T","TDS","Salinidad","Resistividad","OD","target")
data_ml <- data_ml %>% select(!OD)
summary(data_ml)
#Desviación estandar
sapply(data_ml[ , 1:5],sd)
#Asimetria
library(e1071)
apply(data_ml[ ,1:5],2,skewness)
args(apply)
#Correlaciones
correlation <-cor(data_ml[ ,1:5])


####Visualizacion####
#Univariante:
plot_list = list()
for (i in 1:5) {
  p <- plot(data_ml[ ,i], main = paste(names(data_ml[ ,i]), "variable"))
  plot_list[[i]] = p
}
#Histograma 
par(mfrow=c(2,3))
for (i in 1:4){
  hist(data_ml[ ,i], main = names(data_ml)[i])
}
#Densidad
library(lattice)
par(mfrow=c(2,3))
for (i in 1:5){
  plot(density(data_ml[ ,i]), main=names(data_ml)[i]) 
}
#Boxplot
par(mfrow=c(2,3))
for (i in 1:5){
  boxplot(data_ml[ ,i], main=names(data_ml)[i])
}

####Visualizacion Multivariable####
#Grafico de Correlacion

corrplot(correlation, method ="circle")

#Matriz de Dispersion
library(ggplot2)
library(GGally)
pairs(data_ml, pch=19, col='blue', lower.panel=panel.smooth)
a <- ggpairs(data_ml)
ggplotly(a)
# Ce= 223.9
data_ml %>% filter(!target==223.9)
a <- ggplot(data_ml)+
  geom_point(aes(x=Salinidad, y = target))
ggplotly(a)


#Matriz de Dispersión Por Clase (en caso exista clases: solo ejemplo)
data(iris)
pairs(Species~., data=iris, col=iris$Species)
plot(iris[1:2], pch=21, bg=c("red","green3","blue"))
pairs(iris[1:4], main = "IRIS 3 species", pch=21, 
      legend=c("Setosa","Versicolor","Virginica"),
      bg = c("red","green3","blue"))

#Matriz de Densidad por Clase
x<-iris[ ,1:4]
y<-iris[ ,5]
scale<-list(x=list(relation ="free"),y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scale)

#Boxplot por Clase
x<-iris[ ,1:4]
y<-iris[ ,5]
featurePlot(x=x, y=y, plot="boxplot")


summary(data_ml)
### Procesamiento-Tratamiento

# En nuestro caso no usaremos este paso: 
# Escalamiento, Centrado, Estandarizacion, Normalizacion, Box-Cox
# Yeo-Jhonson, PCA, ICA, entre otros:


#### Modelo Aprendijaze Superisado (Problema de Regresion) ----
library(tidymodels)
# Paquetes auxiliares
library(tidyverse)
library(vip)

data_ml_bc <- data_ml

glimpse(data_ml_bc %>% filter(!target==223.9))
modelado <-  data_ml_bc %>% filter(!target==223.9)
modelado
modelado2 <- modelado %>% 
   group_by(across(-target)) %>% 
   summarize(target = mean(target),
             .groups = "drop")
modelado
nrow(modelado)

# semilla inicializadora
set.seed(1501)
# dividimos un 75% de data para el entramiento con selección aleatoria
modelado_split <- initial_split(modelado, strata = target, prop=0.75)
modelado_train <- training(modelado_split)
modelado_test <- testing(modelado_split)

set.seed(1502)

# k-fold cross-validation (validacion cruzada de ladata)
modelado_folds <- vfold_cv(modelado_train, strata = target, repeats = 5)


# Algunos modelos (ANN, KNN, SVM) requieren estar centrados y escalados
# Para otros un diseño superficial de expansión es buena idea

normalized_rec <- recipe(target ~., data=modelado_train) %>%
  step_normalize(all_predictors())

poly_recipe <- normalized_rec %>%
  step_poly(all_predictors()) %>% # uso de polinomiales ortogonales ("pca")
  step_interact(~all_predictors():all_predictors()) # use information before

library(rules)
library(baguette)
library(parsnip)

library(ranger)
library(xgboost)
library(kknn)

#penalty: El numero total de regularizaciones
# mixture: Proporciond de regularizaciones L1

#tune: argumentos a "tunear"

# set_engine() : modelo a usar
# set_mode() : especifica tarea (regression en nuestro caso)
# en caso no este presente significa que algoritmo es de regresion solamente.

# regresion lineal
linear_reg_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

# neural net
nnet_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet", MaxNWts = 2600) %>% # MaxNWts = maximo numero de pesos
  set_mode("regression")

# mars
mars_spec <- 
  mars(prod_degree = tune()) %>%  #<- use GCV to choose terms
  set_engine("earth") %>% 
  set_mode("regression")

# support vector machine radial (svm)
svm_r_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

# support vector machine polynomial (svm_p)
svm_p_spec <- 
  svm_poly(cost = tune(), degree = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

# k neares neighbors (knn)
knn_spec <- 
  nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

# decision tree (cart)
cart_spec <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

# bagged tree (bag_cart)
bag_cart_spec <- 
  bag_tree() %>% 
  set_engine("rpart", times = 50L) %>% 
  set_mode("regression")

# random forest (rf)
rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

# xg boost machine (xgb)
xgb_spec <- 
  boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
             min_n = tune(), sample_size = tune(), trees = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

# Cubist (cubist)
cubist_spec <- 
  cubist_rules(committees = tune(), neighbors = tune()) %>% 
  set_engine("Cubist") 

# escogiendo capas ocultas ANN
nnet_param <- 
  nnet_spec %>% 
  extract_parameter_set_dials() %>% 
  update(hidden_units = hidden_units(c(7, 5)))


# Modelos que necesitan normalizacion SVM_r
normalized <- 
  workflow_set(
    preproc = list(normalized = normalized_rec), 
    models = list(SVM_radial = svm_r_spec, SVM_poly = svm_p_spec, 
                  KNN = knn_spec, neural_network = nnet_spec)
  )
normalized
normalized %>% extract_workflow(id = "normalized_KNN")

normalized <- 
  normalized %>% 
  option_add(param_info = nnet_param, id = "normalized_neural_network")
normalized

model_vars <- 
  workflow_variables(outcomes = target, 
                     predictors = everything())
no_pre_proc <- 
  workflow_set(
    preproc = list(simple = model_vars), 
    models = list(MARS = mars_spec, CART = cart_spec, CART_bagged = bag_cart_spec,
                  RF = rf_spec, boosting = xgb_spec, Cubist = cubist_spec)
  )
no_pre_proc

with_features <- 
  workflow_set(
    preproc = list(full_quad = poly_recipe), 
    models = list(linear_reg = linear_reg_spec, KNN = knn_spec)
  )

all_workflows <- 
  bind_rows(no_pre_proc, normalized, with_features) %>% 
  # Make the workflow ID's a little more simple: 
  mutate(wflow_id = gsub("(simple_)|(normalized_)", "", wflow_id))
all_workflows

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

grid_results <-
  all_workflows %>%
  workflow_map(
    seed = 1503,
    resamples = modelado_folds,
    grid = 25,
    control = grid_ctrl
  )
grid_results

grid_results %>% 
  rank_results() %>% 
  filter(.metric == "rmse") %>% 
  select(model, .config, rmse = mean, rank)

autoplot(
  grid_results,
  rank_metric = "rmse",  # <- how to order models
  metric = "rmse",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) +
  geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 1.5,
            vjust =1.5) +
  #lims(y = c(3.5, 9.5)) +
  theme(legend.position = "none")

autoplot(grid_results, id = "Cubist", metric = "rmse")


library(finetune)

race_ctrl <-
  control_race(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

race_results <-
  all_workflows %>%
  workflow_map(
    "tune_race_anova",
    seed = 1503,
    resamples = modelado_folds,
    grid = 25,
    control = race_ctrl
  )

autoplot(
  race_results,
  rank_metric = "rmse",  
  metric = "rmse",       
  select_best = TRUE    
) +
  geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 1.5,
            vjust =1.5) +
  #lims(y = c(3.0, 9.5)) +
  theme(legend.position = "none")

matched_results <- 
  rank_results(race_results, select_best = TRUE) %>% 
  select(wflow_id, .metric, race = mean, config_race = .config) %>% 
  inner_join(
    rank_results(grid_results, select_best = TRUE) %>% 
      select(wflow_id, .metric, complete = mean, 
             config_complete = .config, model),
    by = c("wflow_id", ".metric"),
  ) %>%  
  filter(.metric == "rmse")

library(ggrepel)

matched_results %>% 
  ggplot(aes(x = complete, y = race)) + 
  geom_abline(lty = 3) + 
  geom_point() + 
  geom_text_repel(aes(label = model)) +
  coord_obs_pred() + 
  labs(x = "Complete Grid RMSE", y = "Racing RMSE") 


##FINALIZING A MODEL 

best_results <- 
  race_results %>% 
  extract_workflow_set_result("boosting") %>% 
  select_best(metric = "rmse")
best_results
#> # A tibble: 1 × 7
#>   trees min_n tree_depth learn_rate loss_reduction sample_size .config              
#>   <int> <int>      <int>      <dbl>          <dbl>       <dbl> <chr>                
#> 1  1957     8          7     0.0756    0.000000145       0.679 Preprocessor1_Model04

boosting_test_results <- 
  race_results %>% 
  extract_workflow("boosting") %>% 
  finalize_workflow(best_results) %>% 
  last_fit(split = modelado_split)

collect_metrics(boosting_test_results)


boosting_test_results %>% 
  collect_predictions() %>% 
  ggplot(aes(x = target, y = .pred)) + 
  geom_abline(color = "gray50", lty = 2) + 
  geom_point(alpha = 0.5) + 
  coord_obs_pred() + 
  labs(x = "observed", y = "predicted")







