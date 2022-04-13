library(keras)
library(tensorflow)
library(dplyr)
library(plotly)
library(dotenv)

load_dot_env(file = ".env")
setwd(Sys.getenv("lib_path"))

source('generate_data.R')
source('keras_utils.R')
source('explainers.R')

### Example 1

df = sample_multifeature_cat(1000,6,noise_ratio = 0.2,correlated_features_ratio = 0.6)

n_classes = 4
class = df$y
categorical_labels <- to_categorical(df$y, n_classes)
categorical_labels = data.frame(categorical_labels)
colnames(categorical_labels) = c('y_1','y_2','y_3','y_4')


df = cbind(df[,-length(df)],categorical_labels)

df = split_data(df)

model <- keras_model_sequential() %>% 
  layer_dense(
    units = 64,
    activation = "relu",
    input_shape = c(ncol(df$df_train)-n_classes)
  )%>%
  layer_dense(
    units = 32,
    activation = "relu"
  ) %>%
  layer_dense(
    activation = "softmax",
    units = n_classes) 

model %>% 
  compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(),
    metrics = list("categorical_accuracy")
  )

history <- model %>% fit(
  x = df$df_train %>% select(-c('y_1','y_2','y_3','y_4'))%>% as.matrix(),
  y = df$df_train[,c('y_1','y_2','y_3','y_4')]%>% as.matrix(),
  epochs = 100,
  validation_split = 0.2,
  callbacks = list(
    callback_early_stopping(
      monitor = "val_loss",
      min_delta = 0.01,
      patience = 50,
      verbose = 0,
      mode = "min",
      restore_best_weights = TRUE
    )
  )
)

predicted_probs = model %>% predict(df$df_test %>% select(-c('y_1','y_2','y_3','y_4'))%>% as.matrix())
predicted_indices = max.col(predicted_values)

real_values = df$df_test %>% select(c('y_1','y_2','y_3','y_4'))
real_indices = max.col(real_values)


colMeans(abs(real_values - predicted_probs))

fig <- plot_ly(df$df_test,
               x = seq(0,1,length.out = nrow(df$df_test)), 
               y = rowMeans(abs(real_values - predicted_probs)), 
               name = 'Distribution of sum absolute error',
               mode = 'markers', 
               type = 'scatter')
fig

mean(abs(predicted_indices - real_indices))

table(predicted_indices,real_indices)
