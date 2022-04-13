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

df = sample_groups(1000)

fig <- plot_ly(df, x = ~x_1, y= ~x_2)
fig <- fig %>% add_trace(z = ~y, name = 'Real value',mode = 'markers', type = 'scatter3d')
fig

df = split_data(df)


model <- keras_model_sequential() %>% 
  layer_dense(
    units = 64,
    activation = "relu",
    input_shape = c(ncol(df$df_train)-1)
  )%>%
  layer_dense(
    units = 32,
    activation = "relu"
  )%>%
  layer_dense(
    units = 1,
    activation = "sigmoid"
  )


model %>% 
  compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(),
    metrics = list("accuracy")
  )

history <- model %>% fit(
  x = df$df_train %>% select(-y)%>% as.matrix(),
  y = df$df_train$y,
  epochs = 100,
  validation_split = 0.2,
  callbacks = list(
    callback_early_stopping(
      monitor = "val_loss",
      min_delta = 0.001,
      patience = 10,
      verbose = 0,
      mode = "min",
      restore_best_weights = TRUE
    )
  )
)

predicted_values = model %>% predict(df$df_test %>% select(-y)%>% as.matrix())

fig <- plot_ly(df$df_test, x = ~x_1, y= ~x_2)
fig <- fig %>% add_trace(z = ~y, name = 'Real values',mode = 'markers', type = 'scatter3d')
fig <- fig %>% add_trace(z = predicted_values, name = 'Predictions',mode = 'markers', type = 'scatter3d')
fig

# Explain the model with DALEX
explainer = DALEX::explain(model = model,
                           data = df$df_test %>%select(-y)%>%as.matrix(),
                           y = df$df_test$y,
                           type = "classification",
                           label = "Keras model",
                           colorize = FALSE)

plot(model_performance(explainer))
plot(model_parts(explainer))
plot(model_profile(explainer))
plot(model_diagnostics(explainer))