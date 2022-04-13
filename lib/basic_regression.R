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

df = sample_linear(1000)

plot(df$x,df$y)

df = split_data(df)


model <- keras_model_sequential() %>% 
  layer_dense(
    units = 1,
    activation = "linear",
    input_shape = c(ncol(df$df_train)-1)
  )


model %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_sgd(),
    metrics = list("mean_absolute_error")
  )

history <- model %>% fit(
  x = df$df_train %>% select(-y)%>% as.matrix(),
  y = df$df_train$y,
  epochs = 10,
  validation_split = 0.2
)

predicted_values = model %>% predict(df$df_test %>% select(-y)%>% as.matrix())

fig <- plot_ly(df$df_test, x = ~x)
fig <- fig %>% add_trace(y = ~y, name = 'Real values',mode = 'markers', type = 'scatter')
fig <- fig %>% add_trace(y = predicted_values, name = 'Predictions', mode = 'markers', type = 'scatter')
fig

plot(df$df_test$x, df$df_test$y, col='blue')
points(df$df_test$x, predicted_values, col = 'red')

### Example 2 - Sinusoid

df = sample_sinusoid(1000)

plot(df$x,df$y)

df = split_data(df)

model <- keras_model_sequential() %>% 
  layer_dense(
    units = 128,
    activation = "relu",
    input_shape = c(ncol(df$df_train)-1)
  )%>%
  layer_dense(
    units = 64,
    activation = "relu"
  )%>%
  layer_dense(
    units = 32,
    activation = "relu"
  )%>%
  layer_dense(
    units = 16,
    activation = "relu"
  )%>%
  layer_dense(
    units = 1,
    activation = "linear"
  )

model %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_sgd(
      learning_rate = 0.01,
      decay = 0
    ),
    metrics = list("mean_absolute_error")
  )

history <- model %>% fit(
  x = df$df_train %>% select(-y)%>% as.matrix(),
  y = df$df_train$y,
  epochs = 100,
  validation_split = 0.2
)

predicted_values = model %>% predict(df$df_test %>% select(-y)%>% as.matrix())

plot(df$df_test$x, df$df_test$y, col='blue')
points(df$df_test$x, predicted_values, col = 'red')


fig <- plot_ly(df$df_test, x = ~x)
fig <- fig %>% add_trace(y = ~y, name = 'Real values',mode = 'markers', type = 'scatter')
fig <- fig %>% add_trace(y = predicted_values, name = 'Predictions', mode = 'markers', type = 'scatter')
fig

# Try at home - 5 layers with number of neurons 128-64-32-16-1 and sgd(0.005) after 500 iterations, then with Adam optimizer (default lr, 100 iterations)


### Example 3 - Optimizer matters


df = sample_spherical(1000)

plot_ly(data = df, x=~x_1,y = ~x_2, z = ~y, type = 'scatter3d', mode = 'markers')

df = split_data(df)


model <- keras_model_sequential() %>% 
  layer_dense(
    units = 128,
    activation = "relu",
    input_shape = c(ncol(df$df_train)-1)
  )%>%
  layer_dense(
    units = 64,
    activation = "relu"
  )%>%
  layer_dense(
    units = 32,
    activation = "relu"
  )%>%
  layer_dense(
    units = 1,
    activation = "linear"
  )


model %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = list("mean_absolute_error")
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
fig <- fig %>% add_trace(z = predicted_values, name = 'Predictions', mode = 'markers', type = 'scatter3d')
fig


# Explain the model with DALEX
explainer = DALEX::explain(model = model,
                           data = df$df_test %>%select(-y)%>%as.matrix(),
                           y = df$df_test$y,
                           type = "regression",
                           label = "Keras model",
                           colorize = FALSE)

plot(model_performance(explainer))
plot(model_parts(explainer))
plot(model_profile(explainer))
plot(model_diagnostics(explainer))
