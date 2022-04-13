library(keras)
library(tensorflow)
library(plotly)
library(DALEX)
library(dotenv)

load_dot_env(file = ".env")
setwd(Sys.getenv("lib_path"))

source('generate_data.R')
source('keras_utils.R')
source('explainers.R')


df = sample_multifeature(1000, 8, noise_ratio = 0.5, correlated_features_ratio = 0.5)

for( column in colnames(df)){
  plot(df[,column], df[,'y'])
}

### Define parameters 

models_params = list(
  model1 = list( # Group 1 - optimizers + relu
    optimizer = optimizer_adam(), 
    layers = list(
      layer1 = list(
        n_units = 64,
        activation_function = 'relu'
      ),
      layer2 = list(
        n_units = 32,
        activation_function = 'relu'
      )
    )
  ),
  model2 = list(
    optimizer = optimizer_sgd(),
    layers = list(
      layer1 = list(
        n_units = 64,
        activation_function = 'relu'
      ),
      layer2 = list(
        n_units = 32,
        activation_function = 'relu'
      )
    )
  ),# Group 2 - optimizers + sigmoid
  model3 = list(
    optimizer = optimizer_adam(),
    layers = list(
      layer1 = list(
        n_units = 64,
        activation_function = 'sigmoid'
      ),
      layer2 = list(
        n_units = 32,
        activation_function = 'sigmoid'
      )
    )
  ),
  model4 = list(
    optimizer = optimizer_sgd(),
    layers = list(
      layer1 = list(
        n_units = 64,
        activation_function = 'sigmoid'
      ),
      layer2 = list(
        n_units = 32,
        activation_function = 'sigmoid'
      )
    )
  ), # Group 3 - optimizers with tanh
  model5 = list(
    optimizer = optimizer_adam(),
    layers = list(
      layer1 = list(
        n_units = 64,
        activation_function = 'tanh'
      ),
      layer2 = list(
        n_units = 32,
        activation_function = 'tanh'
      )
    )
  ),
  model6 = list(
    optimizer = optimizer_sgd(),
    layers = list(
      layer1 = list(
        n_units = 64,
        activation_function = 'tanh'
      ),
      layer2 = list(
        n_units = 32,
        activation_function = 'tanh'
      )
    )
  )
)


df = split_data(df)
models = train_models(models_params = models_params, df)

### Explainability with DALEX

explainers = explain_models(models = models)

plot(explainers$model_performance_value)
plot(explainers$model_performance_value, geom = "boxplot")
plot(explainers$model_parts_value)
plot(explainers$model_diagnostics_value)
plot(explainers$model_profile_value)