library(keras)
library(dplyr)

split_data <-function(df, split_fraction=0.8){
  
  train_ids    <- sort(sample(seq_len(nrow(df)), size=round(nrow(df) * split_fraction)))
  df_train   <- df[train_ids, ]
  df_test <- df[-train_ids, ]
  return(list("df_train"=df_train,"df_test"=df_test))
}

build_reg_model <- function(optimizer,layers) {
  model <- keras_model_sequential()%>%
    layer_dense(units = layers[[1]][[1]],
                activation = layers[[1]][[2]])
  
  for(i in 2:length(layers)){
    model = model %>%
      layer_dense(units = layers[[i]][[1]], activation = layers[[i]][[2]])
  }
  model = model %>% 
    layer_dense(units = 1) 
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer,
      metrics = list("mean_absolute_error")
    )
  
  return(model)
}

build_class_model <- function(optimizer,layers, n_classes) {
  model <- keras_model_sequential()%>%
    layer_dense(units = layers[[1]][[1]],
                activation = layers[[1]][[2]])
  
  for(i in 2:length(layers)){
    model = model %>%
      layer_dense(units = layers[[i]][[1]], activation = layers[[i]][[2]])
  }
  model = model %>% 
    layer_dense(
      activation = "softmax",
      units = n_classes) 
  
  model %>% 
    compile(
      loss = "categorical_crossentropy",
      optimizer = optimizer,
      metrics = list("categorical_accuracy")
    )
  
  return(model)
}

train_models <-function(models_params, df){
  models = vector(
    mode = "list", length = length(models_params)
  )
  names(models) = paste0(rep('model_',length(models_params)),as.character(1:length(models_params)))
  for(i in 1:length(models_params)){
    models[[i]] = list(
      model=NA,
      history= NA
    )
    models[[i]]$model = build_reg_model(models_params[[i]]$optimizer,models_params[[i]]$layers)
    
    models[[i]]$history<- models[[i]]$model %>% fit(
      x = df$df_train %>% select(-y)%>% as.matrix(),
      y = df$df_train$y,
      epochs = 50,
      validation_split = 0.2,
      callbacks = list(
        callback_early_stopping(
          monitor = "val_loss",
          min_delta = 0.01,
          patience = 10,
          verbose = 0,
          mode = "min",
          restore_best_weights = TRUE
        )
      )
    )
    
  }
  return(models)
}

select_by_metrics <- function(models,models_params){
  for(model in names(models)){
    print(model)
    print(models[[model]]$history)
  }
  for(metric_name in names(models[[model]]$history$metrics)){
    
  }
}
