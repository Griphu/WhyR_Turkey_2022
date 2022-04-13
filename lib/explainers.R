library(DALEX)

explain_models_dep <- function(models){
  explainers = vector(
    mode = "list", length = length(models)
  )
  names(explainers) = paste0(rep('explainer_',length(models)),as.character(1:length(models)))
  
  for(i in 1:length(models)){
    explainers[[i]] = list(
      explainer = NA,
      model_performance_value = NA,
      model_parts_value = NA
    )
    
    
    explainers[[i]]$explainer <- DALEX::explain(model = models[[i]]$model,
                                                data = df$df_test %>%select(-y)%>%as.matrix(),
                                                y = df$df_test$y,
                                                type = "regression",
                                                label = paste0("Keras model ",i),
                                                colorize = FALSE)
    explainers[[i]]$model_performance_value = model_performance(explainers[[i]]$explainer)
    explainers[[i]]$model_parts_value = model_parts(explainers[[i]]$explainer)
  }
  return(explainers)
}

explain_models <-function(models){
  explainers = list(
    explainer= vector(
      mode = "list", length = length(models)
    ),
    model_performance_value= vector(
      mode = "list", length = length(models)
    ),
    model_parts_value= vector(
      mode = "list", length = length(models)
    ),
    model_profile_value= vector(
      mode = "list", length = length(models)
    ),
    model_diagnostics_value= vector(
      mode = "list", length = length(models)
    )
  )
  for(i in 1:length(models)){
    explainers$explainer[[i]] <- DALEX::explain(model = models[[i]]$model,
                                                data = df$df_test %>%select(-y)%>%as.matrix(),
                                                y = df$df_test$y,
                                                type = "regression",
                                                label = paste0("Keras model ",i),
                                                colorize = FALSE)
    explainers$model_performance_value[[i]] = model_performance(explainers$explainer[[i]])
    explainers$model_parts_value[[i]] = model_parts(explainers$explainer[[i]])
    explainers$model_profile_value[[i]] = model_profile(explainers$explainer[[i]])
    explainers$model_diagnostics_value[[i]] = model_diagnostics(explainers$explainer[[i]])
    
  }
  return(explainers)
}
