linear <-function(x,a=NA,b=NA){
  if(is.na(a)&is.na(b)){
    a = runif(1,min = -5,max = 5)
    b = runif(1,min = -5,max = 5)
  }
  return(a*x+b)
}

sample_linear<-  function(npoints, radius = 1,random_state=42,normalize=TRUE,noise = TRUE){
  set.seed(random_state)
  x = runif(npoints)
  y = linear(x)
  dataset = data.frame(list(x,y))
  colnames(dataset) = c('x','y')
  return(dataset)
} 

sample_spherical<-  function(npoints, radius = 1,random_state=42,normalize=TRUE,noise = TRUE){
  set.seed(random_state)
  theta = runif(1000, min = 0, max = 2*pi)
  x = radius * cos(theta) + runif(npoints,0,0.1)
  y = radius * sin(theta) + runif(npoints,0,0.1)
  z = tanh(sqrt(abs(x*y)))
  dataset = data.frame(list(x,y,z))
  colnames(dataset) = c('x_1','x_2','y')
  return(dataset)
}


sample_sinusoid<-function(npoints,random_state=42){
  set.seed(random_state)
  x = seq(0,4*pi,length.out = npoints)-runif(npoints,max = 0.1)
  set.seed(random_state-1)
  y = sin(x)+runif(npoints,max = 0.2)
  dataset = data.frame(list(x,y))
  colnames(dataset) = c('x','y')
  return(dataset)
}



sample_groups <- function(npoints,random_state = 42){
  set.seed(random_state)
  g1 = 0.8 * runif(npoints) + 15
  g2 = 5 * seq(0,1,length.out = npoints) -10 + 1.5*runif(npoints)
  g3 = 20 * sin(seq(0,1,length.out = npoints)) - 3.5*runif(npoints)
  x = c(g1,g2,g3)
  y = sin(seq(0,15,length.out = 3*npoints))
  z = ((x*y)>mean(x*y))*1
  dataset = data.frame(list(x,y,z))
  colnames(dataset) = c('x_1','x_2','y')
  return(dataset)
}

sample_multifeature <- function(npoints,n_features,noise_ratio = 0.8,correlated_features_ratio = 0.8,random_state = 42){
  set.seed(random_state)
  function_list = c(asin,acos,atan, atanh, log, sqrt, linear)
  n_correlated_features = round(n_features*correlated_features_ratio)
  output = sin(seq(0,4*pi,length.out = npoints)-runif(npoints,max = 0.1))
  
  values = vector(mode = "list", length = n_features+1)
  for(i in 1:n_correlated_features){
    sel_fn = function_list[[sample(1:6, 1)]]
    values[[i]] = sel_fn(output)
    values[[i]] = (values[[i]]-min(values[[i]]))/(max(values[[i]])-min(values[[i]])) + runif(npoints)*noise_ratio
  }
  for(i in ((n_correlated_features/2)+1):n_correlated_features){
    values[[i]] = linear(values[[i]] * values[[1+n_correlated_features-i]])
  }
  for(i in (n_correlated_features+1):n_features){
    sel_fn = function_list[[sample(1:6, 1)]]
    values[[i]] = sel_fn(runif(npoints))+runif(npoints)*noise_ratio
  }
  values[[length(values)]] = output
  dataset = data.frame(values)
  colnames(dataset) = c(paste0(rep('x_',length(n_features)),as.character(1:n_features)),'y')
  return(dataset)
}


sample_multifeature_cat <- function(npoints,n_features,noise_ratio = 0.8,correlated_features_ratio = 0.8,random_state = 42){
  set.seed(random_state)
  function_list = c(asin,acos,atan, atanh, log, sqrt, linear)
  n_correlated_features = round(n_features*correlated_features_ratio)
  output = abs(rbeta(n = npoints, shape1 = 5, shape2 =2.5) - runif(npoints))
  
  values = vector(mode = "list", length = n_features+1)
  for(i in 1:n_correlated_features){
    sel_fn = function_list[[sample(1:6, 1)]]
    values[[i]] = sel_fn(output)
    values[[i]] = (values[[i]]-min(values[[i]]))/(max(values[[i]])-min(values[[i]])) + runif(npoints)*noise_ratio
  }
  for(i in ((n_correlated_features/2)+1):n_correlated_features){
    values[[i]] = linear(values[[i]] * values[[1+n_correlated_features-i]])
  }
  for(i in (n_correlated_features+1):n_features){
    sel_fn = function_list[[sample(1:6, 1)]]
    values[[i]] = sel_fn(runif(npoints))+runif(npoints)*noise_ratio
  }
  
  output = cut(output, breaks = quantile(output), labels = c(0,1,2,3),include.lowest = T)
  
  values[[length(values)]] = output
  dataset = data.frame(values)
  colnames(dataset) = c(paste0(rep('x_',length(n_features)),as.character(1:n_features)),'y')
  return(dataset)
}