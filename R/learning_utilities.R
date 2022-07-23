library(caret)
library(glmnet)


cv_folds <- function(path) {
  kfold <- read.table(path, header=FALSE, fill=T, skip=0, stringsAsFactors = FALSE)

  folds <- vector(mode="list", length = 8)
  for (i in 1:4) {
    folds[[i]] <- na.omit(as.integer(kfold[2*i - 1,]) + 1L)
    folds[[i+4]] <- na.omit(as.integer(kfold[2*i,]) + 1L)
  }

  return(folds)
}

# The re-sampling method is superseded  when index and IndexOut are specified.
# However, the printout will claim that bootstrapping was used in re-sampling even though it was not used.
caret_pipeline <- function(dataset, tuning_index, tuning_indexOut, cv_index, cv_indexOut, model_name, modelGrid, seed) {

  # haberman-survival has only 3 predictors and mtry should be <= # of predictors
  if (model_name == "parRF" & ncol(dataset) == 4) {
    modelGrid <- data.frame(mtry=c(2, 3))
  }

  set.seed(seed)
  tuneFit <- train(as.factor(clase) ~ ., data = dataset,
                   method = model_name,
                   preProcess= c("center", "scale"),
                   trControl = trainControl(returnResamp = "all", index = tuning_index, indexOut = tuning_indexOut),
                   tuneGrid = modelGrid)

  cvFit <- train(as.factor(clase) ~ ., data = dataset,
                 method = model_name,
                 preProcess= c("center", "scale"),
                 trControl = trainControl(returnResamp = "all",index = cv_index, indexOut = cv_indexOut),
                 tuneGrid = tuneFit$bestTune)

  return(list(tuneFit, cvFit))
}


# Fit all the caret models
fit_caret_models <- function(datasets, N_datasets=4, tuning_indices, tuning_indicesOut, cv_indices, cv_indicesOut,
                             model_names, model_grids, N_models=4, seed) {

  fitted_models <- vector(mode = "list", length = N_datasets * N_models)
  fit_names <- vector(length = N_datasets * N_models)
  k <- 1
  for (i in 1:N_models) {
    for (j in 1:N_datasets) {
      fitted_models[[k]] <- caret_pipeline(dataset = datasets[[j]],
                                           tuning_index = tuning_indices[[j]],
                                           tuning_indexOut = tuning_indicesOut[[j]],
                                           cv_index = cv_indices[[j]],
                                           cv_indexOut = cv_indicesOut[[j]],
                                           model_name = model_names[i],
                                           modelGrid = model_grids[[i]],
                                           seed = seed)

      fit_names[k] <- paste(model_names[i], names(datasets)[j], sep=": ")

      print(paste0("Models fitted: ", k, "/", N_datasets * N_models))
      k <- k + 1
    }
  }

  names(fitted_models) <- fit_names
  return(fitted_models)
}


# Try to find good lambda sequence for caret parameter tuning
# by fitting glmnet models directly with all the different
# lambda values.
find_lambdas <- function(dataset, dataset_name, tuning_idx, alphas) {

  y <- as.factor(dataset$clase)
  x <- as.matrix(dataset[, -ncol(dataset)])

  min_lambdas <- vector(length = length(alphas))
  max_lambdas <- vector(length = length(alphas))
  for (i in 1:length(alphas)) {
    glmFit <- glmnet(x=x[tuning_idx, ], y=y[tuning_idx], family="binomial",
                     alpha=alphas[i], standardize = TRUE)
    min_lambdas[i] <- min(glmFit$lambda)
    max_lambdas[i] <- max(glmFit$lambda)
  }

  return(data.frame(Dataset=rep(dataset_name, length(alphas)), alpha=alphas, Min_lambda=min_lambdas, Max_lambda=max_lambdas))
}
