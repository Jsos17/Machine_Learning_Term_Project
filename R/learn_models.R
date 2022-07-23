library(caret)

source("R/learning_utilities.R")
source("R/check_datasets.R")


##################################################################
### Load data and tuning & cv indices
##################################################################

# Dataset order:
# 1) bc-wisc-prog
# 2) haberman-survival
# 3) parkinsons
# 4) spambase

paths_datasets <- c("data/breast-cancer-wisc-prog/breast-cancer-wisc-prog_R.dat", "data/haberman-survival/haberman-survival_R.dat",
                    "data/parkinsons/parkinsons_R.dat", "data/spambase/spambase_R.dat")

paths_conxuntos <- c("data/breast-cancer-wisc-prog/conxuntos.dat", "data/haberman-survival/conxuntos.dat",
                     "data/parkinsons/conxuntos.dat", "data/spambase/conxuntos.dat")

paths_conxuntos_kfold <- c("data/breast-cancer-wisc-prog/conxuntos_kfold.dat", "data/haberman-survival/conxuntos_kfold.dat",
                           "data/parkinsons/conxuntos_kfold.dat", "data/spambase/conxuntos_kfold.dat")

datasets <- vector(mode="list", length = 4)
names(datasets) <- c("bc-wisc-prog", "haberman-survival", "parkinsons", "spambase")
tuning_indices <- vector(mode="list", length = 4)
tuning_indicesOut <- vector(mode="list", length = 4)
cv_indices <- vector(mode="list", length = 4)
cv_indicesOut <- vector(mode="list", length = 4)

for (i in 1:4) {
  datasets[[i]] <- read.table(paths_datasets[i], header=TRUE, skip=0, stringsAsFactors = FALSE)

  conxuntos <- read.table(paths_conxuntos[i], header=FALSE, fill=T, skip=0, stringsAsFactors = FALSE)
  tuning_indices[[i]] <- list(as.integer(conxuntos[1, ]) + 1L)
  tuning_indicesOut[[i]] <- list(na.omit(as.integer(conxuntos[2, ]) + 1L))

  folds <- cv_folds(paths_conxuntos_kfold[i])
  cv_indices[[i]] <- folds[1:4]
  cv_indicesOut[[i]] <- folds[5:8]
}

##################################################################
### Check data and indices
##################################################################

tolerance <- 0.00001

check_data_and_indices <- function() {
  mutex_and_indices_OK(datasets, tuning_indices, tuning_indicesOut, cv_indices, cv_indicesOut)
  check_fold_lengths(datasets, tuning_indices, tuning_indicesOut, cv_indices, cv_indicesOut)
  check_approx_centered_scaled(datasets, tol=tolerance)
  no_NAs_in_datasets(datasets)
}

# check_data_and_indices()

##################################################################
### Gauge a good lambda sequence for glmnet_caret
##################################################################

lambdas_for_tuning <- function() {
  alphas <- c(0, 0.2, 0.5, 0.8, 1)
  print(find_lambdas(datasets[[1]], names(datasets)[1], tuning_indices[[1]][[1]], alphas))
  print(find_lambdas(datasets[[2]], names(datasets)[2], tuning_indices[[2]][[1]], alphas))
  print(find_lambdas(datasets[[3]], names(datasets)[3], tuning_indices[[3]][[1]], alphas))
  print(find_lambdas(datasets[[4]], names(datasets)[4], tuning_indices[[4]][[1]], alphas))
}

# lambdas_for_tuning()

##################################################################
### Models setup
##################################################################

# Model order:
# 1) avNNet
# 2) glmnet
# 3) parRf
# 4) svmPoly

model_names <- c("avNNet", "glmnet", "parRF", "svmPoly")
model_grids <- vector(mode="list", length = 4)

model_grids[[1]] <- expand.grid(size = c(1, 3, 5), decay = c(0, 0.1, 10^(-4)), bag = FALSE)
model_grids[[2]] <- expand.grid(alpha=c(0, 0.2, 0.5, 0.8, 1), lambda=exp(seq(log(1e-05), log(2), length.out = 100)))
model_grids[[3]] <- data.frame(mtry=seq(2, 8, by=2))
model_grids[[4]] <- expand.grid(degree=c(1, 2, 3), scale=c(0.001, 0.01, 0.1), C=c(0.25, 0.5, 1))

##################################################################
### Train all models
##################################################################

train_all <- function() {
  start_time <- Sys.time()
  fitted_models <- fit_caret_models(datasets = datasets, N_datasets = 4,
                                    tuning_indices = tuning_indices, tuning_indicesOut = tuning_indicesOut,
                                    cv_indices = cv_indices, cv_indicesOut = cv_indicesOut,
                                    model_names = model_names, model_grids = model_grids, N_models = 4, seed=1729)
  end_time <- Sys.time()
  end_time - start_time

  return(fitted_models)
}

# fitted_models <- train_all()
# save(fitted_models, file="RData/models.RData")
