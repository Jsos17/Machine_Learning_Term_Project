netInfo <- getModelInfo("avNNet")
netInfo$avNNet$parameters
parRFInfo <- getModelInfo("parRF")
parRFInfo$parRF$parameters
svmInfo <- getModelInfo("svmPoly")
svmInfo$svmPoly$parameters


haberman_surv <- read.table("data/haberman-survival/haberman-survival_R.dat", header=TRUE, skip=0, stringsAsFactors = FALSE)
apply(haberman_surv, MARGIN = 2, FUN = mean)
apply(haberman_surv, MARGIN = 2, FUN = sd)

haberman_tuning <- read.table("data/haberman-survival/conxuntos.dat", header=FALSE, fill=T, skip=0, stringsAsFactors = FALSE)
haberman_tuning_index <- list(as.integer(haberman_tuning[1, ]))
haberman_tuning_indexOut <- list(as.integer(haberman_tuning[2, ]))

haberman_kfold <- read.table("data/haberman-survival/conxuntos_kfold.dat", header=FALSE, fill=T, skip=0, stringsAsFactors = FALSE)
haberman_folds <- train_val_folds(haberman_kfold)
haberman_cv_index <- haberman_folds[1:4]
haberman_cv_indexOut <- haberman_folds[5:8]

haberman_tuning_ctrl <- tuning_ctrl(tuning_index=haberman_tuning_index, tuning_indexOut=haberman_tuning_indexOut)
haberman_cv_ctrl <- cv_ctrl(cv_index=haberman_cv_index, cv_indexOut=haberman_cv_indexOut)

# readLines("data/haberman-survival/haberman-survival_R.dat", n=10)

# View(haberman_kfold)

# as.numeric(sort(haberman_kfold[2,]))
# intersect(as.numeric(haberman_kfold[1,]), as.numeric(haberman_kfold[2,]))
# intersect(haberman_kfold[3,], haberman_kfold[4,])
# intersect(haberman_kfold[5,], haberman_kfold[6,])
# intersect(haberman_kfold[7,], haberman_kfold[8,])
#
# View(haberman_tuning)

sort(intersect(as.numeric(haberman_kfold[2,1:76]), as.numeric(haberman_kfold[4,1:76])))
sort(intersect(as.numeric(haberman_kfold[2,1:76]), as.numeric(haberman_kfold[6,1:76])))
sort(intersect(as.numeric(haberman_kfold[2,1:76]), as.numeric(haberman_kfold[8,1:76])))
sort(intersect(as.numeric(haberman_kfold[4,1:76]), as.numeric(haberman_kfold[6,1:76])))
sort(intersect(as.numeric(haberman_kfold[4,1:76]), as.numeric(haberman_kfold[8,1:76])))
sort(intersect(as.numeric(haberman_kfold[6,1:76]), as.numeric(haberman_kfold[8,1:76])))

as.numeric(sort(haberman_kfold[2,]))
as.numeric(sort(haberman_kfold[4,]))
as.numeric(sort(haberman_kfold[6,]))
as.numeric(sort(haberman_kfold[8,]))

# readLines("data/breast-cancer-wisc-prog/breast-cancer-wisc-prog_R.dat", n=10)
bc_wisc_prog <- read.table("data/breast-cancer-wisc-prog/breast-cancer-wisc-prog_R.dat", header=TRUE, skip=0)
apply(bc_wisc_prog, MARGIN = 2, FUN = mean)
apply(bc_wisc_prog, MARGIN = 2, FUN = sd)

# readLines("data/spambase/spambase_R.dat", n=10)
spam <- read.table("data/spambase/spambase_R.dat", header=TRUE, skip=0)
spam_kfold <- read.table("data/spambase/conxuntos_kfold.dat", header=FALSE, fill=T, skip=0)

str(spam$f1)

sort(intersect(as.numeric(spam_kfold[2,1:1150]), as.numeric(spam_kfold[4,1:1150])))
sort(intersect(as.numeric(spam_kfold[2,1:1150]), as.numeric(spam_kfold[6,1:1150])))
sort(intersect(as.numeric(spam_kfold[2,1:1150]), as.numeric(spam_kfold[8,1:1150])))
sort(intersect(as.numeric(spam_kfold[4,1:1150]), as.numeric(spam_kfold[6,1:1150])))
sort(intersect(as.numeric(spam_kfold[4,1:1150]), as.numeric(spam_kfold[8,1:1150])))
sort(intersect(as.numeric(spam_kfold[6,1:1150]), as.numeric(spam_kfold[8,1:1150])))

# readLines("data/spectf/spectf_train_R.dat", n=10)
spectf_train <- read.table("data/spectf/spectf_train_R.dat", header=TRUE, skip=0)


haberman_cv_ctrl <- trainControl(method = "boot", index = haberman_cv_index, indexOut = haberman_cv_indexOut)


getModelInfo("avNNet")$avNNet$parameters
getModelInfo("glmnet")$glmnet$parameters
getModelInfo("parRF")$parRF$parameters
getModelInfo("svmPoly")$svmPoly$parameters

modelLookup("avNNet")
modelLookup("glmnet")
modelLookup("parRF")
modelLookup("svmPoly")

set.seed(8)
# avNNetFit <- train(as.factor(clase) ~ ., data = haberman_surv,
#                    method = "avNNet",
#                    preProcess= c("center", "scale"),
#                    trControl = haberman_tuning_ctrl,
#                    tuneGrid = avNNetGrid)
#
# avNNetFit2 <- train(as.factor(clase) ~ ., data = haberman_surv,
#                     method = "avNNet",
#                     preProcess= c("center", "scale"),
#                     trControl = haberman_cv_ctrl,
#                     tuneGrid = avNNetFit$bestTune)

svmPolyFit <- train(as.factor(clase) ~ ., data = haberman_surv,
                    method = "svmPoly",
                    offset = 1,
                    preProcess= c("center", "scale"),
                    trControl = haberman_tuning_ctrl,
                    tuneGrid = svmPolyGrid)

svmPolyFit2 <- train(as.factor(clase) ~ ., data = haberman_surv,
                     method = "svmPoly",
                     offset = 1,
                     preProcess= c("center", "scale"),
                     trControl = haberman_cv_ctrl,
                     tuneGrid = svmPolyFit$bestTune)

svmPolyFit
svmPolyFit2


lst <- vector(mode="list", length = 4)
n <- c("a", "b", "c", "d")
for (i in 1:4) {
  # lst[n[i]] <- i
  lst[[i]] <- i
}

lst

alphas <- c(0.5, 1)

for (a in alphas) {
  print(a)
}

library(caret)

## Not run:
load(url("http://topepo.github.io/caret/exampleModels.RData"))

resamps <- resamples(list(CART = rpartFit,
                          CondInfTree = ctreeFit,
                          MARS = earthFit))

dotplot(resamps,
        scales =list(x = list(relation = "free")),
        between = list(x = 2))

bwplot(resamps,
       metric = "RMSE")

densityplot(resamps,
            auto.key = list(columns = 3),
            pch = "|")

xyplot(resamps,
       models = c("CART", "MARS"),
       metric = "RMSE")

splom(resamps, metric = "RMSE")
splom(resamps, variables = "metrics")

parallelplot(resamps, metric = "RMSE")

for (k in 1:16) {
  print(paste0("Models fitted: ", k, "/", 16))
}

progBar <- "="
for (k in 1:16) {

  print(paste0("Progress: ", progBar, (k/16)*100, "%"))
  progBar <- paste0(progBar, "=")
}

glmnet_fits <- caret_pipeline(dataset = spambase,
                              tuning_index = spambase_tuning_index,
                              tuning_indexOut = spambase_tuning_indexOut,
                              cv_index = spambase_cv_index,
                              cv_indexOut = spambase_cv_indexOut,
                              model_name = "glmnet",
                              modelGrid = NULL,
                              seed=1729)

glmnetFit <- train(as.factor(clase) ~ ., data = spambase,
                   method = "glmnet",
                   preProcess= c("center", "scale"),
                   trControl = trainControl(method = "LGOCV", p=0.5, returnResamp = "all",
                                            index = spambase_tuning_index, indexOut = spambase_tuning_indexOut),
                   tuneLength = 20)

View(glmnetFit$resample)

tuning_idx <- tuning_indices[[4]][[1]]
tuning_idxOut <- tuning_indicesOut[[4]][[1]]
y <- as.factor(spambase$clase)
x <- as.matrix(spambase[, -ncol(spambase)])

glmFit2 <- glmnet(x=x[tuning_idx, ], y=y[tuning_idx], family="binomial", alpha=0, standardize = TRUE)
sort(glmFit2$lambda)


idx <- tuning_indices[[1]][[1]]
idxOut <- tuning_indicesOut[[1]][[1]]

print(paste(names(datasets)[i], "Tuning", sep=": "))
cat("MUTEX:          ", test_mutual_exclusivity(idx, idxOut), "\n")
cat("Correct indices:", all.equal(sort(c(idx, idxOut)), 1:nrow(datasets[[1]])), "\n")

equal_acc <- function(Acc) {
  rows <- c()
  colReplace <- c()
  colReplaceWith <- c()

  for (i in 1:3) {
    mask <- as.matrix((Acc[,i] == Acc[, i:4])[, -1])
    res <- which(mask == TRUE, arr.ind = TRUE)

    rows <- c(rows, res[, 1])
    colReplace <- c(colReplace, res[, 2] + i)
    colReplaceWith <- c(colReplaceWith, rep(i, length(res[, 1])))
  }

  data.frame(Row=rows, Replace=colReplace, ReplceWith=colReplaceWith)
}

equal_ranks <- function(ranks, rows, replace_col, replace_with_col) {

  if (length(rows) > 0) {
    for (i in 1:length(rows)) {
      ranks[rows[i], replace_col[i]] <- ranks[rows[i], replace_with_col]
    }
  }

  return(ranks)
}

equal_acc(Accuracies)

# readLines("data/breast-cancer-wisc-prog/breast-cancer-wisc-prog_R.dat", n=10)
bc_wisc_prog <- datasets[[1]]
bc_wisc_prog_tuning_index <- tuning_indices[[1]]
bc_wisc_prog_tuning_indexOut <- tuning_indicesOut[[1]]
bc_wisc_prog_cv_index <- cv_indices[[1]]
bc_wisc_prog_cv_indexOut <- cv_indicesOut[[1]]

haberman_surv <- datasets[[2]]
haberman_tuning_index <- tuning_indices[[2]]
haberman_tuning_indexOut <- tuning_indicesOut[[2]]

haberman_cv_index <- cv_indices[[2]]
haberman_cv_indexOut <- cv_indicesOut[[2]]

# readLines("data/parkinsons/parkinsons_R.dat", n=10)
parkinsons <- datasets[[3]]
parkinsons_tuning_index <- tuning_indices[[3]]
parkinsons_tuning_indexOut <- tuning_indicesOut[[3]]

parkinsons_cv_index <- cv_indices[[3]]
parkinsons_cv_indexOut <- cv_indicesOut[[3]]

# readLines("data/spambase/spambase_R.dat", n=10)
spambase <- datasets[[4]]
spambase_tuning_index <- tuning_indices[[4]]
spambase_tuning_indexOut <- tuning_indicesOut[[4]]

spambase_cv_index <- cv_indices[[4]]
spambase_cv_indexOut <- cv_indicesOut[[4]]

# View(bc_wisc_prog)
# View(haberman_surv)
# View(parkinsons)
# View(spam)

par(mfrow=c(4, 4))
plot(fitted_models[[1]][[1]])
plot(fitted_models[[2]][[1]])
plot(fitted_models[[3]][[1]])
plot(fitted_models[[4]][[1]])
plot(fitted_models[[5]][[1]])
plot(fitted_models[[6]][[1]])
plot(fitted_models[[7]][[1]])
plot(fitted_models[[8]][[1]])
plot(fitted_models[[9]][[1]])
plot(fitted_models[[10]][[1]])
plot(fitted_models[[11]][[1]])
plot(fitted_models[[12]][[1]])
plot(fitted_models[[13]][[1]])
plot(fitted_models[[14]][[1]])
plot(fitted_models[[15]][[1]])
plot(fitted_models[[16]][[1]])

plots <- vector(mode="list", length = 16)
for (k in 1:16) {
  # print(ggplot(fitted_models[[k]][[1]]) +
  #   theme_bw())
  plots[[k]] <- plot(fitted_models[[k]][[1]])
}

grid.arrange(grobs=plots, as.table=FALSE)



ggplot(fitted_models[[1]][[1]], plotType = "level", nameInStrip = TRUE, highlight = TRUE) +
  theme_bw() +
  ylab("Accuracy")

# tuning_ctrl <- trainControl(method = "LGOCV", p = 0.5, index = tuning_index, indexOut = tuning_indexOut)

# Compare with method = none and predict on test set


for (k in 1:16) {
  cat(fitted_models[[k]][[2]]$method,
      round(100 * fitted_models[[k]][[2]]$results$Accuracy, 1), "\n")
}

train_model <- function() {
  start_time <- Sys.time()
  models <- fit_caret_models(datasets = datasets, N_datasets = 4,
                             tuning_indices = tuning_indices, tuning_indicesOut = tuning_indicesOut,
                             cv_indices = cv_indices, cv_indicesOut = cv_indicesOut,
                             model_names = c("glmnet"), model_grids = list(expand.grid(alpha=c(0, 0.2, 0.5, 0.8, 1), lambda=exp(seq(log(1e-05), log(2), length.out = 100)))),
                             N_models = 1, seed=1729)
  end_time <- Sys.time()
  end_time - start_time

  return(models)
}

parRFs <- train_model()
avNNets <- train_model()
svmPolys <- train_model()
glmnets <- train_model()
