
mutex_and_indices_OK <- function(datasets, tuning_indices, tuning_indicesOut,
                                 cv_indices, cv_indicesOut) {

  test_mutual_exclusivity <- function(train_fold, val_fold) {
    common <- intersect(train_fold, val_fold)

    return(0 == length(common))
  }

  for (i in 1:length(tuning_indices)) {
    idx <- tuning_indices[[i]][[1]]
    idxOut <- tuning_indicesOut[[i]][[1]]

    print(paste(names(datasets)[i], "Tuning", sep=": "))
    cat("MUTEX:          ", test_mutual_exclusivity(idx, idxOut), "\n")
    cat("Correct indices:", all.equal(sort(c(idx, idxOut)), 1:nrow(datasets[[i]])), "\n")
  }

  for (i in 1:length(cv_indices)) {
    index <- cv_indices[[i]]
    indexOut <- cv_indicesOut[[i]]

    print(paste(names(datasets)[i], "CV", sep=": "))
    mutex_status <- vector(length = length(index))
    idx_status <- vector(length = length(index))
    for (j in 1:length(index)) {
      mutex_status[j] <- test_mutual_exclusivity(index[[j]], indexOut[[j]])
      idx_status[j] <- all.equal(sort(c(index[[j]], indexOut[[j]])), 1:nrow(datasets[[i]]))
    }

    cat("MUTEX:   ", mutex_status, "\n")
    cat("Index OK:", idx_status, "\n")
  }
}


check_fold_lengths <- function(datasets, tuning_indices, tuning_indicesOut,
                               cv_indices, cv_indicesOut) {

  for (i in 1:length(tuning_indices)) {
    idx <- tuning_indices[[i]][[1]]
    idxOut <- tuning_indicesOut[[i]][[1]]

    print(paste(names(datasets)[i], "Tuning", sep=": "))
    cat("Idx len:   ", length(idx), "\n")
    cat("IdxOut len:", length(idxOut), "\n")
  }

  for (i in 1:length(cv_indices)) {
    index <- cv_indices[[i]]
    indexOut <- cv_indicesOut[[i]]

    print(paste(names(datasets)[i], "CV", sep=": "))
    index_lengths <- vector(length = length(index))
    indexOut_lengths <- vector(length = length(index))
    for (j in 1:length(index)) {
      index_lengths[j] <- length(index[[j]])
      indexOut_lengths[j] <- length(indexOut[[j]])
    }

    cat("index lengths:   ", index_lengths, "\n")
    cat("indexOut lengths:", indexOut_lengths, "\n")
  }
}


check_approx_centered_scaled <-  function(datasets, tol=0.00001) {

  approx_centered <- function(dataset, tol=0.00001) {
    means <- apply(dataset, MARGIN = 2, FUN = mean)
    d <- ncol(dataset)

    all.equal(rep(0, d - 1), means[-d], tolerance=tol, check.names=FALSE)
  }


  approx_scaled <- function(dataset, tol=0.00001) {
    sds <- apply(dataset, MARGIN = 2, FUN = sd)
    d <- dim(dataset)[2]

    all.equal(rep(1, d - 1), sds[-d], tolerance=tol, check.names=FALSE)
  }

  for (i in 1:length(datasets)) {
    d <- datasets[[i]]

    cat(names(datasets)[i], ", approx (centered, scaled): ", approx_centered(d, tol=tol), " ", approx_scaled(d, tol=tol), "\n", sep="")
  }
}


no_NAs_in_datasets <- function(datasets) {

  for (i in 1:length(datasets)) {
    cat(names(datasets)[i], ", no NAs: ", 0 == sum(is.na(datasets[[i]])), "\n", sep="")
  }
}
