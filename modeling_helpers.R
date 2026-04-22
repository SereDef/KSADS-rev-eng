# Install once if needed:
# install.packages(c("rpart", "rpart.plot"))

library(rpart)
library(rpart.plot)

run_cart <- function(train_data,
                     outcome,
                     predictors,
                     test_data = NULL,
                     cp = 0.01,
                     minsplit = 20,
                     maxdepth = 30,
                     xval = 10,
                     method = NULL,
                     print_rules = TRUE,
                     plot_tree = FALSE) {
  
  # Basic checks
  stopifnot(is.data.frame(train_data))
  stopifnot(outcome %in% names(train_data))
  stopifnot(all(predictors %in% names(train_data)))
  
  if (!is.null(test_data)) {
    stopifnot(is.data.frame(test_data))
    stopifnot(outcome %in% names(test_data))
    stopifnot(all(predictors %in% names(test_data)))
  }
  
  # Keep only needed columns and remove missing rows
  train_df <- train_data[, c(outcome, predictors), drop = FALSE]
  train_df <- train_df[complete.cases(train_df), , drop = FALSE]
  
  if (!is.null(test_data)) {
    test_df <- test_data[, c(outcome, predictors), drop = FALSE]
    test_df <- test_df[complete.cases(test_df), , drop = FALSE]
  } else {
    test_df <- NULL
  }
  
  # Decide whether this is classification or regression
  y <- train_df[[outcome]]
  
  if (is.null(method)) {
    method <- if (is.factor(y) || is.character(y) || is.logical(y)) "class" else "anova"
  }
  
  if (method == "class") {
    train_df[[outcome]] <- as.factor(train_df[[outcome]])
    if (!is.null(test_df)) {
      test_df[[outcome]] <- factor(test_df[[outcome]], levels = levels(train_df[[outcome]]))
    }
  }
  
  # Build formula
  fml <- as.formula(
    paste(outcome, "~", paste(predictors, collapse = " + "))
  )
  
  # Fit CART model
  fit <- rpart(
    formula = fml,
    data = train_df,
    method = method,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    control = rpart.control(
      cp = cp,
      minsplit = minsplit,
      maxdepth = maxdepth,
      xval = xval
    )
  )

  # Extract rules as a data frame, round probabilities to class labels
  rules_df   <- rpart.rules(fit, roundint = FALSE, nn = TRUE)
  class_cols <- grep("^symptom", names(rules_df), value = TRUE)

  # Find the probability column (numeric), convert to 0/1 class label
  prob_col <- which(sapply(rules_df, is.numeric))
  rules_df[[prob_col]] <- ifelse(rules_df[[prob_col]] >= 0.5, 1, 0)

  rules_text <- capture.output(print(rules_df, row.names = FALSE))

  if (print_rules) {
    cat("\nLearned decision rules:\n")
    cat(paste(rules_text, collapse = "\n"))
    cat("\n")
  }
    
  # # Capture decision rules as text
  # rules_text <- capture.output(
  #   rpart.rules(fit, roundint = FALSE)
  # )
  
  # if (print_rules) {
  #   cat("\nLearned decision rules:\n")
  #   cat(paste(rules_text, collapse = "\n"))
  #   cat("\n")
  # }
  
  if (plot_tree) {
    rpart.plot(fit, roundint = FALSE, extra = 101)
  }
  
  # Variable importance
  var_importance <- fit$variable.importance
  if (is.null(var_importance)) {
    var_importance <- numeric(0)
  }
  
  # Performance on train
  performance <- list()
  
  if (method == "class") {
    train_pred_class <- predict(fit, newdata = train_df, type = "class")
    train_pred_prob  <- predict(fit, newdata = train_df, type = "prob")
    train_true <- train_df[[outcome]]
    
    train_accuracy <- mean(train_pred_class == train_true)
    train_confusion <- table(predicted = train_pred_class, observed = train_true)
    
    performance$train <- list(
      accuracy = train_accuracy,
      confusion_matrix = train_confusion,
      predicted_class = train_pred_class,
      predicted_prob = train_pred_prob
    )
    
    if (!is.null(test_df)) {
      test_pred_class <- predict(fit, newdata = test_df, type = "class")
      test_pred_prob  <- predict(fit, newdata = test_df, type = "prob")
      test_true <- test_df[[outcome]]
      
      test_accuracy <- mean(test_pred_class == test_true)
      test_confusion <- table(predicted = test_pred_class, observed = test_true)
      
      performance$test <- list(
        accuracy = test_accuracy,
        confusion_matrix = test_confusion,
        predicted_class = test_pred_class,
        predicted_prob = test_pred_prob
      )
    }
    
  } else if (method == "anova") {
    train_pred <- predict(fit, newdata = train_df)
    train_true <- train_df[[outcome]]
    
    train_rmse <- sqrt(mean((train_true - train_pred)^2))
    train_mae  <- mean(abs(train_true - train_pred))
    train_r2   <- 1 - sum((train_true - train_pred)^2) / sum((train_true - mean(train_true))^2)
    
    performance$train <- list(
      RMSE = train_rmse,
      MAE = train_mae,
      R2 = train_r2,
      predicted = train_pred
    )
    
    if (!is.null(test_df)) {
      test_pred <- predict(fit, newdata = test_df)
      test_true <- test_df[[outcome]]
      
      test_rmse <- sqrt(mean((test_true - test_pred)^2))
      test_mae  <- mean(abs(test_true - test_pred))
      test_r2   <- 1 - sum((test_true - test_pred)^2) / sum((test_true - mean(test_true))^2)
      
      performance$test <- list(
        RMSE = test_rmse,
        MAE = test_mae,
        R2 = test_r2,
        predicted = test_pred
      )
    }
  } else {
    stop("This helper currently supports method = 'class' or method = 'anova'.")
  }
  
  return(list(
    model = fit,
    formula = fml,
    method = method,
    rules = rules_text,
    variable_importance = var_importance,
    performance = performance
  ))
}
