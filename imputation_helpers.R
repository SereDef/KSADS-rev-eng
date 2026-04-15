summarise_item <- function(x) {
  
  if (inherits(x, "Date")) {
    n = sum(!is.na(x))
    n_na = sum(is.na(x))
    
    if (n == 0) {
      list(
        n = 0L,
        n_na = n_na,
        min = as.Date(NA),
        max = as.Date(NA))
      
    } else {
      list(
        'n (observed)' = n,
        'NA' = n_na,
        min = as.character(as.Date(min(x, na.rm = TRUE))),
        max = as.character(as.Date(max(x, na.rm = TRUE))))
    }
    
  } else if (is.factor(x)) {

    tab <- table(x, useNA = "always")
    
    levels <- names(tab)
    levels[is.na(levels)] <- 'NA'
    
    list(
      n = sum(!is.na(x)),
      levels = levels,
      counts = as.integer(tab)
    )
    
  } else if (is.numeric(x)) {
    
    n = sum(!is.na(x))
    n_na = sum(is.na(x))
    
    list(
        'n (observed)' = n,
        'NA' = n_na,
        mean = round(mean(x, na.rm = TRUE), 2),
        median = stats::median(x, na.rm = TRUE),
        min = ifelse(n == 0, NA, min(x, na.rm = TRUE)),
        max = ifelse(n == 0, NA, max(x, na.rm = TRUE)))
    
  } else {
    stop('Expected a Date, factor or numeric.')
  }
}

summarise_item_table <- function(x_comp, x_cast) {
  s1 <- summarise_item(x_comp)
  s2 <- summarise_item(x_cast)
  
  if ('levels' %in% names(s1)) { # infer factor
    
    all_levels <- union(s1$levels, s2$levels)
    
    comp_counts  <- setNames(s1$counts, s1$levels)
    cast_counts  <- setNames(s2$counts, s2$levels)
    
    tab <- data.frame(
      COMP   = c(s1$n, as.integer(comp_counts[all_levels])),
      Castor = c(s2$n, as.integer(cast_counts[all_levels])))
    
    rownames(tab) <- c("n (observed)", all_levels)
    
  } else {
    tab <- cbind(s1, s2)
    colnames(tab) <- c('COMP','Castor')
  }
  
  tab
}

compare_uniques <- function(x_comp, x_cast) {
  
  u_comp <- sort(unique(x_comp))
  u_cast <- sort(unique(x_cast))
  
  
  # levels observed in Castor but not COMP → problem
  extra_in_cast <- setdiff(u_cast, u_comp)  
  # in COMP but not Castor → less critical
  extra_in_comp <- setdiff(u_comp, u_cast)  
  
  if (length(extra_in_cast) > 0) {
    flag <- 'yes'      # Castor has levels COMP never saw → needs attention
  } else if (length(extra_in_comp) > 0) {
    flag <- 'maybe'    # COMP has levels Castor never saw → less critical
  } else {
    flag <- 'no'
  }
  
  # Old version: There
  # problem <- !identical(u_comp, u_cast)
  # 
  # if ((is.numeric(x_comp) || inherits(x_comp, "Date")) & problem){
  #   flag <- 'maybe'
  # } else if (problem) { flag <- 'yes'
  # } else { flag <- 'no' }
  
  flag
}

check_vars_included <- function(data, items, symptoms, diagnoses) {
  
  if ((length(items) + length(symptoms) + length(diagnoses)) != ncol(data)-1) {
    print(length(items))
    print(length(symptoms))
    print(length(diagnoses))
    print(ncol(data))
  }
  
  return(invisible(NULL))
}

numericize <- function(data, to_factor = TRUE) {
  
  labs <- lapply(data, attr, 'label')
  
  data[] <- lapply(seq_along(data), function(i) {
    c <- data[[i]]
    
    nc <- if(haven::is.labelled(c)) {
      if (to_factor) as.factor(as.character(as.numeric(c))) else as.numeric(c) 
    } else {c}
    
    attr(nc, 'label') <- labs[[i]]
    return(nc)
  })
  
  return(data)
}

force_binary <- function(data, vars) {
  
  for (v in vars) {
    
    lab <- attr(data[[v]], 'label')
    
    data[, v] <- as.factor(ifelse(data[,v] == 0, 'No', 'Yes'))
    
    attr(data[[v]], 'label') <-lab
    
  }
  
  return(data)
}

is_constant <- function(x) {
  
  if ((is.factor(x) && nlevels(droplevels(x)) == 1) | 
      (is.numeric(x) && length(unique(x)) == 1)) return(TRUE)
  
  return(FALSE)
}

get_rules <- function(outcome, predictors, data){
  
  # Initialize output object 
  out <- list()
  
  out$outcome <- outcome
  out$outcome_label <- attr(data[[outcome]], 'label')
  
  message('\n\n', outcome, ':\t', out$outcome_label)
  
  # TMP: constant outcomes cannot be predicted, return empty 
  if (is_constant(data[[outcome]])) {
    out$predictors <- ''
    out$predictor_label <- ''
    out$rules <- data.frame(outcome = outcome, outcome_value = NA, 
                            cover = 0, rule = NA)
    out$fit <- list()
    out$total_cover <- NA
    out$method <- NA
    message('\tVariable is constant. Returning NULL.')
    return(out)
  }
  
  # Fit the CART model ---------------------------------------------------------
  
  data_subset <- cbind(data[predictors], data[, outcome])
  
  fit <- rpart::rpart(as.formula(paste(outcome, '~ .')), data = data_subset, 
                      # Let him guess the method ("class" or "anova") for now
                      # Overfit this baby
                      control = rpart::rpart.control( 
                        minsplit = 2, minbucket = 1, # no minimum number of obs
                        cp = 0.001, # minimum improvement of fit required
                        xval = 0)) # no cross-validation
  
  # Extract and preprocess rules -----------------------------------------------
  
  rules <- rpart.plot::rpart.rules(fit, cover = TRUE, nn = TRUE, facsep=" // ", 
                                   when = '')
  
  # Combine (variable number of) rules into one column
  rules$rule <- apply(rules[, names(rules) == '', drop = FALSE], 1, 
                      paste, collapse = ' ')
  rules[names(rules) == ""] <- NULL
  
  # Clean up column name
  names(rules) <- c('outcome', 'outcome_value', 'cover', 'rule')
  rules$outcome <- outcome
  
  # Add more info the the output object ----------------------------------------
  
  out$predictors <- unique(fit$frame$var[fit$frame$var != '<leaf>'])
  
  out$predictor_label <- vapply(out$predictors,
                                function(p) attr(data[[p]], 'label'),
                                character(1))
  out$rules <- rules
  
  out$fit <- fit # For prediction
  
  # Total coverage (for checking)
  out$total_cover <- sum(as.numeric(gsub('%','', rules$cover)))
  
  # Method picked by the algorithm (for checking)
  out$method <- fit$method
  
  # Print out report
  if (out$method == 'class') {
    cat('  Levels: ', levels(data[[outcome]])) 
  } else {
    cat('  Range: ', paste(range(data[[outcome]], na.rm=TRUE), collapse = ' - '))
  }
  
  cat('\n  Total coverage: ', out$total_cover, '%')
  cat('\n  Predictors:\n    ', paste(out$predictors, out$predictor_label, 
                                     collapse = '\n   - ', sep = ': '), sep ='')
  
  return(out)
}