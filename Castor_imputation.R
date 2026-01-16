# ==============================================================================
#                       --- CASTOR data imputation ---
# ==============================================================================

# ~ Set up ~ -------------------------------------------------------------------

# devtools::install_github('SereDef/GenR.helpR')
require('genR.helpR')

require(haven)
require(rpart)
require(rpart.plot)

data_dir <- "\\\\store/department/genr/isi-store/Behaviour_and_Cognition/Focus_op_17/KSADS/KSADS_DataF17/Data cleaning KSADS_F17/Merging COMP and Castor datasets/Serena"

# list.files(data_dir)

# ~ Read ~ ---------------------------------------------------------------------

# Read data (one module at the time)
# TODO: make sure module names are consistent

comput <- file.path(data_dir, "Merge_items_diagnoses_COMP_ED module.sav") |> 
  haven::read_sav()

castor <- file.path(data_dir, "Castor_dataset_only_cleaned_ED module.sav") |>
  haven::read_sav()
  
# ~ Explore & clean ~ ----------------------------------------------------------

names(castor)
names(comput)

# Variable overlap:
# - in castor not in computerized version 
setdiff(names(castor), names(comput))
# - in computerized version not in castor
setdiff(names(comput), names(castor))

# TODO: make sure the IDs 
# - have the same name in both datasets 
# - have no duplicates 
# - Are either numeric or character vectors (no SPSS bs)
# - (optional) there is no overlap in people between datasets

# More than one id variable
# nrow(castor[castor$participant_id != castor$R_number_participant_1, ])
# castor$R_number_participant_1 <- NULL # remove extra id
 
# Duplicate IDs
# sum(duplicated(castor$participant_id))
# Remove one duplicate
# dup_id <- castor$participant_id[duplicated(castor$participant_id)]

# View(castor[castor$participant_id == dup_id, ])
# castor <- castor[!duplicated(castor$participant_id), ]

# View(castor[castor$participant_id == dup_id, ])

# summary(castor)

comput$participant_id <- as.character(comput$KSADS_Child17_ID_cleaned)

# TODO: "Extra" variables (e.g. date interview) remove for now..?

comput$KSADS_Child17_ID_cleaned <- NULL
comput$KSADS_Child17_DateofInterview_year_cleaned <- NULL
comput$KSADS_Child17_DateofInterview_month_cleaned <- NULL

# summary(comput)

# TODO: All NAs are now interpreted as "meaningful"
# TODO: check Constant 5 NA??? in symptoms
# Check there are no empty rows
# any(rowSums(is.na(comput)) == ncol(comput))

# data <- merge(castor, comput, all = TRUE)

# Check merging 
# waldo::compare(data[data$participant_id %in% castor$participant_id, ], castor)

# TODO: ensure there are no "labelled" and other SPSS bs in the dataset 
# factors are factors and numbers are numeric!

# data <- clean_spss_bs(comput) # TODO: eh.. some labels are used as notes??

data <- factorize(comput, min_levels = 3) # consider only yes / no

# Ok porca troia: i cannot even 
labs <- lapply(data, attr, 'label')

data[] <- lapply(seq_along(data), function(i) {
  c <- data[[i]]
  
  nc <- if(haven::is.labelled(c)) as.numeric(c) else c
  
  attr(nc, 'label') <- labs[[i]]
  return(nc)
})

data_overview(data)

summary(data)

# TODO: ensure labels used are correct!
# Some "labelled" have too many levels! what do to... TMP: do not compute for now

# KSADS_Child17_S241 is still weird omg
# TMP: MANUAL FIXES ~~~~~~~~~~~~~~~~~
# problem_symptoms <- paste0('KSADS_Child17_S2', c(26, 27, 36, 37, 38, 39, 40, 41, 42))
# 
# for (s in problem_symptoms) {
#   data[, s] <- as.factor(ifelse(data[,s] == 0, 'No', 'Yes'))
#   
# }

#summary(data)

# ~ Prediction ~ ---------------------------------------------------------------

items <- grep('KSADS_Child17_[0-9+]', names(data), value = TRUE)
symptoms <- grep('KSADS_Child17_S[0-9+]', names(data), value = TRUE)
diagnoses <- grep('KSADS_Child17_D[0-9+]', names(data), value = TRUE)

# Making no variables are missing
if ((length(items) + length(symptoms) + length(diagnoses)) != ncol(data)-1) {
  print(length(items))
  print(length(symptoms))
  print(length(diagnoses))
  print(ncol(data))
}

# ==============================================================================
# ~ CART (Classification and Regression Trees) ~
# ==============================================================================

# https://mdsr-book.github.io/mdsr3e/11-learningI.html

get_rules <- function(outcome, predictors, data){
  
  message(outcome)
  
  # if (is.factor(data[, outcome]) && nlevels(data[, outcome]) > 10) {
  #   cat(' ! too many levels, skipping.')
  #   empty_df <- 
  #   return(NULL)
  # }
  
  data_subset <- cbind(data[predictors], data[, outcome])
  
  fit <- rpart::rpart(as.formula(paste(outcome, '~ .')), data = data_subset)
    # method = "class", # let him guess
    # control = rpart.control(cp = 0.01, minbucket = 20)) 
    # TODO: Tune cp, maxdepth, minbucket to balance fidelity vs simplicity
  
  rules <- rpart.plot::rpart.rules(fit, cover = TRUE, nn = TRUE, facsep=" // ")
  names(rules) <- c('outcome', 'outcome_value', 'when', 
                    'predictor', 'op', 'predictor_value', 'cover')
  rules$outcome <- outcome
  
  rules$outcome_label <- attr(data_subset[,outcome], 'label')
  rules$predictor_label <- vapply(rules$predictor, 
                                  function(p) attr(data_subset[, p], 'label'), 
                                  character(1))
  
  rules$tot_cover <- sum(as.numeric(gsub('%','', rules$cover)))
  
  return(rules)
}

symptoms_rules <- do.call(rbind, lapply(symptoms, function(s) {
  #TODO: TMP!! ensure only actual factors are factors!
  
  get_rules(outcome=s, predictors=items, data=data)
}))

outcome = symptoms[3]
r = get_rules(outcome=s, predictors=items, data=data)
# ==============================================================================
# regularized GLMs (LASSO)
# ==============================================================================

# library(glmnet)

# X: matrix/data.frame of symptoms, coded 0-2
# y: numeric diagnosis score

# X <- as.matrix(data[items])       # n x p symptoms
# y <- data[, y]
# 
# # Handle missing values in X!
# cvfit <- cv.glmnet(X, y, family = "binomial", alpha = 1)  # LASSO
# 
# cvfit <- cv.glmnet(X, y, family = "gaussian", alpha = 1)  # LASSO
# coef(cvfit, s = "lambda.1se")

# ==============================================================================
# IMPUTATION 
# ==============================================================================
# require(mice)
# require(ranger)
# 
# raw_questions <- grep('Q', names(data), value=TRUE)
# 
# pred = make.predictorMatrix(data)
# # pred[c('participant_id', raw_questions), ] <- 0
# 
# meth <- make.method(data, defaultMethod = rep('rf', 4))
# meth[c('participant_id', raw_questions)] <- ""  # these won't be imputed
# meth
# 
# imp = mice::mice(data, m = 1, maxit = 5, method = meth, # predictorMatrix = pred, method=meth,
#                  ntree = 10, rfPackage = 'ranger')
#                  # n.core = 5, n.imp.core = 4, parallelseed = 310896, print=T)
# 
# 
# data_compl <- complete(imp, action=1)
# 
# pdf('Castor_iation_QC.pdf')
# 
# for (v in names(data)) { 
#   if (nrow(imp$imp[[v]]) > 1) {
#     try(print(densityplot(imp, as.formula(paste('~',v)))))
#   }
# }
# dev.off()
# 
