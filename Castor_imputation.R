# ==============================================================================
#                       --- CASTOR data imputation ---
# ==============================================================================

# ~ Set up ~ -------------------------------------------------------------------

# devtools::install_github('SereDef/GenR.helpR')
library('genR.helpR')

library(haven)
library(rpart)
library(rpart.plot)

source('imputation_helpers.R')

data_dir <- "\\\\store/department/genr/isi-store/Behaviour_and_Cognition/Focus_op_17/KSADS/KSADS_DataF17/Data cleaning KSADS_F17/Merging COMP and Castor datasets/Serena"

# list.files(data_dir)

data_filename <- 'final_dataset_KSADS_F17_cleaned_23022026noSUD.sav'

# ~ Read ~ ---------------------------------------------------------------------

data <- file.path(data_dir, data_filename) |> 
  haven::read_sav()

# ~ Explore & clean ~ ----------------------------------------------------------

# Variable types:

# 1. General assessment variables
general_vars <- c("Rnummer_corrected",                                     
              "KSADS_Child17_Datasource_cleaned",                     
              "KSADS_Child17_ID_cleaned",                     
              "KSADS_Child17_DateofInterview_year_cleaned",          
              "KSADS_Child17_DateofInterview_month_cleaned")

summary(data[general_vars])
head(data[general_vars], 3)

table(data$KSADS_Child17_Datasource_cleaned, useNA='ifany') # TODO: ID missing ?   

# 2. Raw item scores (for all participants), can be recognized by numeric item IDcode; 
# i.e. var name format such as KSADS_Child17_0.0.0.Q3_cleaned. Tips for reading the KSADS question IDs:
#   	The first number tells you whether the item was part of the screener (1) or supplement (2). 
# E.g.: 1.1 is a depressive disorders screener item, 2.1 is a depressive disorder supplement item
# 	The second number represents the module (see list below)
# 	The third number reflects the symptom/topic the question relates to. (e.g. anhedonia)
# 	The number after “Q..” is sub question-specific. (often Q1 = present, Q2 = past)

raw_items  <- grep('^KSADS_Child17_[0-9]+', names(data), value=TRUE)

symptoms <- grep('^KSADS_Child17_S[0-9]', names(data), value=TRUE) #TODO: missing??

diagnoses <- grep('^KSADS_Child17_D[0-9]', names(data), value=TRUE)

# meaningful na?
# if screener is missing that is an accident 


# 3. Symptom variables (COMP participants only). Computed by KSADS-COMP algorithm, 
#    thus far unaltered us. Can be recognized by letter S in variable name.
#     * Note, we recently found out that sometimes, these variables include odd categories with n=1.
#       As discussed before, in such cases you can collapse all non-0 levels into 1 (so that we have 
#       two levels only: 0=No and 1=Yes).

# 4. Diagnosis variables (COMP participants only). 
#     * Computed by KSADS-COMP algorithm, thus far unaltered by us. 
#       Can be recognized by letter D in variable name. E.g. KSADS_Child17_D1
#     * Aggregate diagnosis variables. Made by us by collapsing a few diagnosis 
#       variables into one category. Var names include disorder class description 
#       (e.g. KSADS_Child17_Depressive_disorders_1)

# o	All modules (except for the SUD modules, that will follow in an updated dataset later this year).
# 	KSADS modules can be recognized based on their module number, which is always the second number in the variable name (e.g. KSADS_Child17_0.0.0.Q3_cleaned = item from introduction module)
# 	Introduction module = 0
# 	Depressive disorders module = 1
# 	Bipolar disorders module = 2
# 	Sleep problems module = 22
# 	Suicidality module = 23
# 	Psychotic disorders module = 4
# 	Social anxiety = 8
# 	Generalized anxiety = 10
# 	ED module = 13
# 	AUD module = 19
# 	(Each module, with exception of sleep problems and introduction, consists of 
# a screener (1) and supplement (2) -> first number in variable name shows if item 
# is from screener or supplement) (e.g. KSADS_Child17_1.1.1.Q1_cleaned = screen item)


# Completed the computer version
comput <- data |>
  dplyr::filter(KSADS_Child17_Datasource_cleaned == 1)

# TODO: for some of these the raw items do not correspond to 
# the symptoms (the raw score was corrected, the symptom should be recalculated)

# Paper-and-pencil / Castor -> must be imputed 
castor <- data |>
  dplyr::filter(KSADS_Child17_Datasource_cleaned == 2)
# Read data (one module at the time)
module <- 'ED'

# TODO: make sure module names are consistent

# comput <- file.path(data_dir, 
#                     paste0('Merge_items_diagnoses_COMP_', module, ' module.sav')) |> 
#   haven::read_sav()
# 
# castor <- file.path(data_dir,
#                     paste0('Castor_dataset_only_cleaned_', module, ' module.sav')) |>
#   haven::read_sav()
  

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
# all 1.soemthig are NA: did not do the screener
screener_items <- grep('^KSADS_Child17_1.', names(comput), value = TRUE)
did_not_complete_screener <- rowSums(is.na(comput[,screener_items])) == length(screener_items)
comput[did_not_complete_screener, ] <- NULL

# Check: in castor fill them with NA 


# TODO: check Constant 5 NA??? in symptoms
# Check there are no empty rows
# any(rowSums(is.na(comput)) == ncol(comput))

# data <- merge(castor, comput, all = TRUE)

# Check merging 
# waldo::compare(data[data$participant_id %in% castor$participant_id, ], castor)

# TODO: ensure there are no "labelled" and other SPSS bs in the dataset 
# factors are factors and numbers are numeric!

data <- factorize(comput, min_levels = 3) # consider only yes / no

# ~~~~~~~~ TMP: MANUAL FIXES ~~~~~~~~~~~~~~~~~

# TODO: ensure labels used are correct!
# Some "labelled" have too many levels, treat them as numeric
data <- numericize(data, to_factor = TRUE)

# Some have odd labels that *I think* should be turned to Yes - No
data <- force_binary(data, paste0('KSADS_Child17_S2', c(36, 38, 41)))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

data_overview(data)

summary(data)

# ~ Prediction ~ ---------------------------------------------------------------

items <- grep('KSADS_Child17_[0-9+]', names(data), value = TRUE)
symptoms <- grep('KSADS_Child17_S[0-9+]', names(data), value = TRUE)
diagnoses <- grep('KSADS_Child17_D[0-9+]', names(data), value = TRUE)

# Making sure no variables are missing
check_vars_included(data, items, symptoms, diagnoses)

# TODO: PROBLEM = what to do when no observed cases? (e.g. KSADS_Child17_S243)
# Dropping them for now 

# ==============================================================================
# ~ CART (Classification and Regression Trees) ~
# ==============================================================================

# https://mdsr-book.github.io/mdsr3e/11-learningI.html


symptoms_preds <- lapply(symptoms, function(s) {
  get_rules(outcome = s, predictors = items, data = data)
})


# ~ Inspect predictions ~ 
# check_preds <- data

for (s in symptoms_preds) {
  
  outcome <- s$outcome
  
  message(outcome)
  
  # check_preds[!is.na(data[, outcome]), outcome] <- predict(s$fit, newdata = data)
  if (s$predictors == '') {
    next
  }
  
  pred <- predict(s$fit, newdata = data, 
                  type = if (s$method == "class") "class" else "vector")
  
  print(table(cbind(pred, data[, outcome]), useNA = 'ifany'))
  
  cat('\n\n')
}

# ~ Inspect rules ~ 
symptoms_rules <- do.call(rbind, lapply(symptoms_preds, `[[`, "rules"))

# in the castor take a row and check if you never saw this before 
# flag items that are completely missing for the paper and pencil 


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
