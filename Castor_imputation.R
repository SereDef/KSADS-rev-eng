# ==============================================================================
#                       --- CASTOR data imputation ---
# ==============================================================================

# NOTES:
# * All NAs in COMP are now interpreted as 'meaningful' (not as missing values)
#  If the screener is NA (did not do the screener) a module is missing, skip it 

# ~ Set up ~ -------------------------------------------------------------------

# devtools::install_github('SereDef/GenR.helpR')
library('genR.helpR')
library(dplyr)

library(haven)
library(rpart)
library(rpart.plot)

source('imputation_helpers.R')

data_dir <- '\\\\store/department/genr/isi-store/Behaviour_and_Cognition/Focus_op_17/KSADS/KSADS_DataF17/Data cleaning KSADS_F17/Merging COMP and Castor datasets/Serena'

# list.files(data_dir)

# data_filename <- 'final_dataset_KSADS_F17_cleaned_23022026noSUD.sav'
data_filename <- 'final_dataset_KSADS_F17_cleaned_03042026noSUD.sav'

# ~ Read ~ ---------------------------------------------------------------------

data <- file.path(data_dir, data_filename) |> 
  haven::read_sav() |>
  haven::as_factor(only_labelled = TRUE)

# Completed the computer version
COMP <- data |>
  dplyr::filter(KSADS_Child17_Datasource_cleaned == 'COMP')

Castor <- data |>
  dplyr::filter(KSADS_Child17_Datasource_cleaned == 'Castor')


# ~ Explore & clean ~ ----------------------------------------------------------

# Variable types:

# 1. General assessment variables
general_vars <- c('Rnummer_corrected',                                     
                  'KSADS_Child17_Datasource_cleaned',                     
                  'KSADS_Child17_ID_cleaned',  # TODO: missing in the Castor?                   
                  'KSADS_Child17_DateofInterview_year_cleaned',          
                  'KSADS_Child17_DateofInterview_month_cleaned')

data_overview(data[general_vars])
summary(data[general_vars])

# ------------------------------------------------------------------------------ 
# 2. Raw item scores (for all participants), can be recognized by numeric item IDcode; 
# i.e. var name format such as KSADS_Child17_0.0.0.Q3_cleaned. 
#	- The first number: whether the item was part of the screener (1) or supplement (2)
#   (each module, except sleep problems and introduction, has a screener and a supplement)
# -	The second number: the module (see map below)
#   TMP: SUD modules are missing, will be updated later this year
# - The third number: the symptom/topic the question relates to. (e.g. anhedonia)
# -	(optional) number after “Q..” is question-specific sub (often Q1 = present, Q2 = past)

raw_items  <- grep('^KSADS_Child17_[0-9]+', names(data), value=TRUE)

# 3. Symptom variables (COMP participants only). Computed by KSADS-COMP algorithm, 
#    thus far unaltered us. Can be recognized by letter S in variable name.
#     * Note, we recently found out that sometimes, these variables include odd categories with n=1.
#       As discussed before, in such cases you can collapse all non-0 levels into 1 (so that we have 
#       two levels only: 0=No and 1=Yes).

symptoms <- grep('^KSADS_Child17_S[0-9]', names(data), value=TRUE)

# 4. Diagnosis variables (COMP participants only). 
#     * Computed by KSADS-COMP algorithm, thus far unaltered by us. 
#       Can be recognized by letter D in variable name. E.g. KSADS_Child17_D1
#     * Aggregate diagnosis variables. Made by us by collapsing a few diagnosis 
#       variables into one category. Var names include disorder class description 
#       (e.g. KSADS_Child17_Depressive_disorders_1)

diagnoses <- grep('^KSADS_Child17_D[0-9]', names(data), value=TRUE)

aggr_diagnoses <- setdiff(names(data), c(general_vars, raw_items, symptoms, diagnoses))

cli::cli_bullets(c('*' = '{.val {length(general_vars)}} general variables',
                   '*' = '{.val {length(raw_items)}} raw items',
                   '*' = '{.val {length(symptoms)}} symptoms',
                   '*' = '{.val {length(diagnoses)}} diagnoses',
                   '*' = '{.val {length(aggr_diagnoses)}} aggregate diagnoses',
                   '>' = 'total = {.val {length(c(general_vars, raw_items, symptoms, 
                          diagnoses, aggr_diagnoses))}} / {ncol(data)}'))

data_labels <- data_overview(data)

# --- some raw items were only administered to the Castor people! --------------

# Find raw items that were only used in Castor
# castor_only_items <- names(which(colSums(is.na(COMP[, raw_items])) == nrow(COMP)))
# Correction: actually some of regular items happen to be all NA in COMP (suicidality method)
# So define them manually: 
castor_only_items <- c("KSADS_Child17_2.1.13.q4_cleaned", 
                       "KSADS_Child17_2.1.7.Q4_cleaned",
                       "KSADS_Child17_2.2.13.q2b_cleaned", 
                       "KSADS_Child17_2.2.14.q2b_cleaned", 
                       "KSADS_Child17_2.4.19_cleaned")

# summary(COMP[castor_only_items])
# data_overview(Castor[castor_only_items])
summary(Castor[castor_only_items])

cli::cli_alert_info(c('{.val {length(castor_only_items)}} / {length(raw_items)} items',
                      ' were administerd only in the Castor set, dropping them.'))

raw_items_clean <- setdiff(raw_items, castor_only_items)

# --- Todo: handle raw items that were recoded manually! -----------------------

# TODO: duplicates!
# View(data[data$Rnummer_corrected %in% data$Rnummer_corrected[which(duplicated(data$Rnummer_corrected))],])

# Map modules ------------------------------------------------------------------

module_map <- c(
  '0'  = 'Introduction',
  '1'  = 'Depressive disorders',
  '2'  = 'Bipolar disorders',
  '3'  = 'SUD', # ... i guess??
  '22' = 'Sleep problems',
  '23' = 'Suicidality',
  '4'  = 'Psychotic disorders',
  '8'  = 'Social anxiety',
  '10' = 'Generalized anxiety',
  '13' = 'ED',
  '19' = 'AUD'
)

item_map <- gsub('KSADS_Child17_|_cleaned', '', raw_items) |> # remove useless parts
  strsplit('\\.') |> # separate elements
  (\(parts) {
    # TODO: TMP some sub-questions have different formats, pad to ensure same length
    parts <- lapply(parts, \(p) { length(p) <- 4; p })
    do.call(rbind, parts)
  })() |>
  as.data.frame() |>
  setNames(c('screener','module','question','sub')) 

rownames(item_map) <- raw_items

item_map$module_name <- unname(module_map[ item_map$module ])

# table(item_map$screener)

# We will identify symptoms and diagnoses belonging to that module by using their
# position in the dataset (they come after each set of raw items)
item_map$pos <- match(rownames(item_map), names(data))

module_intervals <- item_map |>
  # Note: we need to exclude the Castor only items here because they are placed 
  # (sometimes?) at the end of datasets and therefore break the max(pos) logic
  dplyr::filter(rownames(item_map) %in% raw_items_clean) |> 
  dplyr::group_by(module) |>
  dplyr::summarise(
    module_name = dplyr::first(module_name),
    # position of first raw item in this module
    start_items = min(pos),  
    # position of last  raw item in this module
    end_items   = max(pos),   
    .groups = "drop"
  ) |>
  dplyr::arrange(start_items) |>
  dplyr::mutate(
    # position interval for this module symptoms and diagnoses
    end_module = dplyr::lead(start_items, 
                             # Disregard last 4 items that are castor only
                             default = ncol(data) - 3L) - 1L
  )


inspect_module <- function(module) {
  
  module_set <- item_map[item_map$module_name == module, ]
  
  tot_items <- nrow(module_set)
  
  intro_items  <- rownames(module_set)[module_set$screener == 0]
  screen_items <- rownames(module_set)[module_set$screener == 1]
  suppl_items  <- rownames(module_set)[module_set$screener == 2]
  
  # Fetch this module's position interval from module_intervals
  module_pos   <- module_intervals[module_intervals$module_name == module, ]
  start_module <- module_pos$end_items
  end_module   <- module_pos$end_module
  
  # Helper: pick only variables from inside the module interval
  in_interval <- function(vars) {
    pos <- match(vars, names(data))
    vars[!is.na(pos) & pos >= start_module & pos <= end_module]
  }
  
  module_symptoms  <- in_interval(symptoms)
  module_diagnoses <- in_interval(diagnoses)
  module_aggr_diangoses <- in_interval(aggr_diagnoses)
  
  get_summaries <- function(var_list){
    
    if (length(var_list) < 1) return(NULL)
    
    sapply(var_list, \(x) list(
      name = x, # use names instead
      label = data_labels[data_labels$name == x, 'label'],
      compr = summarise_item_table(COMP[[x]], Castor[[x]]),
      flag = compare_uniques(COMP[[x]], Castor[[x]]) # Or other flag??
    ), simplify = FALSE, USE.NAMES = TRUE)
  }
  
  
  list(
    # module  = module, use names instead
    n_items      = nrow(module_set),
    n_screener   = length(screen_items),
    n_supplement = length(suppl_items),
    n_symptoms   = length(module_symptoms),
    n_diagnoses  = length(module_diagnoses),
    n_aggr_diagnoses = length(module_aggr_diangoses),
    screener   = get_summaries(screen_items),
    supplement = get_summaries(suppl_items),
    other      = get_summaries(intro_items),
    symptoms   = get_summaries(module_symptoms),
    diagnoses  = get_summaries(module_diagnoses),
    aggr_diagnoses = get_summaries(module_aggr_diangoses)
  )

}


module_summaries <- sapply(unname(module_map), inspect_module, 
                           simplify = FALSE, USE.NAMES = TRUE)

saveRDS(module_summaries, 'module_summaries.rds')
# Visualise in quarto doc

# Helper: fetching the items / symptoms / diagnoses etc. from a module 
get <- function(module, group) {
  
  if (group == 'items') {

    s0 <- module_summaries[[module]][['screener']] |> names()
    s1 <- module_summaries[[module]][['supplement']] |> names()
    
    out <- c(s0, s1)
    
  } else {
    out <- module_summaries[[module]][[group]] |> names()
  }
  
  setdiff(out, castor_only_items) # always remove the castor only ones?
}

# === ITEM RESPONSE PATTERNS====================================================

item_response_patterns <- function(module) {
  
  mi <- get(module, 'items')
  
  # Exclude date items
  is_date_or_numeric <- sapply(mi, \(item) {
    grepl("month|date|year", item, ignore.case = TRUE) ||
      is.numeric(data[[item]])
  })
  # print to inspect...?
  
  mi_facts <- mi[!is_date_or_numeric]
  
  # TMP: Exclude problematic Depression item
  # mi <- mi[!grepl('2.1.22.Q2', mi)]
  
  if (length(mi_facts) == 0) return(NULL)
  
  # Helper: compute pattern frequencies for one dataset
  make_patterns <- function(df, id_col = NULL) {
    df[mi_facts] |>
      # NOTE: Assume no meaningful NA
      mutate(across(everything(), \(x) ifelse(is.na(x), "_", as.character(x)))) |>
      tidyr::unite("pattern", everything(), sep = "|") |> # paste
      count(pattern, name = "n") |>
      arrange(desc(n))
  }
  
  pat_comp  <- make_patterns(COMP)
  pat_cast  <- make_patterns(Castor)
  
  # inspect most popular pattern
  # rbind(pat_comp[1,], pat_cast[1, ])
  # identical(pat_comp$pattern[1], pat_cast$pattern[1])
  
  d1 <- setdiff(pat_comp$pattern, pat_cast$pattern) # in comp not in cast
  d2 <- setdiff(pat_cast$pattern, pat_comp$pattern) # in cast not in comp
  
  # Report
  cli::cli_rule(module)
  cli::cli_text('{.strong {length(mi_facts)}} / {length(mi)} items considered.\n\n')
  
  cli::cli_bullets(c(
    '*' = '{.val {nrow(pat_comp)}} unique patterns in COMP (n = {nrow(COMP)})
           {cli::symbol$arrow_right} {cli::col_yellow(cli::symbol$warning)} {length(d1)} patterns in COMP are not in Castor',
    '*' = '{.val {nrow(pat_cast)}} unique patterns in Castor (n = {nrow(Castor)})
           {cli::symbol$arrow_right} {cli::col_red(cli::symbol$cross)} {length(d2)} patterns in Castor are not in COMP'
  ))
  
  cli::cli_text('\n\n{.strong Most popular pattern:}')
  # cat(pat_comp$pattern[1], "\n\n")
  # cat(pat_cast$pattern[1], "\n\n")
  
  withr::with_options(list(cli.width = Inf), {
    cli::cli_text(pat_comp$pattern[1])
    cli::cli_text(pat_cast$pattern[1])
  })
  
  cli::cli_text()
  
  invisible(NULL)
}

n <- lapply(unname(module_map), item_response_patterns)

# === PREDICTION VALIDATION ====================================================

module <- 'Bipolar disorders'

mi <- get(module, 'items')
ms <- get(module, 'symptoms')
md <- get(module, 'diagnoses')



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

# TODO: ensure there are no 'labelled' and other SPSS bs in the dataset 
# factors are factors and numbers are numeric!

data <- factorize(comput, min_levels = 3) # consider only yes / no

# ~~~~~~~~ TMP: MANUAL FIXES ~~~~~~~~~~~~~~~~~

# TODO: ensure labels used are correct!
# Some 'labelled' have too many levels, treat them as numeric
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
                  type = if (s$method == 'class') 'class' else 'vector')
  
  print(table(cbind(pred, data[, outcome]), useNA = 'ifany'))
  
  cat('\n\n')
}

# ~ Inspect rules ~ 
symptoms_rules <- do.call(rbind, lapply(symptoms_preds, `[[`, 'rules'))

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
# cvfit <- cv.glmnet(X, y, family = 'binomial', alpha = 1)  # LASSO
# 
# cvfit <- cv.glmnet(X, y, family = 'gaussian', alpha = 1)  # LASSO
# coef(cvfit, s = 'lambda.1se')

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
# meth[c('participant_id', raw_questions)] <- ''  # these won't be imputed
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
