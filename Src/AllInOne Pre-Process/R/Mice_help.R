#' Include a list of methods available for mice function and the description of the method
#'
#' @noRd
#'
mice_help <- base::list  (
  'pmm' = ' Method\'s description : Predictive mean matching (available for any type of data)',
  'midastouch' = ' Method\'s description : Weighted predictive mean matching (available for any type of data)',
  'sample' = ' Method\'s description : Random sample from observed values (available for any type of data)',
  'cart' = ' Method\'s description : Classification and regression trees (available for any type of data)',
  'rf' = ' Method\'s description : Random forest imputations (available for any type of data)',
  'mean' = ' Method\'s description : Unconditional mean imputation (available for numeric data)',
  'norm' = ' Method\'s description : Bayesian linear regression (available for numeric data)',
  'norm.nob' = ' Method\'s description : Linear regression ignoring model error (available for numeric data)',
  'norm.boot' = ' Method\'s description : Linear regression using bootstrap (available for numeric data)',
  'norm.predict' = ' Method\'s description : Linear regression, predicted values (available for numeric data)',
  'lasso.norm' = ' Method\'s description : Lasso linear regression (available for numeric data)',
  'lasso.select.norm' = ' Method\'s description : Lasso select + linear regression (available for numeric data)',
  'quadratic' = ' Method\'s description : Imputation of quadratic terms (available for numeric data)',
  'ri' = ' Method\'s description : Random indicator for nonignorable data (available for numeric data)',
  'logreg' = ' Method\'s description : Logistic regression (available for binary data)',
  'logreg.boot' = ' Method\'s description : binary Logistic regression with bootstrap (available for binary data)',
  'lasso.logreg' = ' Method\'s description : Weighted (available for binary data)',
  'lasso.select.logreg' = ' Method\'s description : Lasso select + logistic regression (available for binary data)',
  'polr' = ' Method\'s description : Proportional odds model (available for ordered data)',
  'polyreg' = ' Method\'s description : Polytomous logistic regression (available for unordered data)',
  'lda' = ' Method\'s description : Linear discriminant analysis (available for unordered data)',
  '2l.norm' = ' Method\'s description : Level-1 normal heteroscedastic (available for numeric data)',
  '2l.lmer' = ' Method\'s description : Level-1 normal homoscedastic, lmer (available for numeric data)',
  '2l.pan' = ' Method\'s description : Level-1 normal homoscedastic, pan (available for numeric data)',
  '2l.bin' = ' Method\'s description : Level-1 logistic, glmer (available for binary data)',
  '2lonly.mean' = ' Method\'s description : Level-2 class mean (available for numeric data)',
  '2lonly.norm' = ' Method\'s description : Level-2 class normal (available for numeric data)',
  '2lonly.pmm' = ' Method\'s description : Level-2 class predictive mean matching (available for any type of data)'

)
