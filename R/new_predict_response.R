#compared to the original predict_response function
#here we added the step of handling missing values

predict_response <- function(model, terms, margin = "mean_reference", ci_level = 0.95,
                                type = "fixed", condition = NULL, interval = "confidence",
                                back_transform = TRUE, vcov = NULL, vcov_args = NULL, weights = NULL,
                                bias_correction = FALSE, verbose = TRUE, na.rm = FALSE, na.action = "omit", ...)
{
  margin <- getOption("ggeffects_margin", margin)
  margin <- insight::validate_argument(argument = margin, options = c("mean_reference",
                                                                      "mean_mode", "marginalmeans", "empirical", "counterfactual",
                                                                      "full_data", "average", "marginaleffects"))
  model_name <- insight::safe_deparse(substitute(model))
  type <- insight::validate_argument(type, c("fixed", "random", "response", "link"))
  interval <- insight::validate_argument(interval, c("confidence",
                                                     "prediction"))

  # Capture additional arguments
  dots <- list(...)
  newdata <- dots$newdata

  # Check and handle missing values
  if (!is.null(newdata)) {
    predictor_names <- insight::find_predictors(model)$conditional
    required_vars <- c(predictor_names, weights)
    newdata <- newdata[, required_vars, drop = FALSE]
    missing_mask <- !complete.cases(newdata)

    if (any(missing_mask)) {
      if (na.action == "omit") {
        message(sprintf("⚠️ Warning: %d rows of `newdata` containing missing values have been removed.", sum(missing_mask)))
        newdata <- newdata[!missing_mask, , drop = FALSE]
      } else if (na.action == "fail") {
        stop("Error: `newdata` contains missing values. Please handle them before proceeding.")
      } else if (na.action == "mean_impute") {
        for (var in required_vars) {
          if (is.numeric(newdata[[var]])) {
            newdata[[var]][is.na(newdata[[var]])] <- mean(newdata[[var]], na.rm = TRUE)
          }
        }
        message("⚠️ Warning: Mean imputation performed for missing values.")
      }
    }
  }

  dots$newdata <- newdata  # Add newdata to dots

  out <- switch(margin,
                mean_reference = ggpredict(model, terms = terms, ci_level = ci_level,
                                           type = type, typical = "mean", condition = condition,
                                           back_transform = back_transform, vcov = vcov, vcov_args = vcov_args,
                                           interval = interval, bias_correction = bias_correction,
                                           verbose = verbose, ...),
                mean_mode = ggpredict(model,
                                      terms = terms, ci_level = ci_level, type = type, typical = c(numeric = "mean",
                                                                                                   factor = "mode"), condition = condition, back_transform = back_transform,
                                      vcov = vcov, vcov_args = vcov_args, interval = interval,
                                      bias_correction = bias_correction, verbose = verbose,
                                      ...),
                marginalmeans = ggemmeans(model, terms = terms,
                                          ci_level = ci_level, type = type, typical = "mean", condition = condition,
                                          back_transform = back_transform, vcov = vcov, vcov_args = vcov_args,
                                          interval = interval, bias_correction = bias_correction,
                                          weights = weights, verbose = verbose, ...),
                average = , counterfactual = , marginaleffects = , empirical = {
                  do.call(ggaverage, c(list(model = model, terms = terms, ci_level = ci_level,
                                            type = type, typical = "mean", condition = condition,
                                            back_transform = back_transform, vcov = vcov, vcov_args = vcov_args,
                                            weights = weights, verbose = verbose), dots))
                },
                full_data = ggpredict(model, terms = terms, ci_level = ci_level,
                                      type = type, typical = "full", condition = condition,
                                      back_transform = back_transform, vcov = vcov, vcov_args = vcov_args,
                                      interval = interval, bias_correction = bias_correction,
                                      verbose = verbose, newdata = insight::get_data(model), ...))

  attr(out, "model.name") <- model_name
  out
}
