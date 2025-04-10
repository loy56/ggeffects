.has_splines <- function(model, form = NULL) {
  if (is.null(form)) {
    form <- .get_pasted_formula(model)
  }
  if (is.null(form)) return(FALSE)
  any(
    grepl("s\\(([^,)]*)", form) | grepl("bs\\(([^,)]*)", form) |
      grepl("ns\\(([^,)]*)", form) | grepl("pspline\\(([^,)]*)", form) |
      grepl("poly\\(([^,)]*)", form)
  )
}


.has_poly <- function(model, form = NULL) {
  if (is.null(form)) {
    form <- .get_pasted_formula(model)
  }
  if (is.null(form)) return(FALSE)
  any(grepl("I\\(.*?\\^.*?\\)", form) | grepl("poly\\(([^,)]*)", form))
}


.has_trigonometry <- function(model, form = NULL) {
  if (is.null(form)) {
    form <- .get_pasted_formula(model)
  }
  if (is.null(form)) return(FALSE)
  any(grepl("(sin|cos|tan)\\(([^,)]*)", form))
}


.has_spline_or_poly <- function(model) {
  form <- .get_pasted_formula(model)
  .has_splines(model, form) || .has_poly(model, form) || .has_trigonometry(model, form)
}


.has_log <- function(model) {
  any(.get_log_terms(model))
}



.has_poly_term <- function(x) {
  any(grepl("poly\\(([^,)]*)", x))
}


.get_poly_term <- function(x) {
  p <- "(.*)poly\\(([^,]*)[^)]*\\)(.*)"
  sub(p, "\\2", x)
}


.get_poly_degree <- function(x) {
  p <- "(.*)poly\\(([^,]*)([^)])*\\)(.*)"
  tryCatch(as.numeric(sub(p, "\\3", x)), error = function(x) 1)
}


.get_log_terms <- function(model) {
  form <- .get_pasted_formula(model)
  if (is.null(form)) return(FALSE)
  grepl("(log|log1|log10|log1p|log2)\\(([^,)]*).*", form)
}


.get_offset_log_terms <- function(model) {
  form <- .get_pasted_formula(model)
  if (is.null(form)) return(FALSE)
  grepl("offset\\((log|log1|log10|log1p|log2)\\(([^,)]*).*", form)
}


.get_offset_transformation <- function(model) {
  form <- .get_pasted_formula(model)
  log_offset <- .get_offset_log_terms(model)
  unname(gsub("offset\\((log|log1|log10|log1p|log2)\\(([^,)]*).*", "\\1", form[log_offset]))
}


.get_pasted_formula <- function(model) {
  tryCatch(
    {
      model_terms <- unlist(.compact_list(insight::find_terms(model, verbose = FALSE)[c("conditional", "random", "instruments")])) # nolint
      if (model_terms[1] %in% c("0", "1")) {
        model_terms <- model_terms[-1]
      }
      model_terms
    },
    error = function(x) NULL
  )
}


.which_log_terms <- function(model) {
  form <- .get_pasted_formula(model)
  if (is.null(form)) return(NULL)
  log_terms <- form[grepl("(log|log1|log10|log1p|log2)\\(([^,)]*).*", form)]
  if (length(log_terms) > 0) {
    log_terms <- insight::clean_names(log_terms)
  } else {
    log_terms <- NULL
  }
  log_terms
}


.uses_all_tag <- function(terms) {
  tags <- unlist(regmatches(
    terms,
    gregexpr(
      pattern = "\\[(.*)\\]",
      text = terms,
      perl = TRUE
    )
  ), use.names = FALSE)

  "[all]" %in% tags
}
