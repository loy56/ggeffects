skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("datawizard")
skip_if_not_installed("sjlabelled")

test_that("ggpredict, format", {
  data(efc, package = "ggeffects")
  efc$c172code <- datawizard::to_factor(efc$c172code)
  efc$e42dep <- datawizard::to_factor(efc$e42dep)
  efc$c82cop1 <- as.numeric(efc$c82cop1)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c82cop1 + e42dep + c161sex + c172code, data = efc)
  pr <- ggpredict(fit, terms = "neg_c_7 [quart2]")

  out <- format(pr)
  expect_identical(dim(out), c(3L, 3L))
  expect_identical(out[["95% CI"]], c("85.95, 95.93", "83.95, 93.93", "80.75, 91.12"))

  pr <- ggpredict(fit, terms = "c161sex")
  out <- format(pr, value_labels = TRUE)
  expect_identical(out$c161sex, c("[1] Male  ", "[2] Female"))
  out <- format(pr, variable_labels = TRUE)
  expect_identical(colnames(out)[2], "Predicted values of Total score BARTHEL INDEX")

  fit <- lm(barthtot ~ c161sex * c172code, data = efc)
  pr <- ggpredict(fit, c("c161sex", "c172code"))
  out <- format(pr)
  expect_identical(
    out$groups,
    c(
      "low level of education", "low level of education", "intermediate level of education",
      "intermediate level of education", "high level of education",
      "high level of education"
    )
  )
  out <- format(pr, group_name = TRUE)
  expect_identical(
    out$groups,
    c(
      "c172code: low level of education", "c172code: low level of education",
      "c172code: intermediate level of education", "c172code: intermediate level of education",
      "c172code: high level of education", "c172code: high level of education"
    )
  )
  pr <- ggpredict(fit, terms = "c161sex")
  out <- format(pr)
  expect_identical(out[["95% CI"]], c("55.21, 73.33", "54.65, 64.67"))
  out <- format(pr, ci_brackets = c("[", "]"))
  expect_identical(out[["95% CI"]], c("[55.21, 73.33]", "[54.65, 64.67]"))
})


test_that("format, collapse tables", {
  data(iris)
  m <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  pr <- ggpredict(m, c("Petal.Length", "Species"))
  out <- format(pr, collapse_tables = TRUE, n = 3)
  expect_named(out, c("Petal.Length", "Species", "Predicted", "95% CI"))
  expect_identical(out$Species, c("setosa", "", "", "versicolor", "", "", "virginica", "", ""))
})


skip_if_not_installed("withr")

withr::with_options(
  list(ggeffects_ci_brackets = c("(", ")")),
  test_that("ggpredict, parenthesis-option", {
    data(efc, package = "ggeffects")
    fit <- lm(barthtot ~ c161sex, data = efc)
    pr <- ggpredict(fit, terms = "c161sex")
    out <- format(pr)
    expect_identical(out[["95% CI"]], c("(62.86, 70.86)", "(61.54, 66.02)"))
  })
)
