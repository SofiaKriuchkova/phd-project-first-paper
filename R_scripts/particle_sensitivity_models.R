n_tokens <- sum(d[[response]])
zero_obs <- sum(d[[response]] == 0L)
warnings <- character()
err <- ''
model <- NULL
if (n_tokens > 0 && n_distinct(d[[response]]) > 1) {
  model <- tryCatch(
    withCallingHandlers(
      glmmTMB(make_formula(response), data = d,
              family = family_obj,
              control = glmmTMBControl(optimizer = nlminb)),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart('muffleWarning')
      }),
    error = function(e) { err <<- conditionMessage(e); NULL }
  )
} else {
  err <- 'Zero or invariant response: model cannot be estimated'
}

code <- if (is.null(model)) NA_integer_ else as.integer(model$fit$convergence)
pd_hess <- if (is.null(model)) FALSE else isTRUE(model$sdr$pdHess)
coef <- if (is.null(model)) NULL else tryCatch(summary(model)$coefficients$cond,
                                               error = function(e) NULL)
all_se <- !is.null(coef) && all(is.finite(coef[, 'Std. Error'])) &&
  all(coef[, 'Std. Error'] > 0) && all(is.finite(coef[, 'Estimate']))
usable <- !is.null(model) && !is.na(code) && code == 0L && pd_hess && all_se
# Never publish an effect estimate without reliable uncertainty.
estimate <- se <- irr <- lower <- upper <- p <- NA_real_
if (usable && 'gender1' %in% rownames(coef)) {
  estimate <- as.numeric(coef['gender1', 'Estimate'])
  se <- as.numeric(coef['gender1', 'Std. Error'])
  p <- as.numeric(coef['gender1', 'Pr(>|z|)'])
  irr <- exp(estimate)
  lower <- exp(estimate - 1.96 * se)
  upper <- exp(estimate + 1.96 * se)
}

tibble(category, analysis, response, specification = 'full',
       family = family_label,
       total_tokens = n_tokens,
       zero_observations = zero_obs,
       convergence_code = code,
       positive_definite_hessian = pd_hess,
       finite_positive_se = all_se,
       usable = usable,
       b = estimate,
       SE = se,
       IRR = irr,
       CI_95_low = lower,
       CI_95_high = upper,
       p_unadjusted = p,
       warnings = paste(unique(warnings), collapse = ' | '),
       error = err)
}

comparison <- bind_rows(
  fit_and_extract('jumal', 'religious', 'original'),
  fit_and_extract('jumal_without_issand', 'religious', 'without_issand'),
  fit_and_extract('kurat', 'obscene', 'original'),
  fit_and_extract('kurat_without_kurat', 'obscene', 'without_kurat')
)

# Exploratory sensitivity tests: report nominal p-values descriptively,
# not as if they were members of the primary 13-test BH family.
# Explicit base-R counting prevents ambiguous evaluations inside summarise().
# Report zero counts for original and leave-one-lemma-out responses.
count_by_gender <- purrr::map_dfr(levels(d$gender), function(g) {
  sub <- d[as.character(d$gender) == g, , drop = FALSE]
  tibble(
    gender = g,
    n_obs = nrow(sub),
    words = sum(sub$speaker_word_count),
    jumal_original = sum(sub$jumal),
    jumal_without_issand = sum(sub$jumal_without_issand),
    kurat_original = sum(sub$kurat),
    kurat_without_kurat = sum(sub$kurat_without_kurat),
    zeros_jumal_original = sum(sub$jumal == 0),
    zeros_jumal_without_issand = sum(sub$jumal_without_issand == 0),
    zeros_kurat_original = sum(sub$kurat == 0),
    zeros_kurat_without_kurat = sum(sub$kurat_without_kurat == 0)
  )
})

# Verify the descriptive table against the ungrouped data and model table.
for (item in list(
  c('jumal_without_issand', 'zeros_jumal_without_issand', 'without_issand'),
  c('kurat_without_kurat', 'zeros_kurat_without_kurat', 'without_kurat')
)) {
  response <- item[[1]]
  zero_column <- item[[2]]
  analysis_name <- item[[3]]
  stopifnot(
    sum(count_by_gender[[zero_column]]) == sum(d[[response]] == 0),
    sum(count_by_gender[[zero_column]]) ==
      comparison$zero_observations[comparison$analysis == analysis_name]
  )
}

readr::write_csv(comparison, file.path(out_dir, 'sensitivity_model_comparison.csv'))
readr::write_csv(count_by_gender, file.path(out_dir, 'sensitivity_descriptives.csv'))

print(comparison %>% select(category, analysis, specification, family, total_tokens,
                            zero_observations, usable, IRR, CI_95_low,
                            CI_95_high, p_unadjusted))
print(count_by_gender)
cat('\nSaved results to ', out_dir, '\n', sep = '')

