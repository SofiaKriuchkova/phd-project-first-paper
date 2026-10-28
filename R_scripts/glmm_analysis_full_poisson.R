# REVISED GLMM ANALYSIS FOR MANUSCRIPT RESUBMISSION
# This script uses ONLY the 13 predeclared primary marker categories.
# All kurat models use Poisson; all other marker categories use NB2.
# The primary kurat M2 is full Poisson when valid; the reduced fit is retained for auditing.
# Comments are in English. Review the classification of jumal/kurat before publication.
# Run from the directory containing this script and the data/ folder.

library(tidyverse)
library(glmmTMB)
library(openxlsx)

set.seed(20261022)
data_dir <- "data"
# Keep the previous NB2 outputs intact for the response to reviewers.
out_dir <- file.path("results", "poisson_full_revision")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 1. EXPLICIT INPUT FILES AND MARKER CATEGORIES -------------------------------
# Pronouns are modeled separately. pronouns_all is descriptive only.
# particle and the five individual CPT lemmas are NOT primary outcomes.
marker_files <- list(
  eitea = c("eitea"),
  intensifiers = c("intensifiers"),
  kp_markers = c("kp_markers"),
  particles = c("polite_words", "jumal", "kurat", "evaluation",
                "certainty", "probability", "approximation"),
  pronouns_1face = c("pronouns_1face"),
  pronouns_2face = c("pronouns_2face"),
  reflexives = c("reflexives")
)
primary_markers <- unlist(marker_files, use.names = FALSE)
stopifnot(length(primary_markers) == 13L,
          !anyDuplicated(primary_markers))

# Labels used consistently in manuscript figures and exported result tables.
# Keep original marker codes intact for modelling and reproducibility.
marker_labels <- c(
  polite_words = "Polite words",
  certainty = "Certainty/confirmation",
  probability = "Probability/possibility",
  approximation = "Hedging/approximation",
  evaluation = "Evaluation",
  kp_markers = "CTPs (complement-taking predicate markers)",
  eitea = "Epistemic markers (ma ei tea and ei tea)",
  kurat = "Obscene particles",
  jumal = "Religious particles",
  intensifiers = "Intensifiers",
  pronouns_1face = "First-person personal pronouns",
  pronouns_2face = "Second-person personal pronouns",
  reflexives = "Reflexives"
)
# Display the categories in the same order as Section 3.2 of the manuscript.
marker_display_order <- names(marker_labels)
stopifnot(setequal(names(marker_labels), primary_markers),
          length(marker_labels) == 13L,
          !anyDuplicated(unname(marker_labels)))

add_marker_labels <- function(df) {
  if (!"marker" %in% names(df)) return(df)
  keys <- as.character(df[["marker"]])
  unknown <- setdiff(unique(keys[!is.na(keys)]), names(marker_labels))
  if (length(unknown)) {
    stop("Missing human-readable label for marker code(s): ",
         paste(unknown, collapse = ", "))
  }
  df %>%
    mutate(marker_label = unname(marker_labels[as.character(.data$marker)]),
           .after = marker)
}

# Include the code-to-label dictionary among the saved results.
marker_dictionary <- tibble(
  marker = names(marker_labels),
  marker_label = unname(marker_labels)
)
readr::write_csv(marker_dictionary, file.path(out_dir, "marker_labels.csv"))

metadata_cols <- c(
  "dialogue", "id", "conditions", "gender", "year", "age",
  "education", "age_diff", "second_speaker_gender", "speaker_word_count"
)

read_marker_data <- function(stem, columns) {
  path <- file.path(data_dir, paste0(stem, ".csv"))
  if (!file.exists(path)) stop("Missing input file: ", path)
  x <- readr::read_csv(path, show_col_types = FALSE) %>% janitor::clean_names()
  needed <- c(metadata_cols, columns)
  if (!all(needed %in% names(x))) {
    stop("Missing columns in ", path, ": ",
         paste(setdiff(needed, names(x)), collapse = ", "))
  }
  x <- x %>%
    mutate(dialogue = as.character(dialogue), id = as.character(id)) %>%
    arrange(dialogue, id)
  if (anyDuplicated(x[, c("dialogue", "id")])) {
    stop("Duplicate participant-dialogue observations in ", path)
  }
  if (nrow(x) != 180L || n_distinct(x$dialogue) != 90L ||
      n_distinct(x$id) != 150L) {
    stop("Unexpected sample size in ", path,
         ": expected N=180, 90 dialogues and 150 unique speakers")
  }
  if (anyNA(x[, columns])) {
    stop("Missing marker counts in ", path, "; investigate, do not replace NA with 0")
  }
  for (col in columns) {
    if (!is.numeric(x[[col]]) || any(x[[col]] < 0) ||
        any(x[[col]] != floor(x[[col]]))) {
      stop("Marker must be a non-negative integer: ", stem, " / ", col)
    }
  }
  x
}

dfs <- purrr::imap(marker_files, ~ read_marker_data(.y, .x))
base_metadata <- dfs[[1]] %>% select(all_of(metadata_cols))

# Assert the same participants, dialogue assignments and metadata across files.
for (stem in names(dfs)) {
  cmp <- dfs[[stem]] %>% select(all_of(metadata_cols))
  if (!isTRUE(all.equal(base_metadata, cmp, check.attributes = FALSE))) {
    stop("Metadata mismatch for ", stem, ". Correct the input files first.")
  }
}

# Build one observation-level analysis dataset with exactly 13 outcomes.
model_data <- base_metadata
for (stem in names(dfs)) {
  model_data <- model_data %>%
    left_join(dfs[[stem]] %>% select(dialogue, id, all_of(marker_files[[stem]])),
              by = c("dialogue", "id"))
}

# Check the split-pronoun identity against the combined file (not fitted).
combined_path <- file.path(data_dir, "pronouns_all.csv")
if (file.exists(combined_path)) {
  combined <- readr::read_csv(combined_path, show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    mutate(dialogue = as.character(dialogue), id = as.character(id)) %>%
    select(dialogue, id, pronouns)
  comparison <- model_data %>%
    left_join(combined, by = c("dialogue", "id"))
  if (anyNA(comparison$pronouns) ||
      any(comparison$pronouns != comparison$pronouns_1face +
            comparison$pronouns_2face)) {
    stop("Pronoun subgroups do not sum to the combined counts")
  }
}

model_data <- model_data %>%
  mutate(
    id = factor(id), dialogue = factor(dialogue),
    gender = factor(gender, levels = c(0, 1)),
    second_speaker_gender = factor(second_speaker_gender, levels = c(0, 1)),
    conditions = factor(conditions, levels = c(0, 1)),
    year_scaled = as.numeric(scale(year))
  )

required_predictors <- c("gender", "second_speaker_gender", "conditions",
                         "age", "age_diff", "year_scaled", "speaker_word_count")
if (anyNA(model_data[, required_predictors]) ||
    any(model_data$speaker_word_count <= 0)) {
  stop("Missing predictor(s) or nonpositive word counts: resolve before fitting")
}
if (any(table(model_data$dialogue) != 2L)) {
  stop("Each dialogue must contain exactly two speaker observations")
}
stopifnot(nrow(model_data) == 180L)

# Descriptive information requested by Reviewer 2.
marker_descriptives <- purrr::map_dfr(primary_markers, function(marker) {
  model_data %>%
    group_by(gender) %>%
    summarise(marker = marker,
              n_observations = n(),
              total_tokens = sum(.data[[marker]]),
              zero_observations = sum(.data[[marker]] == 0),
              word_tokens = sum(speaker_word_count),
              per_10000_words = total_tokens / word_tokens * 10000,
              .groups = "drop")
})
readr::write_csv(add_marker_labels(marker_descriptives), file.path(out_dir, "marker_descriptives.csv"))

corpus_summary <- tibble(
  n_observations = nrow(model_data),
  n_dialogues = n_distinct(model_data$dialogue),
  n_speakers = n_distinct(model_data$id),
  speakers_in_multiple_dialogues = sum(table(model_data$id) > 1),
  word_tokens = sum(model_data$speaker_word_count),
  mean_words_per_observation = mean(model_data$speaker_word_count),
  median_words_per_observation = median(model_data$speaker_word_count),
  min_words_per_observation = min(model_data$speaker_word_count),
  max_words_per_observation = max(model_data$speaker_word_count)
)
readr::write_csv(corpus_summary, file.path(out_dir, "corpus_summary.csv"))

# 2. FORMULAS AND FIT DIAGNOSTICS -------------------------------------------
# NB2: variance increases quadratically with the expected count.
# For kurat, a very large NB2 dispersion parameter led to an unstable Hessian.
# Both full and reduced Poisson GLMMs were checked with DHARMa separately.
# The full Poisson model had a valid Hessian and no flagged residual issues.
# Prefer full Poisson M2 to retain age_diff and year_scaled for kurat.
# A reduced Poisson M2 is fitted for audit/fallback and sensitivity comparisons.
# Full and reduced NB2 alternatives are retained for diagnosis only.
# Default glmmTMB optimizer is nlminb; stated explicitly for reproducibility.
off <- "offset(log(speaker_word_count))"
re <- "(1 | id) + (1 | dialogue)"

build_formulas <- function(response) {
  form <- function(rhs) stats::as.formula(paste(response, "~", rhs, "+", off, "+", re))
  list(
    m0 = form("1"),
    m1 = form("gender"),
    m2_full = form("gender + age + year_scaled + conditions + age_diff + second_speaker_gender"),
    m2_reduced = form("gender + age + conditions + second_speaker_gender"),
    gender_age = form("gender * age + year_scaled + conditions + age_diff + second_speaker_gender"),
    gender_age_diff = form("gender * age_diff + age + year_scaled + conditions + second_speaker_gender"),
    gender_interlocutor = form("gender * second_speaker_gender + age + year_scaled + conditions + age_diff"),
    gender_year = form("gender * year_scaled + age + conditions + age_diff + second_speaker_gender")
  )
}

fit_once <- function(formula, df, family, family_label) {
  warnings <- character()
  error_msg <- ""
  mod <- tryCatch(
    withCallingHandlers(
      glmmTMB(formula, data = df, family = family,
              control = glmmTMBControl(optimizer = nlminb)),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error_msg <<- conditionMessage(e)
      NULL
    }
  )

  convergence <- if (is.null(mod)) NA_integer_ else mod$fit$convergence
  pd_hess <- if (is.null(mod)) FALSE else isTRUE(mod$sdr$pdHess)
  coef_se_ok <- FALSE
  if (!is.null(mod)) {
    coef_matrix <- tryCatch(summary(mod)$coefficients$cond,
                            error = function(e) NULL)
    if (!is.null(coef_matrix) && nrow(coef_matrix) > 0L) {
      coef_se_ok <- all(is.finite(coef_matrix[, "Estimate"])) &&
        all(is.finite(coef_matrix[, "Std. Error"])) &&
        all(coef_matrix[, "Std. Error"] > 0)
    }
  }
  usable <- !is.null(mod) && identical(as.integer(convergence), 0L) &&
    pd_hess && coef_se_ok

  list(
    model = mod,
    status = tibble(
      family = family_label,
      usable = usable,
      convergence_code = as.integer(convergence),
      positive_definite_hessian = pd_hess,
      estimable_coefficients = coef_se_ok,
      n_observations = if (is.null(mod)) NA_integer_ else as.integer(nobs(mod)),
      warnings = paste(unique(warnings), collapse = " | "),
      error = error_msg
    )
  )
}

extract_fixed <- function(mod) {
  cmat <- summary(mod)$coefficients$cond
  tibble(
    term = rownames(cmat),
    estimate = as.numeric(cmat[, "Estimate"]),
    std.error = as.numeric(cmat[, "Std. Error"]),
    statistic = as.numeric(cmat[, "z value"]),
    p.value = as.numeric(cmat[, "Pr(>|z|)"])
  ) %>%
    mutate(IRR = exp(estimate),
           conf.low = exp(estimate - 1.96 * std.error),
           conf.high = exp(estimate + 1.96 * std.error))
}

model_store <- list()
diagnostic_rows <- list()
model_specs <- c("m0", "m1", "m2_full", "gender_age", "gender_age_diff",
                 "gender_interlocutor", "gender_year")

# Fit the complete set of model specifications for every category.
# Poisson is used for kurat throughout, including exploratory interactions.
for (marker in primary_markers) {
  cat("Fitting:", marker, "\n")
  formulas <- build_formulas(marker)
  is_kurat <- identical(marker, "kurat")
  main_family <- if (is_kurat) poisson(link = "log") else nbinom2()
  family_name <- if (is_kurat) "Poisson" else "NB2"

  for (spec in model_specs) {
    ans <- fit_once(formulas[[spec]], model_data, main_family, family_name)
    key <- paste(marker, spec, sep = "::")
    model_store[[key]] <- ans$model
    diagnostic_rows[[length(diagnostic_rows) + 1L]] <-
      tibble(marker = marker, specification = spec,
             formula = paste(deparse(formulas[[spec]]), collapse = " ")) %>%
      bind_cols(ans$status)
  }

  # For kurat, fit reduced Poisson M2 as an additional audit/fallback.
  # For other categories, fit reduced NB2 M2 only if the full fit fails.
  full_ans <- diagnostic_rows[[length(diagnostic_rows) - 4L]]
  if (is_kurat || !isTRUE(full_ans$usable)) {
    ans <- fit_once(formulas$m2_reduced, model_data, main_family, family_name)
    model_store[[paste(marker, "m2_reduced", sep = "::")]] <- ans$model
    diagnostic_rows[[length(diagnostic_rows) + 1L]] <-
      tibble(marker = marker, specification = "m2_reduced",
             formula = paste(deparse(formulas$m2_reduced), collapse = " ")) %>%
      bind_cols(ans$status)
  }

  # Keep the failed/unstable NB2 alternative visible in the diagnostic audit.
  # These comparison models are NEVER used in main inference or FDR.
  if (is_kurat) {
    for (spec in c("m2_full", "m2_reduced")) {
      comparison_spec <- paste0(spec, "_nb2_comparison")
      ans <- fit_once(formulas[[spec]], model_data, nbinom2(), "NB2")
      model_store[[paste(marker, comparison_spec, sep = "::")]] <- ans$model
      diagnostic_rows[[length(diagnostic_rows) + 1L]] <-
        tibble(marker = marker, specification = comparison_spec,
               formula = paste(deparse(formulas[[spec]]), collapse = " ")) %>%
        bind_cols(ans$status)
    }
  }
}

diagnostics <- bind_rows(diagnostic_rows)
readr::write_csv(add_marker_labels(diagnostics), file.path(out_dir, "model_diagnostics.csv"))

# Select full Poisson M2 for kurat if it passes convergence/Hessian checks.
# All alternatives and failed fits remain explicitly recorded in diagnostics.
# Main NB2 outcomes use full M2 when valid and reduced M2 otherwise.
selection <- purrr::map_dfr(primary_markers, function(marker) {
  rows <- diagnostics %>% filter(.data$marker == .env$marker)
  full_ok <- rows %>% filter(specification == "m2_full") %>% pull(usable)
  reduced_ok <- rows %>% filter(specification == "m2_reduced") %>% pull(usable)

  if (identical(marker, "kurat")) {
    selected <- if (length(full_ok) == 1L && isTRUE(full_ok)) {
      "m2_full"
    } else if (length(reduced_ok) == 1L && isTRUE(reduced_ok)) {
      "m2_reduced"
    } else {
      NA_character_
    }
    selected_reason <- if (identical(selected, "m2_full")) {
      "Full Poisson M2 passed convergence/Hessian; NB2 dispersion/Hessian instability"
    } else if (identical(selected, "m2_reduced")) {
      "Full Poisson M2 failed diagnostics; reduced Poisson M2 fallback (review required)"
    } else {
      "No usable Poisson M2 for kurat"
    }
  } else {
    selected <- if (length(full_ok) == 1L && isTRUE(full_ok)) {
      "m2_full"
    } else if (length(reduced_ok) == 1L && isTRUE(reduced_ok)) {
      "m2_reduced"
    } else {
      NA_character_
    }
    selected_reason <- if (identical(selected, "m2_reduced")) {
      "Full NB2 M2 failed diagnostics; reduced NB2 M2 selected"
    } else if (identical(selected, "m2_full")) {
      "Full NB2 M2 passed diagnostics"
    } else {
      "No usable M2 found"
    }
  }

  family_selected <- if (identical(marker, "kurat")) "Poisson" else "NB2"
  selected_warnings <- if (is.na(selected)) TRUE else {
    any(nzchar(rows$warnings[rows$specification == selected]))
  }
  tibble(marker = marker,
         selected_model = selected,
         selected_family = family_selected,
         selected_reason = selected_reason,
         full_model_usable = length(full_ok) == 1L && isTRUE(full_ok),
         review_required = is.na(selected) || selected == "m2_reduced" ||
           selected_warnings)
})
readr::write_csv(add_marker_labels(selection), file.path(out_dir, "model_selection_review.csv"))

if (anyNA(selection$selected_model)) {
  stop("One or more primary models failed diagnostics; see model_diagnostics.csv")
}
if (!identical(selection$selected_model[selection$marker == "kurat"], "m2_full")) {
  warning("Full Poisson kurat M2 was not usable in this run; ",
          "see model_diagnostics.csv and review the fallback before publication.")
}

# Save the selected kurat model and both Poisson specifications for audit.
final_model_dir <- file.path(out_dir, "final_models")
dir.create(final_model_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(model_store[[paste0("kurat::", selection$selected_model[selection$marker == "kurat"])]],
        file.path(final_model_dir, "kurat_poisson_selected_m2.rds"))
saveRDS(model_store[["kurat::m2_full"]],
        file.path(final_model_dir, "kurat_poisson_full_m2.rds"))
saveRDS(model_store[["kurat::m2_reduced"]],
        file.path(final_model_dir, "kurat_poisson_reduced_m2.rds"))

# Store coefficients only for models with valid convergence/Hessian/SE.
all_coef <- purrr::map_dfr(seq_len(nrow(diagnostics)), function(i) {
  d <- diagnostics[i, ]
  if (!isTRUE(d$usable)) return(tibble())
  mod <- model_store[[paste(d$marker, d$specification, sep = "::")]]
  extract_fixed(mod) %>%
    mutate(marker = d$marker, specification = d$specification,
           family = d$family, .before = 1)
})
readr::write_csv(add_marker_labels(all_coef), file.path(out_dir, "all_estimable_coefficients.csv"))

main <- all_coef %>%
  inner_join(selection, by = "marker") %>%
  filter(specification == selected_model)

# 3. EXPLICIT MULTIPLE-TESTING FAMILIES -------------------------------------
# Main confirmatory families: 13 speaker-gender tests, and separately 13
# interlocutor-gender tests. Unadjusted Wald 95% CIs are retained.
main_gender_terms <- c("gender1", "second_speaker_gender1")
main_index <- tidyr::expand_grid(marker = primary_markers,
                                 term = main_gender_terms)
main_tests <- main_index %>%
  left_join(main %>% select(marker, term, selected_model, selected_family,
                            IRR, conf.low, conf.high, p.value),
            by = c("marker", "term")) %>%
  group_by(term) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  mutate(significant_fdr = !is.na(p_fdr) & p_fdr < 0.05)
stopifnot(nrow(main_tests) == 26L)
readr::write_csv(add_marker_labels(main_tests), file.path(out_dir, "main_gender_effects.csv"))

main <- main %>%
  left_join(main_tests %>% select(marker, term, p_fdr),
            by = c("marker", "term"))
readr::write_csv(add_marker_labels(main), file.path(out_dir, "main_model_coefficients.csv"))

# Exploratory interaction family: all 4 interaction types x 13 categories = 52.
# Adjustments do not pool these exploratory tests with confirmatory gender tests.
interaction_terms <- c(
  gender_age = "gender1:age",
  gender_age_diff = "gender1:age_diff",
  gender_interlocutor = "gender1:second_speaker_gender1",
  gender_year = "gender1:year_scaled"
)
interaction_index <- tidyr::expand_grid(
  marker = primary_markers,
  specification = names(interaction_terms)
) %>%
  mutate(term = unname(interaction_terms[specification]))
interaction_tests <- interaction_index %>%
  left_join(all_coef %>% select(marker, specification, term, family,
                               IRR, conf.low, conf.high, p.value),
            by = c("marker", "specification", "term")) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH"),
         significant_fdr = !is.na(p_fdr) & p_fdr < 0.05)
stopifnot(nrow(interaction_tests) == 52L)
readr::write_csv(add_marker_labels(interaction_tests), file.path(out_dir, "interaction_effects.csv"))

# 4. BASIC FIXED-EFFECT COLLINEARITY DIAGNOSTICS -----------------------------
# Classical VIF computed from the full fixed-effects design matrix.
# These values do not replace model-specific numerical diagnostics.
X <- model.matrix(~ gender + age + year_scaled + conditions + age_diff +
                    second_speaker_gender, data = model_data)
X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
vif <- vapply(seq_len(ncol(X)), function(j) {
  y <- X[, j]
  others <- X[, -j, drop = FALSE]
  r <- lm.fit(x = cbind(Intercept = 1, others), y = y)$residuals
  r2 <- 1 - sum(r^2) / sum((y - mean(y))^2)
  if (r2 >= 1) Inf else 1 / (1 - r2)
}, numeric(1))
collinearity <- tibble(predictor = colnames(X), VIF = vif)
readr::write_csv(collinearity, file.path(out_dir, "collinearity_vif.csv"))

# 5. CLEAN TABLES, PLOTS, AND REPRODUCIBILITY -------------------------------
openxlsx::write.xlsx(
  list(
    "Corpus" = corpus_summary,
    "Marker descriptions" = add_marker_labels(marker_descriptives),
    "Model diagnostics" = add_marker_labels(diagnostics),
    "Model selection" = add_marker_labels(selection),
    "Main gender effects" = add_marker_labels(main_tests),
    "Main coefficients" = add_marker_labels(main),
    "Exploratory interactions" = add_marker_labels(interaction_tests),
    "Collinearity VIF" = collinearity,
    "Marker labels" = marker_dictionary
  ),
  file = file.path(out_dir, "results_revised.xlsx"),
  overwrite = TRUE
)

# Exactly two publication figures: Section 4.1 and Section 4.2.
# Exploratory interactions (Section 4.3) are CSV/XLSX only, not plotted.
make_main_forest <- function(term_code, title, filename_stem) {
  df <- main_tests %>%
    filter(.data$term == term_code) %>%
    add_marker_labels()
  if (nrow(df) != 13L || anyDuplicated(df$marker) ||
      !setequal(df$marker, primary_markers)) {
    stop("Expected 13 unique marker categories for ", term_code)
  }
  if (anyNA(df$significant_fdr) ||
      any(!is.finite(df$IRR) | !is.finite(df$conf.low) |
          !is.finite(df$conf.high) | df$conf.low <= 0 |
          df$conf.high <= 0 | df$IRR <= 0)) {
    stop("Non-estimable main effect for ", term_code,
         "; check model diagnostics before generating a figure")
  }
  df <- df %>%
    mutate(marker_label = factor(
      .data$marker_label,
      levels = rev(unname(marker_labels[marker_display_order]))
    ))

  p <- ggplot(df, aes(x = .data$IRR, y = .data$marker_label)) +
    geom_vline(xintercept = 1, linetype = "dashed",
               linewidth = 0.5, color = "grey45") +
    geom_segment(aes(x = .data$conf.low, xend = .data$conf.high,
                     yend = .data$marker_label),
                 linewidth = 0.65, color = "grey55") +
    geom_point(aes(shape = .data$significant_fdr),
               size = 2.9, stroke = 0.85, color = "black") +
    scale_shape_manual(
      values = c(`TRUE` = 16, `FALSE` = 1),
      breaks = c("TRUE", "FALSE"),
      labels = c("FDR < .05", "Not significant"),
      name = "Inference"
    ) +
    scale_x_log10() +
    labs(
      title = title,
      subtitle = "13 marker categories; unadjusted 95% Wald CIs; BH-adjusted p-values",
      x = "Incidence rate ratio (log scale)", y = NULL,
      caption = paste(
        "Dashed line: IRR = 1 (no difference).",
        "Poisson GLMM: obscene particles. NB2 GLMMs: other categories.",
        "Reduced NB2 adjusted models: polite words and certainty/confirmation.",
        sep = "\n"
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.caption = element_text(size = 9, hjust = 0),
      plot.margin = margin(12, 22, 12, 12)
    )

  ggsave(file.path(out_dir, paste0(filename_stem, ".png")), p,
         width = 11.5, height = 7.4, dpi = 400, bg = "white")
  ggsave(file.path(out_dir, paste0(filename_stem, ".pdf")), p,
         width = 11.5, height = 7.4, bg = "white")
  invisible(p)
}

# Fig. 1: 4 FDR-significant effects (provided final data match the manuscript).
forest_speaker_gender <- make_main_forest(
  "gender1", "Speaker's gender effects", "forest_speaker_gender"
)
# Fig. 2: no FDR-significant effects in the final analysis.
forest_interlocutor_gender <- make_main_forest(
  "second_speaker_gender1", "Interlocutor's gender effects",
  "forest_interlocutor_gender"
)

writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info.txt"))
cat("\nCompleted. Outputs saved in:", normalizePath(out_dir), "\n")
cat("Primary marker categories:", length(primary_markers), "\n")
cat("Main gender tests:", nrow(main_tests), "(two separate families of 13)\n")
cat("Exploratory interaction tests:", nrow(interaction_tests), "\n")
cat("Models requiring review:", sum(selection$review_required), "\n")
cat("Selected kurat model:", selection$selected_model[selection$marker == "kurat"],
    "Poisson GLMM (NB2 comparisons retained as diagnostics).\n")
cat("New outputs were written to results/poisson_full_revision; previous results are preserved.\n")
cat("Figures: forest_speaker_gender and forest_interlocutor_gender (PNG/PDF).\n")
cat("All marker-bearing CSV/XLSX tables include marker_label; original marker codes are unchanged.\n")
cat("IMPORTANT: examine model_diagnostics.csv before reporting IRRs.\n")
