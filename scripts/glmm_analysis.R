
rm(list = ls())

library(tidyverse)
library(glmmTMB)
library(purrr)
library(readr)
library(janitor)
library(ggplot2)
library(patchwork)
library(openxlsx)
library(glue)

# ══════════════════════════════════════════════════════════════════════════════
# 1. LOAD AND PREPARE DATA
# ══════════════════════════════════════════════════════════════════════════════

data_dir <- ""
files    <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

dfs <- map(files, ~
             read_delim(.x, delim = NULL, show_col_types = FALSE) %>%
             clean_names()
)

names(dfs) <- tools::file_path_sans_ext(basename(files))

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

dfs <- map(dfs, ~ .x %>%
             mutate(
               id                    = as.factor(id),
               dialogue              = as.factor(dialogue),
               gender                = as.factor(gender),
               second_speaker_gender = as.factor(second_speaker_gender),
               conditions            = as.factor(conditions),
               age                   = to_num(age),
               year                  = to_num(year),
               age_diff              = to_num(age_diff),
               speaker_word_count    = to_num(speaker_word_count)
             ) %>%
             filter(speaker_word_count > 0) %>%
             mutate(
               year_scaled = as.numeric(scale(year))
             )
)

cat("Datasets loaded:", paste(names(dfs), collapse = ", "), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. IDENTIFY LINGUISTIC MARKERS
# ══════════════════════════════════════════════════════════════════════════════

common_cols <- c(
  "dialogue", "id", "conditions", "gender",
  "year", "year_scaled", "age", "education",
  "age_diff", "second_speaker_gender", "speaker_word_count"
)

tasks <- imap_dfr(dfs, ~ {
  markers <- setdiff(names(.x), common_cols)
  markers <- markers[map_lgl(.x[markers], is.numeric)]
  tibble(dataset = .y, marker = markers)
})

cat("Total markers found:", nrow(tasks), "\n")
print(tasks)

# ══════════════════════════════════════════════════════════════════════════════
# 3. MODELS
#
# Model 0: null baseline
# Model 1: gender only
# Model 2: adjusted model
#
# Interaction models:
#   gender × age
#   gender × age_diff
#   gender × second_speaker_gender
#   gender × year_scaled
# ══════════════════════════════════════════════════════════════════════════════

fit_models <- function(df, response) {
  
  df[[response]] <- tidyr::replace_na(df[[response]], 0)
  
  if (dplyr::n_distinct(df[[response]]) < 2) return(NULL)
  
  off <- "offset(log(speaker_word_count))"
  re  <- "(1 | id) + (1 | dialogue)"
  
  fml_m2 <- as.formula(paste0(
    response,
    " ~ gender + age + year_scaled + conditions + age_diff + second_speaker_gender + ",
    off, " + ", re
  ))
  
  fml_m2r <- as.formula(paste0(
    response,
    " ~ gender + age + conditions + second_speaker_gender + ",
    off, " + ", re
  ))
  
  formulas <- list(
    m0 = as.formula(paste0(
      response, " ~ ",
      off, " + ", re
    )),
    
    m1 = as.formula(paste0(
      response, " ~ gender + ",
      off, " + ", re
    )),
    
    m2 = fml_m2,
    
    gender_age = as.formula(paste0(
      response,
      " ~ gender * age + year_scaled + conditions + age_diff + second_speaker_gender + ",
      off, " + ", re
    )),
    
    gender_age_diff = as.formula(paste0(
      response,
      " ~ gender * age_diff + age + year_scaled + conditions + second_speaker_gender + ",
      off, " + ", re
    )),
    
    gender_interlocutor = as.formula(paste0(
      response,
      " ~ gender * second_speaker_gender + age + year_scaled + conditions + age_diff + ",
      off, " + ", re
    )),
    
    gender_year = as.formula(paste0(
      response,
      " ~ gender * year_scaled + age + conditions + age_diff + second_speaker_gender + ",
      off, " + ", re
    ))
  )
  
  fit_one <- function(fml) {
    w <- character()
    
    m <- tryCatch(
      withCallingHandlers(
        glmmTMB(fml, data = df, family = nbinom2()),
        warning = function(e) {
          w <<- c(w, conditionMessage(e))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) NULL
    )
    
    list(
      model = m,
      warnings = paste(unique(w), collapse = " | ")
    )
  }
  
  results <- map(formulas, fit_one)
  
  m2_failed <- is.null(results$m2$model) || results$m2$warnings != ""
  
  if (m2_failed) {
    results$m2 <- fit_one(fml_m2r)
    results$m2$warnings <- paste0(
      "[reduced: dropped age_diff + year_scaled] ",
      results$m2$warnings
    )
  }
  
  results
}

safe_fit <- purrr::safely(fit_models)

get_model <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (is.null(x[[name]])) return(NULL)
  x[[name]]$model %||% NULL
}

get_warning <- function(x, name) {
  if (is.null(x)) return("")
  if (is.null(x[[name]])) return("")
  x[[name]]$warnings %||% ""
}

cat("\nFitting models...\n")

tasks2 <- tasks %>%
  mutate(
    fit    = map2(dataset, marker, ~ safe_fit(dfs[[.x]], .y)),
    result = map(fit, "result"),
    error  = map(fit, "error"),
    
    m0 = map(result, get_model, "m0"),
    m1 = map(result, get_model, "m1"),
    m2 = map(result, get_model, "m2"),
    
    gender_age          = map(result, get_model, "gender_age"),
    gender_age_diff     = map(result, get_model, "gender_age_diff"),
    gender_interlocutor = map(result, get_model, "gender_interlocutor"),
    gender_year         = map(result, get_model, "gender_year"),
    
    warn_m0 = map_chr(result, get_warning, "m0"),
    warn_m1 = map_chr(result, get_warning, "m1"),
    warn_m2 = map_chr(result, get_warning, "m2"),
    
    warn_gender_age          = map_chr(result, get_warning, "gender_age"),
    warn_gender_age_diff     = map_chr(result, get_warning, "gender_age_diff"),
    warn_gender_interlocutor = map_chr(result, get_warning, "gender_interlocutor"),
    warn_gender_year         = map_chr(result, get_warning, "gender_year")
  )

cat("\nConvergence summary:\n")

tasks2 %>%
  summarise(
    m0_ok = sum(!map_lgl(m0, is.null)),
    m1_ok = sum(!map_lgl(m1, is.null)),
    m2_ok = sum(!map_lgl(m2, is.null)),
    gender_age_ok = sum(!map_lgl(gender_age, is.null)),
    gender_age_diff_ok = sum(!map_lgl(gender_age_diff, is.null)),
    gender_interlocutor_ok = sum(!map_lgl(gender_interlocutor, is.null)),
    gender_year_ok = sum(!map_lgl(gender_year, is.null)),
    total = n()
  ) %>%
  print()

cat("\nMarkers with warnings, Model 2:\n")

tasks2 %>%
  filter(warn_m2 != "") %>%
  select(dataset, marker, warn_m2) %>%
  print()

# ══════════════════════════════════════════════════════════════════════════════
# 4. EXTRACT FIXED EFFECTS
# ══════════════════════════════════════════════════════════════════════════════

extract_fixed <- function(m) {
  co <- as.data.frame(summary(m)$coefficients$cond)
  
  tibble::rownames_to_column(co, var = "term") %>%
    rename(
      estimate  = Estimate,
      std.error = `Std. Error`,
      statistic = `z value`,
      p.value   = `Pr(>|z|)`
    ) %>%
    select(term, estimate, std.error, statistic, p.value)
}

results_all <- tasks2 %>%
  filter(!map_lgl(m2, is.null)) %>%
  mutate(fixed = map(m2, extract_fixed)) %>%
  unnest(fixed) %>%
  mutate(
    IRR        = exp(estimate),
    conf.low   = exp(estimate - 1.96 * std.error),
    conf.high  = exp(estimate + 1.96 * std.error),
    model_type = ifelse(startsWith(warn_m2, "[reduced"), "reduced", "full"),
    converged  = warn_m2 == "" | model_type == "reduced"
  )

results_ok <- results_all %>%
  filter(converged) %>%
  group_by(term) %>%
  mutate(p_adj = p.adjust(p.value, method = "fdr")) %>%
  ungroup()

interaction_models <- c(
  "gender_age",
  "gender_age_diff",
  "gender_interlocutor",
  "gender_year"
)

results_interactions <- tasks2 %>%
  select(dataset, marker, all_of(interaction_models)) %>%
  pivot_longer(
    cols = all_of(interaction_models),
    names_to = "interaction_model",
    values_to = "model"
  ) %>%
  filter(!map_lgl(model, is.null)) %>%
  mutate(fixed = map(model, extract_fixed)) %>%
  unnest(fixed) %>%
  filter(str_detect(term, ":")) %>%
  mutate(
    IRR       = exp(estimate),
    conf.low  = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error)
  ) %>%
  group_by(interaction_model, term) %>%
  mutate(p_adj = p.adjust(p.value, method = "fdr")) %>%
  ungroup()

# ══════════════════════════════════════════════════════════════════════════════
# 5. OUTPUT FOLDER
# ══════════════════════════════════════════════════════════════════════════════

run_date <- format(Sys.Date(), "%Y-%m-%d")
out_dir  <- file.path(data_dir, glue("results_{run_date}"))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat("\nOutput folder:", out_dir, "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 6. FOREST PLOTS
# ══════════════════════════════════════════════════════════════════════════════

COL_SIG  <- "#1d6b9e"
COL_NS   <- "#a0b4c7"
COL_CI   <- "#8da8bc"
COL_VREF <- "#cc0000"

make_forest_plot <- function(data,
                             term_filter,
                             title,
                             use_fdr  = TRUE,
                             x_breaks = c(0.25, 0.5, 1, 2, 4),
                             x_limits = NULL) {
  
  sig_var   <- if (use_fdr) "p_adj"     else "p.value"
  sig_label <- if (use_fdr) "FDR < .05" else "p < .05"
  ns_label  <- if (use_fdr) "ns (FDR)"  else "ns (p)"
  
  df <- data %>%
    filter(term == term_filter) %>%
    mutate(
      sig = if_else(!is.na(.data[[sig_var]]) & .data[[sig_var]] < 0.05, TRUE, FALSE),
      marker_label = str_replace_all(marker, "_", " "),
      marker_label = factor(marker_label, levels = marker_label[order(IRR)])
    )
  
  if (nrow(df) == 0) {
    warning("No data for: ", term_filter)
    return(NULL)
  }
  
  present_levels <- intersect(c("FALSE", "TRUE"), as.character(unique(df$sig)))
  
  legend_labels <- c(
    `FALSE` = ns_label,
    `TRUE`  = sig_label
  )
  
  legend_values <- c(
    `FALSE` = COL_NS,
    `TRUE`  = COL_SIG
  )
  
  n_sig    <- sum(df$sig, na.rm = TRUE)
  subtitle <- glue("n = {nrow(df)} markers; significant: {n_sig}")
  
  ggplot(df, aes(x = IRR, y = marker_label)) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      color = COL_VREF,
      linewidth = 0.5
    ) +
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      height = 0.25,
      color = COL_CI,
      linewidth = 0.45
    ) +
    geom_point(
      aes(fill = sig),
      shape = 21,
      size = 2.7,
      color = "white",
      stroke = 0.3
    ) +
    scale_fill_manual(
      values = legend_values,
      breaks = present_levels,
      labels = legend_labels[present_levels],
      name   = "Significance"
    ) +
    scale_x_log10(
      breaks = x_breaks,
      labels = as.character(x_breaks),
      limits = x_limits
    ) +
    labs(
      x = "Incidence Rate Ratio (log scale)",
      y = NULL,
      title = title,
      subtitle = subtitle,
      caption = "Negative binomial regression. 95% CI. FDR: Benjamini-Hochberg."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
      
      axis.text.y        = element_text(size = 9, color = "grey20"),
      axis.text.x        = element_text(size = 9, color = "grey20"),
      axis.title.x       = element_text(
        size = 10,
        color = "grey20",
        margin = margin(t = 6)
      ),
      
      legend.position    = "bottom",
      legend.text        = element_text(size = 9, color = "grey20"),
      legend.title       = element_text(size = 9, color = "grey20"),
      
      plot.title         = element_text(face = "bold", size = 13, color = "grey15"),
      plot.subtitle      = element_text(size = 10, color = "grey35"),
      plot.caption       = element_text(size = 8, color = "grey45", hjust = 0),
      plot.margin        = margin(10, 15, 10, 10)
    )
}

make_forest_plot_pattern <- function(data,
                                     term_pattern,
                                     title,
                                     use_fdr  = TRUE,
                                     x_breaks = c(0.25, 0.5, 1, 2, 4),
                                     x_limits = NULL) {
  
  sig_var   <- if (use_fdr) "p_adj"     else "p.value"
  sig_label <- if (use_fdr) "FDR < .05" else "p < .05"
  ns_label  <- if (use_fdr) "ns (FDR)"  else "ns (p)"
  
  df <- data %>%
    filter(str_detect(term, term_pattern)) %>%
    mutate(
      sig = if_else(!is.na(.data[[sig_var]]) & .data[[sig_var]] < 0.05, TRUE, FALSE),
      marker_label = str_replace_all(marker, "_", " "),
      term_label = str_replace_all(term, "_", " "),
      plot_label = if_else(
        n_distinct(term) > 1,
        paste0(marker_label, " | ", term_label),
        marker_label
      ),
      plot_label = factor(plot_label, levels = plot_label[order(IRR)])
    )
  
  if (nrow(df) == 0) {
    warning("No data for pattern: ", term_pattern)
    return(NULL)
  }
  
  present_levels <- intersect(c("FALSE", "TRUE"), as.character(unique(df$sig)))
  
  legend_labels <- c(
    `FALSE` = ns_label,
    `TRUE`  = sig_label
  )
  
  legend_values <- c(
    `FALSE` = COL_NS,
    `TRUE`  = COL_SIG
  )
  
  n_sig    <- sum(df$sig, na.rm = TRUE)
  subtitle <- glue("n = {nrow(df)} effects; significant: {n_sig}")
  
  ggplot(df, aes(x = IRR, y = plot_label)) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      color = COL_VREF,
      linewidth = 0.5
    ) +
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      height = 0.25,
      color = COL_CI,
      linewidth = 0.45
    ) +
    geom_point(
      aes(fill = sig),
      shape = 21,
      size = 2.7,
      color = "white",
      stroke = 0.3
    ) +
    scale_fill_manual(
      values = legend_values,
      breaks = present_levels,
      labels = legend_labels[present_levels],
      name   = "Significance"
    ) +
    scale_x_log10(
      breaks = x_breaks,
      labels = as.character(x_breaks),
      limits = x_limits
    ) +
    labs(
      x = "Incidence Rate Ratio (log scale)",
      y = NULL,
      title = title,
      subtitle = subtitle,
      caption = "Negative binomial regression. 95% CI. FDR: Benjamini-Hochberg."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
      
      axis.text.y        = element_text(size = 8.5, color = "grey20"),
      axis.text.x        = element_text(size = 9, color = "grey20"),
      axis.title.x       = element_text(
        size = 10,
        color = "grey20",
        margin = margin(t = 6)
      ),
      
      legend.position    = "bottom",
      legend.text        = element_text(size = 9, color = "grey20"),
      legend.title       = element_text(size = 9, color = "grey20"),
      
      plot.title         = element_text(face = "bold", size = 13, color = "grey15"),
      plot.subtitle      = element_text(size = 10, color = "grey35"),
      plot.caption       = element_text(size = 8, color = "grey45", hjust = 0),
      plot.margin        = margin(10, 15, 10, 10)
    )
}

p_speaker <- make_forest_plot(
  results_ok,
  "gender1",
  "Effect of speaker's gender"
)

p_interlocutor <- make_forest_plot(
  results_ok,
  "second_speaker_gender1",
  "Effect of interlocutor's gender"
)

p_age <- make_forest_plot(
  results_ok,
  "age",
  "Effect of speaker's age"
)

p_year <- make_forest_plot(
  results_ok,
  "year_scaled",
  "Effect of year of recording"
)

p_combined <- p_speaker + p_interlocutor +
  plot_annotation(
    title    = "Gender effects on discursive marker use",
    subtitle = "Model 2: adjusted model",
    caption  = "Negative binomial regression. Random effects: speaker + dialogue.",
    theme    = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(size = 10, color = "grey40"),
      plot.caption    = element_text(size = 8, color = "grey50")
    )
  )

p_gender_age <- make_forest_plot(
  results_interactions,
  "gender1:age",
  "Interaction: speaker's gender × age"
)

p_gender_age_diff <- make_forest_plot(
  results_interactions,
  "gender1:age_diff",
  "Interaction: speaker's gender × age difference"
)

p_gender_interlocutor <- make_forest_plot(
  results_interactions,
  "gender1:second_speaker_gender1",
  "Interaction: speaker's gender × interlocutor's gender"
)

p_gender_year <- make_forest_plot(
  results_interactions,
  "gender1:year_scaled",
  "Interaction: speaker's gender × year of recording"
)


p_interactions_combined <-
  p_gender_age +
  p_gender_age_diff +
  p_gender_interlocutor +
  p_gender_year +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Interaction effects on discursive marker use",
    subtitle = "Negative binomial models with speaker's gender interactions",
    caption = "IRR > 1 means the predictor effect is stronger for gender level 1 relative to the reference level.",
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(size = 10, color = "grey40"),
      plot.caption    = element_text(size = 8, color = "grey50")
    )
  )

n_markers   <- results_ok %>% distinct(marker) %>% nrow()
plot_height <- max(6, n_markers * 0.32)


# ══════════════════════════════════════════════════════════════════════════════
# 7. TABLES
# ══════════════════════════════════════════════════════════════════════════════

term_labels <- c(
  "(Intercept)"                    = "Intercept",
  "gender1"                        = "Speaker's gender",
  "second_speaker_gender1"         = "Interlocutor's gender",
  "age"                            = "Age",
  "year_scaled"                    = "Year (scaled)",
  "conditions1"                    = "Condition",
  "age_diff"                       = "Age difference",
  "gender1:age"                    = "Speaker's gender × age",
  "gender1:age_diff"               = "Speaker's gender × age difference",
  "gender1:second_speaker_gender1" = "Speaker's gender × interlocutor's gender",
  "gender1:year_scaled"            = "Speaker's gender × year"
)

sig_stars <- function(p) {
  case_when(
    is.na(p)   ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ ".",
    TRUE      ~ ""
  )
}

make_pretty_term <- function(term) {
  case_when(
    term %in% names(term_labels) ~ recode(term, !!!term_labels),
    TRUE ~ term
  )
}

table_full <- results_ok %>%
  mutate(
    term_label = make_pretty_term(term),
    IRR        = round(IRR, 3),
    conf.low   = round(conf.low, 3),
    conf.high  = round(conf.high, 3),
    CI_95      = glue("[{conf.low}, {conf.high}]"),
    estimate   = round(estimate, 4),
    std.error  = round(std.error, 4),
    statistic  = round(statistic, 3),
    p.value    = round(p.value, 4),
    p_adj      = round(p_adj, 4),
    sig        = sig_stars(p.value),
    sig_fdr    = sig_stars(p_adj)
  ) %>%
  select(
    Dataset     = dataset,
    Marker      = marker,
    Predictor   = term_label,
    Model       = model_type,
    `b (log)`   = estimate,
    SE          = std.error,
    z           = statistic,
    IRR,
    `95% CI`    = CI_95,
    p           = p.value,
    `p (FDR)`   = p_adj,
    Sig         = sig,
    `Sig (FDR)` = sig_fdr
  ) %>%
  arrange(Dataset, Marker, Predictor)

table_gender <- results_ok %>%
  filter(term %in% c("gender1", "second_speaker_gender1")) %>%
  mutate(
    Predictor = make_pretty_term(term),
    IRR       = round(IRR, 3),
    conf.low  = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    CI_95     = glue("[{conf.low}, {conf.high}]"),
    p.value   = round(p.value, 4),
    p_adj     = round(p_adj, 4),
    sig_fdr   = sig_stars(p_adj)
  ) %>%
  select(
    Dataset     = dataset,
    Marker      = marker,
    Predictor,
    IRR,
    `95% CI`    = CI_95,
    p           = p.value,
    `p (FDR)`   = p_adj,
    `Sig (FDR)` = sig_fdr
  ) %>%
  arrange(Dataset, Predictor, desc(IRR))

table_interactions <- results_interactions %>%
  mutate(
    Predictor = make_pretty_term(term),
    IRR       = round(IRR, 3),
    conf.low  = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    CI_95     = glue("[{conf.low}, {conf.high}]"),
    estimate  = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 3),
    p.value   = round(p.value, 4),
    p_adj     = round(p_adj, 4),
    sig       = sig_stars(p.value),
    sig_fdr   = sig_stars(p_adj)
  ) %>%
  select(
    Dataset     = dataset,
    Marker      = marker,
    Model       = interaction_model,
    Predictor,
    Term        = term,
    `b (log)`   = estimate,
    SE          = std.error,
    z           = statistic,
    IRR,
    `95% CI`    = CI_95,
    p           = p.value,
    `p (FDR)`   = p_adj,
    Sig         = sig,
    `Sig (FDR)` = sig_fdr
  ) %>%
  arrange(Dataset, Model, Marker)


table_summary <- results_ok %>%
  group_by(dataset, marker) %>%
  summarise(
    n_predictors  = n(),
    n_sig_p       = sum(p.value < 0.05, na.rm = TRUE),
    n_sig_fdr     = sum(p_adj   < 0.05, na.rm = TRUE),
    speaker_IRR   = round(IRR[term == "gender1"][1], 3),
    speaker_p_fdr = round(p_adj[term == "gender1"][1], 4),
    speaker_sig   = sig_stars(speaker_p_fdr),
    interl_IRR    = round(IRR[term == "second_speaker_gender1"][1], 3),
    interl_p_fdr  = round(p_adj[term == "second_speaker_gender1"][1], 4),
    interl_sig    = sig_stars(interl_p_fdr),
    .groups = "drop"
  ) %>%
  rename(
    Dataset                         = dataset,
    Marker                          = marker,
    `N predictors`                  = n_predictors,
    `Sig (p < .05)`                 = n_sig_p,
    `Sig (FDR)`                     = n_sig_fdr,
    `IRR (Speaker's gender)`        = speaker_IRR,
    `p_FDR (Speaker's gender)`      = speaker_p_fdr,
    `Sig (Speaker's gender)`        = speaker_sig,
    `IRR (Interlocutor's gender)`   = interl_IRR,
    `p_FDR (Interlocutor's gender)` = interl_p_fdr,
    `Sig (Interlocutor's gender)`   = interl_sig
  ) %>%
  arrange(Dataset, Marker)

# ══════════════════════════════════════════════════════════════════════════════
# 8. EXCEL
# ══════════════════════════════════════════════════════════════════════════════

style_header <- createStyle(
  fontName       = "Arial",
  fontSize       = 10,
  fontColour     = "#FFFFFF",
  fgFill         = "#1d6b9e",
  halign         = "CENTER",
  valign         = "CENTER",
  textDecoration = "bold",
  wrapText       = TRUE
)

style_body <- createStyle(
  fontName = "Arial",
  fontSize = 9,
  halign = "LEFT"
)

style_num <- createStyle(
  fontName = "Arial",
  fontSize = 9,
  halign = "RIGHT",
  numFmt = "0.000"
)

style_sig <- createStyle(fgFill = "#ddeef7")
style_alt <- createStyle(fgFill = "#f5f5f5")

write_sheet <- function(wb, sheet_name, df, sig_col = NULL, num_cols = NULL) {
  
  addWorksheet(wb, sheet_name, gridLines = FALSE)
  
  writeData(
    wb,
    sheet_name,
    df,
    startRow = 1,
    startCol = 1,
    headerStyle = style_header
  )
  
  nr <- nrow(df)
  nc <- ncol(df)
  
  if (nr > 0) {
    for (r in seq(2, nr + 1, by = 2)) {
      addStyle(
        wb,
        sheet_name,
        style_alt,
        rows = r,
        cols = 1:nc,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
    
    if (!is.null(sig_col) && sig_col %in% names(df)) {
      sig_rows <- which(df[[sig_col]] != "") + 1
      
      if (length(sig_rows) > 0) {
        addStyle(
          wb,
          sheet_name,
          style_sig,
          rows = sig_rows,
          cols = 1:nc,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
    
    if (!is.null(num_cols) && length(num_cols) > 0) {
      addStyle(
        wb,
        sheet_name,
        style_num,
        rows = 2:(nr + 1),
        cols = num_cols,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
    
    addStyle(
      wb,
      sheet_name,
      style_body,
      rows = 2:(nr + 1),
      cols = 1:nc,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  
  setColWidths(wb, sheet_name, cols = 1:nc, widths = "auto")
  freezePane(wb, sheet_name, firstRow = TRUE)
  addFilter(wb, sheet_name, row = 1, cols = 1:nc)
}

wb <- createWorkbook()

options("openxlsx.borderColour" = "#CCCCCC")

write_sheet(
  wb,
  "All results",
  table_full,
  sig_col = "Sig (FDR)",
  num_cols = which(names(table_full) %in%
                     c("b (log)", "SE", "z", "IRR", "p", "p (FDR)"))
)

write_sheet(
  wb,
  "Gender effects",
  table_gender,
  sig_col = "Sig (FDR)",
  num_cols = which(names(table_gender) %in%
                     c("IRR", "p", "p (FDR)"))
)

write_sheet(
  wb,
  "Interactions",
  table_interactions,
  sig_col = "Sig (FDR)",
  num_cols = which(names(table_interactions) %in%
                     c("b (log)", "SE", "z", "IRR", "p", "p (FDR)"))
)


write_sheet(
  wb,
  "Summary by marker",
  table_summary,
  sig_col = "Sig (Speaker's gender)",
  num_cols = which(names(table_summary) %in%
                     c("IRR (Speaker's gender)", "p_FDR (Speaker's gender)",
                       "IRR (Interlocutor's gender)",
                       "p_FDR (Interlocutor's gender)"))
)

addWorksheet(wb, "Legend", gridLines = FALSE)

writeData(
  wb,
  "Legend",
  data.frame(
    Symbol      = c("***", "**", "*", ".", ""),
    Threshold   = c("p < .001", "p < .01", "p < .05", "p < .10", "ns"),
    Description = c(
      "Highly significant",
      "Very significant",
      "Significant",
      "Marginal",
      "Not significant"
    ),
    Note = c(
      "FDR columns: Benjamini-Hochberg correction per predictor",
      "Interaction models test moderation by speaker's gender (excluding recording condition)",
      "Model 2: gender + age + year_scaled + conditions + age_diff + interlocutor gender",
      "Random effects: (1|id) + (1|dialogue)",
      "Family: Negative Binomial nbinom2; offset: log(speaker_word_count)"
    )
  ),
  headerStyle = style_header
)

setColWidths(wb, "Legend", cols = 1:4, widths = c(10, 15, 25, 70))

# ══════════════════════════════════════════════════════════════════════════════
# 9. SAVE ALL OUTPUT
# ══════════════════════════════════════════════════════════════════════════════

saveWorkbook(
  wb,
  file.path(out_dir, "results_table.xlsx"),
  overwrite = TRUE
)

write_csv(results_ok,               file.path(out_dir, "model_results_converged.csv"))
write_csv(results_all,              file.path(out_dir, "model_results_all.csv"))
write_csv(results_interactions,     file.path(out_dir, "model_results_interactions.csv"))

write_csv(table_full,               file.path(out_dir, "table_full.csv"))
write_csv(table_gender,             file.path(out_dir, "table_gender.csv"))
write_csv(table_interactions,       file.path(out_dir, "table_interactions.csv"))
write_csv(table_summary,            file.path(out_dir, "table_summary.csv"))

save_forest <- function(plot, name, width = 7, height = plot_height) {
  
  if (is.null(plot)) {
    warning("Plot is NULL, skipped: ", name)
    return(invisible(NULL))
  }
  
  ggsave(
    file.path(out_dir, paste0(name, ".pdf")),
    plot,
    width = width,
    height = height,
    units = "in",
    device = cairo_pdf,
    bg = "white"
  )
  
  ggsave(
    file.path(out_dir, paste0(name, ".png")),
    plot,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )
  
  ggsave(
    file.path(out_dir, paste0(name, ".tiff")),
    plot,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    compression = "lzw",
    bg = "white"
  )
}

save_forest(p_speaker,      "forest_speaker_gender")
save_forest(p_interlocutor, "forest_interlocutor_gender")
save_forest(p_age,          "forest_age")
save_forest(p_year,         "forest_year")

save_forest(
  p_combined,
  "forest_gender_combined",
  width = 14,
  height = plot_height
)

save_forest(p_gender_age,          "forest_interaction_gender_age")
save_forest(p_gender_age_diff,     "forest_interaction_gender_age_diff")
save_forest(p_gender_interlocutor, "forest_interaction_gender_interlocutor")
save_forest(p_gender_year,         "forest_interaction_gender_year")


save_forest(
  p_interactions_combined,
  "forest_interactions_combined",
  width = 14,
  height = plot_height
)

cat("\n══════════════════════════════════════════════\n")
cat("Done. Saved to:", out_dir, "\n")
cat("  results_table.xlsx\n")
cat("  model_results_converged.csv\n")
cat("  model_results_all.csv\n")
cat("  model_results_interactions.csv\n")
cat("  table_full / gender / interactions / summary .csv\n")
cat("  forest_*.pdf\n")
cat("  forest_*.png, 600 dpi\n")
cat("  forest_*.tiff, 600 dpi\n")
cat("══════════════════════════════════════════════\n")

