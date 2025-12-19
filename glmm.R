
## glmmTMB      : fits generalized linear mixed models incl. negative binomial
## DHARMa       : simulation-based residual diagnostics for GLMMs
## broom.mixed  : extracts model results into tidy data frames
## dplyr/tidyr  : data manipulation
## ggplot2      : plotting
## ggeffects    : marginal predictions (easy, publication-friendly)

packages <- c("glmmTMB","DHARMa","broom.mixed","dplyr","tidyr","ggplot2","ggeffects")
install.packages(setdiff(packages, rownames(installed.packages())))
invisible(lapply(packages, library, character.only = TRUE))



## Load and preprocess data


df <- read.csv(
  "Desktop/Uni/phd/Статья 1/dialogue_speakers.csv",
  encoding = "UTF-8"
)

## Convert identifiers and categorical predictors to factors
df$ID        <- as.factor(df$ID)         # speaker ID
df$dialogue  <- as.factor(df$dialogue)   # dialogue/conversation ID
df$gender    <- as.factor(df$gender)     # main social predictor
df$condition <- as.factor(df$conditions) # experimental/contextual condition

## Numeric predictors
df$age       <- as.numeric(df$age)
df$year      <- as.numeric(df$year)
df$age_diff  <- as.numeric(df$age_diff)

##Scale year for stability and interpretability
## After scaling: 0 = mean year, 1 = one SD above mean.
df$year_scaled <- scale(df$year)

## offset(log(speaker_word_count)) requires word_count > 0
df <- df[df$speaker_word_count > 0, ]



## Define particle inventory used in analysis

## These are the particles you decided to keep for modeling (speaker-level sufficient).
## kurat IS included (as requested).
particles_individual <- c(
  "no","vot","noh","jah","okei","mhmh","vaat","oot","jumal","kurat"
)

## Optional: quick sanity check that all columns exist
stopifnot(all(particles_individual %in% names(df)))



## Frequency diagnostics (token vs speaker-level)
## =========================================================
## token_freq   : total number of tokens across the whole corpus
## speaker_freq : number of speaker×dialogue rows with at least one token (>0)
##
## NOTE:
## - token_freq is good for corpus description
## - speaker_freq matters for model stability (how many non-zero observations)

freq_table <- data.frame(
  particle = particles_individual,
  token_freq = colSums(df[particles_individual], na.rm = TRUE),
  speaker_freq = sapply(particles_individual, function(p) sum(df[[p]] > 0, na.rm = TRUE))
)

print(freq_table)


## =========================================================
## Global particle usage: ALL particles together
## =========================================================
## Goal:
## - test whether speakers differ in their overall propensity to use particles,
##   regardless of which specific particle.
##
## We sum the counts of the selected particles and model the rate per word.

df <- df %>%
  mutate(
    all_particles = rowSums(select(., all_of(particles_individual)), na.rm = TRUE)
  )

m_all_particles <- glmmTMB(
  all_particles ~ gender + condition + year_scaled + age + age_diff +
    offset(log(speaker_word_count)) +
    (1 | ID) + (1 | dialogue),
  data = df,
  family = nbinom2()
)

## Extract fixed effects into a tidy table (with confidence intervals)
res_all_particles <- broom.mixed::tidy(m_all_particles, effects = "fixed", conf.int = TRUE) %>%
  mutate(outcome = "all_particles", model_type = "NB_global")


## =========================================================
## KP markers (composite pragmatic strategy)
## =========================================================
## Goal:
## - analyze a composite marker class (kp_markers) as a pragmatic strategy
##   (e.g., epistemic/hedging markers).
##
## This is a COUNT outcome, so NB + offset is appropriate.
stopifnot("kp_markers" %in% names(df))

m_kp <- glmmTMB(
  kp_markers ~ gender + condition + year_scaled + age + age_diff +
    offset(log(speaker_word_count)) +
    (1 | ID) + (1 | dialogue),
  data = df,
  family = nbinom2()
)

res_kp <- broom.mixed::tidy(m_kp, effects = "fixed", conf.int = TRUE) %>%
  mutate(outcome = "kp_markers", model_type = "NB_composite")


## =========================================================
## Functional classes of particles (composites)
## =========================================================
## Goal:
## - test functional classes (strategies) rather than individual lexemes.
##
## IMPORTANT:
## - These composites should be theory-motivated and documented in the paper.

df <- df %>%
  mutate(
    regulators   = no + noh + vot,     # example class: discourse regulators
    backchannels = mhmh,               # example class: listener response/backchannel
    epistemic    = vaat + oot,         # example class: epistemic stance
    affective    = jumal + kurat       # affective/swear/affect markers
  )

class_vars <- c("regulators","backchannels","epistemic","affective")

class_models  <- list()
class_results <- data.frame()

for (cl in class_vars) {
  
  f <- as.formula(paste0(
    cl,
    " ~ gender + condition + year_scaled + age + age_diff + ",
    "offset(log(speaker_word_count)) + ",
    "(1 | ID) + (1 | dialogue)"
  ))
  
  m <- glmmTMB(f, data = df, family = nbinom2())
  
  class_models[[cl]] <- m
  
  out <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    mutate(outcome = cl, model_type = "NB_class")
  
  class_results <- bind_rows(class_results, out)
}


## =========================================================
## Individual particles (NB-GLMMs)
## =========================================================
## Goal:
## - examine specific lexical items (particles) individually.
##
## NOTE:
## - This often produces many tests; we will control FDR later.

particle_models  <- list()
particle_results <- data.frame()

for (p in particles_individual) {
  
  f <- as.formula(paste0(
    p,
    " ~ gender + condition + year_scaled + age + age_diff + ",
    "offset(log(speaker_word_count)) + ",
    "(1 | ID) + (1 | dialogue)"
  ))
  
  m <- glmmTMB(f, data = df, family = nbinom2())
  particle_models[[p]] <- m
  
  out <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    mutate(outcome = p, model_type = "NB_individual")
  
  particle_results <- bind_rows(particle_results, out)
}


## =========================================================
## Lexical choice model: kurat vs jumal
## =========================================================
## Goal:
## - model RELATIVE preference: if a speaker uses either of the two,
##   which one do they choose?
##
## Why binomial?
## - the denominator is (kurat + jumal), so no offset is needed.
## - this is a choice model, not a rate model.

df_kj <- df %>%
  mutate(total_swear = kurat + jumal) %>%
  filter(total_swear > 0)

m_kurat_vs_jumal <- glmmTMB(
  cbind(kurat, jumal) ~ gender + condition + year_scaled + age + age_diff +
    (1 | ID) + (1 | dialogue),
  data = df_kj,
  family = binomial()
)

res_kj <- broom.mixed::tidy(m_kurat_vs_jumal, effects = "fixed", conf.int = TRUE) %>%
  mutate(outcome = "kurat_vs_jumal", model_type = "binomial_choice")


## =========================================================
## Sina/Teie choice model: sina vs teie
## =========================================================
## Same logic: choice model (binomial), no offset.

stopifnot(all(c("sinatama","tietama") %in% names(df)))

df_tv <- df %>%
  mutate(total_tv = sinatama + tietama) %>%
  filter(total_tv > 0)

m_tv <- glmmTMB(
  cbind(sinatama, tietama) ~ gender + condition + year_scaled + age + age_diff +
    (1 | ID) + (1 | dialogue),
  data = df_tv,
  family = binomial()
)

res_tv <- broom.mixed::tidy(m_tv, effects = "fixed", conf.int = TRUE) %>%
  mutate(outcome = "sina_vs_teie", model_type = "binomial_choice")


## =========================================================
## Combine all results into one table
## =========================================================
all_results <- bind_rows(
  res_all_particles,
  res_kp,
  class_results,
  particle_results,
  res_kj,
  res_tv
)


## =========================================================
##  Multiple testing correction (FDR / BH)
## =========================================================
## IMPORTANT:
## - You should NOT interpret raw p-values when you run dozens/hundreds of tests.
## - BH controls false discovery rate and is common in exploratory corpus research.

all_results <- all_results %>%
  mutate(p_adj_fdr = p.adjust(p.value, method = "BH")) %>%
  arrange(p_adj_fdr)


## =========================================================
## IMPORTANT: how to read the output
## =========================================================
## 1) Intercepts are almost always "significant" and NOT interpreted.
## 2) Focus on non-intercept terms (gender/condition/year/age/age_diff).
## 3) Use p_adj_fdr for inference.
##
## Create two “paper-ready” tables:
## - cleaned table without intercepts
## - significant results after FDR

results_clean <- all_results %>%
  filter(term != "(Intercept)")

results_sig <- results_clean %>%
  filter(p_adj_fdr < 0.05)

results_clean
results_sig


## =========================================================
## Diagnostics (DHARMa)


## If diagnostics show issues (zero inflation, dispersion, etc.),
## consider alternative families / ziformula.
##
## NOTE: do NOT run DHARMa for every single model unless needed
## (it can be slow and mostly redundant).

sim_mhmh <- simulateResiduals(particle_models[["mhmh"]], n = 1000)
plot(sim_mhmh)

