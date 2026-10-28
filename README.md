# Gender-Related Variation in Spoken Estonian

**Supplementary materials for:** *Gender-Related Variation in Spoken Estonian: A Corpus-Based Study of Pragmatic and Discourse Markers*.

This repository contains corpus-processing notebooks, statistical analysis scripts, derived tables, model diagnostics, and figures accompanying the manuscript. Its purpose is to make the marker extraction rules and analytical decisions transparent and to provide the detailed results underlying the reported findings.

## Study overview

The study examines associations between speakers' and interlocutors' gender and the frequency of 13 categories of pragmatic and discourse-related lexical markers in spontaneous spoken Estonian. The analysed material comes from the **Phonetic Corpus of Estonian Spontaneous Speech** and comprises:

- 90 dyadic conversations recorded in 2006–2022;
- 150 unique speakers (77 women and 73 men);
- 180 speaker–dialogue observations; and
- 478,318 word tokens.

The main analyses use generalized linear mixed-effects models (negative binomial NB2, or Poisson for obscene particles), with speaker and dialogue random intercepts and an offset for the number of words in each observation. The analyses also include exploratory interactions, sensitivity checks excluding dominant expressive lemmas, and a descriptive lemma-level keyness analysis. Statistical significance in the primary analyses is assessed using Benjamini–Hochberg false discovery rate (FDR) correction.

## Repository structure

```text
phd-project-first-paper/
├── README.md
├── intensiivistajad.txt
├── data/
│   ├── eitea.csv
│   ├── intensifiers.csv
│   ├── kp_markers.csv
│   ├── particles.csv
│   ├── particles_sensitivity.csv
│   ├── pronouns_1face.csv
│   ├── pronouns_2face.csv
│   ├── pronouns_all.csv
│   ├── reflexives.csv
│   ├── particle_lemma_composition_final.csv
│   └── all_words_from_corpus.csv *
├── notebooks/
│   ├── corpus_processing.ipynb
│   └── keyness_analysis.ipynb
├── R_scripts/
│   ├── glmm_analysis_full_poisson.R
│   └── particle_sensitivity_models.R
└── results/
    ├── glmm/
    ├── sensitivity_particles/
    └── keyness/
```

`intensiivistajad.txt` is a reference word list of Estonian intensifiers used to document the intensifier category and its lexical inventory. It is kept at the repository root, alongside this README.

`*` **Access note:** `all_words_from_corpus.csv` is a token-level corpus export. Its public redistribution must be consistent with the source corpus's terms of use; if it cannot be shared, it should be omitted from the public repository. The repository can still document the analysis and provide permitted derived outputs.

### `data/`

Speaker–dialogue-level marker counts and related inputs for the statistical analyses. The first- and second-person pronoun counts are provided separately (`pronouns_1face.csv` and `pronouns_2face.csv`); `pronouns_all.csv` is retained as an additional descriptive file, not as a separate primary outcome. `particles_sensitivity.csv` contains the counts used for the dominant-lemma exclusion analyses.

### `notebooks/`

- `corpus_processing.ipynb`: processing the corpus annotations and extracting linguistic markers.
- `keyness_analysis.ipynb`: comparing lemma frequencies across the two gender groups.

Re-running corpus processing requires access to the relevant source corpus materials and metadata. Notebook input paths may need to be configured for the local environment.

### `R_scripts/`

Scripts for fitting the mixed-effects models, evaluating their results, generating figures, and conducting particle sensitivity analyses. The main analysis uses **13 separately modelled marker categories**, including separate first- and second-person pronoun outcomes.

When reproducing the analyses, run scripts from the repository's root directory so that relative references to `data/` and `results/` resolve correctly. Package installation and access to the required input files are prerequisites; scripts may create their own output subdirectories.

### `results/glmm/`

Primary and exploratory model outputs, including:

- `main_gender_effects.csv` — primary speaker-gender effects and FDR-adjusted p-values;
- `main_model_coefficients.csv` and `all_estimable_coefficients.csv` — model coefficients;
- `interaction_effects.csv` — exploratory interaction tests;
- `model_selection_review.csv` and `model_diagnostics.csv` — model selection and numerical diagnostics;
- `collinearity_vif.csv` — collinearity checks;
- `corpus_summary.csv` and `marker_descriptives.csv` — descriptive summaries;
- `forest_speaker_gender.pdf` / `.png` and `forest_interlocutor_gender.pdf` / `.png` — the two forest plots; and
- `results_revised.xlsx` — compiled model results.

The fitted models differ where justified by numerical diagnostics. In particular, obscene particles are analysed using an adjusted Poisson model; reduced NB2 models are used for polite words and certainty particles.

### `results/sensitivity_particles/`

- `sensitivity_descriptives.csv` — token counts and zero-count observations before and after excluding the dominant lemma;
- `sensitivity_model_comparison.csv` — estimates from the refitted sensitivity models; and
- `particle_lemma_composition.csv` — lemma-by-gender counts for the expressive particle categories.

The sensitivity analyses exclude *kurat* from the obscene-particle category and *issand* from the religious-particle category. Their reported p-values are exploratory and are not part of the primary FDR-adjusted testing family.

The same lemma composition is also presented in the more reader-friendly `data/particle_lemma_composition_final.csv`. Both files correspond to **175 obscene-particle tokens** and **334 religious-particle tokens**, distributed by speaker gender. In the underlying coded data, `gender = 0` denotes women and `gender = 1` denotes men.

### `results/keyness/`

Lemma-level keyness outputs, including the full set of eligible lemmas and the top-15 tables for women and men. The keyness analysis uses log-likelihood (G²), base-2 log ratios, and FDR-adjusted p-values; frequencies are normalised per 10,000 word tokens within each gender group.

Unlike the mixed-effects models, keyness compares pooled token frequencies without adjusting for age, recording conditions, interlocutor characteristics, or speaker clustering. These results should therefore be interpreted as **descriptive and exploratory**, not as independent confirmation of the regression findings.

## Software and reproducibility notes

The mixed-effects analyses reported in the manuscript were conducted using **R 4.4.3**, **glmmTMB 1.1.13**, and **DHARMa 0.4.7**. Python notebooks use standard scientific-computing and corpus-processing libraries. Exact notebook dependencies and file paths should be checked against the published versions of the scripts.

To assess reproducibility, begin with the documented marker definitions and the files in `data/`, then consult the model specifications in `R_scripts/` and the associated outputs in `results/`. Some stages require access to the original corpus and cannot be reproduced from summary tables alone.

## Data source and availability

The original recordings, transcriptions, and annotations belong to the **Phonetic Corpus of Estonian Spontaneous Speech** (Lippus et al. 2023). Availability and redistribution of corpus-derived materials are subject to the corpus provider's access conditions. This repository does not grant independent permission to redistribute restricted corpus content.

## Corpus citation

Lippus, Pärtel, Kätlin Aare, Anton Malmi, Tuuli Tuisk & Pire Teras. 2023. *Phonetic Corpus of Estonian Spontaneous Speech v1.3*. Institute of Estonian and General Linguistics, University of Tartu. https://doi.org/10.23673/re-438
