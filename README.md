# Gender Variation in Spoken Estonian
# Gender Variation in Spoken Estonian: A Corpus-Based Study of Pragmatic and Discourse Markers

This repository contains the data, scripts, and analysis results for the paper:

> **Gender-Related Variation in Spoken Estonian: A Corpus-Based Study of Pragmatic and Discourse Markers**

---

## Abstract

The absence of grammatical gender in Estonian has often led to the assumption that gender plays a limited role in language use. This study examines potentially gender-related lexical variation in spoken Estonian and whether patterns reported for other languages also appear there.

The analysis is based on 90 dialogues from the *Phonetic Corpus of Estonian Spontaneous Speech*, involving 150 speakers, and examines pragmatic and discourse markers using mixed-effects regression models and a keyness analysis of individual lemmas.

Only a few gender-related differences between men and women were found. After correction for multiple comparisons, significant differences emerged only for obscene and religious particles and for first- and second-person pronouns: men used obscene particles more, while women preferred religious particles and personal pronouns. The keyness analysis further revealed gender-related preferences at the level of individual lemmas, including response tokens and expressive items, not detectable through aggregated categories. Gender-related variation thus persists despite the absence of grammatical gender.

---

# Repository structure

```
.
├── data/
│   ├── eitea.csv
│   ├── intensifiers.csv
│   ├── kp_markers.csv
│   ├── particles.csv
│   ├── pronouns.csv
│   └── reflexives.csv
│
├── scripts/
│   ├── corpus_processing.ipynb
│   ├── glmm_analysis.R
│   └── keyness_analysis.ipynb
│
├── results_glmm/
│   ├── model_results_all.csv
│   ├── model_results_converged.csv
│   ├── model_results_interactions.csv
│   ├── results_table.xlsx
│   ├── table_full.csv
│   ├── table_gender.csv
│   ├── table_interactions.csv
│   └── table_summary.csv
│
├── results_keyness/
│   ├── keyness_significant_men.csv
│   ├── keyness_significant_women.csv
│   └── keyness_style_lemmas.csv
│
├── intensiivistajad.txt
└── README.md
```

---

# Repository contents

## data/

This directory contains the datasets used for statistical analyses. Each file corresponds to one category of pragmatic or discourse markers extracted from the corpus.

| File | Description |
|------|-------------|
| `eitea.csv` | Occurrences of *ei tea* ("I don't know") |
| `intensifiers.csv` | Intensifiers |
| `kp_markers.csv` | Pragmatic and discourse markers |
| `particles.csv` | Particles |
| `pronouns.csv` | Personal pronouns |
| `reflexives.csv` | Reflexive pronouns |

---

## scripts/

Scripts used for data processing and statistical analyses.

| File | Description |
|------|-------------|
| `corpus_processing.ipynb` | Corpus preprocessing and extraction of variables |
| `glmm_analysis.R` | Generalized linear mixed-effects models (GLMMs) |
| `keyness_analysis.ipynb` | Keyness analysis of individual lemmas |

---

## results_glmm/

Output of the generalized linear mixed-effects models.

| File | Description |
|------|-------------|
| `model_results_all.csv` | Results of all fitted models |
| `model_results_converged.csv` | Successfully converged models |
| `model_results_interactions.csv` | Models including interaction effects |
| `table_full.csv` | Complete statistical results |
| `table_gender.csv` | Main effects of gender |
| `table_interactions.csv` | Interaction effects |
| `table_summary.csv` | Summary table reported in the paper |
| `results_table.xlsx` | Excel version of the results tables |

---

## results_keyness/

Output of the keyness analysis.

| File | Description |
|------|-------------|
| `keyness_significant_men.csv` | Lemmas significantly associated with male speakers |
| `keyness_significant_women.csv` | Lemmas significantly associated with female speakers |
| `keyness_style_lemmas.csv` | Stylistically marked lemmas identified in the analysis |

---

## intensiivistajad.txt

Reference list of Estonian intensifiers used during corpus annotation and preprocessing.

---

# Reproducing the analyses

The analyses can be reproduced in the following order:

1. Run `scripts/corpus_processing.ipynb` to prepare the datasets.
2. Run `scripts/glmm_analysis.R` to fit the generalized linear mixed-effects models.
3. Run `scripts/keyness_analysis.ipynb` to perform the keyness analysis.
4. The resulting output files will be written to `results_glmm/` and `results_keyness/`.

---

# Software

The analyses were conducted using:

- R (version 4.x)
- Python (version 3.x)
- Jupyter Notebook

Main R packages:

- lme4
- lmerTest
- emmeans
- performance
- broom.mixed
- tidyverse

Main Python libraries:

- pandas
- numpy
- scipy
- nltk
- matplotlib

---

# Data

The analyses are based on the **Phonetic Corpus of Estonian Spontaneous Speech v1.3** (Lippus et al., 2023), developed at the University of Tartu.

Dataset DOI: **10.23673/RE-438**

---

# Citation

If you use this repository, please cite:

> Kriuchkova, S. *Gender-Related Variation in Spoken Estonian: A Corpus-Based Study of Pragmatic and Discourse Markers.*

Please also cite the corpus:

> Lippus, P., Aare, K., Malmi, A., Tuisk, T., & Teras, P. (2023). *Phonetic Corpus of Estonian Spontaneous Speech v1.3*. University of Tartu. https://doi.org/10.23673/RE-438
