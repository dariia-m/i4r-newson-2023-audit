# Reproduction code: comment on Newson, White & Whitehouse (2023)

Code behind the Institute for Replication (I4R) comment on

> Newson, M., White, F., & Whitehouse, H. (2023). Does loving a group mean hating its rivals?
> Exploring the relationship between ingroup cohesion and outgroup hostility among soccer fans.
> *International Journal of Sport and Exercise Psychology*, 21(4), 706-724.

The original analyses were run in JASP and no code was deposited, so the analyses are
reconstructed here in R from the description in the paper. The scripts run only what is reported
in the comment: the numbers quoted in its text, its 17 tables and its figure.

The repository contains code only. The authors' data and the report are not included.

## Data

Download the two files of the authors' deposit from <https://osf.io/ndyq7> and put them, with
their original names, into `data/raw/`:

```
data/raw/Study 1 UK fans threat OSF.xls
data/raw/Study 2 re-ananlysis nov 2021 mean centred.csv
```

`data/` is ignored by git.

## How to run

From the repository folder:

```
Rscript run_all.R
```

or run any numbered script on its own, for example `Rscript 01_study1_checks.R`.
The whole run takes well under a minute.

## Scripts

| Script | What it does | Output |
|---|---|---|
| `R/helpers.R` | Data loading, the versions of each dataset (original and corrected), model definitions, standardised coefficients as in JASP, Cronbach's alpha. Sourced by the other scripts. | none |
| `01_study1_checks.R` | Study 1 numbers quoted in the text of the comment | printed |
| `02_study2_checks.R` | Study 2 numbers quoted in the text of the comment | printed |
| `03_report_tables.R` | Tables 1-17 of the comment in LaTeX | `output/tables/tables.tex` |
| `04_figure1.R` | Figure 1 of the comment | `output/figures/fig1_all_panels.png` |
| `run_all.R` | Runs the four scripts in order and logs what they print | `output/results_log.txt` |

`output/` is ignored by git.

## Where each result of the comment comes from

Study 1 (`01_study1_checks.R`):

- sample size: 496 rows against the reported 497
- SDO items not deposited; the sixth past-altruism item recovered from the stored total
- the deposited Intergroup variable against the definition in the paper, and the SDO
  coefficient of Table SM7 under each version
- desired against past outgroup hostility: counts, t-tests and Cohen's d
- gender coding: the published regressions with and without the two respondents coded 3, the
  correlations of Table SM1, the lower-order fusion coefficients, the fusion slopes behind Figure 1
- SDO and ingroup altruism (Table SM5)
- the two B cells of Table 2 of the paper
- the out-of-range value in the past-spending item
- fusion descriptives of Table SM1
- reliability coefficients
- fit statistics of Tables SM5 and SM7

Study 2 (`02_study2_checks.R`):

- sample size: 445 rows, participant numbers up to 511
- stored totals of participants 498-511 against the sum of their own items, and the effect of
  recomputing them
- the reverse-coded SDO items, and the effect of recoding them
- participants 496 and 328, and the effect of removing participant 496
- reliability coefficients under several samples
- fit statistics of the barrier-crossing altruism model
- sample sizes of the corrected versions and the terms that cross p = .05

Tables (`03_report_tables.R`): Tables 1-10 are Study 1 (Tables 1 and 2 of the paper, the fusion
slopes behind Figure 1, Tables SM1-SM7), Tables 11-17 are Study 2 (sample sizes, Tables SM8-SM10
and the three barrier-crossing altruism models reported in the text of the paper).

## Versions of the data

Study 1

- **Original**: as deposited. Gender coded 1 = men, 2 = women; Intergroup as deposited.
- **Corrected**: gender recoded 0 = men, 1 = women; the out-of-range past-spending response set
  to missing.
- **Past - past**: Corrected, with Intergroup recomputed as past ingroup altruism minus past
  outgroup hostility, as the paper describes it.

Study 2

- **Original**: as deposited, respondents who reported their gender.
- **Without last 15** and **Last 15 rebuilt**: both recode the two reverse-coded SDO items as
  11 minus the raw response, set the SDO index of the respondent who skipped an SDO item to
  missing and remove participant 496, who answered no behaviour item. The first also removes
  participants 497-511; the second keeps them and recomputes the behaviour totals from their
  own items.

## Software

R 4.6.1 with tidyverse 2.0.0, readxl 1.5.0, broom 1.0.13 and here 1.0.2.

```r
install.packages(c("tidyverse", "readxl", "broom", "here"))
```
