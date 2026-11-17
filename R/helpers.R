## Shared code for the reproduction of Newson, White & Whitehouse (2023):
## data loading, model definitions and small statistical helpers.
## Sourced by the numbered scripts; it does not produce output by itself.

suppressPackageStartupMessages(library(tidyverse))


## ---- Data ----------------------------------------------------------------------------------------

## The two files of the authors' OSF deposit (https://osf.io/ndyq7) go into data/raw/.
## They are not part of this repository.
data_dir <- here::here("data", "raw")
s1_file  <- file.path(data_dir, "Study 1 UK fans threat OSF.xls")
s2_file  <- file.path(data_dir, "Study 2 re-ananlysis nov 2021 mean centred.csv")

missing_files <- c(s1_file, s2_file)[!file.exists(c(s1_file, s2_file))]
if (length(missing_files) > 0) {
  stop("Data file(s) not found:\n  ", paste(missing_files, collapse = "\n  "),
       "\nDownload the deposit from https://osf.io/ndyq7 and put both files into data/raw/.")
}

read_study1 <- function() readxl::read_excel(s1_file)

## Study 2 gender: 0 = men, 1 = women, 3 = prefer not to say
read_study2 <- function() {
  read_csv(s2_file, show_col_types = FALSE) |>
    mutate(gender = gender.male0.female1.3prefernottosay)
}

out_dir <- function(...) {
  d <- here::here("output", ...)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}


## ---- Small helpers -------------------------------------------------------------------------------

section <- function(title) cat("\n\n", strrep("=", 100), "\n", title, "\n", strrep("=", 100), "\n", sep = "")

## OLS with the standardised coefficients JASP reports (the paper used JASP):
## beta = B * sd(x) / sd(y), each product term standardised as a column of its own.
fit_model <- function(formula, data) {
  m <- lm(as.formula(formula), data = data)
  X <- model.matrix(m)
  y <- model.response(model.frame(m))
  s <- summary(m)
  coefs <- broom::tidy(m) |>
    mutate(beta = if_else(term == "(Intercept)", NA_real_, estimate * apply(X, 2, sd)[term] / sd(y)))
  list(model = m, coefs = coefs, r2 = s$r.squared, F = unname(s$fstatistic[1]),
       df1 = unname(s$fstatistic[2]), df2 = unname(s$fstatistic[3]), n = nobs(m))
}

## Coefficient table of a fit, rounded for printing
show_fit <- function(fit, terms = NULL) {
  out <- fit$coefs |>
    transmute(term, B = round(estimate, 3), SE = round(std.error, 3), beta = round(beta, 3),
              t = round(statistic, 2), p = round(p.value, 4))
  if (!is.null(terms)) out <- out |> filter(term %in% terms)
  print(as.data.frame(out), row.names = FALSE)
  cat(sprintf("R2 = %.3f, F(%d, %d) = %.2f, N = %d\n", fit$r2, fit$df1, fit$df2, fit$F, fit$n))
  invisible(fit)
}

## Raw Cronbach's alpha on complete rows: k/(k-1) * (1 - sum of item variances / variance of the total)
cronbach_alpha <- function(items) {
  items <- drop_na(items)
  k <- ncol(items)
  (k / (k - 1)) * (1 - sum(map_dbl(items, var)) / var(rowSums(items)))
}

## Standardised alpha, from the mean inter-item correlation
cronbach_alpha_std <- function(items) {
  items <- drop_na(items)
  k <- ncol(items)
  r <- cor(items)
  r_bar <- mean(r[lower.tri(r)])
  k * r_bar / (1 + (k - 1) * r_bar)
}


## ---- Study 1: versions of the data and models ---------------------------------------------------

## Original    : as deposited. Gender coded 1 = men, 2 = women (3 = not stated), not centred;
##               Intergroup as deposited (= desired future ingroup altruism - past outgroup hostility).
## Corrected   : gender recoded 0 = men, 1 = women (not stated = NA); the out-of-range past-spending
##               response (6 on a 0-2 item) set to missing, so that respondent's past altruism total is NA.
## Past - past : Corrected, with Intergroup rebuilt as past ingroup altruism - past outgroup hostility,
##               which is how the paper describes it.
study1_versions <- function(S1_raw) {
  orig <- S1_raw |> mutate(gender = sex, threat = Exp.1hi.0lo)
  corr <- S1_raw |>
    mutate(gender         = case_when(sex == 1 ~ 0, sex == 2 ~ 1, .default = NA),
           threat         = Exp.1hi.0lo,
           p_own_spend_ok = if_else(p_own_spend > 2, NA, p_own_spend),
           ## only five of the six past-altruism items are deposited, so the total cannot be rebuilt
           ## from items; subtracting the bad value and adding NA sets that respondent's total to missing
           p.own.Total    = p.own.Total - p_own_spend + p_own_spend_ok)
  pp <- corr |> mutate(Intergroup = p.own.Total - p.rival.Total)
  list("Original" = orig, "Corrected" = corr, "Past - past" = pp)
}

## Regression sample of the paper: men and women only (N = 494); Table 2 also needs the threat condition (N = 493)
study1_reg_sample <- function(d, needs_threat = FALSE) {
  d <- d |> filter(sex %in% c(1, 2))
  if (needs_threat) d <- d |> filter(!is.na(threat))
  d
}

## One row per published table. intergroup = TRUE models are also fitted in the past - past version.
study1_specs <- tribble(
  ~id,                 ~formula,                                                                   ~threat, ~intergroup,
  "table1_hostility",  "p.rival.Total ~ age_m.cen + gender + F_m.cen",                             FALSE,   FALSE,
  "table1_altruism",   "p.own.Total ~ age_m.cen + gender + F_m.cen",                               FALSE,   FALSE,
  "table1_intergroup", "Intergroup ~ age_m.cen + gender + F_m.cen",                                FALSE,   TRUE,
  "table2",            "f.rival.Total ~ age_m.cen * F_m.cen * threat + gender * F_m.cen * threat", TRUE,    FALSE,
  "sm2",               "p.rival.Total ~ age_m.cen * F_m.cen + gender * F_m.cen",                   FALSE,   FALSE,
  "sm3",               "p.rival.Total ~ age_m.cen + gender + F_m.cen + ID.MC + SDO.MC",            FALSE,   FALSE,
  "sm4",               "p.own.Total ~ age_m.cen * F_m.cen + gender * F_m.cen",                     FALSE,   FALSE,
  "sm5",               "p.own.Total ~ age_m.cen + gender + F_m.cen + ID.MC + SDO.MC",              FALSE,   FALSE,
  "sm6",               "Intergroup ~ age_m.cen * F_m.cen + gender * F_m.cen",                      FALSE,   TRUE,
  "sm7",               "Intergroup ~ age_m.cen + gender + F_m.cen + ID.MC + SDO.MC",               FALSE,   TRUE
)
study1_labels <- c(age_m.cen = "Age", gender = "Gender", F_m.cen = "Fusion", threat = "Threat",
                   ID.MC = "Identification", SDO.MC = "SDO")

## Every Study 1 model in every version it applies to: list (by table id) of lists (by version)
fit_study1 <- function(versions) {
  pmap(study1_specs, \(id, formula, threat, intergroup) {
    vs <- if (intergroup) names(versions) else names(versions)[1:2]
    map(set_names(vs), \(v) fit_model(formula, study1_reg_sample(versions[[v]], needs_threat = threat)))
  }) |>
    set_names(study1_specs$id)
}

## Items of the Study 1 scales that are deposited
s1_past_altruism_items    <- c("p_own_hug", "p_own_support", "p_own_charity", "p_own_stand", "p_own_spend")   # sixth item not deposited
s1_past_hostility_items   <- c("p_rival_swore", "p_rival_insult", "p_rival_shout", "p_rival_spit", "p_rival_throw", "p_rival_physical")
s1_future_altruism_items  <- c("f_own_smile", "f_own_stop", "f.hug", "f.support", "f_own_charity", "f_own_stand", "f_own_spend")
s1_future_hostility_items <- c("f_swear", "f_insult", "f_shout", "f_rival_throw", "f_rival_damage", "f_rival_physical")


## ---- Study 1: Figure 1 ----------------------------------------------------------------------------

## Figure 1 of the paper plots predicted desired outgroup hostility from the Table 2 model at
## -1/+1 SD of age (younger/older) and fusion (low/high) in each threat condition.
##   A. Original : gender coded 1/2, evaluated at gender = 0 (a value no respondent has)
##   B. Corrected: gender dummy, evaluated for men
##   C. Corrected: gender dummy, evaluated for women
##   D. No gender interactions: gender as a control only, evaluated at the sample share of women
figure1_setup <- function(S1_raw) {
  d <- S1_raw |>
    filter(sex %in% c(1, 2), !is.na(Exp.1hi.0lo)) |>
    mutate(threat = Exp.1hi.0lo, female = if_else(sex == 2, 1, 0))
  models <- list(
    orig = lm(f.rival.Total ~ age_m.cen * F_m.cen * threat + sex * F_m.cen * threat, data = d),
    corr = lm(f.rival.Total ~ age_m.cen * F_m.cen * threat + female * F_m.cen * threat, data = d),
    nog  = lm(f.rival.Total ~ age_m.cen * F_m.cen * threat + female, data = d))
  panels <- tribble(
    ~panel,                                          ~model, ~gender_value,
    "A. Original: 1/2 coding, gender = 0 (paper)",   "orig", list(sex = 0),
    "B. Corrected: men",                             "corr", list(female = 0),
    "C. Corrected: women",                           "corr", list(female = 1),
    "D. No gender interactions: average respondent", "nog",  list(female = mean(d$female)))
  sd_age    <- sd(d$age_m.cen)
  sd_fusion <- sd(d$F_m.cen)
  grid <- expand_grid(threat = c(1, 0), age = c("Older", "Younger"), fusion = c("Low fusion", "High fusion")) |>
    mutate(age_m.cen = if_else(age == "Older", sd_age, -sd_age),
           F_m.cen   = if_else(fusion == "High fusion", sd_fusion, -sd_fusion))
  list(data = d, models = models, panels = panels, grid = grid)
}

## Slope of the prediction in fusion for each line of each panel. The model is linear in fusion,
## so the slope is the contrast L = X(fusion = 1) - X(fusion = 0), with SE = sqrt(L' V L).
figure1_slopes <- function(setup) {
  slope_of <- function(m, nd) {
    tt  <- delete.response(terms(m))
    L   <- model.matrix(tt, mutate(nd, F_m.cen = 1)) - model.matrix(tt, mutate(nd, F_m.cen = 0))
    est <- drop(L %*% coef(m))
    se  <- sqrt(diag(L %*% vcov(m) %*% t(L)))
    tibble(slope = est, se = se, p = 2 * pt(-abs(est / se), df.residual(m)))
  }
  setup$panels |>
    mutate(res = map2(model, gender_value, \(m, g) {
      nd <- expand_grid(filter(setup$grid, fusion == "Low fusion"), as_tibble(g))
      bind_cols(nd |> select(threat, age), slope_of(setup$models[[m]], nd))
    })) |>
    select(panel, res) |>
    unnest(res) |>
    mutate(line = paste0(if_else(threat == 1, "High threat", "Low threat"), ", ", age))
}


## ---- Study 2: versions of the data and models ---------------------------------------------------

## Each stored total and the five items (scored 0-2) it should be the sum of
s2_scale_items <- list(
  past.own.altruism      = c("p_own_smile", "p_own_help", "p_own_charity", "p_own_stand", "p_own_spend"),
  past.rival.alturism    = c("p_rival_smile", "p_rival_stop", "p_rival_charity", "p_rival_stand", "p_rival_spend"),
  past.rival.hostility   = c("p_rival_gesture", "p_rival_spit", "p_rival_throw", "p_rival_damage", "p_rival_physical"),
  future.own.altruism    = c("f_own_smile", "f_own_stop", "f_own_charity", "f_own_stand", "f_own_spend"),
  future.rival.altruism  = c("f_rival_smile", "f_rival_stop", "f_rival_charity", "f_rival_stand", "f_rival_spend"),
  future.rival.hostility = c("f_rival_gesture", "f_rival_spit", "f_rival_throw", "f_rival_damage", "f_rival_physical")
)
s2_past_totals <- names(s2_scale_items)[1:3]     # the outcomes of the Study 2 regressions
s2_past_items  <- unlist(s2_scale_items[s2_past_totals], use.names = FALSE)

## Totals recomputed from their own items. A missing item counts as zero, which is how the stored
## totals of the first 431 rows were built (they all equal the sum of their items).
s2_rebuild_totals <- function(d, totals = s2_past_totals) {
  for (t in totals) d[[t]] <- rowSums(select(d, all_of(s2_scale_items[[t]])), na.rm = TRUE)
  d
}

## Original        : as deposited, men and women only (the paper's regression sample, N = 443).
## Both corrected versions: SDO1RC and SDO3RC recoded as 11 - raw and the SDO mean recomputed (missing
##                   for participant 328, who skipped one SDO item); participant 496, who answered no
##                   behaviour item but has stored totals of 0, removed; predictors re-centred on the
##                   analysis sample. The deposited centred columns are not centred exactly on the
##                   sample means, so this moves the intercept and, very slightly, the lower-order
##                   terms of the model with interactions.
## Without last 15 : participants 497-511, numbered above the 496 recruited, also removed.
## Last 15 rebuilt : all remaining rows kept, totals recomputed from their own items (this corrects
##                   the stored totals of participants 498-511).
study2_versions <- function(S2_raw) {
  recentre <- function(d) {
    d |> mutate(Age.mc = age - mean(age),
                F.mc   = Fusion.mean - mean(Fusion.mean),
                ID.mc  = Identification - mean(Identification),
                SDO.mc = SDO.mean - mean(SDO.mean, na.rm = TRUE))
  }
  orig  <- S2_raw |> filter(gender %in% c(0, 1))
  fixed <- S2_raw |>
    mutate(no_outcome_data = rowSums(!is.na(pick(all_of(s2_past_items)))) == 0,
           SDO1RC   = 11 - SDO1,
           SDO3RC   = 11 - SDO3,
           SDO.mean = rowMeans(pick(SDO1RC, SDO2, SDO3RC, SDO4))) |>
    filter(gender %in% c(0, 1), !no_outcome_data)
  list("Original"        = orig,
       "Without last 15" = fixed |> filter(Participant <= 496) |> s2_rebuild_totals() |> recentre(),
       "Last 15 rebuilt" = fixed |> s2_rebuild_totals() |> recentre())
}

study2_specs <- tribble(
  ~id,           ~formula,
  "sm9",         "past.rival.hostility ~ Age.mc + gender + F.mc + ID.mc + SDO.mc",
  "sm10",        "past.own.altruism ~ Age.mc + gender + F.mc + ID.mc + SDO.mc",
  "barrier_int", "past.rival.alturism ~ Age.mc * F.mc + gender * F.mc",
  "barrier",     "past.rival.alturism ~ Age.mc + gender + F.mc",
  "barrier_adj", "past.rival.alturism ~ Age.mc + gender + F.mc + ID.mc + SDO.mc"
)
study2_labels <- c(Age.mc = "Age", gender = "Gender", F.mc = "Fusion", ID.mc = "Identification", SDO.mc = "SDO")

fit_study2 <- function(versions) {
  map(set_names(study2_specs$formula, study2_specs$id), \(f) map(versions, \(d) fit_model(f, d)))
}

## Betas and p-values of one model under two versions of the data, side by side
compare_fits <- function(a, b, names = c("a", "b")) {
  tibble(term = a$coefs$term,
         beta_a = round(a$coefs$beta, 3), p_a = round(a$coefs$p.value, 4),
         beta_b = round(b$coefs$beta, 3), p_b = round(b$coefs$p.value, 4)) |>
    filter(term != "(Intercept)") |>
    mutate(crosses_05 = if_else((p_a < .05) != (p_b < .05), "*", "")) |>
    rename_with(\(x) str_replace(x, "_a$", paste0("_", names[1])) |> str_replace("_b$", paste0("_", names[2])))
}
