## Study 1: every number quoted in the text of the report (Introduction and Section 3).
## The regression tables themselves are built by 03_report_tables.R.
## Run as a standalone script: Rscript 01_study1_checks.R

source(here::here("R", "helpers.R"))
options(width = 160)

S1       <- read_study1()
versions <- study1_versions(S1)
fits     <- fit_study1(versions)
S1_mf    <- S1 |> filter(sex %in% c(1, 2))            # men and women, the paper's regression sample


## ---- Sample size (comment "Study 1 sample size") --------------------------------------------------
section("Study 1 sample: paper reports n = 497 after 3 exclusions from 500")
cat("Rows in the deposited data:", nrow(S1), "\n")
cat("Participant numbers absent from 1-500:", setdiff(1:500, S1$ppt.no), "\n")
cat("Sex (1 = male, 2 = female, 3 = other/prefer not to say):\n"); print(table(S1$sex))
cat("Regression samples: men and women", nrow(S1_mf), "| with a recorded threat condition",
    sum(!is.na(S1_mf$Exp.1hi.0lo)), "\n")


## ---- SDO items and the missing altruism item ------------------------------------------------------
section("SDO items are not deposited; past altruism has 5 of 6 items")
cat("SDO columns in the Study 1 data:", str_subset(names(S1), regex("sdo", ignore_case = TRUE)), "\n")
cat("Past altruism item columns:", str_subset(names(S1), "^p_own_"), "\n")

## The sixth item ("stopping to help") can be recovered as the stored total minus the five
## deposited items; a difference of 0, 1 or 2 in every row is what one omitted 0-2 item gives.
S1 <- S1 |> mutate(sixth_item = p.own.Total - rowSums(pick(all_of(s1_past_altruism_items))))
cat("Stored past altruism total minus the five deposited items:\n"); print(table(S1$sixth_item))
cat(sprintf("Alpha of past altruism with the recovered item: %.3f (paper: .80); without the row with the impossible value: %.3f\n",
            cronbach_alpha(S1[c(s1_past_altruism_items, "sixth_item")]),
            cronbach_alpha(S1 |> filter(p_own_spend <= 2) |> select(all_of(s1_past_altruism_items), sixth_item))))


## ---- Intergroup variable -----------------------------------------------------------------------------
section("Intergroup difference: deposited variable vs the paper's description")
cat("Rows where Intergroup = desired future altruism - past hostility:", sum(S1$Intergroup == S1$f.own.Total - S1$p.rival.Total), "of", nrow(S1), "\n")
cat("Rows where Intergroup = past altruism - past hostility:          ", sum(S1$Intergroup == S1$p.own.Total - S1$p.rival.Total), "of", nrow(S1), "\n")
cat("Desired altruism total is the sum of", length(s1_future_altruism_items), "items in all rows:",
    all(rowSums(S1[s1_future_altruism_items]) == S1$f.own.Total), "| its maximum:", max(S1$f.own.Total),
    "| maximum of past altruism:", max(S1$p.own.Total), "\n")

cat("\nTable SM7, SDO coefficient under each version:\n")
iwalk(fits$sm7, \(f, v) { cat("--", v, "\n"); show_fit(f, "SDO.MC") })

## Past - past with the impossible altruism value left in (only the recoding of Intergroup applied)
pp_value_kept <- versions[["Corrected"]] |> mutate(p.own.Total = S1$p.own.Total, Intergroup = p.own.Total - p.rival.Total)
cat("-- Past - past, impossible value kept\n")
show_fit(fit_model("Intergroup ~ age_m.cen + gender + F_m.cen + ID.MC + SDO.MC", study1_reg_sample(pp_value_kept)), "SDO.MC")


## ---- "Overwhelming majority wanted to decrease hostility" ---------------------------------------------
section("Desired minus past outgroup hostility (paper: t's > 15.74, Cohen's d's > 1.04)")
hostility_change <- function(d, label) {
  diff <- d$f.rival.Total - d$p.rival.Total
  tt   <- t.test(diff)
  tibble(sample = label, n = length(diff),
         decrease = sum(diff < 0), same = sum(diff == 0), increase = sum(diff > 0),
         pct_decrease = round(100 * mean(diff < 0), 1), pct_same = round(100 * mean(diff == 0), 1),
         pct_increase = round(100 * mean(diff > 0), 1),
         t = round(unname(tt$statistic), 3), df = unname(tt$parameter), p = signif(tt$p.value, 2),
         cohens_d = round(mean(diff) / sd(diff), 2))
}
bind_rows(hostility_change(S1, "All respondents"),
          hostility_change(filter(S1, Exp.1hi.0lo == 1), "High threat"),
          hostility_change(filter(S1, Exp.1hi.0lo == 0), "Low threat"),
          hostility_change(filter(S1, p.rival.Total > 0), "Past hostility above 0")) |>
  as.data.frame() |> print(row.names = FALSE)
cat("Share of respondents with no past hostility:", round(100 * mean(S1$p.rival.Total == 0), 1), "%\n")


## ---- Gender coding -------------------------------------------------------------------------------------
section("Gender coding: are the two respondents with sex = 3 in the published analyses?")
cat("Table 1 of the paper, outgroup hostility: B 2.55 / -0.003 / -0.77 / 0.38, F(3, 490) = 33.14\n")
cat("-- men and women only\n");            show_fit(fits$table1_hostility[["Original"]])
cat("-- all rows, sex entered as 1/2/3\n"); show_fit(fit_model("p.rival.Total ~ age_m.cen + sex + F_m.cen", S1))

cat("\nTable SM1, correlations with gender (supplement: -.08, -.08, -.11, -.25, .08, -.24, .03)\n")
sm1_vars <- c(Fusion = "Fusion.M", SDO = "SDO.m", Identification = "Id", `Outgroup hostility` = "p.rival.Total",
              `Ingroup altruism` = "p.own.Total", `Future OG hostility` = "f.rival.Total", Age = "age")
gender_cors <- function(d, g) map_dbl(sm1_vars, \(v) cor(d[[v]], d[[g]], use = "complete.obs")) |> round(2)
cat("-- all 496 rows, code 3 treated as a number:\n"); print(gender_cors(versions[["Original"]], "gender"))
cat("-- code 3 set to missing:\n");                    print(gender_cors(versions[["Corrected"]], "gender"))

cat("\nLower-order fusion coefficient in the models with gender interactions, original vs corrected coding\n")
for (id in c("table2", "sm2", "sm6")) {
  cat("--", id, "\n")
  iwalk(fits[[id]][c("Original", "Corrected")], \(f, v) { cat("   ", v, ": "); show_fit(f, "F_m.cen") })
}
cat("\nFusion in the main-effects model of past hostility (Table 1 of the paper):\n")
show_fit(fits$table1_hostility[["Original"]], "F_m.cen")

cat("\nFusion slopes behind each line of Figure 1 (Table 3 of the report):\n")
figure1_slopes(figure1_setup(S1)) |>
  transmute(panel = str_sub(panel, 1, 1), line, slope = round(slope, 3), se = round(se, 3), p = round(p, 3)) |>
  as.data.frame() |> print(row.names = FALSE)


## ---- "High SDO goes with less ingroup altruism" ---------------------------------------------------------
section("SDO and past ingroup altruism in Study 1 (Table SM5)")
show_fit(fits$sm5[["Original"]], "SDO.MC")


## ---- Table 2, column B -------------------------------------------------------------------------------------
section("Table 2 of the paper: B printed as 0.7 (fusion x threat) and -0.02 (gender x fusion x threat)")
show_fit(fits$table2[["Original"]], c("F_m.cen:threat", "F_m.cen:threat:gender"))


## ---- Impossible value ----------------------------------------------------------------------------------------
section("Out-of-range value in the past-spending item (scored 0-2)")
print(table(S1$p_own_spend))
S1 |> filter(p_own_spend > 2) |> select(ppt.no, sex, p_own_spend, p.own.Total) |> as.data.frame() |> print(row.names = FALSE)
cat("Table 1 of the paper, past ingroup altruism, with and without that respondent:\n")
iwalk(fits$table1_altruism, \(f, v) { cat("--", v, "\n"); show_fit(f) })


## ---- Fusion descriptives ---------------------------------------------------------------------------------------
section("Table SM1: fusion mean and SD (supplement: 4.98, SD 1.16)")
S1 |> summarise(fusion_mean = mean(Fusion.M), fusion_sd = sd(Fusion.M),
                fusion_mean_from_items = mean(rowMeans(pick(fusion_1:fusion_7))),
                identification_mean = mean(Id), identification_sd = sd(Id)) |>
  mutate(across(everything(), \(x) round(x, 2))) |> as.data.frame() |> print(row.names = FALSE)


## ---- Reliability coefficients ----------------------------------------------------------------------------------
section("Study 1 alphas (paper: past hostility .71 and .72, past altruism .80 and .84)")
tibble(scale = c("Past outgroup hostility (6 items)", "Desired outgroup hostility (6 items)",
                 "Past ingroup altruism (5 deposited + recovered item)", "Desired ingroup altruism (7 items)",
                 "Fusion (7 items)"),
       alpha = c(cronbach_alpha(S1[s1_past_hostility_items]), cronbach_alpha(S1[s1_future_hostility_items]),
                 cronbach_alpha(S1[c(s1_past_altruism_items, "sixth_item")]), cronbach_alpha(S1[s1_future_altruism_items]),
                 cronbach_alpha(select(S1, fusion_1:fusion_7))) |> round(3)) |>
  as.data.frame() |> print(row.names = FALSE)


## ---- Fit statistics of Tables SM5 and SM7 -------------------------------------------------------------------------
section("Fit lines (supplement prints R2 = .27, F(5, 488) = 36.40 under both SM5 and SM7)")
for (id in c("sm5", "sm7")) {
  f <- fits[[id]][["Original"]]
  cat(sprintf("%s: R2 = %.3f, F(%d, %d) = %.2f, N = %d\n", toupper(id), f$r2, f$df1, f$df2, f$F, f$n))
}


## ---- Terms whose significance at .05 changes after the corrections --------------------------------------------------
section("Study 1: terms that cross p = .05 relative to the original version")
imap(fits, \(fl, id) {
  base <- fl[[1]]$coefs |> select(term, p_original = p.value)
  imap(fl[-1], \(f, v) f$coefs |> left_join(base, by = "term") |>
         filter(term != "(Intercept)", (p.value < .05) != (p_original < .05)) |>
         transmute(table = id, version = v, term, beta = round(beta, 2), p_original = round(p_original, 3), p = round(p.value, 3))) |>
    list_rbind()
}) |>
  list_rbind() |> as.data.frame() |> print(row.names = FALSE)
