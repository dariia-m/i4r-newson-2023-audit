## Study 2: every number quoted in the text of the report (Introduction and Section 3).
## The regression tables themselves are built by 03_report_tables.R.
## Run as a standalone script: Rscript 02_study2_checks.R

source(here::here("R", "helpers.R"))
options(width = 160)

S2       <- read_study2()
versions <- study2_versions(S2)
fits     <- fit_study2(versions)
S2_mf    <- S2 |> filter(gender %in% c(0, 1))          # men and women, the paper's regression sample (N = 443)

## The Study 2 models on the deposited predictors, used to isolate the effect of one correction at a time
models <- c("SM9 hostility"              = "past.rival.hostility ~ Age.mc + gender + F.mc + ID.mc + SDO.mc",
            "SM10 ingroup altruism"      = "past.own.altruism ~ Age.mc + gender + F.mc + ID.mc + SDO.mc",
            "Barrier-crossing"           = "past.rival.alturism ~ Age.mc + gender + F.mc",
            "Barrier-crossing + SDO, ID" = "past.rival.alturism ~ Age.mc + gender + F.mc + ID.mc + SDO.mc")
isolated_effect <- function(data_corrected, label) {
  iwalk(models, \(f, name) {
    a <- fit_model(f, S2_mf)
    b <- fit_model(f, data_corrected)
    cat(sprintf("\n-- %s | N %d -> %d | R2 %.3f -> %.3f\n", name, a$n, b$n, a$r2, b$r2))
    compare_fits(a, b, c("deposited", label)) |> as.data.frame() |> print(row.names = FALSE)
  })
}


## ---- Sample size (comment "Study 2 sample size") --------------------------------------------------
section("Study 2 sample: paper reports 496 recruited, 2 + 64 excluded, 432 retained")
cat("Rows in the deposited data:", nrow(S2), "\n")
cat("Participant numbers: range", range(S2$Participant), "| duplicated", sum(duplicated(S2$Participant)),
    "| above 496:", sum(S2$Participant > 496), "| up to 496:", sum(S2$Participant <= 496),
    "| numbers absent from 1-496:", length(setdiff(1:496, S2$Participant)), "\n")
cat("Gender (0 = men, 1 = women, 3 = prefer not to say):\n"); print(table(S2$gender))
cat(sprintf("Age on all rows: M = %.2f, SD = %.2f (paper: 38.36, 13.57)\n", mean(S2$age), sd(S2$age)))
cat("Regression sample (men and women):", nrow(S2_mf), "\n")


## ---- Stored totals in the last rows ---------------------------------------------------------------
section("Stored behaviour totals vs the sum of their own five items")
rebuilt     <- s2_rebuild_totals(S2, names(s2_scale_items))
total_wrong <- map(names(s2_scale_items), \(t) S2[[t]] != rebuilt[[t]]) |> reduce(`|`)
cat("Rows with at least one stored total different from the sum of its items:", sum(total_wrong), "\n")
cat("  their rows in the file:", which(total_wrong), "\n")
cat("  their participant numbers:", S2$Participant[total_wrong], "\n")
cat("  participant 497 affected:", total_wrong[S2$Participant == 497], "\n")

late <- which(total_wrong)
col_position <- \(v) match(v, names(S2))
map(names(s2_scale_items), \(t) {
  own   <- col_position(s2_scale_items[[t]])
  shift <- rowSums(S2[late, own - 2], na.rm = TRUE)         # the five columns two to the left
  tibble(total = t,
         rows_wrong = sum(S2[[t]][late] != rebuilt[[t]][late]),
         max_abs_difference = max(abs(S2[[t]][late] - rebuilt[[t]][late])),
         rows_equal_to_shifted_window = sum(S2[[t]][late] == shift),
         shifted_window = paste(names(S2)[own - 2], collapse = ", "))
}) |>
  list_rbind() |> as.data.frame() |> print(row.names = FALSE)
cat("All behaviour items of these rows lie in 0-2:", all(unlist(S2[late, unlist(s2_scale_items)]) %in% c(0, 1, 2, NA)), "\n")
unnamed <- paste0("V", 57:66)
cat("Unnamed columns V57-V66: rows with any value:", which(rowSums(!is.na(S2[unnamed])) > 0), "\n")

cat("\nEffect of recomputing the totals only (deposited predictors, men and women):\n")
isolated_effect(s2_rebuild_totals(S2_mf), "rebuilt")
cat(sprintf("\nExact p of fusion in the barrier-crossing model with SDO and identification, totals rebuilt: %.5f\n",
            fit_model(models[["Barrier-crossing + SDO, ID"]], s2_rebuild_totals(S2_mf))$coefs |> filter(term == "F.mc") |> pull(p.value)))


## ---- Reverse-coded SDO items ------------------------------------------------------------------------
section("SDO1RC and SDO3RC: stored value against the raw response (correct value: 11 - raw)")
cat("SDO1 (rows) -> SDO1RC (columns):\n"); print(table(raw = S2$SDO1, stored = S2$SDO1RC))
cat("SDO3 (rows) -> SDO3RC (columns):\n"); print(table(raw = S2$SDO3, stored = S2$SDO3RC))
off1 <- S2$SDO1RC != 11 - S2$SDO1
off3 <- S2$SDO3RC != 11 - S2$SDO3
cat("Rows with a wrong reverse code: SDO1RC", sum(off1, na.rm = TRUE), "| SDO3RC", sum(off3, na.rm = TRUE),
    "| either", sum(off1 | off3, na.rm = TRUE), "\n")
cat("Rows where the stored SDO mean equals the mean of SDO1RC, SDO2, SDO3RC, SDO4:",
    sum(abs(S2$SDO.mean - rowMeans(S2[c("SDO1RC", "SDO2", "SDO3RC", "SDO4")])) < 1e-9, na.rm = TRUE), "of", nrow(S2), "\n")

sdo_fixed <- S2 |> mutate(SDO.mean_fixed = rowMeans(cbind(11 - SDO1, SDO2, 11 - SDO3, SDO4)))
cat(sprintf("Alpha of the SDO items: stored %.3f (paper: .75), correctly reversed %.3f\n",
            cronbach_alpha(S2[c("SDO1RC", "SDO2", "SDO3RC", "SDO4")]),
            cronbach_alpha(S2 |> transmute(11 - SDO1, SDO2, 11 - SDO3, SDO4))))
cat(sprintf("SDO mean: stored %.2f, corrected %.2f\n", mean(S2$SDO.mean), mean(sdo_fixed$SDO.mean_fixed, na.rm = TRUE)))

cat("\nEffect of the SDO recode only (the respondent with a missing SDO item drops out):\n")
isolated_effect(S2_mf |> mutate(SDO.mean = rowMeans(cbind(11 - SDO1, SDO2, 11 - SDO3, SDO4)),
                                SDO.mc   = SDO.mean - mean(SDO.mean, na.rm = TRUE)), "recoded")


## ---- Two respondents with missing data ----------------------------------------------------------------
section("Participants 496 and 328")
future_items <- unlist(s2_scale_items[4:6], use.names = FALSE)
S2 |>
  filter(Participant %in% c(328, 496)) |>
  transmute(Participant, age, gender, Fusion.mean = round(Fusion.mean, 2), Identification, SDO1, SDO2, SDO3, SDO4, SDO.mean,
            past_items_missing   = rowSums(is.na(pick(all_of(s2_past_items)))),
            future_items_missing = rowSums(is.na(pick(all_of(future_items)))),
            past.own.altruism, past.rival.alturism, past.rival.hostility) |>
  as.data.frame() |> print(row.names = FALSE)
cat("Respondents with all 15 past-behaviour items missing:", S2$Participant[rowSums(is.na(S2[s2_past_items])) == 15], "\n")
cat("Respondents with a missing SDO item:", S2$Participant[rowSums(is.na(S2[c("SDO1", "SDO2", "SDO3", "SDO4")])) > 0], "\n")
cat("Lowest stored SDO mean:", min(S2$SDO.mean), "(scale minimum is 1)\n")

cat("\nEffect of removing participant 496 only:\n")
isolated_effect(S2_mf |> filter(Participant != 496), "without_496")


## ---- Reliability coefficients ---------------------------------------------------------------------------
section("Study 2 alphas (paper: hostility .43, ingroup altruism .77, barrier-crossing altruism .77)")
alpha_row <- function(d, label, f = cronbach_alpha) {
  tibble(sample = label,
         hostility = f(d[s2_scale_items$past.rival.hostility]),
         ingroup_altruism = f(d[s2_scale_items$past.own.altruism]),
         barrier_crossing = f(d[s2_scale_items$past.rival.alturism]),
         complete_rows = nrow(drop_na(d[s2_scale_items$past.rival.hostility])))
}
bind_rows(alpha_row(S2, "All rows"),
          alpha_row(S2_mf, "Men and women"),
          alpha_row(filter(S2, Participant <= 496), "Participant numbers up to 496"),
          alpha_row(S2[1:431, ], "First 431 rows"),
          alpha_row(S2 |> mutate(across(all_of(s2_past_items), \(x) replace_na(x, 0))), "All rows, missing items as 0"),
          alpha_row(S2, "All rows, standardised alpha", cronbach_alpha_std)) |>
  mutate(across(hostility:barrier_crossing, \(x) round(x, 3))) |>
  as.data.frame() |> print(row.names = FALSE)


## ---- Published models on the deposited data -----------------------------------------------------------------
section("Original models: SDO in Table SM10 and the fit line of the barrier-crossing model")
cat("Table SM10 (paper: SDO beta = -.10, p = .014)\n")
show_fit(fits$sm10[["Original"]], "SDO.mc")
cat("\nBarrier-crossing altruism, main effects (paper: R2 = .06, F(3, 447) = 9.52; beta .19, .10, -.09)\n")
show_fit(fits$barrier[["Original"]])
cat("Observations implied by F(3, 447):", 447 + 4, "\n")
cat("\nBarrier-crossing altruism with SDO and identification (paper: R2 = .07, F(5, 437) = 6.51)\n")
show_fit(fits$barrier_adj[["Original"]])


## ---- Corrected versions -----------------------------------------------------------------------------------------
section("Study 2: sample sizes and terms that cross p = .05 in the corrected versions")
cat("Rows per version:\n"); print(map_int(versions, nrow))
cat("Rows with an SDO mean per version:\n"); print(map_int(versions, \(d) sum(!is.na(d$SDO.mc))))
imap(fits, \(fl, id) {
  base <- fl[[1]]$coefs |> select(term, p_original = p.value)
  imap(fl[-1], \(f, v) f$coefs |> left_join(base, by = "term") |>
         filter(term != "(Intercept)", (p.value < .05) != (p_original < .05)) |>
         transmute(table = id, version = v, term, beta = round(beta, 2), p_original = round(p_original, 3), p = round(p.value, 3))) |>
    list_rbind()
}) |>
  list_rbind() |> as.data.frame() |> print(row.names = FALSE)
