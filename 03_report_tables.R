## Tables 1-17 of the report: every Study 1 and Study 2 model under the original and the corrected
## versions of the data, in the layout of the I4R template (caption, label, booktabs body, \tablenote).
## Output: output/tables/tables.tex, which the report pulls in with \input{tables}.
##
## Rounding follows the template: p-values three decimals, everything else two decimals.
## Run as a standalone script: Rscript 03_report_tables.R

source(here::here("R", "helpers.R"))

b_digits  <- 2    # decimals for B, beta and slopes
r2_digits <- 2    # decimals for R2


## ---- 1. Data and models ----------------------------------------------------------------------------

S1_raw      <- read_study1()
s1_versions <- study1_versions(S1_raw)
s1_fits     <- fit_study1(s1_versions)
s1_labels   <- study1_labels

S2_raw      <- read_study2()
s2_versions <- study2_versions(S2_raw)
s2_fits     <- fit_study2(s2_versions)
s2_labels   <- study2_labels

fig_setup <- figure1_setup(S1_raw)
slopes    <- figure1_slopes(fig_setup) |> mutate(panel = str_sub(panel, 1, 1))


## ---- 2. Formatting ---------------------------------------------------------------------------------

## Row labels in the paper's wording, and a key that puts interaction terms in the paper's order
term_info <- function(term, piece_label) {
  piece_order <- set_names(seq_along(piece_label), names(piece_label))
  if (term == "(Intercept)") return(tibble(term = term, label = "Intercept", key = "0"))
  pieces <- str_split_1(term, ":")
  pieces <- pieces[order(piece_order[pieces])]
  lab    <- piece_label[pieces]
  lab    <- paste(c(lab[1], str_to_lower(lab[-1])), collapse = " x ")
  lab    <- str_replace(lab, "x sdo", "x SDO")
  tibble(term = term, label = if_else(lab == "Threat", "Threat (high vs. low)", lab),
         key = paste0(length(pieces), "-", paste(piece_order[pieces], collapse = "")))
}
labelled <- function(coefs, piece_label) {
  coefs |> left_join(map(coefs$term, \(t) term_info(t, piece_label)) |> list_rbind(), by = "term") |> arrange(key)
}

## A real minus sign; no leading zero for p, R2 and correlations
fmt_p   <- \(p) case_when(is.na(p) ~ "", p < .001 ~ "$<$.001", .default = str_remove(sprintf("%.3f", p), "^0"))
tex_num <- \(x, d) if_else(is.na(x), "", str_replace(sprintf(paste0("%.", d, "f"), x), "^-", "$-$"))
tex_r2  <- \(x) str_remove(sprintf(paste0("%.", r2_digits, "f"), x), "^0")
tex_x   <- \(label) str_replace_all(label, " x ", " $\\\\times$ ")

## One row per term and version, with a flag for p-values that cross .05 relative to the first version
coef_grid <- function(fit_list, piece_label) {
  base_p <- labelled(fit_list[[1]]$coefs, piece_label) |> select(label, p_orig = p.value)
  imap(fit_list, \(f, v) {
    labelled(f$coefs, piece_label) |>
      left_join(base_p, by = "label") |>
      transmute(key, label, version = v, estimate, beta, p.value,
                flip = !is.na(p_orig) & (p.value < .05) != (p_orig < .05))
  }) |>
    list_rbind() |>
    mutate(version = factor(version, levels = names(fit_list)))
}


## ---- 3. LaTeX builders (I4R template layout) --------------------------------------------------------

table_env <- function(caption, label, header_rows, colspec, body, note, size = "\\small", tabcolsep = "3pt") {
  c("\\begin{table}[htbp]",
    paste0("  \\caption{", caption, "}"),
    paste0("  \\label{", label, "}"),
    paste0("  ", size),
    paste0("  \\setlength\\tabcolsep{", tabcolsep, "}"),
    paste0("  \\tagpdfsetup{table/header-rows={", header_rows, "}}"),
    paste0("  \\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}} ", colspec, " @{}}"),
    paste0("    ", body),
    "  \\end{tabular*}",
    paste0("  \\tablenote{", note, "}"),
    "\\end{table}",
    "")
}

version_tex <- \(v) str_replace(v, " - ", " $-$ ")

## Two header rows: version names over B, beta, p
regression_header <- function(versions) {
  k <- length(versions)
  c("\\toprule",
    paste0(" & ", paste(sprintf("\\multicolumn{3}{c}{\\textbf{%s}}", version_tex(versions)), collapse = " & "), " \\\\"),
    paste(sprintf("\\cmidrule(lr){%d-%d}", 2 + 3 * (seq_len(k) - 1), 4 + 3 * (seq_len(k) - 1)), collapse = " "),
    paste0(" & ", paste(rep("\\textbf{\\textit{B}} & $\\beta$ & \\textbf{\\textit{p}}", k), collapse = " & "), " \\\\"),
    "\\midrule")
}

## Coefficient rows and fit rows of one model; `all_versions` pads versions the model was not
## fitted in (Table 1 panels without the past - past version) with empty cells
regression_rows <- function(fit_list, piece_label, all_versions = names(fit_list)) {
  grid <- coef_grid(fit_list, piece_label) |>
    mutate(cells = paste(tex_num(estimate, b_digits), tex_num(beta, b_digits),
                         if_else(flip, paste0("\\textbf{", fmt_p(p.value), "}"), fmt_p(p.value)), sep = " & "))
  pad <- length(all_versions) - length(fit_list)
  coef_rows <- grid |>
    arrange(key, version) |>
    summarise(line = paste0(tex_x(label[1]), " & ", paste(cells, collapse = " & "), strrep(" & & &", pad), " \\\\"),
              .by = c(key, label)) |>
    pull(line)
  fit_row <- function(stat, vals) {
    paste0(stat, " & ", paste(sprintf("\\multicolumn{3}{c}{%s}", vals), collapse = " & "), strrep(" & & &", pad), " \\\\")
  }
  c(coef_rows,
    "\\addlinespace",
    fit_row("$R^2$", map_chr(fit_list, \(f) tex_r2(f$r2))),
    fit_row("$F$ (\\textit{df})", map_chr(fit_list, \(f) sprintf("%.2f (%d, %d)", f$F, f$df1, f$df2))),
    fit_row("$N$", map_chr(fit_list, \(f) as.character(f$n))))
}

regression_table <- function(fit_list, piece_label, caption, label, note) {
  k <- length(fit_list)
  table_env(caption, label, "1,2", paste0("l *{", 3 * k, "}{r}"),
            c(regression_header(names(fit_list)), regression_rows(fit_list, piece_label), "\\bottomrule"), note)
}

## Correlation matrix as in the supplement: diagonal = "M (SD)", lower triangle = r with stars
cor_matrix <- function(d, vars, fix_minus_zero = FALSE) {
  x <- d |> select(all_of(vars))
  k <- length(vars)
  m <- matrix("", k, k - 1, dimnames = list(paste0(seq_len(k), ". ", names(vars)), seq_len(k - 1)))
  for (i in seq_len(k)) for (j in seq_len(min(i, k - 1))) {
    if (i == j) {
      m[i, j] <- sprintf("%.2f (%.2f)", mean(x[[i]], na.rm = TRUE), sd(x[[i]], na.rm = TRUE))
    } else {
      ct    <- cor.test(x[[i]], x[[j]])
      stars <- case_when(ct$p.value < .001 ~ "$^{***}$", ct$p.value < .01 ~ "$^{**}$",
                         ct$p.value < .05 ~ "$^{*}$", .default = "")
      r_txt <- sprintf("%.2f", ct$estimate)
      if (fix_minus_zero) r_txt <- str_replace(r_txt, "^-0\\.00$", "0.00")
      m[i, j] <- paste0(str_replace(r_txt, "^(-?)0", "\\1"), stars)
    }
  }
  m
}

## Report layout of a correlation matrix: correlations in columns 1..k-1, then M and SD
## (taken from the diagonal). The last variable (gender) has no diagonal cell, as in the supplement.
cor_rows <- function(m) {
  k <- nrow(m)
  map_chr(seq_len(k), \(i) {
    r_cells <- map_chr(seq_len(k - 1), \(j) if (j < i) str_replace(m[i, j], "^-", "$-$") else if (j == i) "\\textendash" else "")
    msd <- if (i <= k - 1) str_match(m[i, i], "^([0-9.]+) \\(([0-9.]+)\\)$")[, 2:3] else c("", "")
    paste0(rownames(m)[i], " & ", paste(c(r_cells, msd), collapse = " & "), " \\\\")
  })
}
cor_table <- function(panels, caption, label, note) {
  k <- nrow(panels[[1]])
  header <- c("\\toprule",
              paste0(" & ", paste(sprintf("\\textbf{%d}", seq_len(k - 1)), collapse = " & "),
                     " & \\textbf{\\textit{M}} & \\textbf{\\textit{SD}} \\\\"),
              "\\midrule")
  body <- imap(panels, \(m, nm) c(sprintf("\\multicolumn{%d}{@{}l}{\\textit{%s}} \\\\", k + 2, nm), cor_rows(m), "\\addlinespace")) |>
    unlist(use.names = FALSE)
  table_env(caption, label, "1", paste0("l *{", k + 1, "}{r}"),
            c(header, head(body, -1), "\\bottomrule"), note, size = "\\footnotesize", tabcolsep = "2pt")
}


## ---- 4. Correlation tables and sample flow -----------------------------------------------------------

## Table SM1 uses all 496 rows; the original treats gender code 3 as a number, the corrected as missing
sm1_vars <- c("Fusion" = "Fusion.M", "SDO" = "SDO.m", "Identification" = "Id",
              "Outgroup hostility" = "p.rival.Total", "Ingroup altruism" = "p.own.Total",
              "Future OG hostility" = "f.rival.Total", "Age" = "age", "Gender" = "gender")
sm1 <- list("Panel A. Original"  = cor_matrix(s1_versions[["Original"]], sm1_vars),
            "Panel B. Corrected" = cor_matrix(s1_versions[["Corrected"]], sm1_vars))

sm8_vars <- c("Fusion" = "Fusion.mean", "SDO" = "SDO.mean", "Identification" = "Identification",
              "Outgroup hostility" = "past.rival.hostility", "Ingroup altruism" = "past.own.altruism",
              "Barrier-crossing altruism" = "past.rival.alturism", "Age" = "age", "Gender" = "gender")
sm8 <- map(s2_versions, \(d) cor_matrix(d, sm8_vars, fix_minus_zero = TRUE))
names(sm8) <- paste0("Panel ", c("A", "B", "C"), ". ", names(sm8))

## Rows left after each step, per version
n_gender  <- sum(S2_raw$gender %in% 0:1)
n_orig    <- nrow(s2_versions[["Original"]])
sample_flow <- tribble(
  ~step,                                                   ~a,           ~b,                                                   ~c,
  "Rows in the deposited file",                            nrow(S2_raw), nrow(S2_raw),                                         nrow(S2_raw),
  "Gender not stated (code 3) removed",                    n_orig,       n_gender,                                             n_gender,
  "Participant 496 removed (no behaviour items answered)", n_orig,       nrow(s2_versions[["Last 15 rebuilt"]]),               nrow(s2_versions[["Last 15 rebuilt"]]),
  "Participants 497-511 removed",                          n_orig,       nrow(s2_versions[["Without last 15"]]),               nrow(s2_versions[["Last 15 rebuilt"]]),
  "Models with SDO (SDO index available)",                 sum(!is.na(s2_versions[["Original"]]$SDO.mc)),
                                                           sum(!is.na(s2_versions[["Without last 15"]]$SDO.mc)),
                                                           sum(!is.na(s2_versions[["Last 15 rebuilt"]]$SDO.mc)))


## ---- 5. Notes ------------------------------------------------------------------------------------

n_beta <- paste("\\textit{B} is the unstandardised coefficient and $\\beta$ the standardised coefficient, computed as",
                "$B \\times SD(x)/SD(y)$ with each product term treated as a regressor of its own (the definition",
                "that reproduces the $\\beta$ printed in Tables 1 and 2 of the article).")
n_bold <- "\\textit{p}-values in bold change significance at the .05 level relative to Original."
n_s1   <- paste("OLS regressions on the deposited Study~1 data; the two respondents who did not report their gender",
                "are excluded, as in the article.")
n_s1_v <- paste("Original: gender coded 1~=~men, 2~=~women, deposited totals. Corrected: gender recoded 0~=~men,",
                "1~=~women, and the out-of-range past-spending response (6 on a 0-2 item) set to missing, which",
                "removes one respondent from the models that use past ingroup altruism.")
n_pp   <- paste("Past $-$ past: as Corrected, with the intergroup difference recomputed as past ingroup altruism minus",
                "past outgroup hostility, as described in the article; the deposited variable (used in Original and",
                "Corrected) equals desired future ingroup altruism minus past outgroup hostility.")
n_int  <- paste("Because the model contains interactions with gender, the lower-order terms that interact with gender",
                "refer to gender~=~0: a value no respondent has under the original coding, and men under the corrected coding.")
n_s2   <- paste("OLS regressions on the deposited Study~2 data; the two respondents who did not report their gender",
                "are excluded, as in the article.")
n_s2_v <- paste("Original: deposited totals and SDO index. Both corrected versions recode the two reverse-coded SDO",
                "items as 11 minus the raw response and recompute the SDO index (missing for the respondent who skipped",
                "one SDO item), remove participant 496, who answered none of the behaviour items, and re-centre the",
                "predictors on the analysis sample. Without last 15: participants numbered 497-511 are also removed.",
                "Last 15 rebuilt: these participants are kept and the behaviour totals are recomputed from their own",
                "items, which corrects the stored totals of participants 498-511.")
n_s1_short <- "OLS regressions; the sample, the versions and $\\beta$ are defined in the note to Table~\\ref{tab:s1_table1}."
n_s2_short <- "OLS regressions; the sample, the versions and $\\beta$ are defined in the note to Table~\\ref{tab:s2_sm9}."
n_cor  <- "$^{*}p < .05$, $^{**}p < .01$, $^{***}p < .001$. Correlations are pairwise complete."


## ---- 6. Tables -----------------------------------------------------------------------------------

## Table 1 of the article: three outcomes as panels
table1_versions <- names(s1_versions)
table1_body <- c(
  regression_header(table1_versions),
  sprintf("\\multicolumn{10}{@{}l}{\\textit{%s}} \\\\", "Panel A. Past outgroup hostility"),
  regression_rows(s1_fits$table1_hostility, s1_labels, table1_versions),
  "\\addlinespace",
  sprintf("\\multicolumn{10}{@{}l}{\\textit{%s}} \\\\", "Panel B. Past ingroup altruism"),
  regression_rows(s1_fits$table1_altruism, s1_labels, table1_versions),
  "\\addlinespace",
  sprintf("\\multicolumn{10}{@{}l}{\\textit{%s}} \\\\", "Panel C. Intergroup difference"),
  regression_rows(s1_fits$table1_intergroup, s1_labels, table1_versions),
  "\\bottomrule")

s1_reg <- function(id, caption, note) {
  regression_table(s1_fits[[id]], s1_labels, caption, paste0("tab:s1_", id), note)
}
s2_reg <- function(id, caption, full_note = FALSE) {
  note <- if (full_note) paste(n_s2, n_s2_v, n_beta, n_bold) else paste(n_s2_short, n_bold)
  regression_table(s2_fits[[id]], s2_labels, caption, paste0("tab:s2_", id), note)
}

## Fusion slopes behind Figure 1
slope_body <- c(
  "\\toprule",
  paste0(" & ", paste(sprintf("\\multicolumn{2}{c}{\\textbf{%s}}",
                              c("A. Original", "B. Men", "C. Women", "D. No gender int.")), collapse = " & "), " \\\\"),
  paste(sprintf("\\cmidrule(lr){%d-%d}", c(2, 4, 6, 8), c(3, 5, 7, 9)), collapse = " "),
  paste0(" & ", paste(rep("\\textbf{Slope} & \\textbf{\\textit{p}}", 4), collapse = " & "), " \\\\"),
  "\\midrule",
  slopes |>
    mutate(cell = paste(tex_num(slope, b_digits), fmt_p(p), sep = " & ")) |>
    summarise(l = paste0(line[1], " & ", paste(cell, collapse = " & "), " \\\\"), .by = line) |>
    pull(l),
  "\\bottomrule")

flow_body <- c(
  "\\toprule",
  " & \\textbf{Original} & \\textbf{Without last 15} & \\textbf{Last 15 rebuilt} \\\\",
  "\\midrule",
  sprintf("%s & %d & %d & %d \\\\", sample_flow$step, sample_flow$a, sample_flow$b, sample_flow$c),
  "\\bottomrule")

tables <- c(
  "% Generated by 03_report_tables.R - do not edit by hand; re-run the script instead.",
  "",
  table_env("Study 1: regressions of past behaviour on age, gender and fusion (Table 1 of the article), original and corrected",
            "tab:s1_table1", "1,2", "l *{9}{r}", table1_body,
            paste(n_s1, n_s1_v, n_pp, n_beta, n_bold, "The Original columns reproduce the published Table 1 up to rounding in the last digit.")),
  s1_reg("table2", "Study 1: desired outgroup hostility on age, gender, fusion and threat (Table 2 of the article), original and corrected gender coding",
         paste(n_s1, "The respondent without a recorded threat condition is also excluded.",
               "Original: gender coded 1~=~men, 2~=~women. Corrected: gender recoded 0~=~men, 1~=~women; the model is otherwise identical.",
               n_int, n_beta, n_bold,
               "The Original columns reproduce the published Table 2 up to rounding in the last digit, except for the two \\textit{B} cells discussed in Section~\\ref{sec:commentary}.")),
  table_env("Study 1: slope of desired outgroup hostility in fusion behind each panel of Figure~\\ref{Fig_1}",
            "tab:s1_fig1_slopes", "1,2", "l *{8}{r}", slope_body,
            paste0("Change in predicted desired outgroup hostility per scale point of fusion, with its \\textit{p}-value, at one ",
                   "standard deviation above (older) and below (younger) the mean age in each threat condition ($N$ = ", nobs(fig_setup$models$orig), "). ",
                   "A: the Table 2 model with gender coded 1/2, evaluated at gender~=~0, a value no respondent has. ",
                   "B and C: the same model with gender coded 0~=~men, 1~=~women, evaluated for men and for women. ",
                   "D: a model with the age $\\times$ fusion $\\times$ threat terms and gender as a control only, ",
                   "evaluated at the sample share of women."),
            size = "\\footnotesize"),
  "\\clearpage",
  cor_table(sm1, "Study 1: correlations and descriptive statistics (Table SM1), original and corrected", "tab:s1_sm1",
            paste("All 496 rows of the deposited Study~1 data. Original: gender coded 1~=~men, 2~=~women, 3~=~not stated and",
                  "treated as a number. Corrected: gender coded 0~=~men, 1~=~women (not stated set to missing) and the",
                  "ingroup altruism total of the respondent with the out-of-range past-spending response set to missing. Future OG hostility is desired",
                  "future outgroup hostility. The supplement reports no mean for gender.", n_cor)),
  s1_reg("sm2", "Study 1: past outgroup hostility with interactions (Table SM2), original and corrected", paste(n_s1_short, n_int, n_bold)),
  s1_reg("sm3", "Study 1: past outgroup hostility with identification and SDO (Table SM3), original and corrected", paste(n_s1_short, n_bold)),
  "\\clearpage",
  s1_reg("sm4", "Study 1: past ingroup altruism with interactions (Table SM4), original and corrected", paste(n_s1_short, n_int, n_bold)),
  s1_reg("sm5", "Study 1: past ingroup altruism with identification and SDO (Table SM5), original and corrected", paste(n_s1_short, n_bold)),
  "\\clearpage",
  s1_reg("sm6", "Study 1: intergroup difference with interactions (Table SM6), original and corrected", paste(n_s1_short, n_int, n_bold)),
  s1_reg("sm7", "Study 1: intergroup difference with identification and SDO (Table SM7), original and corrected", paste(n_s1_short, n_bold)),
  "\\clearpage",
  table_env("Study 2: number of observations in the original and the two corrected versions", "tab:s2_flow", "1", "L{0.42\\linewidth} *{3}{r}", flow_body,
            paste("Number of rows left after each step. Original is the sample that reproduces the published Study~2 regressions.",
                  "Participants 497-511 are removed in the version Without last 15 only.",
                  "The last row gives the sample of the models that include SDO.")),
  cor_table(sm8, "Study 2: correlations and descriptive statistics (Table SM8), original and corrected", "tab:s2_sm8",
            paste("Respondents who reported their gender (0~=~men, 1~=~women).", n_s2_v, "The supplement reports no mean for gender.", n_cor)),
  "\\clearpage",
  s2_reg("sm9", "Study 2: past outgroup hostility with SDO and identification (Table SM9), original and corrected", full_note = TRUE),
  s2_reg("sm10", "Study 2: past ingroup altruism with SDO and identification (Table SM10), original and corrected"),
  "\\clearpage",
  s2_reg("barrier_int", "Study 2: barrier-crossing altruism with interactions (reported in the text of the article), original and corrected"),
  s2_reg("barrier", "Study 2: barrier-crossing altruism, main effects (reported in the text of the article), original and corrected"),
  "\\clearpage",
  s2_reg("barrier_adj", "Study 2: barrier-crossing altruism with SDO and identification (reported in the text of the article), original and corrected")
)

out_file <- file.path(out_dir("tables"), "tables.tex")
write_lines(tables, out_file)
cat("Written:", out_file, "-", sum(str_detect(tables, fixed("\\begin{table}"))), "tables\n")
