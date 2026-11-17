## Runs everything behind the report, in order, and keeps a log of the printed results.
## Usage, from the repository folder: Rscript run_all.R
## Output: output/results_log.txt, output/tables/tables.tex, output/figures/fig1_all_panels.png

scripts <- c("01_study1_checks.R",   # Study 1 numbers quoted in the text
             "02_study2_checks.R",   # Study 2 numbers quoted in the text
             "03_report_tables.R",   # Tables 1-17
             "04_figure1.R")         # Figure 1

dir.create(here::here("output"), showWarnings = FALSE)
log_file <- here::here("output", "results_log.txt")
sink(log_file, split = TRUE)

for (s in scripts) {
  cat("\n\n", strrep("#", 100), "\n# ", s, "\n", strrep("#", 100), "\n", sep = "")
  ## each script runs in its own environment, so nothing carries over from one to the next
  source(here::here(s), local = new.env())
}

cat("\n\n"); print(sessionInfo())
sink()
cat("\nLog written to", log_file, "\n")
