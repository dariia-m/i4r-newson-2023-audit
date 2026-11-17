## Figure 1 of the report: Figure 1 of the paper redrawn under the original and the corrected
## gender coding. Predicted desired outgroup hostility from the Table 2 model at -1/+1 SD of age
## and fusion in each threat condition; the four panels are defined in R/helpers.R (figure1_setup).
## Output: output/figures/fig1_all_panels.png
## Run as a standalone script: Rscript 04_figure1.R

source(here::here("R", "helpers.R"))

setup <- figure1_setup(read_study1())

## Predicted values for every line of every panel
preds <- setup$panels |>
  mutate(data = map2(model, gender_value, \(m, g) {
    nd <- expand_grid(setup$grid, as_tibble(g))
    nd |> mutate(pred = predict(setup$models[[m]], newdata = nd))
  })) |>
  select(panel, data) |>
  unnest(data) |>
  mutate(line   = str_c(if_else(threat == 1, "High threat", "Low threat"), ", ", age),
         fusion = factor(fusion, levels = c("Low fusion", "High fusion")),
         panel  = fct_inorder(panel))

## Same layout and markers as the paper's figure
fig <- ggplot(preds, aes(fusion, pred, group = line, linetype = line, shape = line)) +
  geom_line() +
  geom_point(size = 2.5) +
  facet_wrap(~ panel, ncol = 2) +
  scale_shape_manual(values = c(5, 18, 0, 15)) +
  scale_linetype_manual(values = c("dotted", "dashed", "longdash", "solid")) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(x = NULL, y = "Desired future outgroup hostility", shape = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

out_file <- file.path(out_dir("figures"), "fig1_all_panels.png")
ggsave(out_file, fig, width = 10, height = 8.5, dpi = 300)
cat("Written:", out_file, "\n")

## Predicted values, low -> high fusion
preds |>
  mutate(panel = str_sub(as.character(panel), 1, 1), pred = round(pred, 2)) |>
  select(panel, line, fusion, pred) |>
  pivot_wider(names_from = fusion, values_from = pred) |>
  as.data.frame() |> print(row.names = FALSE)
