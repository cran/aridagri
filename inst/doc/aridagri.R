## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6.5,
                      fig.height = 4, dpi = 96)
has_gg <- requireNamespace("ggplot2", quietly = TRUE)
library(aridagri)

## ----rbd----------------------------------------------------------------------
res <- anova_rbd(mothbean_rbd, response = "seed_yield",
                 treatment = "treatment", block = "block",
                 posthoc = "duncan")

## ----rbd-plot, eval = has_gg--------------------------------------------------
plot(res, ylab = "Seed yield (kg/ha)")

## ----traits-------------------------------------------------------------------
tab <- anova_traits(mothbean_rbd,
                    traits = c("plant_height", "pods_per_plant",
                               "test_weight", "seed_yield", "harvest_index"),
                    design = "rbd", treatment = "treatment", block = "block",
                    posthoc = "duncan")

## ----spd----------------------------------------------------------------------
sp <- anova_spd(clusterbean_spd, response = "seed_yield",
                main_plot = "irrigation", sub_plot = "nitrogen",
                replication = "rep")

## ----spd-plot, eval = has_gg--------------------------------------------------
plot_interaction(sp, ylab = "Seed yield (kg/ha)")

## ----pooled-------------------------------------------------------------------
pool <- anova_rbd_pooled(pearlmillet_met, response = "grain_yield",
                         treatment = "genotype", environment = "environment",
                         block = "rep", verbose = FALSE)
pool$bartlett_test[c("chi_sq", "p_value")]
pool$pooled_anova

## ----stability----------------------------------------------------------------
st <- stability_analysis(pearlmillet_met, genotype = "genotype",
                         environment = "environment", replication = "rep",
                         trait = "grain_yield", verbose = FALSE)
st$eberhart[, c("Genotype", "Mean", "bi", "p_bi", "S2di", "p_S2di")]
st$ammi$anova

## ----stability-plot, eval = has_gg--------------------------------------------
plot_stability(st, "eberhart")
plot_stability(st, "ammi2")

## ----gge, eval = has_gg-------------------------------------------------------
gg <- gge_biplot(pearlmillet_met, "genotype", "environment", "grain_yield",
                 verbose = FALSE)
gg$which_won_where
plot(gg, type = "which_won_where")

## ----augmented----------------------------------------------------------------
au <- anova_augmented(chickpea_augmented, response = "seed_yield",
                      genotype = "entry", block = "block",
                      check_names = c("C1", "C2", "C3", "C4"),
                      verbose = FALSE)
head(au$test_vs_checks[, c("Entry", "Adjusted_mean", "Pct_over_best_check",
                           "Sig_vs_best")])
au$genetic_parameters

## ----alpha--------------------------------------------------------------------
al <- anova_alpha_lattice(cowpea_alpha, response = "grain_yield",
                          genotype = "genotype", replication = "rep",
                          block = "block", verbose = FALSE)
al$efficiency
head(al$genotype_means)

## ----repeated-----------------------------------------------------------------
rm <- anova_repeated(mungbean_repeated, response = "spad",
                     treatment = "treatment", time = "das", block = "block",
                     verbose = FALSE)
rm$sphericity
rm$corrected
rm$covariance_models

## ----repeated-plot, eval = has_gg---------------------------------------------
plot(rm, ylab = "SPAD reading")

## ----indices------------------------------------------------------------------
thermal_indices(tmax = c(31, 33, 35, 36, 38), tmin = c(16, 17, 19, 20, 22),
                base_temp = 10, sunshine_hours = rep(9.5, 5),
                day_length = day_length(28, 70:74), crop_yield = 1250)

nue_calculate(yield_fertilized = c(1450, 1720, 1850), yield_control = 1200,
              nutrient_applied = c(20, 40, 60),
              nutrient_uptake_fert = c(48, 61, 68),
              nutrient_uptake_ctrl = 35)

nr <- nutrient_response(dose = c(0, 20, 40, 60, 80, 100),
                        yield = c(1100, 1350, 1520, 1610, 1650, 1660),
                        model = "all", price_output = 60,
                        price_nutrient = 15)

## ----response-plot, eval = has_gg---------------------------------------------
plot_response(nr, c(0, 20, 40, 60, 80, 100),
              c(1100, 1350, 1520, 1610, 1650, 1660))

## ----letters------------------------------------------------------------------
compare_means(c(T1 = 1180, T2 = 1265, T3 = 1342, T4 = 1410, T5 = 1238),
              mse = 3600, df_error = 12, reps = 4, method = "duncan")

