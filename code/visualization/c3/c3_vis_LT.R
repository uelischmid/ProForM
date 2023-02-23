## visualization of LT
## 6.2.23, us

# setup -------------------------------------------------------------------
library(tidyverse)
library(cowplot)

folder <- "results/c3_analysis_LT/"

source("code/visualization/c3/c3_vis_functions.R")


# load and prepare data ---------------------------------------------------
analysis_LT_orig <- read_rds(str_c(folder, "LT.rds"))
LT <- prep_data(analysis_LT_orig)


# plot --------------------------------------------------------------------
plot_combinations <- expand_grid(stratum = c("UM", "HM", "SA"),
                                 init    = "LT",
                                 nat_haz = c("A", "LED"),
                                 resp_var = c("sha_y_MP_met", "sha_i_MP_met",
                                              "sha_y_IP_met", "sha_i_IP_met",
                                              "mean_i_total",
                                              "neg_dist_MP", "neg_dist_IP"))

for (i in 1:nrow(plot_combinations)) {
  cat(i, "/", nrow(plot_combinations), "\n")
  
  # plot with different color scales
  gg_4cs <- plot_mgm_4cs_all(stratum_sel  = plot_combinations[[i, "stratum"]],
                             init_sel     = plot_combinations[[i, "init"]],
                             nathaz_sel   = plot_combinations[[i, "nat_haz"]],
                             resp_var_sel = plot_combinations[[i, "resp_var"]],
                             res          = LT)
  ggsave(filename = str_c(folder, "LT_",
                          plot_combinations[[i, "stratum"]], "_",
                          plot_combinations[[i, "nat_haz"]], "_",
                          plot_combinations[[i, "resp_var"]], "_4cs.jpg"),
         plot = gg_4cs)
  
  
  # plot with one color scale
  gg_1cs <- plot_mgm_1cs_all(stratum_sel  = plot_combinations[[i, "stratum"]],
                             init_sel     = plot_combinations[[i, "init"]],
                             nathaz_sel   = plot_combinations[[i, "nat_haz"]],
                             resp_var_sel = plot_combinations[[i, "resp_var"]],
                             res          = LT)
  
  ggsave(filename = str_c(folder, "LT_",
                          plot_combinations[[i, "stratum"]], "_",
                          plot_combinations[[i, "nat_haz"]], "_",
                          plot_combinations[[i, "resp_var"]], "_1cs.jpg"),
         plot = gg_1cs)
 }

