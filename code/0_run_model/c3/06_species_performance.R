## visualize species performance in c3
## 23.1.24

# setup -------------------------------------------------------------------
library(tidyverse)
library(cowplot)

folder_out <- "results/c3_species_performance/"


# height growth of regeneration -------------------------------------------
source("code/visualization/vis_H_vs_dH_c3.R")


# max diameter growth -----------------------------------------------------
source("code/visualization/vis_max_dD_adu.R")


# maximum cohort ages -----------------------------------------------------
source("code/visualization/vis_max_cohort_ages_c3.R")


# plot together -----------------------------------------------------------
plot_grid(gg_dH, gg_ddmax, gg_maxage,
          ncol        = 1,
          labels      = c("a", "b", "c"),
          align       = "v",
          axis        = "lr",
          rel_heights = c(1, 1, 2))

ggsave(str_c(folder_out, "species_performance_c3.jpg"),
       width = 16,
       height = 20,
       units = "cm",
       scale = 1.2)

