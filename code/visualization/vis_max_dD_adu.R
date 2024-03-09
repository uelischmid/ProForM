## visualize maximal diameter growth rates of adult trees
## strata of c3
## 23.1.24


# setup -------------------------------------------------------------------
library(tidyverse)

# folder_out <- "results/ddmax_adu/"

param_fordyn <- read_delim("data/processed/paramfordyn/paramfordyn_cal.csv", ";",
                           col_types = cols(
                             `function` = col_character(),
                             f.expl     = col_character(),
                             stratum    = col_character(),
                             species    = col_character(),
                             p.name     = col_character(),
                             p.value    = col_double(),
                             p.expl     = col_character()
                           )) %>% 
  select(stratum, species, p.name, p.value)

param_ddmax <- param_fordyn %>% 
  filter(p.name == "dgadu_ddmax") %>% 
  filter(stratum != "SM") %>% 
  mutate(stratum = factor(stratum, levels = c("UM", "HM", "SA")),
         species = factor(species, levels = c("pabi", "aalb", "fsyl", "apse")))

gg_ddmax <- ggplot(param_ddmax, aes(species, p.value, fill = species)) +
  geom_col(position = "dodge") +
  facet_wrap(~stratum) +
  labs(x = "Species",
       y = expression(dD[max]*" (cm/y)")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "none")

ggsave(str_c(folder_out, "ddmax_adu_c3.jpg"),
       width = 10,
       height = 4,
       units = "cm",
       scale = 1.5)

rm(list = setdiff(ls(), c("folder_out", "gg_dH", "gg_ddmax")))
