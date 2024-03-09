## retrieve max cohort ages from simulation
## c3 LT NOM simulations
## 12.1.24


# setup -------------------------------------------------------------------
library(tidyverse)

# folder_out <- "results/max_ages_c3/"

simulations <- list.dirs("data/processed/simoutput/",
                         full.names = FALSE,
                         recursive  = FALSE)
simulations <- simulations[str_detect(simulations, "NOM")]
simulations <- simulations[str_detect(simulations, "LT")]


# get max ages ------------------------------------------------------------
max_ages_all <- vector(mode = "list", length = length(simulations))

for (a in seq_along(simulations)) {
  simulation <- simulations[a]
  
  res_standdev <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/res_standdev.rds")) %>% 
    filter(State_nr == 1) %>% 
    select(co_ID, time_Step, Species, H)
  
  sim_info <- str_split(simulation, pattern = "_") %>% 
    magrittr::extract2(1)
  
  cohorts <- sort(unique(res_standdev$co_ID))
  
  max_ages_sim <- vector(mode = "list", length = length(cohorts))
  
  for (i in seq_along(cohorts)) {
    res_coh <- res_standdev %>%
      filter(co_ID == cohorts[i]) %>% 
      filter(Species != "none") %>% 
      mutate(dH         = H - lag(H),
             dH         = replace_na(dH, replace = -10),
             first_y    = case_when(dH < 0 ~ TRUE,
                                    TRUE   ~ FALSE),
             last_y     = lead(first_y),
             first_last = first_y + last_y) %>% 
    filter(first_last == 1)
    
    if (head(res_coh, 1)$first_y == FALSE) {
      res_coh <- res_coh[-1, ]
    }
    
    if (tail(res_coh, 1)$last_y == FALSE && nrow(res_coh) > 1) {
      res_coh <- res_coh[-(nrow(res_coh)),]
    }
    
    if (nrow(res_coh) == 1) {
      max_age <- max(res_standdev$time_Step) - pull(res_coh, time_Step)
      
      max_ages_sim[[i]] <- tibble(Species = pull(res_coh, Species),
                                  max_age = max_age,
                                  co_ID   = cohorts[i])
    } else {
      max_ages_sim[[i]] <- res_coh %>% 
        mutate(group_nr = rep(1:(nrow(res_coh) / 2), each = 2)) %>% 
        group_by(group_nr) %>% 
        mutate(max_age = time_Step - lag(time_Step)) %>% 
        ungroup() %>% 
        filter(!is.na(max_age)) %>% 
        select(Species, max_age) %>% 
        mutate(co_ID = cohorts[i])
    }
  }
  
  max_ages_all[[a]] <- bind_rows(max_ages_sim) %>% 
    mutate(sim     = simulation,
           stratum = sim_info[3],
           q_site  = as.numeric(str_sub(sim_info[4], start = 1, end = 1)),
           q_reg   = as.numeric(str_sub(sim_info[4], start = 2, end = 2))) %>% 
    select(sim, stratum, q_site, q_reg, co_ID, spc = Species, max_age)
  
}

max_ages_all <- bind_rows(max_ages_all)
write_rds(max_ages_all,
          str_c(folder_out, "max_ages_c3_NOM.rds"))

# rm(list = setdiff(ls(), c("max_ages_all", "simulations", "folder_out")))

# maximum values ----------------------------------------------------------
# max_ages_all <- read_rds(str_c(folder_out, "max_ages_c3_NOM.rds"))
max_ages_all <- max_ages_all %>% 
  mutate(stratum = factor(stratum, levels = c("UM", "HM", "SA")),
         spc     = factor(spc, levels = c("pabi", "aalb", "fsyl", "apse")),
         spc     = fct_rev(spc))

# quantiles
age_quant_spc <- max_ages_all %>% 
  group_by(spc) %>% 
  summarise(age_q0950 = quantile(max_age, probs = 0.95),
            age_q0975 = quantile(max_age, probs = 0.975),
            age_q1000  = max(max_age)) %>% 
  pivot_longer(cols      = age_q0950:age_q1000,
               names_to  = "quantile",
               values_to = "value")

age_quant_str_spc <- max_ages_all %>% 
  group_by(stratum, spc) %>% 
  summarise(age_q0950 = quantile(max_age, probs = 0.95),
            age_q0975 = quantile(max_age, probs = 0.975),
            age_q1000  = max(max_age),
            .groups = "drop") %>% 
  pivot_longer(cols      = age_q0950:age_q1000,
               names_to  = "quantile",
               values_to = "value")

# visualization
gg_maxage <- ggplot(max_ages_all, aes(max_age)) +
  geom_density() +
  geom_vline(aes(xintercept = value, color = quantile),
             data = age_quant_str_spc) +
  facet_grid(vars(spc), vars(stratum),
             scales = "free_y") +
  scale_color_discrete(name   = "Quantiles",
                       labels = c("95%", "97.5%", "100%")) +
  labs(x = "Maximum cohort age (y)",
       y = "Density") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

ggsave(str_c(folder_out, "max_ages_c3_NOM_str_spc.jpg"),
       width  = 16,
       height = 8,
       units  = "cm",
       scale  = 1.5)

# save table
age_quant_str_spc %>% 
  pivot_wider(names_from  = "quantile",
              values_from = "value") %>% 
  write_delim(str_c(folder_out, "max_ages_c3_NOM_quantiles.csv"),
              delim = ";")

rm(list = setdiff(ls(), c("folder_out", "gg_dH", "gg_ddmax", "gg_maxage")))
