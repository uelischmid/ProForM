## compare dH-results
## across strata & species
## at no competition
## strata & qualities used in c3
## 23.1.24


# setup -------------------------------------------------------------------
library(tidyverse)

# folder_out <- "results/dH_comparison_c3/"

simulations <- list.dirs("data/processed/simoutput/",
                         full.names = FALSE,
                         recursive  = FALSE)
simulations <- simulations[str_detect(simulations, "NOM")]
simulations <- simulations[str_detect(simulations, "LT")]
simulations <- simulations[str_detect(simulations, "_51_", negate = TRUE)]


h_d12 <- list()
h_d12$aalb <- 862
h_d12$pabi <- 850
h_d12$fsyl <- 1612
h_d12$apse <- 1702

# growth function ---------------------------------------------------------
f.dyn_hgreg <- function(Species, H, CIh, Topo = "even", p = param) {
  ## Inputs
  # Species, H, CIh, Topo
  # p: whole parameter list
  
  ## Output
  # dyn_H for trees with D < 12 cm
  
  if (Species == "pabi") {
    dyn_H_log <- 
      p$hgreg_int_pabi +
      p$hgreg_c_h_pabi * log(H) +
      p$hgreg_c_ci_pabi * CIh +
      p$hgreg_c_dd1_pabi * p$hgreg_v_dd1_all +
      p$hgreg_c_spei1_pabi * p$hgreg_v_spei1_all +
      p$hgreg_c_spei2_pabi * p$hgreg_v_spei2_all
    
    if (Topo == "mound") {
      dyn_H_log <- dyn_H_log + p$hgreg_c_topo_mound_pabi
    } else if (Topo == "pit") {
      dyn_H_log <- dyn_H_log + p$hgreg_c_topo_pit_pabi
    }
    
    dyn_H_temp <- exp(dyn_H_log)
    
  } else if (Species == "aalb") {
    dyn_H_log <- 
      p$hgreg_int_aalb +
      p$hgreg_c_h_aalb * log(H) +
      p$hgreg_c_ci_aalb * CIh +
      p$hgreg_c_dd2_aalb * p$hgreg_v_dd2_all +
      p$hgreg_c_spei1_aalb * p$hgreg_v_spei1_all +
      p$hgreg_c_slasp_aalb * p$sim_sloasp
    
    if (Topo == "mound") {
      dyn_H_log <- dyn_H_log + p$hgreg_c_topo_mound_aalb
    } else if (Topo == "pit") {
      dyn_H_log <- dyn_H_log + p$hgreg_c_topo_pit_aalb
    }
    
    dyn_H_temp <- exp(dyn_H_log)
    
  } else if (Species == "fsyl") {
    dyn_H_log <- 
      p$hgreg_int_fsyl +
      p$hgreg_c_h_fsyl * log(H) +
      p$hgreg_c_ci_fsyl * CIh +
      p$hgreg_c_dd2_fsyl * p$hgreg_v_dd2_all +
      p$hgreg_c_spei1_fsyl * p$hgreg_v_spei1_all +
      p$hgreg_c_slasp_fsyl * p$sim_sloasp
    
    dyn_H_temp <- exp(dyn_H_log)
    
  } else if (Species == "apse") {
    dyn_H_log <- 
      p$hgreg_int_apse +
      p$hgreg_c_h_apse * log(H) +
      p$hgreg_c_ci_apse * CIh +
      p$hgreg_c_dd1_apse * p$hgreg_v_dd1_all +
      p$hgreg_c_dd2_apse * p$hgreg_v_dd2_all +
      p$hgreg_c_spei1_apse * p$hgreg_v_spei1_all +
      p$hgreg_c_spei2_apse * p$hgreg_v_spei2_all +
      p$hgreg_c_slasp_apse * p$sim_sloasp
    
    dyn_H_temp <- exp(dyn_H_log)
    
  } else {
    dyn_H_temp <- 999999999
  }
  
  # modify growth with prefactor
  dyn_H <- p$pf_dh * dyn_H_temp
  
  return(dyn_H)
}


# calculate dH ------------------------------------------------------------
res_dH <- vector(mode = "list", length = length(simulations))
df_aalb <- tibble(Species = "aalb",
                  H       = seq(10, h_d12$aalb, 10),
                  CIh     = 0)
df_apse <- tibble(Species = "apse",
                  H       = seq(10, h_d12$apse, 10),
                  CIh     = 0)
df_fsyl <- tibble(Species = "fsyl",
                  H       = seq(10, h_d12$fsyl, 10),
                  CIh     = 0)
df_pabi <- tibble(Species = "pabi",
                  H       = seq(10, h_d12$pabi, 10),
                  CIh     = 0)
df_full <- bind_rows(df_aalb, df_apse, df_fsyl, df_pabi)
rm(df_aalb, df_apse, df_fsyl, df_pabi)

for (i in seq_along(simulations)) {
  simulation <- simulations[i]
  param <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/param.rds"))
  sim_info <- str_split(simulation, "_") %>% 
    magrittr::extract2(1)
  
  if (sim_info[3] == "UM") {
    df <- df_full %>% 
      mutate(Stratum = sim_info[3],
             Qreg    = as.numeric(str_sub(sim_info[4], -1, -1))) %>% 
      select(Stratum, Qreg, everything())
  } else if (sim_info[3] == "HM") {
    df <- df_full %>% 
      filter(Species %in% c("aalb", "pabi")) %>%
      mutate(Stratum = sim_info[3],
             Qreg    = as.numeric(str_sub(sim_info[4], -1, -1))) %>% 
      select(Stratum, Qreg, everything())
  } else if (sim_info[3] == "SA") {
    df <- df_full %>% 
      filter(Species == "pabi") %>% 
      mutate(Stratum = sim_info[3],
             Qreg    = as.numeric(str_sub(sim_info[4], -1, -1))) %>% 
      select(Stratum, Qreg, everything())
  }
  
  df$dH <- pmap_dbl(list(Species = df$Species,
                         H       = df$H,
                         CIh     = df$CIh),
                    f.dyn_hgreg)
  res_dH[[i]] <- df
}

res_dH <- bind_rows(res_dH)


# save res ----------------------------------------------------------------
write_delim(res_dH,
            str_c(folder_out, "dH_comparison.csv"),
            delim = ";")


# vis res -----------------------------------------------------------------
res_dH <- res_dH %>% 
  mutate(Stratum = factor(Stratum, levels = c("UM", "HM", "SA")),
         Species = factor(Species, levels = c("pabi", "aalb", "fsyl", "apse")),
         Qreg    = factor(Qreg, levels = c("5", "3", "1")))

gg_dH <- res_dH %>% 
  pivot_wider(names_from = Qreg,
              names_prefix = "dH_Qreg_",
              values_from = dH) %>% 
  ggplot(aes(H, dH_Qreg_3, ymin = dH_Qreg_1, ymax = dH_Qreg_5, color = Species, fill = Species)) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  facet_wrap(~Stratum) +
  labs(x = "H (cm)",
       y = "dH (cm/y)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

ggsave(str_c(folder_out, "dH_comparison.jpg"),
       width = 16,
       height = 8,
       units = "cm",
       scale = 1.5)

rm(list = setdiff(ls(), c("folder_out", "gg_dH")))
