#####
# visualize nais assessment
# multiple simulations
# F2
# 7.6.22, us
#####


PROFORM_vis_n_nais_F2 <- function(nais_assessments, names, comb_name, avg_across, out_subfolder) {
  # setup -------------------------------------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))
  
  
  # load data  -------------------------------------------------
  # reduced nais inidces
  nais_indices_red <- vector(mode = "list", length = length(nais_assessments))
  for (i in seq_along(nais_indices_red)) {
    nais_indices_red[[i]] <- read_rds(str_c("data/processed/naisoutput/2_assessment/", nais_assessments[i], "/nais_indices_red.rds")) %>% 
      mutate(name = names[i])
  }
  nais_indices_red <- bind_rows(nais_indices_red) %>% 
    mutate(name = factor(name, levels = names))
  
  
  # summarise across years (rolling mean) -----------------------------------
  if (!is.na(avg_across)) {
    nais_indices_red <- nais_indices_red %>% 
      group_by(name) %>% 
      mutate(across(mix:sapthi, ~zoo::rollmean(.x,
                                               k     = avg_across,
                                               fill  = NA,
                                               na.rm = TRUE))) %>% 
      ungroup()
  }
  
  
  # plot --------------------------------------------------------------------
  gg <- nais_indices_red %>% 
    pivot_longer(cols      = c(mix:sapthi),
                 names_to  = "index",
                 values_to = "value") %>% 
    mutate(index = factor(index, levels = c("mix", "vert", "horiz", "supptr", "seedl", "sapthi"))) %>% 
    ggplot(aes(time_Step, value, color = name)) +
    geom_line(alpha = 0.8) +
    geom_point(alpha = 0.8) +
    facet_wrap(~index, ncol = 1) +
    scale_x_continuous(name = "time [y]") +
    scale_y_continuous(name   = "",
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1),
                       labels = c("very bad", "minimal", "ideal")) +
    scale_color_discrete(name = "simulation") +
    coord_flip() +
    labs(title = "NaiS form 2") +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
  
  if (!is.na(avg_across)) {
    gg <- gg +
      labs(caption = str_c("Average across ", avg_across, " years"))
  }
  
  
  # save --------------------------------------------------------------------
  out_folder <- "results/nais_vis/n_sim_F2/"
  if (!is.na(out_subfolder)){
    out_folder <- str_c(out_folder, out_subfolder, "/")
    dir.create(out_folder, showWarnings = FALSE)
  }
  
  if (is.na(avg_across)) {
    fname <- str_c(out_folder, "F2_", comb_name, ".png")
  } else {
    fname <- str_c(out_folder, "F2_", comb_name, "_avg", avg_across, ".png")
  }
  
  suppressWarnings(ggsave(filename = fname,
                          width    = 4,
                          height   = 15,
                          units    = "cm",
                          scale    = 2))
}

