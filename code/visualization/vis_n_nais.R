#####
# visualize nais assessment
# multiple simulations
# 7.6.22, us
#####


PROFORM_vis_n_nais <- function(nais_assessments, names, comb_name, avg_across, out_subfolder) {
  suppressWarnings(suppressMessages(library(tidyverse)))
  
  
  # load data  -------------------------------------------------
  # reduced nais inidces
  nais_indices_red <- vector(mode = "list", length = length(nais_assessments))
  for (i in seq_along(nais_indices_red)) {
    nais_indices_red[[i]] <- read_rds(str_c("data/processed/naisoutput/", nais_assessments[i], "/nais_indices_red.rds")) %>% 
      mutate(name = names[i])
  }
  nais_indices_red <- bind_rows(nais_indices_red) %>% 
    mutate(name = factor(name, levels = names))
  
  
  
  # summarise across years --------------------------------------------------
  if (!is.na(avg_across)) {
    nais_indices_red_gr <- nais_indices_red %>% 
      mutate(ts_group = cut_width(time_Step,
                                  width = avg_across,
                                  boundary = min(time_Step) - 1)) %>% 
      group_by(name, ts_group) %>% 
      mutate(ts_group_min = min(time_Step),
             ts_group_max = max(time_Step),
             ts_group_mean = (ts_group_min + ts_group_max) / 2)
    
    ts_means <- nais_indices_red_gr %>% 
      select(simulation:rockfall_scenario, name:ts_group_mean) %>% 
      distinct() %>% 
      rename(time_Step = "ts_group_mean")
    
    nais_indices_red <- nais_indices_red_gr %>% 
      summarise(across(mix:sapthi, mean, na.rm = TRUE),
                .groups = "drop") %>% 
      left_join(ts_means, by = c("name", "ts_group")) %>% 
      select(-starts_with("ts_gr"))
  }
  
  
  
  # plot --------------------------------------------------------------------
  gg <- nais_indices_red %>% 
    pivot_longer(cols = -c(simulation, natural_hazard, rockfall_scenario, time_Step, name),
                 names_to = "index",
                 values_to = "value") %>% 
    mutate(index = factor(index, levels = c("mix", "vert", "horiz", "supptr", "seedl", "sapthi"))) %>% 
    ggplot(aes(time_Step, value, color = name)) +
    geom_line(alpha = 0.8) +
    geom_point(alpha = 0.8) +
    facet_wrap(~index, ncol = 1) +
    scale_x_continuous(name = "Jahre") +
    scale_y_continuous(name = "",
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1),
                       labels = c("very bad", "minimal", "ideal")) +
    scale_color_discrete(name = "Simulation") +
    coord_flip() +
    labs(title    = "NaiS Form 2") +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
  
  if (!is.na(avg_across)) {
    gg <- gg +
      labs(caption = str_c("Average across ", avg_across, " years"))
  }
  
  
  # save --------------------------------------------------------------------
  out_folder <- "results/nais_vis/n_sim/"
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
                          width = 4,
                          height = 15,
                          units = "cm",
                          scale = 2))
}

