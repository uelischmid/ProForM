#####
# visualize nais assessment
# one simulation
# F2
# 7.6.22, us
#####


PROFORM_vis_1_nais_F2 <- function(nais_assessment, avg_across, out_subfolder) {
  # setup -------------------------------------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))

  # load data ---------------------------------------------------------------
  nais_indices_red <- read_rds(str_c("data/processed/naisoutput/2_assessment/", nais_assessment, "/nais_indices_red.rds"))
  
  
  # summarise across years (rolling mean) -----------------------------------
  if (!is.na(avg_across)) {
    nais_indices_red <- nais_indices_red %>% 
      mutate(across(mix:sapthi, ~zoo::rollmean(.x,
                                               k     = avg_across,
                                               fill  = NA,
                                               na.rm = TRUE)))
  }
  
  
  # plot --------------------------------------------------------------------
  gg <- nais_indices_red %>%
    pivot_longer(cols      = c(mix:sapthi),
                 names_to  = "index",
                 values_to = "value") %>%
    mutate(index = factor(index, levels = c("mix", "vert", "horiz", "supptr", "seedl", "sapthi"))) %>%
    ggplot(aes(time_Step, value)) +
    geom_line() +
    geom_point() +
    facet_wrap(~index, ncol = 1) +
    scale_x_continuous(name = "time [y]") +
    scale_y_continuous(name   = "",
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1),
                       labels = c("very bad", "minimal", "ideal")) +
    coord_flip() +
    labs(title    = "NaiS form 2",
         subtitle = nais_assessment) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
  
  if (!is.na(avg_across)) {
    gg <- gg +
      labs(caption = str_c("Rolling mean across ", avg_across, " years"))
  }
  

  # save --------------------------------------------------------------------
  out_folder <- "results/nais_vis/1_sim_F2/"
  if (!is.na(out_subfolder)){
    out_folder <- str_c(out_folder, out_subfolder, "/")
    dir.create(out_folder, showWarnings = FALSE)
  }
  
  if (is.na(avg_across)) {
    fname <- str_c(out_folder, "F2_", nais_assessment, ".png")
  } else {
    fname <- str_c(out_folder, "F2_", nais_assessment, "_rm", avg_across, ".png")
  }
  
  suppressWarnings(ggsave(filename = fname,
                          width    = 5,
                          height   = 15,
                          units    = "cm",
                          scale    = 2))
}

