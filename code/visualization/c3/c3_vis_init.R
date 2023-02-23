## visualize init files
## 25.1.23, us


# setup -------------------------------------------------------------------
library(tidyverse)

init_folder <- "data/processed/init/c3_init/"
init_files <- list.files(path    = init_folder,
                         pattern = ".csv")
out_folder <- "results/init_vis/"

# visualize init ----------------------------------------------------------
for (i in seq_along(init_files)) {
  init_file <- init_files[i]
  init_name <- str_remove(init_file, ".csv")
  
  init <- read_csv(str_c(init_folder, init_file),
                   show_col_types = FALSE) %>% 
    filter(D >= 4) %>% 
    mutate(Species = factor(Species, levels = c("aalb", "apse", "fsyl", "pabi", "none")),
           D_class = cut_width(D, width = 4, boundary = 0, closed = "left"))
    
    basn <- init %>% 
      filter(D >= 12) %>% 
      mutate(BA = N * pi * (0.5 * D / 100)^2) %>% 
      summarise(BA_ha = sum(BA),
                SN_ha = sum(N)) %>% 
      mutate(text = str_c("BA: ", round(BA_ha, 0), " m2/ha\n",
                          "SN: ", round(SN_ha, 0), "/ha"))
  
    gg <-  init %>%
      group_by(Species, D_class,
               .drop = FALSE) %>% 
      summarise(N_ha    = sum(N),
                .groups = "drop") %>% 
      filter(Species != "none") %>% 
      ggplot(aes(D_class, N_ha, fill = Species)) +
      geom_col() +
      geom_text(data        = basn,
                mapping     = aes(x = Inf, y = Inf, label = text),
                inherit.aes = FALSE,
                hjust       = 1,
                vjust       = 1) +
      labs(title    = init_name,
           caption  = "Limit DD: 4 cm; Limit BA/SN: 12 cm",
           x        = "Diameter class",
           y        = "N/ha") +
      scale_x_discrete(breaks = function(x) {x[c(TRUE, rep(FALSE, 2 - 1))]}) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            axis.text.x      = element_text(angle = 90))
    
    ggsave(str_c(out_folder, init_name, ".png"))
}

