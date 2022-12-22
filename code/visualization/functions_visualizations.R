#####
# visualize model output
# plot functions
# 14.05.2021, us
#####


# custom ggplot2-theme ----------------------------------------------------
theme_custom <- theme_bw() +
  theme(panel.grid.minor = element_blank())

# basal area development --------------------------------------------------
f.vis_ba_1sim <- function(standdev, capt, dlimit, p = param, c_theme = theme_custom) {
  gg_p <- standdev %>%
    group_by(time_Step2, Species,
             .drop = FALSE) %>%
    filter(D >= dlimit) %>% 
    summarise(ba = sum(BA) / p$sim_area_ha,
              .groups = "drop") %>%
    filter(Species != "none") %>% 
    ggplot(aes(time_Step2, ba, fill = Species)) +
    geom_area(position = "stack",
              alpha = 0.8) +
    labs(title = str_c("Stand Basal Area (D>=", dlimit, "cm)"),
         caption = capt,
         x = "Time Step", 
         y = bquote("BA "(m^2/ha))) +
    c_theme
  return(gg_p)
}

f.vis_ba_nsim_efm <- function(simstanddev, efm, targetrange, myears, dlimit, capt,
                              p = param, c_theme = theme_custom) {
  BA_SIM <- simstanddev %>%
    group_by(time_Step2, name,
             .drop = FALSE) %>% 
    filter(D >= dlimit) %>% 
    summarise(total_BAha = sum(BA) / p$sim_area_ha,
              .groups = "drop")
  
  if (!is.null(efm)) {
    # summarise efm
    BA_EFM <- efm$standdev %>% 
      group_by(time_Step2,
               .drop = FALSE) %>% 
      filter(diameter >= dlimit) %>% 
      summarise(total_BAha = sum(BA_calc_m2) / (efm$meta$area)) %>% 
      mutate(name = "EFM") %>% 
      select(time_Step2, name, total_BAha)
    
    # combine data
    BA_full <- bind_rows(BA_SIM, BA_EFM) %>% 
      mutate(name = factor(name, levels = c(levels(BA_SIM$name), "EFM")))
    
    # scales
    colorscale <- c(scales::hue_pal()(length(levels(BA_SIM$name))), "#000000")
    ltyscale <- c(rep(1, length(levels(BA_SIM$name))), 3)
    
  } else {
    # copy data
    BA_full <- BA_SIM
    
    # scales
    colorscale <- scales::hue_pal()(length(levels(BA_SIM$name)))
    ltyscale <- rep(1, length(levels(BA_SIM$name)))
  }
  
  gg_p <- ggplot(BA_full, aes(time_Step2, total_BAha, color = name, lty = name)) +
    geom_line() +
    scale_color_manual(values = colorscale) +
    scale_linetype_manual(values = ltyscale) +
    ylim(0, NA) +
    labs(title = str_c("Stand Basal Area (D>=", dlimit, "cm)"),
         caption = capt,
         x = "Time Step",
         y = bquote("BA "(m^2/ha))) +
    guides(color = guide_legend(title = NULL),
           linetype = FALSE) +
    theme_custom
  
  if (!is.null(efm)) {
    gg_p <- gg_p +
      geom_point(data = BA_EFM)
    
    if (!is.na(targetrange)) {
      gg_p$layers <- c(geom_ribbon(data = BA_EFM,
                                   aes(x = time_Step2,
                                       ymin = total_BAha * (1 - targetrange),
                                       ymax = total_BAha * (1 + targetrange)),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       geom_ribbon(data = BA_EFM,
                                   aes(x = time_Step2,
                                       ymin = total_BAha * (1 - (targetrange * 2 / 3)),
                                       ymax = total_BAha * (1 + (targetrange * 2 / 3))),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       geom_ribbon(data = BA_EFM,
                                   aes(x = time_Step2,
                                       ymin = total_BAha * (1 - (targetrange * 1 / 3)),
                                       ymax = total_BAha * (1 + (targetrange * 1 / 3))),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       gg_p$layers)
      
      gg_p <- gg_p +
        annotate(geom = "text",
                 label = str_c("Grey ribbons: EFM-Data \u00B1",
                               round(1 / 3 * targetrange * 100, 0), "/",
                               round(2 / 3 * targetrange * 100, 0), "/",
                               round(targetrange * 100, 0), 
                               "%  "),
                 x = Inf, y = 0,
                 hjust = "inward", size = 3.5)
    }
    
    if (sum(is.na(myears)) == 0) {
      gg_p <- gg_p +
      geom_text(data = myears,
                mapping = aes(x = time_Step2,
                              y = y_val,
                              vjust = vjust_val,
                              label = label_val),
                inherit.aes = FALSE,
                size = 10) +
        labs(subtitle = "*  years with >= 5% trees missing in EFM-data")
      
      
    }
    
  }
  return(gg_p)
}


# volume development ------------------------------------------------------
f.vis_vol_1sim <- function(standdev, capt, dlimit, p = param, c_theme = theme_custom) {
  gg_p <- standdev %>%
    group_by(time_Step2, Species,
             .drop = FALSE) %>%
    filter(D >= dlimit) %>% 
    summarise(vol = sum(Vol) / p$sim_area_ha,
              .groups = "drop") %>%
    filter(Species != "none") %>% 
    ggplot(aes(time_Step2, vol, fill = Species)) +
    geom_area(position = "stack",
              alpha = 0.8) +
    labs(title = str_c("Stand Volume (D>=", dlimit, "cm)"),
         caption = capt,
         x = "Time Step", 
         y = bquote("Volume "(m^3/ha))) +
    c_theme
  return(gg_p)
}

f.vis_vol_nsim <- function(standdev, capt, dlimit, p = param, c_theme = theme_custom) {
  standdev %>%
    group_by(time_Step2, name,
             .drop = FALSE) %>% 
    filter(D >= dlimit) %>% 
    summarise(total_Vol = sum(Vol) / p$sim_area_ha,
              .groups = "drop") %>% 
    ggplot(aes(time_Step2, total_Vol, color = name)) +
    geom_line() +
    ylim(0, NA) +
    labs(title = str_c("Stand Volume (D>=", dlimit, "cm)"),
         caption = capt,
         x = "Time Step", 
         y = bquote("Volume "(m^3/ha)),
         color = NULL) +
    c_theme
}


# stem number development -------------------------------------------------
f.vis_sn_1sim <- function(standdev, capt, dlimit, p = param, c_theme = theme_custom) {
  gg_p <- standdev %>% 
    filter(D >= dlimit) %>% 
    group_by(time_Step2, Species,
             .drop = FALSE) %>%
    summarise(total_N = sum(N) / p$sim_area_ha,
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    ggplot(aes(time_Step2, total_N, fill = Species)) +
    geom_area(position = "stack",
              alpha = 0.8) +
    ylim(0, NA) +
    labs(title = str_c("Stem Number (D>=", dlimit, "cm)"),
         caption = capt,
         x = "Time Step", 
         y = "N/ha") +
    c_theme
  return(gg_p)
}

f.vis_sn_nsim_efm <- function(simstanddev, efm, targetrange, myears, dlimit, capt,
                              p = param, c_theme = theme_custom) {
  SN_SIM <- simstanddev %>%
    filter(D >= dlimit) %>% 
    group_by(time_Step2, name) %>% 
    summarise(total_Nha = sum(N) / param$sim_area_ha,
              .groups = "drop")
  
  if (!is.null(efm)) {
    # summarise efm
    SN_EFM <- efm$standdev %>% 
      filter(diameter >= dlimit) %>% 
      group_by(time_Step2) %>% 
      summarise(total_Nha = n() / (efm$meta$area)) %>% 
      mutate(name = "EFM") %>% 
      select(time_Step2, name, total_Nha)
    
    # combine data
    SN_full <- bind_rows(SN_SIM, SN_EFM) %>% 
      mutate(name = factor(name, levels = c(levels(SN_SIM$name), "EFM")))
    
    # scales
    colorscale <- c(scales::hue_pal()(length(levels(SN_SIM$name))), "#000000")
    ltyscale <- c(rep(1, length(levels(SN_SIM$name))), 3)
    
  } else {
    # copy data
    SN_full <- SN_SIM
    
    # scales
    colorscale <- scales::hue_pal()(length(levels(SN_SIM$name)))
    ltyscale <- rep(1, length(levels(SN_SIM$name)))
  }
  
  gg_p <- ggplot(SN_full, aes(time_Step2, total_Nha, color = name, lty = name)) +
    geom_line() +
    scale_color_manual(values = colorscale) +
    scale_linetype_manual(values = ltyscale) +
    ylim(0, NA) +
    labs(title = str_c("Stem Number (D>=", dlimit, "cm)"),
         caption = capt,
         x = "Time Step",
         y = "N/ha") +
    guides(color = guide_legend(title = NULL),
           linetype = FALSE) +
    theme_custom
  
  if (!is.null(efm)) {
    gg_p <- gg_p +
      geom_point(data = SN_EFM)
    
    if (!is.na(targetrange)) {
      gg_p$layers <- c(geom_ribbon(data = SN_EFM,
                                   aes(x = time_Step2,
                                       ymin = total_Nha * (1 - targetrange),
                                       ymax = total_Nha * (1 + targetrange)),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       geom_ribbon(data = SN_EFM,
                                   aes(x = time_Step2,
                                       ymin = total_Nha * (1 - (targetrange * 2 / 3)),
                                       ymax = total_Nha * (1 + (targetrange * 2 / 3))),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       geom_ribbon(data = SN_EFM,
                                   aes(x = time_Step2,
                                       ymin = total_Nha * (1 - (targetrange * 1 / 3)),
                                       ymax = total_Nha * (1 + (targetrange * 1 / 3))),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       gg_p$layers)
      
      gg_p <- gg_p +
        annotate(geom = "text",
                 label = str_c("Grey ribbons: EFM-Data \u00B1",
                               round(1 / 3 * targetrange * 100, 0), "/",
                               round(2 / 3 * targetrange * 100, 0), "/",
                               round(targetrange * 100, 0), 
                               "%  "),
                 x = Inf, y = 0,
                 hjust = "inward", size = 3.5)
    }
    
    if (sum(is.na(myears)) == 0) {
      gg_p <- gg_p +
        geom_text(data = myears,
                  mapping = aes(x = time_Step2,
                                y = y_val,
                                vjust = vjust_val,
                                label = label_val),
                  inherit.aes = FALSE,
                  size = 10) +
        labs(subtitle = "*  years with >= 5% trees missing in EFM-data")
    }
  }
  return(gg_p)
}


# canopy cover development ------------------------------------------------
f.vis_cc_1sim <- function(standdev_lc, capt, p = param, c_theme = theme_custom) {
  gg_p <- standdev_lc %>%
    filter(Stage >= 4) %>% 
    group_by(time_Step2, Species,
             .drop = FALSE) %>%
    summarise(cc_perc = n() / p$sim_ncells,
              .groups = "drop") %>%
    filter(Species != "none") %>% 
    ggplot(aes(time_Step2, cc_perc, fill = Species)) +
    geom_area(position = "stack",
              alpha = 0.8) +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::percent) +
    labs(title = "Canopy Cover > 12 cm DBH (cohort with largest DBH)",
         caption = capt,
         x = "Time Step", 
         y = "Canopy Cover") +
    c_theme
  return(gg_p)
}

f.vis_cc_nsim <- function(standdev_lc, capt, p = param, c_theme = theme_custom) {
  cc <- standdev_lc %>% 
    filter(Stage >= 4) %>% 
    group_by(name, time_Step2) %>% 
    summarise(cc_perc = n() / p$sim_ncells,
              .groups = "drop")
  
  gg_p <- ggplot(cc, aes(time_Step2, cc_perc, color = name)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::percent) +
    labs(title = "Canopy Cover > 12 cm DBH (cohort with largest DBH)",
         caption = capt,
         x = "Time Step", 
         y = "Canopy Cover",
         color = NULL) +
    c_theme
  return(gg_p)
}

# crown ratios ------------------------------------------------------------
f.vis_cr_1sim <- function(standdev, capt, p = param, c_theme = theme_custom) {
  dom100_N <- round(p$sim_area_ha * 100, 0)
  
  avg_CR_all <- standdev %>% 
    group_by(time_Step2, Species,
             .drop = FALSE) %>% 
    summarise(average_CR = weighted.mean(CR, w = N, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "all")
  
  avg_CR_12plus <- standdev %>% 
    group_by(time_Step2, Species,
             .drop = FALSE) %>% 
    filter(D >= 12) %>% 
    summarise(average_CR = weighted.mean(CR, w = N, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "D >= 12cm")
  
  avg_CR_dom100 <- standdev %>% 
    select(time_Step2, Species, N, D, CR) %>% 
    uncount(N) %>% 
    group_by(time_Step2) %>% 
    slice_max(order_by = D,
              n = dom100_N,
              with_ties = FALSE) %>% 
    group_by(Species,
             .add = TRUE, .drop = FALSE) %>% 
    summarise(average_CR = mean(CR, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "dom100")
  
  avg_CR_comb <- bind_rows(avg_CR_all, avg_CR_12plus, avg_CR_dom100) %>% 
    mutate(Trees = factor(group, levels = c("all", "D >= 12cm", "dom100"))) %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  
  gg_p <- ggplot(avg_CR_comb, aes(time_Step2, average_CR, color = Species)) +
    geom_line(na.rm = TRUE) +
    scale_color_discrete(drop = FALSE) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    labs(title = "Average Crown Ratio",
         caption = capt,
         x = "Time Step",
         y = "Crown Ratio") +
    facet_wrap(~Trees) +
    c_theme
  
  return(gg_p)
}

f.vis_cr_nsim_efm <- function(simstanddev, efm, targetrange, myears, capt,
                              p = param, c_theme = theme_custom) {
  # extract CR-averages from sim-data
  sim_dom100_N <- round(p$sim_area_ha * 100, 0)
  
  sim_CR_all_comb <- simstanddev %>% 
    group_by(time_Step2, name) %>% 
    summarise(average_CR = weighted.mean(CR, w = N, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "all",
           Species = "combined") %>% 
    select(time_Step2, name, Species, average_CR, group)
  
  sim_CR_all_sp <- simstanddev %>% 
    group_by(time_Step2, name, Species,
             .drop = FALSE) %>% 
    summarise(average_CR = weighted.mean(CR, w = N, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "all")
  
  sim_CR_12plus_comb <- simstanddev %>% 
    filter(D >= 12) %>% 
    group_by(time_Step2, name) %>% 
    summarise(average_CR = weighted.mean(CR, w = N, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "D >= 12cm",
           Species = "combined") %>% 
    select(time_Step2, name, Species, average_CR, group)
  
  sim_CR_12plus_sp <- simstanddev %>% 
    filter(D >= 12) %>% 
    group_by(time_Step2, name, Species,
             .drop = FALSE) %>% 
    summarise(average_CR = weighted.mean(CR, w = N, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "D >= 12cm")
  
  sim_CR_dom100_temp <- simstanddev %>% 
    select(name, time_Step2, Species, N, D, CR) %>% 
    uncount(N) %>% 
    group_by(time_Step2, name) %>% 
    slice_max(order_by = D,
              n = sim_dom100_N,
              with_ties = FALSE)
  
  sim_CR_dom100_comb <- sim_CR_dom100_temp %>% 
    group_by(name, time_Step2) %>% 
    summarise(average_CR = mean(CR, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "dom100",
           Species = "combined") %>% 
    select(time_Step2, name, Species, average_CR, group)
  
  sim_CR_dom100_sp <- sim_CR_dom100_temp %>% 
    group_by(name, time_Step2, Species,
             .drop = FALSE) %>% 
    summarise(average_CR = mean(CR, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "dom100")
  
  sim_CR_comb <- bind_rows(sim_CR_all_comb, sim_CR_all_sp,
                           sim_CR_12plus_comb, sim_CR_12plus_sp,
                           sim_CR_dom100_comb, sim_CR_dom100_sp) %>% 
    mutate(Species = factor(Species, levels = c("combined", levels(simstanddev$Species))),
           Trees = factor(group, levels = c("all", "D >= 12cm", "dom100"))) %>% 
    filter(Species != "none") %>% 
    filter(Species != "other") %>% 
    mutate(Species = fct_drop(Species))
  
  if (!is.null(efm)) {
    # extract CR-averages from efm-data
    efm_dom100_N <- round(efm$meta$area * 100, 0)
    
    efm_CR_all_comb <- efm$standdev %>% 
      group_by(time_Step2) %>% 
      summarise(average_CR = mean(CR_calc)) %>% 
      mutate(group = "all",
             Species = "combined") %>% 
      select(time_Step2, Species, average_CR, group)
    
    efm_CR_all_sp <- efm$standdev %>% 
      group_by(time_Step2, Species,
               .drop = FALSE) %>% 
      summarise(average_CR = mean(CR_calc),
                .groups = "drop") %>% 
      mutate(group = "all")
    
    efm_CR_12plus_comb <- efm$standdev %>% 
      filter(diameter >= 12) %>% 
      group_by(time_Step2) %>% 
      summarise(average_CR = mean(CR_calc)) %>% 
      mutate(group = "D >= 12cm",
             Species = "combined") %>% 
      select(time_Step2, Species, average_CR, group)
    
    efm_CR_12plus_sp <- efm$standdev %>% 
      filter(diameter >= 12) %>% 
      group_by(time_Step2, Species,
               .drop = FALSE) %>% 
      summarise(average_CR = mean(CR_calc),
                .groups = "drop") %>% 
      mutate(group = "D >= 12cm")
    
    efm_CR_dom100_comb <- efm$standdev %>% 
      group_by(time_Step2) %>% 
      slice_max(order_by = diameter,
                n = efm_dom100_N,
                with_ties = FALSE) %>% 
      summarise(average_CR = mean(CR_calc)) %>% 
      mutate(group = "dom100",
             Species = "combined") %>% 
      select(time_Step2, Species, average_CR, group)
    
    efm_CR_dom100_sp <- efm$standdev %>% 
      group_by(time_Step2) %>% 
      slice_max(order_by = diameter,
                n = efm_dom100_N,
                with_ties = FALSE) %>% 
      group_by(time_Step2, Species,
               .drop = FALSE) %>% 
      summarise(average_CR = mean(CR_calc),
                .groups = "drop") %>% 
      mutate(group = "dom100")
    
    
    efm_CR_comb <- bind_rows(efm_CR_all_comb, efm_CR_all_sp,
                             efm_CR_12plus_comb, efm_CR_12plus_sp,
                             efm_CR_dom100_comb, efm_CR_dom100_sp) %>% 
      mutate(Species = factor(Species, levels = c("combined", levels(efm$standdev$Species))),
             Trees = factor(group, levels = c("all", "D >= 12cm", "dom100")),
             name = "EFM") %>% 
      filter(Species != "none") %>% 
      filter(Species != "other") %>% 
      mutate(Species = fct_drop(Species)) %>% 
      select(time_Step2, name, Species, average_CR, group, Trees)
    
    # combine data
    CR_comb <- bind_rows(sim_CR_comb, efm_CR_comb) %>% 
      mutate(name = factor(name, levels = c(levels(sim_CR_comb$name), "EFM")))
    
    # scales
    colorscale <- c(scales::hue_pal()(length(levels(sim_CR_comb$name))), "#000000")
    ltyscale <- c(rep(1, length(levels(sim_CR_comb$name))), 3)
  } else {
    # copy data
    CR_comb <- sim_CR_comb
    
    # scales
    colorscale <- scales::hue_pal()(length(levels(sim_CR_comb$name)))
    ltyscale <- rep(1, length(levels(sim_CR_comb$name)))
  }
  
  gg_p <- ggplot(CR_comb, aes(time_Step2, average_CR, color = name, lty = name)) +
    geom_line() +
    scale_color_manual(values = colorscale) +
    scale_linetype_manual(values = ltyscale) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    labs(title = "Average Crown Ratio",
         caption = capt,
         x = "Time Step",
         y = "Crown Ratio") +
    guides(color = guide_legend(title = NULL),
           linetype = FALSE) +
    facet_grid(rows = vars(Species),
               cols = vars(Trees)) +
    c_theme
  
  
  if (!is.null(efm)) {
    gg_p <- gg_p +
      geom_point(data = efm_CR_comb,
                 size = 0.5)
    
    if (!is.na(targetrange)) {
      gg_p$layers <- c(geom_ribbon(data = efm_CR_comb,
                                   aes(x = time_Step2,
                                       ymin = average_CR * (1 - targetrange),
                                       ymax = pmin(1, average_CR * (1 + targetrange))),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       geom_ribbon(data = efm_CR_comb,
                                   aes(x = time_Step2,
                                       ymin = average_CR * (1 - (targetrange * 2 / 3)),
                                       ymax = pmin(1, average_CR * (1 + (targetrange * 2 / 3)))),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       geom_ribbon(data = efm_CR_comb,
                                   aes(x = time_Step2,
                                       ymin = average_CR * (1 - (targetrange * 1 / 3)),
                                       ymax = pmin(1, average_CR * (1 + (targetrange * 1 / 3)))),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       gg_p$layers)
      
      lab <- tibble(Species = "pabi",
                    Trees = "dom100",
                    label = str_c("Grey ribbons: EFM-Data \u00B1",
                                  round(1 / 3 * targetrange * 100, 0), "/",
                                  round(2 / 3 * targetrange * 100, 0), "/",
                                  round(targetrange * 100, 0), 
                                  "%  ")) %>% 
        mutate(Species = factor(Species, levels = levels(efm_CR_comb$Species)))
      
      gg_p <- gg_p +
        geom_text(data = lab,
                  aes(label = label),
                  x = Inf, y = 0,
                  hjust = "inward", size = 3.5,
                  inherit.aes = FALSE)
    }
    
    if (sum(is.na(myears)) == 0) {
      gg_p <- gg_p +
        geom_text(data = myears,
                  mapping = aes(x = time_Step2,
                                y = 0,
                                vjust = 0,
                                label = label_val),
                  inherit.aes = FALSE,
                  size = 10) +
        labs(subtitle = "*  years with >= 5% trees missing in EFM-data")
    }
  }
  
  return(gg_p)
}

f.vis_crdom_nsim_efm <- function(simstanddev, efm, targetrange, myears, capt,
                                 p = param, c_theme = theme_custom) {
  # extract CR-averages from sim-data
  sim_dom100_N <- round(p$sim_area_ha * 100, 0)
  
  sim_CR_dom100_temp <- simstanddev %>% 
    select(name, time_Step2, Species, N, D, CR) %>% 
    uncount(N) %>% 
    group_by(time_Step2, name) %>% 
    slice_max(order_by = D,
              n = sim_dom100_N,
              with_ties = FALSE)
  
  sim_CR_dom100_comb <- sim_CR_dom100_temp %>% 
    group_by(name, time_Step2) %>% 
    summarise(average_CR = mean(CR, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "dom100",
           Species = "combined") %>% 
    select(time_Step2, name, Species, average_CR, group)
  
  sim_CR_dom100_sp <- sim_CR_dom100_temp %>% 
    group_by(name, time_Step2, Species,
             .drop = FALSE) %>% 
    summarise(average_CR = mean(CR, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(group = "dom100")
  
  sim_CR_comb <- bind_rows(sim_CR_dom100_comb, sim_CR_dom100_sp) %>% 
    mutate(Species = factor(Species, levels = c("combined", levels(simstanddev$Species))),
           Trees = factor(group, levels = c("all", "D >= 12cm", "dom100"))) %>% 
    filter(Species != "none") %>% 
    filter(Species != "other") %>% 
    mutate(Species = fct_drop(Species))
  
  if (!is.null(efm)) {
    # extract CR-averages from efm-data
    efm_dom100_N <- round(efm$meta$area * 100, 0)
    
    efm_CR_dom100_comb <- efm$standdev %>% 
      group_by(time_Step2) %>% 
      slice_max(order_by = diameter,
                n = efm_dom100_N,
                with_ties = FALSE) %>% 
      summarise(average_CR = mean(CR_calc)) %>% 
      mutate(group = "dom100",
             Species = "combined") %>% 
      select(time_Step2, Species, average_CR, group)
    
    efm_CR_dom100_sp <- efm$standdev %>% 
      group_by(time_Step2) %>% 
      slice_max(order_by = diameter,
                n = efm_dom100_N,
                with_ties = FALSE) %>% 
      group_by(time_Step2, Species,
               .drop = FALSE) %>% 
      summarise(average_CR = mean(CR_calc),
                .groups = "drop") %>% 
      mutate(group = "dom100")
    
    
    efm_CR_comb <- bind_rows(efm_CR_dom100_comb, efm_CR_dom100_sp) %>% 
      mutate(Species = factor(Species, levels = c("combined", levels(efm$standdev$Species))),
             Trees = factor(group, levels = c("all", "D >= 12cm", "dom100")),
             name = "EFM") %>% 
      filter(Species != "none") %>% 
      filter(Species != "other") %>% 
      mutate(Species = fct_drop(Species)) %>% 
      select(time_Step2, name, Species, average_CR, group, Trees)
    
    # combine data
    CR_comb <- bind_rows(sim_CR_comb, efm_CR_comb) %>% 
      mutate(name = factor(name, levels = c(levels(sim_CR_comb$name), "EFM")))
    
    # scales
    colorscale <- c(scales::hue_pal()(length(levels(sim_CR_comb$name))), "#000000")
    ltyscale <- c(rep(1, length(levels(sim_CR_comb$name))), 3)
  } else {
    # copy data
    CR_comb <- sim_CR_comb
    
    # scales
    colorscale <- scales::hue_pal()(length(levels(sim_CR_comb$name)))
    ltyscale <- rep(1, length(levels(sim_CR_comb$name)))
  }
  
  gg_p <- ggplot(CR_comb, aes(time_Step2, average_CR, color = name, lty = name)) +
    geom_line() +
    scale_color_manual(values = colorscale) +
    scale_linetype_manual(values = ltyscale) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    labs(title = "Average Crown Ratio",
         caption = capt,
         x = "Time Step",
         y = "Crown Ratio") +
    guides(color = guide_legend(title = NULL),
           linetype = FALSE) +
    facet_grid(rows = vars(Species),
               cols = vars(Trees)) +
    c_theme
  
  
  if (!is.null(efm)) {
    gg_p <- gg_p +
      geom_point(data = efm_CR_comb,
                 size = 0.5)
    
    if (!is.na(targetrange)) {
      gg_p$layers <- c(geom_ribbon(data = efm_CR_comb,
                                   aes(x = time_Step2,
                                       ymin = average_CR * (1 - targetrange),
                                       ymax = pmin(1, average_CR * (1 + targetrange))),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       geom_ribbon(data = efm_CR_comb,
                                   aes(x = time_Step2,
                                       ymin = average_CR * (1 - (targetrange * 2 / 3)),
                                       ymax = pmin(1, average_CR * (1 + (targetrange * 2 / 3)))),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       geom_ribbon(data = efm_CR_comb,
                                   aes(x = time_Step2,
                                       ymin = average_CR * (1 - (targetrange * 1 / 3)),
                                       ymax = pmin(1, average_CR * (1 + (targetrange * 1 / 3)))),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       gg_p$layers)
      
      lab <- tibble(Species = "pabi",
                    Trees = "dom100",
                    label = str_c("Grey ribbons: EFM-Data \u00B1",
                                  round(1 / 3 * targetrange * 100, 0), "/",
                                  round(2 / 3 * targetrange * 100, 0), "/",
                                  round(targetrange * 100, 0), 
                                  "%  ")) %>% 
        mutate(Species = factor(Species, levels = levels(efm_CR_comb$Species)))
      
      gg_p <- gg_p +
        geom_text(data = lab,
                  aes(label = label),
                  x = Inf, y = 0,
                  hjust = "inward", size = 3.5,
                  inherit.aes = FALSE)
    }
    
    if (sum(is.na(myears)) == 0) {
      gg_p <- gg_p +
        geom_text(data = myears,
                  mapping = aes(x = time_Step2,
                                y = 0,
                                vjust = 0,
                                label = label_val),
                  inherit.aes = FALSE,
                  size = 10) +
        labs(subtitle = "*  years with >= 5% trees missing in EFM-data")
    }
  }
  
  return(gg_p)
}


# dbh-distributions -------------------------------------------------------
f.vis_dd_1sim <- function(standdev, dlimit, ps, capt, p = param, c_theme = theme_custom) {
  plotrows <- ceiling(length(ps) / 2)
  gg_p <- standdev %>%
    filter(State_nr == 1) %>%
    filter(D >= dlimit) %>% 
    filter(time_Step %in% ps) %>% 
    mutate(D_class = cut_width(D, width = 4, boundary = 0, closed = "left")) %>% 
    group_by(time_Step, Species, D_class,
             .drop = FALSE) %>% 
    summarise(N_sum = sum(N) / p$sim_area_ha,
              .groups = "drop") %>% 
    rename("Time Step" = time_Step) %>% 
    filter(Species != "none") %>% 
    ggplot(aes(D_class, N_sum, fill = Species)) +
    geom_col() +
    facet_wrap(~`Time Step`,
               ncol = 2,
               labeller = label_both,
               dir = "v",
               drop = FALSE,
               scales = "free_y") +
    labs(title = str_c("DBH-Distribution (D>=", dlimit, "cm)"),
         subtitle = "State 1, spring",
         caption = capt,
         x = "Diameter class",
         y = "N/ha") +
    scale_x_discrete(breaks = function(x) {x[c(TRUE, rep(FALSE, 2 - 1))]}) +
    c_theme +
    theme(axis.text.x = element_text(angle = 90))
  return(gg_p)
}

f.vis_dd_nsim_efm <- function(simstanddev, efm, targetrange, myears, dlimit, ps, capt,
                              p = param, c_theme = theme_custom) {
  # overwrite plotsteps if efm data is compared and no timesteps are specified
  if (!is.null(efm) && is.null(ps)) {
    ps <- efm$standdev %>% 
      filter(State_nr == 1) %>% 
      pull(time_Step2) %>% 
      unique()
  }
  # summarise sim
  DD_SIM <- simstanddev %>% 
    filter(State_nr == 1) %>% 
    filter(D >= dlimit) %>% 
    filter(time_Step2 %in% ps) %>% 
    mutate(D_class = cut_width(D, width = 4, boundary = 0, closed = "left"),
           `Time Step` = factor(time_Step2)) %>% 
    group_by(`Time Step`, D_class, name, .drop = FALSE) %>% 
    summarise(N_sum = sum(N) / p$sim_area_ha,
              .groups = "drop")
  
  if (!is.null(efm)) {
    # summarise efm
    DD_EFM <- efm$standdev %>% 
      filter(State_nr == 1) %>% 
      filter(diameter >= dlimit) %>% 
      filter(time_Step2 %in% ps) %>% 
      mutate(D_class = cut_width(diameter, width = 4, boundary = 0, closed = "left"),
             `Time Step` = factor(time_Step2),
             name = "EFM",
             name = factor(name)) %>% 
      group_by(`Time Step`, D_class, name, .drop = FALSE) %>% 
      summarise(N_sum = n() / (efm$meta$area),
                .groups = "drop")
    
    # combine data
    DD_full <- bind_rows(DD_SIM, DD_EFM) %>% 
      mutate(name = factor(name, levels = c(levels(DD_SIM$name), "EFM")))
    
    # scale
    colorscale <- c(scales::hue_pal()(length(levels(DD_SIM$name))), "#000000")
  } else {
    # copy data
    DD_full <- DD_SIM
    
    # scale
    colorscale <- scales::hue_pal()(length(levels(DD_SIM$name)))
  }
  
  gg_p <- ggplot(DD_full, aes(D_class, N_sum, group = name, color = name)) +
    geom_line() +
    scale_color_manual(values = colorscale) +
    scale_x_discrete(breaks = function(x) {x[c(TRUE, rep(FALSE, 2 - 1))]},
                     drop = FALSE) +
    facet_wrap(~`Time Step`,
               ncol = 2,
               labeller = label_both,
               dir = "v",
               drop = FALSE,
               scales = "free_y") +
    labs(title = str_c("DBH-Distribution (D>=", dlimit, "cm)"),
         subtitle = "State 1, spring",
         caption = capt,
         x = "Diameter class",
         y = "N/ha",
         color = NULL) +
    c_theme +
    theme(axis.text.x = element_text(angle = 90))
  
  if (!is.null(efm)) {
    if (sum(is.na(myears)) == 0) {
      myears <- myears %>% 
        mutate('Time Step' = factor(`Time Step`, levels = levels(DD_full$`Time Step`)))
      gg_p <- gg_p +
        geom_text(data = myears,
                  mapping = aes(x = Inf,
                                y = y_val,
                                vjust = vjust_val,
                                hjust = 1.5,
                                label = label_val),
                  inherit.aes = FALSE,
                  size = 10) +
        labs(subtitle = "State 1, spring; *  years with >= 5% trees missing in EFM-data")
    }
    
    if (!is.na(targetrange)) {
      gg_p$layers <- c(geom_ribbon(data = filter(DD_full, name == "EFM"),
                                   aes(x = D_class,
                                       ymin = N_sum * (1 - targetrange),
                                       ymax = N_sum * (1 + targetrange),
                                       group = name),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       geom_ribbon(data = filter(DD_full, name == "EFM"),
                                   aes(x = D_class,
                                       ymin = N_sum * (1 - (targetrange * 2 / 3)),
                                       ymax = N_sum * (1 + (targetrange * 2 / 3)),
                                       group = name),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       geom_ribbon(data = filter(DD_full, name == "EFM"),
                                   aes(x = D_class,
                                       ymin = N_sum * (1 - (targetrange * 1 / 3)),
                                       ymax = N_sum * (1 + (targetrange * 1 / 3)),
                                       group = name),
                                   fill = "grey70",
                                   alpha = 0.5,
                                   inherit.aes = FALSE,
                                   show.legend = FALSE),
                       gg_p$layers)
      
      lab <- tibble('Time Step' = tail(levels(DD_full$`Time Step`), 1),
                    label = str_c("Grey ribbons: EFM-Data \u00B1",
                                  round(1 / 3 * targetrange * 100, 0), "/",
                                  round(2 / 3 * targetrange * 100, 0), "/",
                                  round(targetrange * 100, 0),
                                  "%  ")) %>%
        mutate('Time Step' = factor(`Time Step`, levels = levels(DD_full$`Time Step`)))
      
      
      gg_p <- gg_p +
        geom_text(data = lab,
                  aes(label = label),
                  x = Inf, y = Inf,
                  hjust = "inward", vjust = "inward",
                  size = 3.5,
                  inherit.aes = FALSE)
      
    }
  }
  return(gg_p)
}


# stage maps --------------------------------------------------------------
f.vis_stgmps_1sim <- function(standdev_lc, coor, ps, capt, p = param, c_theme = theme_custom) {
  gg_p <- standdev_lc %>%
    filter(State_nr == 1) %>%
    filter(time_Step %in% ps) %>% 
    left_join(coor, by = c("cc_ID" = "cell_ID")) %>% 
    rename("Time Step" = time_Step) %>% 
    ggplot(aes(corner_x, corner_y, group = cc_ID, fill = factor(Stage))) +
    geom_polygon(color = "black", size = 0.1) +
    scale_fill_manual(name = "Stage",
                      values = c("#FFFFFF", viridis::viridis(8, direction = -1))) +
    facet_wrap(~`Time Step`,
               ncol = 2,
               labeller = label_both,
               drop = FALSE,
               dir = "v") +
    coord_equal() +
    labs(title = "Stage Maps (cohort with largest DBH)",
         subtitle = "State 1, spring",
         caption = capt,
         x = "x (m)",
         y = "y (m)") +
    c_theme +
    theme(panel.grid = element_blank())
  return(gg_p)
}


# species maps ------------------------------------------------------------
f.vis_spcmps_1sim <- function(standdev_lc, coor, ps, capt, p = param, c_theme = theme_custom) {
  gg_p <- standdev_lc %>%
    filter(State_nr == 1) %>%
    filter(time_Step %in% ps) %>% 
    left_join(coor, by = c("cc_ID" = "cell_ID")) %>% 
    rename("Time Step" = time_Step) %>% 
    ggplot(aes(corner_x, corner_y, group = cc_ID, fill = Species)) +
    geom_polygon(color = "black", size = 0.1) +
    scale_fill_manual(values = c("#FFFFFF", scales::hue_pal()(length(levels(standdev_lc$Species)) - 1)),
                      drop = FALSE) +
    facet_wrap(~`Time Step`,
               ncol = 2,
               labeller = label_both,
               drop = FALSE,
               dir = "v") +
    coord_equal() +
    labs(title = "Species Maps (cohort with largest DBH)",
         subtitle = "State 1, spring",
         caption = capt,
         x = "x (m)",
         y = "y (m)") +
    c_theme +
    theme(panel.grid = element_blank())
  return(gg_p)
}


# mgm: dbh-distributions --------------------------------------------------
f.vis_mgmdd_1sim <- function(mgmres, dlimit, capt, p = param, c_theme = theme_custom) {
  gg_p <- mgmres %>% 
    filter(s2_D >= dlimit) %>%
    filter(time_Step %in% p$mgm_interv_steps) %>%
    mutate(D_class = cut_width(s2_D, width = 4, boundary = 0, closed = "left")) %>% 
    group_by(time_Step, D_class) %>% 
    summarise(N_remain = sum(remain_N),
              N_remove = sum(mgm_N),
              .groups = "drop") %>%
    pivot_longer(cols = N_remain:N_remove,
                 names_to = "type", values_to = "N") %>% 
    rename("Time Step" = time_Step) %>% 
    ggplot(aes(D_class, N, fill = type)) +
    geom_col(position = position_stack(reverse = T)) +
    scale_fill_manual(name = element_blank(),
                      values = c("grey30", "red"),
                      labels = c("remain in stand", "remove"),
                      guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~`Time Step`,
               ncol = 2,
               labeller = label_both,
               drop = FALSE) +
    labs(title = "DBH-Distribution at Interventions",
         subtitle = str_c("D>=", dlimit, "cm; State 2, fall"),
         caption = capt,
         x = "Diameter class",
         y = "N") +
    scale_x_discrete(breaks = function(x) {x[c(TRUE, rep(FALSE, 2 - 1))]}) +
    c_theme +
    theme(axis.text.x = element_text(angle = 90))
  return(gg_p)
}


# mgm: maps ---------------------------------------------------------------
f.vis_mgmmps_1sim <- function(mgmres, coor, capt, p = param, c_theme = theme_custom) {
  gg_p <- mgmres %>% 
    filter(time_Step %in% p$mgm_interv_steps) %>% 
    group_by(time_Step, cc_ID) %>% 
    summarise(aux2_BA_cell = sum(aux2_BA, na.rm = TRUE),
              mgm_BA_cell  = sum(mgm_BA, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(mgm_BA_rel = mgm_BA_cell / aux2_BA_cell,
           mgm_BA_rel = replace_na(mgm_BA_rel, 0)) %>% 
    rename("Time Step" = time_Step) %>% 
    left_join(coor, by = c("cc_ID" = "cell_ID")) %>% 
    ggplot(aes(corner_x, corner_y, group = cc_ID, fill = mgm_BA_rel)) +
    geom_polygon(color = "black", size = 0.1) +
    scale_fill_gradient(name = "BA removed",
                        low = "white",
                        high = "red",
                        limits = c(0, 1),
                        labels = scales::percent) +
    facet_wrap(~`Time Step`,
               ncol = 2,
               labeller = label_both,
               drop = FALSE) +
    coord_equal() +
    labs(title = "Intervention Intensity Maps",
         caption = capt,
         x = "x (m)",
         y = "y (m)") +
    c_theme +
    theme(panel.grid = element_blank())
  return(gg_p)
}
