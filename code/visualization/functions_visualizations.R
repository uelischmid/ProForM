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
    filter(D >= dlimit) %>% 
    group_by(time_Step2, Species,
             .drop = FALSE) %>%
    summarise(ba = sum(BA) / p$sim_area_ha,
              .groups = "drop") %>%
    filter(Species != "none") %>% 
    ggplot(aes(time_Step2, ba, fill = Species)) +
    geom_area(alpha = 0.8) +
    labs(title   = str_c("Stand Basal Area (D>=", dlimit, "cm)"),
         caption = capt,
         x       = "Time Step", 
         y       = bquote("BA "(m^2/ha))) +
    c_theme
  return(gg_p)
}


f.vis_ba_1sim_rel <- function(standdev, capt, dlimit, p = param, c_theme = theme_custom) {
  gg_p <- standdev %>% 
    filter(D >= dlimit) %>% 
    group_by(time_Step2, Species,
             .drop = FALSE) %>% 
    summarise(ba_abs = sum(BA),
              .groups = "drop") %>% 
    group_by(time_Step2) %>% 
    mutate(ba_rel = ba_abs / sum(ba_abs)) %>% 
    ungroup() %>% 
    filter(Species != "none") %>% 
    ggplot(aes(time_Step2, ba_rel, fill = Species)) +
    geom_area(alpha = 0.8) +
    scale_y_continuous(labels = scales::percent) +
    labs(title   = str_c("BA Species Shares (D>=", dlimit, "cm)"),
         caption = capt,
         x       = "Time Step",
         y       = "BA share") +
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


f.vis_ba_1sim_efm_spc <- function(simstanddev, efm, myears, dlimit, capt,
                                  p = param, c_theme = theme_custom) {
  summary_sim <- simstanddev %>%
    group_by(time_Step2, Species,
             .drop = FALSE) %>%
    filter(D >= dlimit) %>% 
    summarise(ba = sum(BA) / p$sim_area_ha,
              .groups = "drop") %>%
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  summary_efm <- efm$standdev %>% 
    filter(State_nr == 2) %>% 
    group_by(time_Step2, Species,
             .drop = FALSE) %>% 
    filter(diameter >= dlimit) %>% 
    summarise(ba = sum(BA_calc_m2) / efm$meta$area,
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  
  gg_p <- ggplot(summary_sim, aes(time_Step2, ba, fill = Species)) +
    geom_area(position = "stack",
              alpha = 0.5) +
    geom_col(data = summary_efm,
             width = 1) +
    scale_fill_manual(values = c(scales::hue_pal()(4), "#808080"),
                      drop = FALSE) +
    labs(title = str_c("Stand Basal Area (D>=", dlimit, "cm)"),
         subtitle = str_c("Columns: EFM ", efm_plot, "; Area: simulation"),
         caption = capt,
         x = "Time Step", 
         y = bquote("BA "(m^2/ha))) +
    c_theme
  
  if (sum(is.na(myears)) == 0) {
    gg_p <- gg_p +
      geom_text(data = myears,
                mapping = aes(x = time_Step2,
                              y = y_val,
                              vjust = vjust_val,
                              label = label_val),
                inherit.aes = FALSE,
                size = 10) +
      labs(subtitle = str_c("Columns: EFM ", efm_plot, "; Area: simulation",
                            "\n*  years with >= 5% trees missing in EFM-data"))
  }
  return(gg_p)
}

f.vis_ba_1sim_efm_spc2 <- function(simstanddev, efm, myears, dlimit, capt,
                                   p = param, c_theme = theme_custom) {
  summary_sim <- simstanddev %>%
    group_by(time_Step2, Species,
             .drop = FALSE) %>%
    filter(D >= dlimit) %>% 
    summarise(ba = sum(BA) / p$sim_area_ha,
              .groups = "drop") %>%
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species),
           dat = "Sim.",
           comb = str_c(dat, "_", as.character(Species)))
  summary_efm <- efm$standdev %>% 
    group_by(time_Step2, Species,
             .drop = FALSE) %>% 
    filter(diameter >= dlimit) %>% 
    summarise(ba = sum(BA_calc_m2) / efm$meta$area,
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species),
           dat = "EFM",
           comb = str_c(dat, "_", as.character(Species)))
  summary_both <- bind_rows(summary_sim, summary_efm) %>% 
    mutate(dat = factor(dat, levels = c("Sim.", "EFM")))
  
  gg_p <- ggplot(summary_both, aes(time_Step2, ba, group = comb, color = Species, lty = dat)) +
    geom_point(data = filter(summary_both, dat == "EFM")) +
    geom_line() +
    scale_color_manual(values = c(scales::hue_pal()(4), "#808080"),
                       drop = FALSE) +
    scale_linetype_manual(name = "Data",
                          breaks = c("Sim.", "EFM"),
                          values = c(1, 3)) +
    labs(title = str_c("Species Basal Area (D>=", dlimit, "cm)"),
         caption = capt,
         x = "Time Step", 
         y = bquote("BA "(m^2/ha))) +
    c_theme
  
  if (sum(is.na(myears)) == 0) {
    gg_p <- gg_p +
      geom_text(data = myears,
                mapping = aes(x = time_Step2,
                              y = y_val,
                              vjust = vjust_val,
                              label = label_val),
                inherit.aes = FALSE,
                size = 10) +
      labs(subtitle = str_c("* years with >= 5% trees missing in EFM-data"))
  }
  return(gg_p)
}

f.vis_ba_1sim_efm_spc3 <- function(simstanddev, efm, myears, dlimit, capt,
                                   p = param, c_theme = theme_custom) {
  summary_sim_spc <- simstanddev %>%
    group_by(time_Step2, Species,
             .drop = FALSE) %>%
    filter(D >= dlimit) %>% 
    summarise(ba = sum(BA) / p$sim_area_ha,
              .groups = "drop") %>%
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  summary_sim_tot <- summary_sim_spc %>% 
    group_by(time_Step2,
             .drop = FALSE) %>% 
    summarise(ba = sum(ba),
              .groups = "drop") %>% 
    mutate(Species = "all")
  summary_sim <- bind_rows(summary_sim_spc, summary_sim_tot) %>% 
    mutate(Species = factor(Species, levels = c("all", "aalb", "apse", "fsyl", "pabi", "other")),
           dat = "Sim.",
           comb = str_c(dat, "_", as.character(Species)))
  
  
  summary_efm_spc <- efm$standdev %>% 
    group_by(time_Step2, Species,
             .drop = FALSE) %>% 
    filter(diameter >= dlimit) %>% 
    summarise(ba = sum(BA_calc_m2) / efm$meta$area,
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  summary_efm_tot <- summary_efm_spc %>% 
    group_by(time_Step2,
             .drop = FALSE) %>% 
    summarise(ba = sum(ba),
              .groups = "drop") %>%
    mutate(Species = "all")
  summary_efm <- bind_rows(summary_efm_spc, summary_efm_tot) %>% 
    mutate(Species = factor(Species, levels = c("all", "aalb", "apse", "fsyl", "pabi", "other")),
           dat = "EFM",
           comb = str_c(dat, "_", as.character(Species)))
  
  summary_both <- bind_rows(summary_sim, summary_efm) %>% 
    mutate(dat = factor(dat, levels = c("Sim.", "EFM")))
  
  gg_p <- ggplot(summary_both, aes(time_Step2, ba, group = comb, color = Species, lty = dat)) +
    geom_point(data = filter(summary_both, dat == "EFM")) +
    geom_line() +
    scale_color_manual(values = c("#000000", scales::hue_pal()(4), "#808080"),
                       drop = FALSE) +
    scale_linetype_manual(name = "Data",
                          breaks = c("Sim.", "EFM"),
                          values = c(1, 3)) +
    labs(title = str_c("Species Basal Area (D>=", dlimit, "cm)"),
         caption = capt,
         x = "Time Step", 
         y = bquote("BA "(m^2/ha))) +
    c_theme
  
  if (sum(is.na(myears)) == 0) {
    gg_p <- gg_p +
      geom_text(data = myears,
                mapping = aes(x = time_Step2,
                              y = y_val,
                              vjust = vjust_val,
                              label = label_val),
                inherit.aes = FALSE,
                size = 10) +
      labs(subtitle = str_c("* years with >= 5% trees missing in EFM-data"))
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
    geom_area(alpha = 0.8) +
    ylim(0, NA) +
    labs(title   = str_c("Stem Number (D>=", dlimit, "cm)"),
         caption = capt,
         x       = "Time Step", 
         y       = "N/ha") +
    c_theme
  return(gg_p)
}

f.vis_sn_1sim_rel <- function(standdev, capt, dlimit, p = param, c_theme = theme_custom) {
  gg_p <- standdev %>% 
    filter(D >= dlimit) %>% 
    group_by(time_Step2, Species,
             .drop = FALSE) %>% 
    summarise(N_abs = sum(N),
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    group_by(time_Step2) %>% 
    mutate(N_rel = N_abs / sum(N_abs)) %>% 
    ungroup() %>% 
    ggplot(aes(time_Step2, N_rel, fill = Species)) +
    geom_area(alpha = 0.8) +
    scale_y_continuous(labels = scales::percent) +
    labs(title   = str_c("SN Species Shares (D>=", dlimit, "cm)"),
         caption = capt,
         x       = "Time Step",
         y       = "SN share") +
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

f.vis_sn_1sim_efm_spc <- function(simstanddev,efm, myears, dlimit, capt,
                                  p = param, c_theme = theme_custom) {
  SN_SIM <- simstanddev %>%
    filter(D >= dlimit) %>% 
    group_by(time_Step2, Species, .drop = FALSE) %>% 
    summarise(total_Nha = sum(N) / param$sim_area_ha,
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  
  SN_EFM <- efm$standdev %>% 
    filter(State_nr == 2) %>% 
    filter(diameter >= dlimit) %>% 
    group_by(time_Step2, Species, .drop = FALSE) %>% 
    summarise(total_Nha = n() / (efm$meta$area),
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  
  
  gg_p <- ggplot(SN_SIM, aes(time_Step2, total_Nha, fill = Species)) +
    geom_area(position = "stack",
              alpha = 0.5) +
    geom_col(data = SN_EFM,
             width = 1) +
    scale_fill_manual(values = c(scales::hue_pal()(4), "#808080"),
                      drop = FALSE) +
    labs(title = str_c("Stem Number (D>=", dlimit, "cm)"),
         subtitle = str_c("Columns: EFM ", efm_plot, "; Area: simulation"),
         caption = capt,
         x = "Time Step",
         y = "N/ha") +
    theme_custom
  
  
  if (sum(is.na(myears)) == 0) {
    gg_p <- gg_p +
      geom_text(data = myears,
                mapping = aes(x = time_Step2,
                              y = y_val,
                              vjust = vjust_val,
                              label = label_val),
                inherit.aes = FALSE,
                size = 10) +
      labs(subtitle = str_c("Columns: EFM ", efm_plot, "; Area: simulation",
                            "\n*  years with >= 5% trees missing in EFM-data"))
  }
  
  return(gg_p)
}

f.vis_sn_1sim_efm_spc2 <- function(simstanddev, efm, myears, dlimit, capt,
                                   p = param, c_theme = theme_custom) {
  SN_SIM <- simstanddev %>%
    filter(D >= dlimit) %>% 
    group_by(time_Step2, Species, .drop = FALSE) %>% 
    summarise(total_Nha = sum(N) / param$sim_area_ha,
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species),
           dat = "Sim.",
           comb = str_c(dat, "_", as.character(Species)))
  
  SN_EFM <- efm$standdev %>% 
    filter(diameter >= dlimit) %>% 
    group_by(time_Step2, Species, .drop = FALSE) %>% 
    summarise(total_Nha = n() / (efm$meta$area),
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species),
           dat = "EFM",
           comb = str_c(dat, "_", as.character(Species)))
  
  SN_both <- bind_rows(SN_SIM, SN_EFM) %>% 
    mutate(dat = factor(dat, levels = c("Sim.", "EFM")))
  
  gg_p <- ggplot(SN_both, aes(time_Step2, total_Nha, group = comb, color = Species, lty = dat)) +
    geom_point(data = filter(SN_both, dat == "EFM")) +
    geom_line() +
    scale_color_manual(values = c(scales::hue_pal()(4), "#808080"),
                       drop = FALSE) +
    scale_linetype_manual(name = "Data",
                          breaks = c("Sim.", "EFM"),
                          values = c(1, 3)) +
    labs(title = str_c("Species Stem Number (D>=", dlimit, "cm)"),
         caption = capt,
         x = "Time Step", 
         y = "N/ha") +
    c_theme
  
  
  if (sum(is.na(myears)) == 0) {
    gg_p <- gg_p +
      geom_text(data = myears,
                mapping = aes(x = time_Step2,
                              y = y_val,
                              vjust = vjust_val,
                              label = label_val),
                inherit.aes = FALSE,
                size = 10) +
      labs(subtitle = str_c("* years with >= 5% trees missing in EFM-data"))
  }
  
  return(gg_p)
}


f.vis_sn_1sim_efm_spc3 <- function(simstanddev, efm, myears, dlimit, capt,
                                   p = param, c_theme = theme_custom) {
  SN_SIM_spc <- simstanddev %>%
    filter(D >= dlimit) %>% 
    group_by(time_Step2, Species, .drop = FALSE) %>% 
    summarise(total_Nha = sum(N) / param$sim_area_ha,
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  SN_SIM_tot <- SN_SIM_spc %>% 
    group_by(time_Step2,
             .drop = FALSE) %>% 
    summarise(total_Nha = sum(total_Nha),
              .groups = "drop") %>% 
    mutate(Species = "all")
  SN_SIM <- bind_rows(SN_SIM_spc, SN_SIM_tot) %>% 
    mutate(Species = factor(Species, levels = c("all", "aalb", "apse", "fsyl", "pabi", "other")),
           dat = "Sim.",
           comb = str_c(dat, "_", as.character(Species)))
  
  
  SN_EFM_spc <- efm$standdev %>% 
    filter(diameter >= dlimit) %>% 
    group_by(time_Step2, Species, .drop = FALSE) %>% 
    summarise(total_Nha = n() / (efm$meta$area),
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  SN_EFM_tot <- SN_EFM_spc %>% 
    group_by(time_Step2,
             .drop = FALSE) %>% 
    summarise(total_Nha = sum(total_Nha),
              .groups = "drop") %>% 
    mutate(Species = "all")
  SN_EFM <- bind_rows(SN_EFM_spc, SN_EFM_tot) %>% 
    mutate(Species = factor(Species, levels = c("all", "aalb", "apse", "fsyl", "pabi", "other")),
           dat = "EFM",
           comb = str_c(dat, "_", as.character(Species)))
  
  SN_both <- bind_rows(SN_SIM, SN_EFM) %>% 
    mutate(dat = factor(dat, levels = c("Sim.", "EFM")))
  
  gg_p <- ggplot(SN_both, aes(time_Step2, total_Nha, group = comb, color = Species, lty = dat)) +
    geom_point(data = filter(SN_both, dat == "EFM")) +
    geom_line() +
    scale_color_manual(values = c("#000000", scales::hue_pal()(4), "#808080"),
                       drop = FALSE) +
    scale_linetype_manual(name = "Data",
                          breaks = c("Sim.", "EFM"),
                          values = c(1, 3)) +
    labs(title = str_c("Species Stem Number (D>=", dlimit, "cm)"),
         caption = capt,
         x = "Time Step", 
         y = "N/ha") +
    c_theme
  
  
  if (sum(is.na(myears)) == 0) {
    gg_p <- gg_p +
      geom_text(data = myears,
                mapping = aes(x = time_Step2,
                              y = y_val,
                              vjust = vjust_val,
                              label = label_val),
                inherit.aes = FALSE,
                size = 10) +
      labs(subtitle = str_c("* years with >= 5% trees missing in EFM-data"))
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

f.vis_dd_1sim_efm_spc <- function(simstanddev, efm, myears, dlimit, ps, capt,
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
           `Time Step` = factor(time_Step2),
           name = fct_drop(name)) %>% 
    group_by(`Time Step`, D_class, Species, name, .drop = FALSE) %>% 
    summarise(N_sum = sum(N) / p$sim_area_ha,
              .groups = "drop")
  
  # summarise efm
  DD_EFM <- efm$standdev %>% 
    filter(State_nr == 1) %>% 
    filter(diameter >= dlimit) %>% 
    filter(time_Step2 %in% ps) %>% 
    mutate(D_class = cut_width(diameter, width = 4, boundary = 0, closed = "left"),
           `Time Step` = factor(time_Step2),
           name = "EFM",
           name = factor(name)) %>% 
    group_by(`Time Step`, D_class, Species, name, .drop = FALSE) %>% 
    summarise(N_sum = n() / (efm$meta$area),
              .groups = "drop")
  
  # combine data
  DD_full <- bind_rows(DD_SIM, DD_EFM) %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  
  gg_p <- ggplot(DD_full, aes(D_class, N_sum, group = interaction(name, Species), lty = name, color = Species)) +
    geom_line() +
    scale_color_manual(values = c(scales::hue_pal()(4), "#808080")) +
    scale_x_discrete(breaks = function(x) {x[c(TRUE, rep(FALSE, 2 - 1))]}) +
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
         color = "Species",
         lty = "Data") +
    c_theme +
    theme(axis.text.x = element_text(angle = 90))
  
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


# mgm: RDC-distributions --------------------------------------------------
f.vis_mgmrdc_1sim <- function(mgmres, mgminstr, capt, p = param, c_theme = theme_custom) {
  min_D <- unique(mgminstr$Dmin)
  no_classes <- max(mgminstr$Dclass_rel)
  
  gg_p <- mgmres %>%
    filter(time_Step %in% p$mgm_interv_steps) %>%
    filter(s2_D >= min_D) %>%
    group_by(time_Step) %>% 
    mutate(Dclass_rel = cut_interval(s2_D, n = no_classes, labels = FALSE)) %>% 
    group_by(time_Step, Dclass_rel) %>%
    summarise(N_remain = sum(remain_N),
              N_remove = sum(mgm_N),
              .groups = "drop") %>%
    pivot_longer(cols = N_remain:N_remove,
                 names_to = "type", values_to = "N") %>% 
    rename("Time Step" = time_Step) %>% 
    ggplot(aes(Dclass_rel, N, fill = type)) +
    geom_col(position = position_stack(reverse = T)) +
    scale_fill_manual(name = element_blank(),
                      values = c("grey30", "red"),
                      labels = c("remain in stand", "remove"),
                      guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~`Time Step`,
               ncol = 2,
               labeller = label_both,
               drop = FALSE) +
    labs(title = "RDC removal",
         subtitle = str_c("min. D = ", min_D, " cm; State 2, fall"), 
         caption = capt,
         x = "Relative Diameter Class",
         y = "N") +
    c_theme
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


# old graphs for mgm skyline ----------------------------------------------
# old mgm-code: see Model_2020



# nais form 2 -------------------------------------------------------------
f.vis_nais_F2 <- function(i_r, ass, tc) {
  
  i_r_long <- i_r %>% 
    pivot_longer(cols = mix:sapthi,
                 names_to = "index",
                 values_to = "value") %>% 
    select(time_Step, index, value) %>% 
    mutate(index = factor(index, levels = c("mix", "vert", "horiz", "supptr", "seedl", "sapthi")))
  
  gg_p <- ggplot(i_r_long, aes(time_Step, value)) +
    geom_line() +
    geom_point() +
    facet_wrap(~index, ncol = 1) +
    scale_x_continuous(name = "Time") +
    scale_y_continuous(name = "",
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1),
                       labels = c("very bad", "minimal", "ideal")) +
    coord_flip() +
    labs(title    = "NaiS form 2",
         subtitle = ass) +
    tc
  
  return(gg_p)
}


# nais subindices ---------------------------------------------------------
f.vis_nais_si_mix_stand <- function(st, np, tc) {
  
  v_spc <- c("aalb", "apse","pabi", "fsyl", "coni")
  plots <- vector(mode = "list", length = 6)
  legend_saved <- FALSE
  
  for (i in seq_along(v_spc)) {
    spc <- v_spc[i]
    
    pr_val <- tibble(Profile  = rep(c("IP", "MP"), each = 2),
                     Limit    = rep(c("lower", "upper"), times = 2),
                     value    = np$mixture[[spc]]) %>% 
      mutate(Profile = factor(Profile, levels = c("MP", "IP")),
             Limit   = factor(Limit, levels = c("lower", "upper")))
    
    gg_p <- ggplot(st, aes_string("time_Step", str_c("mix_", spc))) +
      geom_line() +
      scale_y_continuous(name   = "Species BA share",
                         limits = c(0, 1)) +
      scale_x_continuous(name = "Time") +
      labs(title = str_c("mix ", spc)) +
      tc
    
    if (length(np$mixture[[spc]]) > 1) {
      gg_p <- gg_p +
        geom_hline(data    = pr_val,
                   mapping = aes(yintercept = value,
                                 linetype   = Limit,
                                 color      = Profile))
      if (legend_saved == FALSE) {
        plots[[6]] <- get_legend(gg_p)
        legend_saved <- TRUE
      }
    }
    plots[[i]] <- gg_p +
      theme(legend.position = "none")
  }
  
  out <- plot_grid(plotlist = plots,
                   nrow     = 2,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_mix_ind <- function (i_f, tc) {
  
  v_ind <- c("mix_aalb", "mix_apse", "mix_pabi", "mix_fsyl","mix_coni", "mix")
  plots <- vector(mode = "list", length = 6)
  
  for (i in seq_along(v_ind)) {
    
    if (is.numeric(i_f[[v_ind[i]]]) == FALSE) next
    
    plots[[i]] <- ggplot(i_f, aes_string("time_Step", v_ind[i])) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = str_c("ind ", v_ind[i]),
           x     = "Time",
           y     = "Index value") +
      tc
  }
  
  out <- plot_grid(plotlist = plots,
                   nrow     = 2,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_vert_dclass <- function (np, st, i_f, tc) {
  
  dclass_profiles <- tibble(Class = c(1:4),
                            MP    = np$va_dc$minimal,
                            IP    = np$va_dc$ideal) %>% 
    pivot_longer(cols      = MP:IP,
                 names_to  = "Profile",
                 values_to = "value") %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  plots <- vector(mode = "list", length = 6)
  
  for (i in 1:4) {
    class <- i
    class_c <- as.character(class)
    
    dclass_profiles_temp <- filter(dclass_profiles, Class == class)
    
    gg_p <- ggplot(st, aes_string("time_Step", str_c("dclass_D", class_c))) +
      geom_line() +
      geom_hline(data    = dclass_profiles_temp,
                 mapping = aes(yintercept = value,
                               color      = Profile)) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = str_c("dclass ", class_c),
           x     = "Time",
           y     = "Dclass coh share") +
      tc
    
    if (i == 1) {
      plots[[5]] <- get_legend(gg_p)
    }
    
    plots[[i]] <- gg_p +
      theme(legend.position = "none")
  }
  
  plots[[6]] <- ggplot(i_f, aes(time_Step, dclass)) +
    geom_line() +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title = "ind dclass",
         x     = "Time",
         y     = "Index value") +
    tc
  
  out <- plot_grid(plotlist = plots,
                   byrow = FALSE)
  return(out)
}


f.vis_nais_si_vert_stand_RF <- function (np, st, tc) {
  
  pr_val <- np$RF$n_ba_limits %>%
    select(cat, MP = min, IP = ide) %>% 
    pivot_longer(cols      = MP:IP,
                 names_to  = "Profile",
                 values_to = "value") %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  plots <- vector(mode = "list", length = 6)
  
  for (i in 1:4) {
    pr_val_temp <- pr_val %>% 
      filter(cat == str_c("N", as.character(i)))
    
    plots[[i]] <- ggplot(st, aes_string("time_Step", str_c("sn_N", as.character(i)))) +
      geom_line() +
      geom_hline(data    = pr_val_temp,
                 mapping = aes(yintercept = value,
                               color      = Profile)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(title = str_c("sn_N", as.character(i)),
           x     = "Time",
           y     = str_c("N/ha class ", as.character(i))) +
      tc +
      theme(legend.position = "none")
  }
  
  pr_val_temp <- pr_val %>% 
    filter(cat == "BA")
  
  gg_p <- ggplot(st, aes(time_Step, ba_ha)) +
    geom_line() +
    geom_hline(data    = pr_val_temp,
               mapping = aes(yintercept = value,
                             color      = Profile)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "BA",
         x     = "Time",
         y     = "BA (m2/ha)") +
    tc
  
  plots[[6]] <- get_legend(gg_p)
  plots[[5]] <- gg_p +
    theme(legend.position = "none")
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_vert_ind_RF <- function (i_f, tc) {
  
  plots <- vector(mode = "list", length = 6)
  v_ind <- c("sn_N1", "sn_N2", "sn_N3", "sn_N4", "sn", "ba_ha")
  
  for (i in seq_along(v_ind)) {
    plots[[i]] <- ggplot(i_f, aes_string("time_Step", v_ind[i])) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = str_c("ind ", v_ind[i]),
           x     = "Time", 
           y     = "Index value") +
      tc
  }
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_vert_ind <- function (i_f, tc) {
  
  plots <- vector(mode = "list", length = 4)
  v_ind <- c("dclass", "sn", "ba_ha", "vert")
  
  for (i in seq_along(v_ind)) {
    
    if (is.numeric(i_f[[v_ind[i]]]) == FALSE) next
    
    plots[[i]] <- ggplot(i_f, aes_string("time_Step", v_ind[i])) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = str_c("ind ", v_ind[i]),
           x     = "Time", 
           y     = "Index value") +
      tc
  }
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_horiz_stand_A <- function (st, np, tc) {
  
  plots <- vector(mode = "list", length = 3)
  
  plots[[1]] <- ggplot(st, aes(time_Step, n_gaps_not_ideal)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "gaps not ideal",
         x     = "Time",
         y     = "# gaps not ideal") +
    tc
  
  plots[[2]] <- ggplot(st, aes(time_Step, n_gaps_not_minimal)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "gaps not minimal",
         x     = "Time",
         y     = "# gaps not minimal") +
    tc
  
  
  cc_profile_a <- tibble(Profile = c("IP", "MP"),
                         Value   = np$A$cc) %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  
  plots[[3]] <- ggplot(st, aes(time_Step, cc)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    geom_hline(data    = cc_profile_a,
               mapping = aes(yintercept = Value,
                             color      = Profile)) +
    labs(title = "stand cc",
         x     = "Time",
         y     = "Canopy cover") +
    tc
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_horiz_stand_LED <- function (st, np, tc) {
  
  plots <- vector(mode = "list", length = 3)
  
  plots[[1]] <- ggplot(st, aes(time_Step, n_gaps_not_ideal)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "gaps not ideal",
         x     = "Time",
         y     = "# gaps not ideal") +
    tc
  
  plots[[2]] <- ggplot(st, aes(time_Step, n_gaps_not_minimal)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "gaps not minimal",
         x     = "Time",
         y     = "# gaps not minimal") +
    tc
  
  cc_profile_led <- tibble(Profile = c("IP", "MP"),
                           Value   = np$LED$cc) %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  plots[[3]] <- ggplot(st, aes(time_Step, cc)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    geom_hline(data    = cc_profile_led,
               mapping = aes(yintercept = Value,
                             color      = Profile)) +
    labs(title = "stand cc",
         x     = "Time",
         y     = "Canopy cover") +
    tc
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_horiz_stand_RF <- function (st, np, tc) {
  
  out <- ggplot(st, aes(time_Step, gap_length_ext_max)) +
    geom_line() +
    geom_hline(yintercept = np$RF$gaplength_ext,
               color      = "red") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Max. gap length",
         x     = "Time",
         y     = "Max. gap length (m)") +
    tc
  
  return(out)
}


f.vis_nais_si_horiz_stand_TF <- function (np, st, tc) {
  
  stand_attr <- c("gap_length_max", "gap_area_max", "cc")
  
  pr_val <- tibble(cat     = rep(c("gap_length_max", "gap_area_max", "cc"), each = 2),
                   Profile = rep(c("IP", "MP"), times = 3),
                   value   = c(np$TF$gaplength,
                               np$TF$gaparea,
                               np$TF$cc)) %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  plots <- vector(mode = "list", length = 4)
  
  for (i in seq_along(stand_attr)) {
    pr_val_temp <- pr_val %>% 
      filter(cat == stand_attr[i])
    
    gg_p <- ggplot(st, aes_string("time_Step", stand_attr[i])) +
      geom_line() +
      geom_hline(data    = pr_val_temp,
                 mapping = aes(yintercept = value,
                               color      = Profile)) +
      tc
    
    if (i == 1) {
      gg_p <- gg_p +
        scale_y_continuous(limits = c(0, NA)) +
        labs(title = "Max. gap length",
             x     = "Time",
             y     = "Max. gap length (m)")
      plots[[4]] <- get_legend(gg_p)
      plots[[i]] <- gg_p +
        theme(legend.position = "none")
    } else if (i == 2) {
      plots[[i]] <- gg_p +
        scale_y_continuous(limits = c(0, NA)) +
        labs(title = "Max. gap area",
             x     = "Time",
             y     = "Max. gap area (m2)") +
        theme(legend.position = "none")
    } else if (i == 3) {
      plots[[i]] <- gg_p +
        scale_y_continuous(limits = c(0, 1)) +
        labs(title = "CC",
             x     = "Time",
             y     = "Canopy Cover") +
        theme(legend.position = "none")
    }
  }
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_horiz_ind <- function(i_f, tc) {
  
  v_ind <- c("gap", "gap_l", "gap_a", "cc", "horiz")
  plots <- vector(mode = "list", length = 5)
  
  for (i in seq_along(v_ind)) {
    if (is.numeric(i_f[[v_ind[i]]]) == FALSE) next
    
    plots[[i]] <- ggplot(i_f, aes_string("time_Step", v_ind[i])) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = str_c("ind ", v_ind[i]),
           x     = "Time",
           y     = "Index value") +
      tc
  }
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_supptr <- function (np, st, tc, i_f) {
  
  plots <- vector(mode = "list", length = 5)
  stand_attr <- c("aalb", "pabi", "coni")
  
  pr_val <- tibble(cat     = rep(c("aalb", "pabi", "coni"), each = 2),
                   Profile = rep(c("IP", "MP"), times = 3),
                   value   = as.numeric(c(np$suptr_cr$aalb,
                                          np$suptr_cr$pabi,
                                          np$suptr_cr$coni))) %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  for (i in seq_along(stand_attr)) {
    pr_val_temp <- pr_val %>% 
      filter(cat == stand_attr[i])
    
    gg_p <- ggplot(st, aes_string("time_Step", str_c("cr_", stand_attr[i]))) +
      geom_line() +
      geom_hline(data    = pr_val_temp,
                 mapping = aes(yintercept = value,
                               color      = Profile)) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = str_c("CR ", stand_attr[i]),
           x     = "Time",
           y     = str_c("CR ", stand_attr[i])) +
      tc
    
    if (i == 1) {
      plots[[4]] <- get_legend(gg_p)
    }
    
    plots[[i]] <- gg_p +
      theme(legend.position = "none")
  }
  
  if (is.numeric(i_f$supptr)) {
    plots[[5]] <- ggplot(i_f, aes(time_Step, supptr)) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = "ind supptr",
           x     = "Time",
           y     = "Index value") +
      tc
  }
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_seedl <- function (np, st, tc, i_f) {
  
  plots <- vector(mode = "list", length = 6)
  
  # stand seedling share
  pr_val <- tibble(Profile = c("IP", "MP"),
                   value   = np$seedl$share) %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  if(is.numeric(st$seedl_gap_sha)) {
    plots[[1]] <- ggplot(st, aes(time_Step, seedl_gap_sha)) +
      geom_line() +
      geom_hline(data    = pr_val,
                 mapping = aes(yintercept = value,
                               color      = Profile)) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = "Gap cells w/ seedl.",
           x     = "Time",
           y     = "Gap cell share w/ seedl.") +
      tc +
      theme(legend.position = "none")
  }
  
  # stand species count
  pr_val <- tibble(Profile = c("IP", "MP"),
                   value   = np$seedl$species_n) %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  stand_spc_count <- st %>% 
    select(time_Step, seedl_gap_spc) %>% 
    mutate(spc_l     = map(seedl_gap_spc, str_split, pattern = ","),
           spc_char  = map(spc_l, 1),
           spc_count = map_dbl(spc_char, ~ sum(. %in% np$seedl$species)))
  
  gg_p <- ggplot(stand_spc_count, aes(time_Step, spc_count)) +
    geom_line() +
    geom_hline(data    = pr_val,
               mapping = aes(yintercept = value,
                             color      = Profile)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Species in gaps",
         x     = "Time",
         y     = "# Species in gaps") +
    tc
  
  plots[[5]] <- get_legend(gg_p)
  plots[[2]] <- gg_p +
    theme(legend.position = "none")
  
  
  # ind seedling share
  if (is.numeric(i_f$seedl_sha)) {
    plots[[3]] <- ggplot(i_f, aes(time_Step, seedl_sha)) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = "ind seedl share",
           x     = "Time",
           y     = "Index value") +
      tc
  }
  
  
  # ind species count
  plots[[4]] <- ggplot(i_f, aes(time_Step, seedl_spc)) +
    geom_line() +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title = "ind seedl spc count",
         x     = "Time",
         y     = "Index value") +
    tc
  
  # ind seedl
  plots[[6]] <- ggplot(i_f, aes(time_Step, seedl)) +
    geom_line() +
    scale_y_continuous(limits = c(-1, 1))+ 
    labs(title = "ind seedl",
         x     = "Time",
         y     = "Index value") +
    tc
  
  out <- plot_grid(plotlist = plots,
                   nrow     = 2,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_sapthi_stand <- function (np, st, tc) {
  
  plots <- vector(mode = "list", length = 9)
  spc <- c("aalb", "pabi", "coni", "apse", "fsyl")
  legend_saved <- FALSE
  
  for (i in seq_along(spc)) {
    pr_val <- tibble(Profile  = rep(c("IP", "MP"), each = 2),
                     Limit    = rep(c("lower", "upper"), times = 2),
                     value    = np$sapthi[[spc[i]]]) %>% 
      mutate(Profile = factor(Profile, levels = c("MP", "IP")),
             Limit   = factor(Limit, levels = c("lower", "upper")))
    
    gg_p <- ggplot(st, aes_string("time_Step", str_c("st_", spc[i]))) +
      geom_line() +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = str_c("st ", spc[i]),
           x     = "Time",
           y     = "Share") +
      tc
    
    if (length(np$sapthi[[spc[i]]]) > 1) {
      gg_p <- gg_p +
        geom_hline(data    = pr_val,
                   mapping = aes(yintercept = value,
                                 linetype   = Limit,
                                 color      = Profile))
      if (legend_saved == FALSE) {
        plots[[7]] <- get_legend(gg_p +
                                   guides(linetype = FALSE))
        plots[[8]] <- get_legend(gg_p + 
                                   guides(color = FALSE))
        legend_saved <- TRUE
      }
    }
    plots[[i]] <- gg_p +
      theme(legend.position = "none")
  }
  
  pr_val <- tibble(Profile = c("IP", "MP"),
                   value   = np$sapthi$tot) %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  plots[[9]] <- ggplot(st, aes(time_Step, st_tot)) +
    geom_line() +
    geom_hline(data    = pr_val,
               mapping = aes(yintercept = value,
                             color      = Profile)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "st tot",
         x     = "Time",
         y     = "Share") +
    tc +
    theme(legend.position = "none")
  
  out <- plot_grid(plotlist = plots,
                   byrow = FALSE)
  
  return(out)
}


f.vis_nais_si_sapthi_ind <- function (i_f, tc) {
  
  plots <- vector(mode = "list", length = 9)
  
  spc <- c("aalb", "pabi", "coni", "apse", "fsyl")
  
  for (i in seq_along(spc)) {
    
    if (is.numeric(i_f[[str_c("st_mix_", spc[i])]]) == FALSE) next
    
    plots[[i]] <- ggplot(i_f, aes_string("time_Step", str_c("st_mix_", spc[i]))) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = str_c("ind mix ", spc[i]),
           x     = "Time",
           y     = "Index value") +
      tc
  }
  
  v_ind <- c("st_mix", "st_cc", "sapthi")
  
  for (i in seq_along(v_ind)) {
    plots[[6 + i]] <- ggplot(i_f, aes_string("time_Step", v_ind[i])) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = str_c("ind ",v_ind[i]),
           x     = "Time",
           y     = "Index value") +
      tc
  }
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}

