# visualization of c3 analysis % simulation
# functions
# 6.2.23, us


# c3 analysis -------------------------------------------------------------
prep_data <- function (orig) {
  res_mgm <- orig %>% 
    filter(mgm != "NOM")
  
  res_nom <- orig %>% 
    filter(mgm == "NOM")
  
  res_nom_mgm <- vector(mode = "list", length = nrow(res_nom))
  mgms <- unique(res_mgm$mgm)
  for (i in seq_along(res_nom_mgm)) {
    res_nom_mgm[[i]] <- res_nom[i, ] %>% 
      slice(rep(1, 6)) %>% 
      mutate(mgm = mgms)
  }
  res_nom_mgm <- bind_rows(res_nom_mgm)
  
  res <- bind_rows(res_mgm, res_nom_mgm) %>% 
    mutate(mgm     = factor(mgm, levels = c("STS", "GRS1", "GRS2", "CAB1", "CAB2", "SC")),
           quality = factor(quality, levels = c("33", "55", "31", "51")))
  
  return(res)
}

plot_mgm_4cs <- function(stratum_selected,
                         init_selected,
                         nathaz_selected,
                         quality_selected,
                         response_var,
                         dat) {
  
  # reduce data
  dat_red <- dat %>% 
    filter(stratum == stratum_selected) %>% 
    filter(init == init_selected) %>% 
    filter(nat_haz == nathaz_selected) %>% 
    filter(quality == quality_selected)
  
  # calculate midpoint
  mp <- dat_red %>% 
    filter(mgm_interval == 0) %>% 
    pull(response_var) %>% 
    unique()
  
  # plot
  gg <- ggplot(dat_red, aes_string(x     = "mgm_interval",
                                   y     = "mgm_intensity",
                                   color = response_var)) +
    geom_point(size = 3) +
    scale_color_gradient2(low      = "red",
                          mid      = "yellow",
                          high     = "green",
                          midpoint = mp) +
    facet_wrap(~ mgm) +
    labs(title = str_c("Q", quality_selected),
         x     = "Interval (y)",
         y     = "Intensity (%)",
         color = NULL) +
    coord_equal() +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  return(gg)
}

plot_mgm_4cs_all <- function(stratum_sel,
                             init_sel,
                             nathaz_sel,
                             resp_var_sel,
                             res) {
  
  # response variable in title
  if (resp_var_sel == "sha_y_MP_met") {
    resp_var_title <- "time MP met"
  } else if (resp_var_sel == "sha_y_IP_met") {
    resp_var_title <- "time IP met"
  } else if (resp_var_sel == "sha_i_MP_met") {
    resp_var_title <- "indices meeting MP"
  } else if (resp_var_sel == "sha_i_IP_met") {
    resp_var_title <- "indices meeting IP"
  } else if (resp_var_sel == "mean_i_total") {
    resp_var_title <- "mean index value"
  } else if (resp_var_sel == "neg_dist_MP") {
    resp_var_title <- "neg. distance to MP"
  } else if (resp_var_sel == "neg_dist_IP") {
    resp_var_title <- "neg. distance to IP"
  }
  
  # plot title
  if (init_sel == "LT") {
    title_str <- str_c(stratum_sel, " ",
                       nathaz_sel, " - ",
                       resp_var_title)
  } else {
    title_str <- str_c(stratum_sel,
                       " init ", init_sel, " ",
                       nathaz_sel, " - ",
                       resp_var_title)
  }
  
  # plot
  plots <- vector(mode = "list", length = 4)
  for (i in seq_along(levels(res$quality))) {
    plots[[i]] <- plot_mgm_4cs(stratum_selected = stratum_sel,
                               init_selected    = init_sel,
                               nathaz_selected  = nathaz_sel,
                               quality_selected = levels(res$quality)[i],
                               response_var     = resp_var_sel,
                               dat              = res)
  }
  
  plots_mgm <- plot_grid(plotlist = plots,
                         align    = "v")
  
  plot_title <- ggplot() +
    annotate(geom     = "text",
             x        = 0,
             y        = 0,
             label    = title_str,
             angle    = 0,
             fontface = "bold") +
    theme_void()
  
  plots_tot <- plot_grid(plot_title, plots_mgm,
                         nrow        = 2,
                         rel_heights = c(0.1, 1))
  
  return(plots_tot)
}

plot_mgm_1cs <- function(stratum_selected,
                         init_selected,
                         nathaz_selected,
                         quality_selected,
                         response_var,
                         mp,
                         lims,
                         dat) {
  
  # reduce data
  dat_red <- dat %>% 
    filter(stratum == stratum_selected) %>% 
    filter(init == init_selected) %>% 
    filter(nat_haz == nathaz_selected) %>% 
    filter(quality == quality_selected)
  
  
  # plot
  gg <- ggplot(dat_red, aes_string(x     = "mgm_interval",
                                   y     = "mgm_intensity",
                                   color = response_var)) +
    geom_point(size = 3) +
    scale_color_gradient2(low      = "red",
                          mid      = "yellow",
                          high     = "green",
                          midpoint = mp,
                          limits   = lims) +
    facet_wrap(~ mgm) +
    labs(title = str_c("Q", quality_selected),
         x     = "Interval (y)",
         y     = "Intensity (%)",
         color = NULL) +
    coord_equal() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "none")
  
  return(gg)
}

plot_mgm_1cs_all <- function(stratum_sel,
                             init_sel,
                             nathaz_sel,
                             resp_var_sel,
                             res) {
  
  # response variable in title
  if (resp_var_sel == "sha_y_MP_met") {
    resp_var_title <- "time MP met"
  } else if (resp_var_sel == "sha_y_IP_met") {
    resp_var_title <- "time IP met"
  } else if (resp_var_sel == "sha_i_MP_met") {
    resp_var_title <- "indices meeting MP"
  } else if (resp_var_sel == "sha_i_IP_met") {
    resp_var_title <- "indices meeting IP"
  } else if (resp_var_sel == "mean_i_total") {
    resp_var_title <- "mean index value"
  } else if (resp_var_sel == "neg_dist_MP") {
    resp_var_title <- "neg. distance to MP"
  } else if (resp_var_sel == "neg_dist_IP") {
    resp_var_title <- "neg. distance to IP"
  }
  
  # plot title
  if (init_sel == "LT") {
    title_str <- str_c(stratum_sel, " ",
                       nathaz_sel, " - ",
                       resp_var_title)
  } else {
    title_str <- str_c(stratum_sel,
                       " init ", init_sel, " ",
                       nathaz_sel, " - ",
                       resp_var_title)
  }
  
  # scale limits
  scale_limits <- res %>% 
    filter(stratum == stratum_sel) %>% 
    filter(init == init_sel) %>% 
    filter(nat_haz == nathaz_sel) %>% 
    pull(resp_var_sel) %>% 
    range()
  
  # scale midpoint
  midpoint <- res %>% 
    filter(stratum == stratum_sel) %>% 
    filter(init == init_sel) %>% 
    filter(nat_haz == nathaz_sel) %>% 
    pull(resp_var_sel) %>% 
    mean()
  
  # plot
  plots <- vector(mode = "list", length = 4)
  for (i in seq_along(levels(res$quality))) {
    plots[[i]] <- plot_mgm_1cs(stratum_selected = stratum_sel,
                               init_selected    = init_sel,
                               nathaz_selected  = nathaz_sel,
                               quality_selected = levels(res$quality)[i],
                               response_var     = resp_var_sel,
                               mp               = midpoint,
                               lims             = scale_limits,
                               dat              = res)
  }
  
  plots_mgm <- plot_grid(plotlist = plots)
  
  plot_scale <- get_legend(tibble(x = 1,
                                  y = 1,
                                  val = 2) %>% 
                             ggplot(aes(x, y, color = val)) +
                             geom_point() +
                             scale_color_gradient2(low      = "red",
                                                   mid      = "yellow",
                                                   high     = "green",
                                                   midpoint = midpoint,
                                                   limits   = scale_limits,
                                                   name     = NULL))
  plots_mgm2 <- plot_grid(plots_mgm, plot_scale,
                          nrow = 1,
                          rel_widths = c(1, 0.2))
  
  plot_title <- ggplot() +
    annotate(geom     = "text",
             x        = 0,
             y        = 0,
             label    = title_str,
             angle    = 0,
             fontface = "bold") +
    theme_void()
  
  plots_tot <- plot_grid(plot_title, plots_mgm2,
                         nrow = 2,
                         rel_heights = c(0.1, 1))
  
  return(plots_tot)
}




# c3 simulation vis -------------------------------------------------------
theme_custom <- theme_bw() +
  theme(panel.grid.minor = element_blank())


f.vis_ba_2sim_c3 <- function(simstanddev, dlimit, capt,
                             p = param, c_theme = theme_custom) {
  
  summary_sim <- simstanddev %>%
    group_by(name, time_Step, Species,
             .drop = FALSE) %>%
    filter(D >= dlimit) %>% 
    summarise(ba = sum(BA) / p$sim_area_ha,
              .groups = "drop") %>%
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  
  temp_summary_sim_tot <- summary_sim %>% 
    group_by(name, time_Step,
             .drop = FALSE) %>% 
    summarise(ba = sum(ba),
              .groups = "drop") %>% 
    mutate(Species = "total") %>% 
    select(name, time_Step, Species, ba)
  
  summary_sim_tot <- bind_rows(summary_sim, temp_summary_sim_tot) %>% 
    mutate(Species = factor(Species, levels = c("total", levels(summary_sim$Species))))
 
  
  gg_p <- ggplot(summary_sim_tot, aes(time_Step, ba, color = Species, lty = name)) +
    geom_line() +
    scale_color_manual(values = c("#000000", scales::hue_pal()(4)),
                       drop = FALSE) +
    labs(title    = "Stand Basal Area",
         subtitle = str_c("D >= ", dlimit, " cm"),
         # caption  = capt,
         lty      = "Simulation",
         x        = "Time Step", 
         y        = bquote("BA "(m^2/ha))) +
    c_theme
 
  return(gg_p)  
}


f.vis_sn_2sim_c3 <- function(simstanddev, dlimit, capt,
                             p = param, c_theme = theme_custom) {
  
  summary_sim <- simstanddev %>%
    group_by(name, time_Step, Species,
             .drop = FALSE) %>%
    filter(D >= dlimit) %>% 
    summarise(sn = sum(N) / p$sim_area_ha,
              .groups = "drop") %>%
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  
  temp_summary_sim_tot <- summary_sim %>% 
    group_by(name, time_Step,
             .drop = FALSE) %>% 
    summarise(sn = sum(sn),
              .groups = "drop") %>% 
    mutate(Species = "total") %>% 
    select(name, time_Step, Species, sn)
  
  summary_sim_tot <- bind_rows(summary_sim, temp_summary_sim_tot) %>% 
    mutate(Species = factor(Species, levels = c("total", levels(summary_sim$Species))))
  
  
  gg_p <- ggplot(summary_sim_tot, aes(time_Step, sn, color = Species, lty = name)) +
    geom_line() +
    scale_color_manual(values = c("#000000", scales::hue_pal()(4)),
                       drop = FALSE) +
    labs(title    = "Stem Number",
         subtitle = str_c("D >= ", dlimit, " cm"),
         # caption  = capt,
         lty      = "Simulation",
         x        = "Time Step", 
         y        = "N/ha") +
    c_theme +
    theme(legend.position = "none")
  
  return(gg_p)  
}


f.vis_cc_2sim_c3 <- function(standdev_lc, capt,
                             p = param, c_theme = theme_custom) {
  cc <- standdev_lc %>% 
    filter(Stage >= 4) %>% 
    group_by(name, time_Step) %>% 
    summarise(cc_perc = n() / p$sim_ncells,
              .groups = "drop")
  
  gg_p <- ggplot(cc, aes(time_Step, cc_perc, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::percent) +
    labs(title    = "Canopy Cover",
         subtitle = "D >= 12 cm, cohort with largest DBH",
         # caption  = capt,
         x        = "Time Step", 
         y        = "Canopy Cover",
         lty      = "Simulation") +
    c_theme +
    theme(legend.position = "none")
  
  return(gg_p)
}


f.vis_crdom_2sim_c3 <- function(simstanddev, capt,
                                p = param, c_theme = theme_custom) {
  
  # extract CR-averages from sim-data
  sim_dom100_N <- round(p$sim_area_ha * 100, 0)
  
  avg_CR_dom100 <- simstanddev %>% 
    select(name, time_Step, Species, N, D, CR) %>% 
    uncount(N) %>% 
    group_by(name, time_Step) %>% 
    slice_max(order_by  = D,
              n         = sim_dom100_N,
              with_ties = FALSE) %>% 
    group_by(name, Species,
             .add = TRUE, .drop = FALSE) %>% 
    summarise(average_CR = mean(CR, na.rm = TRUE),
              .groups = "drop") %>% 
    filter(Species != "none") %>% 
    mutate(Species = fct_drop(Species))
  
  
  gg_p <- ggplot(avg_CR_dom100, aes(time_Step, average_CR, lty = name, color = Species)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::percent) +
    labs(title    = "Average Crown Ration",
         subtitle = "100 trees with largest DBH per ha",
         # caption  = capt,
         x        = "Time Step", 
         y        = "Crown Ratio",
         lty      = "Simulation") +
    c_theme +
    theme(legend.position = "none")
  
  return(gg_p)
}


f.vis_dd_2sim_c3 <- function(simstanddev, dlimit, ps, capt,
                             p = param, c_theme = theme_custom) {
  
  DD_sim <- simstanddev %>% 
    filter(D >= dlimit) %>% 
    filter(time_Step %in% ps) %>% 
    mutate(D_class     = cut_width(D, width = 4, boundary = 0, closed = "left"),
           `Time Step` = factor(time_Step)) %>% 
    group_by(`Time Step`, D_class, name, .drop = FALSE) %>% 
    summarise(N_sum   = sum(N) / p$sim_area_ha,
              .groups = "drop")
    
    
  gg_p <- ggplot(DD_sim, aes(D_class, N_sum, group = name, lty = name)) +
    geom_line() +
    scale_x_discrete(breaks = function(x) {x[c(TRUE, rep(FALSE, 2 - 1))]},
                     drop = FALSE) +
    facet_wrap(~`Time Step`,
               ncol = 2,
               labeller = label_both,
               dir = "v",
               drop = FALSE,
               scales = "free_y") +
    labs(title    = "DBH-Distribution",
         subtitle = str_c("D >= ", dlimit, " cm"),
         caption  = capt,
         x        = "Diameter class",
         y        = "N/ha",
         lty      = "Simulation") +
    c_theme +
    theme(axis.text.x     = element_text(angle = 90),
          legend.position = "none")
  
  return(gg_p)
}


# c3 nais subindices ------------------------------------------------------
threshold_colors2 <- c("MP" = "#D40C12", "IP"= "#0F26E4")
threshold_colors4 <- c("MP lower" = "#D40C12",
                       "MP upper"= "#E49194",
                       "IP lower"= "#0F26E4",
                       "IP upper"= "#8A94E8")



f.vis_F2_2ass_c3 <- function(i_r, capt, c_theme = theme_custom) {
  
  gg_p <- i_r %>% 
    mutate(name = factor(name, levels = c("NOM", "mgm"))) %>% 
    pivot_longer(cols      = mix:sapthi,
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
    scale_color_discrete(name = "Simulation") +
    coord_flip() +
    labs(title    = "NaiS form 2",
         caption  = capt) +
    c_theme
  
  return(gg_p)
}


f.vis_nais_si_mix_stand_2ass_c3 <- function(st, np,
                                            c_theme    = theme_custom,
                                            cscale_thr = threshold_colors4) {
  
  v_spc <- c("aalb", "apse","pabi", "fsyl", "coni")
  plots <- vector(mode = "list", length = 6)
  legend_saved <- FALSE
  
  for (i in seq_along(v_spc)) {
    spc <- v_spc[i]
    
    pr_val <- tibble(Profile  = rep(c("IP", "MP"), each = 2),
                     Limit    = rep(c("lower", "upper"), times = 2),
                     value    = np$mixture[[spc]]) %>% 
      mutate(Threshold = str_c(Profile, " ", Limit),
             Threshold = factor(Threshold, levels = c("MP lower", "MP upper", "IP lower", "IP upper")))
    
    gg_p <- ggplot(st, aes_string("time_Step", str_c("mix_", spc), lty = "name")) +
      geom_line() +
      scale_y_continuous(name   = "Species BA share",
                         limits = c(0, 1)) +
      scale_x_continuous(name = "Time") +
      labs(title = str_c("mix ", spc),
           lty   = "Simulation") +
      c_theme
    
    if (length(np$mixture[[spc]]) > 1) {
      gg_p <- gg_p +
        geom_hline(data    = pr_val,
                   mapping = aes(yintercept = value,
                                 # linetype   = Limit,
                                 color      = Threshold)) +
        scale_color_manual(values = cscale_thr)
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


f.vis_nais_si_mix_ind_2ass_c3 <- function (i_f, c_theme = theme_custom) {
  
  v_ind <- c("mix_aalb", "mix_apse", "mix_pabi", "mix_fsyl","mix_coni", "mix")
  plots <- vector(mode = "list", length = 8)
  
  legend_saved <- FALSE
  
  for (i in seq_along(v_ind)) {
    
    if (is.numeric(i_f[[v_ind[i]]]) == FALSE) next
    
    gg_p <- ggplot(i_f, aes_string("time_Step", v_ind[i], lty = "name")) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = str_c("ind ", v_ind[i]),
           lty   = "Simulation",
           x     = "Time",
           y     = "Index value") +
      c_theme
    
    if (legend_saved == FALSE) {
      plots[[8]] <- get_legend(gg_p)
      legend_saved <- TRUE
    }
    
    plots[[i]] <- gg_p +
      theme(legend.position = "none")
  }
  
  out <- plot_grid(plotlist = plots,
                   nrow     = 2,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_vert_dclass_2ass_c3 <- function (np, st, i_f,
                                               c_theme   = theme_custom,
                                               cscale_thr = threshold_colors2) {
  
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
    
    gg_p <- ggplot(st, aes_string("time_Step", str_c("dclass_D", class_c), lty = "name")) +
      geom_line() +
      geom_hline(data    = dclass_profiles_temp,
                 mapping = aes(yintercept = value,
                               color      = Profile)) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_color_manual(values = cscale_thr) +
      labs(title = str_c("dclass ", class_c),
           x     = "Time",
           y     = "Dclass coh share",
           color = "Threshold",
           lty   = "Simulation") +
      c_theme
    
    if (i == 1) {
      plots[[5]] <- get_legend(gg_p)
    }
    
    plots[[i]] <- gg_p +
      theme(legend.position = "none")
  }
  
  plots[[6]] <- ggplot(i_f, aes(time_Step, dclass, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title = "ind dclass",
         x     = "Time",
         y     = "Index value",
         lty   = "Simulation") +
    c_theme +
    theme(legend.position = "none")
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_vert_ind_2ass_c3 <- function (i_f, c_theme = theme_custom) {
  
  plots <- vector(mode = "list", length = 4)

  gg_p <- ggplot(i_f, aes(time_Step, dclass, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title = "ind dclass",
         x     = "Time", 
         y     = "Index value",
         lty   = "Simulation") +
    c_theme
  
  plots[[1]] <- gg_p +
    theme(legend.position = "none")
  
  plots[[3]] <- get_legend(gg_p)
  
  plots[[4]] <- ggplot(i_f, aes(time_Step, vert, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title = "ind vert",
         x     = "Time", 
         y     = "Index value",
         lty   = "Simulation") +
    c_theme +
    theme(legend.position = "none")
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_horiz_stand_A_2ass_c3 <- function (st, np,
                                                 c_theme    = theme_custom,
                                                 cscale_thr = threshold_colors2) {
  
  plots <- vector(mode = "list", length = 4)
  
  plots[[1]] <- ggplot(st, aes(time_Step, n_gaps_not_ideal, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "gaps not ideal",
         x     = "Time",
         y     = "# gaps not ideal") +
    c_theme +
    theme(legend.position = "none")
  
  plots[[2]] <- ggplot(st, aes(time_Step, n_gaps_not_minimal, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "gaps not minimal",
         x     = "Time",
         y     = "# gaps not minimal") +
    c_theme +
    theme(legend.position = "none")
  
  
  cc_profile_a <- tibble(Profile = c("IP", "MP"),
                         Value   = np$A$cc) %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  
  gg_p <- ggplot(st, aes(time_Step, cc, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    geom_hline(data    = cc_profile_a,
               mapping = aes(yintercept = Value,
                             color      = Profile)) +
    scale_color_manual(values = cscale_thr) +
    labs(title = "stand cc",
         x     = "Time",
         y     = "Canopy cover",
         lty   = "Simulation",
         color = "Threshold") +
    c_theme
  
  plots[[3]] <- gg_p +
    theme(legend.position = "none")
  
  plots[[4]] <- get_legend(gg_p)
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_horiz_stand_LED_2ass_c3 <- function (st, np,
                                                   c_theme    = theme_custom,
                                                   cscale_thr = threshold_colors2) {
  
  plots <- vector(mode = "list", length = 4)
  
  plots[[1]] <- ggplot(st, aes(time_Step, n_gaps_not_ideal, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "gaps not ideal",
         x     = "Time",
         y     = "# gaps not ideal") +
    c_theme +
    theme(legend.position = "none")
  
  plots[[2]] <- ggplot(st, aes(time_Step, n_gaps_not_minimal, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "gaps not minimal",
         x     = "Time",
         y     = "# gaps not minimal") +
    c_theme +
    theme(legend.position = "none")
  
  cc_profile_led <- tibble(Profile = c("IP", "MP"),
                           Value   = np$LED$cc) %>% 
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))
  
  gg_p <- ggplot(st, aes(time_Step, cc, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    geom_hline(data    = cc_profile_led,
               mapping = aes(yintercept = Value,
                             color      = Profile)) +
    scale_color_manual(values = cscale_thr) +
    labs(title = "stand cc",
         x     = "Time",
         y     = "Canopy cover",
         lty   = "Simulation",
         color = "Threshold") +
    c_theme
  
  plots[[3]] <- gg_p +
    theme(legend.position = "none")
  
  plots[[4]] <- get_legend(gg_p)
  
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}



f.vis_nais_si_horiz_ind_2ass_c3 <- function(i_f, c_theme = theme_custom) {
  
  v_ind <- c("gap", "gap_l", "gap_a", "cc", "horiz")
  plots <- vector(mode = "list", length = 6)
  legend_saved <- FALSE
  
  for (i in seq_along(v_ind)) {
    if (is.numeric(i_f[[v_ind[i]]]) == FALSE) next
    
    gg_p <- ggplot(i_f, aes_string("time_Step", v_ind[i], lty = "name")) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = str_c("ind ", v_ind[i]),
           x     = "Time",
           y     = "Index value",
           lty   = "Simulation") +
      c_theme
    
    if (legend_saved == FALSE) {
      plots[[6]] <- get_legend(gg_p)
      legend_saved <- TRUE
    }
    
    plots[[i]] <- gg_p +
      theme(legend.position = "none")
  }
  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_supptr_2ass_c3 <- function (np, st, i_f,
                                          c_theme    = theme_custom,
                                          cscale_thr = threshold_colors2) {

  plots <- vector(mode = "list", length = 6)
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

    gg_p <- ggplot(st, aes_string("time_Step", str_c("cr_", stand_attr[i]), lty = "name")) +
      geom_line() +
      geom_hline(data    = pr_val_temp,
                 mapping = aes(yintercept = value,
                               color      = Profile)) +
      scale_color_manual(values = cscale_thr) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = str_c("CR ", stand_attr[i]),
           x     = "Time",
           y     = str_c("CR ", stand_attr[i]),
           lty   = "Simulation",
           color = "Threshold") +
      c_theme

    if (i == 1) {
      plots[[6]] <- get_legend(gg_p)
    }

    plots[[i]] <- gg_p +
      theme(legend.position = "none")
  }

  if (is.numeric(i_f$supptr)) {
    plots[[5]] <- ggplot(i_f, aes(time_Step, supptr, lty = name)) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = "ind supptr",
           x     = "Time",
           y     = "Index value") +
      c_theme +
      theme(legend.position = "none")
  }

  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_seedl_2ass_c3 <- function (np, st, i_f,
                                         c_theme    = theme_custom,
                                         cscale_thr = threshold_colors2) {

  plots <- vector(mode = "list", length = 6)

  # stand seedling share
  pr_val <- tibble(Profile = c("IP", "MP"),
                   value   = np$seedl$share) %>%
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))

  if(is.numeric(st$seedl_gap_sha)) {
    plots[[1]] <- ggplot(st, aes(time_Step, seedl_gap_sha, lty = name)) +
      geom_line() +
      geom_hline(data    = pr_val,
                 mapping = aes(yintercept = value,
                               color      = Profile)) +
      scale_color_manual(values = cscale_thr) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = "Gap cells w/ seedl.",
           x     = "Time",
           y     = "Gap cell share w/ seedl.") +
      c_theme +
      theme(legend.position = "none")
  }

  # stand species count
  pr_val <- tibble(Profile = c("IP", "MP"),
                   value   = np$seedl$species_n) %>%
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))

  stand_spc_count <- st %>%
    select(name, time_Step, seedl_gap_spc) %>%
    mutate(spc_l     = map(seedl_gap_spc, str_split, pattern = ","),
           spc_char  = map(spc_l, 1),
           spc_count = map_dbl(spc_char, ~ sum(. %in% np$seedl$species)))

  gg_p <- ggplot(stand_spc_count, aes(time_Step, spc_count, lty = name)) +
    geom_line() +
    geom_hline(data    = pr_val,
               mapping = aes(yintercept = value,
                             color      = Profile)) +
    scale_color_manual(values = cscale_thr) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Species in gaps",
         x     = "Time",
         y     = "# Species in gaps",
         color = "Threshold",
         name  = "Simulation") +
    c_theme

  plots[[5]] <- get_legend(gg_p)
  plots[[2]] <- gg_p +
    theme(legend.position = "none")


  # ind seedling share
  if (is.numeric(i_f$seedl_sha)) {
    plots[[3]] <- ggplot(i_f, aes(time_Step, seedl_sha, lty = name)) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = "ind seedl share",
           x     = "Time",
           y     = "Index value") +
      c_theme
  }


  # ind species count
  plots[[4]] <- ggplot(i_f, aes(time_Step, seedl_spc, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title = "ind seedl spc count",
         x     = "Time",
         y     = "Index value") +
    c_theme +
    theme(legend.position = "none")

  # ind seedl
  plots[[6]] <- ggplot(i_f, aes(time_Step, seedl, lty = name)) +
    geom_line() +
    scale_y_continuous(limits = c(-1, 1))+
    labs(title = "ind seedl",
         x     = "Time",
         y     = "Index value") +
    c_theme +
    theme(legend.position = "none")

  out <- plot_grid(plotlist = plots,
                   nrow     = 2,
                   byrow    = FALSE)
  return(out)
}


f.vis_nais_si_sapthi_stand_2ass_c3 <- function (np, st,
                                                c_theme     = theme_custom,
                                                cscale_thr2 = threshold_colors2,
                                                cscale_thr4 = threshold_colors4) {

  plots <- vector(mode = "list", length = 9)
  spc <- c("aalb", "pabi", "coni", "apse", "fsyl")
  legend_saved <- FALSE

  for (i in seq_along(spc)) {
    pr_val <- tibble(Profile  = rep(c("IP", "MP"), each = 2),
                     Limit    = rep(c("lower", "upper"), times = 2),
                     value    = np$sapthi[[spc[i]]]) %>%
      mutate(Threshold = str_c(Profile, " ", Limit),
             Threshold = factor(Threshold, levels = c("MP lower", "MP upper", "IP lower", "IP upper")))
    
    gg_p <- ggplot(st, aes_string("time_Step", str_c("st_", spc[i]), lty = "name")) +
      geom_line() +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = str_c("st ", spc[i]),
           x     = "Time",
           y     = "Share",
           lty   = "Simulation") +
      c_theme

    if (length(np$sapthi[[spc[i]]]) > 1) {
      gg_p <- gg_p +
        geom_hline(data    = pr_val,
                   mapping = aes(yintercept = value,
                                 color      = Threshold)) +
        scale_color_manual(values = cscale_thr4)
      
      if (legend_saved == FALSE) {
        plots[[7]] <- get_legend(gg_p +
                                   guides(linetype = "none"))
        plots[[8]] <- get_legend(gg_p +
                                   guides(color = "none"))
        legend_saved <- TRUE
      }
    }
    plots[[i]] <- gg_p +
      theme(legend.position = "none")
  }

  pr_val <- tibble(Profile = c("IP", "MP"),
                   value   = np$sapthi$tot) %>%
    mutate(Profile = factor(Profile, levels = c("MP", "IP")))

  plots[[9]] <- ggplot(st, aes(time_Step, st_tot, lty = name)) +
    geom_line() +
    geom_hline(data    = pr_val,
               mapping = aes(yintercept = value,
                             color      = Profile)) +
    scale_color_manual(values = cscale_thr2) +
  scale_y_continuous(limits = c(0, 1)) +
    labs(title = "st tot",
         x     = "Time",
         y     = "Share") +
    c_theme +
    theme(legend.position = "none")

  out <- plot_grid(plotlist = plots,
                   byrow = FALSE)

  return(out)
}


f.vis_nais_si_sapthi_ind_2ass_c3 <- function (i_f, c_theme = theme_custom) {

  plots <- vector(mode = "list", length = 9)
  legend_saved <- FALSE
  
  spc <- c("aalb", "pabi", "coni", "apse", "fsyl")
  for (i in seq_along(spc)) {
    if (is.numeric(i_f[[str_c("st_mix_", spc[i])]]) == FALSE) next

    gg_p <- ggplot(i_f, aes_string("time_Step", str_c("st_mix_", spc[i]), lty = "name")) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = str_c("ind mix ", spc[i]),
           x     = "Time",
           y     = "Index value",
           lty   = "Simulation") +
      c_theme
    
    if (legend_saved == FALSE) {
      plots[[6]] <- get_legend(gg_p)
      legend_saved <- TRUE
    }
    
    plots[[i]] <- gg_p +
      theme(legend.position = "none")
  }

  v_ind <- c("st_mix", "st_cc", "sapthi")
  for (i in seq_along(v_ind)) {
    plots[[6 + i]] <- ggplot(i_f, aes_string("time_Step", v_ind[i], lty = "name")) +
      geom_line() +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = str_c("ind ",v_ind[i]),
           x     = "Time",
           y     = "Index value") +
      c_theme +
      theme(legend.position = "none")
  }

  out <- plot_grid(plotlist = plots,
                   byrow    = FALSE)
  return(out)
}
