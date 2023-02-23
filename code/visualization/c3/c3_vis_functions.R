# visualization of c3 analysis
# functions
# 6.2.23, us


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