## analysis of LT
## relaxed assessment
## 12.5.23, us


# setup -------------------------------------------------------------------
library(tidyverse)

assessments <- list.dirs("data/processed/naisoutput/2_assessment_relaxed/",
                         full.names = FALSE,
                         recursive  = FALSE)
assessments <- assessments[str_detect(assessments, "c3_LT")]


# analyze -----------------------------------------------------------------
res <- vector(mode = "list", length = length(assessments))

for (i in seq_along(assessments)) {
  cat("\r", i, "/", length(assessments), sep = "")
  
  # load data ----
  assessment <- assessments[i]
  assessment_infos <- str_split(assessment, "_") %>% 
    magrittr::extract2(1)
  
  nais_red <- read_rds(str_c("data/processed/naisoutput/2_assessment_relaxed/",
                             assessment, "/nais_indices_red.rds")) %>% 
    select(time_Step:sapthi)
  
  if (sum(is.na(nais_red$seedl)) > 0) {
    nais_red <- replace_na(nais_red,
                           list(seedl  = 0,
                                supptr = 0))
      
  }
  
  if (sum(is.na(nais_red)) > 0) quit(save = "ask")
  
  # MP met ----
  MP_met <- nais_red %>% 
    mutate(across(mix:sapthi, ~ case_when(.x >= 0 ~ TRUE,
                                          TRUE    ~ FALSE))) %>% 
    mutate(sha_ind_met = rowSums(across(mix:sapthi)) / 6,
           overall_met = case_when(sha_ind_met == 1.0 ~ TRUE,
                                   TRUE               ~  FALSE))
  
  # IP met ----
  IP_met <- nais_red %>% 
    mutate(across(mix:sapthi, ~ case_when(.x == 1.0 ~ TRUE,
                                          TRUE      ~ FALSE))) %>% 
    mutate(sha_ind_met = rowSums(across(mix:sapthi)) / 6,
           overall_met = case_when(sha_ind_met == 1.0 ~ TRUE,
                                   TRUE               ~  FALSE))
  
  # mean index value ----
  mean_ind <- nais_red %>% 
    mutate(mean_index = rowMeans(across(mix:sapthi)))

  # negative distance to MP ----
  dist_MP <- nais_red %>% 
    mutate(across(mix:sapthi, ~ pmin(.x, 0)),
           neg_dist      = rowSums(across(mix:sapthi)),
           neg_dist_norm = neg_dist / 6)

  # negative distance to IP ----
  dist_IP <- nais_red %>% 
    mutate(across(mix:sapthi, ~ .x - 1),
           neg_dist      = rowSums(across(mix:sapthi)),
           neg_dist_norm = neg_dist / 12)

  # assemble ----
  res[[i]] <- tibble(stratum       = assessment_infos[3],
                     quality       = assessment_infos[4],
                     q_site        = as.numeric(str_sub(assessment_infos[4], end = 1)),
                     q_reg         = as.numeric(str_sub(assessment_infos[4], start = 2)),
                     init          = "LT",
                     mgm           = assessment_infos[5],
                     mgm_interval  = as.numeric(str_sub(assessment_infos[6], start = 2)),
                     mgm_intensity = as.numeric(str_sub(assessment_infos[7], start = 2)),
                     nat_haz       = assessment_infos[8],
                     sha_y_MP_met  = sum(MP_met$overall_met) / nrow(MP_met),
                     sha_i_MP_met  = mean(MP_met$sha_ind_met),
                     sha_y_IP_met  = sum(IP_met$overall_met) / nrow(IP_met),
                     sha_i_IP_met  = mean(IP_met$sha_ind_met),
                     mean_i_total  = mean(mean_ind$mean_index),
                     neg_dist_MP   = mean(dist_MP$neg_dist_norm),
                     neg_dist_IP   = mean(dist_IP$neg_dist_norm))
}


# combine and save --------------------------------------------------------
res <- bind_rows(res)

write_rds(res,
          "results/c3_analysis_LT/LT_relaxed.rds")
