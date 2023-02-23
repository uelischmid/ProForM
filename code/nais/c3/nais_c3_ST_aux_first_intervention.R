## ST aux simulations
## determine time of first intervention based on nais assessments
## 28.1.23, us


# setup -------------------------------------------------------------------
library(tidyverse)

nais_assessments <- list.dirs("data/processed/naisoutput/2_assessment/",
                              full.names = FALSE,
                              recursive  = FALSE)
nais_assessments <- nais_assessments[str_detect(nais_assessments, "c3_ST_aux")]


# determine year of first intervention ------------------------------------
first_interventions <- vector(mode = "list", length = length(nais_assessments))

for (i in seq_along(nais_assessments)) {
  # load data ----
  assessment <- nais_assessments[i]
  assessment_infos <- str_split(assessment, "_") %>% 
    magrittr::extract2(1)
  ind_red <- read_rds(str_c("data/processed/naisoutput/2_assessment/",
                            assessment, "/nais_indices_red.rds"))

  # check MP for subindices
  ind_MP <- ind_red %>%
    mutate(across(mix:sapthi, ~ case_when(.x >= 0 ~ TRUE,
                                             TRUE    ~ FALSE)))
  
  # ignore mix if regeneration quality = 1
  if (str_sub(assessment_infos[5], start = 2) == "1") {
    ind_MP <- ind_MP %>% 
      mutate(mix = TRUE)
  }
  
  # check overall MP
  ind_MP <- ind_MP %>% 
    mutate(MP_score = (mix + vert + horiz + supptr + seedl + sapthi) / 6,
           MP_met   = case_when(MP_score == 1 ~ TRUE,
                                TRUE          ~ FALSE))
  
  # first year where MP is not met
  first_interventions[[i]] <- ind_MP %>% 
    filter(MP_met == FALSE) %>% 
    slice_min(order_by = time_Step,
              n        = 1) %>% 
    select(y_MP_not_met = time_Step) %>% 
    mutate(y_first_int = max(c(y_MP_not_met - 50, 1)),
           stratum = assessment_infos[4],
           quality = assessment_infos[5],
           init = assessment_infos[6],
           nat_haz = assessment_infos[7]) %>% 
    select(stratum, quality, init, nat_haz, y_MP_not_met, y_first_int)
}


first_interventions <- bind_rows(first_interventions)

first_interventions_red <- first_interventions %>% 
  group_by(stratum, quality, init) %>% 
  slice_min(order_by  = y_first_int,
            n         = 1,
            with_ties = FALSE) %>% 
  select(-nat_haz)

print(first_interventions_red, n = 36)
