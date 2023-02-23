### calculate mean nais-indices per year
### c3 su eq mgm simulations/assessments
### and determine year for init ST2
### 27.1.23, us


# setup -------------------------------------------------------------------
library(tidyverse)

nais_assessments <- list.dirs("data/processed/naisoutput/2_assessment", full.names = FALSE, recursive = FALSE)
nais_assessments <- nais_assessments[str_detect(nais_assessments, "c3_su_eq_mgm")]

mean_indices <- vector(mode = "list", length = length(nais_assessments))

for (i in seq_along(nais_assessments)) {
  assessment_info <- nais_assessments[i] %>% 
    str_split("_") %>% 
    magrittr::extract2(1)
  
  assessment <- read_rds(str_c("data/processed/naisoutput/2_assessment/",
                               nais_assessments[i],
                               "/nais_indices_red.rds"))
  
  mean_indices[[i]] <- assessment %>%
    pivot_longer(cols     = mix:sapthi,
                 names_to = "index") %>% 
    group_by(time_Step) %>% 
    summarise(index_mean = mean(value, na.rm = TRUE)) %>% 
    mutate(stratum = assessment_info[5],
           quality = assessment_info[6],
           nat_haz = assessment_info[7])
}

mean_indices <- bind_rows(mean_indices)

init_years <- mean_indices %>% 
  pivot_wider(names_from   = nat_haz,
              names_prefix = "mean_index_",
              values_from  = index_mean) %>% 
  mutate(mean_index_both = mean_index_A + mean_index_LED) %>% 
  group_by(stratum, quality) %>% 
  slice_max(order_by  = mean_index_both,
            n         = 1,
            with_ties = FALSE) %>% 
  select(stratum, quality, time_Step, mean_index_both)

print(init_years)
