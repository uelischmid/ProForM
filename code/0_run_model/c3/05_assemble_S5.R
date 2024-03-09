### Master Script
### Assemble graphs for supplementary material S5
### 2.1.24, us


# setup -------------------------------------------------------------------
library(tidyverse)
folder_target_base <- "results/c3_S5/"

# get mgm-simulation names
simulations_orig <- list.dirs("data/processed/simoutput/",
                              full.names = FALSE,
                              recursive = FALSE)
simulations_c3 <- simulations_orig[str_detect(simulations_orig, "c3_LT|c3_ST_def")] %>% 
  str_replace_all("c3_", "") %>% 
  str_replace("def_", "")

simulations_c3_mgm <- simulations_c3[str_detect(simulations_c3, "NOM", negate = TRUE)]

# create folders in target
sapply(str_c(folder_target_base, "/", simulations_c3_mgm), dir.create)

rm(simulations_orig, simulations_c3)


# simulation visualizations -----------------------------------------------
folder_source_simvis <- "results/sim_vis/2_sim_c3/"

files_simvis <- list.files(folder_source_simvis)
table_simvis <- tibble(source_file = files_simvis) %>% 
  mutate(target_subf = source_file %>% 
           str_replace("Sim_", "") %>% 
           str_replace(".jpg", ""),
         source_path = str_c(folder_source_simvis, source_file),
         target_path = str_c(folder_target_base, target_subf, "/", source_file))

cat("Copy simulation visualizations\n")
for (i in seq_along(files_simvis)) {
  cat("\r copying ", i, "/", length(files_simvis))
  file.copy(from      = table_simvis[[i, "source_path"]],
            to        = table_simvis[[i, "target_path"]],
            overwrite = TRUE)
}
cat("\n\n")

rm(folder_source_simvis, files_simvis, table_simvis, i)


# NaiS F2 -----------------------------------------------------------------
folder_source_f2 <- "results/nais_vis/n_sim_F2/c3_mgm_NOM/"

files_f2 <- list.files(folder_source_f2)
table_f2 <- tibble(source_file = files_f2) %>%
  mutate(target_subf = source_file %>%
           str_replace("F2_", "") %>%
           str_replace("_A.png", "") %>%
           str_replace("_LED.png", ""),
         source_path = str_c(folder_source_f2, source_file),
         target_path = str_c(folder_target_base, target_subf, "/", source_file))

cat("Copy F2\n")
for (i in seq_along(files_f2)) {
  cat("\r copying ", i, "/", length(files_f2))
  file.copy(from      = table_f2[[i, "source_path"]],
            to        = table_f2[[i, "target_path"]],
            overwrite = TRUE)
}
cat("\n\n")

rm(folder_source_f2, files_f2, table_f2, i)


# NaiS gap analysis -------------------------------------------------------
folder_source_gaps <- "results/nais_vis/gaps/c3_mgm_NOM/"

files_gaps <- list.files(folder_source_gaps)
table_gaps <- tibble(source_file = files_gaps) %>%
  mutate(target_subf = source_file %>%
           str_replace("Gaps_", "") %>%
           str_replace(".jpg", ""),
         source_path = str_c(folder_source_gaps, source_file),
         target_path = str_c(folder_target_base, target_subf, "/", source_file))

cat("Copy Gaps\n")
for (i in seq_along(files_gaps)) {
  cat("\r copying ", i, "/", length(files_gaps))
  file.copy(from      = table_gaps[[i, "source_path"]],
            to        = table_gaps[[i, "target_path"]],
            overwrite = TRUE)
}
cat("\n\n")

rm(folder_source_gaps, files_gaps, table_gaps, i)


# NaiS subindices ---------------------------------------------------------
folder_source_si <- "results/nais_vis/1_sim_subind/c3_relaxed_mgm_nom/"

folders_si <- list.dirs(folder_source_si,
                        full.names = FALSE,
                        recursive  = FALSE)
# folders_si <- folders_si[str_detect(folders_si, "NOM", negate = TRUE)]

table_si <- tibble(source_folder = folders_si) %>%
  mutate(source_path      = str_c(folder_source_si, source_folder, "/"),
         target_subfolder = source_folder %>%
           str_replace("c3_", "") %>%
           str_replace("def_", "") %>%
           str_replace("_A", "") %>%
           str_replace("_LED", ""),
         target_path      = str_c(folder_target_base, target_subfolder, "/"),
         old_subfolder    = str_c(target_path, source_folder, "/"),
         new_subfolder1   = case_when(str_detect(source_folder, "_A") == TRUE ~ "NaiS_subindices_A",
                                      TRUE ~ "NaiS_subindices_LED"),
         new_subfolder2   = str_c(folder_target_base, target_subfolder, "/",
                                  new_subfolder1, "/"))
cat("Copy subindices\n")
for(i in seq_along(folders_si)) {
  cat("\r copying ", i, "/", length(folders_si))
  file.copy(from      = table_si[[i, "source_path"]],
            to        = table_si[[i, "target_path"]],
            overwrite = TRUE,
            recursive = TRUE)
  file.rename(from = table_si[[i, "old_subfolder"]],
              to   = table_si[[i, "new_subfolder2"]])
}
cat("\n\n")

rm(folder_source_si, folders_si, table_si, i)


# Rename simulation folders -----------------------------------------------
table_folders <- tibble(folder_old = simulations_c3_mgm) %>% 
  mutate(path_old   = str_c(folder_target_base, folder_old, "/"),
         folder_new = case_when(str_detect(folder_old, "LT_") ~ str_c(str_sub(folder_old, 4, 8),
                                                                      "_LT_initLT_",
                                                                      str_sub(folder_old, 10, -1)),
                                TRUE ~ str_c(str_sub(folder_old, 4, 8),
                                             "_ST_",
                                             str_sub(folder_old, 10, -1))),
         path_new   = str_c(folder_target_base, folder_new, "/"))

cat("Rename folders\n")
for (i in seq_along(simulations_c3_mgm)) {
  file.rename(from = table_folders[[i, "path_old"]],
              to   = table_folders[[i, "path_new"]])
}
cat("\n")

rm(table_folders, i)
