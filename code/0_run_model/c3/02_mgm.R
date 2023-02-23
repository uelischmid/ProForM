### Master Script
### Prepare mgm files
### 28.1.23, us


# single tree selection STS -----------------------------------------------
source("code/prepmgm/mgm_c3_STS.R")
rm(list = ls())


# group selection ---------------------------------------------------------
source("code/prepmgm/mgm_c3_GRS.R")
rm(list = ls())


# cable yarding -----------------------------------------------------------
source("code/prepmgm/mgm_c3_CAB1.R")
rm(list = ls())

source("code/prepmgm/mgm_c3_CAB2.R")
rm(list = ls())


# slit cuts ---------------------------------------------------------------
source("code/prepmgm/mgm_c3_SC.R")
rm(list = ls())

