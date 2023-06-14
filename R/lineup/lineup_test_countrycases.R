library(dtplyr)
library(data.table)
library(tidyverse)
source("R/lineup/db_utils.R")
source("R/lineup/svy_nac.R")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load means data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This data is suuposed to be obtained within the pipeline, not afterwards

dt_ref_mean_pred <- qs::qread("data/dt_ref_mean_pred.qs")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extrapolation   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

country <- "AGO"





