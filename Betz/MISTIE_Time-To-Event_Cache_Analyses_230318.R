# Cache MISTIE III Time-To-Event TMLE Analyses
# Author: Josh Betz (jbetz@jhu.edu)
# Description: The TMLE estimates of the survival curve involve quite a lot of
# computing time. This will save an .Rda with the survival metadata, which makes
# it quicker to compute 

library(dplyr)
library(tidyr)
library(adjrct)

data_url <-
  "https://github.com/jbetz-jhu/CovariateAdjustmentTutorial/raw/main/Simulated_MISTIE_III_v1.2.csv"

sim_miii <-
  read.csv(file = url(data_url)) %>% 
  dplyr::slice(
    1:500
  )

# Read in data: Recast categorical variables as factors
sim_miii <-
  sim_miii %>% 
  dplyr::tibble() %>% 
  dplyr::mutate(
    male =
      factor(x = male, levels = 0:1, labels = c("0. Female", "1. Male")),
    across(
      .cols = all_of(x = c("hx_cvd", "hx_hyperlipidemia",
                           "on_anticoagulants", "on_antiplatelets")),
      .fns = function(x) factor(x, levels = 0:1, labels = c("0. No", "1. Yes"))
    ),
    across(
      .cols = starts_with("gcs") | starts_with("mrs"),
      .fns = factor
    ),
    ich_location =
      factor(
        x = ich_location,
        levels = c("Deep", "Lobar")
      ),
    arm =
      factor(
        x = arm,
        levels = c("medical", "surgical")
      ),
    tx = 1*(arm == "surgical")
  )

surv_metadata_unadj <-
  adjrct::survrct(
    outcome.formula =
      Surv(days_on_study, died_on_study) ~ tx, 
    trt.formula = tx ~ 1,
    data = sim_miii,
  )

surv_metadata_adj <-
  adjrct::survrct(
    outcome.formula = Surv(days_on_study, died_on_study) ~ tx +
      age + male + hx_cvd + hx_hyperlipidemia +
      on_anticoagulants + on_antiplatelets + ich_location +
      ich_s_volume + ivh_s_volume + gcs_category,
    trt.formula =
      tx ~
      age + male + hx_cvd + hx_hyperlipidemia +
      on_anticoagulants + on_antiplatelets + ich_location +
      ich_s_volume + ivh_s_volume + gcs_category,
    data = sim_miii
  )

save(
  list = c("surv_metadata_unadj", "surv_metadata_adj"),
  file = file.path(getwd(), "Betz", "sim_mistie_iii_1.2_tmle.Rdata")
)