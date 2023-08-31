library(tidyverse)
rm(list = ls())

################################################################################
# Prep for Poststratify

poststrat_df <- read_csv("data-collection/data/input/pid/output/poststratdf-12.csv") #nolint
poststrat_df$year <- 2012

# load models
load("/home/reed/Dropbox/01-samara-ra/data-collection/models/weighted/baseline-w.RData")
load("/home/reed/Dropbox/01-samara-ra/data-collection/models/weighted/stpreds-w.RData")
load("/home/reed/Dropbox/01-samara-ra/data-collection/models/weighted/stpreds-plus-w.RData")
load("/home/reed/Dropbox/01-samara-ra/data-collection/models/weighted/stpreds-christian-w.RData")
load("/home/reed/Dropbox/01-samara-ra/data-collection/models/other/spec1-res.RData")
load("/home/reed/Dropbox/01-samara-ra/data-collection/models/other/spec2-res.RData")
load("/home/reed/Dropbox/01-samara-ra/data-collection/models/other/spec3-res.RData")
load("/home/reed/Dropbox/01-samara-ra/data-collection/models/other/spec4-res.RData")

baseline <- baseline_w
stpreds <- stpreds_w
stpreds_plus <- stpreds_plus_w
stpreds_christian <- stpreds_christian_w

# source poststratification functions
source("/home/reed/Dropbox/01-samara-ra/data-collection/scripts/pid/modeling-functions.r")

# add state-level predictors to the poststratification frame

# add state abbreviation
fips <- read.csv("data-collection/data/input/helpers/us-state-ansi-fips.csv")
fips <- fips |> select(-st, abbr = stusps)
fips$abbr <- fips$abbr |> str_trim()
poststrat_df <- poststrat_df |>
    left_join(
        fips,
        by = c("state" = "stname")
    )
# add state-level predictors, except region and pct_christian
state_df <- rio::import("data-collection/data/final.csv")
poststrat_df <- poststrat_df |>
    left_join(
        state_df |>
            select(
                state,
                year,
                rep_vote_share,
                pct_black,
                pct_urban_pop
            ),
        by = c("state", "year")
    )
# add region
region_df <- rio::import("data-collection/data/input/state-region.csv")
region_df <- region_df |> filter(abbr != "AK", abbr != "HI")
poststrat_df <- poststrat_df |>
    left_join(
        region_df,
        by = "abbr"
    )
# add pct_christian
load("/home/reed/Dropbox/01-samara-ra/data-collection/data/output/christian-2012.rds")
poststrat_df <- poststrat_df |>
    left_join(
        christian_2012,
        by = "abbr"
    )
view(poststrat_df)
################################################################################
# Prep test data

# load training and testing data
load("/home/reed/Dropbox/01-samara-ra/data-collection/data/input/pid/input/ces/train-test.RData")

test <- test_5000
train <- train_5000

# recode
test$dem_lean <- NA
test$dem_lean[test$pid_lean == "Dem"] <- 1
test$dem_lean[test$pid_lean != "Dem"] <- 0
test$rep_lean <- NA
test$rep_lean[test$pid_lean == "Rep"] <- 1
test$rep_lean[test$pid_lean != "Rep"] <- 0
test$ind_lean <- NA
test$ind_lean[test$pid_lean == "Ind"] <- 1
test$ind_lean[test$pid_lean != "Ind"] <- 0
disag_state_test <- test |>
    group_by(state, eth, male, educ, age) |>
    summarize(
        pct_dem = sum(dem_lean) / n(),
        pct_rep = sum(rep_lean) / n(),
        pct_ind = sum(ind_lean) / n()
    )
# join poststrat. table to survey summary of cells
poststrat_df_baseline <- poststrat_df |>
    select(
        eth,
        male,
        educ,
        age,
        state,
        n
    )
disag_state_test <- disag_state_test |>
    left_join(
        poststrat_df_baseline,
        by = c("eth", "male", "educ", "age", "state")
    )
# poststratify
disag_state_test <- disag_state_test |>
    group_by(state) |>
    summarize(
        pct_dem_post = sum(n * pct_dem) / sum(n),
        pct_rep_post = sum(n * pct_rep) / sum(n),
        pct_ind_post = sum(n * pct_ind) / sum(n)
    )
disag_state_test <- disag_state_test |>
    group_by(state) |>
    summarize(
        dem_est = mean(pct_dem_post),
        rep_est = mean(pct_rep_post),
        ind_est = mean(pct_ind_post)
    )
# get a dataframe for each party ID
disag_state_test_dem <- disag_state_test |>
    select(
        state,
        truth_col = dem_est
        )
disag_state_test_rep <- disag_state_test |>
    select(
        state,
        truth_col = rep_est
        )
disag_state_test_ind <- disag_state_test |>
    select(
        state,
        truth_col = ind_est
        )

################################################################################
# Poststratify
##########
# Disaggregated
# recode
train$dem_lean <- NA
train$dem_lean[train$pid_lean == "Dem"] <- 1
train$dem_lean[train$pid_lean != "Dem"] <- 0
train$rep_lean <- NA
train$rep_lean[train$pid_lean == "Rep"] <- 1
train$rep_lean[train$pid_lean != "Rep"] <- 0
train$ind_lean <- NA
train$ind_lean[train$pid_lean == "Ind"] <- 1
train$ind_lean[train$pid_lean != "Ind"] <- 0
disag_state_post <- train |>
    group_by(state, eth, male, educ, age) |>
    summarize(
        pct_dem = sum(dem_lean) / n(),
        pct_rep = sum(rep_lean) / n(),
        pct_ind = sum(ind_lean) / n()
    )
# join poststrat. table to survey summary of cells
poststrat_df_baseline <- poststrat_df |>
    select(
        eth,
        male,
        educ,
        age,
        state,
        n
    )
disag_state_post <- disag_state_post |>
    left_join(
        poststrat_df_baseline,
        by = c("eth", "male", "educ", "age", "state")
    )
# poststratify
disag_state_post <- disag_state_post |>
    group_by(state) |>
    summarize(
        pct_dem_post = sum(n * pct_dem) / sum(n),
        pct_rep_post = sum(n * pct_rep) / sum(n),
        pct_ind_post = sum(n * pct_ind) / sum(n)
    )
disag_state_post <- disag_state_post |>
    group_by(state) |>
    summarize(
        dem_est = mean(pct_dem_post),
        rep_est = mean(pct_rep_post),
        ind_est = mean(pct_ind_post)
    )
################################################################################
##########
# Baseline
baseline_state_output <- estimate_state_pid(
    baseline,
    poststrat_df
)
##########
# stpreds (region and rep_vote_share)
stpreds_state_output <- estimate_state_pid(
    stpreds,
    poststrat_df
)
##########
# stpreds_plus_w
stpreds_plus_state_output <- estimate_state_pid(
    stpreds_plus,
    poststrat_df
)
##########
# stpreds_christian
stpreds_christian_state_output <- estimate_state_pid(
    stpreds_christian,
    poststrat_df
)
################################################################################
# other model spec. testing
##########
# spec 1
spec1_state_output <- estimate_state_pid(
    spec1,
    poststrat_df
)
##########
# spec 2
spec2_state_output <- estimate_state_pid(
    spec2,
    poststrat_df
)
##########
# spec 3
spec3_state_output <- estimate_state_pid(
    spec3,
    poststrat_df
)
##########
# spec 4
spec4_state_output <- estimate_state_pid(
    spec4,
    poststrat_df
)
################################################################################
# prep data for presenting results

##########
# Split data by party and model for "compare to truth" plots in the analysis doc

# Disaggregated data
disag_state_dem <- disag_state_post |>
    select(
        state,
        mean_pid = dem_est
    )
disag_state_rep <- disag_state_post |>
    select(
        state,
        mean_pid = rep_est
    )
disag_state_ind <- disag_state_post |>
    select(
        state,
        mean_pid = ind_est
    )
# Baseline data 
baseline_state_dem <- baseline_state_output |>
    filter(
         party == "dem"
    )
baseline_state_rep <- baseline_state_output |>
    filter(
        party == "rep"
    )
baseline_state_ind <- baseline_state_output |>
    filter(
        party == "ind"
    )
# Stpreds data 
stpreds_state_dem <- stpreds_state_output |>
    filter(
        party == "dem"
    )
stpreds_state_rep <- stpreds_state_output |>
    filter(
        party == "rep"
    )
stpreds_state_ind <- stpreds_state_output |>
    filter(
        party == "ind"
    )
# stpreds_plus data
stpreds_plus_state_dem <- stpreds_plus_state_output |>
    filter(
        party == "dem"
    )
stpreds_plus_state_rep <- stpreds_plus_state_output |>
    filter(
        party == "rep"
    )
stpreds_plus_state_ind <- stpreds_plus_state_output |>
    filter(
        party == "ind"
    )

################################################################################
# combine data to analyze error, performance scores, etc...
##########
# main models
stpreds_plus <- stpreds_plus_state_output |>
    mutate(
        model = "3"
    )
stpreds <- stpreds_state_output |>
    mutate(
        model = "2"
    )
baseline <- baseline_state_output |>
    mutate(
        model = "1"
    )
christian <- stpreds_christian_state_output |>
    mutate(
        model = "4"
    )
mrp_results <- rbind(
    stpreds_plus,
    stpreds,
    baseline,
    christian
)
mrp_results <- mrp_results |>
    select(
        model,
        state,
        party,
        mean_pid,
        se_pid = se
    )
##########
# other model spec. testing (mostly tweaks stpreds_plus)
spec1 <- spec1_state_output |>
    mutate(
        model = "int. eth:blk"
    )
spec2 <- spec2_state_output |>
    mutate(
        model = "urban + christian"
    )
spec3 <- spec3_state_output |>
    mutate(
        model = "urban only"
    )
spec4 <- spec4_state_output |>
    mutate(
        model = "pct. black only"
    )
other_spec_results <- rbind(
    spec1,
    spec2,
    spec3,
    spec4
)
other_spec_results <- other_spec_results |>
    select(
        model,
        state,
        party,
        mean_pid,
        se_pid = se
    )
##########
# output truth data for comparison
all_truth <- disag_state_test |>
    rename(
        dem = dem_est,
        rep = rep_est,
        ind = ind_est
    ) |>
    pivot_longer(
        cols = c(dem, rep, ind),
        names_to = "party",
        values_to = "truth_pid"
    )

################################################################################
# output

save(
    mrp_results,
    other_spec_results,
    all_truth,
    disag_state_dem,
    disag_state_rep,
    disag_state_ind,
    disag_state_test_dem,
    disag_state_test_rep,
    disag_state_test_ind,
    baseline_state_dem,
    baseline_state_rep,
    baseline_state_ind,
    stpreds_state_dem,
    stpreds_state_rep,
    stpreds_state_ind,
    stpreds_plus_state_dem,
    stpreds_plus_state_rep,
    stpreds_plus_state_ind,
    file = "data-collection/scripts/pid/ces-08to20/model-testing/analysis-w-prep.RData"
)
