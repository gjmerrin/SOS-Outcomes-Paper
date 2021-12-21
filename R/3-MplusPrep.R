###########################################
#                                         #
#           Sources of Strength           #
#    Preparing Data for Mplus Analysis    #
#                                         #
###########################################

## Loading packages and functions
source("Scripts/PnF.R")
load("Output/SoS_Data.RData")
load("Output/Longitudinal_MI.RData")


# ------  Adding School-level Variables -----------------------------

## School demographics
schdemos <- readxl::read_excel("SoS Data and Codes/RSVP2 School Stats.xlsx", sheet = "RReady") %>%
  mutate(Rural = ifelse(Rural_Urban == "Urban", 0, 1))

#### Investigating School Climate Indicators ####
# Note: See 4-Analysis_ML.R for checks
schclimate.orig <- read_sav("../00 School Climate W1-W4 cleaned/SPSS/sources_school_climate_w1_2021-1-27.sav")

## Dichotomizing responses and re-checking baseline differences
schclimate <- schclimate.orig %>%
  mutate(School = as.character(as_factor(RECODED_SCHOOLS_W1)),
         Teasing = case_when(is.na(AGGRESSION_PROBLEM_AT_SCHOOL_3_W1) ~ NA_real_,
                             AGGRESSION_PROBLEM_AT_SCHOOL_3_W1 == 3 ~ NA_real_,    # Don't know
                             AGGRESSION_PROBLEM_AT_SCHOOL_3_W1 %in% c(4, 5) ~ 1,   # pretty big or huge problem
                             TRUE ~ 0),
         MH_Training = case_when(is.na(SCHOOL_COMMITMENT_MT_HEALTH_6_W1) ~ NA_real_,
                                 SCHOOL_COMMITMENT_MT_HEALTH_6_W1 == 3 ~ NA_real_,  # Don't know
                                 SCHOOL_COMMITMENT_MT_HEALTH_6_W1 %in% c(4, 5) ~ 1, # fair amount or a lot of training
                                 TRUE ~ 0))

#### Joining school-level variables to student-level data ####

ds3 <- ds3 %>%
  mutate(School = as.character(as_factor(School))) %>%
  left_join(schdemos, by = "School") %>%
  left_join(schclimate %>%
              group_by(School) %>%
              summarize(Teasing_Per = mean(Teasing, na.rm = TRUE),           # % of school staff reporting teasing was pretty big or huge problem
                        MH_Training_Per = mean(MH_Training, na.rm = TRUE)),  # % of school staff reporting fair amount or a lot of mental health training
            by = "School")

#############################################################################

## Selecting variables for Mplus dataset and renaming to 8 or fewer characters
mpluswide <- ds3 %>%
  select(ID = StudentID, School:Grade, TransG = Transgender, Gender:Asian, MultiR = Multiracial, Indigen = Indigenous,
         POC:GL, Quest = Questioning, OtherSO, LGBQ,
         FGxTx = FemalexTx, MGxTx = MalexTx, OGxTx = OtherGxTx,
         WRxTx = WhitexTx, LRxTx = LatinxxTx, BRxTx = BlackxTx, ARxTx = AsianxTx, MRxTx = MultiracialxTx, IRxTx = IndigenousxTx, POCxTx,
         SSxTx = StraightxTx, BSxTx = BisexualxTx, GLxTx, QSxTx = QuestioningxTx, OSxTx = OtherSOxTx, LGBQxTx,
         AGE_W1:AGE_W4,
         starts_with(c(NoContactPerpVars, ContactPerpVars,         # Primary Outcome Items
                       NoContactVictVars, ContactVictVars,
                       CyberVars, DismisssvVars,
                       HNCPerpVars, HNCVictVars)),
         starts_with(c(WellBeingVars, SHHelpAttitudeVars, SHHelpIntentVars)), # Secondary Outcome (Protective Factor) Items
         starts_with(c("TotAdultNoms", "Trusted_Adult",
                       "Active_Exposure_Score", "Passive_Exposure_Score",  # Raw scale scores
                       "No_Contact_Perpetration_Score", "No_Contact_Victimization_Score",
                       "Contact_Perpetration_Score", "Contact_Victimization_Score",
                       "HNC_Perpetration_Score", "HNC_Victimization_Score",
                       "Cybersex_Perpetration_Score", "SV_Dismissiveness_Score",
                       "General_Well.being_Score", "SH_Help_Attitudes_Score", "SH_Help_Seeking_Score")),
                  NCPerp1:Passive4,                                                                            # Factor Scores
         Funding = Funding_Per_Pupil, Rural, Teas_Per = Teasing_Per, MHT_Per = MH_Training_Per) %>%   # School-level variables
  rename_with(~str_replace(.x, "HasASurvey", "srvy"), .cols = starts_with("HasASurvey")) %>%          # Renaming variables to <= 8 characters per Mplus requirement
  rename_with(~str_replace(.x, "SEX_VIOL_PERP_", "SVP"), .cols = starts_with("SEX_VIOL_PERP_")) %>%
  rename_with(~str_replace(.x, "SEX_VIOL_VICT_", "SVV"), .cols = starts_with("SEX_VIOL_VICT_")) %>%
  rename_with(~str_replace(.x, "HOM_PERP_", "HOP"), .cols = starts_with("HOM_PERP_")) %>%
  rename_with(~str_replace(.x, "HOM_VICT_", "HOV"), .cols = starts_with("HOM_VICT_")) %>%
  rename_with(~str_replace(.x, "CYBER_SEX_PERP_", "CYP"), .cols = starts_with("CYBER_SEX_PERP_")) %>%
  rename_with(~str_replace(.x, "DISMISS_SEX_VIOL_", "DSV"), .cols = starts_with("DISMISS_SEX_VIOL_")) %>%
  rename_with(~str_replace(.x, "GEN_WELL_BEING_", "GWB"), .cols = starts_with("GEN_WELL_BEING_")) %>%
  rename_with(~str_replace(.x, "SEX_HAR_HELP_ATTITUDE_", "SHA"), .cols = starts_with("SEX_HAR_HELP_ATTITUDE_")) %>%
  rename_with(~str_replace(.x, "SEX_HAR_HELP_INTENT_", "SHI"), .cols = starts_with("SEX_HAR_HELP_INTENT_")) %>%
  rename_with(~str_replace(.x, "TotAdultNoms_W", "TotAN_"), .cols = starts_with("TotAdultNoms_W")) %>%
  rename_with(~str_replace(.x, "Trusted_Adult_W", "TA_"), .cols = starts_with("Trusted_Adult_W")) %>%
  rename_with(~str_replace(.x, "Active_Exposure_Score_W", "AEXS_"), .cols = starts_with("Active_Exposure_Score_W")) %>%
  rename_with(~str_replace(.x, "Passive_Exposure_Score_W", "PEXS_"), .cols = starts_with("Passive_Exposure_Score_W")) %>%
  rename_with(~str_replace(.x, "No_Contact_Perpetration_Score_W", "NCPS_"), .cols = starts_with("No_Contact_Perpetration_Score_W")) %>%
  rename_with(~str_replace(.x, "No_Contact_Victimization_Score_W", "NCVS_"), .cols = starts_with("No_Contact_Victimization_Score_W")) %>%
  rename_with(~str_replace(.x, "Contact_Perpetration_Score_W", "CPS_"), .cols = starts_with("Contact_Perpetration_Score_W")) %>%
  rename_with(~str_replace(.x, "Contact_Victimization_Score_W", "CVS_"), .cols = starts_with("Contact_Victimization_Score_W")) %>%
  rename_with(~str_replace(.x, "HNC_Perpetration_Score_W", "HOPS_"), .cols = starts_with("HNC_Perpetration_Score_W")) %>%
  rename_with(~str_replace(.x, "HNC_Victimization_Score_W", "HOVS_"), .cols = starts_with("HNC_Victimization_Score_W")) %>%
  rename_with(~str_replace(.x, "Cybersex_Perpetration_Score_W", "CYPS_"), .cols = starts_with("Cybersex_Perpetration_Score_W")) %>%
  rename_with(~str_replace(.x, "SV_Dismissiveness_Score_W", "DSVS_"), .cols = starts_with("SV_Dismissiveness_Score_W")) %>%
  rename_with(~str_replace(.x, "General_Well.being_Score_W", "GWBS_"), .cols = starts_with("General_Well.being_Score_W")) %>%
  rename_with(~str_replace(.x, "SH_Help_Attitudes_Score_", "SHAS_"), .cols = starts_with("SH_Help_Attitudes_Score_")) %>%
  rename_with(~str_replace(.x, "SH_Help_Seeking_Score_W", "SHIS_"), .cols = starts_with("SH_Help_Seeking_Score_W")) %>%
  rename_with(~str_replace(.x, "Cybersex", "CSPerp"), .cols = starts_with("Cybersex")) %>%
  rename_with(~str_replace(.x, "GenWellBeing", "GenWB"), .cols = starts_with("GenWellBeing")) %>%
  rename_with(~str_replace(.x, "SHHelpAtt", "SHHAtt"), .cols = starts_with("SHHelpAtt")) %>%
  rename_with(~str_replace(.x, "SHHelpSeek", "SHHSeek"), .cols = starts_with("SHHelpSeek")) %>%
  rename_with(~str_replace(.x, "_W", "_"), .cols = matches("_W[0-9]$")) %>%
  mutate(School = factor(School, levels = SchoolNames)) %>%
  mutate(across(where(is.factor), as.numeric))                       # Converting factors to numeric per Mplus requirements

table(apply(mpluswide, 2, class))


## distribution of teacher nominations; use a negative binomial model b/c sds are consistently higher than the mean (overdispersion)
nomstats <- mpluswide %>%
  select(ID, School, Tx, starts_with("Tot")) %>%
  gather(Wave, Noms, starts_with("Tot")) %>%
  group_by(Wave) %>%
  skimr::skim(Noms)

lapply(paste0("TotAN_", 1:4), function(x) table(mpluswide[[x]]))

## checking if frequencies match between original data and renamed mplus data
# No contact perpetration
map2(.x = paste0(rep(paste0(OutcomeVars$No_Contact_Perpetration, "_W"), each = 4), 1:4),
     .y = paste0(rep(c("SVP1_", "SVP2_", "SVP3_", "SVP4_"), each = 4), 1:4),
     ~table(ds3[[.x]], mpluswide[[.y]]))

# SH Help Seeking
map2(.x = paste0(rep(paste0(ProtectiveVars$SH_Help_Seeking, "_W"), each = 4), 1:4),
     .y = paste0(rep(c("SHI1_", "SHI2_", "SHI3_", "SHI4_"), each = 4), 1:4),
     ~table(ds3[[.x]], mpluswide[[.y]]))

# Trusted Adult
map2(.x = paste0("Trusted_Adult_W", 1:4),
     .y = paste0("TA_", 1:4),
     ~table(ds3[[.x]], mpluswide[[.y]]))

# Saving csv data file for mplus models 
# write.csv(mpluswide,   file = "mpluswide_wTANoms_colnames.csv", row.names = FALSE, na = "-999")
# write.table(mpluswide, file = "mpluswide_wTANoms.csv", row.names = FALSE, col.names = FALSE,  na = "-999", sep = ",")


#########################################################################################################

#### Results set up ####
# Note: readModels organizes them in alphabetical order
mplusoutcomes = c(ncp = "SH Perpetration", ncv = "SH Victimization", cp = "FSC Perpetration", cv = "FCS Victimization", 
                  hop = "HNC Perpetration", hov = "HNC Victimization", cyber = "Cybersex Perpetration", dismiss = "SV Dismissiveness",
                  gwb = "General Well-Being", sha = "SH Attitudes", shi = "SH Help Intent", ta = "Trusted Adult")

outcome_shortcut <- function(df, remove, recodes){
  df <- df %>%
    mutate(outcome = str_remove(outcome, remove),
           outcome = dplyr::recode(outcome, !!!recodes) %>%
             factor(., levels = recodes)) %>%
    arrange(outcome)
  return(df)
}

# ------  Latent Growth Model Mplus Results -------------------

lgm.preds <- c(I = "Intercept", S = "Intercept")

## Importing models and converting results to tables
lgm.ss.mods <- MplusAutomation::readModels(target = "Mplus/Scale Scores/LGM")
lgm.ss.fit <- map_dfr(lgm.ss.mods[1:11], ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_ss_lgm.out", recodes = mplusoutcomes[1:11])


lgm.ss.est <- map_dfr(lgm.ss.mods[1:11], ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized"), .id = "outcome") %>%
  outcome_shortcut(remove = "_ss_lgm.out", recodes = mplusoutcomes[1:11]) %>%
  select(outcome, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(-S.WITH_I, S.WITH_I)

#### Compiling results objects into one object ####
lgm.ss.results <- list(Models = lgm.ss.mods[1:11],
                       Fit = lgm.ss.fit,
                       Estimates = lgm.ss.est,
                       R2 = map_dfr(.x = lgm.ss.mods, ~.x$parameters$r2, .id = "outcome") %>%
                         outcome_shortcut(remove = "_ss_lgm.out", recodes = mplusoutcomes[1:11]),
                       Warn_Error = map_dfr(.x = lgm.ss.mods[1:11],
                                            ~data.frame(errors = length(.x$errors),
                                                        warnings = length(.x$warnings)),
                                            .id = "outcome") %>%
                         outcome_shortcut(remove = "_ss_lgm.out", recodes = mplusoutcomes[1:11]))

## Outcomes are labelled incorrectly
# test.lgm.ss.results <- format_mplus(lgm.ss.mods, recodes = lgm.preds, outcomes = mplusoutcomes[-12],
#                                     std = "unstandardized", reg = FALSE, r2 = TRUE)

######################################################################################

# ------  Tx Mplus Results -------------------

## Importing models
tx.ss.mods <- MplusAutomation::readModels(target = "Mplus/Scale Scores/TX")
tx.only.mods <- which(str_detect(names(tx.ss.mods[1:22]), "_only"))
tx.covs.mods <- which(str_detect(names(tx.ss.mods[1:22]), "_covs"))

########## Tx Only ###########

tx.only.preds <- c(I = "Intercept", S = "Intercept", TX = "Sources")

## Model Fit
tx.only.ss.fit <- map_dfr(tx.ss.mods[tx.only.mods], ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_ss_tx_only.out", recodes = mplusoutcomes[1:11])

## Fixed effect estimates
tx.only.ss.est <- map_dfr(.x = tx.ss.mods[tx.only.mods],
                      ~mplus_est(.x, std = "stdy.standardized", params = c("ON", "Intercepts"),
                                 digits = 2, combine = TRUE, ci = FALSE) %>%
  filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
  mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
         predictor = recode(param, !!!tx.only.preds) %>%
           factor(., levels = unique(tx.only.preds)),
         temp = paste(predictor, is, sep = ".")) %>%  # specific to latent growth models where I and S recoded to same value
         select(temp, estimate) %>%
    spread(temp, estimate), .id = "outcome") %>%
  outcome_shortcut(remove = "_ss_tx_only.out", recodes = mplusoutcomes[1:11]) %>%
  select(outcome, Sources.I, Sources.S)

########## Tx With Covariates ###########

tx.covs.preds <- c(I = "Intercept", S = "Intercept", TX = "Sources",
                   FEMALE = "Female", OTHERG = "Other Gender",
                   LATINX = "Latinx", POC = "POC", LGBQ = "LGBQ",
                   FGXTX = "Female:Sources", OGXTX = "OG:Sources",
                   LRXTX = "Latinx:Sources", POCXTX = "POC:Sources", LGBQXTX = "LGBQ:Sources",
                   RURAL = "Rural", TEAS_PER = "School Teasing", MHT_PER = "School MH Training")

## Model fit
# all models in single dataframe
tx.covs.ss.fit <- map_dfr(tx.ss.mods[tx.covs.mods], ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_ss_tx_covs.out", recodes = mplusoutcomes[1:11])

# in a list for joining with estimates
tx.covs.ss.fitlong <- map(tx.ss.mods[tx.covs.mods],
                          ~mplus_fit(.x, digits = 2) %>%
                            mutate(x2_33 = case_when(pvalue < .01 ~ paste0(chisq, "**"),
                                                     pvalue < .05 ~ paste0(chisq, "*"),
                                                     TRUE ~ chisq),
                                   RMSEA = paste(rmsea, rmsea.ci)) %>%
                            select(x2_33, CFI = cfi, RMSEA, SRMR = srmr) %>%
                            gather(predictor, I))


## Fixed effect estimates
tx.covs.ss.est <- map(.x = tx.ss.mods[tx.covs.mods],
                          ~mplus_est(.x, std = "stdy.standardized", params = c("ON", "Intercepts"), #"stdy.standardized"
                                     digits = 2, combine = TRUE, ci = FALSE) %>%
                            filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
                            mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
                                   predictor = recode(param, !!!tx.covs.preds) %>%
                                     factor(., levels = unique(tx.covs.preds))) %>%
                            select(predictor, is, estimate) %>%
                            spread(is, estimate))

## Random effect estimates and R2
tx.covs.ss.re <- map(.x = tx.ss.mods[tx.covs.mods],
                     ~mplus_est(.x, std = "unstandardized", params = "Residual.Variances",
                                digits = 2, combine = TRUE, ci = FALSE) %>%
                       bind_rows(.x$parameters$r2 %>%
                                   mutate(estimate = format(round(est, 2), nsmall = 2))) %>%
                       filter(param %in% c("I", "S")) %>%
                       mutate(predictor = c(rep("Variance", 2), rep("R2", 2)) %>% as_factor()) %>%
                       select(predictor, param, estimate) %>%
                       spread(param, estimate))


## Binding all results into a single dataframe for each model
tx.covs.ss.results <- map2(.x = tx.covs.ss.est, .y = tx.covs.ss.re,
                           ~bind_rows(.x, .y)) %>%
  map2(.x = ., .y = tx.covs.ss.fitlong,
       ~bind_rows(.x, .y))


## Adjusting names and order of results
names(tx.covs.ss.results) <- names(tx.covs.ss.results) %>% str_remove(., "_ss_tx.covs.out") %>% recode(., !!!mplusoutcomes[1:11])
tx.covs.ss.results <- tx.covs.ss.results[mplusoutcomes[1:11]]


#### Gathering estimates for dichotomized teacher nominations ####
ta.results <- map_dfr(.x = list(lgm.ss.mods$ta_ss_lgm.out, tx.ss.mods$ta_ss_tx_only.out, tx.ss.mods$ta_ss_tx_noint.out),
                      ~mplus_est(.x, std = "unstandardized", params = c("Means", "Variances", "ON", "Intercept")) %>%
                        filter(str_detect(paramHeader, "ON") | param %in% c("I", "S")) %>%
                        mutate(OR = format(round(exp(est), 2), nsmall = 2)) %>%
                        select(parameter, estimate, OR), .id = "Model") %>%
  separate(parameter, into = c("temp1", "temp2"), sep = "_") %>%
  mutate(temp1 = str_remove(temp1, ".ON"),
         is = ifelse(temp2 %in% c("I", "S"), temp2, temp1),
         parameter = ifelse(!(temp2 %in% c("I", "S")), temp2, temp1) %>%
           str_remove(., "Residual.") %>%
           str_replace(., "Means", "Intercepts")) %>%
  select(-temp1, -temp2) %>%
  gather(temp, value, estimate, OR) %>%
  unite(temp1, Model, is, temp) %>%
  spread(temp1, value) %>%
  mutate(parameter = recode(parameter, !!!tx.covs.preds) %>%
           factor(., levels = c("Intercepts", tx.covs.preds[3:8], "Variances"))) %>%
  arrange(parameter)


#############################################################


# ------  Exposure Mplus Results -------------------
## Note. This section could use some updating if we are actually using results

exp.only.preds <- rep(c("Active", "Passive"), each = 3)
names(exp.only.preds) <- paste(rep(c("AEXS", "PEXS"), each = 3), 2:4, sep = "_")
exp.covs.preds <- c(exp.only.preds, c(I = "Intercept", S = "Intercept",
                  FEMALE = "Female", OTHERG = "Other Gender",
                  LATINX = "Latinx", POC = "POC", LGBQ = "LGBQ",
                  RURAL = "Rural", TEAS_PER = "School Teasing", MHT_PER = "School MH Training"))

mplusoutcomes2 = c(NCP = "SH Perpetration", NCV = "SH Victimization", CP = "FSC Perpetration", CV = "FCS Victimization", 
                  HOP = "HNC Perpetration", HOV = "HNC Victimization", CYP = "Cybersex Perpetration", DSVS = "SV Dismissiveness",
                  GWB = "General Well-Being", SHA = "SH Attitudes", SHI = "SH Help Intent")

## Importing models and converting results to tables
exp.ss.mods <- MplusAutomation::readModels(target = "Mplus/Scale Scores/Exposure")
exp.only.mods <- which(str_detect(names(exp.ss.mods), "_only"))
exp.covs.mods <- which(str_detect(names(exp.ss.mods), "_covs"))

exp.only.ss.fit <- map_dfr(exp.ss.mods[exp.only.mods], ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_ss_exp_only.out", recodes = mplusoutcomes[-12])

exp.covs.ss.fit <- map_dfr(exp.ss.mods[exp.covs.mods], ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_ss_exp_covs.out", recodes = mplusoutcomes[-12])

exp.only.ss.est <- map_dfr(.x = exp.ss.mods[exp.only.mods],
                          ~mplus_est(.x, std = "stdy.standardized", params = c("ON", "Intercepts"),
                                     digits = 2, combine = TRUE, ci = FALSE) %>%
                            filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
                            mutate(Wave = str_extract(paramHeader, "[2-4]"),
                                   predictor = recode(param, !!!exp.only.preds) %>%
                                     factor(., levels = unique(exp.only.preds)),
                                   temp = paste(predictor, Wave, sep = ".")) %>% 
                            select(temp, estimate) %>%
                            spread(temp, estimate), .id = "outcome") %>%
  outcome_shortcut(remove = "_ss_exp_only.out", recodes = mplusoutcomes[-12])

exp.covs.ss.est <- map(.x = exp.ss.mods[exp.covs.mods],
                      ~mplus_est(.x, std = "stdy.standardized", params = c("ON", "Intercepts"),
                                 digits = 2, combine = TRUE, ci = FALSE) %>%
                        filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
                        mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
                               is = ifelse(str_detect(is, "[2-4]"), str_extract(is, "[2-4]"), is),
                               predictor = recode(param, !!!exp.covs.preds) %>%
                                 factor(., levels = unique(exp.covs.preds))) %>%  
                        select(predictor, is, estimate) %>%
                        spread(is, estimate))

## Adjusting names and order of results
names(exp.covs.ss.est) <- names(exp.covs.ss.est) %>% str_remove(., "_ss_exp.covs.out") %>% recode(., !!!mplusoutcomes[-12])
exp.covs.ss.est <- exp.covs.ss.est[mplusoutcomes[-12]]


##############################################################

# -----  Interaction Plots ---------------

## extracting intercept and slope estimates
intx.data <- map_dfr(list(tx.ss.mods$ncp_ss_tx_covs.out, tx.ss.mods$gwb_ss_tx_covs.out), #tx.ss.mods$dismiss_ss_tx_covs.out
                     ~.x %>%
                       mplus_est(params = c("ON", "Intercepts"), std = "unstandardized", digits = 3) %>%
                       filter(str_detect(paramHeader, "\\.ON")|(paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
                       select(parameter, est) %>%
                       spread(parameter, est) %>%
                       bind_rows(replicate(3, ., simplify = FALSE)) %>%
                       bind_cols(expand.grid(LATINX = c(1, 0), TX = c(1, 0)) %>% mutate(LRXTX = LATINX*TX), .) %>%
                       mutate(I.Latx = Intercepts_I + I.ON_LATINX*LATINX + I.ON_TX*TX + I.ON_LRXTX*LRXTX,
                              S.Latx = Intercepts_S + S.ON_LATINX*LATINX + S.ON_TX*TX + S.ON_LRXTX*LRXTX) %>%
                       select(LATINX, TX, I.Latx, S.Latx) %>%
                       gather(temp, value, -LATINX, -TX) %>%
                       separate(temp, into = c("IS", "Group"), sep = "\\.") %>%
                       spread(IS, value) %>%
                       mutate(Wave1 = I,
                              Wave2 = I + 1*S,
                              Wave3 = I + 2*S,
                              Wave4 = I + 3*S) %>%
                       gather(Wave, Estimate, starts_with("Wave")) %>%
                       mutate(Wave = str_remove(Wave, "Wave") %>% as.numeric()), .id = "Outcome") %>%
  mutate(Outcome = recode(Outcome, `1` = "SH Perpetration", `2` = "Natural Coping Resources"),
         Ethnicity = recode(LATINX, `0` = "White", `1` = "Hispanic"),
         School = recode(TX, `0` = "Waitlist", `1` = "Sources of Strength"),
         Condition = paste(Ethnicity, School, sep = " - "))



intx.plot <- intx.data %>%
  ggplot(aes(x = Wave, y = Estimate, group = Condition)) +
  geom_line(aes(linetype = School), size = 1) +
  geom_point(aes(shape = Ethnicity), size = 2) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_shape_manual(values = c(0, 2)) +
  scale_y_continuous(limits = c(-.02, 3), breaks = seq(0, 3, .5)) +
  labs(x = "Wave", y = "Estimated Scale Score", linetype = "Condition") +
  theme_bw(base_size = 20) +
  facet_wrap(~Outcome)

#############################################################

# --------  Impact of SoS on Perpetrators ----------------

library(lme4)
regvars <- ds3 %>% select(Tx, Female, OtherG, Latinx, POC, LGBQ,
                          Rural, Teasing_Per, MH_Training_Per,
                          FemalexTx, OtherGxTx, LatinxxTx, POCxTx, LGBQxTx) %>% names()

onperp.mlm <- map(.x = OutcomeNames[c(1,3,5,8)],
            ~lmer(formula(paste0(paste0(.x, "_Score_W4"), " ~ 1 + ", paste0(.x, "_Score_W1"), "*Tx + ", paste(regvars[-1], collapse = " + "), " + (1|School)")),
                  data = ds3)) %>%
  set_names(OutcomeNames[c(1, 3, 5, 8)])

onperp.lm <- map(.x = OutcomeNames[c(1,3,5,8)],
                  ~lm(formula(paste0(paste0(.x, "_Score_W4"), " ~ 1 + ", paste0(.x, "_Score_W1"), "*Tx + ", paste(regvars[-1], collapse = " + "))),
                        data = ds3)) %>%
  set_names(OutcomeNames[c(1, 3, 5, 8)])


params.mlm <- map(onperp.mlm, ~parameters::model_parameters(.x, standardize = "refit"))
perform.mlm <- map_dfr(onperp.mlm, ~performance::model_performance(.x), .id = "Outcome")
mcplots.mlm <- map(onperp.mlm, ~ModelCheckPlots(.x))

params.lm <- map(onperp.lm, ~parameters::model_parameters(.x, standardize = "refit"))
perform.lm <- map_dfr(onperp.lm, ~performance::model_performance(.x), .id = "Outcome")
mcplots.lm <- map(onperp.lm, ~ModelCheckPlots(.x))


## lm
#ncp - w1, female (d), lgbq, rural, teasing (lm only), interaction
#cp - female (d), lgbq, rural, teasing (lm only), interaction
#hnc - w1, female (d), rural
#svd - w1, female (d), rural

ncp.int.plot <- sjPlot::plot_model(onperp.mlm$No_Contact_Perpetration, type = "int")
cp.int.plot <- sjPlot::plot_model(onperp.mlm$Contact_Perpetration, type = "int")

#############################################################


# ----- Saving Latent Growth Model Objects -----------------
save(lgm.ss.results, 
     tx.ss.mods, tx.only.ss.fit, tx.only.ss.est,
     tx.covs.ss.fit, tx.covs.ss.est, tx.covs.ss.re, tx.covs.ss.fitlong,
     tx.covs.ss.results, ta.results,
     exp.ss.mods, exp.only.ss.fit, exp.covs.ss.fit, exp.only.ss.est, exp.covs.ss.est,
     intx.data, intx.plot, onperp.lm, onperp.mlm,
     # params.lm, perform.lm, mcplots.lm,
     params.mlm, perform.mlm, mcplots.mlm,
     ncp.int.plot, cp.int.plot,
     file = "Output/Mplus_SS_Results.RData")

load("Output/Mplus_SS_Results.RData")


# -------- Using a multilevel modeling approach -----------------
# library(lme4)
# library(performance)
# 
# # Note 1: MLM residual variances are assumed to be equal across time
# #         whereas they are estimated in LGM
# #         the nlme package can estimate unequal variances, but lme4 cannot
# # Note 2: MLM will listwise delete missingness at Level2 which is ~200 students,
# #         whereas in LGM the variables can be brought into the model as outcomes
# 
# ## Defining predictors and outcome variables
# int.preds <- c("Tx", "Female", "OtherG", "Latinx", "POC", "LGBQ",
#                "FGxTx", "OGxTx", "LRxTx", "POCxTx", "LGBQxTx",
#                "Rural", "Teas_Per", "MHT_Per")
# mlm.preds <- c(int.preds, paste0(int.preds, "xW"))
# 
# mlm.outcomes <- mplusoutcomes
# names(mlm.outcomes <- c("NCPS", "NCVS", "CPS", "CVS", "HOPS", "HOVS",
#                         "CYPS", "DSVS", "GWBS", "SHAS", "SHIS", "TA"))
# 
# 
# ## Pivoting to long format
# mpluslong <- mpluswide %>%
#   select(ID:LGBQxTx, ends_with("Avg"), Funding:MHT_Per,
#          NCPS_1:SHIS_4, starts_with("TotAN"), starts_with("TA")) %>%
#   # rename_with(~str_remove(., "_"), .cols = starts_with("srvy")|starts_with("TotAN")|starts_with("TA")) %>%
#   gather(Temp, Score, dplyr::matches("[0-9]$")) %>%
#   separate(Temp, into = c("Scale", "Wave"), sep = "_") %>%
#   spread(Scale, Score) %>%
#   mutate(Wave = as.numeric(Wave) - 1) %>%
#   mutate(across(.cols = all_of(mlm.preds), ~. * Wave, .names = "{col}xW"))
# 
# 
# unc <- lmer(as.formula(paste0(mlm.outcomes[[1]], " ~ 1 + Wave + factor(School) + (1 + Wave|ID)")), data = mpluslong, REML = FALSE)
# summary(unc)
# model_performance(unc)
# 
# ## Produces singular model when random effect for school is included; could include fixed effects for school and remove school-level predictors
# cond <- lmerTest::lmer(as.formula(paste0(mlm.outcomes[[1]], " ~ 1 + Wave + ", paste(mlm.preds[-c(12:14, 26:28)], collapse = " + "),
#                                          " + factor(School) + (1 + Wave|ID)")),
#              data = mpluslong, REML = FALSE)
# summary(cond)
# 
# ## including random effect for wave produced singular model
# ta0 <- glmer(as.formula(paste0(mlm.outcomes[[12]], " ~ 1 + Wave + (1|ID)")),
#              data = mpluslong, family = binomial(link = "logit"))
# 
# ta.tx <- glmer(as.formula(paste0(mlm.outcomes[[12]], " ~ 1 + Wave + Tx + TxxW + (1|ID)")),
#               data = mpluslong, family = binomial(link = "logit"))
# 
# # failed to converge with all predictors
# tictoc::tic()
# ta1 <- glmer(as.formula(paste0(mlm.outcomes[[12]], " ~ 1 + Wave + ", paste(mlm.preds[-c(12:14, 26:28)], collapse = " + "), " + (1|ID)")),
#              data = mpluslong, family = binomial(link = "logit"))
# tictoc::toc()
# 
# ta2 <- glmer(as.formula(paste0(mlm.outcomes[[12]], " ~ 1 + Wave + ", paste(mlm.preds[-c(12:14, 26:28)], collapse = " + "),
#                                         " + factor(School) + (1 +|ID)")),
#                       data = mpluslong, family = binomial(link = "logit"))
# summary(ta.tx)
# model_performance(ta0)
# r2(ta0)


#######################################################
