########################################
#                                      #
#         Sources of Strength          #
#   Propensity Scores for Baseline     #
#          Measurement Models          #
#                                      #
########################################

## Notes:
# These analyses were run using the dscombo data prior to the minor fixes that were rectified in the dsclean data

## Loading packages, functions, and data
source("Scripts/PnF.R")
load("Output/SoS_Data.RData")

# ----- Importing data ------------------

#### student-level ####
## Gathering student-level variables for propensity score analysis
dsps <- dsclean %>%
  filter_at(vars(starts_with("Inaccurate_Response")),              # removing inaccurate response cases (4 cases)
            all_vars(is.na(.) | . == 2)) %>%
  filter(rowSums(select(., HasASurvey_W1:HasASurvey_W4)) != 0) %>% # Removing students with NA on Tx indicator for all waves (1196 cases)
  filter(!is.na(CONDITION_W1)) %>%                                # removing additional students without W1 data (additional 1622 cases)
  mutate(Skip_School = ifelse(SKIP_SCHOOL_1_W1 == 0, 0, 1),
         Suicide_Attempts = ifelse(SUICIDE_ATTEMPTS_W1 == 0, 0, 1)) %>%
  select(StudentID, Skip_School, Suicide_Attempts, ends_with("_W1")) %>%
  rename_with(~str_remove(., "_W1")) %>%                         # removing W1 suffix
  select(StudentID, indegree, outdegree, betweenness, coreness,  # keep non-scale W1 variables
         egodenout, TotAdultNoms, reciprate,  Skip_School,
         SUICIDAL_IDEATION_1, SUICIDAL_IDEATION_PLAN_1, Suicide_Attempts) %>%
  left_join(ds2 %>% select(StudentID, School, Tx, Grade:LGBQ, ends_with("Score_W1")) %>% # adding demographics and W1 scale scores
              rename_with(~str_remove(., "_W1")),  # removing W1 suffix
            by = "StudentID")

## Score variable shortcut - used in a few places
# not using ends_with in order to remove Exposure variables and put scores in order
ScoreNames <- dsps %>% 
  select(paste0(c(OutcomeNames, ProtectiveNames[c(1,3,4)], OtherScaleNames), "_Score")) %>% names()

ScoreNamesRecode <- c("SH Perpetration", "SH Victimization", "FSC Perpetration", "FSC Victimization",
                      "HNC Perpetration", "HNC Victimization", "Cybersex Perpetration", "SH Dismissiveness",
                      "General Well-being", "SH Help Attitudes", "SH Help Seeking", "Alcohol and Drugs",
                      "Bullying", "Cyberbullying Perpetration", "Depression.Anxiety", "Peer Victimization")
names(ScoreNamesRecode) <- ScoreNames

#### School-level ####

## School demographics from 2018-2019 (Year 2)
schdemos <- readxl::read_excel("SoS Data and Codes/RSVP2 School Stats.xlsx", sheet = "RReady") %>%
  mutate(Rural = ifelse(Rural_Urban == "Urban", 0, 1),
         Funding100 = Funding_Per_Pupil / 100,
         SchN100 = School_N/100,
         SchN = scale(SchN100, center = TRUE, scale = FALSE)[,1]) # centered

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
                                 TRUE ~ 0)) %>%
  group_by(School) %>%
  summarize(Teasing_Per = mean(Teasing, na.rm = TRUE),           # % of school staff reporting teasing was pretty big or huge problem
            MH_Training_Per = mean(MH_Training, na.rm = TRUE))  # % of school staff reporting fair amount or a lot of mental health training

##########################################################

#################################################
####       DIF Analysis by Tx at Baseline    ####
# detach("package:WBdif", unload=TRUE)
library(WBdif)

# table(ds2$HasASurvey_W1)

# ---- Primary Outcomes -------------------------
## Note: The baseline standardized mean differences here will account for clustering whereas
## those in the Baseline Equivalence section do not

####   DIF Data Prep    ####
primary.prep <- map(.x = OutcomeVars,
                    ~dif_data_prep(item.data = ds2[ds2$HasASurvey_W1 == 1, paste0(.x, "_W1")],
                                   dif.group.id = as_factor(ds2[ds2$HasASurvey_W1 == 1, ]$Tx) %>% fct_recode(Waitlist = "0", SoS = "1"),
                                   std.group = NULL,
                                   cluster.id = ds2[ds2$HasASurvey_W1 == 1, ]$School,
                                   na.to.0 = FALSE)) %>%
  set_names(OutcomeNames)


####    No Contact Perpetration   ####
lapply(paste0(OutcomeVars$No_Contact_Perpetration, "_W1"), function(x) table(primary.prep$No_Contact_Perpetration$tx.group.id, primary.prep$No_Contact_Perpetration$item.data[[x]]))
ncperp.analysis <- dif_analysis(primary.prep$No_Contact_Perpetration, dif.methods = c("IRT"))
ncperp.difmod <- dif_models(dif.analysis = ncperp.analysis, biased.items = c(1)) # arbitrary item
ncperp.effect <- effect_robustness(ncperp.difmod)
dif_effect_report(ncperp.analysis, ncperp.difmod, ncperp.effect,
                  file.name = "NoContactPerp_Tx_Effects",
                  report.title = "No Contact Perpetration Baseline DIF by Treatment Group",
                  measure.name = "No Contact Perpetration")


####    No Contact Victimization   ####
lapply(paste0(OutcomeVars$No_Contact_Victimization, "_W1"), function(x) table(primary.prep$No_Contact_Victimization$tx.group.id, primary.prep$No_Contact_Victimization$item.data[[x]]))
ncvict.analysis <- dif_analysis(primary.prep$No_Contact_Victimization, dif.methods = c("IRT"))
ncvict.difmod <- dif_models(dif.analysis = ncvict.analysis, biased.items = c(1))
ncvict.effect <- effect_robustness(ncvict.difmod)
dif_effect_report(ncvict.analysis, ncvict.difmod, ncvict.effect,
                  file.name = "NoContactVict_Tx_Effects",
                  report.title = "No Contact Victimization Baseline DIF by Treatment Group",
                  measure.name = "No Contact Victimization")

####    Contact Perpetration   ####
lapply(paste0(OutcomeVars$Contact_Perpetration, "_W1"), function(x) table(primary.prep$Contact_Perpetration$tx.group.id, primary.prep$Contact_Perpetration$item.data[[x]]))
conperp.analysis <- dif_analysis(primary.prep$Contact_Perpetration, dif.methods = c("IRT"))
conperp.difmod <- dif_models(dif.analysis = conperp.analysis, biased.items = c(6, 9)) # possibly biased items
conperp.effect <- effect_robustness(conperp.difmod)
dif_effect_report(conperp.analysis, conperp.difmod, conperp.effect,
                  file.name = "ContactPerp_Tx_Effects",
                  report.title = "Contact Perpetration Baseline DIF by Treatment Group",
                  measure.name = "Contact Perpetration")


####    Contact Victimization   ####
lapply(paste0(OutcomeVars$Contact_Victimization, "_W1"), function(x) table(primary.prep$Contact_Victimization$tx.group.id, primary.prep$Contact_Victimization$item.data[[x]]))
convict.analysis <- dif_analysis(primary.prep$Contact_Victimization, dif.methods = c( "IRT"))
convict.difmod <- dif_models(dif.analysis = convict.analysis, biased.items = c(1))
convict.effect <- effect_robustness(convict.difmod)
dif_effect_report(convict.analysis, convict.difmod, convict.effect,
                  file.name = "ContactVict_Tx_Effects",
                  report.title = "Contact Victimization Baseline DIF by Treatment Group",
                  measure.name = "Contact Victimization")


####    Homophobic Name-Calling Perpetration   ####
lapply(paste0(OutcomeVars$HNC_Perpetration, "_W1"), function(x) table(primary.prep$HNC_Perpetration$tx.group.id, primary.prep$HNC_Perpetration$item.data[[x]]))
hncperp.analysis <- dif_analysis(primary.prep$HNC_Perpetration, dif.methods = c("loess", "IRT"))
hncperp.difmod <- dif_models(dif.analysis = hncperp.analysis, biased.items = c(1))# "IRT"
hncperp.effect <- effect_robustness(hncperp.difmod)
dif_effect_report(hncperp.analysis, hncperp.difmod, hncperp.effect,
                  file.name = "HNCPerp_Tx_Effects",
                  report.title = "HNC Perpetration Baseline DIF by Treatment Group",
                  measure.name = "HNC Perpetration")


####    Homophobic Name-Calling Victimization   ####
lapply(paste0(OutcomeVars$HNC_Victimization, "_W1"), function(x) table(primary.prep$HNC_Victimization$tx.group.id, primary.prep$HNC_Victimization$item.data[[x]]))
hncvict.analysis <- dif_analysis(primary.prep$HNC_Victimization, dif.methods = c("loess", "IRT"))
hncvict.difmod <- dif_models(dif.analysis = hncvict.analysis, biased.items = c(1)) #"IRT" using arbitrary item
hncvict.effect <- effect_robustness(hncvict.difmod)
dif_effect_report(hncvict.analysis, hncvict.difmod, hncvict.effect,
                  file.name = "HNCVict_Tx_Effects",
                  report.title = "HNC Victimization Baseline DIF by Treatment Group",
                  measure.name = "HNC Victimization")



####    Cybersex Perpetration   ####
lapply(paste0(OutcomeVars$Cybersex_Perpetration, "_W1"), function(x) table(primary.prep$Cybersex_Perpetration$tx.group.id, primary.prep$Cybersex_Perpetration$item.data[[x]]))
cyber.analysis <- dif_analysis(primary.prep$Cybersex_Perpetration, dif.methods = c("IRT"))
cyber.difmod <- dif_models(dif.analysis = cyber.analysis, biased.items = c(1))
cyber.effect <- effect_robustness(cyber.difmod)
dif_effect_report(cyber.analysis, cyber.difmod, cyber.effect,
                  file.name = "Cybersex_Tx_Effects",
                  report.title = "Cybersex Perpetration Baseline DIF by Treatment Group",
                  measure.name = "Cybersex Perpetration")



####    SV Dismissiveness    ####
dismiss.analysis <- dif_analysis(primary.prep$SV_Dismissiveness, dif.methods = c("loess", "IRT"))
dismiss.difmod <- dif_models(dif.analysis = dismiss.analysis, biased.items = c(2)) # using possibly biased item
dismiss.effect <- effect_robustness(dismiss.difmod)
dif_effect_report(dismiss.analysis, dismiss.difmod, dismiss.effect,
              file.name = "SV_Dismiss_Tx_Effects",
              report.title = "SV Dismissiveness Baseline DIF by Treatment Group",
              measure.name = "SV Dismissiveness")

##############################################

# ---- Secondary Outcomes -------------------------

####   DIF Data Prep    ####
secondary.prep <- map(.x = ProtectiveVars,
                      ~dif_data_prep(item.data = ds2[ds2$HasASurvey_W1 == 1, paste0(.x, "_W1")],
                                     dif.group.id = as_factor(ds2[ds2$HasASurvey_W1 == 1, ]$Tx) %>% fct_recode(Waitlist = "0", SoS = "1"),
                                     std.group = NULL,
                                     cluster.id = ds2[ds2$HasASurvey_W1 == 1, ]$School,
                                     na.to.0 = FALSE)) %>%
  set_names(ProtectiveNames)

####    General Well-Being    ####
genwb.analysis <- dif_analysis(secondary.prep$General_Well.being, dif.methods = c("loess", "IRT"))
genwb.difmod <- dif_models(dif.analysis = genwb.analysis, biased.items = c(4, 8)) # using possibly biased item
genwb.effect <- effect_robustness(genwb.difmod)
dif_effect_report(genwb.analysis, genwb.difmod, genwb.effect,
                  file.name = "General_Well_Being_Tx_Effects",
                  report.title = "General Well-Being Baseline DIF by Treatment Group",
                  measure.name = "General Well-Being")

####    General Help Attitudes    ####
# helpatt.analysis <- dif_analysis(secondary.prep$General_Help_Attitudes, dif.methods = c("loess", "IRT"))
# helpatt.difmod <- dif_models(dif.analysis = helpatt.analysis, biased.items = "IRT") # using possibly biased item
# helpatt.effect <- effect_robustness(helpatt.difmod)
# dif_effect_report(helpatt.analysis, helpatt.difmod, helpatt.effect,
#                   file.name = "General_Help_Attitudes_Tx_Effects",
#                   report.title = "General Help Attitudes Baseline DIF by Treatment Group",
#                   measure.name = "General Help Attitudes")

####    SH Help Attitudes    ####
shhelpatt.analysis <- dif_analysis(secondary.prep$SH_Help_Attitudes, dif.methods = c("loess", "IRT"))
shhelpatt.difmod <- dif_models(dif.analysis = shhelpatt.analysis, biased.items = c(1)) 
shhelpatt.effect <- effect_robustness(shhelpatt.difmod)
dif_effect_report(shhelpatt.analysis, shhelpatt.difmod, shhelpatt.effect,
                  file.name = "SH_Help_Attitudes_Tx_Effects",
                  report.title = "SH Help Attitudes Baseline DIF by Treatment Group",
                  measure.name = "SH Help Attitudes")


####    SH Help Seeking    ####
shhelpseek.analysis <- dif_analysis(secondary.prep$SH_Help_Seeking, dif.methods = c("loess", "IRT"))
shhelpseek.difmod <- dif_models(dif.analysis = shhelpseek.analysis, biased.items = c(1))
shhelpseek.effect <- effect_robustness(shhelpseek.difmod)
dif_effect_report(shhelpseek.analysis, shhelpseek.difmod, shhelpseek.effect,
                  file.name = "SH_Help_Seeking_Tx_Effects",
                  report.title = "SH Help Seeking Baseline DIF by Treatment Group",
                  measure.name = "SH Help Seeking")


####    Staff Help Intent    ####
staffhelp.analysis <- dif_analysis(secondary.prep$Staff_Help_Intent, dif.methods = c("loess", "IRT"))
staffhelp.difmod <- dif_models(dif.analysis = staffhelp.analysis, biased.items = c(1)) 
staffhelp.effect <- effect_robustness(staffhelp.difmod)
dif_effect_report(staffhelp.analysis, staffhelp.difmod, staffhelp.effect,
                  file.name = "Staff_Help_Intent_Tx_Effects",
                  report.title = "Staff Help Intent Baseline DIF by Treatment Group",
                  measure.name = "Staff Help Intent")


#######################################################

##############################################################

# ---- Gathering and Comparing Student Raw Scores -------
## compare to the S_ variables created by (Alberto?, Kelly?)
scorecomp <- dsps %>%
  select(StudentID, Tx,
         starts_with("S_"), all_of(ScoreNames)) %>%
  mutate(StudentID = as.character(StudentID))
  # left_join(dsalberto %>% select(StudentID = SUBJECT_ID, starts_with("S_")),
  #           by = "StudentID")

# studentscorecor <- map2_dfr(.x = names(SchoolScales)[c(1:3,7,4:6,8)],
#                         .y = select(scorecomp, starts_with("S_")) %>% names(),
#                         ~data.frame(Mine = .x, Orig = .y,
#                                     cor = cor(scorecomp[[.x]], scorecomp[[.y]], use = "pairwise.complete.obs")))

# Largely the scores are the same; where they differ I have no clue what Kelly did; possibly different items
cor(scorecomp$S_CYBER_SEX_PERP, scorecomp$Cybersex_Perpetration_Score, use = "pairwise.complete.obs") # 1
cor(scorecomp$S_HOMO_PERP, scorecomp$HNC_Perpetration_Score, use = "pairwise.complete.obs") # 1
cor(scorecomp$S_HOMO_VICT, scorecomp$HNC_Victimization_Score, use = "pairwise.complete.obs") # 1
cor(scorecomp$S_DISMISS_SEX_VIOL, scorecomp$SV_Dismissiveness_Score, use = "pairwise.complete.obs") #.999
cor(scorecomp$S_SEX_HAR_HELP_ATTITUDE, scorecomp$SH_Help_Attitudes_Score, use = "pairwise.complete.obs") #.975; not sure how Kelly got her scores
cor(scorecomp$S_SEX_HAR_HELP_INTENT, scorecomp$SH_Help_Seeking_Score, use = "pairwise.complete.obs") # 1
cor(scorecomp$S_GEN_WELL_BEING, scorecomp$General_Well.being_Score, use = "pairwise.complete.obs") # .999; 1 observation different (200130, I'm right)
cor(scorecomp$S_STAFF_INT_TOTAL, scorecomp$Staff_Help_Intent_Score, use = "pairwise.complete.obs") # 1
cor(scorecomp$S_HELP_SEEK_GEN_BELIEF, scorecomp$General_Help_Attitudes_Score, use = "pairwise.complete.obs") # .991

scorecomp %>%
  ggplot(aes(x = S_DISMISS_SEX_VIOL, y = SV_Dismissiveness_Score)) +
  geom_point() + geom_smooth(method = "lm")


dsps %>%
  select(StudentID,all_of(paste0(DismisssvVars, "_W1")), SV_Dismissiveness_Score_W1, S_DISMISS_SEX_VIOL_W1) %>%
  mutate(diff = SV_Dismissiveness_Score_W1 - S_DISMISS_SEX_VIOL_W1) %>% View()

table(is.na(dsps$SV_Dismissiveness_Score_W1), is.na(dsps$S_DISMISS_SEX_VIOL_W1))

############################################################################


# ------ ICCs and School-level Variables ----------------------

w1scoreicc <- map_dfr(.x = ScoreNames,
                     ~lmer(as.formula(paste0(.x, " ~ 1 + (1|School)")), data = dsps) %>%
                      school_effects() %>%
                      mutate(Scale = .x)) %>%
  mutate(Scale = str_remove(Scale, "_Score"))


#### Aggregating student-level information ####
# Already have school-level info on most demographics
aggvars <- dsps %>%
  select(Straight, LGBQ, # Bisexual, Questioning, GL, OtherSO,
         indegree, outdegree, betweenness, coreness,
         egodenout, TotAdultNoms, Skip_School,      #reciprate, 
         Suicide_Attempts, SUICIDAL_IDEATION_1, SUICIDAL_IDEATION_PLAN_1, 
         all_of(paste0(OtherScaleNames, "_Score"))) %>%
  names()

## School-level descriptive statistics
schoolaggdescrips <- dsps %>%
  mutate(across(all_of(aggvars), as.numeric)) %>%
  group_by(School) %>%
  skimr::skim(all_of(aggvars))

## joining school means to school demographics
schoolagg <- schoolaggdescrips %>%
  select(skim_variable, School, numeric.mean) %>%
  spread(skim_variable, numeric.mean) %>%
  select(School, all_of(aggvars)) %>%
  mutate(across(.cols = c(Straight, LGBQ, Skip_School, Suicide_Attempts, SUICIDAL_IDEATION_1, SUICIDAL_IDEATION_PLAN_1),
                ~.x * 100)) %>%
  mutate(School = as_factor(School)) %>%
  rename_with(~paste0(.x, "_Per"), .cols = c(Straight, LGBQ, Skip_School, Suicide_Attempts, SUICIDAL_IDEATION_1, SUICIDAL_IDEATION_PLAN_1)) %>%
  rename_with(~paste0(.x, "_sch"), .cols = !ends_with("_Per")) %>%
  rename(School = School_sch) %>%
  left_join(schclimate, by = "School") %>%
  left_join(schdemos %>%
              select(School, Rural, Funding100, SchN100, ends_with("_Per")),
            by = "School") %>%
  mutate(across(.cols = -c(School, Rural), ~scale(.)[,], .names = "{col}_grand"))


## Joining school-level information to student data
dspssch <- dsps %>%
  mutate(School = as_factor(School)) %>%
  left_join(select(schoolagg, -betweenness_sch), by = "School") %>%
  mutate(grandbetweenness = scale(betweenness)[,])



#################################################################################

# ------ Baseline Equivalence -----------------------------

#### Defining Level 1 and Level 2 variables ####

## Level 1 Predictors
L1baselinevars <- dspssch %>%
  select(Female:Indigenous, Straight:OtherSO,
         indegree, outdegree, coreness, # grandbetweenness
         egodenout, TotAdultNoms, reciprate, Skip_School,
         Suicide_Attempts, SUICIDAL_IDEATION_1, SUICIDAL_IDEATION_PLAN_1,
         all_of(paste0(OtherScaleNames, "_Score"))) %>% names()

## Level 2 Predictors
L2baselinevars <- dspssch %>%
  select(Rural, Funding100, SchN100, ends_with("_Per")) %>% names() #ends_with("_sch")


#### Calculating Standardized Mean Difference ####
# Note: using WBdif::hedges2007 which accounts for clustering and missing data (via na.rm = T)
## adding Tx variable to school data
schoolagg <- schoolagg %>%
  left_join(select(ds2, School, Tx) %>% unique() %>% mutate(School = as_factor(School)),
            by = "School")

## Student (L1) differences
L1equiv <- map_dfr(c(ScoreNames[c(1:6, 8:11)], L1baselinevars),
                   ~cbind(data.frame(Variable = .x), as.data.frame(t(WBdif::hedges2007(dsps[[.x]], tx.group.id = dsps$Tx, cluster.id = dsps$School)))))

## School (L2) differences
L2equiv <- map_dfr(c(L2baselinevars),
                   ~cbind(data.frame(Variable = .x), as.data.frame(t(WBdif::hedges2007(schoolagg[[.x]], tx.group.id = schoolagg$Tx)))))

## Combined
BaseEquiv <- L1equiv %>%
  mutate(Level = "Student") %>%
  bind_rows(mutate(L2equiv, Level = "School")) %>%
  mutate(ci.low = effect.size - qnorm(.975)*effect.size.se,
         ci.high = effect.size + qnorm(.975)*effect.size.se)

## Plot
BaseEquivPlot <- BaseEquiv %>%
  mutate(effect.size = abs(effect.size),
         Color = case_when(effect.size <= .05 ~ "green",
                           effect.size > .05 & effect.size <= .25 ~ "yellow",
                           TRUE ~ "red"),
         Variable = recode(Variable, !!!ScoreNamesRecode) %>%
           str_replace_all("_", " ") %>%
           str_replace(" Per", " - % in School") %>%
           as_factor() %>% fct_reorder(effect.size)) %>%
  ggplot(aes(x = Variable, y = effect.size, group = Color)) +
  geom_hline(yintercept = .05, linetype = 1, color = "black", size = 1.5) +
  geom_hline(yintercept = .25, linetype = "dashed", color = "black", size = 1.5) +
  geom_point(aes(shape = Color, color = Color), size = 4) + #position = position_dodge(1)) +
  ylab("Standardized Mean Difference (g)") + xlab("") +
  # scale_shape_manual(values = c(19, 18, 17)) + # 15,7,3
  # scale_color_brewer(palette = "Set2") +
  scale_color_manual(values = c("#009E73", "#D55E00", "#F0E442")) +
  coord_flip() +
  theme_bw(base_size = 22) +
  theme(panel.grid.major = element_line(color = "gray87"),
        panel.grid.minor = element_line(color = "gray90"),
        panel.background = element_rect(fill = "white", color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        legend.position = "none")
# legend.justification = c(1, 0), legend.position = c(.9, .1))


#### Equivalence of Outcomes with Distributions ####
## gathering differences (g)
OutBaseDiff <- BaseEquiv %>%
  filter(Variable %in% ScoreNames) %>%
  mutate(Scale = as_factor(Variable) %>%
           recode(!!!ScoreNamesRecode))

## calculating means
OutBaseMeansLong <- dspssch %>%
  select(Tx, all_of(ScoreNames)) %>%
  gather(Scale, Score, -Tx) %>%
  group_by(Tx, Scale) %>%
  summarize(Mean = mean(Score, na.rm = TRUE),
            SD = sd(Score, na.rm = TRUE)) %>%
  mutate(Scale = recode(Scale, !!!ScoreNamesRecode) %>%
           factor(., levels = ScoreNamesRecode),
         Tx = case_when(Tx == 0 ~ "Waitlist",
                        Tx == 1 ~ "SoS"))

## creating distribution plots
OutBaseDistPlot <- dspssch %>%
  select(School, StudentID, Tx, all_of(ScoreNames)) %>%
  gather(Scale, Score, all_of(ScoreNames)) %>%
  filter(!is.na(Score) & !is.na(Tx)) %>%
  mutate(Scale = recode(Scale, !!!ScoreNamesRecode) %>% factor(., levels = ScoreNamesRecode),
         Tx = case_when(Tx == 0 ~ "Waitlist",
                        Tx == 1 ~ "SoS")) %>%
  filter(Scale %in% ScoreNamesRecode[c(1:6, 8)]) %>% # Only need certain outcomes for final paper
  ggplot(aes(x = Score)) +
  geom_density(aes(group = Tx, fill = Tx), alpha = .4) +
  geom_vline(data = filter(OutBaseMeansLong, Scale %in% ScoreNamesRecode[c(1:6, 8)]), aes(xintercept = Mean, group = Tx, color = Tx), size = 1) +
  geom_text(data = filter(OutBaseDiff, Scale %in% ScoreNamesRecode[c(1:6, 8)]), aes(x = 3, y = Inf, vjust = 1.5,
                                     label = paste0("g = ", format(round(effect.size, 2), nsmall = 2))),
            size = 5) +
  ylab("Density") +
  scale_color_discrete(guide = 'none') +
  theme_bw(base_size = 18) +
  facet_wrap(~Scale, scales = "free_y", ncol = 4) +
  theme(legend.position = "top", legend.title = element_blank())


## The DIF reports produced above suggest baseline differences for:
# Higher for SoS - SH Help Attitudes (~.15), SH Help seeking (~.11)
# Higher for Waitlist - SV Dismiss (~.15), HNC Perp (~.23), Contact Perp (~.12), No Contact Perp (~.14), No Contact Vict (~.13)

## SMD on scale scores here suggest baseline differences for:
# Higher for SoS - SH Help Attitudes (~.16), SH Help seeking (~.11)
# Higher for Waitlist - SV Dismiss (~.17), HNC Perp (~.12), No Contact Perp (~.11), No Contact Vict (~.11)

# The LGM had significant Intercept ~ Tx for:
# No contact perp & vict, contact perp & vict, HNC perp & vict, cybersex perp, SV dismiss,
# SH help attitudes & seeking, staff help intent


#### Propensity Score Model #####

## Excluding time-variant student-level data in order to retain a larger sample size
# most of these measures are included at the school-level though
psvars <- dspssch %>%
  select(Female:Indigenous, Straight,LGBQ,
         Rural, ends_with("_grand")) %>% names()


psdata <- dspssch %>%
  select(StudentID, School, Tx, all_of(psvars)) %>%
  drop_na()

psvarcors <- rquery.cormat(psdata[,psvars], type = "flatten", graph = FALSE)
## indegree, outdegree, and coreness are highly correlated at the school level; sd(schoolagg$outdegree_Sch) is highest of the 3
# as is suicide ideation and plan
# The models indicate collinearity amongst the Scores, but none of the correlations are alarmingly high
schcors <- rquery.cormat(select(schoolagg, Rural, ends_with("_grand")), type = "flatten", graph = FALSE)
schoolskim <- skimr::skim(schoolagg[-1])  

excludevars <- c("Male", "Latinx", "Straight", "Male_Per_grand", "Hispanic_Per_grand", "Straight_Per_grand", # these are the reference groups
                 "Indigenous", "Indigenous_Per_grand", "Asian_Per_grand", "Black_Per_grand", "White_Per_grand", "Islander_Per_grand", "Multiracial_Per_grand",
                 "SUICIDAL_IDEATION_PLAN_1_Per_grand", "indegree_sch_grand", "coreness_sch_grand", "egodenout_sch_grand", "betweenness_sch_grand", # these vars were co-linear
                 "GayLes_Per_grand", "Bisexual_Per_grand", "OtherO_Per_grand", #"Female_Per_grand", "FRPL_Per_grand",# high VIFs
                 "Cyberbullying_Score_sch_grand", "Peer_Victimization_Score_sch_grand")

### Can't seem to find a model that converges properly ###
psmodel <- f.build("Tx", psvars[!(psvars %in% excludevars)])
ps.mod <- glm(as.formula(psmodel), family = binomial("logit"), data = psdata)
summary(ps.mod)
car::vif(ps.mod)

## Could try CBPS

## Entropy Balancing 
## a lot more temperamental toward collinearity; errors whenever there are more than 3 school-level variables (doesn't seem to matter which ones)
# a lot faster than I expected though
tictoc::tic()
eb <- ebal::ebalance(Treatment = psdata$Tx, X = psdata[,psvars[!(psvars %in% excludevars)][c(1:9,16,17,19,20)]]) #skip_school seems to be an issue or maybe suicidal ideation or maybe just more than 3 schoollevel variables
tictoc::toc()

sos.weights <- WeightIt::get_w_from_ps(ps = fitted(ps.mod), estimand = "ATE",
                                       treat = psdata$Tx, treated = 1)
psdatawps <- psdata %>%
  mutate(logit = predict(ps.mod),
         ps = fitted(ps.mod),
         weight = sos.weights)

dspsschwps <- dspssch %>%
  left_join(select(psdatawps, StudentID, logit, ps, weight), by = "StudentID") %>%
  drop_na(weight) # removing cases with NAs


WeightBaseDiff <- bal.tab(x = f.build("Tx", baselinevars),
        data = dspsschwps, estimand = "ATE",
        continuous = "std", binary = "std", s.d.denom = "pooled",
        abs = TRUE, un = TRUE,
        disp.means = TRUE, disp.sds = TRUE, disp.v.ratio = TRUE,
        weights = dspsschwps$weight, method = "weighting")



#### Saving Objects ####
save(dsps, dspssch, w1scoreicc, ScoreNames,
     BaseEquiv, BaseEquivPlot,
     OutBaseDiff, OutBaseMeansLong, OutBaseDistPlot,
  file = "Output/PS_Results.RData")
