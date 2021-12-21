########################################
#                                      #
#         Sources of Strength          #
#    School Level Data Preparation     #
#                                      #
########################################

# To Do:
  # - Can any of the school climate scales show decent fit?
  # - Aggregate said scales to school level and combine with schdemos
  # - Join school level info with dsps
  # - In dsps, aggregate student info to school level
  # - Re-run PS models with school variables and time invariant student variables
  # - try entropy balancing or other model types

## Loading packages, functions, and data
source("Scripts/PnF.R")
source("Scripts/efa_cfa_syntax.R")

# ----- Importing data ------------------

#### school climate ####
schclimate.orig <- read_sav("../00 School Climate W1-W4 cleaned/SPSS/sources_school_climate_w1_2021-1-27.sav")

## Recoding Don't Knows to NA and other cleanup
schclimate <- schclimate.orig %>%
  rename_with(~str_remove(.x, "_W1")) %>%
  mutate(School = as_factor(RECODED_SCHOOLS)) %>%
  mutate(across(where(is.labelled), labelled::to_factor)) %>%
  mutate(across(where(is.factor), ~fct_recode(., NULL = "Don't Know", NULL = "Don't know")))

## School demographics
schdemos <- readxl::read_excel("SoS Data and Codes/RSVP2 School Stats.xlsx", sheet = "RReady")

# transforming variables for use in the propensity score model
schdemos <- schdemos %>%
  mutate(across(.cols = ends_with("_Per"), ~.x*100)) %>%  # scale percentages to 0-100
  mutate(Rural = ifelse(Rural_Urban == "Rural", 1, 0),
         Funding100 = Funding_Per_Pupil / 100,
         School_N10 = School_N / 10)


# ----- Initial Scales  -------

## Item Frequencies
itemfreqs <- map(.x = SchoolScales,
                 ~schclimate %>% select(all_of(.x)) %>%
                   mutate(across(.fns = as.numeric)) %>%
                   Get_ItemFreqs, NAto0 = TRUE)

#### Collapsing Categories ####
schclimate <- schclimate %>%
  mutate(across(.cols = all_of(SchoolScales$You_Intervene), ~fct_recode(.x, Unlikely = "Very Unlikely"))) %>%
  mutate(across(.cols = all_of(SchoolScales$Positive_School_Climate), ~fct_recode(.x, Disagree = "Strongly Disagree")))

#### Confirmatory Factor Analysis ####
# NEED TO CONFIRM CFA_WRAPPER STILL WORKS HERE
schscorecfa <- map2(.x = SchoolScales, .y = c("StuInv", "StaffInv", "YouInv",
                                              "SchBully", "SchSH", "SchMH",
                                              "SchAggr", "PosSchCli"),
                    ~cfa_wrapper(data = schclimate, items = .x, fname = .y,
                                 waves = NULL))

lapply(schscorecfa, "[[", "reliabilities")
lapply(schscorecfa, "[[", "fits")  # None of these are great
lapply(schscorecfa, "[[", "loadings")


possch.mod <- paste0("PosSchCli =~ ",  paste(SchoolScales$Positive_School_Climate, collapse = " + "))

possch <- cfa(model = possch.mod,
            data = schclimate,
            ordered = SchoolScales$Positive_School_Climate,
            missing = "pairwise")

summary(possch, fit.measures = TRUE)
modindices(possch, sort. = TRUE)


youint.mod <- paste0("You =~ ",  paste(SchoolScales$You_Intervene[-c(2,5)], collapse = " + "))

youint <- cfa(model = youint.mod,
              data = schclimate,
              ordered = SchoolScales$You_Intervene,
              missing = "pairwise")
round(Get_lavaan_fits(youint), 3)

staffint.mod <- paste0("Staff =~ ",  paste(SchoolScales$Staff_Intervene[-c(3)], collapse = " + "))

staffint <- cfa(model = staffint.mod,
              data = schclimate,
              ordered = SchoolScales$Staff_Intervene,
              missing = "pairwise")
round(Get_lavaan_fits(staffint), 3)
summary(staffint, fit.measures = TRUE)

mhcommit.mod <- paste0("MH =~ ",  paste(c(SchoolScales$School_Commitment_to_Mental_Health, "SCHOOL_COMMITMENT_OTHER_1", "SCHOOL_COMMITMENT_OTHER_2"), collapse = " + "))
mhcommit <- cfa(model = mhcommit.mod,
                data = schclimate,
                ordered = c(SchoolScales$School_Commitment_to_Mental_Health, "SCHOOL_COMMITMENT_OTHER_1", "SCHOOL_COMMITMENT_OTHER_2"),
                missing = "pairwise")
round(Get_lavaan_fits(mhcommit), 3)
summary(mhcommit, fit.measures = TRUE, standardized = TRUE)

############################################################


##########################################################
####  Exploratory and Confirmatory Factor Analysis   #####


# ---- Samples for Factor Exploration ------------

## creating efa and cfa samples by randomly selecting rows in wide format data
set.seed(210223)

## EFA
efa.sample <- schclimate %>%
  group_by(School) %>%
  sample_frac(size = .5, replace = FALSE)
## CFA
cfa.sample <- schclimate[!(schclimate$SUBJECT_ID %in% efa.sample$SUBJECT_ID), ]

# ------- Intervene ------------------------------

## Sample, polychoric correlations, consensus number of factors
intervene <- efa_cfa_prelim(efa.sample, unlist(SchoolScales[1:3]), Wave = NULL)
summary(intervene$nf)
round(intervene$cor,2)
# Staff4 and 5, and you 4 and 5 highly correlated


#### Running and comparing EFA models ####
intervene.efa <- vector("list", length = 3)

for(nf in 1:3){
  unrotated <- semTools::efaUnrotate(data = intervene$sample[,unlist(SchoolScales[1:3])],
                                     varList = unlist(SchoolScales[1:3]),
                                     nf = nf,
                                     start = FALSE,
                                     ordered = unlist(SchoolScales[1:3]),
                                     missing = "pairwise",
                                     parameterization = "theta")
  intervene.efa[[nf]] <- unrotated
}

intervene.efa[[4]] <- semTools::efaUnrotate(data = intervene$sample[,unlist(SchoolScales[1:3])],
                                   varList = unlist(SchoolScales[1:3]),
                                   nf = 4,
                                   start = FALSE,
                                   ordered = unlist(SchoolScales[1:3]),
                                   missing = "pairwise",
                                   parameterization = "theta")

lavTestLRT(intervene.efa[[3]], intervene.efa[[4]]) # 3 factor fits better than 2; not sure what 4 factor is doing

#### Extracting Factor Loadings ####
## Rotating 2 factor model loadings
intervene.rot <- lapply(intervene.efa[2:4], get_std_loadings, type = "std.all")
intervene.rot <- lapply(intervene.rot, GPArotation::GPFoblq, method = "oblimin")
# Loadings show Item 7 loading on its own

intervene.efa.loadings <- list(get_std_loadings(intervene.efa[[1]]),
                         intervene.rot[[1]]$loadings, intervene.rot[[2]]$loadings, intervene.rot[[3]]$loadings)

## converting EFA results to CFA syntax
intervene.syntax <- lapply(intervene.efa.loadings, efa_cfa_syntax, single.item = 'drop')

#### Running and comparing CFA models ####

intervene.cfa <- map(intervene.syntax,
                      ~cfa(mod = .x,
                               data = cfa.sample,
                               ordered = unlist(SchoolScales[1:3]),
                               missing = "pairwise")) %>%
  cfa_wrapper(mods = .)




## CONCLUSION: For each topic, Items 2 and 3 seem to function differently than the others; these are also the less severe items
## Overall, nothing is a good fit though


# ------- Commitment ------------------------------

## Sample, polychoric correlations, consensus number of factors
commitment <- efa_cfa_prelim(efa.sample, c(unlist(SchoolScales[4:6]), "SCHOOL_COMMITMENT_OTHER_1", "SCHOOL_COMMITMENT_OTHER_2"), Wave = NULL)
summary(commitment$nf)
round(commitment$cor,2)


#### Running and comparing EFA models ####
commitment.efa <- vector("list", length = 3)

for(nf in 1:3){
  unrotated <- semTools::efaUnrotate(data = commitment$sample[,c(unlist(SchoolScales[4:6]), "SCHOOL_COMMITMENT_OTHER_1", "SCHOOL_COMMITMENT_OTHER_2")],
                                     varList = c(unlist(SchoolScales[4:6]),"SCHOOL_COMMITMENT_OTHER_1", "SCHOOL_COMMITMENT_OTHER_2"),
                                     nf = nf,
                                     start = FALSE,
                                     ordered = c(unlist(SchoolScales[4:6]),"SCHOOL_COMMITMENT_OTHER_1", "SCHOOL_COMMITMENT_OTHER_2"),
                                     missing = "pairwise",
                                     parameterization = "theta")
  commitment.efa[[nf]] <- unrotated
}

lavTestLRT(commitment.efa[[1]], commitment.efa[[2]]) # 2 factor fits better than 1
lavTestLRT(commitment.efa[[2]], commitment.efa[[3]]) # 3 factor fits better than 2

#### Extracting Factor Loadings ####
## Rotating 2 factor model loadings
commitment.rot <- lapply(commitment.efa[2:3], get_std_loadings, type = "std.all")
commitment.rot <- lapply(commitment.rot, GPArotation::GPFoblq, method = "oblimin")
# Loadings show Item 7 loading on its own

commitment.efa.loadings <- list(get_std_loadings(commitment.efa[[1]]),
                               commitment.rot[[1]]$loadings, commitment.rot[[2]]$loadings)

## converting EFA results to CFA syntax
commitment.syntax <- lapply(commitment.efa.loadings, efa_cfa_syntax, single.item = 'drop')
# second factor in 3 factor model contains no unique items

cat(commitment.syntax[[3]])

#### Running and comparing CFA models ####

commitment.cfa <- map(commitment.syntax[1:2],
                     ~cfa(mod = .x,
                          data = cfa.sample,
                          ordered = c(unlist(SchoolScales[4:6]),"SCHOOL_COMMITMENT_OTHER_1", "SCHOOL_COMMITMENT_OTHER_2"),
                          missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

# CONCLUSION: Mental Health appears to be a separate factor (with the other items) from Bullying/SH, but RMSEA is high for all models


#################################################################

#### Calculating scale scores ####
school.scores <- map_dfc(.x = SchoolScales,
                         ~scale_score(dsch, .x, type = "mean", min.valid = 3)) # must answer at least 3 items to get a score


#### Comparing to scores already in the file ####
schscorecomp <- dsch %>%
  select(SUBJECT_ID, School, all_of(names(SchoolScales)), ends_with("SCALE"))

schscorecor <- map2_dfr(.x = names(SchoolScales)[c(1:3,7,4:6,8)],
                        .y = select(schscorecomp, ends_with("SCALE")) %>% names(),
                        ~data.frame(Mine = .x, Orig = .y,
                                    cor = cor(schscorecomp[[.x]], schscorecomp[[.y]], use = "pairwise.complete.obs")))
# Only aggression has differences (r = .98)
schscorecomp %>%
  ggplot(aes(x = School_Aggression_Problems, y = AGGRESSION_PROBLEM_AT_SCHOOL_SCALE)) +
  geom_point() + geom_smooth(method = "lm")


#### Aggregating to school-level ####
dsch.agg <- dsch %>%
  select(SUBJECT_ID, School, all_of(names(SchoolScales))) %>%
  gather(Scale, Value, -SUBJECT_ID, -School) %>%
  group_by(School, Scale) %>%
  summarize(Mean = mean(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE),
            n = sum(!is.na(Value)))