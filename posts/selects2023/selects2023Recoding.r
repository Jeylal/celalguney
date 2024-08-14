
```{r set up}
#| echo: false
#| warning: false
#| message: false
rm(list=ls())
library(tidyverse)
library(haven)
library(sjlabelled)
library(lme4)
library(marginaleffects)
library(modelsummary)
library(sjPlot)
library(FactoMineR)
library(questionr)
library(explor)



```


```{r recoding selects2023}
#| echo: false
#| warning: false
#| message: false
#| include: false
#| eval: false

selects2023 <- read_sav("data/2634_Selects2023_PES_Data_v1.0.sav")

selects2023 <- 
selects2023 %>% 
  rename(
    "cantonVoteRight2023" = f10000,
    "PolInterest" = f10100,
    "MostImpProblem" = f12700rec,
    "VoteDummy2023" = f11100rec,
    "VoteChoiceMain2023" = f11800main6,
    "VoteChoiceMain2023Hyp" = f11400main6,
    "VoteChoice2019" = f10300main7_combined,
    "lr" = f15200,
    "StatifsDemocracy" = f13700,
    "PartyCloseness" = f14010main6,
    "PartcitipationRateVotes" = f12500,
    "UnionDummy" = n13_030a1,
    "ProbsVotePLR" = f14400a,
    "ProbsVotePS" = f14400c,
    "ProbsVoteUDC" = f14400d,
    "ProbsVotePES" = f14400e,
    "ProbsVotePVL" = f14400f,
    "ProbsVoteLega" = f14400h,
    "ProbsVoteMCG" = f14400i,
    "probsVoteCenter" = f14400j,
    "MaritalStatus" = E11,
    "HouseholdSize" = f20500,
    "LanguageFamily" = f20221,
    "Religion" = f20760,
    "Religious" = f20900,
    "EducationLevel" = f21310,
    "EmploymentSituation" = f21400,
    "PositionatWork" = f21500_f24900,
    "Sector" = f21700_f25100,
    "ManageIncome" = f29100,
    "Language" = lingreg_sample,
    "Canton" = canton_sample,
    "TrustFedCouncil" = f12800a,
    "TrustFedAssembly" = f12800b,
    "TrustPolParties" = f12800c,
    "TrustJustice" = f12800g,
    "TrustScientists" = f12800j,
    "TrustMedia" = f12800l,
    "TrustSocialMedia" = f12800m
    
    
    
  ) %>% 
  mutate(
    lr_recoded = case_when(
      lr %in% c(0,1,2) ~ 1, #left
      lr %in% c(3, 4) ~ 2, # center-left
      lr == 5 ~ 3, # center
      lr %in% c(6,7) ~ 4,# center-right
      lr %in% c(8, 9, 10) ~ 5  # right
    ),
    socspendingDummy = if_else(f15420 %in% c(1,2), 1,
                               if_else(is.na(f15420), NA, 0)),# 1 == for reduction
    socspend = case_when(f15420 %in% c(1,2) ~ 1, # for reduction
                         f15420 == 3 ~ 2, # neither nor
                         f15420 %in% c(4,5) ~ 3), # for increase
    
    EuMembershipDummy = case_when(f15430 %in% c(1,2) ~ 1, # for eu membership
                                  f15430 %in% c(3:5) ~ 0),
    EuMembership = case_when(f15430 %in% c(1,2) ~ 1, # for integration
                             f15430 == 3 ~ 2, # neither nor
                             f15430 %in% c (4,5) ~ 3), # stay outside
    StateInterventionDummy = if_else(f15435 %in% c(1,2), 1, # for more state intervention
                                     if_else(is.na(f15435), NA, 0)),
    StateIntervention = case_when(
      f15435 %in% c(1,2) ~ 1, # for more state intervention
      f15435 %in% c(3) ~ 2, # neither nor
      f15435 %in% c(4,5) ~ 3 # for more free market competition
    ),
    
    EqualOpportunitiyCH_F = case_when(
      f15440 %in% c(1,2) ~ 1, # for equal opportunities
      f15440 == 3 ~ 2, # neither nor
      f15440 %in% c(4,5) ~ 3 # for better opportunities for the swiss
    ),
    
    Environment_vs_growth = case_when(
      
      f15470 %in% c(1,2) ~ 1, # environmental protec mor important 
      f15470 == 3 ~ 2, # neither nor
      f15470 %in% c(4,5) ~ 3 # econ growth more important 
      
    ),
    
    TaxesHighInc = case_when(
      f15480 %in% c(1,2) ~ 1, # for increase
      f15480 == 3 ~ 2, # neither nor
      f15480 %in% c(4,5) ~3 # for a reduction
    ),
    
    TaxesHighIncDummy = if_else(f15480 %in% c(1,2), 1, # for increase
                                if_else(is.na(f15480), NA, 0)),
    
    ModernityTraditions = case_when(
      f15485 %in% c(1,2) ~ 1, # for modern ch
      f15485 ==3 ~ 2, # neither nor
      f15485 %in% c(4,5) ~ 3 # defend traditions
    ),
    
    Retirement67 = case_when(
      f15811 %in% c(1,2) ~ 1, # against
      f15811 == 3 ~ 2, # neither nor
      f15811 %in% c(4,5) ~ 3 # in favor 
    ),
    
    Retirement67Dummy = if_else(f15811 %in% c(1,2), 1, # against
                                if_else(is.na(f15811), NA, 0)),
    
    MinWage4kchf = case_when(
      f15815 %in% c(1,2) ~ 1, # against
      f15815 %in% c(3,4) ~ 2 # in favor (note: there is no indifference category...)
    ),
    
    MinWage4KDummy = if_else(f15815 %in% c(3,4),1,
                             if_else(is.na(f15815), NA, 0)),
    
    VoteRightsForeignMunicip = case_when(
      f15816 %in% c(1,2) ~ 1, # against
      f15816 %in% c(3,4) ~ 2 # in favor
    ),
    
    VoteRightsForeignMunicipDummy = if_else(f15816  %in% c(3,4), 1,
                                            if_else(is.na(f15816), NA, 0)),
    
    EqualRightsHomoCouples = case_when(
      f15817 %in% c(1,2) ~ 1, # against
      f15817 %in% c(3,4) ~ 2 # in favor
    ),
    
    EqualRightsHomoCouplesDummy = if_else(f15817 %in% c(3,4), 1,
                                          if_else(is.na(f15817), NA, 0)),
    
    SelfEmployedDummy = if_else(PositionatWork == 4, 1,
                                if_else(is.na(PositionatWork), NA, 0)),
    
    StrongLeader = case_when(
      Q04c %in% c(1,2) ~ 1, # disagree
      Q04c %in% c(3) ~ 2, # neither nor
      Q04c %in% c(4,5) ~ 3 # agree
    ),
    
    Income = case_when(
      
      f28910 == 1 ~ 2000,
      f28910 == 2 ~ (3000+2001)/2,
      f28910 == 3 ~ (4000+3001)/2,
      f28910 == 4 ~ (5000+4001)/2,
      f28910 == 5 ~ (6000+5001)/2,
      f28910 == 6 ~ (7000+6001)/2,
      f28910 == 7 ~ (8000+7001)/2,
      f28910 == 8 ~ (9000+8001)/2,
      f28910 == 9 ~ (10000+9001)/2,
      f28910 == 10 ~ (11000+10001)/2,
      f28910 == 11 ~ (12000+11001)/2,
      f28910 == 12 ~ (13000+12001)/2,
      f28910 == 13 ~ (14000+13001)/2,
      f28910 == 14 ~ (15000+14001)/2,
      f28910 == 15 ~ (16000+15001)/2,
      f28910 == 16 ~ (17000+16001)/2,
      f28910 == 17 ~ (18000+17001)/2,
      f28910 == 18 ~ (19000+18001)/2,
      f28910 == 19 ~ (20000+19001)/2,
      f28910 == 20 ~ 20000
      
    ),
    
    IncomeAdjusted = Income/hhsize_sample,
    IncomeAdjustedDecile = ntile(IncomeAdjusted, 10),
    IncomeDecile = ntile(Income, 10),
    
    HouseOwnership = if_else(f21200 == 2, 1,
                             if_else(is.na(f21200), NA, 0)),
    
    voteChoice2023Combined = case_when(
      VoteDummy2023 == 0 ~ VoteChoiceMain2023Hyp, # combine vote choice and hypothetical vote choice of non-voters
      VoteDummy2023 == 1 ~ VoteChoiceMain2023
    ),
    
    education_recoded = case_when(
      
      EducationLevel %in% c(1:4) ~ 1, # primary 
      EducationLevel %in% c(5:9) ~ 2, # secondary
      EducationLevel %in% c(10:13) ~ 3, # tertiary
      EducationLevel == 14 ~ NA # "other" ==> NA
      
    ),
    
    VotePRD = if_else(voteChoice2023Combined == 10, 1,
                      if_else(is.na(voteChoice2023Combined), NA, 0)),
    
    VoteCenter = if_else(voteChoice2023Combined == 21, 1, 
                         if_else(is.na(voteChoice2023Combined), NA, 0)),
    
    VoteSP = if_else(voteChoice2023Combined == 30, 1,
                     if_else(is.na(voteChoice2023Combined), NA, 0)),
    
    VoteSVP = if_else(voteChoice2023Combined == 40, 1,
                      if_else(is.na(voteChoice2023Combined), NA, 0)),
    
    VotePES = if_else(voteChoice2023Combined == 50, 1,
                      if_else(is.na(voteChoice2023Combined), NA, 0)),
    
    VotePLS = if_else(voteChoice2023Combined == 60, 1,
                      if_else(is.na(voteChoice2023Combined), NA, 0))
    
  )


selects2023$age_cat <- cut(selects2023$age, c(18, 30, 45, 65, 93), right = FALSE, include.lowest = TRUE)

selects2023$lr_recoded <- labelled(selects2023$lr_recoded, labels = c("left" = 1,
                                                                      "center-left" = 2,
                                                                      "center" = 3,
                                                                      "center-right" = 4,
                                                                      "right" = 5))

selects2023$education_recoded <- labelled(selects2023$education_recoded, labels = c("Primary" = 1, "Secondary" = 2, "Tertiary" = 3))

selects2023$socspend <- labelled(selects2023$socspend, labels = c("for reduction" = 1, "neither nor" = 2, "for increase" = 3))

selects2023$EuMembership <- labelled(selects2023$EuMembership, labels = c("for EU integration" = 1, "neither nor" = 2, "against EU integration" = 3))

selects2023$StateIntervention <- labelled(selects2023$StateIntervention, labels = c("more state intervention" = 1, "neither nor" = 2, "more free market competition" = 3))

selects2023$EqualOpportunitiyCH_F <- labelled(selects2023$EqualOpportunitiyCH_F, labels = c("equal opportunities" = 1, "neither nor" = 2, "better opp for the Swiss" = 3))

selects2023$Environment_vs_growth <- labelled(selects2023$Environment_vs_growth, labels = c("env protection more important" = 1, "neither nor" = 2, "econ growth more important" = 3))

selects2023$TaxesHighInc <- labelled(selects2023$TaxesHighInc, labels = c("for increase" = 1, "neither nor" = 2, "for reduction" = 3))


selects2023$ModernityTraditions <- labelled(selects2023$ModernityTraditions, labels = c("for modern ch" = 1, "neither nor" = 2, "defend traditions" = 3))

selects2023$Retirement67 <- labelled(selects2023$Retirement67, labels = c("against" = 1, "neither nor" = 2, "in favor" = 3))

selects2023$MinWage4kchf <- labelled(selects2023$MinWage4kchf, labels = c("against" = 1, "in favor" = 2))

selects2023$VoteRightsForeignMunicip <- labelled(selects2023$VoteRightsForeignMunicip, labels = c("in favor" = 2, "against" = 1))

selects2023$EqualRightsHomoCouples <- labelled(selects2023$EqualRightsHomoCouples, labels = c("against" = 1, "in favor" = 2))



write_sav(selects2023, "selects2023_recoded.sav")


```


```{r}

selects2023 <- read_sav("C:/Users/Celal/OneDrive/Bureau/celalguney/posts/selects2023/selects2023_recoded.sav")
names <- tibble(names = names(selects2023))

selects2023 <- 
  selects2023 %>% 
  mutate(vote2023 = if_else(is.na(voteChoice2023Combined), "NA/Abstention", as_label(voteChoice2023Combined)))

```

```{r}
# need first to construct the dataeset
selects2023MCA <-
selects2023 %>%
    select(socspend, EuMembership, TaxesHighInc, StateIntervention, Retirement67, MinWage4kchf, ModernityTraditions, Environment_vs_growth, EqualRightsHomoCouples, vote2023, lr_recoded, EducationLevel, sex, class8, age, IncomeAdjustedDecile) %>% drop_na()

selects2023MCA <- 
  selects2023MCA %>% 
  select(socspend, EuMembership, TaxesHighInc, StateIntervention, Retirement67, MinWage4kchf, ModernityTraditions, Environment_vs_growth, EqualRightsHomoCouples, vote2023, lr_recoded, EducationLevel, sex, class8) %>% 
  as_label %>% 
  as_factor %>% 
  mutate(
    income = selects2023MCA$IncomeAdjustedDecile,
    age = selects2023MCA$age
  )

mca = MCA(selects2023MCA,
          quali.sup = c(10:14),
          quanti.sup = 15:16,
          ncp = Inf)
mca %>% explor

clustermca <- HCPC(mca, graph = FALSE)
plot(clustermca, choice = "tree")

```


```{r}
res <- explor::prepare_results(mca)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = TRUE, var_sup_choice = c("vote2023",
    "EducationLevel", "class8", "income"), var_lab_min_contrib = 0, col_var = "Variable",
    symbol_var = "Type", size_var = NULL, size_range = c(10, 300), labels_size = 10,
    point_size = 56, transitions = TRUE, labels_positions = NULL, labels_prepend_var = FALSE,
    xlim = c(-1.71, 1.68), ylim = c(-1.18, 2.21))
```
