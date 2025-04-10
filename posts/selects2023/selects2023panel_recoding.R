# selects 2023 panel dataset recoding 


rm(list=ls())
library(tidyverse)
library(haven)
library(sjlabelled)
library(MASS)
library(marginaleffects)
library(gt)
library(modelsummary)
library(lme4)

selects2023panel = read_sav("C:/Users/Gueney/Documents/GitHub/celalguney/posts/selects2023/selects2023Panel/data/selects2023panel.sav")



selects2023panel2 <- 
  selects2023panel  %>% 
  rename(
    "cantonVoteRight2023" = W1_f10000,
    "satisfDemocracy" = W1_f13700,
    "polNotCarePeople" = W1_Q04b,
    "polTrustworthy" = W1_Q04c,
    "peopleShouldMakeImpDecisions" = W1_Q04f,
    "polOnlyCareAbtRich" = W1_Q04g,
    "mostImpProblReduced" = W1_f12700rec,
    "secMostImpProblReduced" = W1_f12730rec,
    "evolEconPast12Months" = W1_f14610,
    "partyVoted2019" = W1_f10300rec,
    "partyVoted2019Main" = W1_f10300main6,
    "voteIntention2023main" = W1_f1085_90main6, # uses combined variable
    "particip_fedVotes" = W1_f12500, # 0 to 10
    "N_times_loose_fedVotes" = W1_f12501, # 0 out of 10 to 10 out of 10,
    "participVote18June23" = W1_f10750, # 1 = participated (dummy)
    "perceptionFamilies" = W1_f19101a,
    "perceptionWorkers" = W1_f19101b,
    "perceptionRichPeople" = W1_f19101c,
    "perceptionFeminists" = W1_f19101d,
    "perceptionMigranst" = W1_f19101e,
    "perceptionClimateActivists" = W1_f19101f,
    "perceptionEntrepreneurs" = W1_f19101g,
    "partyBestRepresentFamilies" = W1_f19102arec,
    "partyBestRepresentWorkers" = W1_f19102brec,
    "partyBestRepresentRich" = W1_f19102crec,
    "partyBestRepresentFeminists" = W1_f19102drec,
    "partyBestRepresentMigrants" = W1_f19102erec,
    "partyBestRepresentClimAct" = W1_f19102frec,
    "partyBestRepresentEntreprnrs" = W1_f19102grec,
    "partyClosenessMain" = W1_f14010main6,
    "probsVoteFDP" = W1_f14400a,
    "probsVoteCenter" = W1_f14400j,
    "probsVoteSP" = W1_f14400c,
    "probsVoteSVP" = W1_f14400d,
    "probsVoteGreens" = W1_f14400e,
    "probsVoteLibGreens" = W1_f14400f,
    "probsVoteLega" = W1_f14400h,
    "probsVoteMCG" = W1_f14400i,
    "lr" = W1_f15200,
    "religion" = W1_f20760rec,
    "churchAttendance" = W1_f20900, # 1 to 7 (never)
    "education" = W1_f21310rec,
    "workingSit" = W1_f21400,
    "positionWork" = W1_f21500,
    "workingSector" = W1_f21700,
    "grossHouseholdIncome" = W1_f28910,
    "householdSize" = W1_hhsize_sample
    
    
    
    
  ) %>% 
  mutate(
    
    Income = case_when(
      grossHouseholdIncome == 1 ~ 2000,
      grossHouseholdIncome == 2 ~ (3000+2001)/2,
      grossHouseholdIncome == 3 ~ (4000+3001)/2,
      grossHouseholdIncome == 4 ~ (5000+4001)/2,
      grossHouseholdIncome == 5 ~ (6000+5001)/2,
      grossHouseholdIncome == 6 ~ (7000+6001)/2,
      grossHouseholdIncome == 7 ~ (8000+7001)/2,
      grossHouseholdIncome == 8 ~ (9000+8001)/2,
      grossHouseholdIncome == 9 ~ (10000+9001)/2,
      grossHouseholdIncome == 10 ~ (11000+10001)/2,
      grossHouseholdIncome == 11 ~ (12000+11001)/2,
      grossHouseholdIncome == 12 ~ (13000+12001)/2,
      grossHouseholdIncome == 13 ~ (14000+13001)/2,
      grossHouseholdIncome == 14 ~ (15000+14001)/2,
      grossHouseholdIncome == 15 ~ (16000+15001)/2,
      grossHouseholdIncome == 16 ~ (17000+16001)/2,
      grossHouseholdIncome == 17 ~ (18000+17001)/2,
      grossHouseholdIncome == 18 ~ (19000+18001)/2,
      grossHouseholdIncome == 19 ~ (20000+19001)/2,
      grossHouseholdIncome == 20 ~ 20000
    ),
    
    IncomeAdjusted = Income/sqrt(householdSize),
    IncomeAdjustedDecile = ntile(IncomeAdjusted, 10),
    IncomeDecile = ntile(Income, 10),
    
    education_recoded = case_when(
      
      education %in% c(1:4) ~ 1, # primary 
      education %in% c(5:9) ~ 2, # secondary
      education %in% c(10:13) ~ 3, # tertiary
      education == 14 ~ NA # "other" ==> NA
      
    ),
    
    op_privProfit_socializeLoss = case_when( # warning: the question is "Les banques encaissent des profits exorbitants, alors que les pertes sont toujours payées par les contribuables, dans quelle mesure êtes-vous d'accord ?"
      W1_f14651   %in%  c(1,2) ~ 1, #disagree
      W1_f14651 == 3 ~ 2, # neither
      W1_f14651  %in% c(3,4) ~ 3 # agree
    ),
    
    op_bankingRegulation = case_when(
      W1_f14652 %in% c(1,2) ~ 1, # disagree
      W1_f14652 == 3 ~ 2, # neither
      W1_f14652  %in% c(4, 5) ~ 3 # agree
    ),
    
    op_warMat_re_export = case_when(
      W1_f15383 %in% c(0:4) ~ 1, # in favor
      W1_f15383 == 5 ~ 2, # neither
      W1_f15383  %in% c(6:10) ~ 3 # against
    ),
    
    op_easy_accessForMigrants = case_when(
      
      W1_f15381 %in% c(0:4) ~ 1, # in favor 
      W1_f15381 == 5 ~ 2, # neither
      W1_f15381  %in% c(6,10) ~ 3 # against
    ),
    
    op_genderSensitiveLang = case_when(
      
      W1_f15382 %in% c(0:4) ~ 1, # in favor
      W1_f15382 == 5 ~ 2, # neither
      W1_f15382  %in% c(6:10) ~ 3 # against
    ),
    
    vote_oecd_taxation = case_when(
      
      W1_f10751a == 1 | W1_f10756a   %in%   c(3,4) ~ 1, # voted yes or is hypothetically in favor
      W1_f10751a  %in% c(3) |  W1_f10756a == 8 ~ 2, # voted blank or don't know
      W1_f10751a == 2 | W1_f10756a   %in%  c(1,2) ~ 3 # voted no or is hyp against
      
    ),
    
    vote_climateLaw = case_when(
      
      W1_f10751b == 1 | W1_f10756b %in% c(3,4) ~ 1, # voted yes or hyp in favor
      W1_f10751b == 3 | W1_f10756b == 8 ~ 2, # voted blank or don't know
      W1_f10751b == 2 | W1_f10756b  %in%  c(1,2) ~ 3 # voted no or hyp against
      
    ),
    
    vote_covidAct = case_when(
      
      W1_f10751c == 1 | W1_f10756c  %in%  c(3,4) ~ 1, # in favor
      W1_f10751c == 3 | W1_f10756c == 8 ~ 2, # neither (blank or did not vote)
      W1_f10751c == 2 | W1_f10756c   %in% c(1,2) ~ 3 # against (voted no)
      
    ),
    
    lr_recoded = case_when(
      
      lr  %in% c(0,1,2) ~ 1, #left
      lr  %in% c(3,4) ~ 2, #center-left
      lr == 5 ~ 3, # center
      lr %in% c(6,7) ~ 4, # center-right
      lr %in% c(8,9,10) ~ 5 # right
    ),
    
    op_limit_immigration = case_when(
      W1_f15340b %in% c(1,2) ~ 1, # against
      W1_f15340b == 3  ~ 2, # neither
      W1_f15340b  %in% c(4,5) ~ 3 # in favor
    ),
    
    op_funding_childcare = case_when(
      
      W1_f15340c %in% c(1,2) ~ 1, # against
      W1_f15340c == 3 ~ 2, # neither
      W1_f15340c  %in% c(4,5) ~ 3 # in favor 
      
    ),
    
    op_environm_protection = case_when(
      
      W1_f15340d %in% c(1,2) ~ 1, # against
      W1_f15340d == 3 ~ 2, # neither
      W1_f15340d  %in% c(4,5) ~ 3 # in favor 
      
    ),
    
    op_compet_swissEcon = case_when(
      
      W1_f15340e %in% c(1,2) ~ 1, # against
      W1_f15340e == 3 ~ 2, # neither
      W1_f15340e  %in% c(4,5) ~ 3 # in favor 
      
    ),
    
    op_incr_minstand_healthIns = case_when(
      
      W1_f15340f %in% c(1,2) ~ 1, # against
      W1_f15340f == 3 ~ 2, # neither
      W1_f15340f  %in% c(4,5) ~ 3 # in favor 
      
    ),
    
    op_socialExpend = case_when(
      
      W1_f15420 %in% c(1,2) ~ 1, # against
      W1_f15420 == 3 ~ 2, # neither
      W1_f15420  %in% c(4,5) ~ 3 # in favor 
      
    ),
    
    op_eu_integration = case_when( # in the sense of integration without membership
      
      W1_f15425  %in% c(1,2) ~ 1, # in favor
      W1_f15425 == 3 ~ 2, # neither
      W1_f15425  %in% c(4,5) ~ 3 # against
      
    ),
    
    op_eu_membership = case_when(
      
      W1_f15431 %in% c(1,2) ~ 1, # against
      W1_f15431 == 3 ~ 2, # neither
      W1_f15431  %in% c(4,5) ~ 3 # in favor 
      
    ),
    
    op_stateVScomp = case_when(
      W1_f15435 %in%  c(1,2) ~ 1, # more state intervention
      W1_f15435 == 3 ~ 2, # neither
      W1_f15435 %in% c(4,5) ~ 3 # more competition in the economy
    ),
    
    op_chances_foreignVSswiss = case_when(
      
      W1_f15440 %in% c(1,2) ~ 1, # in favor 
      W1_f15440 == 3 ~ 2, # neither
      W1_f15440 %in% c(4,5) ~ 3 # against (more chances for the swiss)
    ),
    
    op_tmwAb_e_ov_CandP = case_when(
      
      W1_f15478 %in% c(1,2) ~ 1, # disagree 
      W1_f15478 == 3 ~ 2, # neither
      W1_f15478 %in% c(4,5) ~ 3 # agree (too much worries about environment over cost and prices)
    ),
    
    op_gender_equality = case_when(
      
      W1_f15475 %in% c(1,2) ~ 1, # against (habe gone too far)
      W1_f15475 == 3 ~ 2, # neither
      W1_f15475 %in% c(4,5) ~ 3 # in favor (not far enough)
    ),
    
    op_taxes_high_Inc = case_when(
      
      W1_f15480 %in% c(1,2) ~ 1, # in favor (for increase)
      W1_f15480 == 3 ~ 2, # neither
      W1_f15480 %in% c(4,5) ~ 3 # against (for reduction)
    ),
    
    op_traditionVSmodernity = case_when(
      
      W1_f15485 %in% c(1,2) ~ 1, # modernity
      W1_f15485 == 3 ~ 2, # neither
      W1_f15485 %in% c(4,5) ~ 3 # traditions
    ),
    
    op_minWage = case_when(
      
      W1_f15815 %in% c(1,2) ~ 1, # against
      W1_f15815 %in% c(3,4) ~ 2 # in favor
    ),
    
    op_increase_retirementAge = case_when(
      
      W1_f15811 %in% c(1,2) ~ 1, # against
      W1_f15811 %in% c(3,4) ~ 2 # in favor
    ),
    
    op_equalRights_homo = case_when(
      
      W1_f15817 %in% c(1,2) ~ 1, # against
      W1_f15817 %in% c(3,4) ~ 2 # in favor
    ),
    
    op_voteRightForeign = case_when(
      
      W1_f15816 %in% c(1,2) ~ 1, # against
      W1_f15816 %in% c(3,4) ~ 2 # in favor
    ),
    
    
    
    participVote2019Dummy = if_else(W1_f10200rec == 1, 1, if_else(is.na(W1_f10200rec), NA, 0)),
    willVote2023 = if_else(W1_f10800 %in% c(3,4), 1, if_else(is.na(W1_f10800), NA, 0)),
    voteInt_FDP = if_else(voteIntention2023main == 10, 1, if_else(is.na(voteIntention2023main), NA, 0)),
    voteInt_center = if_else(voteIntention2023main == 21, 1, if_else(is.na(voteIntention2023main), NA, 0)),
    voteInt_SP = if_else(voteIntention2023main == 30, 1,  if_else(is.na(voteIntention2023main), NA, 0)),
    voteInt_SVP = if_else(voteIntention2023main == 40, 1,  if_else(is.na(voteIntention2023main), NA, 0)),
    voteInt_greens = if_else(voteIntention2023main == 50, 1,  if_else(is.na(voteIntention2023main), NA, 0)),
    voteInt_greensLib = if_else(voteIntention2023main == 60, 1,  if_else(is.na(voteIntention2023main), NA, 0))
    
  )


selects2023panel2$op_privProfit_socializeLoss <- labelled(selects2023panel2$op_privProfit_socializeLoss, labels = c("disagree" = 1, "neither" = 2, "agree" = 3))

selects2023panel2$op_bankingRegulation <- labelled(selects2023panel2$op_bankingRegulation, labels = c("disagree" = 1, "neither" = 2, "agree" = 3))


selects2023panel2$op_warMat_re_export <- labelled(selects2023panel2$op_warMat_re_export, labels = c("in favor" = 1, "neither" = 2, "against" = 3))


selects2023panel2$op_easy_accessForMigrants <- labelled(selects2023panel2$op_easy_accessForMigrants, labels = c("in favor" = 1, "neither" = 2, "against" = 3))


selects2023panel2$op_genderSensitiveLang <- labelled(selects2023panel2$op_genderSensitiveLang, labels = c("in favor" = 1, "neither" = 2, "against" = 3))



selects2023panel2$lr_recoded <- labelled(selects2023panel2$lr_recoded, labels = c("left" = 1, "center-left" = 2, "center" = 3, "center-right" = 4, "right" = 5))



selects2023panel2$op_limit_immigration <- labelled(selects2023panel2$op_limit_immigration, labels = c("against" = 1, "neither" = 2, "in favor" = 3))


selects2023panel2$op_funding_childcare <- labelled(selects2023panel2$op_funding_childcare, labels = c("against" = 1, "neither" = 2, "in favor" = 3))


selects2023panel2$op_environm_protection <- labelled(selects2023panel2$op_environm_protection, labels = c("against" = 1, "neither" = 2, "in favor" = 3))



selects2023panel2$op_compet_swissEcon <- labelled(selects2023panel2$op_compet_swissEcon, labels = c("against" = 1, "neither" = 2, "in favor" = 3))



selects2023panel2$op_incr_minstand_healthIns <- labelled(selects2023panel2$op_incr_minstand_healthIns, labels = c("against" = 1, "neither" = 2, "in favor" = 3))



selects2023panel2$op_socialExpend <- labelled(selects2023panel2$op_socialExpend, labels = c("against" = 1, "neither" = 2, "in favor" = 3))


selects2023panel2$op_eu_integration <- labelled(selects2023panel2$op_eu_integration, labels = c("in favor" = 1, "neither" = 2, "against" = 3))



selects2023panel2$op_eu_membership <- labelled(selects2023panel2$op_eu_membership, labels = c("against" = 1, "neither" = 2, "in favor" = 3))


selects2023panel2$op_stateVScomp <- labelled(selects2023panel2$op_stateVScomp, labels = c("more state intervention" = 1, "neither" = 2, "more competition in the economy" = 3))



selects2023panel2$op_chances_foreignVSswiss <- labelled(selects2023panel2$op_chances_foreignVSswiss, labels = c("in favor" = 1, "neither" = 2, "against" = 3))



selects2023panel2$op_tmwAb_e_ov_CandP <- labelled(selects2023panel2$op_tmwAb_e_ov_CandP, labels = c("disagree" = 1, "neither" = 2, "agree" = 3))



selects2023panel2$op_gender_equality <- labelled(selects2023panel2$op_gender_equality, labels = c("against" = 1, "neither" = 2, "in favor" = 3))



selects2023panel2$op_taxes_high_Inc <- labelled(selects2023panel2$op_taxes_high_Inc, labels = c("in favor" = 1, "neither" = 2, "against" = 3))


selects2023panel2$op_traditionVSmodernity <- labelled(selects2023panel2$op_traditionVSmodernity, labels = c("modernity" = 1, "neither" = 2, "traditions" = 3))


selects2023panel2$op_minWage <- labelled(selects2023panel2$op_minWage, labels = c("against" = 1, "in favor" = 2))


selects2023panel2$op_increase_retirementAge <- labelled(selects2023panel2$op_increase_retirementAge, labels = c("against" = 1, "in favor" = 2))


selects2023panel2$op_equalRights_homo <- labelled(selects2023panel2$op_equalRights_homo, labels = c("against" = 1, "in favor" = 2))


selects2023panel2$op_voteRightForeign <- labelled(selects2023panel2$op_voteRightForeign, labels = c("against" = 1, "in favor" = 2))


selects2023panel2$education_recoded <- labelled(selects2023panel2$education_recoded, labels = c("Primary" = 1, "Secondary" = 2, "Tertiary" = 3))

write_sav(selects2023panel2, "selects2023panel_recoded.sav")



selects2023panel$vote_oecd_taxation <- labelled(selects2023panel$vote_oecd_taxation, labels = c("in favor" = 1, "neither" = 2, "against" = 3))
selects2023panel$vote_climateLaw <- labelled(selects2023panel$vote_climateLaw, labels = c("in favor" = 1, "neither" = 2, "against" = 3))
selects2023panel$vote_covidAct <- labelled(selects2023panel$vote_covidAct, labels = c("in favor" = 1, "neither" = 2, "against" = 3))

