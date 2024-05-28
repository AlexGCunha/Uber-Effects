################################################################################
#This code will:
# - Run regressions
# - run after transp_prep_others
################################################################################

library(tidyverse)
library(haven)
library(readxl)
library(arrow)
library(lfe)
library(stargazer)
library(sandwich)
library(lmtest)
library(fixest) #for evet study
library(broom) #for tidying regs outputs
library(cowplot)
library(did)
library(fastDummies)


rm(list=ls())
data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Output"

#########################################
#Read data and define dummies
#########################################
#choose file to openb
setwd(data_path)
df_did <- read_parquet("merged_month_munic.parquet") %>%
  filter(ano >=2010& ano <=2019)


#Add UF dummies
region_dums <- dummy_cols(df_did$region)
region_dums <- region_dums %>% select(3:(ncol(region_dums)-1))
df_did <- cbind(df_did, region_dums)

#Define controls
controls_reg <- as.formula(paste0("~",paste(colnames(region_dums),collapse="+")))
controls_up <- update(controls_reg,
                      ~ .+ pib_p_14  +populacao_14 
                      )#+ sbrancos_14 + shomens_14 +shs_14
controls_gdp <- update(controls_reg,
                      ~ . + pib_p_14  #+ sbrancos_14 + shomens_14 +shs_14
                      )
controls_pop <- update(controls_reg,
                       ~ . + populacao_14  #+ sbrancos_14 + shomens_14 +shs_14
                       )
controls_noreg = as.formula("~ pib_p_14 + populacao_14")


#########################################
#MAIN:Semester estimates - gdp controls and filter small groups
#########################################
#Log Employment
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                                         ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment")+theme_minimal()
summary(agg.es)

#Log Admissions
rm(did_att)
did_att <- att_gt(yname="lsaldo_s_all_hire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p1 = ggdid(agg.es)+labs(title="Effect on Log Admissions")#+theme_minimal()
summary(agg.es)


#Log Layoffs
rm(did_att)
did_att <- att_gt(yname="lsaldo_s_all_fire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p2 = ggdid(agg.es)+labs(title="Effect on Log Layoffs")#+theme_minimal()
summary(agg.es)

plot_grid(p1,p2)

#Log Employment - LTHS
rm(did_att)
did_att <- att_gt(yname="llths",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - LTHS")+theme_minimal()
summary(agg.es)


#Log Employment - HS 
rm(did_att)
did_att <- att_gt(yname="lhs",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - HS")+theme_minimal()
summary(agg.es)

#Log Employment - MTHS 
rm(did_att)
did_att <- att_gt(yname="lmths",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - MTHS")+theme_minimal()
summary(agg.es)


#Log Employment - Men 
rm(did_att)
did_att <- att_gt(yname="lmale",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - Men")+theme_minimal()
summary(agg.es)


#Log Employment - Women 
rm(did_att)
did_att <- att_gt(yname="lfemale",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - Women")+theme_minimal()
summary(agg.es)

#Log Employment - White 
rm(did_att)
did_att <- att_gt(yname="lwhite",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - White")+theme_minimal()
summary(agg.es)

#Log Employment - Non-White 
rm(did_att)
did_att <- att_gt(yname="lnwhite",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - Non-White")+theme_minimal()
summary(agg.es)


#Log Employment - White - LTHS
rm(did_att)
did_att <- att_gt(yname="lwhite_lths",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - White - LTHS")+theme_minimal()
summary(agg.es)
ggdid(did_att)

#Log Employment - Non-White - LTHS
rm(did_att)
did_att <- att_gt(yname="lnwhite_lths",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - Non-White - LTHS")+theme_minimal()
summary(agg.es)
ggdid(did_att)

#Log Employment - White - HS
rm(did_att)
did_att <- att_gt(yname="lwhite_hs",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - White - HS")+theme_minimal()
summary(agg.es)
ggdid(did_att)

#Log Employment - Non-White - HS
rm(did_att)
did_att <- att_gt(yname="lnwhite_hs",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))
agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment - Non-White - HS")+theme_minimal()
summary(agg.es)
ggdid(did_att)

#Log wages of admission - ALL
rm(did_att)
did_att <- att_gt(yname="lwage_s_all_hire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Wages of Admission")+theme_minimal()
summary(agg.es)


#Log wages of admission - LTHS
rm(did_att)
did_att <- att_gt(yname="lwage_s_lths_hire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Wages of Admission - LTHS")+theme_minimal()
summary(agg.es)

#Log wages of admission - HS
rm(did_att)
did_att <- att_gt(yname="lwage_s_hs_hire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Wages of Admission - HS")+theme_minimal()
summary(agg.es)

#Log wages of admission - MTHS
rm(did_att)
did_att <- att_gt(yname="lwage_s_mths_hire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Wages of Admission - MTHS")+theme_minimal()
summary(agg.es)

#Log wages of Layoff
rm(did_att)
did_att <- att_gt(yname="lwage_s_all_fire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Wages of Layoff")+theme_minimal()
summary(agg.es)

#Public
rm(did_pub)
did_pub <- att_gt(yname="lemprego_pub",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) & mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.pub <- aggte(did_pub, type="dynamic")
ggdid(agg.pub)+labs(title="Effect on Log Public Employment")
summary(agg.pub)



#########################################
#Alternative specifications
#########################################
#All municipalities  > 100k hab as controls, filter small groups and gdp control
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #base_period = "universal",
                  #alp=.05,
                  #control_group = "notyettreated",
                  data=df_did %>% filter(mes %in% c(6,12)&
                                           (problem==0|ever_treated==0) &
                                           populacao >100000  & 
                                           obs_group_s_100 >5
                  ))


agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment")+theme_minimal()
summary(agg.es)


#All municipalities as controls. population and gdp controls
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_up,
                  clustervars = c("id_municipio"),
                  #base_period = "universal",
                  #alp=.05,
                  #control_group = "notyettreated",
                  data=df_did %>% filter(mes %in% c(6,12)&
                                           (problem==0|ever_treated==0) &
                                           obs_group_s_100 >9
                  ))


agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment")+theme_minimal()
summary(agg.es)