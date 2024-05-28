#################################################################################################
#This code will:
# - Run regressions
#################################################################################################

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
  filter(ano >=2011& ano <=2019)

#Add UF dummies
region_dums <- dummy_cols(df_did$region)
region_dums <- region_dums %>% select(3:(ncol(region_dums)-1))
df_did <- cbind(df_did, region_dums)

#Define controls
controls_reg <- as.formula(paste0("~",paste(colnames(region_dums),collapse="+")))
controls_dem <- as.formula("~populacao")
controls_up <- update(controls_reg,
                      ~ . + pib_p_14 +populacao #+ sbrancos_14 + shomens_14 +shs_14
)#+ pib_p_14 + sbrancos_14 + shomens_14 +shs_14
controls_gdp <- update(controls_reg,
                       ~ . + pib_p_14  #+ sbrancos_14 + shomens_14 +shs_14
)

#########################################
#Slide comparação de resultados
#########################################
#Log Employment - todos os grupos e controle original
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_reg,
                  clustervars = c("id_municipio","mmc"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0 & ano <=2017))

agg.es <- aggte(did_att, type="dynamic")
p1 <-ggdid(agg.es)+labs(title="All groups and region controls")#+theme_minimal()
summary(agg.es)


#Log Employment - Exclui pequenos grupos e controle original
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_reg,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p2 <-ggdid(agg.es)+labs(title="No small groups and region controls")#+theme_minimal()
summary(agg.es)


#Log Employment - Exclui pequenos grupos e controle pib per capita
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p3 <-ggdid(agg.es)+labs(title="No small groups and add GDP")#+theme_minimal()
summary(agg.es)


#Log Employment - Exclui pequenos grupos e alternative control group
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_up,
                  clustervars = c("id_municipio"),
                  #alp=.05,
                  #control_group = "notyettreated",
                  data=df_did %>% filter(mes %in% c(6,12)&(problem==0|ever_treated==0) & populacao >100000  & obs_group_s_100 >10
                  ))

agg.es <- aggte(did_att, type="dynamic")
p4 <-ggdid(agg.es)+labs(title="No small groups and de facto never treated")#+theme_minimal()
summary(agg.es)


plot_grid(p1,p2,p3,p4)
setwd(output_path)
ggsave("graph_compare.png", width=10, height=7)



#########################################
#Slides de resultados
#########################################
#COntrole PIB e exclui pequenos grupos
#Log Employment
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+labs(title="Effect on Log Employment")#+theme_minimal()
summary(agg.es)
setwd(output_path)
ggsave("lemp_reg.png", width=5, height=4 )

#Results splitted by group
ggdid(did_att)
ggsave("lemp_reg_group.png", width=10, height=7)


#Results by scolarity level
rm(did_att)
did_att <- att_gt(yname="llths",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p1 <- ggdid(agg.es)+labs(title="Effect on Log Employment - LTHS")#+theme_minimal()

rm(did_att)
did_att <- att_gt(yname="lhs",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p2 <- ggdid(agg.es)+labs(title="Effect on Log Employment - HS")#+theme_minimal()

rm(did_att)
did_att <- att_gt(yname="lmths",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p3 <- ggdid(agg.es)+labs(title="Effect on Log Employment - MTHS")#+theme_minimal()

plot_grid(p1,p2,p3)
ggsave("schooling_reg.png", width=10, height=7)


#Results by sex
rm(did_att)
did_att <- att_gt(yname="lmale",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p1 <- ggdid(agg.es)+labs(title="Effect on Log Employment - Men")+ylim(-0.14,0.07)#+theme_minimal()

rm(did_att)
did_att <- att_gt(yname="lfemale",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p2 <- ggdid(agg.es)+labs(title="Effect on Log Employment - Women")+ylim(-0.14,0.07)#+theme_minimal()

plot_grid(p1,p2)
ggsave("sex_reg.png", width=10, height=5)


#Public Employment result
rm(did_pub)
did_pub <- att_gt(yname="lemprego_pub",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s>5 & emprego_public>0
                  ))

agg.pub <- aggte(did_pub, type="dynamic")
ggdid(agg.pub)+labs(title="Effect on Log Public Employment")
ggsave("pub_reg.png", width=5, height=4)

#Results by wage
rm(did_att)
did_att <- att_gt(yname="lwage_s_all_hire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p1 <- ggdid(agg.es)+labs(title="Effect on Log Admission Wages")

rm(did_att)
did_att <- att_gt(yname="lwage_s_all_fire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p2 <- ggdid(agg.es)+labs(title="Effect on Log Layoff Wages")


plot_grid(p1,p2)
ggsave("wage_reg.png", width=10, height=5)

#########################################
#Tables
#########################################

teste <- df_did %>% 
  filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 ) %>% 
  filter(ano==2015 & mes==12) %>% 
  group_by(semester_entry) %>% 
  summarise(count=n())

