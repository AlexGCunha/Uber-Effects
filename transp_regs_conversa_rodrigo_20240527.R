################################################################################
#This code will:
# - Run regressions
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
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Job Displacement/Apresentações/Conversa Rodrigo 20240526"

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
controls_pop <- as.formula("~populacao_14")
controls_up <- update(controls_reg,
                      ~ .+ pib_p_14  +populacao_14 #+ sbrancos_14 + shomens_14 +shs_14
)#+ pib_p_14 + sbrancos_14 + shomens_14 +shs_14
controls_gdp <- update(controls_reg,
                       ~ . + pib_p_14  #+ sbrancos_14 + shomens_14 +shs_14
)

#Começando com resultados bonitos
#All groups and region controls - pseudo never treated
setwd(output_path)
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_reg,
                  #base_period = "universal",
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&
                                           problem==0 & ano <=2017))

agg.es <- aggte(did_att, type="dynamic")
ggdid(agg.es)+
  labs(title="Ln Employment - All groups & region controls")+
  theme_minimal()

ggsave("uber_all.png", width = 5, height = 4)

ggdid(did_att)+
  labs(title="Ln Employment - All groups & region controls")
ggsave("uber_all_per_group.png", width = 9, height = 7)


#Problema 1: resultado  não é robusto
#No small groups groups and region controls - pseudo never treated
font_size = 9
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_reg,
                  #base_period = "universal",
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&
                                           problem==0 & ano <=2017 & obs_group_s > 5))

agg.es <- aggte(did_att, type="dynamic")
p1 = ggdid(agg.es)+
  labs(title="Ln Employment - No small groups & region controls")+
  theme_minimal()+
  theme(legend.position = c(0.1,0.1),
        text = element_text(size = font_size))

#MAIN: no small groups and gdp controls
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  #base_period = "universal",
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")

p2 = ggdid(agg.es)+labs(title="No small groups and GDP controls")+
  theme_minimal()+
  theme(legend.position = c(0.1,0.1),
        text = element_text(size = font_size))



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
p3 = ggdid(agg.es)+
  labs(title="Big Cities, no small groups, gdp controls, truly never treated")+
  theme_minimal()+
  theme(legend.position = c(0.1,0.1),
        text = element_text(size = font_size))

#Universal base period, no small groups and gdp controls
rm(did_att)
did_att <- att_gt(yname="lemprego",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  base_period = "universal",
                  #anticipation = 1,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p4 = ggdid(agg.es)+labs(title="Universal base; No small groups and GDP controls")+
  theme_minimal()+
  theme(legend.position = c(0.1,0.1),
        text = element_text(size = font_size))

#Plot 4 graphs
plot_grid(p1, p2,p3,p4)
ggsave("uber_multi_emp.png", width = 9, height = 7)


#Problema 2: admissões e demissões caged simplesmente não conversam
#########################################
#Log Admissions
rm(did_att)
did_att <- att_gt(yname="lsaldo_s_all_hire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry) 
                                         & mes %in% c(6,12) & problem==0 
                                         & ano <=2017 & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p1 = ggdid(agg.es)+labs(title="Log Admissions")+ 
  theme(legend.position = c(0.2,0.1))
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
p2 = ggdid(agg.es)+labs(title="Log Layoffs")+ 
  theme(legend.position = c(0.2,0.1))
summary(agg.es)

plot_grid(p1,p2)
ggsave("uber_caged.png", width = 7, height = 6)


#Heterogeneous effects
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
                                         & problem==0& ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p1 = ggdid(agg.es)+labs(title="Log Employment - LTHS")+theme_minimal()+
  theme(legend.position = c(0.2, 0.1),
        text = element_text(size = font_size))



#Log Employment - HS 
rm(did_att)
did_att <- att_gt(yname="lhs",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p2 = ggdid(agg.es)+labs(title="Log Employment - HS")+theme_minimal()+
  theme(legend.position = c(0.2, 0.1),
        text = element_text(size = font_size))


#Log Employment - MTHS 
rm(did_att)
did_att <- att_gt(yname="lmths",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12) 
                                         & problem==0 & ano <=2017 
                                         & obs_group_s_100 >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p3 = ggdid(agg.es)+labs(title="Log Employment - MTHS")+theme_minimal()+
  theme(legend.position = c(0.2, 0.1),
        text = element_text(size = font_size))

plot_grid(p1,p2, p3)



