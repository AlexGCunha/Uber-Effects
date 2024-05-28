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
library(xtable)
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
                  alp=0.05,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p1 <- ggdid(agg.es)+labs(title="Effect on Log Employment")+
  annotate(geom="text",x=-1, y=0.04, label="ATT=-0.025**")
summary(agg.es)

#Results splitted by group
ggdid(did_att)
ggsave("lemp_reg_group.png", width=10, height=7)

#Employment/Pop
rm(did_att)
did_att <- att_gt(yname="emprego_pop",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  alp=0.01,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
p2 <- ggdid(agg.es)+labs(title="Effect on Employment/Population")+
  annotate(geom="text",x=-1, y=0.01, label="ATT=-0.006***")
summary(agg.es)


plot_grid(p1,p2)
setwd(output_path)
ggsave("emp_l_pop_reg.png", width=10, height=5 )

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
                  alp=0.01,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
summary(agg.es)
p1 <- ggdid(agg.es)+labs(title="Effect on Log Employment - LTHS")+ylim(-0.25,0.32)+
  annotate(geom="text",x=-1, y=0.25, label="ATT=-0.042***")

rm(did_att)
did_att <- att_gt(yname="lhs",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  alp=0.01,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
summary(agg.es)
p2 <- ggdid(agg.es)+labs(title="Effect on Log Employment - HS")+ylim(-0.25,0.32)+
  annotate(geom="text",x=-1, y=0.25, label="ATT=-0.047***")

rm(did_att)
did_att <- att_gt(yname="lmths",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  alp=0.1,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
summary(agg.es)
p3 <- ggdid(agg.es)+labs(title="Effect on Log Employment - MTHS")+ylim(-0.25,0.32)+
  annotate(geom="text",x=-1, y=0.25, label="ATT=0.029")

plot_grid(p1,p2,p3)
ggsave("schooling_reg.png", width=10, height=7)


#Results by sex
rm(did_att)
did_att <- att_gt(yname="lmale",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  alp=0.05,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
summary(agg.es)
p1 <- ggdid(agg.es)+labs(title="Effect on Log Employment - Men")+ylim(-0.14,0.07)+
  annotate(geom="text",x=-1, y=0.05, label="ATT=-0.028**")

rm(did_att)
did_att <- att_gt(yname="lfemale",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  alp=0.05,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
summary(agg.es)
p2 <- ggdid(agg.es)+labs(title="Effect on Log Employment - Women")+ylim(-0.14,0.07)+
  annotate(geom="text",x=-1, y=0.05, label="ATT=-0.019**")

plot_grid(p1,p2)
ggsave("sex_reg.png", width=10, height=5)


#Public Employment result
rm(did_pub)
did_pub <- att_gt(yname="lemprego_pub",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  alp=0.1,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s>5 & emprego_public>0
                  ))

agg.pub <- aggte(did_pub, type="dynamic")
summary(agg.pub)
ggdid(agg.pub)+labs(title="Effect on Log Public Employment")+
  annotate(geom="text",x=-1, y=0.1, label="ATT=-0.02")
ggsave("pub_reg.png", width=5, height=4)

#Results by wage
rm(did_att)
did_att <- att_gt(yname="lwage_s_all_hire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  alp=0.1,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
summary(agg.es)
p1 <- ggdid(agg.es)+labs(title="Effect on Log Admission Wages")+ylim(-0.14,0.12)+
  annotate(geom="text",x=-1, y=0.08, label="ATT=0.009")

rm(did_att)
did_att <- att_gt(yname="lwage_s_all_fire",
                  tname="period_did_s",
                  idname="id_municipio",
                  gname="period_treat_s",
                  xformla= controls_gdp,
                  alp=0.1,
                  clustervars = c("id_municipio"),
                  #control_group = "notyettreated",
                  data=df_did %>% filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 
                                         & obs_group_s >5
                  ))

agg.es <- aggte(did_att, type="dynamic")
summary(agg.es)
p2 <- ggdid(agg.es)+labs(title="Effect on Log Layoff Wages")+ylim(-0.14,0.12)+
  annotate(geom="text",x=-1, y=0.08, label="ATT=-0.001")


plot_grid(p1,p2)
ggsave("wage_reg.png", width=10, height=5)

#########################################
#Tables
#########################################
#Date of entry
table_entry <- df_did %>% 
  filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0& ano <=2017 ) %>% 
  filter(ano==2015 & mes==12) %>% 
  group_by(semester_entry) %>% 
  summarise(count=n()) %>% 
  rename("Semester of entry"=semester_entry, Count=count)


print(xtable(table_entry), include.rownames=FALSE)

#Summary Statistics
table_summary <- df_did %>% 
  filter(ano==2015 & mes==12) %>% 
  filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0 & obs_group_s >5) %>% 
  mutate(treat = ifelse(year_entry<=2017,1,0)) %>% 
  group_by(treat) %>% 
  summarise(populacao = mean(populacao_14),
            emprego_tot = sum(emprego),
            emprego_avg = mean(emprego),
            wage_tot = sum(wage_avg*emprego),
            homens = sum(homens),
            brancos = sum(brancos),
            lths = sum(lths),
            hs = sum(hs),
            mths = sum(mths)) %>% 
  ungroup() %>% 
  mutate_at(c("homens","brancos","lths","hs","mths","wage_tot"),~./emprego_tot) %>% 
  mutate_at(c("homens","brancos","lths","hs","mths"),~round(.,2)) %>% 
  mutate_at(c("populacao", "emprego_avg","wage_tot"),~round(.)) %>% 
  select(treat, populacao, emprego_avg, wage_tot, homens, brancos, lths, hs, mths) %>% 
  rename(Treated = treat,
         Population = populacao,
         "Formal Employment" = emprego_avg,
         Wages = wage_tot,
         "Share of Men" = homens,
         "Share of White" = brancos,
         "Share LTHS"=lths,
         "Share HS"=hs,
         "Share MTHS"=mths) %>% 
  select(!Treated)
row.names(table_summary) = c("Control","Treated")

print(xtable(t(table_summary)))

