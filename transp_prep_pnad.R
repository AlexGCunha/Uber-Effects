#------------------------------------------------------------------------------------------------
#This code will:
# - Work on PNAD
#------------------------------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(readxl)
library(arrow)

rm(list=ls())
data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Output"

setwd(data_path)


df <- read_parquet("pnad_agg.parquet")

###########################################
#Labor variables
###########################################
#FIlter only capitals
df <- df %>% 
  filter(!is.na(capital))
gc()  

#Define informal
df <- df %>% 
  #informal A will be private employees without a signed booklet
  mutate(informalA = case_when(occupied ==1 & position_main == 2 ~ 1,
         T ~0)) %>% 
  #informal B will be private employees without a signed booklet or self employed
  mutate(informalB = case_when(occupied ==1 & position_main %in% c(2,9) ~ 1,
         T ~0))

  
#Aggregate by date and municipality
df <- df %>% 
  mutate(year_quarter=paste0(year,quarter)) %>% 
  group_by(year_quarter, capital) %>% 
  summarise(employed = sum(weight_post[occupied==1],na.rm=T),
            work_force = sum(weight_post[pea==1],na.rm=T),
            formal = sum(weight_post[position_main==1],na.rm=T),
            informalA = sum(weight_post[informalA==1],na.rm=T),
            informalB = sum(weight_post[informalB==1],na.rm=T),
            self_emp = sum(weight_post[position_main==9],na.rm=T),
            bico = sum(weight_post[bico==1],na.rm=T),
            employed_sec = sum(weight_post[position_sec %in%c(1:7)],na.rm=T),
            self_emp_sec = sum(weight_post[position_sec ==6],na.rm=T),
            desalento = sum(weight_post[desalento ==1],na.rm=T),
            wage_mass = sum(wage_all_hab[occupied==1],na.rm=T)*sum(weight_post[occupied==1],na.rm=T)) %>% 
  ungroup()
gc()

#Additional variables (mostly rates)
df <- df %>% 
  mutate(unemp_r = 1-employed/work_force) %>% 
  mutate(emp_r = employed/work_force) %>% 
  mutate(formal_r = formal/work_force) %>% 
  mutate(informalA_r = informalA/work_force) %>% 
  mutate(informalB_r = informalB/work_force) %>% 
  mutate(self_emp_r = self_emp/work_force) %>% 
  mutate(bico_r = bico/work_force) %>% 
  mutate(employed_sec_r = employed_sec/work_force) %>% 
  mutate(self_emp_sec_r = self_emp_sec/work_force) %>% 
  mutate(desalento_r = desalento/work_force) %>% 
  mutate(lwage_mass = log(wage_mass)) %>% 
  mutate(avg_wage = wage_mass/employed) %>% 
  mutate(lavg_wage = log(avg_wage))

#get ids for capitals
df_id <- read_excel("ibge_codes_capitals.xlsx") %>% 
  rename(capital=uf) %>% 
  mutate_at(c("capital","id_municipio"),~as.numeric(.))

df <-df %>% 
  left_join(df_id,by="capital",na_matches="never") %>% 
  mutate(capital=1)

#Save
write_parquet(df,"pnad_prepared.parquet")
rm(list=ls())
gc()




