#------------------------------------------------------------------------------------------------
#This code will:
# - Define when each municipality was treated
# - Prepare data for regressions
# - Merge with other dataset
# - Run after transp_prep_employment_panel
#------------------------------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(readxl)
library(arrow)

rm(list=ls())
a=Sys.time()
data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Output"

setwd(data_path)
a=Sys.time()

#########################################
#Dates when uber first came to each municipality
#########################################
df_first <- read_excel("initial_dates.xlsx") %>% 
  rename(id_municipio = munic) %>% 
  mutate(id_municipio = as.character(id_municipio)) %>% 
  mutate(month_temp = paste0("0",month_entry)) %>% 
  mutate(month_temp = substr(month_temp, nchar(month_temp)-1,nchar(month_temp))) %>% 
  #Date of entry
  mutate(date_entry = ifelse(is.na(month_entry),NA_real_,paste0(year_entry, month_temp))) %>%  
  #Quarter of entry
  mutate(month_entry = as.numeric(month_entry)) %>% 
  mutate(quarter_entry = case_when(month_entry %in% c(1,2,3) ~1,
                                   month_entry %in% c(4,5,6) ~2,
                                   month_entry %in% c(7,8,9) ~3,
                                   month_entry %in% c(10,11,12)~4,
                                   T ~ NA_real_)) %>% 
  mutate(quarter_entry = ifelse(is.na(month_entry),NA_real_,paste0(year_entry,quarter_entry))) %>% 
  mutate(semester_entry = paste0(year_entry, semester_entry)) %>% 
  select(!c(month_temp,month_entry, obs,source)) %>% 
  filter(!is.na(year_entry))

munics <- unique(df_first$id_municipio)

#########################################
#Work with RAIS data and merge
#########################################
df <- read_parquet("rais_treated.parquet") %>% 
  select(!mais40) %>% 
  filter(ano <= 2019) %>% 
  left_join(df_first, by="id_municipio",na_matches="never") %>% 
  #define never treated
  mutate(ever_treated = case_when(ever_treated==1~1,T~0))

rm(df_first)

#Create a balanced panel of municipalities
munics <- unique(df$id_municipio)
year <- unique(df$ano)
months <- unique(df$mes)

df_aux <- expand.grid(id_municipio = munics, ano = year, mes=months)

df <- df %>% 
  right_join(df_aux, by=c("id_municipio","ano","mes"),na_matches="never") %>%
  #Fill NA values with 0
  mutate(emprego = ifelse(is.na(emprego),0,emprego))

rm(df_aux)

#Merge with population df
df_pop <- read_excel("pop_mun.xlsx") 

df_pop <- df_pop%>%
  mutate_at(2:ncol(df_pop),~ifelse(.=="...",NA,.)) %>% 
  mutate_at(2:ncol(df_pop),as.numeric) %>% 
  pivot_longer(2:ncol(df_pop), names_to="ano",values_to="populacao") %>% 
  mutate_at(c("id_municipio"),as.character) %>% 
  mutate(populacao = as.numeric(populacao)) %>% 
  mutate(ano = as.numeric(ano))

df <- df %>% 
  left_join(df_pop, by=c("id_municipio","ano"),na_matches="never")
rm(df_pop)

#Census DF to get working-age population
df_censo <- read_parquet("census_10_pes_ibge.parquet")
df_censo <- df_censo %>% 
  filter(age >=14 & age <=66) %>% # people between 18 and 70 years when uber first arrived in 2014
  mutate(id_municipio = paste0(uf, munic)) %>%
  group_by(id_municipio) %>% 
  summarise(pop_censo = sum(weight, na.rm=T)) %>% 
  ungroup() 

df <- df %>% 
  left_join(df_censo, by=c("id_municipio"),na_matches="never")
rm(df_censo)
gc()
  


#Arrange
df<- df  %>% 
  arrange(id_municipio, ano, mes) %>% 
  select(id_municipio, ano, mes, everything())


#Create additional variables
df <- df %>% 
  mutate(lpop = log(populacao)) %>% 
  #Dates
  mutate_at(c("ano","mes"),as.integer) %>%
  mutate(quarter = case_when(mes %in% c(1,2,3)~1,
                             mes %in% c(4,5,6)~2,
                             mes %in% c(7,8,9)~3,
                             mes %in% c(10,11,12)~4,
                             T~NA_real_)) %>% 
  mutate(semester = case_when(mes %in% c(1,2,3,4,5,6)~1,
                             mes %in% c(7,8,9,10,11,12)~2,
                             T~NA_real_)) %>%

  mutate(year_quarter = paste0(ano, quarter)) %>% 
  mutate(year_sem = paste0(ano, semester)) %>% 
  mutate(lths = fundamental_incompleto+fundamental_completo+medio_incompleto) %>% 
  mutate(hs = medio_completo) %>% 
  mutate(mths = superior_incompleto + superior_completo) %>% #more than High School
  mutate(n_brancos = emprego - brancos) %>% 
  mutate(n_brancos_lths = lths - brancos_lths) %>% 
  mutate(n_brancos_hs = hs - brancos_hs) %>% 
  mutate(n_brancos_mths = mths - brancos_mths) %>% 
  mutate(mulheres = emprego- homens) %>% 
  mutate(mulheres_lths = lths - homens_lths) %>% 
  mutate(mulheres_hs = hs - homens_hs) %>% 
  mutate(mulheres_mths = mths - homens_mths) %>% 
  mutate(sbrancos = brancos/emprego) %>% 
  mutate(shomens =homens/emprego) %>% 
  mutate(sfund_incompleto = fundamental_incompleto/emprego) %>%
  mutate(sfund_completo = fundamental_completo/emprego) %>% 
  mutate(smedio_incompleto = medio_incompleto/emprego) %>%
  mutate(smedio_completo = medio_completo/emprego) %>%
  mutate(ssup_incompleto = superior_incompleto/emprego) %>%
  mutate(ssup_completo = superior_completo/emprego) %>%
  #mutate(smais40 = mais40/emprego) %>% 
  mutate(slths = lths/emprego) %>% 
  mutate(shs = hs/emprego) %>%
  mutate(smths = mths/emprego) %>% 
  #Shares in 2014
  group_by(id_municipio) %>% 
  mutate(sbrancos_14 = brancos[ano==2014&mes==12]/emprego[ano==2014&mes==12]) %>% 
  mutate(shomens_14 = homens[ano==2014&mes==12]/emprego[ano==2014&mes==12]) %>% 
  mutate(slths_14 = lths[ano==2014&mes==12]/emprego[ano==2014&mes==12]) %>%
  mutate(shs_14 = hs[ano==2014&mes==12]/emprego[ano==2014&mes==12]) %>%
  ungroup() %>%
  mutate_at(c("ano", "mes"),as.integer) %>% 
  #Create metrics based on population
  mutate(populacao = as.numeric(populacao)) %>% 
  mutate(emprego_pop = emprego/populacao) %>% 
  mutate(pub_pop = emprego_public/populacao) %>% 
  mutate(lths_p = lths/populacao) %>% 
  mutate(hs_p = hs/populacao) %>%
  mutate(mths_p = mths/populacao) %>% 
  mutate(uf = substr(as.character(id_municipio),1,2)) %>% 
  mutate(region = substr(as.character(id_municipio),1,1)) %>% 
  mutate(white_p = brancos/populacao) %>%
  mutate(nwhite_p = n_brancos/populacao) %>% 
  mutate(male_p = homens/populacao) %>% 
  mutate(female_p = mulheres/populacao) %>% 
  #Create metrics based on census workng age population
  mutate(emprego_pc = emprego/pop_censo) %>% 
  mutate(pub_pc = emprego_public/pop_censo) %>% 
  mutate(lths_pc = lths/pop_censo) %>% 
  mutate(hs_pc = hs/pop_censo) %>%
  mutate(mths_pc = mths/pop_censo) %>% 
  mutate(white_pc = brancos/pop_censo) %>%
  mutate(nwhite_pc = n_brancos/pop_censo) %>% 
  mutate(male_pc = homens/pop_censo) %>% 
  mutate(female_pc = mulheres/pop_censo) %>% 
  #Variables in logs
  mutate(lemprego = log(emprego)) %>% 
  mutate(lemprego_pub = log(emprego_public)) %>%
  mutate(llths = log(lths)) %>% 
  mutate(lhs = log(hs)) %>% 
  mutate(lmths = log(mths)) %>% 
  mutate(lnwhite = log(n_brancos)) %>% 
  mutate(lnwhite_lths = log(n_brancos_lths)) %>% 
  mutate(lnwhite_hs = log(n_brancos_hs)) %>%
  mutate(lnwhite_mths = log(n_brancos_mths)) %>%
  mutate(lwhite = log(brancos)) %>% 
  mutate(lwhite_lths = log(brancos_lths)) %>% 
  mutate(lwhite_hs = log(brancos_hs)) %>%
  mutate(lwhite_mths = log(brancos_mths)) %>%
  mutate(lmale = log(homens)) %>% 
  mutate(lmale_lths = log(homens_lths)) %>% 
  mutate(lmale_hs = log(homens_hs)) %>% 
  mutate(lmale_mths = log(homens_mths)) %>% 
  mutate(lfemale = log(mulheres)) %>% 
  mutate(lfemale_lths = log(mulheres_lths)) %>% 
  mutate(lfemale_hs = log(mulheres_hs)) %>% 
  mutate(lfemale_mths = log(mulheres_mths)) %>% 
  select(id_municipio, year_quarter, everything())


#filter quarters
df <- df %>%
  filter(mes %in% c(03,06,09,12))

#Define job creation
df <- df %>% 
  group_by(id_municipio) %>% 
  mutate(saldo_rais = emprego -lag(emprego)) %>% 
  mutate(saldo_rais_s = emprego - lag(emprego,n=2L)) %>% 
  mutate(saldo_rais_y = emprego - lag(emprego,n=4L)) %>% 
  ungroup() %>%
  mutate(saldo_rais_p = saldo_rais/populacao) %>% 
  mutate(saldo_rais_p_s = saldo_rais_s/populacao) %>% 
  mutate(saldo_rais_p_y = saldo_rais_y/populacao)

#########################################
#Define variables for DiD Package
#########################################
######For quarter estimates

#Define periods for did package
periods <- unique(df$year_quarter)
periods_df <- data.frame(year_quarter = periods,
                         period_did = 1:length(periods))

#Define current time in periods format
df <- df %>% 
  left_join(periods_df, by="year_quarter",na_matches="never")

#Define date of treatment in periods format
periods_df <- periods_df %>% 
  rename(quarter_entry=year_quarter,
         period_treat = period_did)

df <- df %>% 
  left_join(periods_df, by="quarter_entry",na_matches="never") %>% 
  mutate(period_treat = ifelse(is.na(period_treat),0,period_treat)) %>% 
  mutate(id_municipio = as.numeric(id_municipio))


######For Semester estimates

#Define periods for did package
periods <- unique(df$year_sem)
periods_df <- data.frame(year_sem = periods,
                         period_did_s = 1:length(periods))

#Define current time in periods format
df <- df %>% 
  left_join(periods_df, by="year_sem",na_matches="never")

#Define date of treatment in periods format
periods_df <- periods_df %>% 
  rename(semester_entry=year_sem,
         period_treat_s = period_did_s)

df <- df %>% 
  left_join(periods_df, by="semester_entry",na_matches="never") %>% 
  mutate(period_treat_s = ifelse(is.na(period_treat_s),0,period_treat_s))


######For Year estimates

#Define periods for did package
periods <- unique(df$ano)
periods_df <- data.frame(ano = periods,
                         period_did_y = 1:length(periods))

#Define current time in periods format
df <- df %>% 
  left_join(periods_df, by="ano",na_matches="never")

#Define date of treatment in periods format
periods_df <- periods_df %>% 
  rename(year_entry=ano,
         period_treat_y = period_did_y)

df <- df %>% 
  left_join(periods_df, by="year_entry",na_matches="never") %>% 
  mutate(period_treat_y = ifelse(is.na(period_treat_y),0,period_treat_y))


#Count how many municipalities is in each treatment group
#Overall
df <- df %>% 
  group_by(period_treat_s) %>% 
  mutate(obs_group_s = length(unique(id_municipio))) %>% 
  ungroup() %>% 
  group_by(period_treat_y) %>% 
  mutate(obs_group_y = length(unique(id_municipio))) %>% 
  ungroup()

#Within 100k+ inhabitants
df_aux<-df %>% 
  filter(ano==2015 & mes==12&populacao>=100000) %>% 
  group_by(period_treat_s) %>% 
  mutate(obs_group_s_100 = length(unique(id_municipio))) %>% 
  ungroup() %>% 
  group_by(period_treat_y) %>% 
  mutate(obs_group_y_100 = length(unique(id_municipio))) %>% 
  ungroup() %>% 
  select(id_municipio, obs_group_s_100,obs_group_y_100)

df <- df %>% 
  left_join(df_aux, by="id_municipio", na_matches="never")

#Save
setwd(data_path)
write_parquet(df,"after_rais.parquet")

b=Sys.time()
print(b-a)
rm(list=ls())
gc()




