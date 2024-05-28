################################################################################
#This code will:
# - Compare specifications for the control group to define which is the best
################################################################################
library(tidyverse)
library(arrow)
library(cowplot)
library(did)
library(MatchIt)

data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Output"

setwd(data_path)
df = read_parquet("merged_month_munic.parquet") %>%
  filter(ano >=2010& ano <=2019)


#Add UF dummies
df = df %>% 
  mutate(no = ifelse(region == 1, 1,0),
         ne = ifelse(region == 2, 1,0),
         se = ifelse(region == 3, 1,0),
         su = ifelse(region == 4, 1,0),
         co = ifelse(region == 5, 1,0),)

#Take only one observation per municipality
reduced = df %>% 
  select(id_municipio, period_treat_s, populacao_14, pib_p_14, shomens_14, 
         sbrancos_14, shs_14, no, ne, se, su, co, region) %>% 
  mutate(treated = ifelse(period_treat_s ==  0, 0, 1)) %>% 
  group_by(id_municipio) %>% 
  summarise_all(first) %>% 
  ungroup()

#Simple logistic regression
m1 = glm(treated ~  pib_p_14 + populacao_14 + shomens_14 + sbrancos_14 + shs_14 +
           no + ne + se + su,
        data = reduced ,
        family = binomial(link = "logit"))

summary(m1)
