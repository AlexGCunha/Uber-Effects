#------------------------------------------------------------------------------------------------
#This code will:
# - Aggregate data by treatment/group status and create graphs
#------------------------------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(readxl)
library(arrow)

rm(list=ls())
data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Output"

setwd(data_path)
df <- read_parquet("after_rais.parquet")

#Filter the same municipalities that we run regressions
df <- df %>% 
  filter(!is.na(year_entry)& mes %in% c(6,12)&problem==0 & obs_group_s >5)
gc()
#Group by treatment group and date
df <- df %>% 
  group_by(ano, mes,period_treat_s) %>% 
  summarise(semester_entry = first(semester_entry),
            emprego = sum(emprego),
            homens = sum(homens),
            mulheres = sum(mulheres),
            brancos = sum(brancos),
            n_brancos = sum(n_brancos)) %>% 
  ungroup() %>% 
  mutate(m_aux = paste0(0,mes)) %>% 
  mutate(m_aux = substr(m_aux, nchar(m_aux)-1, nchar(m_aux))) %>% 
  mutate(date = as.Date(paste0("01",m_aux,ano), format="%d%m%Y")) %>% 
  filter(date > as.Date("31122005", format = "%d%m%Y"))

#Create variables in z-score
df <- df %>% 
  group_by(period_treat_s) %>% 
    mutate(emprego_z = (emprego - mean(emprego))/sd(emprego)) %>% 
    mutate(homens_z = (homens - mean(homens))/sd(homens)) %>% 
    mutate(mulheres_z = (mulheres - mean(mulheres))/sd(mulheres)) %>% 
    mutate(brancos_z = (brancos - mean(brancos))/sd(brancos)) %>% 
    mutate(n_brancos_z = (n_brancos - mean(n_brancos))/sd(n_brancos)) %>% 
  ungroup() %>% 
  mutate(period_treat_s=as.character(period_treat_s))

################################
#Total Employment
################################

#Plot 20161
ggplot(df %>% filter(period_treat_s %in% c(0,23)), aes(x=date, y=emprego_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01012016", format="%d%m%Y"))
  


#Plot 20162
ggplot(df %>% filter(period_treat_s %in% c(0,24)), aes(x=date, y=emprego_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01072016", format="%d%m%Y"))


#Plot 20171
ggplot(df %>% filter(period_treat_s %in% c(0,25)), aes(x=date, y=emprego_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01012017", format="%d%m%Y"))





################################
#Women
################################

#Plot 20161
ggplot(df %>% filter(period_treat_s %in% c(0,23)), aes(x=date, y=mulheres_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01012016", format="%d%m%Y"))



#Plot 20162
ggplot(df %>% filter(period_treat_s %in% c(0,24)), aes(x=date, y=mulheres_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01072016", format="%d%m%Y"))


#Plot 20171
ggplot(df %>% filter(period_treat_s %in% c(0,25)), aes(x=date, y=mulheres_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01012017", format="%d%m%Y"))



################################
#Men
################################

#Plot 20161
ggplot(df %>% filter(period_treat_s %in% c(0,23)), aes(x=date, y=homens_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01012016", format="%d%m%Y"))



#Plot 20162
ggplot(df %>% filter(period_treat_s %in% c(0,24)), aes(x=date, y=homens_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01072016", format="%d%m%Y"))


#Plot 20171
ggplot(df %>% filter(period_treat_s %in% c(0,25)), aes(x=date, y=homens_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01012017", format="%d%m%Y"))




################################
#Brancos
################################

#Plot 20161
ggplot(df %>% filter(period_treat_s %in% c(0,23)), aes(x=date, y=brancos_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01012016", format="%d%m%Y"))



#Plot 20162
ggplot(df %>% filter(period_treat_s %in% c(0,24)), aes(x=date, y=brancos_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01072016", format="%d%m%Y"))


#Plot 20171
ggplot(df %>% filter(period_treat_s %in% c(0,25)), aes(x=date, y=brancos_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01012017", format="%d%m%Y"))



################################
#Non White 
################################

#Plot 20161
ggplot(df %>% filter(period_treat_s %in% c(0,23)), aes(x=date, y=n_brancos_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01012016", format="%d%m%Y"))



#Plot 20162
ggplot(df %>% filter(period_treat_s %in% c(0,24)), aes(x=date, y=n_brancos_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01072016", format="%d%m%Y"))


#Plot 20171
ggplot(df %>% filter(period_treat_s %in% c(0,25)), aes(x=date, y=n_brancos_z, color=period_treat_s, group=period_treat_s))+
  geom_rect(aes(xmin =as.Date("01042014", format="%d%m%Y"),xmax =as.Date("31122016", format="%d%m%Y"),
                ymin=-2, ymax=2), fill = "grey90", color="grey90")+theme_minimal()+
  geom_point()+geom_line()+geom_vline(xintercept = as.Date("01012017", format="%d%m%Y"))




