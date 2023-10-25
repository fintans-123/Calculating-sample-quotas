

library(tidyverse)
library(sjlabelled)
library(questionr)

setwd("C:/Users/fintan.smith/Desktop/Fabians_sampling/50-65/Census 2021")

age_sex_region_dt <- read.csv("age_by_sex_by_region_DATA.csv")


useful <- age_sex_region_dt %>%
  filter(Age..101.categories..Code >=50 & Age..101.categories..Code <=65)%>%
  mutate(age_bands = case_when(
    Age..101.categories..Code >=50 & Age..101.categories..Code <=54 ~ "50-54",
    Age..101.categories..Code >=55 & Age..101.categories..Code <=59 ~ "55-59",
    Age..101.categories..Code >=60 & Age..101.categories..Code <=65 ~ "60-65",
  ))

total <- sum(useful$Observation)

useful_age_sex <- useful%>%
  group_by(age_bands,Sex..2.categories..Code)%>%
  summarise(sum_age_bands=sum(Observation),
            age_sex_quotas_total= sum_age_bands/total,.groups = 'drop')

write.csv(useful_age_sex, "Quotas/age_sex_quotas.csv")

useful_age_region <- useful%>%
  group_by(age_bands, Regions,Sex..2.categories..Code)%>%
  summarise(sum_age_region_sex=sum(Observation),
            age_region_quotas= sum_age_region_sex/total,.groups='drop')%>%
  as.data.frame()


write.csv(useful_age_region,"age_region_sex_quotas.csv")
  
#### ECONOMIC STATUS

age_economic_status_dt <- read.csv("age_region_work_stat_DATA.csv")


useful_econ <- age_economic_status_dt %>%
  filter(Age..101.categories..Code >=50 & Age..101.categories..Code <=65)%>%
  mutate(age_bands = case_when(
    Age..101.categories..Code >=50 & Age..101.categories..Code <=54 ~ "50-54",
    Age..101.categories..Code >=55 & Age..101.categories..Code <=59 ~ "55-59",
    Age..101.categories..Code >=60 & Age..101.categories..Code <=65 ~ "60-65",
  ))

total_econ <- sum(useful_econ$Observation)

useful_econ <- useful_econ %>%
  mutate(profile_work_stat= case_when(
    str_detect(Economic.activity.status.last.week..8.categories.,"Working") ~ "Working",
    str_detect(Economic.activity.status.last.week..8.categories.,"Retired") ~ "Retired",
    str_detect(Economic.activity.status.last.week..8.categories.,"Unemployed") ~ "Unemployed",
    str_detect(Economic.activity.status.last.week..8.categories.,"Student") ~ "Student",
    str_detect(Economic.activity.status.last.week..8.categories.,"Looking") ~ "Other",
    str_detect(Economic.activity.status.last.week..8.categories.,"Other") ~ "Other",
    str_detect(Economic.activity.status.last.week..8.categories.,"Disabled") ~ "Other",
    str_detect(Economic.activity.status.last.week..8.categories.,"Does not apply") ~ "Other",
    str_detect(Economic.activity.status.last.week..8.categories.,"ick") ~ "Other",
  ))


econ_activity_age <- useful_econ %>%
  group_by(age_bands, profile_work_stat)%>%
  summarise(profile_work_stat_sum=sum(Observation),
            econ_acttivty_quotas= profile_work_stat_sum/total_econ,.groups='drop')%>%
  as.data.frame()

write.csv(econ_activity_age, "economic_activity_age_quota.csv")



econ_activity_age_region <- useful_econ %>%
  group_by(age_bands,Regions, profile_work_stat)%>%
  summarise(profile_work_stat_sum=sum(Observation),
            econ_acttivty_quotas= profile_work_stat_sum/total_econ,.groups='drop')%>%
  as.data.frame()


write.csv(econ_activity_age_region, "economic_activity_age_region_quota.csv")



# Past vote quotas --------------------------------------------------------



vote_dt <- sjlabelled::read_spss("BES2019_W20_v24.0.sav", drop.labels = FALSE)

vote_useful <- vote_dt%>%
  select(id,p_past_vote_2019,p_eurefvote, age, gor, polAttention,p_turnout_2019,wt)%>%
  filter(!is.na(p_turnout_2019))

vote_useful <- vote_useful%>%
  mutate(pv_turnout_2= ifelse(p_turnout_2019==0, 94, 1))


  vote_useful <- vote_useful%>%
    mutate(pv_recode= case_when(
                              p_past_vote_2019==1 ~ "Conservative",
                              p_past_vote_2019==2~ "Labour",
                              p_past_vote_2019==3~ "Lib Dem",
                              p_past_vote_2019==4~"SNP",
                              p_past_vote_2019==5 ~ "Other",
                              p_past_vote_2019==6 ~ "Other",
                              p_past_vote_2019==7 ~ "Other",
                              p_past_vote_2019==8 ~ "Other",
                              p_past_vote_2019==9 ~ "Other",
                              p_past_vote_2019==11 ~ "Other",
                              p_past_vote_2019==12 ~ "Brexit Party",
                              p_past_vote_2019==13 ~ "Other",
                              p_past_vote_2019==0 ~ "DNV",
                              p_past_vote_2019==9999 ~ "DNV",
                              pv_turnout_2==94 ~ "DNV"
                              ))%>%
    mutate(gor_for_pv= case_when(
      gor == 1 ~  "North",
      gor==2 ~  "North",
      gor==3 ~  "North",
      gor==4 ~  "Midlands",
      gor==5 ~  "Midlands",
      gor==6 ~  "South",
      gor==7 ~  "London",
      gor==8 ~  "South",
      gor==9 ~  "South",
      gor==10 ~  "Wales",
      gor==11 ~  "Scotland"))%>%
    mutate(pv_by_region= paste(pv_recode, gor_for_pv))%>%
    filter(age >=50 & age <=65)%>%
    filter(!is.na(pv_recode))


  

vote_quota_df <- as.data.frame(wtd.table(vote_useful$pv_by_region, weights = vote_useful$wt))

vote_quota_df <- vote_quota_df%>%
  mutate(quota= (Freq/ sum(Freq))*100)

write.csv(vote_quota_df,"vote_quota_region")

vote_quota_noR_df <- as.data.frame(wtd.table(vote_useful$pv_recode, weights = vote_useful$wt))


vote_quota_noR_df <- vote_quota_noR_df%>%
  mutate(quota= (Freq/ sum(Freq))*100)

write.csv(vote_quota_noR_df,"Quotas/vote_QUOTA_noregion.csv")  


vote_useful_eu<- vote_useful%>%
  filter(!is.na(p_eurefvote))

eu_ref_quota_df <- as.data.frame(wtd.table(vote_useful_eu$p_eurefvote, weights=vote_useful_eu$wt))

eu_ref_quota_df <- eu_ref_quota_df%>%
  mutate(quota= (Freq/ sum(Freq))*100)

write.csv(vote_quota_df,"Quotas/vote_QUOTA.csv")  

write.csv(eu_ref_quota_df,"Quotas/EU_ref_QUOTA.csv")



# Political attention -----------------------------------------------------

polAttention_df <- vote_useful%>%
  mutate(pol_attention_r = case_when(
    polAttention==0 | polAttention==1 | polAttention==2 ~ "Low",
    polAttention==3 | polAttention==4 | polAttention==5 | polAttention==6 | polAttention==7  ~ "Medium",
    polAttention==8 | polAttention==9 | polAttention==10 ~  "High"
  ))



polattention_quota <- as.data.frame(wtd.table(x = polAttention_df$pol_attention_r, weights = polAttention_df$wt))
  



polattention_quota <- polattention_quota%>%
  mutate(quota= (Freq/ sum(Freq))*100)


write.csv(polattention_quota,"Quotas/political_attention_quota.csv")



