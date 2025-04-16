library(tidyverse)
library(haven)
nfhs <- read_dta("IAHR52FL.dta")

#Q2:
#df_1 selects all variables between the household id and hv208 plus the wealth index.
df_1<- nfhs %>%
  select(hhid:hv208, hv270)
ncol(df_1)
#48 variables in the data frame df_1

#Q3:
#df_2 holds the household id, the individuals' line numbers, and their education in single years.
names(nfhs)
df_2<- nfhs %>%
  select(hhid, hvidx_01:hvidx_35, hv108_01:hv108_35)
ncol(df_2)
#72 variables in the data frame df_2

#Q4:
df_females <- nfhs %>%
  select(hhid, matches("^ha[0-6]_\\d{2}$"))
names(df_females)
#double check correct output with names()
ncol(df_females)
#78 variables in df_females

#Q5:
df_males <- nfhs %>%
  select(hhid, starts_with("hb"))%>%
  gather(key = "variable", value = "value", -hhid)%>%
  filter(str_detect(variable, "^hb[0-6]_\\d{2}$"))
names(df_males)
#checking output
ncol(df_males)
#3 variables un df_males

#Q6:
education_df2_tidy <- df_2 %>%
  gather(key = "variable", value = "value", -hhid) %>%
  separate(variable, into = c("var", "num"), sep = "_") %>%
  spread(key = var, value = value) %>%
  drop_na(hvidx, hv108) %>%
  select(hhid, hvidx, hv108) %>%
  rename(roster_number = hvidx, education_years = hv108)

  
#Q7:
female_df_tidy <- df_females%>%
  gather(key = "key", value = "value", -hhid)%>%
  separate(key, into = c("var", "num"), sep = "_")%>%
  spread(key = var, value = value)%>%
  select(hhid, roster_number = num, ha1, ha2, ha3)%>%
  rename( age = ha1, weight = ha2, height = ha3)%>%
  drop_na(age, weight, height)
#number of females:
nrow(female_df_tidy)  

#Q8:
male_df_tidy <- df_males%>%
  gather(key = "key", value = "value", -hhid)%>%
  separate(key, into = c("var", "num"), sep = "_")%>%
  spread(key = var, value = value)%>%
  select(hhid, roster_number = num, hb1, hb2, hb3)%>%
  rename( age = hb1, weight = hb2, height = hb3)%>%
  drop_na(age, weight, height)
#number of males:
nrow(male_df_tidy)

#Q9:
combined_females_males <- bind_rows(females_df_tidy, males_df_tidy)
combined_data_1 <- combined_females_males%>%
  left_join(education_df2_tidy, by = c("hhid", "roster_number"))

combined_data_2 <- combined_data_1 %>%
  left_join(df_1, by = "hhid")

#median age for females:
combined_data_2 %>%
  group_by(female)%>%
  summarise(median_age = median(age, na.rm = TRUE))

