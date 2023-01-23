

# install packages

packages <- c("tidyverse", "dplyr", "lubridate", "readr", "ggplot2")
lapply(packages, library, character.only = TRUE)

# clear environment

rm(list = ls())
graphics.off()


# read in

OCA <- read_csv("Data/OCA_supp.csv")
OCA


# create binary vfo flag

OCA <- OCA %>% 
  mutate(arraign_vfo = case_when (larg_vfo != "Not VFO" ~ "VFO",
                               TRUE ~ larg_vfo))

#inspect

OCA %>% 
  count(larg_vfo)
         
# make CC and SC charges/subcharges flaggable

OCA <- OCA %>% 
  mutate(larg_charge = str_squish(toupper(larg_charge))) %>% # squish out whitespace and upper case
  mutate(arraign_charge = str_sub(larg_charge, 4,9)) %>% # extract charge
  mutate(arraign_subcharge = str_sub(larg_charge, 4,12)) %>%  # extract with sub
  mutate(arraign_subcharge = str_trim(arraign_subcharge, side="right")) %>% # trim ws
  mutate(article = str_sub(arraign_charge,1,3))

# inspect

OCA %>% 
  select(arraign_charge, arraign_subcharge, article) %>%
  View()


# make date

OCA <- OCA %>% 
  mutate(arraign_date = make_date(larg_yr, larg_mo))

OCA %>% 
  select(larg_yr, larg_mo, arraign_date) %>% 
  View()

# flag bail-eligible charges

# Text below from Senate bill S1509C, enacting the initial Bail Elimination Act of 2019. 
# A principal stands charged with a qualifying offense for the purposes of this subdivision when he or she stands charged with:
# (a) a felony enumerated in section 70.02 of the penal law, other than burglary in the second degree as defined in subdivision two of section 140.25 of the penal law or robbery in the second degree as defined in subdivision one of section 160.10 of the penal law; 
# (b) a crime involving witness intimidation under section 215.15 of the penal law; 
# (c) a crime involving witness tampering under section 215.11, 215.12 or 215.13 of the penal law; 
# (d) a class A felony defined in the penal law, other than in article two hundred twenty of such law with the exception of section 220.77 of such law; 
# (e) a felony sex offense defined in section 70.80 of the penal law or a crime involving incest as defined in section 255.25, 255.26 or 255.27 of such law, or a misdemeanor defined in article one hundred thirty of such law;
# (f) conspiracy in the second degree as defined in section 105.15 of the penal law, where the underlying allegation of such charge is that the defendant conspired to commit a class A felony defined in article one hundred twenty-five of the penal law; 
# (g) money laundering in support of terrorism in the first degree as defined in section 470.24 of the penal law; money laundering in support of terrorism in the second degree as defined in section 470.23 of the penal law; or a felony crime of terrorism as defined in article four hundred ninety of the penal law, other than the crime defined in section 490.20 of such law;
# (h) criminal contempt in the second degree as defined in subdivision three of section 215.50 of the penal law, criminal contempt in the first degree as defined in subdivision (b), (c) or (d) of section 215.51 of the penal law or aggravated criminal contempt as defined in section 215.52 of the penal law, and the underlying allegation of such charge of criminal contempt in the second degree, criminal contempt in the first degree or aggravated criminal contempt is that the defendant violated a duly served order of protection where the protected party is a member of the defendant's same family or household as defined in subdivision one of section 530.11 of this article; or 
# (i) facilitating a sexual performance by a child with a controlled substance or alcohol as defined in section 263.30 of the penal law, use of a child in a sexual performance as defined in section 263.05 of the penal law or luring a child as defined in subdivision one of section 120.70 of the penal law.


OCA <- OCA %>% 
  mutate(bail_eligible = case_when
         (arraign_vfo == "VFO" & !arraign_subcharge %in% c("140.25 02", "160.10 01") ~ "a",
           arraign_charge == "215.15" ~ "b",
           arraign_charge %in% c("215.11","215.12","215.13") ~ "c",
           arraign_charge %in% c("460.20","125.26","150.20","105.17","490.25","490.45","490.55","135.25","125.27","125.25","220.77", "490.50","130.95","130.96") ~ "d",  #some redundancy here as A-1 already included in VFO flag
           article == "130" | arraign_charge %in% c("255.25", "255.26", "255.27", "230.06") ~ "e",
           arraign_charge %in% c("470.23","470.24","490.10","490.15","490.25","490.30","490.35","490.37","490.40","490.45","490.47","490.50","490.55") ~ "g",
           arraign_subcharge %in% c("215.50 03", "215.51 B", "215.51 BI","215.51 BV", "215.51 C", "215.51 D") ~ "h",
           arraign_charge %in% c("263.30", "263.05") | arraign_subcharge == "120.70 01" ~ "i"))

# Flag any bail-eligible charge

OCA <- OCA %>% 
  mutate(bail_eligible_flag = ifelse(bail_eligible %in% c("a","b","c","d","e","g","h","i"),
                                     "Yes", "No"))

# Inspect bail ineligible cases missed by flag (ie, bail ineligible but bailed). These will be later excluded.

OCA %>% 
  filter((arr_region =="New York City"),
         (arraign_date >="2020-01-01" & arraign_date <="2020-06-01"), 
         (larg_rel_decision != "Disposed at arraign" & !is.na(larg_rel_decision)), 
         (!larg_bail_amount %in% 1:5),
         (bail_eligible_flag=="No" & larg_rel_decision=="Bail-Set")) %>%
          View()

# 426 where flag missed bail-eligibility post-reform; these will be excluded from analysis
  
OCA %>% 
  filter((arr_region =="New York City"),
         (arraign_date >="2020-01-01" & arraign_date <="2020-06-01"), 
         (larg_rel_decision != "Disposed at arraign" & !is.na(larg_rel_decision)), 
         (!larg_bail_amount %in% 1:5),
         (bail_eligible_flag=="No" & larg_rel_decision=="Bail-Set")) %>% 
  count(arraign_subcharge) %>% 
  arrange(desc(n)) %>% 
  print(n=65)


  (!(arraign_date >="2020-01-01" 
   & arraign_date <="2020-06-01" 
   & bail_eligible_flag=="No" 
   & larg_rel_decision=="Bail-Set"))


# indicator for any rearrest with 180 days

OCA <- OCA %>% 
  mutate(any_rearrest_180 = ifelse(rearr_vfo_180 == "Yes" 
                                   | rearr_nonvfo_180 == "Yes" 
                                   | rearr_misd_180 == "Yes",
                                   "Yes", "No"))

# indicator for any rearrest within the tracked period

OCA <- OCA %>% 
  mutate(any_rearrest = ifelse(rearr_vfo == "Yes" 
                               | rearr_nonvfo == "Yes" 
                               | rearr_misd == "Yes",
                               "Yes", "No"))

# generate summary stats for any rearrest within 180 by eligibility 


OCA %>% 
  filter(
    (arr_region =="New York City" 
     & arraign_date >="2019-01-01" 
     & arraign_date <="2020-06-01" 
     & larg_rel_decision != "Disposed at arraign" 
     & !is.na(larg_rel_decision) 
     & !larg_bail_amount %in% 1:5) 
    & (!(arraign_date >= "2020-01-01" & arraign_date<= "2020-06-01" & larg_rel_decision == "Bail-Set" & bail_eligible_flag =="No"))
  ) %>%
  group_by(arraign_date, bail_eligible_flag, any_rearrest_180) %>%
  summarise(N= n()) %>%
  mutate(percent_rearrested_180 = N/sum(N) * 100) %>%
  filter(any_rearrest_180 == "Yes") %>%
  pivot_wider(id_cols = arraign_date, names_from = bail_eligible_flag, values_from = percent_rearrested_180) 

# plot any rearrest within 180

OCA %>% 
  filter(
    (arr_region =="New York City" 
     & arraign_date >="2019-01-01" 
     & arraign_date <="2020-06-01" 
     & larg_rel_decision != "Disposed at arraign" 
     & !is.na(larg_rel_decision) 
     & !larg_bail_amount %in% 1:5) 
    & (!(arraign_date >= "2020-01-01" & arraign_date<= "2020-06-01" & larg_rel_decision == "Bail-Set" & bail_eligible_flag =="No"))
  ) %>%
  group_by(arraign_date, bail_eligible_flag, any_rearrest_180) %>%
  summarise(N= n()) %>%
  mutate(percent_rearrested_180 = N/sum(N) * 100) %>%
  filter(any_rearrest_180 == "Yes") %>%
  ggplot(aes(x = arraign_date,
             y = percent_rearrested_180,
             group=bail_eligible_flag,
             color=bail_eligible_flag))+
  geom_line(aes(linetype=bail_eligible_flag)) + geom_point() + theme_bw() +
  ggtitle("% re-arrested by bail eligibility") +
  xlab("Month") + ylab("% rearrested within 180 days") + geom_vline(xintercept = as.numeric(as.Date("2020-01-01"))) 



# generate summary stats for any re-arrest by elibility 


OCA %>% 
  filter(
    (arr_region =="New York City" 
     & arraign_date >="2019-01-01" 
     & arraign_date <="2020-06-01" 
     & larg_rel_decision != "Disposed at arraign" 
     & !is.na(larg_rel_decision) 
     & !larg_bail_amount %in% 1:5) 
     & (!(arraign_date >= "2020-01-01" & arraign_date<= "2020-06-01" & larg_rel_decision == "Bail-Set" & bail_eligible_flag =="No"))
        ) %>%
  group_by(arraign_date, bail_eligible_flag, any_rearrest) %>%
  summarise(N = n()) %>%
  mutate(percent_rearrested = N/sum(N) * 100) %>%
  filter(any_rearrest == "Yes") %>%
  pivot_wider(id_cols = arraign_date, names_from = bail_eligible_flag, values_from = percent_rearrested) 

# plot any re-arrest

OCA %>% 
  filter(
    (arr_region =="New York City" 
     & arraign_date >="2019-01-01" 
     & arraign_date <="2020-06-01" 
     & larg_rel_decision != "Disposed at arraign" 
     & !is.na(larg_rel_decision) 
     & !larg_bail_amount %in% 1:5) 
     & (!(arraign_date >= "2020-01-01" & arraign_date<= "2020-06-01" & larg_rel_decision == "Bail-Set" & bail_eligible_flag =="No"))
        ) %>%
  group_by(arraign_date, bail_eligible_flag, any_rearrest) %>%
  summarise(N= n()) %>%
  mutate(percent_rearrested = N/sum(N) * 100) %>%
  filter(any_rearrest == "Yes") %>%
  ggplot(aes(x = arraign_date,
             y = percent_rearrested,
             group=bail_eligible_flag,
             color=bail_eligible_flag))+
  geom_line(aes(linetype=bail_eligible_flag)) + geom_point() + theme_bw() +
  ggtitle("% re-arrested by bail eligibility") +
  xlab("Month") + ylab("% rearrested") + geom_vline(xintercept = as.numeric(as.Date("2020-01-01"))) 





OCA %>% 
  count(arr_age) %>% 
  View()

