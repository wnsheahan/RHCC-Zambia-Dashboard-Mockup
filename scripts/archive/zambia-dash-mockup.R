#Zambia data mock-up

library(tidyverse)
library(janitor)
library(lubridate)
library(wesanderson)

palette = wes_palette("Darjeeling1")[c(1:3,5)]

#read in example zambia data
#clean names and make sure district is assigned
chisamba <- read_csv("data/raw/zambia/csv/Chisamba_data_sept25.csv") %>%
  janitor::clean_names() %>%
  mutate(district = "Chisamba")
ngabwe <- read_csv("data/raw/zambia/csv/Ngabwe_data_sept25.csv") %>%
  janitor::clean_names()%>%
  mutate(district = "Ngabwe")
kasama <- read_csv("data/raw/zambia/csv/Kasama_data_sept25.csv") %>%
  janitor::clean_names()%>%
  mutate(district = "Kasama")

#bind all csv files together into one analysis file
combined_data <- rbind(chisamba, ngabwe, kasama)

#rename period date and facility name
combined_data <- combined_data %>%
  mutate(period = if_else(periodname == "August 2025", ymd("2025-08-01"), ymd("2025-07-01"))) %>%
  rename(facility = organisationunitname) %>%
  filter(facility != "zm Zambia Ministry of Health",
         facility != "no Northern Province",
         facility != "no Kasama District",
         facility != "ce Chisamba District",
         facility != "ce Ngabwe District")

#MISSING MORTALITY DATA
# **RHCC1** - Under-5 Mortality per 1,000 Live Births

#MISSING necessary facility vs chw case data
# **RHCC2** - % of *Malaria* Cases Managed in the Community

combined_data %>%
  dplyr::select(district, facility, period, rhcc_positive_malaria_cases_5yrs, rhcc_malaria_cases_referred_5yrs,
                rhcc_malaria_cases_tested_5yrs, rhcc_malaria_cases_treated_5yrs) %>%
  pivot_longer(cols = -c(district, facility, period), names_to = "stat", values_to = "count") %>%
  group_by(district, period, stat) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = period, y = count, fill = stat), position = "dodge") +
  facet_wrap(vars(district)) +
  theme_bw() +
  theme(text = element_text(size = 14), legend.position = "bottom")

#MISSING necessary facility vs chw case data
# **RHCC2** - % of *Diarrhoea* Cases Managed in the Community
# 
# **RHCC2** - % of *Pneumonia* Cases Managed in the Community


#Training data
training_data <- openxlsx::read.xlsx("data/raw/zambia/CHW_Training info_Tracking_RHCC 12_Aug 2025.xlsx", 
                                     sheet = "Aggregates_ICCM_training_RHCC") %>%
  janitor::clean_names()

training_data_grp <- training_data %>%
  filter(!is.na(province)) %>%
  mutate(male_chws_equipped = if_else(work_tools_bicycles_etc_handover_yes_no == "Yes", actual_number_of_male_ch_ws_trained, 0),
         female_chws_equipped = if_else(work_tools_bicycles_etc_handover_yes_no == "Yes", actual_number_of_female_ch_ws_trained, 0)) %>%
  group_by(province, district) %>%
  summarise(male_chws_trained = sum(actual_number_of_male_ch_ws_trained, na.rm = TRUE),
            female_chws_trained = sum(actual_number_of_female_ch_ws_trained, na.rm = TRUE),
            male_chws_equipped = sum(male_chws_equipped, na.rm = TRUE),
            female_chws_equipped = sum(female_chws_equipped, na.rm = TRUE))

# **RHCC3** - # of CHWS Trained in iCCM
training_data_grp %>%
  pivot_longer(cols = male_chws_trained:female_chws_equipped, names_to = "stat", values_to = "count") %>%
  filter(grepl("trained", stat) == TRUE) %>%
  ggplot() +
  geom_col(aes(x = district, y = count, fill = stat),
           position = "dodge") +
  scale_fill_manual(values = palette,
                    labels = c("female_chws_trained" = "Female CHWs",
                               "male_chws_trained" = "Male CHWs")) +
  theme_bw() +
  labs(x = "District", y = "# CHWs Trained")

# **RHCC4** - # of CHWs Equipped According to National Guidelines
training_data_grp %>%
  pivot_longer(cols = male_chws_trained:female_chws_equipped, names_to = "stat", values_to = "count") %>%
  filter(grepl("equipped", stat) == TRUE) %>%
  ggplot() +
  geom_col(aes(x = district, y = count, fill = stat),
           position = "dodge") +
  scale_fill_manual(values = palette,
                    labels = c("female_chws_equipped" = "Female CHWs",
                               "male_chws_equipped" = "Male CHWs")) +
  theme_bw() +
  labs(x = "District", y = "# CHWs Equipped")

training_data_grp %>%
  mutate(pct_male_equipped = round(male_chws_equipped/male_chws_trained * 100, 2),
         pct_female_equipped = round(female_chws_equipped/female_chws_trained * 100, 2)) %>%
  pivot_longer(cols = pct_male_equipped:pct_female_equipped, names_to = "stat", values_to = "count") %>%
  ggplot() +
  geom_col(aes(x = district, y = count, fill = stat),
           position = "dodge") +
  scale_fill_manual(values = palette,
                    labels = c("pct_female_equipped" = "Female CHWs",
                               "pct_male_equipped" = "Male CHWs")) +
  theme_bw() +
  labs(x = "District", y = "% CHWs Equipped")

#Todd's plot
all_zam <- training_data_grp %>%
  pivot_longer(cols = male_chws_trained:female_chws_equipped, names_to = "stat", values_to = "count") %>%
  filter(grepl("trained", stat) == TRUE) %>%
  mutate(district = "Zambia") %>%
  group_by(district) %>%
  summarise(chws_trained = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(target_chws = 2086)

training_data_grp %>%
  pivot_longer(cols = male_chws_trained:female_chws_equipped, names_to = "stat", values_to = "count") %>%
  filter(grepl("trained", stat) == TRUE) %>%
  group_by(district) %>%
  summarise(chws_trained = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(target_chws = case_when(grepl("Kasama", district) == TRUE ~ 747,
                                 grepl("Chisamba", district) == TRUE ~ 346,
                                 grepl("Ngabwe", district) == TRUE ~ 94,
                                 grepl("Luwingu", district) == TRUE ~ 226,
                                 grepl("Chilubi", district) == TRUE ~ 241)) %>%
  rbind(all_zam) %>%
  mutate(pct_trained = round(chws_trained/target_chws * 100)) %>%
  ggplot() +
  geom_col(aes(x = district, y = target_chws), fill = "lightgray", width = 0.5) +
  geom_col(aes(x = district, y = chws_trained), fill = "steelblue1", width = 0.6, alpha = 0.3) +
  geom_text(aes(x = district, y = chws_trained + 100, label = paste0(pct_trained, "%")), 
            color = "black", size = 4) +
  #coord_flip() +
  theme_bw() +
  labs(x = "District", y = "Number of CHWs Trained (% of Target)")



#MISSING CHW vs Facility, just have total per facility referred
#Also would need the total sick children to get percent?
# **RHCC9** - % of Sick Child Cases Recommended for Referral by CHW
#### Assuming this is actually at CHW level we can do this
managed_dat <- combined_data %>%
  dplyr::select(district, facility, period, periodname, diarrhoea_5_female, diarrhoea_5_male, pneumonia_female_5, pneumonia_male_5, rhcc_malaria_cases_treated_5yrs) %>%
  pivot_longer(cols = contains("5"), names_to = "stat2", values_to = "managed_count") %>%
  filter(!is.na(managed_count)) %>%
  mutate(age_group = if_else(grepl("5", stat2) == TRUE, "Under 5", "All Ages"),
         gender = case_when(grepl("_female", stat2) == TRUE ~ "Female",
                            grepl("_male", stat2) == TRUE ~ "Male",
                            TRUE ~ "Combined"),
         disease = case_when(grepl("malaria", stat2) == TRUE ~ "Malaria",
                             grepl("pneumonia", stat2) == TRUE ~ "Pneumonia",
                             grepl("diarrhoea", stat2) == TRUE ~ "Diarrhoea"))
  
referral_dat <- combined_data %>% dplyr::select(district, facility, period, periodname, contains("referred")) %>%
  pivot_longer(cols = contains("referred"), names_to = "stat", values_to = "referral_count") %>%
  filter(!is.na(referral_count)) %>%
  mutate(age_group = if_else(grepl("5", stat) == TRUE, "Under 5", "All Ages"),
         gender = case_when(grepl("_female", stat) == TRUE ~ "Female",
                            grepl("_male", stat) == TRUE ~ "Male",
                            TRUE ~ "Combined"),
         disease = case_when(grepl("malaria", stat) == TRUE ~ "Malaria",
                             grepl("pneumonia", stat) == TRUE ~ "Pneumonia",
                             grepl("diarrhoea", stat) == TRUE ~ "Diarrhoea"))

plotting_dat <- referral_dat %>%
  left_join(managed_dat, by = c("district", "facility", "period", "periodname", "age_group", "gender", "disease")) %>%
  #only under 5s
  filter(!is.na(stat2)) %>%
  group_by(district, period, gender, disease) %>%
  summarise(referral_count = sum(referral_count, na.rm = TRUE), 
            managed_count = sum(managed_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop_referred = round(referral_count/managed_count, 2))

sick_kids_plotting_dat <- referral_dat %>%
  left_join(managed_dat, by = c("district", "facility", "period", "periodname", "age_group", "gender", "disease")) %>%
  #only under 5s
  filter(!is.na(stat2)) %>%
  group_by(district, period, gender, disease) %>%
  summarise(referral_count = sum(referral_count, na.rm = TRUE), 
            managed_count = sum(managed_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop_referred = round(referral_count/managed_count, 2))


#malaria
sick_kids_plotting_dat %>%
  filter(disease == "Malaria") %>%
  ggplot() +
  geom_col(aes(x = as.factor(period), y = prop_referred, fill = gender), position = "dodge") +
  scale_y_continuous(lim = c(0,1), expand = c(0,0)) +
  #geom_hline(aes(yintercept = 1), col = "red", lty = "dashed") +
  labs(x = "Period", y = "Proportion of Sick Under-5 Children Referred") +
  theme_bw() +
  facet_wrap(vars(district))

#pneumonia
plotting_dat %>%
  filter(disease == "Pneumonia") %>%
  ggplot() +
  geom_col(aes(x = as.factor(period), y = prop_referred, fill = gender), position = "dodge") +
  scale_y_continuous(lim = c(0,1), expand = c(0,0)) +
  #geom_hline(aes(yintercept = 1), col = "red", lty = "dashed") +
  labs(x = "Period", y = "Proportion of Sick Under-5 Children Referred") +
  theme_bw() +
  facet_wrap(vars(district))

#diarrhoea
plotting_dat %>%
  filter(disease == "Diarrhoea") %>%
  ggplot() +
  geom_col(aes(x = as.factor(period), y = prop_referred, fill = gender), position = "dodge") +
  scale_y_continuous(lim = c(0,1), expand = c(0,0)) +
  #geom_hline(aes(yintercept = 1), col = "red", lty = "dashed") +
  labs(x = "Period", y = "Proportion of Sick Under-5 Children Referred") +
  theme_bw() +
  facet_wrap(vars(district))


#MISSING Differentiation between CHW and Facility for everything except ACT/RDTs
# **RHCC11** - % of iCCM Sites/Outreach Posts with No Stock-outs of Key *Malaria* Medicines and Diagnostics in the Past Month
combined_data %>%
  replace(is.na(.), 0) %>%
  mutate(act_stockout = if_else(chw_act_stocks_6s_balance_on_hand + chw_act_stocks_12s_balance_on_hand + chw_act_stocks_18s_balance_on_hand + chw_act_stocks_24s_balance_on_hand < 1, 1, 0)) %>%
  group_by(district, period) %>%
  summarise(n_facilities = n(),
            n_stockouts = sum(act_stockout, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent_stocked_out = round(n_stockouts/n_facilities * 100, 2),
         percent_not_stocked_out = 100-percent_stocked_out) %>%
  ggplot() +
  #geom_line(aes(x = period, y = percent_not_stocked_out, color = district)) +
  #scale_color_manual(values = palette) +
  geom_col(aes(x = as.factor(period), y = percent_not_stocked_out, fill = district),
           position = "dodge") +
  scale_fill_manual(values = palette) +
  scale_y_continuous(lim = c(0,100), expand = c(0,0)) +
  labs(x = "Month", y = "Percent of Facilities NOT \nStocked Out of ACTs") +
  theme_bw() +
  facet_wrap(vars(district))

# **RHCC11** - % of iCCM Sites/Outreach Posts with No Stock-outs of Key *Pneumonia* Medicines and Diagnostics in the Past Month
# **RHCC11** - % of iCCM Sites/Outreach Posts with No Stock-outs of Key *Diarrhoea* Medicines and Diagnostics in the Past Month
combined_data %>%
  replace(is.na(.), 0) %>%
  mutate(amoxyl_stockout = if_else(amoxyl < 1, 1, 0),
         zinc_stockout = if_else(zinc < 1, 1, 0),
         ors_stockout = if_else(ors < 1, 1, 0),
         panadol_stockout = if_else(panadol < 1, 1, 0)) %>%
  group_by(district, period) %>%
  summarise(n_facilities = n(),
            n_amoxyl_stockouts = sum(amoxyl_stockout, na.rm = TRUE),
            n_zinc_stockouts = sum(zinc_stockout, na.rm = TRUE),
            n_ors_stockouts = sum(ors_stockout, na.rm = TRUE),
            n_panadol_stockouts = sum(panadol_stockout, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent_amoxyl_stocked_out = round(n_amoxyl_stockouts/n_facilities * 100, 2),
         percent_amoxyl_not_stocked_out = 100-percent_amoxyl_stocked_out,
         percent_zinc_stocked_out = round(n_zinc_stockouts/n_facilities * 100, 2),
         percent_zinc_not_stocked_out = 100-percent_zinc_stocked_out,
         percent_ors_stocked_out = round(n_ors_stockouts/n_facilities * 100, 2),
         percent_ors_not_stocked_out = 100-percent_ors_stocked_out,
         percent_panadol_stocked_out = round(n_panadol_stockouts/n_facilities * 100, 2),
         percent_panadol_not_stocked_out = 100-percent_panadol_stocked_out) %>%
  pivot_longer(cols = c(percent_amoxyl_not_stocked_out, percent_zinc_not_stocked_out,
               percent_ors_not_stocked_out, percent_panadol_not_stocked_out),
               names_to = "stat", values_to = "pct") %>%
  ggplot() +
  geom_col(aes(x = as.factor(period), y = pct, fill = stat),
           position = "dodge") +
  scale_fill_manual(values = palette,
                    labels = c("amoxyl", "zinc", "ORS", "panadol")) +
  scale_y_continuous(lim = c(0,100), expand = c(0,0)) +
  labs(x = "Month", y = "Percent of Facilities NOT \nStocked Out of Medications") +
  theme_bw() +
  facet_wrap(vars(district))




# **RHCC12** - % of CHWs Who Had No Stock-outs of *Malaria* Commodities During the Reporting Period
# 
# **RHCC12** - % of CHWs Who Had No Stock-outs of *Pneumonia* Commodities During the Reporting Period
# 
# **RHCC12** - % of CHWs Who Had No Stock-outs of *Diarrhoea* Commodities During the Reporting Period


# **RHCC15** - % of Complete Reports Submitted
#MISSING Completeness, Accuracy, and Timeliness, just have actual vs expected (actual all 0)
combined_data %>%
  group_by(district, period) %>%
  summarise(actual = sum(rhcc_actual_reports, na.rm = TRUE),
            expected = sum(rhcc_expected_reports, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(completeness = round(actual/expected, 2)) %>%
  ggplot() +
  geom_col(aes(x = district, y = completeness))  +
  theme_bw() +
  labs(x = "District", y = "Actual Reports / Expected Reports")
