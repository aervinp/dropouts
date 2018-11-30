library(tidyverse)

# Input data ---------------------

pop <- read_delim(here::here("data", "eph2017_pobulacion.csv"), delim = ";")
house <- read_delim(here::here("data", "eph2017_vivienda.csv"), delim = ";")

# Select and rename variables -------------------------------
pop <- 
  pop %>% 
  rename(age = P02, sex = P06, language = ED01, years_school = añoest,
         hh_relation = P03, line_mother = P05M, line_father = P05P,
         level_grade_father = NPAD, level_grade_mother = NMAD,
         birth_day = P08D, birth_month = P08M, birth_year = P08A,
         literacy = ED02, enrolled_school = ED08, reason_not_enrolled = ED10,
         dpto_born = P10A, area_born = P10Z, survey_line = L02)

house <- 
  house %>% 
  mutate(hhid = paste(UPM, NVIVI, NHOGA, sep = "_")) %>% 
  rename(floor_type = V04, wall_type = V05, roof_type = V03)

# Housing -------------------------

advanced_housing <- 
  house %>% 
  mutate(adv_housing = case_when((floor_type != 1 & wall_type %in% c(1:5) & roof_type %in% c(1:6)) ~ 1,
                                      TRUE ~ 0)) %>% 
  select(hhid, adv_housing)

rm(house)

# Population file   ------------------------------------
pop <- 
  pop %>% 
  select(c("UPM", "NVIVI", "NHOGA", "DPTO", "AREA", 
           "age", "sex", "language", "years_school", "hh_relation",
           "line_mother", "line_father", "level_grade_father", 
           "level_grade_mother", "birth_day", "birth_month", "birth_year",
           "literacy", "enrolled_school", "reason_not_enrolled",
           "dpto_born", "area_born", "survey_line", "FEX")) %>% 
  mutate(hhid = paste(UPM, NVIVI, NHOGA, sep = "_"),
         person_id = paste(UPM, NVIVI, NHOGA, survey_line, sep = "_"),
         mother_id = paste(UPM, NVIVI, NHOGA, line_mother, sep = "_"),
         father_id = paste(UPM, NVIVI, NHOGA, line_father, sep = "_"),
         female = as.numeric(sex == 6),
         guarani = as.numeric(language == "1"),
         spanish = as.numeric(language == "3"),
         bilingual = as.numeric(language == "2"),
         years_school = as.integer(years_school),
         illiterate = as.numeric(literacy == "6"),
         enrolled_school = as.numeric(as.integer(enrolled_school) %in% c(1:18))) %>% 
  mutate(years_school = na_if(years_school, 99)) %>% 
  left_join(advanced_housing)

# Child file: select sample of observations based on born in Paraguay, language spoken, and age --------------------
children <- 
  pop %>% 
  filter((dpto_born %in% c(0:17)) & (age %in% c(7:18)) & as.numeric(language %in% c(1:3))) %>% 
  rename(child_id = person_id) %>% 
  mutate(child_present = 1)

# enrollment, delays, and dropouts
children <- 
  children %>% 
  mutate(never_started_school = as.numeric(years_school == 0 & enrolled_school == 0),
         delayed_school = as.numeric(years_school<9 & ((age-years_school) > 7) & enrolled_school == 1),
         dropout = as.numeric(years_school < 12 & enrolled_school == 0)) %>% 
  select(child_id, never_started_school, years_school, enrolled_school, delayed_school, age, dropout, everything())

#birth order
children <- 
  children %>% 
  group_by(hhid) %>% 
  arrange(desc(age), .by_group = TRUE) %>% 
  mutate(birth_order = row_number()) %>% 
  ungroup()

#twins
child_twins <- 
  children %>%
  janitor::get_dupes(hhid, birth_day, birth_month, birth_year) %>% 
  distinct(hhid, birth_day, birth_month, birth_year) %>% 
  mutate(twins = 1)

children <- 
  children %>% 
  left_join(child_twins) %>%
  mutate(twins = replace_na(twins, 0))

rm(child_twins)

# Mother file ------------------------------------------
mother <- 
  pop %>% 
  select(mother_id, line_mother) %>% 
  filter(line_mother != 0) %>% 
  distinct(mother_id) %>% 
  mutate(mother_present = 1) %>% 
  rename(person_id = mother_id) %>% 
  left_join(pop) %>% 
  rename(mother_age = age, mother_illiterate = illiterate, mother_guarani = guarani,
         mother_spanish = spanish, mother_bilingual = bilingual, mother_dpto_born = dpto_born,
         mother_area_born = area_born, mother_years_school = years_school) %>% 
  select(person_id, mother_present, mother_age, mother_illiterate, mother_guarani,
         mother_spanish, mother_bilingual, mother_dpto_born, mother_area_born, mother_years_school) %>% 
  rename(mother_id = person_id)
  
# Father file  ---------------------------------------
father <- 
  pop %>% 
  select(father_id, line_father) %>% 
  filter(line_father != 0) %>% 
  distinct(father_id) %>% 
  mutate(father_present = 1) %>% 
  rename(person_id = father_id) %>% 
  left_join(pop) %>% 
  rename(father_age = age, father_illiterate = illiterate, father_guarani = guarani,
         father_spanish = spanish, father_bilingual = bilingual, father_dpto_born = dpto_born,
         father_area_born = area_born, father_years_school = years_school) %>% 
  select(person_id, father_present, father_age, father_illiterate, father_guarani,
         father_spanish, father_bilingual, father_dpto_born, father_area_born, father_years_school) %>% 
  rename(father_id = person_id)


# Merge mother and father information to the child ---------------------------------
children <- 
  children %>% 
  left_join(mother) %>% 
  left_join(father) %>% 
  replace_na(list(mother_present = 0, father_present = 0, child_present = 0)) %>% 
  mutate(caretaker = case_when(mother_present == 1 & father_present == 1 ~ "joint",
                               mother_present == 0 & father_present == 1 ~ "father",
                               mother_present == 1 & father_present == 0 ~ "mother",
                               mother_present == 0 & father_present == 0 ~ "other"))

rm(mother)
rm(father)

# Caretaker info -------------------------------------

# If neither father or mother is in the house use female max and male max.
# check how many don't have mothers
table(children$caretaker)

hh_females <- 
  pop %>% 
  filter(female == 1 & age >= 14) %>% 
  group_by(hhid) %>% 
  summarize(femcare_age = max(age), femcare_illiterate = max(illiterate), femcare_language = min(language),
            femcare_dpto_born = min(dpto_born), femcare_area_born = max(area_born), femcare_years_school = mean(years_school, na.rm = TRUE)) %>% 
  mutate(femcare_guarani = as.numeric(femcare_language == 1),
         femcare_spanish = as.numeric(femcare_language == 3),
         femcare_bilingual = as.numeric(femcare_language == 2),
         femcare_present = 1) %>% 
  ungroup()

hh_males <- 
  pop %>% 
  filter(female == 0 & age >= 14) %>% 
  group_by(hhid) %>% 
  summarize(mencare_age = max(age), mencare_illiterate = max(illiterate), mencare_language = min(language),
            mencare_dpto_born = min(dpto_born), mencare_area_born = max(area_born), mencare_years_school = mean(years_school, na.rm = TRUE)) %>% 
  mutate(mencare_guarani = as.numeric(mencare_language == 1),
         mencare_spanish = as.numeric(mencare_language == 3),
         mencare_bilingual = as.numeric(mencare_language == 2),
         mencare_present = 1) %>% 
  ungroup()

# merge to children
children <- 
  children %>% 
  left_join(hh_females) %>% 
  left_join(hh_males) %>% 
  replace_na(list(femcare_present = 0, mencare_present = 0))

# Fill in mother and fathers info with hh_females and hh_males  -------------------------
children_fill <- 
  children %>% 
  mutate(mother_age = ifelse(is.na(mother_age), femcare_age, mother_age),
         mother_illiterate = ifelse(is.na(mother_illiterate), femcare_illiterate, mother_illiterate),
         mother_guarani = ifelse(is.na(mother_guarani), femcare_guarani, mother_guarani),
         mother_spanish = ifelse(is.na(mother_spanish), femcare_spanish, mother_spanish),
         mother_bilingual = ifelse(is.na(mother_bilingual), femcare_bilingual, mother_bilingual),
         mother_dpto_born  = ifelse(is.na(mother_dpto_born), femcare_dpto_born, mother_dpto_born),
         mother_area_born = ifelse(is.na(mother_area_born), femcare_area_born, mother_area_born),
         mother_years_school = ifelse(is.na(mother_years_school), femcare_years_school, mother_years_school),
         father_age = ifelse(is.na(father_age), mencare_age, father_age),
         father_illiterate = ifelse(is.na(father_illiterate), mencare_illiterate, father_illiterate),
         father_guarani = ifelse(is.na(father_guarani), mencare_guarani, father_guarani),
         father_spanish = ifelse(is.na(father_spanish), mencare_spanish, father_spanish),
         father_bilingual = ifelse(is.na(father_bilingual), mencare_bilingual, father_bilingual),
         father_dpto_born  = ifelse(is.na(father_dpto_born), mencare_dpto_born, father_dpto_born),
         father_area_born = ifelse(is.na(father_area_born), mencare_area_born, father_area_born),
         father_years_school = ifelse(is.na(father_years_school), mencare_years_school, father_years_school)) %>% 
  mutate(mother_age = ifelse(is.na(mother_age), mencare_age, mother_age),
         mother_illiterate = ifelse(is.na(mother_illiterate), mencare_illiterate, mother_illiterate),
         mother_guarani = ifelse(is.na(mother_guarani), mencare_guarani, mother_guarani),
         mother_spanish = ifelse(is.na(mother_spanish), mencare_spanish, mother_spanish),
         mother_bilingual = ifelse(is.na(mother_bilingual), mencare_bilingual, mother_bilingual),
         mother_dpto_born  = ifelse(is.na(mother_dpto_born), mencare_dpto_born, mother_dpto_born),
         mother_area_born = ifelse(is.na(mother_area_born), mencare_area_born, mother_area_born),
         mother_years_school = ifelse(is.na(mother_years_school), mencare_years_school, mother_years_school),
         father_age = ifelse(is.na(father_age), femcare_age, father_age),
         father_illiterate = ifelse(is.na(father_illiterate), femcare_illiterate, father_illiterate),
         father_guarani = ifelse(is.na(father_guarani), femcare_guarani, father_guarani),
         father_spanish = ifelse(is.na(father_spanish), femcare_spanish, father_spanish),
         father_bilingual = ifelse(is.na(father_bilingual), femcare_bilingual, father_bilingual),
         father_dpto_born  = ifelse(is.na(father_dpto_born), femcare_dpto_born, father_dpto_born),
         father_area_born = ifelse(is.na(father_area_born), femcare_area_born, father_area_born),
         father_years_school = ifelse(is.na(father_years_school), femcare_years_school, father_years_school))

rm(hh_males)
rm(hh_females)

#if wanted to fill in missings with medians, but seem better to use household information as above.
# mutate_at(vars(mother_age, mother_years_school, father_age, father_years_school), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) %>%

# check for variables with NA's
dropvars <- names(children_fill %>% select_if(function(x) any(is.na(x))))

children_fill <- 
  children_fill %>% 
  select(-dropvars, hh_relation, line_mother, line_father, level_grade_father, level_grade_mother,
         survey_line, mother_id, father_id)


# Prep analysis ---------------------------------------------------------------
names(children_fill)

children_fill <- 
  children_fill %>% 
  mutate(language = factor(language, labels = c("Guarani", "Bilingual", "Spanish")))


# KM Analysis ------------------------------
#kaplan meier survival curve
library(survival)
library(ggfortify)
km <- with(children_fill, Surv(years_school, dropout))

#simplest possible model: kaplan meier to estimate probability of survival time
km_fit <- survfit(Surv(years_school, dropout) ~ 1, data=children_fill)
summary(km_fit, times = c(1:13))

autoplot(km_fit)

#survival by language
km_lang_fit <- survfit(Surv(years_school, dropout) ~ language, data=children_fill)
autoplot(km_lang_fit)

#survival by caretaker
km_caretaker_fit <- survfit(Surv(years_school, dropout) ~ caretaker, data=children_fill)
autoplot(km_caretaker_fit)

#survival by birthorder #need to record 5 or 6+
km_order_fit <- survfit(Surv(years_school, dropout) ~ birth_order, data=children_fill)
autoplot(km_order_fit)



#some exploration
# p <- qplot(data = children_fill, mother_age, father_age, xlab = "", ylab = "")
# p1 <- p + geom_smooth(method = "loess", size = 1.5)
# 
# p2 <- qplot(data = children_fill, mother_age, dropout, xlab = "", ylab = "")
# p3 <- p2 + geom_smooth(method = "loess", size = 1.5)
# 
# summary(lm(children_fill$mother_age ~ children_fill$father_age))

