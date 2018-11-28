library(tidyverse)

pop <- read_delim(here::here("data", "eph2017_pobulacion.csv"), delim = ";")
house <- read_delim(here::here("data", "eph2017_vivienda.csv"), delim = ";")

#rename
pop <- 
  pop %>% 
  rename(age = P02, sex = P06, language = ED01, years_scho = añoest,
         hh_relation = P03, line_mother = P05M, line_father = P05P,
         level_grade_father = NPAD, level_grade_mother = NMAD,
         birth_day = P08D, birth_month = P08M, birth_year = P08A,
         literacy = ED02, enrolled_school = ED08, reason_not_enrolled = ED10,
         dpto_born = P10A, area_born = P10Z, survey_line = L02)

house <- 
  house %>% 
  rename(floor_type = V04, wall_type = V05, roof_type = V03)

#cleaning population file
pop <- 
  pop %>% 
  select(c("UPM", "NVIVI", "NHOGA", "DPTO", "AREA", 
           "age", "sex", "language", "years_scho", "hh_relation",
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
         years_scho = as.integer(years_scho),
         illiterate = as.numeric(literacy == "6"),
         enrolled_school = as.numeric(as.integer(enrolled_school) %in% c(1:18)))

#child file: select sample of observations based on born in Paraguay, language spoken, and age
children <- 
  pop %>% 
  filter((dpto_born %in% c(0:17)) & (age %in% c(7:18)) & as.numeric(language %in% c(1:3))) %>% 
  rename(child_id = person_id) %>% 
  mutate(child_present = 1)

# mother file
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
         mother_area_born = area_born, mother_years_scho = years_scho) %>% 
  select(person_id, mother_present, mother_age, mother_illiterate, mother_guarani,
         mother_spanish, mother_bilingual, mother_dpto_born, mother_area_born, mother_years_scho) %>% 
  rename(mother_id = person_id)
  
# father file
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
         father_area_born = area_born, father_years_scho = years_scho) %>% 
  select(person_id, father_present, father_age, father_illiterate, father_guarani,
         father_spanish, father_bilingual, father_dpto_born, father_area_born, father_years_scho) %>% 
  rename(father_id = person_id)

# put mother and father information to the child
children <- 
  children %>% 
  left_join(mother) %>% 
  left_join(father) %>% 
  replace_na(list(mother_present = 0, father_present = 0, child_present = 0)) %>% 
  mutate(caretaker = case_when(mother_present == 1 & father_present == 1 ~ "joint",
                               mother_present == 0 & father_present == 1 ~ "father",
                               mother_present == 1 & father_present == 0 ~ "mother",
                               mother_present == 0 & father_present == 0 ~ "other"))

# If neither father or mother is in the house use female max and then male max.
# check how many don't have mothers
table(t$caretaker)
