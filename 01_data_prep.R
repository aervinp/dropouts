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

#check your work
head(pop$years_scho)

#cleaning population file
pop <- 
  pop %>% 
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

#select sample of observations based on born in Paraguay, language spoken, and age
children <- 
  pop %>% 
  filter((dpto_born %in% c(0:17)) & (age %in% c(7:18)) & as.numeric(language %in% c(1:3))) %>% 
  rename(child_id = person_id)

# left off here
mother <- 
  pop %>% 
  select(person_id, mother_id, line_mother) %>% 
  filter(line_mother != 0) %>% 
  rename(child_id = person_id) %>% 
  select(child_id, mother_id)

mother <- 
  mother %>% 
  left_join(pop, by = c("mother_id" = "person_id")) %>% 
  select(hhid, mother_id, mother_id.y, line_mother, child_id, survey_line, everything())


#select variables
pop <- 
  pop %>% 
  select(c("hhid", "UPM", "NVIVI", "NHOGA", "DPTO", "AREA", 
           "age", "female", "years_scho", "language",
           "FEX"))

