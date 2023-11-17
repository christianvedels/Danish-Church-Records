# Read data
# Updated:    2023-11-17
# Auhtors:    Christian Vedel [christian-vs@sam.sdu.dk],
#
# Purpose:    Reads church records
#
# Output:     

# ==== libraries ====
library(tidyverse)
library(data.table)
source("000_Functions.R")
library(tidygeocoder)

# ==== Read =====
# # data0 = read_delim("../../../Parish Records DK/61607_Denmark Church Books_FullDelivery.txt", delim = "|")
# data0 = fread("../../../Parish Records DK/61607_Denmark Church Books_FullDelivery.txt")
# 
# capN = NROW(data0) # 22820160
# 
# # Toy data
# # Smaller sample to work with
# set.seed(20)
# data1 = data0 %>% sample_n(100000)
# 
# save(data1, file = "Data/Tmp_data/toydata.Rdata")
load("Data/Tmp_data/toydata.Rdata")
data0 = data1
data0 %>% names()

# ==== Turn all to character ====
data0 = data0 %>% mutate_all(as.character)

# ==== Descriptive ====
# List of variables
data0 %>% names()

# ==== Functions ====
non_empty = function(x){
  x != ""
}

# ==== Params ====
common_vars = data0 %>% 
  select(
    unique_identifier, 
    SourceDescription, 
    GivenName:Gender, 
    starts_with("Birth"),
    ResidenceParish, ResidenceMunicipality, ResidenceMunicipality,
    FatherGivenName:MotherInLawSurnameAlias # Everything in the end
  ) %>% 
  names()

# ==== Baptism ====
bapt = data0 %>% 
  select(all_of(common_vars), starts_with("Baptism")) %>% 
  filter(
    non_empty(BaptismDay) | non_empty(BaptismMonth) | non_empty(BaptismYear)
  ) %>% 
  rename(
    EventYear = BaptismYear,
    EventMonth = BaptismMonth,
    EventDay =  BaptismDay,
    EventAge = BaptismAge,
    EventPlace = BaptismPlace,
    EventParish = BaptismParish,
    EventMunicipality = BaptismMunicipality,
    EventCounty = BaptismCounty,
    EventState = BaptismState,
    EventCountry = BaptismCountry
  ) %>% 
  mutate(
    event = "Baptism"
  )

# Check values
if(NROW(bapt)<10^5){
  Unique_misc_sum(bapt)
}

# ==== Confirmations ====
conf = data0 %>% 
  select(all_of(common_vars), starts_with("Confirmation")) %>% 
  filter(
    non_empty(ConfirmationDay) | non_empty(ConfirmationMonth) | non_empty(ConfirmationYear)
  ) %>% 
  rename(
    EventYear = ConfirmationYear,
    EventMonth = ConfirmationMonth,
    EventDay =  ConfirmationDay,
    EventAge = ConfirmationAge,
    EventPlace = ConfirmationPlace,
    EventParish = ConfirmationParish,
    EventMunicipality = ConfirmationMunicipality,
    EventCounty =ConfirmationCounty,
    EventState = ConfirmationState,
    EventCountry = ConfirmationCountry
  ) %>% 
  mutate(
    event = "Confirmation"
  )

# Check values
if(NROW(conf)<10^5){
  Unique_misc_sum(conf)
}

# ==== Arrival ====
arr = data0 %>% 
  select(all_of(common_vars), starts_with("Arrival")) %>% 
  filter(
    non_empty(ArrivalDay) | non_empty(ArrivalMonth) | non_empty(ArrivalYear)
  ) %>% 
  rename(
    EventYear = ArrivalYear,
    EventMonth = ArrivalMonth,
    EventDay =  ArrivalDay,
    EventAge = ArrivalAge,
    EventPlace = ArrivalPlace,
    EventParish = ArrivalParish,
    EventMunicipality = ArrivalMunicipality,
    EventCounty =ArrivalCounty,
    EventState = ArrivalState,
    EventCountry = ArrivalCountry
  ) %>% 
  mutate(
    event = "Arrival"
  )

# Check values
if(NROW(arr)<10^5){
  Unique_misc_sum(arr)
}

# ==== Departures ====
# "DepartureDay" "DepartureMonth" "DepartureYear"

depart = data0 %>% 
  select(all_of(common_vars), starts_with("Departure")) %>% 
  filter(
    non_empty(DepartureDay) | non_empty(DepartureMonth) | non_empty(DepartureYear)
  ) %>% 
  rename(
    EventYear = DepartureYear,
    EventMonth = DepartureMonth,
    EventDay =  DepartureDay,
    EventAge = DepartureAge,
    EventPlace = DeparturePlace,
    EventParish = DepartureParish,
    EventMunicipality = DepartureMunicipality,
    EventCounty = DepartureCounty,
    EventState = DepartureState,
    EventCountry = DepartureCountry
  ) %>% 
  mutate(
    event = "Departure"
  )

# Check values
if(NROW(depart)<10^5){
  Unique_misc_sum(depart)
}

# ==== Marriage ====
# "MarriageDay" "MarriageMonth" "MarriageYear"

marr = data0 %>% 
  select(all_of(common_vars), starts_with("Marriage")) %>% 
  filter(
    non_empty(MarriageDay) | non_empty(MarriageMonth) | non_empty(MarriageYear)
  ) %>% 
  rename(
    EventYear = MarriageYear,
    EventMonth = MarriageMonth,
    EventDay =  MarriageDay,
    EventAge = MarriageAge,
    EventPlace = MarriagePlace,
    EventParish = MarriageParish,
    EventMunicipality = MarriageMunicipality,
    EventCounty =MarriageCounty,
    EventState = MarriageState,
    EventCountry = MarriageCountry
  ) %>% 
  mutate(
    event = "Marriage"
  )

# Check values
if(NROW(marr)<10^5){
  Unique_misc_sum(marr)
}

# ==== Death ====
# "DeathDay" "DeathMonth" "DeathYear"
death = data0 %>% 
  select(all_of(common_vars), starts_with("Death")) %>% 
  filter(
    non_empty(DeathDay) | non_empty(DeathMonth) | non_empty(DeathYear)
  ) %>% 
  rename(
    EventYear = DeathYear,
    EventMonth = DeathMonth,
    EventDay =  DeathDay,
    EventAge =  DeathAge,
    EventPlace = DeathPlace,
    EventParish = DeathParish,
    EventMunicipality = DeathMunicipality,
    EventState = DeathState
  ) %>% 
  mutate(
    event = "Death"
  )

# Check values
if(NROW(death)<10^5){
  Unique_misc_sum(death)
}

# ==== Burial ====
# "BurialDay" "BurialMonth" "BurialYear"
burial = data0 %>% 
  select(all_of(common_vars), starts_with("Burial")) %>% 
  filter(
    non_empty(BurialDay) | non_empty(BurialMonth) | non_empty(BurialYear)
  ) %>% 
  rename(
    EventYear = BurialYear,
    EventMonth = BurialMonth,
    EventDay =  BurialDay,
    EventAge =  BurialAge,
    EventPlace = BurialPlace,
    EventParish = BurialParish,
    EventMunicipality = BurialMunicipality,
    EventCounty = BurialCounty,
    EventState = BurialState,
    EventCountry = BurialCountry
  ) %>% 
  mutate(
    event = "Burial"
  )

# Check values
if(NROW(burial)<10^5){
  Unique_misc_sum(burial)
}

# ==== Merge together ====
identifiers = data0$unique_identifier

rm(data0)

data_clean = bapt %>% 
  bind_rows(conf) %>% 
  bind_rows(arr) %>% 
  bind_rows(depart) %>% 
  bind_rows(marr) %>% 
  bind_rows(death) %>%
  bind_rows(burial) %>%
  ungroup()

data_clean %>% NROW()

# Missing
sum(!(identifiers %in% data_clean$unique_identifier)) # 5.5%

# data0 %>% anti_join(data_clean, by = "unique_identifier") %>% 
#   summarise_all(function(x) sum(is.na(x) | x == "")) # But seemingly all is junk

# ==== Geocoding ====
# x = data_clean %>% 
#   mutate(
#     tmp1 = paste0(EventPlace, EventParish),
#     tmp2 = paste(EventCounty, EventState)
#   ) %>% 
#   distinct(
#     tmp1,
#     EventMunicipality,
#     tmp2,
#     EventCountry
#   )
#   
# 
# x = x[1:100,] %>% 
#   geocode(
#     street = tmp1,
#     city = EventMunicipality,
#     county = tmp2,
#     country = EventCountry,
#     method = "osm"
#   )

# ==== Save ====
save(data_clean, file = "Data/Tmp_data/toydata_clean.Rdata")

# ==== Descriptive ====
data_clean %>% 
  group_by(event) %>% 
  count()


