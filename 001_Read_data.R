# Read data
# Updated:    2023-09-07
# Auhtors:    Christian Vedel [christian-vs@sam.sdu.dk],
#
# Purpose:    Reads church records
#
# Output:     

# ==== libraries ====
library(tidyverse)
library(data.table)

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

# ==== Baptism ====
non_empty = function(x){
  x != ""
}

bapt = data1 %>% 
  select(unique_identifier, SourceDescription, GivenName:Gender, starts_with("Birth"), starts_with("Baptism")) %>% 
  filter(
    non_empty(BaptismDay) | non_empty(BaptismMonth) | non_empty(BaptismYear)
  ) %>% 
  rename(
    EventYear = BaptismYear,
    EventMonth = BaptismMonth,
    EventDay =  BaptismDay
  ) %>% 
  mutate(
    event = "Baptism"
  )

# Check dates
bapt$EventDay %>% unique() 
bapt$EventMonth %>% unique()
bapt$EventYear %>% unique()

# ==== Confirmations ====
data1

# ==== Descriptive ====
# List of variables
data1 %>% names()

# Unique values
vals = data1 %>% 
  summarise_all(function(x){
    x = sort(unique(x))
    paste(x, collapse = ",")
  })

data1 %>% names()


