# Descriptive stats
# Updated:    2023-11-17
# Auhtors:    Christian Vedel [christian-vs@sam.sdu.dk],
#
# Purpose:    Descriptive stats
#
# Output:     Various plots

# ==== Libraries ====
library(tidyverse)
source("000_Functions.R")

# ==== Load data ====
load("Data/Tmp_data/toydata_clean.Rdata")

# ==== Params ====
plot_width = 8
plot_aspect = 3/4
plot_height = plot_width*plot_aspect

# ==== Data wrangling ====
data_clean = data_clean %>% 
  mutate(
    EventAge = as.numeric(EventAge)
  ) %>% 
  mutate(
    Gender = case_when(
      Gender == "Kvinde" ~ "F",
      Gender == "Mandlige" ~ "M",
      TRUE ~ "Not available"
    )
  )

# ==== Simple count ====
data_clean %>% 
  group_by(event) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    pct = n/sum(n)
  )

# ==== Ages =====
ages = c(18, 30, 50, 75)

p1 = data_clean %>% 
  ggplot(aes(EventAge, fill = event)) + 
  geom_density(alpha = 0.5) +
  facet_wrap(~event) + 
  theme_bw() + 
  geom_vline(xintercept = ages, lty = 2) +
  theme(
    legend.position = "bottom"
  )
  

# p1
ggsave0(p1, "Event_ages")

for(e in unique(data_clean$event)){
  data_e = data_clean %>%
    filter(event == e)
  
  data_e_allN = NROW(data_e)
  
  data_e = data_e %>%
    drop_na(EventAge)
  
  nNA = data_e_allN - NROW(data_e)
  
  p1 = data_e %>% 
    ggplot(aes(EventAge, fill = Gender)) + 
    geom_density(alpha = 0.5) +
    theme_bw() + 
    geom_vline(xintercept = ages, lty = 2) +
    theme(
      legend.position = "bottom" 
    ) + 
    labs(
      title = e,
      subtitle = paste0(
        "Observations ", NROW(data_e), "\n(NA: ", nNA, ")"
      )
    ) + 
    scale_fill_manual(values = c("F"="#b33d3d", "M"="#273a8f"))
  
  p1
  ggsave0(p1, paste0("Event_ages_", e))
}

# ==== Count by year ====
N_all = 22820160

data_clean = data_clean %>% 
  mutate(
    EventYear = ifelse(EventYear=="", NA, EventYear)
  ) %>% 
  mutate(EventYear_num = as.numeric(EventYear)) %>% 
  mutate(
    coercionNA = is.na(EventYear_num) & !is.na(EventYear)
  )

data_clean %>% 
  summarise(sum(coercionNA))

data_clean %>% 
  filter(coercionNA)

data_clean %>% 
  filter(EventYear_num<500)

n_long = NROW(data_clean %>% filter(EventYear_num>1812, EventYear<1917))

p = data_clean %>% 
  filter(EventYear_num>1812, EventYear<1917) %>% 
  group_by(event, EventYear_num) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    n = n/(sum(n)) * N_all*(n_long/100000)
  ) %>% 
  ggplot(aes(x = EventYear_num, y=n, col = event)) + 
  geom_line() + 
  theme_bw() + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "Event Year",
    col = "Event:"
  )

p
ggsave0(p, "Data_availability_years")

for(e in unique(data_clean$event)){
  data_e = data_clean %>%
    filter(event == e)
  
  data_e_allN = NROW(data_e)
  
  data_e = data_e %>%
    drop_na(EventAge)
  
  nNA = data_e_allN - NROW(data_e)
  
  p1 = data_e %>% 
    filter(EventYear_num>1812, EventYear<1917) %>% 
    group_by(event, EventYear_num) %>% 
    count() %>% 
    ungroup() %>% 
    # mutate(
    #   n = n/(sum(n)) * N_all*(n_long/100000)
    # ) %>% 
    ggplot(aes(x = EventYear_num, y=n)) + 
    geom_line(col = "red") + 
    theme_bw() + 
    theme(
      legend.position = "bottom"
    ) + 
    labs(
      x = "Event Year",
      col = "Event:",
      title = e,
      subtitle = paste0(
        "Observations ", NROW(data_e), "\n(NA: ", nNA, ")"
      )
    )
  
  # p1
  ggsave0(p1, paste0("Event_years_", e))
}


data_e = data_clean %>%
  drop_na(BirthYear) %>% 
  mutate(
    BirthYear = as.numeric(BirthYear)
  )

n_long = NROW(data_e)
p1 = data_e %>% 
  filter(BirthYear>1750, BirthYear<1917) %>%
  group_by(event, BirthYear) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    n = n/(sum(n)) * N_all*(n_long/100000)
  ) %>%
  ggplot(aes(x = BirthYear, y=n, col = event)) + 
  geom_line() + 
  theme_bw() + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "Birth Year",
    col = "Source:"
  )

p1
ggsave0(p1, "Birth_years")

# ==== Geography ====
data_clean$EventParish %>% 
  unique() %>% length()

data_clean$EventCounty %>% 
  unique() %>% length()

data_clean$EventState %>% 
  unique() %>% length()


# ==== Child mortality: Baptism vs. confirmation =====  
p1 = data_clean %>% 
  filter(EventYear_num>1814, EventYear<1917) %>% 
  group_by(event, EventYear_num) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    n = n/(sum(n)) * N_all*(n_short/100000)
  ) %>% 
  pivot_wider(names_from = event, values_from = n) %>% 
  mutate(
    dif14 = lag(Baptism, 14) - Confirmation
  ) %>% 
  mutate(
    dif14_pct = dif14/lag(Baptism, 14)
  ) %>% 
  ggplot(aes(x = EventYear_num, y = dif14_pct)) + 
  geom_line() + 
  geom_hline(yintercept = 0) +
  theme_bw() + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "Difference between baptised and confirmed\n(14 years lag)",
    x = "Year"
  )

ggsave0(p1, "Crude_child_mortality")

# ==== Child mortality: Baptism vs. confirmation =====  
data_clean %>% 
  filter(event %in% c("Arrival", "Departure")) %>% 
  distinct(EventYear)
  
  
