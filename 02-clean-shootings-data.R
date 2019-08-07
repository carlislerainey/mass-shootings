
# load packages
library(tidyverse)
library(lubridate)
# tidy the data
df <- read_csv("shootings-raw.csv") %>%
  mutate(date = mdy(date)) %>%
  filter(fatalities >= 4) %>% # consistent defn of "mass shooting" throughout
  select(case, location, date, fatalities, injured) %>%
  arrange(date) %>%
  glimpse() %>%
  write_rds("shootings.rds") %>%
  write_csv("shootings.csv")


