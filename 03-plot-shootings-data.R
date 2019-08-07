
# load packages
library(tidyverse)
library(lubridate)
library(magrittr)

# wrangle data
df <- read_rds("shootings.rds") %>% 
  group_by(date) %>%
  summarize(n_fatalities = sum(fatalities),
            n_injured = sum(injured), 
            n_mass_shootings = n()) %>%
  ungroup() %>%
  filter(date >= ymd("1993-01-20")) %>%
  arrange(date) %>%
  glimpse()

# presidents data
# from https://www.kaggle.com/harshitagpt/us-presidents
pres_df <- read_csv("us-presidents.csv") %>% 
  mutate(start_date = parse_date_time(start, orders = "mdy"),
         end_date = parse_date_time(end, orders = "mdy"),
         start_date = ymd(start_date),
         end_date = ymd(end_date) - days(1)) %>%
  select(start_date, end_date, president, party) %>%
  glimpse()

# repair a couple of missing pieces
pres_df$end_date[pres_df$president == "Barack Obama"] <- ymd("2017-01-19")
pres_df$end_date[pres_df$president == "Donald Trump"] <- ymd("2021-01-19")

  
# create empty data from for all data
full_df <- tibble(date = seq(ymd("1993-01-20"), ymd("2019-08-06"), by = "days")) %>%
  left_join(df) %>%
  mutate(n_fatalities = ifelse(is.na(n_fatalities), 0, n_fatalities),
         n_injured = ifelse(is.na(n_injured), 0, n_injured),
         n_mass_shootings = ifelse(is.na(n_mass_shootings), 0, n_mass_shootings)) %>%
  arrange(date) %>%
  mutate(president = NA) %>%
  glimpse()

for (i in 1:nrow(pres_df)) {
  tmp <- pres_df[i, ]
  full_df <- full_df %>%
    mutate(president = ifelse(date >= tmp$start_date & date <= tmp$end_date, tmp$president, president))
}
full_df %<>%
  left_join(select(pres_df, president, party))
glimpse(full_df)

# compute cumulative sums by president
gg_df <- full_df %>%
  group_by(president, party) %>% 
  mutate(days_in_office = date - min(date)) %>% 
  mutate(cumulative_fatalities = cumsum(n_fatalities),
         cumulative_injured = cumsum(n_injured),
         cumulative_mass_shootings = cumsum(n_mass_shootings)) %>%
  glimpse() %>%
  mutate(label = NA,
         label = ifelse(days_in_office == max(days_in_office) & cumulative_fatalities > 100, president, NA)) %>%
  select(-starts_with("n_")) %>%
  rename(`Total Fatalities from Mass Shootings` = cumulative_fatalities,
         `Total Injuries from Mass Shootings` = cumulative_injured,
         `Total Mass Shootings` = cumulative_mass_shootings) %>%
  gather(statistic, number, `Total Fatalities from Mass Shootings`:`Total Mass Shootings`) %>%
  glimpse()

# plot data
library(ggrepel)
library(hrbrthemes)
library(directlabels)
ggplot(gg_df, aes(x = days_in_office, y = number, color = party, group = president)) + 
  facet_wrap(vars(statistic), scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Democratic" = scales::muted("blue"), 
                                "Republican" = scales::muted("red"))) + 
  geom_line() + 
  theme_ipsum() + 
  scale_x_continuous(limits = c(NA, 3200)) + 
  geom_text(aes(label = label), box.padding = 0.25, size = 2, segment.size = 0.2, vjust = 0.5, hjust = -0.05) + 
  theme(legend.position = "none") + 
  labs(x = "Days in Office",
       y = "Cumulative Total",
       title = "Mass Shootings Under Bush, Clinton, Obama, and Trump",
       subtitle = "(with correction for inconsistent definitions over time)",
       caption = 'Data from MotherJones.com\n"Mass shooting" (as measured here) is a shooting with four or more casualties.')
ggsave("shootings.png", height = 8, width = 6, scale = 1.1)
