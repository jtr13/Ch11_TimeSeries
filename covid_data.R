library(tidyverse)
library(ggpmisc)
# https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
# https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36
df <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv")
df %>%
  mutate(date = lubridate::mdy(submission_date)) %>%
  filter(date > "2020-07-01") %>%
  group_by(date) %>%
  summarize(new_case = sum(new_case), new_death = sum(new_death)) %>%
  mutate(new_death_rate = new_death/new_case) %>%
  pivot_longer(cols = -date, names_to = "variable", values_to = "value") %>%
  ggplot(aes(date, value)) +
  geom_line() + geom_smooth(span = .25, lwd = .5) +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +
  scale_x_date(breaks = "1 month", date_labels = "%b %n %y") + stat_peaks(span = 31, col = "red")

