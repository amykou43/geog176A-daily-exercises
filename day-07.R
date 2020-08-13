# Name: Amy Kou
# Date: 08/13/20
# Purpose: Graph covid cases & deaths by USA region

#1: Make a faceted plot of the cumulative cases & deaths by USA region.
#Your x axis should be the date and the y axis value/count.
#To do this you will need to join and pivot the COVID-19 data.

library(tidyverse)

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'

covid = read_csv(url)

head(covid)

region = data.frame(state = state.name, region = state.region)
head(region)

inner_join(covid, region, by = "state") %>%
  count(region) %>%
  mutate(tot = sum(n))

full_join(covid, region, by = "state") %>%
  count(region) %>%
  mutate(tot = sum(n))

left_join(covid, region, by = "state") %>%
  count(region) %>%
  mutate(tot = sum(n))

right_join(covid, region, by = "state") %>%
  count(region) %>%
  mutate(tot = sum(n))

covid %>%
  right_join(region, by = "state") %>%
  group_by(region, date) %>%
  summarize(cases = sum(cases),
            deaths = sum(deaths)) %>%
  pivot_longer(cols = c('cases', 'deaths')) ->
  covid_region

ggplot(covid_region, aes(x = date, y = value)) +
  geom_line(aes(col = region)) +
  facet_grid(name~region, scale = "free_y") +
  theme_linedraw() +
  theme(legend.position = "botton") +
  theme(legend.position = "NA") +
  labs(title = "Cumulative Cases and Deaths: Region",
       y = "Daily Cumulative Count",
       x = "Date",
       caption = "Daily Exercise 07",
       subtitle = "COVID-19 Data: NY-Times" )
