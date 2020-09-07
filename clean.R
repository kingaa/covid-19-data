library(tidyverse)
library(stringi)

read_csv("co-est2019-annres.csv",comment="#") %>%
  mutate(
    state=stri_replace_first_regex(county,"(.+?),\\s(.+)$","$2"),
    county=stri_replace_first_regex(county,"\\.(.+?),\\s(.+)$","$1")
  ) %>%
  left_join(
    read_csv("states.csv"),
    by="state"
  ) %>%
  left_join(
    read_csv("national_county.csv"),
    by=c("county","stateiso")
  ) %>%
  filter(!is.na(stfips),!is.na(cofips)) %>%
  mutate(
    fips=paste0(stfips,cofips)
  ) %>%
  select(fips,state,county,stateiso,population) %>%
  arrange(fips) -> dat

## New York
dat %>%
  filter(
    state=="New York",
    county %in% c("New York County","Bronx County","Kings County","Queens County","Richmond County")
  ) %>%
  summarize(
    fips="NYC",
    state="New York",
    stateiso="NY",
    county="New York City",
    population=sum(population)
  ) %>%
  bind_rows(dat) %>%
  arrange(fips) -> dat

dat %>%
  select(fips,state,county,stateiso) %>%
  write_csv("co_fips.csv")

dat %>%
  select(fips,population) %>%
  write_csv("pop_est_2019.csv")

read_csv("co_fips.csv") %>%
  right_join(
    read_csv("us-counties.csv") %>%
      filter(county!="Unknown") %>%
      mutate(
        fips=if_else(county=="New York City","NYC",fips)
      ),
    by=c("fips","state")
  ) %>%
  select(fips,state,county=county.y,date,cases,deaths) -> dat

library(rgdal)
library(rgeos)

readOGR("maps/cb_2018_us_county_20m.shp") -> map
map@data %>%
  select(fips=GEOID) -> map@data

read_csv("co_fips.csv") -> fips
map[map$fips %in% fips$fips,] -> map
map@data %>%
  left_join(fips,by="fips") -> map@data

map %>% saveRDS(file="us_county_map_20m.rds")
