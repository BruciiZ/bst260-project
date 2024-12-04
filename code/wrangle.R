# Load the census-key
rm(list = ls())

# Set working directory and read in the census key
source("census-key.R")

# Libraries
library(httr2)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(purrr)
source("funcs.R")

### 2022 & 2023 Census Data ###
pop_2223 <- read_csv('../data/NST-EST2023-ALLDATA.csv')
pop_2223 <- pop_2223 %>%
  select(NAME, POPESTIMATE2022, POPESTIMATE2023) %>%
  rename(state_name = "NAME", POP_2022 = "POPESTIMATE2022", POP_2023 = "POPESTIMATE2023") %>%
  mutate(POP_2022 = as.character(POP_2022),
         POP_2023 = as.character(POP_2023))

### Census Data Download ###

# Construct the GET request
url <- "https://api.census.gov/data/2021/pep/population"
request <- request(url) %>%
  req_url_query(get = I("POP_2020,POP_2021,NAME"), # I() makes R not interpret this as a formula, instead a string
                `for` = I("state:*"),
                key = census_key)

# Make a request to the US Census API and save the response
response <- request %>%
  req_perform()
response

# Determine the content type of the response
resp_content_type(response)

# Extract the data into a matrix
population <- response %>%
  resp_body_json(simplifyVector = T)
population

### Wrangle the population matrix ###

population <- population %>%
  row_to_names(1) %>%
  as_tibble() %>%
  select(-state) %>%
  rename(state_name = "NAME") %>%
  left_join(pop_2223, by = c("state_name")) %>%
  pivot_longer(cols = c("POP_2020", "POP_2021", "POP_2022", "POP_2023"), names_to = "year", values_to = "population") %>%
  mutate(year = str_remove(year, "POP_")) %>%
  mutate(across(-state_name, as.numeric)) %>%
  mutate(state = state.abb[match(state_name, state.name)]) %>%
  mutate(state = case_when(
    state_name == "District of Columbia" ~ "DC",
    state_name == "Puerto Rico" ~ "PR",
    .default = state
  ))

### 2024 population estimation ###
train_dat <- population %>%
  mutate(year = year - 2020)
state_names <- unique(population$state_name)

for (name in state_names) {
  
  state_data <- train_dat %>%
    filter(state_name == name)
  
  new_data <- data.frame(year = 4)
  
  fit <- lm(population ~ year, data = state_data)
  
  pop_2024 <- predict(fit, newdata = new_data)
  
  population <- population %>%
    add_row(state_name = name, year = 2024, population = pop_2024, state = state_data$state[1])
  
}

population <- population %>%
  arrange(state_name, year, population) %>%
  mutate(population = round(population))

### Download 10 Public Health Service (PHS) defined by CDC ###

# Download and process regions JSON file to read in regions
url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions <- fromJSON(url, simplifyDataFrame = F)

regions <- map_df(regions, function(x) {
  data.frame(region = x$region, region_name = x$region_name, state_name = x$states)
})

# Change the long region name to something shorter
regions <- regions %>%
  mutate(region_name = case_when(
    region_name == "New York and New Jersey, Puerto Rico, Virgin Islands" ~ "NY, NJ, PR, or VI",
    .default = region_name
  ))

# Join regions data with the population data
population <- left_join(population, regions, by = "state_name")
  
### CDC Data Download: cases, hospitalizations, deaths, and vaccinations ###

# Download the data from the given urls

# cases_raw <- get_cdc_data("https://data.cdc.gov/resource/pwn4-m3yp.json") # cases
# hosp_raw <- get_cdc_data("https://data.cdc.gov/resource/39z2-9zu6.json") # hospitalizations
# deaths_raw <- get_cdc_data("https://data.cdc.gov/resource/r8kw-7aab.json") # deaths
# vax_raw <- get_cdc_data("https://data.cdc.gov/resource/rh2h-3yt2.json") # vaccinations

load("../data/cases_raw.rda")
load("../data/hosp_raw.rda")
load("../data/deaths_raw.rda")
load("../data/vax_raw.rda")

# Data wrangling with cases
# cases_raw stores weekly data
cases <- cases_raw %>%
  mutate(cases = parse_number(new_cases),
         date = as_date(ymd_hms(end_date))) %>%
  filter(state %in% population$state) %>%
  mutate(mmwr_week = epiweek(date), mmwr_year = epiyear(date)) %>%
  select(state, mmwr_year, mmwr_week, cases) %>%
  arrange(state, mmwr_year, mmwr_week, cases)

# Data wrangling with hospitalization
# hosp_raw stores daily data
hosp <- hosp_raw %>%
  filter(jurisdiction %in% population$state) %>%
  # filter(!is.na(new_covid_19_hospital)) %>%
  mutate(hosp = parse_number(new_covid_19_hospital), # use new_covid_19_hospital for this
         date = as_date(ymd_hms(collection_date))) %>% # what does as_date do
  mutate(mmwr_week = epiweek(date), mmwr_year = epiyear(date)) %>% # unique identifiers for week
  select(jurisdiction, mmwr_year, mmwr_week, hosp) %>%
  group_by(jurisdiction, mmwr_year, mmwr_week) %>%
  filter(n() == 7) %>%  # Ensure there are 7 records per week
  summarize(hosp = sum(hosp, na.rm = TRUE)) %>%  # Sum up the 7 records to collapse
  ungroup() %>%
  arrange(jurisdiction, mmwr_year, mmwr_week, hosp) %>%
  rename(state = "jurisdiction")

# hosp %>%
#   ggplot(aes(mmwr_week, hosp, color = state)) +
#   geom_line() + facet_wrap(~mmwr_year)

# Data wrangling with deaths
# stores weekly data
deaths <- deaths_raw %>%
  mutate(deaths = parse_number(covid_19_deaths),
         date = as_date(ymd_hms(end_date))) %>%
  filter(state %in% population$state_name) %>%
  mutate(mmwr_year = epiyear(date), mmwr_week = as.numeric(mmwr_week)) %>% # use the original mmwr_week
  select(state, mmwr_year, mmwr_week, deaths) %>%
  rename(state_name = "state") %>%
  arrange(state_name, mmwr_year, mmwr_week, deaths)

# Data wrangling with vaccination data
# stores daily data
vax <- vax_raw %>%
  mutate(series_complete = parse_number(series_complete_cumulative),
         booster = parse_number(booster_cumulative),
         date = as_date(ymd_hms(date))) %>%
  filter(location %in% population$state,
         date_type == "Admin") %>% # use Admin date type
  mutate(mmwr_year = epiyear(date), mmwr_week = epiweek(date)) %>%
  select(location, mmwr_year, mmwr_week, series_complete, booster) %>%
  rename(state = "location") %>%
  group_by(state, mmwr_year, mmwr_week) %>%
  summarise(series_complete = max(series_complete, na.rm = T), # max vaccination of the week
            booster = max(booster, na.rm = T)) %>% # max booster of the week
  ungroup() %>%
  arrange(state, mmwr_year, mmwr_week, series_complete, booster)

### Data Joining ###

# Make all_dates data frame to make sure we cover all the dates

all_dates <- data.frame(date = seq(make_date(2020, 1, 25), # why do we start from 1/25
                                   make_date(2024, 12, 1), 
                                   by = "week")) |>
  mutate(date = ceiling_date(date, unit = "week", week_start = 7) - days(1)) |> # make sure we have the unique combination of date and state
  mutate(mmwr_year = epiyear(date), mmwr_week = epiweek(date))

dates_and_pop <- cross_join(all_dates, data.frame(state = unique(population$state))) |> 
  left_join(population, by = c("state", "mmwr_year" = "year"))

# Join all the tables together
dates_and_pop <- left_join(dates_and_pop, cases, by = c("state", "mmwr_year", "mmwr_week"))
dates_and_pop <- left_join(dates_and_pop, deaths, by = c("state_name", "mmwr_year", "mmwr_week"))
dates_and_pop <- left_join(dates_and_pop, hosp, by = c("state", "mmwr_year", "mmwr_week"))
dat <- left_join(dates_and_pop, vax, by = c("state", "mmwr_year", "mmwr_week")) %>%
  arrange(state, mmwr_year, mmwr_week)

### Save the Wrangled Datasets ###
save(dat, file = "../data/dat.rda")
save(cases_raw, file = "../data/cases_raw.rda")
save(deaths_raw, file = "../data/deaths_raw.rda")
save(hosp_raw, file = "../data/hosp_raw.rda")
save(vax_raw, file = "../data/vax_raw.rda")