library(tidyverse)
library(tidycensus)
library(jsonlite)
library(gt)

# scrape data from Illinois Department of Public Health website -----------

site <- "http://www.dph.illinois.gov/sitefiles/"

idph_data <- fromJSON(paste0(site,"COVIDHistoricalTestResults.json"))
idph_zips <- fromJSON(paste0(site, "COVIDZip.json"))

idph <- idph_data$demographics$age$demographics[[1]]
names(idph) <- idph_data$demographics$age$age_group

idph <- bind_rows(idph, .id = "age_group")


# read census data --------------------------------------------------------

census <- read_tsv("census.txt", n_max = 272, )
census <- rename_all(census, function (x) str_replace_all(tolower(x), " ", "_"))


# clean data --------------------------------------------------------------

# drop total rows
census <- filter(census, is.na(notes))

# drop more than one race category
census <- filter(census, race != "More than one race")

# only need latix totals not by race
census_collapse_latinx <-
  census %>%
  group_by(state,
           state_code,
           age_group,
           age_group_code,
           ethnicity,
           ethnicity_code) %>%
  summarize(population = sum(population)) %>%
  filter(ethnicity == "Hispanic or Latino") %>%
  mutate(race = "Hispanic or Latino") %>%
  ungroup()

# recombine
census <- 
  census %>%
  filter(ethnicity != "Hispanic or Latino") %>%
  bind_rows(census_collapse_latinx)

# align age categories to those available from IDPH
census <- 
  census %>%
  mutate(
    age_group = case_when(
      age_group_code %in% c("0-4", "5-9", "10-14", "15-19") ~ "<20",
      age_group_code %in% c("20-24", "25-29") ~ "20-29",
      age_group_code %in% c("30-34", "35-39") ~ "30-39",
      age_group_code %in% c("40-44", "45-49") ~ "40-49",
      age_group_code %in% c("50-54", "55-59") ~ "50-59",
      age_group_code %in% c("60-64", "65-69") ~ "60-69",
      age_group_code %in% c("70-74", "75-79") ~ "70-79",
      age_group_code %in% c("80-84", "85+") ~ "80+"
    )
  ) %>%
  group_by(age_group, race) %>%
  summarise(population = sum(population))
  

# align IDPH race/ethnicity categories to standards
idph <- 
  idph %>%
  rename(race = description, cases = count) %>%
  mutate(
    race = case_when(
      race == "White" ~ "White",
      race == "Black" ~ "Black or African American",
      race == "Asian" ~ "Asian",
      race == "NH/PI*" ~ "Native Hawaiian or Other Pacific Islander",
      race == "Hispanic" ~ "Hispanic or Latino",
      race == "AI/AN**" ~ "American Indian or Alaska Native",
      race == "Left Blank" ~ "Unknown",
      race == "Other" ~ "Other"
    )
  ) 

# drop unnecessary variables
census <- select(census, race, age_group, population)
idph <- select(idph, race, age_group, tested, cases, deaths)


# merge datasets ----------------------------------------------------------

df <- left_join(idph, census, by = c("age_group", "race"))
df <- arrange(df, race, age_group) 
# df <- 
#   df %>%
#   group_by(race) %>%
#   mutate(race = replace(race, row_number() > 1, "")) %>%
#   ungroup()
  

# make table --------------------------------------------------------------

gt(select(df, -race)) %>%
  tab_header(
    title = md("**COVID-19 tests, cases, and deaths by racial/ethnic group**"),
    subtitle = md("Data reported by Illinois Department of Public Health as of June 18, 2020.")
  ) %>%
  tab_source_note(
    source_note = "Source: http://www.dph.illinois.gov/"
  ) %>% 
  tab_footnote(
    footnote = "Illinois population counts by age and racial/ethnic group are from 2018 US Census Bureau 5-year estimates.",
    locations = cells_column_labels(
      columns = vars(population)
      )
  ) %>%
  tab_row_group(
    group = "Other",
    rows = 46:54
  ) %>%
  tab_row_group(
    group = "Unknown Racial/Ethnic group",
    rows = 55:63
  ) %>%
  tab_row_group(
    group = "American Indian or Alaska Native",
    rows = 1:9
  ) %>%
  tab_row_group(
    group = "Native Hawaiian or Other Pacific Islander",
    rows = 37:45
  ) %>%
  tab_row_group(
    group = "Asian",
    rows = 10:18
  ) %>%
  tab_row_group(
    group = "Hispanic or Latino",
    rows = 28:36
  ) %>%
  tab_row_group(
    group = "Black or African American",
    rows = 19:27
  ) %>%
  tab_row_group(
    group = "White",
    rows = 64:72
  ) %>%
  cols_label(
    age_group = html("Age group"),
    tested = html("Tested<br>(N)"),
    cases = html("Cases<br>(N)"),
    deaths = html("Deaths<br>(N)"),
    population = html("Population<br>(N)")
  )
