# Import Libraries
library(eeptools)
library(ggthemes)
library(janitor)
library(rio)
library(rvest)
library(tidyverse)
library(openxlsx)

# Import hospital bed data per county (2020 data)
hospital_beds <- import("hospital_beds.xlsx") %>%
  row_to_names(1) %>%
  .[-1, ] %>%
  rename(HospitalBedsCount = Count) %>% 
  dplyr::select(County, HospitalBedsCount)

# Make county column lowercase
hospital_beds$County <- tolower(hospital_beds$County)

# Remove commas and convert to numeric
hospital_beds$HospitalBedsCount <- decomma(hospital_beds$HospitalBedsCount)

# Population density data
density <- read_html("https://worldpopulationreview.com/us-counties/states/fl") %>%
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>% 
  .[[1]] %>% 
  rename(County = Name, Population = `2022 Population`, PopulationDensity = `Density (mi²)`) %>% 
  separate(County, c("County"), sep = " County") %>%
  dplyr::select(County, PopulationDensity)

# Make county column lowercase
density$County <- tolower(density$County)

# Remove commas and convert to numeric
density$PopulationDensity <- decomma(density$PopulationDensity)

# Scrape annual per capita income for each county from the web (2020 data)
income <- read_html("https://fred.stlouisfed.org/release/tables?rid=175&eid=266444") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>% 
  .[[1]] %>% 
  .[, -c(1, 4:5)] %>% 
  row_to_names(1) %>% 
  rename(County = Name, PerCapitaPersonalIncome = `2020`) %>% 
  separate(County, c("County"), sep = " County")

# Make county column lowercase
income$County <- tolower(income$County)

# Remove commas from population column and convert to numeric
income$PerCapitaPersonalIncome <- decomma(income$PerCapitaPersonalIncome)

# Voter registration per county (2020 data)
voters <- read_html("https://dos.myflorida.com/elections/data-statistics/voter-registration-statistics/voter-registration-reports/voter-registration-by-county-and-party/") %>%
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>% 
  .[[1]] %>% 
  rename(Republican = `Republican Party of Florida`, Democrat = `Florida Democratic Party`) %>% 
  dplyr::select(County, Republican, Democrat) %>% 
  .[-68, ]

# Make county column lowercase
voters$County <- tolower(voters$County)

# Remove commas and convert to numeric
voters$Republican <- decomma(voters$Republican)
voters$Democrat <- decomma(voters$Democrat)

# Covid19 data per county
covid <- read_html("https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/state/florida") %>%
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>% 
  .[[2]] %>% 
  separate(County, c("County"), sep = " County") %>%
  dplyr::select(County, Deaths, Cases) %>% 
  rename(TotalCovidDeaths = Deaths, TotalCovidCases = Cases)

# Make county column lowercase
covid$County <- tolower(covid$County)

# Remove commas and convert to numeric
covid$TotalCovidCases <- decomma(covid$TotalCovidCases)
covid$TotalCovidDeaths <- decomma(covid$TotalCovidDeaths)

# Percent population over 65 per county
age_65 <- read_html("https://www.indexmundi.com/facts/united-states/quick-facts/florida/percent-of-population-65-and-over#table") %>%
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>% 
  .[[1]] %>% 
  add_case(County = "Miami-Dade", Value = 16.7) %>% 
  rename(PercentOverAge65 = Value)

# Make county column lowercase
age_65$County <- tolower(age_65$County)

# Find which county is missing
# uhoh <- covid %>% anti_join(age_65, by = "County")

vaccines <- read_html("https://data.democratandchronicle.com/covid-19-vaccine-tracker/florida/12/") %>%
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>% 
  .[[3]] %>% 
  rename(OneDose = `People Vaccinated with at least One Dose`, TwoDose = `People Fully Vaccinated`, County = Area, TotalPopulation = Population) %>% 
  separate(OneDose, into = c('OneDoseNumber', 'OneDosePercent'), sep = -6) %>% 
  separate(OneDosePercent, into = 'OneDosePercent', sep = -1, remove = FALSE) %>% 
  separate(TwoDose, into = c('TwoDoseNumber', 'TwoDosePercent'), sep = -6) %>%
  separate(TwoDosePercent, into = 'TwoDosePercent', sep = -1, remove = FALSE) %>% 
  separate(County, c("County"), sep = " County")
  
# Remove commas and convert to numeric
vaccines$OneDoseNumber <- decomma(vaccines$OneDoseNumber)
vaccines$TwoDoseNumber <- decomma(vaccines$TwoDoseNumber)
vaccines$TwoDosePercent <- decomma(vaccines$TwoDosePercent)
vaccines$OneDosePercent <- decomma(vaccines$OneDosePercent)
vaccines$TotalPopulation <- decomma(vaccines$TotalPopulation)

# Make county column lowercase
vaccines$County <- tolower(vaccines$County)

# Join data sets together
everything <- covid %>% 
  left_join(hospital_beds, by = "County") %>%
  left_join(income, by = "County") %>% 
  left_join(voters, by = "County") %>% 
  left_join(density, by = "County") %>% 
  left_join(age_65, by = "County") %>% 
  left_join(vaccines, by = "County")

# Write to Excel file
covid_data <- write.xlsx(everything, file = "decision_support_data.xlsx", sheetName = "Covid_Data",
                         colNames = TRUE, rowNames = FALSE, overwrite = TRUE, asTable = TRUE)

# Regression
model_one <- lm(TotalCovidDeaths ~ OneDosePercent, everything)
summary(model_one)
one_resid <- resid(model_one)
hist(one_resid)
plot(model_one)


model_two <- lm(TotalCovidDeaths ~ TwoDosePercent, everything)
summary(model_two)
two_resid <- resid(model_two)
hist(two_resid)
plot(model_two)

model_three <- lm(TotalCovidDeaths ~ OneDosePercent + TwoDosePercent, everything)
summary(model_three)
three_resid <- resid(model_three)
hist(three_resid)
plot(model_three)

model_four <- lm(TotalCovidDeaths ~ OneDosePercent + 
                   TwoDosePercent +
                   PercentOverAge65, everything)
summary(model_four)
four_resid <- resid(model_four)
hist(four_resid)
plot(model_four)





