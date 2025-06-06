install.packages("tidyverse")
library(tidyverse)
covid_cases_deaths <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_california.csv")

ccd <- covid_cases_deaths

names(ccd)
table(ccd$county)

# x= week
# y= cumulative_doses_shipped


ccd |>
  group_by(county, )