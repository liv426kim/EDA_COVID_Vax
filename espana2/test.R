library(tidyverse)
covid_cases_deaths <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_california.csv")
ccd <- covid_cases_deaths
view(ccd)

colnames(ccd)

head(ccd)


ccd |>
  select(pfizer_doses_shipped) |>
  ggplot(aes(x = pfizer_doses_shipped)) +
  geom_histogram(bins = 30) 
  theme_light()

  
ccd |>
  select(moderna_doses_shipped) |>
  ggplot(aes(moderna_doses_shipped)) +
  geom_histogram(bins = 30) 
  theme_light()
  
ccd |>
  select(jj_doses_shipped) |>
  ggplot(aes(jj_doses_shipped)) +
  geom_histogram(bins = 30) 
  theme_light()
  
  
library(dplyr)
library(tidyr)
library(ggplot2)

  ccd_long <- ccd |>
    select(pfizer_doses_shipped, moderna_doses_shipped, jj_doses_shipped) |>
    pivot_longer(
      cols = everything(),
      names_to = "vaccine",
      values_to = "doses_shipped"
    )

  ggplot(ccd_long, aes(x = doses_shipped)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    facet_wrap(~ vaccine, scales = "free") +
    theme_light() +
    labs(title = "Distribution of Doses Shipped by Vaccine Type", x = "Doses Shipped", y = "Count")
  
  
unique(ccd$county)

colnames(cdd)



library(dplyr)
library(tidyr)
library(ggplot2)

# Calculate proportions per county
ccd_prop <- ccd |>
  group_by(county) |>
  summarise(
    pfizer = sum(pfizer_doses_shipped, na.rm = TRUE),
    moderna = sum(moderna_doses_shipped, na.rm = TRUE),
    jj = sum(jj_doses_shipped, na.rm = TRUE)
  ) |>
  mutate(total = pfizer + moderna + jj) |>
  mutate(
    pfizer = pfizer / total,
    moderna = moderna / total,
    jj = jj / total
  ) |>
  select(-total) |>
  pivot_longer(cols = c(pfizer, moderna, jj), names_to = "vaccine", values_to = "proportion")


ggplot(ccd_prop, aes(x = reorder(county, -proportion), y = proportion, fill = vaccine)) +
  geom_col(position = "stack") +
  coord_flip() +
  theme_light() +
  labs(
    title = "Proportion of Vaccine Doses Shipped by County",
    x = "County",
    y = "Proportion of Total Doses",
    fill = "Vaccine Type"
  )



  
