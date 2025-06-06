
library(tidyverse)
covid_cases_deaths <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_california.csv")

ccd <- covid_cases_deaths
ccd_prop <- ccd

summarize(ccd)
summary(ccd)

names(ccd)
dim(ccd)

library(tidyverse)

# Assuming 'ccd' is your dataset containing vaccine shipment data
ccd_prop <- ccd |>
  group_by(county) |>
  summarise(
    pfizer = sum(pfizer_doses_shipped, na.rm = TRUE),
    moderna = sum(moderna_doses_shipped, na.rm = TRUE),
    jj = sum(jj_doses_shipped, na.rm = TRUE)) |>
  mutate(total = pfizer + moderna + jj) |>
  mutate(
    pfizer = pfizer / total,
    moderna = moderna / total,
    jj = jj / total) |>
  select(-total) |>
  pivot_longer(cols = c(pfizer, moderna, jj),
               names_to = "vaccine",
               values_to = "proportion")

# Create the stacked bar chart
ggplot(ccd_prop, aes(x = fct_rev(factor(county)), y = proportion, fill = vaccine)) +
  geom_col(position = "stack") +
  coord_flip() +  # Flip coordinates for horizontal bars
  theme_light() +
  labs(
    title = "Proportion of Vaccine Doses Shipped by County",
    x = "County",
    y = "Proportion of Total Doses",
    fill = "Vaccine Type") +
  scale_fill_manual(values = c("pfizer" = "darkblue", 
                               "moderna" = "firebrick", 
                               "jj" = "#e31a1c")) 
