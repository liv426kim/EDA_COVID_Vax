Olivia Kim File

library(tidyverse)
covid_cases_deaths <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_california.csv")

ccd <- covid_cases_deaths

summarize(ccd)
summary(ccd)

names(ccd)
dim(ccd)



ccd |>
  mutate(week_date = as.Date(first_date)) |>
  group_by(week_date) |>
  summarize(pfizer_rate = sum(pfizer_doses_delivered, na.rm = TRUE) / sum(pfizer_doses_shipped, na.rm = TRUE),
    moderna_rate = sum(moderna_doses_delivered, na.rm = TRUE) / sum(moderna_doses_shipped, na.rm = TRUE),
    jj_rate = sum(jj_doses_delivered, na.rm = TRUE) / sum(jj_doses_shipped, na.rm = TRUE)) |>
  pivot_longer(cols= c(pfizer_rate, moderna_rate, jj_rate),
               names_to= "brand",
               values_to= "shipping_rate") |>
  mutate(brand = recode(brand,
                        "pfizer_rate"= "Pfizer",
                        "moderna_rate"= "Moderna",
                        "jj_rate" = "Johnson & Johnson")) |>
  ggplot(aes(x = week_date, y = shipping_rate, color= brand)) +
  geom_line(size = 1) +
  labs(
    title = "COVID-19 Vaccine Shipping Rate Over Time by Brand",
    x = "Week",
    y = "Shipping Rate (Delivered / Shipped)") +
  theme_minimal()
