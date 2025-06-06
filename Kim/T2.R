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

ccd |>
  group_by(county) |>
  summarize(Pfizer = sum(pfizer_doses_delivered, na.rm = TRUE),
    Moderna = sum(moderna_doses_delivered, na.rm = TRUE),
    Johnson_Johnson = sum(jj_doses_delivered, na.rm = TRUE)) |>
  pivot_longer(cols = c(Pfizer, Moderna, Johnson_Johnson),
               names_to = "Brand",
               values_to = "Doses_Delivered")|>
  ggplot(aes(x = reorder(county, -Doses_Delivered), y = Doses_Delivered, fill = Brand)) +
  geom_col() +
  coord_flip() +
  labs(title = "Total COVID-19 Vaccine Doses Delivered by County and Brand",
    x = "County",y = "Doses Delivered") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer()

#calc proportions by county
library(dplyr)
library(tidyr)
library(ggplot2)

ccd_prop <- ccd |>
  group_by(county) |>
  summarise(
    pfizer = sum(pfizer_doses_shipped, na.rm = TRUE),
    moderna = sum(moderna_doses_shipped, na.rm = TRUE),
    jj = sum(jj_doses_shipped, na.rm = TRUE)) |>
  mutate(total = pfizer + moderna + jj) |>
  mutate(pfizer = pfizer / total,
    moderna = moderna / total,
    jj = jj / total) |>
  select(-total) |>
  pivot_longer(cols = c(pfizer, moderna, jj),
               names_to = "vaccine",
               values_to = "proportion")

ggplot(ccd_prop, aes(x = fct_rev(factor(county)), y = proportion, fill = vaccine)) +
   geom_col(position = "stack") +
  scale_fill_manual(values = c("pfizer" = "darkblue", 
                               "moderna" = "firebrick", 
                               "jj" = "#FF9999"))  +
   coord_flip() +
   theme_minimal() +
   labs(title = "Proportion of Vaccine Doses Shipped by County",
     x = "County",
     y = "Proportion of Total Doses",
     fill = "Vaccine Type")

