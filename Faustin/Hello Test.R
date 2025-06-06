
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
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = scales:: percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "gray50"),
    axis.ticks.x = element_line(color = "gray50"),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  labs(
    title = "Proportion of Vaccine Doses by County (Alphabetical Order)",
    x = NULL,
    y = "Proportion of Total Doses",
    fill = "Vaccine Manufacturer"
  )+
  scale_fill_manual(values = c("pfizer" = "darkblue", 
                               "moderna" = "firebrick", 
                               "jj" = "#FF9999"))
