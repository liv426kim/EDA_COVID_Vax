# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# Read the CSV file
vaccine_data <- read.csv("ccd.csv")

# Clean and prepare the data
clean_data <- vaccine_data %>%
  # Convert date columns to Date type
  mutate(first_date = as.Date(first_date),
         last_date = as.Date(last_date)) %>%
  # Select relevant columns
  select(county, year, week, first_date, last_date, 
         pfizer_doses_shipped, pfizer_doses_delivered,
         moderna_doses_shipped, moderna_doses_delivered,
         jj_doses_shipped, jj_doses_delivered,
         cumulative_pfizer_doses_shipped, cumulative_pfizer_doses_delivered,
         cumulative_moderna_doses_shipped, cumulative_moderna_doses_delivered,
         cumulative_jj_doses_shipped, cumulative_jj_doses_delivered) %>%
  # Filter to only include rows with vaccine data
  filter(pfizer_doses_shipped > 0 | moderna_doses_shipped > 0 | jj_doses_shipped > 0 |
           pfizer_doses_delivered > 0 | moderna_doses_delivered > 0 | jj_doses_delivered > 0)

# Reshape data for analysis
brand_analysis <- clean_data %>%
  pivot_longer(
    cols = c(starts_with("pfizer"), starts_with("moderna"), starts_with("jj")),
    names_to = c("brand", ".value"),
    names_pattern = "(pfizer|moderna|jj)_(doses_.*)"
  ) %>%
  mutate(brand = case_when(
    brand == "pfizer" ~ "Pfizer",
    brand == "moderna" ~ "Moderna",
    brand == "jj" ~ "Johnson & Johnson"
  )) %>%
  rename(
    shipped = doses_shipped,
    delivered = doses_delivered,
    cum_shipped = cumulative_doses_shipped,
    cum_delivered = cumulative_doses_delivered
  ) %>%
  mutate(
    delivery_efficiency = delivered / shipped,
    shelf_life_estimate = cum_shipped - cum_delivered  # Simple estimate of inventory
  )

# Calculate summary statistics by brand
brand_summary <- brand_analysis %>%
  group_by(brand) %>%
  summarise(
    total_shipped = sum(shipped, na.rm = TRUE),
    total_delivered = sum(delivered, na.rm = TRUE),
    avg_delivery_efficiency = mean(delivery_efficiency, na.rm = TRUE),
    avg_shelf_life = mean(shelf_life_estimate, na.rm = TRUE),
    max_inventory = max(shelf_life_estimate, na.rm = TRUE)
  )

library(tidyverse)
library(lubridate)
library(patchwork)

# Load the dataset
covid_vaccine_data <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_california.csv")

# Calculate shipped vs delivered rate by brand for first year
brand_delivery_rates <- covid_vaccine_data |>
  filter(year == min(year)) |>  # Limit to first year
  group_by(year, week) |>
  summarize(
    pfizer_shipped = sum(pfizer_doses_shipped, na.rm = TRUE),
    pfizer_delivered = sum(pfizer_doses_delivered, na.rm = TRUE),
    moderna_shipped = sum(moderna_doses_shipped, na.rm = TRUE),
    moderna_delivered = sum(moderna_doses_delivered, na.rm = TRUE),
    jj_shipped = sum(jj_doses_shipped, na.rm = TRUE),
    jj_delivered = sum(jj_doses_delivered, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    pfizer_delivery_rate = pfizer_delivered / pfizer_shipped,
    moderna_delivery_rate = moderna_delivered / moderna_shipped,
    jj_delivery_rate = jj_delivered / jj_shipped
  ) |>
  select(year, week, ends_with("delivery_rate")) |>
  pivot_longer(
    cols = ends_with("delivery_rate"),
    names_to = "brand",
    values_to = "delivery_rate"
  ) |>
  mutate(
    brand = str_to_title(str_replace(brand, "_delivery_rate", "")) |>
      (\(x) case_when(
        x == "Jj" ~ "Johnson & Johnson",
        TRUE ~ x
      ))()
  ) |>
  mutate(date = ymd(paste(year, "01", "01")) + weeks(week - 1)) |>
  # Remove any infinite or NA values that might break the lines
  filter(is.finite(delivery_rate))

# Create consistent y-axis limits for all plots
y_limits <- c(0, max(brand_delivery_rates$delivery_rate, na.rm = TRUE) * 1.05)
# Create a color palette
brand_colors <- c("Pfizer" = "#0047AB", "Moderna" = "#D12600", 
                  "Johnson & Johnson" = "#FF6900")

# Combined plot with all brands
combined_plot <- ggplot(brand_delivery_rates, aes(x = date, y = delivery_rate, color = brand)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Add points to show all data points
  scale_y_continuous(labels = scales::percent, limits = y_limits) +
  scale_color_manual(values = brand_colors) +
  labs(
    title = "Combined Vaccine Delivery Rates (First Year)",
    x = "Date",
    y = "Delivery Rate",
    color = "Brand"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Function to create individual brand plots
create_brand_plot <- function(brand_name) {
  ggplot(filter(brand_delivery_rates, brand == brand_name), 
         aes(x = date, y = delivery_rate)) +
    geom_line(color = brand_colors[brand_name], size = 1) +
    geom_point(color = brand_colors[brand_name], size = 2) +  # Add points
    scale_y_continuous(labels = scales::percent, limits = y_limits) +
    labs(
      title = paste0(brand_name, " Delivery Rate (First Year)"),
      x = "Date",
      y = "Delivery Rate"
    ) +
    theme_minimal()
}

# Create individual plots
jj_plot <- create_brand_plot("Johnson & Johnson")
moderna_plot <- create_brand_plot("Moderna")
pfizer_plot <- create_brand_plot("Pfizer")

# Display all plots together with consistent sizing
(combined_plot) / (jj_plot + moderna_plot + pfizer_plot) +
  plot_layout(heights = c(2, 1))  # Make combined plot taller

# Summary table
brand_delivery_summary <- brand_delivery_rates |>
  group_by(brand) |>
  summarize(
    overall_delivery_rate = mean(delivery_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    overall_delivery_rate = scales::percent(overall_delivery_rate, accuracy = 0.1)
  )

print(brand_delivery_summary)
