#! /usr/bin/env Rscript

# Import necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

# Define date range for filtering
start_date <- as.Date("2019-10-01")
end_date <- as.Date("2024-07-01")

# Vectorized function to convert a Date object from fiscal to calendar date
fiscal_to_calendar_date <- function(date, fiscal_start_month = 10) {
  fiscal_year <- year(date)
  fiscal_month <- month(date)
  calendar_year <- ifelse(
    fiscal_month >= fiscal_start_month,
    fiscal_year - 1,
    fiscal_year
  )
  as.Date(paste(calendar_year, fiscal_month, "01", sep = "-"))
}

# Function to read and preprocess birth data
process_birth_data <- function(file_path) {
  read.csv(file_path) %>%
    mutate(Date = as.Date(paste0("01-", Month, "-", Year),
      format = "%d-%b-%Y"
    )) %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(Date, Births) %>%
    pivot_longer(
      cols = Births,
      names_to = "Encounter_Type",
      values_to = "Count"
    )
}

# Function to process border data
process_border_data <- function(file_path) {
  read.csv(file_path) %>%
    mutate(
      Date = as.Date(paste0("01-", X), format = "%d-%b-%y"),
      Gotaways = as.numeric(gsub(",", "", Gotaways))
    ) %>%
    filter(Date >= start_date & Date < end_date) %>%
    pivot_longer(
      cols = Gotaways,
      names_to = "Encounter_Type",
      values_to = "Count"
    )
}

# Function to process nationwide encounter data
process_nationwide_data <- function(file_path, start_date, end_date) {
  data <- read_csv(file_path) %>%
    mutate(Date = fiscal_to_calendar_date(
      as.Date(paste0("01-", `Month (abbv)`, "-", `Fiscal Year`),
        format = "%d-%b-%Y"
      )
    )) %>%
    filter(Date >= start_date & Date < end_date)

  # Southwest Land Border Data
  data_southwest <- data %>%
    filter(
      `Land Border Region` == "Southwest Land Border",
      `Encounter Type` != "Expulsions"
    ) %>%
    group_by(Date) %>%
    summarize(Southwest_Border = sum(`Encounter Count`)) %>%
    pivot_longer(
      cols = Southwest_Border,
      names_to = "Encounter_Type",
      values_to = "Count"
    )

  # Other Areas Data
  data_other <- data %>%
    filter(
      `Land Border Region` != "Southwest Land Border",
      `Encounter Type` != "Expulsions"
    ) %>%
    group_by(Date) %>%
    summarize(Other_Areas = sum(`Encounter Count`)) %>%
    pivot_longer(
      cols = Other_Areas,
      names_to = "Encounter_Type",
      values_to = "Count"
    )

  bind_rows(data_southwest, data_other)
}

# Read and process datasets

# Source: Wonder CDC (https://wonder.cdc.gov/natality.html)
birth_data_long <- process_birth_data("provisional-natality-2016-2024.csv")
# Source: Cato Institute (https://www.cato.org/blog/border-patrol-70-drop-successful-evasions-title-42-ended)
border_data_long <- process_border_data("gotaways-data-cato-institute.csv")
# Source: CBP, FY 2021-2024 (https://www.cbp.gov/document/stats/nationwide-encounters)
nationwide_data_long <- process_nationwide_data(
  "nationwide-encounters-fy21-fy24-aug-aor.csv",
  start_date = start_date,
  end_date = end_date
)
# Source: CBP, FY 2020-2022 (https://www.cbp.gov/sites/default/files/assets/documents/2022-Oct/nationwide-encounters-fy20-fy22.csv)
nationwide_data_2020_2022_long <- process_nationwide_data(
  "nationwide-encounters-fy20-fy22.csv",
  start_date = start_date,
  end_date = as.Date("2020-10-01")
)

# Combine only border-related data for the stacked bars
combined_data <- bind_rows(
  border_data_long,
  nationwide_data_long,
  nationwide_data_2020_2022_long
)

# Create the plot with stacked bars for border-related data and line for birth data
ggplot() +
  # Stacked bars for border-related data
  geom_col(
    data = combined_data,
    aes(
      x = Date,
      y = Count,
      fill = factor(Encounter_Type,
        levels = c("Gotaways", "Southwest_Border", "Other_Areas")
      )
    ),
    position = "stack"
  ) +

  # Line for birth data
  geom_line(
    data = birth_data_long,
    aes(
      x = Date,
      y = Count,
      color = "Births"
    ),
    linewidth = 1
  ) +

  # Customize the plot
  labs(
    title = "Total Border Encounters + Gotaways - Expelled vs US Births Over Time",
    x = "Date",
    y = "Count",
    fill = "",
    color = "",
    caption = "Sources: \nCato Institute: Border Patrol: 70 Percent Drop in Successful Evasions Since Title 42 Ended\nU.S. Customs and Border Protection: Nationwide Encounters (2020-2022)\nU.S. Customs and Border Protection: Nationwide Encounters (2021-2024)\nUS Births: CDC Wonder. Natality Information"
  ) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  scale_y_continuous(breaks = seq(0, 500000, by = 50000), labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("Births" = "red"))

# Save the plot
ggsave("encounters_vs_births_trend.png", height = 8, width = 16, dpi = 300)
