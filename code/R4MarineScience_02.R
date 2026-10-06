# ============================================================
# MB5370 - Techniques in Marine Science 1
# R for Marine Science - Workshop 2
# Advanced data wrangling: Extracting ecological signals from noisy systems
# Kylah Kalbfleisch
# 1 October 2026
# ============================================================


# ------------------------------------------------
# 1. Load packages
# ------------------------------------------------
library(here)
library(tidyverse)
library(palmerpenguins)
library(lubridate)

# ------------------------------------------------
# 2. Tidy data
# ------------------------------------------------

# Run tables
table1
table2
table3

# Calculate rate per 10,000
table1 %>%
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1%>%
  count(year, wt=cases)

# Visualise changes over time
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

# ------------------------------------------------
# 3. Pivoting data
# ------------------------------------------------

# View Billboard dataset
billboard

billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  )

# Remove NA values while pivoting longer
billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )

# Simple pivot_longer example
df <- tribble(
  ~id, ~bp1, ~bp2,
  "A", 100, 120,
  "B", 140, 115,
  "C", 120, 125
)

df |>
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

# ------------------------------------------------
# 4. Pivoting data wider
# ------------------------------------------------

# View example dataset
cms_patient_experience

cms_patient_experience |>
  distinct(measure_cd, measure_title)

cms_patient_experience |>
  pivot_wider(
    names_from = measure_cd,
    values_from = prf_rate
  )

# Simple pivot_wider example
df <- tribble(
  ~id, ~measurement, ~value,
  "A", "bp1", 100,
  "B", "bp1", 140,
  "B", "bp2", 115,
  "A", "bp2", 120,
  "A", "bp3", 105
)

df |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

df |>
  distinct(measurement) |>
  pull()

df |>
  select(-measurement, -value) |>
  distinct()

df |>
  select(-measurement, -value) |>
  distinct() |>
  mutate(x = NA, y = NA, z = NA)

# ------------------------------------------------
# 5. Palmer Penguins pivoting exercises
# ------------------------------------------------

# Create a long version of the penguins dataset
penguins_long <- penguins |>
  pivot_longer(
    cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
    names_to = "measurement_type",
    values_to = "value"
  )

# View the result 
head(penguins_long)

# Make histogram plot
penguins_long |>
  drop_na(value) |>
  ggplot(aes(x = value, fill = species)) +
  geom_histogram(bins = 30, alpha = 0.7, colour = "black") +
  facet_wrap(~ measurement_type, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Morphometric distributions across penguin species",
    x = "Measurement value",
    y = "Frequency"
  )

# Calculate mean body mass for each species on each island
mass_summary <- penguins |>
  drop_na(body_mass_g) |>
  group_by(species, island) |>
  summarise(mean_mass = mean(body_mass_g))
head(mass_summary)

# Widen summary
mass_matrix <- mass_summary |>
  pivot_wider(
    names_from = island,
    values_from = mean_mass)
head(mass_matrix)

# ------------------------------------------------
# 6. Separating and uniting columns
# ------------------------------------------------

# Split rate column into two variables
table3 %>%
  separate(rate, into = c("cases", "population"))

# Run explicit version
table3 %>%
  separate(rate,into = c("cases", "population"),sep = "/")

# Ask seperate() to convert chr types to integer types
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

# Split column by position 
table3 %>%
  separate(year,into = c("century", "year"),sep = 2)

# Use unite() to combine multiple columns into a single column
table5 %>% 
  unite(new, century, year, sep = "")

# ------------------------------------------------
# 7. Wrangling strings and dates
# ------------------------------------------------

# A remarkably messy data frame of field sites
messy_sites <- tibble(
  site_id = c(" Nelly Bay","nelly_bay","NELLY BAY"," Geoffrey_Bay ",
              "geoffrey bay"))

# Using stringr within mutate to standardize the text
clean_sites <- messy_sites |>
  mutate(
    # 1. Convert everything to lowercase
    site_clean = str_to_lower(site_id),
    # 2. Replace any spaces with underscores
    site_clean = str_replace_all(site_clean, pattern = " ", replacement = "_"),
    # 3. Trim any leading or trailing whitespace (invisible spaces at the ends)
    site_clean = str_trim(site_clean))
print(clean_sites)

# Load lubridate package
library(lubridate)

# Parsing different date formats
date_1 <- dmy("25/12/2026")
date_2 <- ymd("2026-12-25")

# R now recognizes these as identical Date objects
date_1 == date_2

# Create sensor data with timestamps
sensor_data <- tibble(
  raw_time = c(
    "14-05-2026 08:30:00",
    "14-05-2026 08:45:00",
    "14-05-2026 09:00:00"),
  temperature = c(24.5, 24.6, 24.4))

#Convert character strings to true POSIXct datetime objects
sensor_clean <- sensor_data |>
  mutate(
    true_time = dmy_hms(raw_time))
print(sensor_clean)

# ------------------------------------------------
# 8. Joining tables
# ------------------------------------------------

# Table 1: Biological observation data
observations <- tibble(
  site_code = c("NB", "GB", "MI", "NB", "HB"),
  species = c("Trout", "Snapper", "Trout", "Cod", "Trout"),
  count = c(5, 2, 1, 3, 8))

# Table 2: Spatial metadata
site_metadata <- tibble(
  site_code = c("NB", "GB", "MI", "RP", "WP"),
  zone = c("Marine National Park", "Conservation Park", "Habitat Protection", "General Use", "Other Use"),
  lat = c(-19.16, -19.15, -19.14, -19.12, -19.11))

# Joining metadata to our observations
joined_data <- observations |>
  left_join(site_metadata, by = join_by(site_code))
print(joined_data)

# Keep only observations that have matching metadata
matched_data <- observations |>
  inner_join(site_metadata,by = join_by(site_code))
glimpse(matched_data)

# Find observations with no matching metadata
missing_context <- observations |>
  anti_join(site_metadata,by = join_by(site_code))
print(missing_context)

# ------------------------------------------------
# 9. Handling missing values
# ------------------------------------------------

# Raw data from an old temperature logger
logger_data <- tibble(
  depth_m = c(10, 20, 30, 40),
  temp_c = c(24.5, 24.1, -999, 23.5)) # -999 is a known sensor error code

# Convert the -999 error codes to true NA values
fixed_logger <- logger_data |>
  mutate(temp_c = na_if(temp_c, -999))
print(fixed_logger)

# Simulate some count data
shark_counts <- tibble(
  site = c("Reef_A", "Reef_B", "Reef_C"),
  shark_count = c(3, NA, 5)) # The NA here actually means 0 sharks were seen

# Replace NA with 0 
shark_fixed <- shark_counts |>
  mutate(shark_count = coalesce(shark_count, 0))
print(shark_fixed)

# Create CPUE data
cpue_data <- tibble(
  site = c("Bay_1", "Bay_2"),
  catch = c(10, 0),
  effort_hours = c(2, 0))

# Calculate catch per unit effort
cpue_calc <- cpue_data |>
  mutate(
    cpue = catch / effort_hours)
print(cpue_calc)

# Raw catch data
raw_catch <- tibble(
  site = c("Reef_1", "Reef_1", "Reef_2"),
  species = c("Pmaculatus", "Pleopardus", "Pmaculatus"),
  count = c(5, 2, 8))

# Force inclusion of hidden zero-catch data
full_catch_matrix <- raw_catch |>
  complete(site, species, fill = list(count = 0))
print(full_catch_matrix)

# Create sensor data with a missing salinity reading
sensor_log <- tibble(
  day = 1:4,
  salinity = c(35.2, 35.1, NA, 35.3))

# Remove rows where salinity is missing
clean_log <- sensor_log |>
  drop_na(salinity)
print(clean_log)

# ------------------------------------------------
# 10. Practical exercises
# ------------------------------------------------

# Exercise 1: Cleaning the messy metadata ###

# Create messy island metadata
island_metadata <- tibble(
  island_name = c(" biscoe", "Dream ", "Torgersen"),
  station_install = c("15/01/2003", "22-03-2004", "05/11/2001"),
  latitude = c(-64.81, -64.73, -64.76))
print(island_metadata)

# Clean island names and convert install dates
clean_metadata <- island_metadata |>
  mutate(
    island_name = str_trim(island_name),
    island_name = str_to_title(island_name),
    station_install = dmy(station_install))
print(clean_metadata)


# Exercise 2: The relational join ###

# Join cleaned metadata to the penguins dataset
penguins_spatial <- penguins |>
  left_join(
    clean_metadata,
    by = join_by(island == island_name))
head(penguins_spatial)

# Exercise 3: The wide summary matrix ###

# Create a wide summary of maximum body mass by species and island
max_mass_matrix <- penguins_spatial |>
  drop_na(body_mass_g) |>
  group_by(species, island) |>
  summarise(
    max_body_mass = max(body_mass_g),
    .groups = "drop") |>
  pivot_wider(
    names_from = island,
    values_from = max_body_mass)
print(max_mass_matrix)

