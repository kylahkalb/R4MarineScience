# ============================================================
# MB5370 - Techniques in Marine Science 1
# R for Marine Science - Workshop 1
# Foundations of Data Science: Wrangling and Plotting
# Kylah Kalbfleisch
# 30 September 2026
# ============================================================


# ------------------------------------------------------------
# 1. Housekeeping
# ------------------------------------------------------------

#Check what objects are currently in the environment
objects()

#Clear the global environment
rm(list = ls())

#Confirm the environment is empty
objects()

# ------------------------------------------------------------
# 2. Load packages
# ------------------------------------------------------------

#Install here() package
#install.packages("here")

#Load packages
library(tidyverse)
library(readxl)
library(here)

# ------------------------------------------------------------
# 3. Practice import data
# ------------------------------------------------------------

#A: Loading a standard comma-separated plain text file
benthic_cover <- read_csv(here::here("data/workshop1/reef_cover_log.csv"))

#B: Parsing a tab-separated telemetry instrument array string
acoustic_stream <- read_tsv(here::here("data/workshop1/acoustic_telemetry_stream.txt"))

#C: Targeting a specific sheeet in a multi-tab Excel spreadsheet
fisheries_annual <- read_excel(here::here("data/workshop1/fish_catch_data.xlsx"), sheet = "Commercial_2026")

# ------------------------------------------------------------
# 3.1 Try loading mangrove dataset
# ------------------------------------------------------------

#Read in mangrove data
#mangrove_data <- read_csv(here::here("data/workshop1/mangrove_survey_raw.csv"))
#^ issues come up

#Use args within read_csv to skip headers and declare missing flags 
mangrove_data <- read_csv(here::here("data/workshop1/mangrove_survey_raw.csv"), skip = 5, na = c(".", "NA", "9999", "ND", "blank"))

# ------------------------------------------------------------
# 4. Tibbles vs legacy tables
# ------------------------------------------------------------

#Convert the tibble into a base R data frame structure
benthic_cover_df <- as.data.frame(benthic_cover)

#Print the base R data frame
print(benthic_cover_df)

#Print the original tibble
print(benthic_cover)

# ------------------------------------------------------------
# 5. Load and explore Palmer Penguins
# ------------------------------------------------------------

#Load Palmer Penguins package
library(palmerpenguins)
data("penguins")

#Examine the structure of the dataset
glimpse(penguins)
str(penguins)

#Generate a summary
summary(penguins)

# ------------------------------------------------------------
# 6. Slicing, filtering, sorting & transforming with dplyr
# ------------------------------------------------------------

# 6.1 Isolating attributes with select() ##

# Keep only specific morphology variables
morphology_metrics <- select(penguins,species,bill_length_mm,bill_depth_mm,body_mass_g)
glimpse(morphology_metrics)

# Keep a continuous block of columns
spatial_block <- select(penguins,species:island)

# Remove the year column
clean_scientific_fields <- select(penguins,-year)

# 6.2 Sifting rows with filter() ##

#Keep only Adelie penguins
adelie_cohort <- filter(penguins,species == "Adelie")

#Keep penguins heavier than 4500 g
heavy_penguins <- filter(penguins,body_mass_g > 4500)

#Keep Gentoo penguins from Biscoe Island
biscoe_gentoo <- filter(penguins,species == "Gentoo" & island == "Biscoe")

#Keep penguins from Dream or Torgersen Islands
sub_islands <- filter(penguins,island %in% c("Dream", "Torgersen"))

# 6.3 Ordering sequences with arrange() ##

#Sort penguins from lightest to heaviest
lightest_first <- arrange( penguins,body_mass_g)

#Sort penguins from heaviest to lightest
heaviest_first <- arrange(penguins,desc(body_mass_g))

#Sort by species, then by bill length from largest to smallest
stratified_morphology <- arrange(penguins,species,desc(bill_length_mm))

# 6.4 Using the pipe ##

#Create bill ratio, then keep only Adelie penguins
penguins_final <- penguins |>mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>filter(species == "Adelie")

# 6.5 Computing new attributes with mutate() ##

#Create new morphology variables
penguin_ratios <- penguins |>mutate(body_mass_kg = body_mass_g / 1000,bill_ratio = bill_length_mm / bill_depth_mm)

#Inspect the new columns
glimpse(penguin_ratios)

# ------------------------------------------------------------
# 7. Grouping and summarizing data
# ------------------------------------------------------------

# 7.1 Group penguins by species ##
grouped_penguins <- group_by(penguins,species)

#View grouped data
print(grouped_penguins)

# 7.2 Summarize mean body mass by species ##
species_mass_summary <- summarise(grouped_penguins,
  mean_mass_g = mean(body_mass_g))

#View the summary
print(species_mass_summary)

# 7.3 Handle missing values correctly ##
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),                                     # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),         # Calculate mean ignoring missing cells
    sd_mass_g   = sd(body_mass_g, na.rm = TRUE)            # Standard deviation calculation
  )

# View the summary table
print(biological_signal)


