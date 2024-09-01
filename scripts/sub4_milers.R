## Load packages annd data ----

# Packages
library(readr)  # reading 
library(dplyr)  # cleaning
library(tidyr)  # cleaning
library(glue)  # cleaning
library(janitor)  #cleaning
library(lubridate)  # cleaning dates and times
library(forcats)  # for factors
library(ggplot2)  # plotting
library(ggtext)  # plotting
library(showtext)  # plotting with fonts

# Data
milers_raw <- read_csv("data/sub4_data.csv", col_names = c("name", "time", "location", "date", "extra"))
universities <- read_csv("data/universities.csv", col_names = c("university"))


## Cleaning ----
# Separate text in columns 
milers <- milers_raw %>% 
  separate(name, c("year", "name"), "\\.") %>% 
  separate(name, c("name", "affiliation"), "\\(") %>% 
  separate(time, c("time", "place"), "\\ ") 

# Sub unneeded characters with "" to remove them
milers$affiliation <-  gsub("\\)", "", milers$affiliation)
milers$time <-  gsub("\\i", "", milers$time)
milers$time <-  gsub("\\*", "", milers$time)

# Fill years and remove unwanted columns
milers <- milers %>% 
  mutate(year = as.numeric(year)) %>%  # convert to year to use ifelse in next step
  mutate(year = ifelse(year < 1000, NA, year)) %>%  # keep the year values and put NA for anything else
  mutate(year = as.numeric(year)) %>% # convert to numeric to use fill in next step
  fill(year) %>%  # fill year down
  filter(!is.na(name))  %>%  # filter out rows with NA in name
  select(-place) 

# Append a 0 to the hundredths place for times that have only tenths
milers$time <- gsub("(\\.\\d{1}$)", "\\1\\0", milers$time)

# Function to convert time string to seconds
time_to_seconds <- function(time_str) {
  
  time_components <- strsplit(time_str, "[:.]")[[1]]
  minutes_to_seconds <- as.numeric(time_components[1]) * 60
  seconds_to_seconds <- as.numeric(time_components[2])
  milliseconds_to_seconds <- as.numeric(time_components[3]) / 100
  total_seconds <- minutes_to_seconds + seconds_to_seconds + milliseconds_to_seconds
  
  return(total_seconds)
}

# Apply the conversion function to each element of the column
milers$seconds <- sapply(milers$time, time_to_seconds)

# Create a new column for decades
milers$decade <- (milers$year %/% 10) * 10

# Cleaned data frame
milers_cleaned <- milers


## Prep for figure ----
# Create a new data frame with running count
running_count <- milers %>%
  group_by(year, affiliation) %>%
  summarise(count = n()) %>%
  group_by(affiliation) %>%
  mutate(running_count = cumsum(count)) %>%
  select(-count)

# Create new df with only universities as there are some professional athletes and we want to exclude those
running_count_uni <- running_count %>% 
  filter(affiliation %in% universities$university)


# For loop to add future years if a university's last sub 4 was before 2023. This is so each affiliation ends at 2023.
# Initialize df with years and empty df to use in the loop
years_df <- tibble(year = 1957:2023)
running_count_uni_2023 <- data.frame()

# Run for loop. This is kind of slow but it works. 
for(school in universities$university){
  
  # filter by school
  running_count_uni_filtered <- running_count_uni %>% 
    filter(affiliation == school)
  
  # create df with all years
  temp_years <- years_df %>% 
    left_join(running_count_uni_filtered, by = "year") %>% 
    fill(running_count) %>% 
    fill(affiliation) %>% 
    filter(!is.na(affiliation))
  
  # bind rows together in main data frame
  running_count_uni_2023 <- rbind(running_count_uni_2023, temp_years)
  
}

# Only include unis with 5 or more milers to simplify the figure
running_count_uni_2023_min5 <- running_count_uni_2023 %>% 
  group_by(affiliation) %>%
  filter(max(running_count) > 4)


## Create df that only has the top 3 universities to use for the standout lines ##
# Filter for the top 3 unis 
running_count_uni_2023_topthree_lines <- running_count_uni_2023 %>% 
  filter(affiliation == "Oregon" | affiliation == "Stanford" | affiliation == "Georgetown")

# Update the order of the top 3
running_count_uni_2023_topthree_lines$affiliation <- factor(running_count_uni_2023_topthree_lines$affiliation, levels = c("Oregon", "Stanford", "Georgetown"))


## Create df that only has the max points of top 3 universities with to use to label the standout lines ##
running_count_uni_2023_topthree_points <- running_count_uni_2023_topthree_lines %>% 
  group_by(affiliation) %>% 
  filter(year == 2023) %>% 
  slice_max(running_count)

# Update the order of the top 3
running_count_uni_2023_topthree_points$affiliation <- factor(running_count_uni_2023_topthree_points$affiliation, levels = c("Oregon", "Stanford", "Georgetown"))


## Create figure ----

# Add fonts
font_add_google("Source Sans 3")
showtext_auto()

# Plotting
sub4_milers_plot <- running_count_uni_2023_min5 %>% 
  ggplot(mapping = aes(x = year, y = running_count, group = factor(affiliation))) +
  labs(
    title = "<span style = 'color:#154733;'>**Oregon**</span> is Miler U", 
    subtitle = "Number of sub-4 milers by team",
    caption = c("Data: Track & Field News\nViz: Tim Fulton, PhD")
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Source Sans 3"),
    plot.title = element_markdown(size = 18, hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = -2)),
    plot.caption = element_text(size = 9, face = "italic", hjust = 0, margin = margin(t = 10)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_line(color = "#D6DBD3", linetype = "dotted"),
    axis.title = element_blank(), 
    axis.text.x = element_text(size = 12, color = "#3c3c3c", margin = margin(t = -2)),
    axis.text.y = element_text(size = 12, color = "#3c3c3c"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  geom_line(
    color = "darkgray", 
    linewidth = 0.85
  ) +
  geom_line(
    inherit.aes = FALSE,
    data = running_count_uni_2023_topthree_lines, 
    mapping = aes(x = year, y = running_count, color = factor(affiliation)),
    linewidth = 1.8
  ) +
  geom_point(
    inherit.aes = FALSE,
    data = running_count_uni_2023_topthree_points, 
    mapping = aes(x = year, y = running_count, color = factor(affiliation)),
    size = 8
  ) +
  geom_text(
    inherit.aes = FALSE,
    data = running_count_uni_2023_topthree_points, 
    mapping = aes(x = year, y = running_count, label = running_count),
    size = 4,
    color = "white"
  ) + 
  geom_text(
    inherit.aes = FALSE,
    data = running_count_uni_2023_topthree_points, 
    mapping = aes(x = year, y = running_count, color = factor(affiliation), label = c(" Oregon   ", " Stanford  ", "Georgetown")),  # Custom adjust the placement with spaces
    size = 4,
    hjust = -0.2
  ) +
  scale_x_continuous(
    limits = c(1957, 2023), 
    breaks = seq(1960, 2020, by = 10),
    labels = seq(1960, 2020, by = 10),
    expand = expansion(add = c(3, 11))
  ) +
  scale_fill_manual(values = c("#154733", "#8C1515", "#041E42")) +  # Custom uni colors
  scale_color_manual(values = c("#154733", "#8C1515", "#041E42"))

# Save figure
ggsave(
  "plots/sub4_milers_plot.png", 
  plot = sub4_milers_plot,
  scale = 1, 
  width = 8, 
  height = 5, 
  dpi = 600,
  units = "in"
)

