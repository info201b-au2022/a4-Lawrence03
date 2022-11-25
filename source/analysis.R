library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(maps)
library(sf)
library(scales)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Section 2  ----
#----------------------------------------------------------------------------#
# Generate summary variables
#----------------------------------------------------------------------------#
# Gather the proportion of black prison population in 1990
get_black_prop_1990 <- function() {
  # Load the data
  data <- read_csv("../data/incarceration_trends.csv")

  # Calculate the proportion of black prisoners in 1970
  black_prop_1990 <- data %>%
    filter(year == 1990) %>%
    summarize(black_prop_1990 = sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE))
  pull(black_prop_1990)

  return(label_percent()(unlist(black_prop_1990)))
}

# Gather the proportion of black prision admission in 1990
get_black_adm_prop_1990 <- function() {
  # Load the data
  data <- read_csv("../data/incarceration_trends.csv")

  # Calculate the proportion of black prisoners in 1970
  black_adm_prop_1990 <- data %>%
    filter(year == 1990) %>%
    summarize(black_adm_prop_1990 = sum(black_prison_adm, na.rm = TRUE) / sum(total_prison_adm, na.rm = TRUE))
  pull(black_adm_prop_1990)

  return(label_percent()(unlist(black_adm_prop_1990)))
}

# Gather the proportion of black prison population in 2010
get_black_prop_2010 <- function() {
  # Load the data
  data <- read_csv("../data/incarceration_trends.csv")

  # Calculate the proportion of black prisoners in 1970
  black_prop_2010 <- data %>%
    filter(year == 2010) %>%
    summarize(black_prop_2010 = sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE))
  pull(black_prop_2010)

  return(label_percent()(unlist(black_prop_2010)))
}

# Gather the proportion of black prision admission in 2010
get_black_adm_prop_2010 <- function() {
  # Load the data
  data <- read_csv("../data/incarceration_trends.csv")

  # Calculate the proportion of black prisoners in 1970
  black_adm_prop_2010 <- data %>%
    filter(year == 2010) %>%
    summarize(black_adm_prop_2010 = sum(black_prison_adm, na.rm = TRUE) / sum(total_prison_adm, na.rm = TRUE))
  pull(black_adm_prop_2010)

  return(label_percent()(unlist(black_adm_prop_2010)))
}

# Gather the proportion of white prison population in 1990
get_white_prop_1990 <- function() {
  # Load the data
  data <- read_csv("../data/incarceration_trends.csv")

  # Calculate the proportion of white prisoners in 1970
  white_prop_1990 <- data %>%
    filter(year == 1990) %>%
    summarize(white_prop_1990 = sum(white_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE))
  pull(white_prop_1990)

  return(label_percent()(unlist(white_prop_1990)))
}

# Gather the proportion of white prision admission in 1990
get_white_adm_prop_1990 <- function() {
  # Load the data
  data <- read_csv("../data/incarceration_trends.csv")

  # Calculate the proportion of white prisoners in 1970
  white_adm_prop_1990 <- data %>%
    filter(year == 1990) %>%
    summarize(white_adm_prop_1990 = sum(white_prison_adm, na.rm = TRUE) / sum(total_prison_adm, na.rm = TRUE))
  pull(white_adm_prop_1990)

  return(label_percent()(unlist(white_adm_prop_1990)))
}

# Gather the proportion of white prison population in 2010
get_white_prop_2010 <- function() {
  # Load the data
  data <- read_csv("../data/incarceration_trends.csv")

  # Calculate the proportion of white prisoners in 1970
  white_prop_2010 <- data %>%
    filter(year == 2010) %>%
    summarize(white_prop_2010 = sum(white_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE))
  pull(white_prop_2010)

  return(label_percent()(unlist(white_prop_2010)))
}

# Gather the proportion of white prision admission in 2010
get_white_adm_prop_2010 <- function() {
  # Load the data
  data <- read_csv("../data/incarceration_trends.csv")

  # Calculate the proportion of white prisoners in 1970
  white_adm_prop_2010 <- data %>%
    filter(year == 2010) %>%
    summarize(white_adm_prop_2010 = sum(white_prison_adm, na.rm = TRUE) / sum(total_prison_adm, na.rm = TRUE))
  pull(white_adm_prop_2010)

  return(label_percent()(unlist(white_adm_prop_2010)))
}

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function return a data frame that can later be used to plot the growth
# of the U.S. prison population from 1970 to 2018.
get_year_jail_pop <- function() {
  # Read in the data
  df <- read_csv("../data/incarceration_trends.csv")

  # Select the columns we need
  df <- df %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(year >= 1970 & year <= 2018)

  # Return the data frame
  return(df)
}

# This function returns a bar chart that shows the growth of the U.S. prison
# population from 1970 to 2018.
plot_jail_pop_for_us <- function() {
  # Call the function to get the data frame
  df <- get_year_jail_pop()

  # Plot the data
  chart <- ggplot(df, aes(x = year, y = total_jail_pop)) +
    geom_bar(stat = "identity") +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme(plot.title = element_text(size = 10))

  # Return the chart
  return(chart)
}

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
#----------------------------------------------------------------------------#
# This function returns a data frame that can later be used to plot the growth
# of the prison population by the given state(s) from 1970 to 2018.
get_jail_pop_by_states <- function(states) {
  # Read in the data
  df <- read_csv("../data/incarceration_trends.csv")

  # Select the columns we need
  df <- df %>%
    select(year, state, total_jail_pop) %>%
    group_by(year, state) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(year >= 1970 & year <= 2018) %>%
    subset(state %in% states)

  # Return the data frame
  return(df)
}

# This function returns a line chart that shows the growth of the prison
# population by the given state(s) from 1970 to 2018.
plot_jail_pop_by_states <- function(states) {
  # Call the function to get the data frame
  df <- get_jail_pop_by_states(states)

  # Plot the data
  chart <- ggplot(df, aes(x = year, y = total_jail_pop)) +
    geom_line(aes(color = state)) +
    scale_color_manual(values = c("red", "blue", "green", "orange", "purple")) +
    labs(
      title = paste0(c("Increase of Jail Population in", states, "(1970-2018)"), collapse = " "),
      x = "Year",
      y = "Total Jail Population"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme(plot.title = element_text(size = 10))

  # Return the chart
  return(chart)
}

## Section 5  ----
#----------------------------------------------------------------------------#
# Comparison of Juvenile Prison Population and Total Population by State in 2018
#----------------------------------------------------------------------------#
# This function returns a data frame that can later be used to plot the
# comparison of the juvenile prison population in 2018
get_juvenile_pop_by_state <- function() {
  # Read in the data
  df <- read_csv("../data/incarceration_trends.csv")

  # Select the columns we need
  df <- df %>%
    filter(year == 2018) %>%
    select(state, total_pop, female_juvenile_jail_pop, male_juvenile_jail_pop)

  # Convert to proportions
  df <- df %>%
    mutate(juvenile_jail_pop = female_juvenile_jail_pop + male_juvenile_jail_pop) %>%
    group_by(state) %>%
    summarise(juvenile_jail_pop = sum(juvenile_jail_pop, na.rm = TRUE), total_pop = sum(total_pop, na.rm = TRUE)) %>%
    select(state, total_pop, juvenile_jail_pop) %>%
    filter(juvenile_jail_pop > 0)

  # Return the data frame
  return(df)
}

# This function returns a scartterplot that shows the comparison of the juvenile prison
plot_juvenile_pop_by_state <- function() {
  # Call the function to get the data frame
  df <- get_juvenile_pop_by_state()

  # Plot the data
  chart <- ggplot(df, aes(x = total_pop, y = juvenile_jail_pop)) +
    geom_point() +
    geom_label_repel(aes(label = state), size = 4) +
    scale_x_continuous(labels = scales::comma) +
    labs(
      x = "Total Population",
      y = "Total Prison Juvenile Population",
      title = "Colleration between State's Total Population and Prison Juvenile Population in 2018"
    ) +
    theme(plot.title = element_text(size = 10))
  return(chart)
}

## Section 6  ----
#----------------------------------------------------------------------------#
# Comparsion between Latinx Jail Population and State's Total Population
# in 2018 using Map
#----------------------------------------------------------------------------#
# This function returns a data frame that can later be used to plot the
# map view of the comparison between Latinx Jail Population and State's Total
# Population in 2018
get_latinx_pop_by_state <- function() {
  # Read in the data
  df <- read_csv("../data/incarceration_trends.csv")

  # Select the columns we need
  df <- df %>%
    filter(year == 2018) %>%
    select(state, total_jail_pop, latinx_jail_pop)

  # Convert to proportions
  df <- df %>%
    group_by(state) %>%
    summarise(latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE), total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    mutate(latinx_jail_prop = latinx_jail_pop / total_jail_pop) %>%
    select(state, latinx_jail_prop) %>%
    filter(latinx_jail_prop > 0)

  # Return the data frame
  return(df)
}

# This function returns a map view that shows the comparison between Latinx
# Jail Population and State's Total Population in 2018
plot_latinx_pop_by_state <- function() {
  # Call the function to get the data frame
  df <- get_latinx_pop_by_state()
  state_code <- read_csv("../source/state_names_and_codes.csv")
  state_code$State <- tolower(state_code$State)
  df <- full_join(df, state_code, by = c("state" = "Code")) %>%
    select(latinx_jail_prop, State)
  states_map <- map_data("state")

  # Plot the data
  chart <- ggplot(df, aes(map_id = State)) +
    geom_map(aes(fill = latinx_jail_prop), map = states_map) +
    coord_sf(
      crs = 5070, default_crs = 4326,
      xlim = c(-125, -70), ylim = c(25, 52)
    ) +
    scale_fill_gradient(low = "white", high = "red", labels = percent) +
    labs(
      title = "Latinx Jail Population in 2018",
      fill = "Latinx Jail Population"
    ) +
    theme(plot.title = element_text(size = 10))

  # Return the chart
  return(chart)
}
