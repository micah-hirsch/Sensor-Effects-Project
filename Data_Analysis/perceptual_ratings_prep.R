# Perceptual Data Prep

# Author: Micah E. Hirsch, M.S.

# Purpose: To load and clean perceptual ratings for the sensors project.

# Loading Required Packages
library(rio) # install.packages("rio")
library(tidyverse) # install.packages("tidyverse")
library(janitor) # install.packages("janitor")
library(datadictionary) # install.packages("datadictionary")

# Loading in the perceptual data

## Create objects to hold directory paths for loading and exporting data
raw_wd <- "~/Documents/Github/Sensor-Effects-Project/Raw_Perceptual_Data/"
demo_wd <- "~/Documents/Github/Sensor-Effects-Project/Listener_demographics/"
save_wd <- "~/Documents/Github/Sensor-Effects-Project/Data_Analysis"

setwd(raw_wd)

## Get file names from raw data folder

file_list <- list.files(path = ".", pattern = ".csv")

## Create empty list to temporarily store imported perceptual data
data_list <- list()

## Initiating Loop to load perceptual data

for (file in file_list) {
  
  ## Import file
  data <- rio::import(file)
  
  data <- data |>
    janitor::clean_names() |>
    dplyr::select(c(participant_private_id, task_name, trial_number, display,
                  object_name, spreadsheet_speaker:spreadsheet_section, store_intel, store_nat)) |>
    dplyr::filter(object_name == "intelligibility rating" | object_name == "naturalness rating") |>
    dplyr::mutate(rating = case_when(object_name == "intelligibility rating" ~ store_intel,
                                     TRUE ~ store_nat),
                  reliability = case_when(spreadsheet_block == 1 ~ "initial",
                                          spreadsheet_block == 3 ~ "initial",
                                          TRUE ~ "reliability"),
                  rating_type = case_when(object_name == "intelligibility rating" ~ "intelligibility",
                                            TRUE ~ "naturalness")) |>
    dplyr::select(c(participant_private_id, task_name, trial_number, spreadsheet_speaker, spreadsheet_time, 
                    spreadsheet_section, rating:rating_type))
  
  ## Add each dataset to the empty data list
  data_list[[length(data_list)+1]] <- data
  
}

## Merging all the perceptual data into one dataframe
percep_data <- do.call(rbind, data_list)

## Removing unneeded objects from the environment
rm( data, data_list, file, file_list)

# Cleaning Perceptual Rating Variables

percep_data <- percep_data |>
  dplyr::rename(listener_id = participant_private_id,
                counterbalance = task_name,
                speaker_id = spreadsheet_speaker,
                time_point = spreadsheet_time,
                cp_section = spreadsheet_section) |>
  dplyr::mutate(counterbalance = gsub("Perceptual Ratings ", "", counterbalance),
                counterbalance = as.factor(counterbalance),
                speaker_id = str_replace(speaker_id, "\\*", ""),
                time_point = factor(time_point, levels = c("before", "sensors", "after")),
                reliability = factor(reliability, levels = c("initial", "reliability")),
                rating_type = factor(rating_type, levels = c("intelligibility", "naturalness")))

# Loading in listener demographics

## Set working directory to raw demographics file
setwd(demo_wd)

## Loading and cleaning listener demographic df
listener_demo <- rio::import("listener_demo_raw.csv") |>
  janitor::clean_names() |>
  dplyr::select(c(participant_private_id, object_name, key, response)) |>
  dplyr::filter(key == "value") |>
  dplyr::select(!key) |>
  tidyr::pivot_wider(names_from = object_name,
                     values_from = response) |>
  dplyr::select(!location) |>
  dplyr::rename(listener_id = participant_private_id) |>
  dplyr::mutate(english = as.factor(english),
                age = as.numeric(age),
                gender = as.factor(gender),
                race = as.factor(race),
                ethnicity = as.factor(ethnicity))

# Exporting Data

## Set working directory
setwd(save_wd)

## Perceptual Ratings
rio::export(percep_data, "perceptual_ratings.csv")

## Data dictionary for perceptual ratings
percep_labels <- c(listener_id = "Listener ID",
                   counterbalance = "Counterbalance List",
                   trial_number = "Trial Number in Block",
                   speaker_id = "Speaker ID",
                   time_point = "Recording Time Point",
                   cp_section = "Section of the Caterpillar Passage Presented to Listener",
                   rating = "VAS Rating",
                   reliability = "Reliability",
                   rating_type = "Rating Type")

percep_data_dict <- datadictionary::create_dictionary(percep_data, var_labels = percep_labels)

rio::export(percep_data_dict, "perceptual_ratings_dictionary.csv")

## Listener Demographics
rio::export(listener_demo, "listener_demographics.csv")

## Listener Demographics Data Dictionary
listener_labels <- c(listener_id = "Listener ID",
                     english = "Fluent in English",
                     age = "Age",
                     gender = "Gender",
                     race = "Race",
                     ethnicity = "Ethnicity")

listener_demo_dict <- datadictionary::create_dictionary(listener_demo, var_labels = listener_labels)

rio::export(listener_demo_dict, "listener_demographics_dictionary.csv")

# Clear Environment
rm(list=ls())
