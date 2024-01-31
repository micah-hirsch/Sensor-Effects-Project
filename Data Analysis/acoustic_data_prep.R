# Acoustic Measurement Prep

# Authors: Micah E. Hirsch, M.S. & Austin R. Thompson, Ph.D.

# Purpose: To prepare acoustic variables of interest for the speakers for analysis.

library(rio) # install.packages("rio")
library(tidyverse) #install.packages("tidyverse")
library(rPraat) # install.packages("rPraat")
library(remotes) # install.packages("remotes")
library(PraatR) # remotes:::install_github("usagi5886/PraatR")

setwd("~/Documents/Github Repositories/Sensor-Effects-Project/Raw Speaker Data/")

# Loading in the data

## Loading in file information

files <- rio::import("Sensor_Effects_Participants_Lab_Version.csv") |>
  dplyr::rename(speaker_id = sub_number) |>
  dplyr::select(speaker_id, caterpillar_no_sensors, caterpillar_conversational,
                caterpillar_end_no_sensors) |>
  dplyr::mutate_all(~gsub("\\*", "", .))

## Creating file paths

textgrid_in_paths <- paths |>
  dplyr::mutate(path = paste("Initial Segments", speaker_id, sep = "/"),
                before = paste(path, caterpillar_no_sensors, sep = "/"),
                sensors = paste(path, caterpillar_conversational, sep = "/"),
                after = paste(path, caterpillar_end_no_sensors, sep = "/")) |>
  dplyr::select(speaker_id, before, sensors, after) |>
  tidyr::pivot_longer(cols = c(before:after),
                      names_to = "timePoint",
                      values_to = "filePath")

loadData <- function(path, speaker) {
  
  path_tp_info <- path %>%
    as.data.frame() %>%
    dplyr::rename(path = 1) %>%
    
    # Finding which TP we have
    dplyr::mutate(tp = case_when(
      grepl(pattern = "before", ignore.case = T, path) ~ "before",
      grepl(pattern = "after", ignore.case = T, path) ~ "after",
      grepl(pattern = "sensor", ignore.case = T, path) ~ "sensors",
    ))
  
  # Load the TextGrid
  tg <- rPraat::tg.read(fileNameTextGrid = path,
                        encoding = "UTF-8")
  
  Phrase <- cbind(tg[[1]][["label"]],tg[[1]][["t1"]], tg[[1]][["t2"]]) %>%
    as.data.frame() %>%
    dplyr::rename(
      Segment = 1,
      onset = 2,
      offset = 3) %>%
    dplyr::mutate(
      Segment = gsub(pattern = " ",
                     replacement = "",
                     x = Segment),
      onset = as.numeric(onset),
      offset = as.numeric(offset)) %>%
    dplyr::filter(Segment != "")
  
  Phoneme <- cbind(tg[[2]][["label"]],tg[[2]][["t1"]], tg[[2]][["t2"]]) %>%
    as.data.frame() %>%
    dplyr::rename(
      Segment = 1,
      onset = 2,
      offset = 3) %>%
    dplyr::mutate(
      Segment = gsub(pattern = " ",
                     replacement = "",
                     x = Segment),
      onset = as.numeric(onset),
      offset = as.numeric(offset)) %>%
    dplyr::filter(Segment != "")
  
  speakerSegments <- rbind(Phrase, Phoneme) %>%
    dplyr::mutate(timePoint = path_tp_info$tp,
                  path = path_tp_info$path)
  
  return(speakerSegments)
  rm(Phrase, Phoneme)
} 
  
  
k <- 1
while (k <= nrow(speakers)) {
  
  speaker <- speakers$SpeakerID[k]
  file_path <- paste(rawDataPath, speaker, sep = "/")
  
  speakerFiles <- list.files(
    path = paste0(file_path,"/")) %>%
    as.data.frame() %>%
    dplyr::rename(File = 1) %>%
    dplyr::filter(grepl(
      pattern = ".TextGrid",
      x = File
    )) %>%
    dplyr::mutate(
      path = paste(file_path, File, sep = "/")
    )
  
  speakerSegments <- base::do.call(rbind,lapply(speakerFiles$path,loadData, speaker = speaker)) %>%
    dplyr::mutate(SpeakerID = speaker)
  
  # Initializing the Segments
  if (k == 1) {
    Segments <- speakerSegments %>%
      dplyr::relocate(SpeakerID, .before = "Segment")
  } else {
    Segments <- rbind(Segments, speakerSegments)
  }
  rm(speakerSegments)
  
  k <- k + 1
} 
  
  
  
  
  

  