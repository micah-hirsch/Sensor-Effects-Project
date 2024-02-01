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
  dplyr::mutate_all(~gsub("\\*", "", .))

## Extracting speaker info

speakers <- files |>
  dplyr::select(speaker_id:age)

## Creating file paths df

textgrid_in_paths <- files |>
  dplyr::mutate(path = paste("Initial Segments", speaker_id, sep = "/"),
                before = paste(path, caterpillar_no_sensors, sep = "/"),
                sensors = paste(path, caterpillar_conversational, sep = "/"),
                after = paste(path, caterpillar_end_no_sensors, sep = "/")) |>
  dplyr::select(speaker_id, before, sensors, after) |>
  tidyr::pivot_longer(cols = c(before:after),
                      names_to = "tp",
                      values_to = "path") |>
  dplyr::mutate(path = paste(path, "TextGrid", sep = "."))

## Loading Textgrids

loadData <- function(path, speaker) {
  
  # Extracting timepoint info
  path_tp_info <- path |>
    as.data.frame() |>
    dplyr::rename(path = 1) |>
    dplyr::left_join(textgrid_in_paths, by = "path")
  
  # Load the TextGrid
  tg <- rPraat::tg.read(fileNameTextGrid = path,
                        encoding = "UTF-8")
  
  # Getting phrase-level segments
  Phrase <- cbind(tg[[1]][["label"]],tg[[1]][["t1"]], tg[[1]][["t2"]]) |>
    as.data.frame() |>
    dplyr::rename(
      Segment = 1,
      onset = 2,
      offset = 3) |>
    dplyr::mutate(
      Segment = gsub(pattern = " ",
                     replacement = "",
                     x = Segment),
      onset = as.numeric(onset),
      offset = as.numeric(offset)) |>
    dplyr::filter(Segment != "")
  
  #Getting phoneme-level segments
  Phoneme <- cbind(tg[[2]][["label"]],tg[[2]][["t1"]], tg[[2]][["t2"]]) |>
    as.data.frame() |>
    dplyr::rename(
      Segment = 1,
      onset = 2,
      offset = 3) |>
    dplyr::mutate(
      Segment = gsub(pattern = " ",
                     replacement = "",
                     x = Segment),
      onset = as.numeric(onset),
      offset = as.numeric(offset)) |>
    dplyr::filter(Segment != "")
  
  speakerSegments <- rbind(Phrase, Phoneme) |>
    dplyr::mutate(timePoint = path_tp_info$tp,
                  path = path_tp_info$path)
  
  return(speakerSegments)
  rm(Phrase, Phoneme)
} 
  
  
k <- 1
while (k <= nrow(files)) {
  
  speaker <- speakers$speaker_id[k]
  file_path <- paste("Initial Segments", speaker, sep = "/")
  
  speakerFiles <- list.files(
    path = paste0(file_path,"/")) |>
    as.data.frame() |>
    dplyr::rename(File = 1) |>
    dplyr::filter(grepl(
      pattern = ".TextGrid",
      x = File
    )) |>
    dplyr::mutate(
      path = paste(file_path, File, sep = "/")
    )
  
  speakerSegments <- base::do.call(rbind,lapply(speakerFiles$path,loadData, speaker = speaker)) |>
    dplyr::mutate(speaker_id = speaker)
  
  # Initializing the Segments
  if (k == 1) {
    Segments <- speakerSegments |>
      dplyr::relocate(speaker_id, .before = "Segment")
  } else {
    Segments <- rbind(Segments, speakerSegments)
  }
  rm(speakerSegments)
  
  k <- k + 1
} 

Segments <- Segments |>
  # Fixing human coding error
  dplyr::mutate(Segment = case_when(Segment == "Phrase_3.2_9" ~ "Phrase3.2_9",
                                    Segment == "Phase3.3_10" ~ "Phrase3.3_10",
                                    TRUE ~ Segment)) |>
  dplyr::left_join(speakers, by = "speaker_id")

## Pulling out phrase segments

phrases <- Segments |>
  dplyr::filter(grepl("phrase", ignore.case = T, Segment)) |>
  dplyr::mutate(phrase = case_when(grepl(pattern = "Phrase1", x = Segment) ~ "Phrase 1",
                                   grepl(pattern = "Phrase2", x = Segment) ~ "Phrase 2",
                                   grepl(pattern = "Phrase3", x = Segment) ~ "Phrase 3"),
                syllables = sub(".*_", "", Segment),
                syllables = as.numeric(syllables))

phonemes <- Segments |>
  dplyr::filter(!grepl(pattern = "phrase",
                       ignore.case = T,
                       x = Segment))

## Loading in wav files

wav_paths <- files |>
  dplyr::mutate(path = paste("Recordings", speaker_id, sep = "/"),
                before = paste(path, caterpillar_no_sensors, sep = "/"),
                sensors = paste(path, caterpillar_conversational, sep = "/"),
                after = paste(path, caterpillar_end_no_sensors, sep = "/")) |>
  dplyr::select(speaker_id, before, sensors, after) |>
  tidyr::pivot_longer(cols = c(before:after),
                      names_to = "tp",
                      values_to = "path") |>
  dplyr::mutate(path = paste(path, "wav", sep = "."))


# Vowel Measures

