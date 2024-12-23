# Splitting Phoneme Segments

# Author: Micah E. Hirsch, Ph.D. (they/them), mehirsch@bu.edu

# Purpose: We are recalculating our VSA measures. In order to do this, we need to prepare individual 
# phonemes as separate sound files to run through FastTrack in PRAAT. This script extracts the vowel
# segments from the existing TextGrids and splits them into their own individual sound files.

library(rio) # install.packages("rio")
library(tidyverse) # install.packages("tidyverse")
library(rPraat) # install.packages("rPraat")
library(remotes) # install.packages("remotes")
library(PraatR) # remotes:::install_github("usagi5886/PraatR")
library(datadictionary) # install.packages("datadictionary")
library(janitor) # install.packages("janitor")


# Setting working directory
setwd("~/Documents/Github/Sensor-Effects-Project/Raw_Speaker_Data/")

files <- rio::import("Sensor_Effects_Participants_Lab_Version.csv") |>
  dplyr::rename(speaker_id = sub_number) |>
  dplyr::mutate_all(~gsub("\\*", "", .)) |>
  dplyr::mutate(caterpillar_conversational = case_when(speaker_id == "Sub32" ~ "Sub32_1_001_sync",
                                                       TRUE ~ caterpillar_conversational),
                caterpillar_end_no_sensors = case_when(speaker_id == "Sub32" ~ "Sub32_1_013_sync",
                                                       TRUE ~ caterpillar_end_no_sensors))

## Extracting speaker info
speakers <- files |>
  dplyr::select(speaker_id:age)

textgrid_paths <- files |>
  dplyr::mutate(path = paste("Segments", speaker_id, sep = "/"),
                before = paste(path, caterpillar_no_sensors, sep = "/"),
                sensors = paste(path, caterpillar_conversational, sep = "/"),
                after = paste(path, caterpillar_end_no_sensors, sep = "/")) |>
  dplyr::select(speaker_id, before, sensors, after) |>
  tidyr::pivot_longer(cols = c(before:after),
                      names_to = "timePoint",
                      values_to = "path") |>
  dplyr::mutate(path = paste(path, "TextGrid", sep = "."))

## Loading Textgrids
loadData <- function(path, speaker) {
  
  # Extracting timepoint info
  path_tp_info <- path |>
    as.data.frame() |>
    dplyr::rename(path = 1) |>
    dplyr::left_join(textgrid_paths, by = "path")
  
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
  
  # Getting phoneme-level segments
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
    dplyr::mutate(timePoint = path_tp_info$timePoint,
                  path = path_tp_info$path)
  
  return(speakerSegments)
  rm(Phrase, Phoneme)
} 

k <- 1
while (k <= nrow(files)) {
  
  speaker <- speakers$speaker_id[k]
  file_path <- paste("Segments", speaker, sep = "/")
  
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
  # Fixing human coding errors
  dplyr::mutate(Segment = case_when(Segment == "Phrase_3.2_9" ~ "Phrase3.2_9",
                                    Segment == "Phase3.3_10" ~ "Phrase3.3_10",
                                    Segment == "Phrase_2.2_3" ~ "Phrase2.2_3",
                                    TRUE ~ Segment)) |>
  dplyr::left_join(speakers, by = "speaker_id") |>
  dplyr::mutate(seg_type = case_when(str_detect(path, "_inter") ~ "interrater",
                                     str_detect(path, "_intra") ~ "intrarater",
                                     TRUE ~ "initial"))

## Pulling out phoneme segments
phonemes <- Segments |>
  dplyr::filter(!grepl(pattern = "phrase",
                       ignore.case = T,
                       x = Segment))

## Loading in wav file paths
wav_paths <- files |>
  dplyr::mutate(path = paste("Recordings", speaker_id, sep = "/"),
                before = paste(path, caterpillar_no_sensors, sep = "/"),
                sensors = paste(path, caterpillar_conversational, sep = "/"),
                after = paste(path, caterpillar_end_no_sensors, sep = "/")) |>
  dplyr::select(speaker_id, before, sensors, after) |>
  tidyr::pivot_longer(cols = c(before:after),
                      names_to = "timePoint",
                      values_to = "path")

## Filtering out vowels from phoneme segments
vowels <- phonemes |>
  dplyr::filter(Segment != "sh") |>
  dplyr::filter(Segment != "s") |>
  dplyr::select(!path) |>
  dplyr::left_join(wav_paths, by = c("speaker_id", "timePoint")) |>
  dplyr::mutate(Row = dplyr::row_number(),
                label = Segment,
                vowel = gsub(
                  pattern = "l",
                  replacement = "",
                  x = label),
                vowel = base::tolower(vowel)) |>
  dplyr::filter(!is.na(path))

vowels <- vowels |>
  dplyr::group_by(path, vowel) |> # Group by recording and vowel
  dplyr::mutate(position = row_number()) |> # Create position label
  dplyr::ungroup()

## Extracting F1 and F2 from temporal midpoint
k <- 1
while (k <= nrow(vowels)) {
  
  targetFile <- vowels |>
    dplyr::filter(row_number() == k) |>
    dplyr::select(path) |>
    as.character()
  
  currentTarget <- vowels |>
    dplyr::mutate(onset = onset,
                  offset = offset) |>
    slice(k)
  
  # Loading in the sound wav
  sndWav <- rPraat::snd.read(paste0(targetFile, ".wav"))
  
  # Cutting the sound wav to be just the target
  rPraat::snd.cut(sndWav,
                  Start = currentTarget$onset,
                  End = currentTarget$offset) |>
    rPraat::snd.write(paste0(targetFile, "_", currentTarget$vowel,"_", 
                             currentTarget$position, ".wav"))
  
  k <- k + 1 
}
