# Acoustic Measurement Prep

# Authors: Micah E. Hirsch, M.S. & Austin R. Thompson, Ph.D.

# Purpose: To prepare acoustic variables of interest for the speakers for analysis.

library(rio) # install.packages("rio")
library(tidyverse) # install.packages("tidyverse")
library(rPraat) # install.packages("rPraat")
library(remotes) # install.packages("remotes")
library(PraatR) # remotes:::install_github("usagi5886/PraatR")
library(datadictionary) # install.packages("datadictionary")
library(janitor) # install.packages("janitor")

# Setting paths to import raw data and export cleaned data
raw_wd <- "~/Documents/Github/Sensor-Effects-Project/Raw_Speaker_Data/"

cleaned_wd <- "~/Documents/Github/Sensor-Effects-Project/Data_Analysis"

# Setting working directory
setwd(raw_wd)

# Loading in the data

## Loading in file information
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

## Creating file paths df
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

## Extracting speakers who have intra-reliability segmentations
intra_speakers <- c("Sub32", "Sub36", "Sub39", "Sub45", "Sub47", "Sub50", "Sub52", "Sub56")

intra <- textgrid_paths |>
  dplyr::filter(speaker_id %in% intra_speakers) |>
  dplyr::mutate(path = case_when(speaker_id == "Sub32" ~ str_replace(path, "\\.TextGrid$", "_MC_intra.TextGrid"),
                                 speaker_id == "Sub36" ~ str_replace(path, "\\.TextGrid$", "_MC_intra.TextGrid"),
                                 speaker_id == "Sub47" ~ str_replace(path, "\\.TextGrid$", "_MC_intra.TextGrid"),
                                 TRUE ~ str_replace(path, "\\.TextGrid$", "_MH_intra.TextGrid")))

## Extracting speakers who have inter-reliability segmentations
inter_speakers <- c("Sub28", "Sub36", "Sub38", "Sub39", "Sub41", "Sub49_2", 
                    "Sub50", "Sub53", "Sub59")

inter <- textgrid_paths |>
  dplyr::filter(speaker_id %in% inter_speakers) |>
  dplyr::mutate(path = case_when(speaker_id == "Sub38" ~ str_replace(path, "\\.TextGrid$", "_MC_inter.TextGrid"),
                                 speaker_id == "Sub39" ~ str_replace(path, "\\.TextGrid$", "_MC_inter.TextGrid"),
                                 speaker_id == "Sub41" ~ str_replace(path, "\\.TextGrid$", "_MC_inter.TextGrid"),
                                 speaker_id == "Sub50" ~ str_replace(path, "\\.TextGrid$", "_MC_inter.TextGrid"),
                                 speaker_id == "Sub59" ~ str_replace(path, "\\.TextGrid$", "_MC_inter.TextGrid"),
                                 TRUE ~ str_replace(path, "\\.TextGrid$", "_MH_inter.TextGrid")))

## Meging inter- and intra-rater reliability back with main textgrid df

textgrid_paths <- rbind(textgrid_paths, intra)
textgrid_paths <- rbind(textgrid_paths, inter)

# Removing unneeded items from environment
rm(inter, intra, inter_speakers, intra_speakers)

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
  

## Pulling out phrase segments
phrases <- Segments |>
  dplyr::filter(grepl("phrase", ignore.case = T, Segment)) |>
  dplyr::mutate(phrase = case_when(grepl(pattern = "Phrase1", x = Segment) ~ "Phrase 1",
                                   grepl(pattern = "Phrase2", x = Segment) ~ "Phrase 2",
                                   grepl(pattern = "Phrase3", x = Segment) ~ "Phrase 3"),
                syllables = sub(".*_", "", Segment),
                syllables = as.numeric(syllables))

## Pulling out phoneme segments
phonemes <- Segments |>
  dplyr::filter(!grepl(pattern = "phrase",
                       ignore.case = T,
                       x = Segment))

## Removing unneeded items from environment
rm(Segments, speakerFiles, textgrid_paths, speaker, k, file_path)

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


# Vowel Measures

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
                vowel = base::tolower(vowel))

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
    rPraat::snd.write(paste0(targetFile,"_target.wav"))
  
  # Creating the current target.Formant
  formantArg <- list(
    .00,
    5.0,
    ifelse(currentTarget$sex == "M", 5000, 5500),
    0.025,
    50
  )
  PraatR::praat( "To Formant (burg)...",
                 arguments = formantArg,
                 input = paste(raw_wd, targetFile,"_target.wav", sep = ""),
                 output = paste(raw_wd, targetFile,"_target.Formant", sep = ""),
                 filetype = "text",
                 overwrite = TRUE)
  
  formants <- rPraat::formant.read(
    fileNameFormant = paste0(targetFile,"_target.Formant"),
    encoding = "auto"
  )
  midFrame <- rPraat::formant.getPointIndexNearestTime(formants,
                                                       time = (formants$xmax/2) + formants$xmin)
  
  midpoint <- formants$frame[[midFrame]][["frequency"]] |>
    as.data.frame() |>
    dplyr::rename(Formants = 1)
  
  F1_mid <- midpoint[1,]
  F2_mid <- midpoint[2,]
  
  vowels <- vowels |>
    filter(Row == k) |>
    dplyr::mutate(
      F1 = as.numeric(F1_mid),
      F2 = as.numeric(F2_mid)) |>
    bind_rows(vowels |>
                filter(Row != k)) |>
    arrange(Row)
  
  # Cleaning up the folder
  file.remove(paste0(targetFile,"_target.wav"))
  file.remove(paste0(targetFile,"_target.Formant"))
  
  k <- k + 1
}

## Cleaning vowels df
vowels <- vowels |>
  dplyr::select(!c(Segment, path, label, Row)) |>
  janitor::clean_names() |>
  dplyr::mutate(time_point = factor(time_point, levels = c("before", "sensors", "after")),
                group = factor(group, levels = c("HC", "PD"), labels = c("Control", "PD")),
                sex = factor(sex, levels = c("M", "F"), labels = c("Male", "Female")),
                vowel = factor(vowel, levels = c("i", "u", "ae", "a")),
                seg_type = factor(seg_type, levels = c("initial", "intrarater", "interrater")),
                age = as.numeric(age))

## Removing unneeded items from the environment

rm(F1_mid, F2_mid, k, midFrame, targetFile, currentTarget, formantArg, formants, midpoint, sndWav)

# Consonant Analysis

## Subsetting Consonants from phoneme df
consonants <- phonemes |>
  dplyr::filter(Segment == "sh" | Segment == "s") |>
  dplyr::select(!path) |>
  dplyr::left_join(wav_paths, by = c("speaker_id", "timePoint")) |>
  dplyr::mutate(Row = row_number(),
                consonant = Segment)

## Calculating M1 and M2
k = 1
while (k <= nrow(consonants)) {
  
  # Indexing current target file path
  targetFile <- consonants |>
    dplyr::filter(row_number() == k) |>
    dplyr::select(path) |>
    as.character()
  
  # Selecting current target from consonant df
  currentTarget <- consonants |>
    slice(k)
  
  # Loading in the sound file
  sndWav <- rPraat::snd.read(paste0(targetFile, ".wav"))
  
  # Cutting the sound wav to just the target
  rPraat::snd.cut(sndWav,
                  Start = currentTarget$onset,
                  End = currentTarget$offset) |>
    rPraat::snd.write(paste0(targetFile,"_target.wav"))
  
  # Creating target spectrum
  PraatR::praat( "To Spectrum...",
                 arguments = list(TRUE),
                 input = paste(raw_wd, targetFile,"_target.wav", sep = ""),
                 output = paste(raw_wd, targetFile,"_target.Spectrum", sep = ""),
                 filetype="binary",
                 overwrite=TRUE)
  
  consonants <- consonants |>
    filter(Row == k) |>
    dplyr::mutate(
      M1 = PraatR::praat( "Get centre of gravity...",
                          arguments = list(1),
                          input = paste(raw_wd, targetFile,"_target.Spectrum", sep = "")) |>
        str_replace(" hertz", "") |>
        as.numeric() / 1000,
      M2 = PraatR::praat( "Get standard deviation...",
                          arguments = list(1),
                          input = paste(raw_wd, targetFile,"_target.Spectrum", sep = "")) |>
        str_replace(" hertz", "") |>
        as.numeric() / 1000,
      # Spectral moment power distribution
      M1_p = PraatR::praat( "Get centre of gravity...",
                            arguments = list(2),
                            input = paste(raw_wd, targetFile,"_target.Spectrum", sep = "")) |>
        str_replace(" hertz", "") |>
        as.numeric() / 1000,
      M2_p = PraatR::praat( "Get standard deviation...",
                            arguments = list(2),
                            input = paste(raw_wd, targetFile,"_target.Spectrum", sep = "")) |>
        str_replace(" hertz", "") |>
        as.numeric() / 1000) |>
    bind_rows(consonants |>
                filter(Row != k)) |>
    arrange(Row)
  
  # Cleaning up the folder
  file.remove(paste0(targetFile,"_target.wav"))
  file.remove(paste0(targetFile,"_target.Spectrum"))
  
  k <- k + 1
  
}

consonants <- consonants |>
  dplyr::select(!c(Segment, path, Row)) |>
  janitor::clean_names() |>
  dplyr::mutate(time_point = factor(time_point, levels = c("before", "sensors", "after")),
                group = factor(group, levels = c("HC", "PD"), labels = c("Control", "PD")),
                sex = factor(sex, levels = c("M", "F"), labels = c("Male", "Female")),
                consonant = factor(consonant, levels = c("s", "sh")),
                seg_type = factor(seg_type, levels = c("initial", "intrarater", "interrater")),
                age = as.numeric(age))

# Articulation Rate

artic_rate <- phrases |>
  dplyr::mutate(duration = offset - onset) |>
  dplyr::group_by(speaker_id, timePoint, phrase, seg_type) |>
  dplyr::summarize(artic_rate = sum(syllables)/sum(duration)) |>
  ungroup() |>
  janitor::clean_names() |>
  dplyr::left_join(speakers, by = "speaker_id") |>
  dplyr::mutate(time_point = factor(time_point, levels = c("before", "sensors", "after")),
                group = factor(group, levels = c("HC", "PD"), labels = c("Control", "PD")),
                sex = factor(sex, levels = c("M", "F"), labels = c("Male", "Female")),
                age = as.numeric(age),
                phrase = as.factor(phrase),
                seg_type = factor(seg_type, levels = c("initial", "intrarater", "interrater")))

## Removing unneeded items from environment
rm(currentTarget, phonemes, sndWav, speakers, wav_paths, k, targetFile)

# Exporting cleaned datasets

## setting new working directory

setwd(cleaned_wd)

## Vowels Export

rio::export(vowels, "vowel_measures.csv")

### Labels for vowels data dictionary
vowel_labels <- c(speaker_id = "Speaker ID",
                  onset = "Onset of Vowel Segment (s)",
                  offset = "Offset of Vowel Segment (s)",
                  time_point = "Time Point of Recording",
                  group = "Group",
                  sex = "Sex",
                  age = "Age",
                  seg_type = "Segment Type",
                  vowel = "Corner Vowel",
                  f1 = "Formant 1",
                  f2 = "Formant 2")

vowel_dict <- datadictionary::create_dictionary(vowels, var_labels = vowel_labels)

### Exporting vowel data dictionary
rio::export(vowel_dict, "vowel_measures_dictionary.csv")

## Consonants Export

rio::export(consonants, "consonant_measures.csv")

### Labels for consonant data dictionary
consonant_labels <- c(speaker_id = "Speaker ID",
                      onset = "Onset of Vowel Segment (s)",
                      offset = "Offset of Vowel Segment (s)",
                      time_point = "Time Point of Recording",
                      group = "Group",
                      sex = "Sex",
                      age = "Age",
                      seg_type = "Segment Type",
                      consonant = "Consonant",
                      m1 = "Spectral Moment M1",
                      m2 = "Spectral Moment M2",
                      m1_p = "M1 Power Distribution",
                      m2_p = "M2 Power Distribution")

consonant_dict <- datadictionary::create_dictionary(consonants, var_labels = consonant_labels)

rio::export(consonant_dict, "consonant_measures_dictionary.csv")

## Artic Rate Export

rio::export(artic_rate, "articulation_rate.csv")

### Labels for artic rate data dictionary

artic_rate_labels <- c(speaker_id = "Speaker ID",
                       time_point = "Time Point of Recording",
                       phrase = "Phrase from Caterpillar Passage",
                       seg_type = "Segment Type",
                       artic_rate = "Articulation Rate (syll/s)",
                       group = "Group",
                       sex = "Sex",
                       age = "Age")

artic_rate_dict <- datadictionary::create_dictionary(artic_rate, var_labels = artic_rate_labels)

rio::export(artic_rate_dict, "articulation_rate_dictionary.csv")

# clear environment
rm(list=ls())
