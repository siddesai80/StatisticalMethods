# Modelling EEG signals using polynomial regression

This repository contains code for Modelling EEG signals using polynomial regression.

This project was created as part of the coursework for module 7089CEM Introduction to Statistical Methods for Data Science for [Coventry University](https://www.coventry.ac.uk/course-structure/pg/eec/data-science-and-computational-intelligence-msc/). Any use of the code needs to be cited appropriately. 

## Usage

The code can be executed in R Studio. 

Make sure you download the dataset files from the [DataFiles](https://github.com/siddesai80/StatisticalMethods/tree/main/DataFiles) folder and keep them in the same path as the [R file](https://github.com/siddesai80/StatisticalMethods/blob/main/RScript_for_stats.R) or change the path of the files if you are keeping them in a different location. 

By default, the code will look for the files below path:
```
# Reading the CSV files from Google Drive
X = read.csv("X.csv")
y = read.csv("y.csv")
time = read.csv("time.csv")
```

## Purpose

The assignment aims to select the best regression model (from a candidate set of nonlinear regression models) that can well describe the evoked brain responses during guided meditation. The data are assumed to be collected during a neuroscience experiment, in which a participant is asked to do a guided meditation with the instructions being delivered through a set of speakers. The researchers are interested in the modulation of neural activity of two distinct brain regions during the mediation. Specifically, the neural activity is measured with electroencephalography (EEG) from the right auditory cortex and the prefrontal cortex. Area is dedicated to the processing of auditory sensations, whereas is related to executive function, planning and consciousness. The researchers hypothesise that the auditory cortex is linearly related to the audio signal (i.e. the voice of the mediation guide), while a nonlinear relationship is predicted in the prefrontal cortex. You are asked to investigate these relationships with the use of nonlinear regression modelling. 

## Data

The ‘simulated’ EEG time-series data and the sound signal are provided in the two separate Excel files. The X.csv file contains the EEG signals 𝐱𝟏 and 𝐱𝟐 that were measured from the prefrontal and auditory cortices respectively, and the y.csv file contains the sound signal 𝐲 (i.e. the voice of the mediation guide). The file time.csv contains the sampling time of all three signals in seconds. There are 2 minutes of signal in total collected with a sampling frequency of 20 Hz. All signals are subject to additive noise (assuming independent and identically distributed (“i.i.d”) Gaussian with zero-mean) with unknown variance due to distortions during recording. 

---

## Research Paper

The paper presented as part of this experiment with all the results and findings can be found in the [Research_Paper](https://github.com/siddesai80/StatisticalMethods/tree/main/Research_Paper) folder.
