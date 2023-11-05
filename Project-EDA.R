title: "Final Project-Maribel Viveros-EDA"
output:
  word_document: default
html_document: default
pdf_document: default
date: "2023-10-31"

suppressPackageStartupMessages({
  suppressWarnings(library(tidyverse))   # for working with the data
  suppressWarnings(library(lubridate))   # for working with datetime data
  suppressWarnings(library(skimr))       # generate a text-based overview of the data
  suppressWarnings(library(visdat))      # generate plots visualizing data types and missingness
  suppressWarnings(library(plotly))      # generate interactive plots
  suppressWarnings(library(readxl))
  suppressWarnings(library(ggplot2))     # needed for the plot of eye metrics
  suppressWarnings(library(dplyr))
  suppressWarnings(library(tibble))
})

# Formulate your question (Item 1-Research Question)

## By analyzing gaze data from 24 participants engaged in eight desktop activities, including time to first fixation (attention) and first fixation duration (initial object impression), this research will enrich our understanding of visual perception. 