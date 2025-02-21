# global.R

library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(patchwork)

# Import data and join tables.
ors_raw <- read_excel("data/or_survey_sim_final_2023-11-29.xlsx")
pde_raw <- read_excel("data/pde_sim_final_2023-11-29.xlsx")
ors_pde_raw <- merge(ors_raw, pde_raw)

# Map text responses to ordinal data.
num_map <- c(
    "Strongly agree" = 5,
    "Somewhat agree" = 4,
    "Neither agree nor disagree" = 3,
    "Somewhat disagree" = 2,
    "Strongly disagree" = 1,
    "Very much like me" = 5,
    "Mostly like me" = 4,
    "Somewhat like me" = 3,
    "Not much like me" = 2,
    "Not at all like me" = 1
)

chr_map <- c(
    "Y" = "Yes",
    "N" = "No"
)

ors_num <- ors_raw
ors_num[-1] <- ors_num[-1] |>
    mutate(across(everything(), ~ recode(.x, !!!num_map)))
ors_pde_num <- merge(ors_num, pde_raw) |>
    mutate(across(FIRST_GENERATION_IND, ~ recode(.x, !!!chr_map)))

# Full statement of each survey question.
ors_qs <- c(
    "Q4_1" = "I feel I belong at Binghamton University.",
    "Q4_2" = "I am ready to meet the academic challenges of Binghamton University.",
    "Q11_1" = "Binghamton University is concerned with my personal health/wellness.",
    "Q11_2" = "Binghamton University has a number of services available to help me be healthy at college.",
    "Q15_1" = "I feel like I have the resources to make me a financially responsible college student.",
    "Q15_2" = "I feel connected to the Binghamton University community.",
    "Q15_3" = "Binghamton University is a place where I am able to perform up to my full potential.",
    "Q15_4" = "I have found one or more communities or groups where I feel I belong at Binghamton University.",
    "Q15_5" = "I am ready to make friends at Binghamton University.",
    "Grit_1" = "New ideas and projects sometimes distract me from previous ones.",
    "Grit_2" = "Setbacks don’t discourage me.",
    "Grit_3" = "I have been obsessed with a certain idea or project for a short time but later lost interest.",
    "Grit_4" = "I am a hard worker.",
    "Grit_5" = "I often set a goal but later choose to pursue a different one.",
    "Grit_6" = "I have difficulty maintaining my focus on projects that take more than a few months to complete.",
    "Grit_7" = "I finish whatever I begin.",
    "Grit_8" = "I am diligent."
)

ors_kws <- c(
    "Q4_1: Belonging" = "Q4_1",
    "Q4_2: Academic Preparedness" = "Q4_2",
    "Q11_1: Wellness Concern" = "Q11_1",
    "Q11_2: Wellness Services" = "Q11_2",
    "Q15_1: Financial Resources" = "Q15_1",
    "Q15_2: Community Connection" = "Q15_2",
    "Q15_3: Full Potential" = "Q15_3",
    "Q15_4: Community Belonging" = "Q15_4",
    "Q15_5: Friends" = "Q15_5",
    "Grit_1: Distraction" = "Grit_1",
    "Grit_2: Setbacks" = "Grit_2",
    "Grit_3: Losing Interest" = "Grit_3",
    "Grit_4: Hard Worker" = "Grit_4",
    "Grit_5: Changing Goals" = "Grit_5",
    "Grit_6: Maintaining Focus" = "Grit_6",
    "Grit_7: Finishing" = "Grit_7",
    "Grit_8: Diligence" = "Grit_8"
)

q_response_order <- c(
    "Strongly agree",
    "Somewhat agree",
    "Neither agree nor disagree",
    "Somewhat disagree",
    "Strongly disagree"
)

grit_response_order <- c(
    "Very much like me",
    "Mostly like me",
    "Somewhat like me",
    "Not much like me",
    "Not at all like me"
)

# Survey questions associated with each topic.
topic_qs <- list(
    "Sociality" = c("Q4_1", "Q15_2", "Q15_4", "Q15_5"),
    "Wellness" = c("Q11_1", "Q11_2"),
    "Success" = c("Q4_2", "Q15_1", "Q15_3"),
    "Grit" = c("Grit_1", "Grit_2", "Grit_3", "Grit_4", "Grit_5", "Grit_6", "Grit_7", "Grit_8")
)