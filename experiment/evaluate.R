# evaluate.R

# Script to measure performance metrics using AI queries.

# Set working directory
setwd(paste0(rstudioapi::getActiveProject(), "/report_v5"))

# If it doesn't already exist, create this folder for containing reports
if(!file.exists("experiment/reports")){ dir.create("experiment/reports") }

library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(stringr, warn.conflicts = FALSE, quietly = TRUE)
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(jsonlite, warn.conflicts = FALSE, quietly = TRUE)
library(httr, warn.conflicts = FALSE, quietly = TRUE)


source("functions.R")
readRenviron(".env")

# For any report-key `id`, this function will find the relevant report,
# load its data in, construct the validation query, and log the results
get_question = function(id, folder = "experiment/reports"){
  # Testing value
  # id = "00036033901"
  report_id = stringr::str_sub(id, start = 1, end = 9)
  key_id = as.integer(str_sub(id, start = -2, end = -1))
  # Construct the path
  path = paste0(folder, "/", report_id, ".rds")
  # Read in the data
  # Narrow into just the key of interest (they are listed in order)
  data = path %>% read_rds() %>% .[[key_id]]

  message = data %>%
    with(
      list(
        Instructions = list(
          format = paste0('Return a JSON of answers for each question on accuracy, formality, etc. ',
                          'Format like: {"accurate": TRUE/FALSE , "accuracy": 1~5, "formality": 1~5, ..., "details": "0~50 word explanation."}'),
          accurate = paste0(
            'Verify that no part of the paragraph in [Report] misinterprets the [Data] supplied. Return TRUE if no misinterpretation. FALSE if any problems.'
          ),
          accuracy = paste0(
            'Rank paragraph in [Report] on a 5 point Likert scale, where 1 = many problems interpreting [Data] vs. 5 = no misinterpretation of [Data].'
          ),
          formality = paste0(
            'Rank paragraph in [Report] on a 5 point Likert scale, where 1 = casual writing vs. 5 = government report writing.'
          ),
          faithfulness = paste0(
            'Rank paragraph in [Report] on a 5 point Likert scale, where 1 = makes grandiose claims not supported by the data vs. 5 = makes claims directly related to the data.'
          ),
          clarity = paste0(
            'Rank paragraph in [Report] on a 5 point Likert scale, where 1 = confusing writing style vs. 5 = clear and precise.'
          ),
          succinctness = paste0(
            'Rank paragraph in [Report] on a 5 point Likert scale, where 1 = unncessarily wordy vs. 5 = succinct.'
          ),
          relevance = paste0(
            'Rank paragraph in [Report] on a 5 point Likert scale, where 1 = irrelevant commentary vs. 5 = relevant commentary about [Data].'
          )

        ),
        Data = request,
        Report = text
      )
    ) %>%
    jsonlite::toJSON(pretty = FALSE) %>%
    as.character() %>%
    tibble(role = "user", content = .)

  return(message)
}
# get_question(id)$content %>% toJSON(pretty = TRUE)

get_answers = function(report_id, id, question, model = "gpt-3.5-turbo"){
  # Query the AI about that section of the report.
  response = chat_create_any(messages = question, model = model)

  # Set a default answer
  answers = tibble(report_id = report_id, id = id, success = FALSE,
                   accurate = as.logical(NA_real_),
                   accuracy = as.integer(NA_integer_),
                   formality = as.integer(NA_integer_),
                   faithfulness = as.integer(NA_integer_),
                   clarity = as.integer(NA_integer_),
                   succinctness = as.integer(NA_integer_),
                   relevance = as.integer(NA_integer_),
                   details = NA_character_)

  if("choices" %in% names(response)){
    if("message" %in% names(response$choices)){
      if("content" %in% names(response$choices$message)){
        answers = response$choices$message$content %>% jsonlite::fromJSON()
        answers = as_tibble(answers)
        answers = answers %>% mutate(report_id = report_id, id = id, success = TRUE)
        # Order the columns
        answers = answers %>% select(report_id, id, success, accurate, accuracy, formality, faithfulness, clarity, succinctness, relevance, details)
      }
    }
  }
  return(answers)
}



loop_answers = function(done, file_records = "experiment/answers.csv", model = "gpt-4o") {

  n_i = nrow(done)
  for(i in 1:n_i){
    #i =1
    report_id = done$report_id[i]

    n_j = nrow(done[i,]$data[[1]])
    for(j in 1:n_j){
      # j = 1
      # Get the id for that section of the report
      id = done[i,]$data[[1]]$id[j]
      # Format the question for that section of the report
      question = get_question(id = id, folder = "experiment/reports")
      # Query OpenAI and get back an answer
      answers = get_answers(report_id = report_id, id = id, question = question)
      # Log the result
      readr::write_csv(x = answers, file = file_records, append = TRUE)
      # Print completion message
      cat("\n", i, "/", n_i,": ", report_id, "-", j, "--------------\n")
    }

  }
}


# If the file doesn't exist yet...
if(!file.exists("experiment/answers.csv")){
  # Create the template for the answers
  tibble(
    report_id = NA_character_,
    id = NA_character_,
    success = as.logical(NA_real_),
    accurate = as.logical(NA_real_),
    accuracy = as.integer(NA_integer_),
    formality = as.integer(NA_integer_),
    faithfulness = as.integer(NA_integer_),
    clarity = as.integer(NA_integer_),
    succinctness = as.integer(NA_integer_),
    relevance = as.integer(NA_integer_),
    details = as.character(NA_character_)
  ) %>%
    slice(0) %>%
    write_csv("experiment/answers.csv")
}

# Get ids of completed reports (as in, .docx files have been made)
completed_reports = str_remove(dir("experiment/reports", pattern = ".rds"), "[.]rds")

# Get ids of evaluated reports (from answers.csv spreadsheet)
evaluated_sections = read_csv("experiment/answers.csv")$id

# Get the grid remaining for processing
done = readRDS("experiment/grid.rds") %>%
  # Get just the reports completed
  filter(report_id %in% completed_reports) %>%
  # Get just the sections NOT YET evaluated
  filter(!id %in% evaluated_sections) %>%
  # Now format them into a nested dataframe
  select(report_id, geoid, year, pollutant, id, key) %>%
  group_by(report_id) %>%
  tidyr::nest(data = c(id, key) )


# Run the experiment!
loop_answers(done = done, file_records = "experiment/answers.csv", model = "gpt-4o")

# Cleanup
rm(list = ls())


