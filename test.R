
data_file <- '/home/michael-hungbo/Downloads/stack-overflow-developer-survey-2024/survey_results_public.csv'
dataset <- read.csv(data_file, header = TRUE, sep = ",", quote = "\"",
         dec = ".", fill = TRUE)
View(dataset)
