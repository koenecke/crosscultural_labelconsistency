library("optparse")
library("tidyverse")
source("utils.R")

readLongData = function(path, outPath) {
  dataset <- read.csv(path) %>% # remove unnecessary columns
    dplyr::select(-X, -V1, -Time.Started, -Date.Submitted, -User.Agent, -Tags, -Legacy.Comments, -Comments, -Referer)
  
  # get the column names of the questions
  namesBetween <- function(col1, col2, data) {
    names(data)[which(names(data) == col1): which(names(data) == col2)]
  }

  # get relevant columns
  questionCols <- namesBetween("control_complexity", "creativity", dataset)
  questionCols <- questionCols[!questionCols %in% c("NA.positive.opinion", "NA.negative.opinion", "NA.art", "NA.feeling")]

  # pivot long
  output <- dataset %>% 
    tidyr::pivot_longer(all_of(questionCols), names_to="Label", values_to="Value") %>%
    dplyr::filter(CONF_AGREE == 1 & AGE_YN == 1 & Status == "Complete") %>%
    dplyr::filter(GENDER %in% c("Male", "Female")) %>%
    dplyr::rename("AgeGroup" = "ageCut") %>%
    dplyr::rename("Gender" = "GENDER") %>%
    dplyr::rename("Country" = "country") %>%
    dplyr::rename("Game" = "game") %>%
    dplyr::select(-Other..please.specify..accessibility, -NA.positive.opinion, -NA.negative.opinion, -NA.feeling, -NA.art, -None.of.these.apply.motivation, -game_id, -language, -CONF_AGREE, -AGE_YN,-AGE) %>%
    dplyr::mutate(Value = ifelse(Label %in% c("control_complexity", "learning_curve", "replayability") & Value %in% c(0, 1, 2), 0, as.integer(Value))) %>% 
    dplyr::mutate(Value = ifelse(Label %in% c("control_complexity", "learning_curve", "replayability") & Value == 3, 1, as.integer(Value))) %>%
    dplyr::mutate(Value = ifelse(Label %in% c("difficulty") & Value %in% c(0,1,2), 0, as.integer(Value))) %>%
    dplyr::mutate(Value = ifelse(Label %in% c("difficulty") & Value %in% c(3, 4), 1, as.integer(Value))) 

  # change naming
  output <- output %>% 
    dplyr::mutate(AgeGroup = case_when(
                                AgeGroup == "18-24" ~ "18 - 24",
                                AgeGroup == "25-34" ~ "25 - 34",
                                AgeGroup == "35-44" ~ "35 - 44",
                                AgeGroup == "45-55" ~ "45 - 55",
                                AgeGroup == "55+" ~ "> 55"))

  # change naming to make demographics.csv and survey consistent
  output$Country[output$Country == "US"] <- "United States"
  output$Country[output$Country == "Saudi.Arabia"] <- "Saudi Arabia"
  output$Country[output$Country == "South.Africa"] <- "South Africa"

  # rename label for clarity
  new.output <- output %>% mutate(Label = case_when(Label == "pacifist" ~ "is pacifist.", 
                     Label == "made.for.kids" ~ "is made for kids.",
                     Label == "cozy" ~ "is cozy.",
                     Label == "zen" ~ "is zen.",
                     Label == "fantasy" ~ "is a fantasy game.",
                     Label == "space" ~ "is located in space.",
                     Label == "heroic" ~ "is centralized around the character being a hero or savior of the world.",
                     Label == "real.world" ~ "is designed to simulate real-world activites.",
                     Label == "violent" ~ "is violent.",
                     Label == "action" ~ "is an action game.",
                     Label == "emotional" ~ "makes me feel strong empathy for the story or character experience.",
                     Label == "comedic" ~ "makes me feel amused or laugh.",
                     Label == "experimental" ~ "makes me feel that I've never played a game like this before.",
                     Label == "strategy" ~ "makes me feel that I must strategize and make decisions to win.",
                     Label == "grinding" ~ "makes me feel that I need to do grinding or repetitive work.",
                     Label == "anime" ~ "has an anime art style.",
                    Label == "hand.drawn" ~ "has an art style that invokes the memory of drawings on paper.",
                    Label == "stylized" ~ "has a unique art style that influences the feeling of the game.",
                    Label == "action.motivation" ~ "is great for the fast-paced action, surprises, thrills, and adrenaline.",
                    Label == "social"~ "is great for socializing and interacting with other players.",
                    Label == "mastery" ~ "is great for mastering the skills and techniques in the game.",
                    Label == "achievement" ~ "is great for completing all the missions, increasing all the stats, or getting a more powerful character or equipment.",
                    Label == "immersion" ~ "is great for the desire to get lost in the world of the game.",
                    Label == "creativity" ~ "is great for  designing your own character, house, clothes, or the world.",
                    Label == "learning_curve" ~ "high_learning_curve",
                    Label == "control_complexity" ~ "high_control_complexity",
                    Label == "replayability" ~ "high_replayability",
                    Label == "difficulty" ~ "high_difficulty", TRUE ~ "bug"
                  ))


  write.csv(new.output, "longDataset.csv")
}


##########################################################################
###################### Parse Command Lines  ##############################
##########################################################################

# Parse Command line input
parser <- OptionParser()
parser <- add_option(parser, c("--path"), action="store", default=NA, type='character', help="Path to the survey csv file")
parser <- add_option(parser, c("--output"), action="store", default=NA, type='character', help="Path to the output csv file")
opt <- parse_args(parser)
readLongData(opt$path, opt$output)