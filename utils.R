library(tidyverse)
library(tidytext)
library(MatchIt)
library(ggpubr)
library(gghighlight)

readGroundTruth <- function(path) {
  countryLevelDf <- read.csv(path)
  countryLevelDf$UserCount <- as.numeric(countryLevelDf$UserCount)
  
  # make the country name consistent with the individual survey dataframe
  countryLevelDf <- countryLevelDf %>% 
    filter(Country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Germany", "Greece", "India", "Japan", "Korea", "Mexico", "Nigeria", "Poland", "Saudi Arabia", "Singapore", "South Africa", "United States"))
  
  # remove unknown data and non-binary gender due to small n
  countryLevelDf <- countryLevelDf %>% filter(AgeGroup != "" & AgeGroup != "Unknown" & !is.na(AgeGroup)) %>%
    filter(Gender != "" & Gender != "Unknown" & !is.na(Gender) & Gender %in% c("Male", "Female")) 
  
  # Calculate the percentage of age+gender group by country
  countryLevelDf <- countryLevelDf %>% group_by(Country) %>%
    mutate(population=sum(UserCount, na.rm=TRUE)) %>%
    mutate(UserCount.per=UserCount/population) %>% select(-population)

  return(countryLevelDf)
}


readSurveyData = function(path) {
  # take the path of the survey data and return a dataframe
  #
  # Args:
  #   path: the path of the survey data
  #
  # Returns:
  #   df: the long dataframe of the survey data
  # Read the processed/cleaned dataset
  data <- read.csv("dataset.csv")

  # Pivot the game-oriented dataframe to a long question-oriented dataframe
  df <- pivotGameToQuestion(data)
  # Clean and rename the gender column
  df <- df %>% dplyr::filter(GENDER %in% c("Male", "Female", "Another gender (please specify)"))
  df$GENDER[df$GENDER == 'Another gender (please specify)'] <- 'NonBinary'
  df$gender <- df$GENDER
}

pivotGameToQuestion = function(gameDf) {
  # Pivot the game-oriented dataframe to a long question-oriented dataframe
  # 
  # Args:
  #   gameDf: the wide dataframe where each row is a game
  # 
  # Returns:
  #   questionDf: the long dataframe where each row is a question (within a game)
  
  questions <- colnames(gameDf)[28:59]  # control_complexity to creativity
  # print(questions)  # Check the output is a list of characters
  questionDf <- gameDf %>% 
    pivot_longer(all_of(questions), names_to="label", values_to="answer") %>%
    mutate(experience = case_when(PC.exp == 6 | Console.exp == 6 | Tablet.exp == 6 | Smartphone.exp == 6 ~ 6,
                                  PC.exp == 5 | Console.exp == 5 | Tablet.exp == 5 | Smartphone.exp == 5 ~ 5,
                                  PC.exp == 4 | Console.exp == 4 | Tablet.exp == 4 | Smartphone.exp == 4 ~ 4,
                                  PC.exp == 3 | Console.exp == 3 | Tablet.exp == 3 | Smartphone.exp == 3 ~ 3,
                                  PC.exp == 2 | Console.exp == 2 | Tablet.exp == 2 | Smartphone.exp == 2 ~ 2,
                                  PC.exp == 1 | Console.exp == 1 | Tablet.exp == 1 | Smartphone.exp == 1 ~ 1)) %>%  # Assign an experience level
    mutate(hardcore = ifelse(experience <= 4, "non-hardcore", "hardcore")) %>%
    mutate(region = case_when(country %in% c("Japan", "Korea") ~ "Confucian",
                              country %in% c("Singapore", "India") ~ "West.South.Asia",
                              country %in% c("Argentina", "Chile", "Colombia", "Mexico", "Brazil") ~ "Latin.America",
                              country %in% c("Germany") ~ "Protestant.Europe",
                              country %in% c("South.Africa", "Nigeria", "Saudi.Arabia") ~ "Islamic",
                              country %in% c("Poland") ~ "Catholic.Europe",
                              country %in% c("Greece") ~ "Orthodox.Europe",
                              country %in% c("US") ~ "US")) %>%  # Assign regions based on the World Value Survey
    mutate(native = ifelse((! country %in% c("US", "Singapore", "India", "Nigeria", "South.Africa")) & (language != "English"), "Native.speaker", "English.speaker") ) %>% # this means they use English versus their native language
    select(Response.ID, SessionID, game, country, GENDER, AGE, ageCut, native, Language, region, experience, hardcore, accessibility, labeler, ambassador, label, answer) 
  
  questionDf[questionDf == ""] <- NA
  questionDf$label <- factor(questionDf$label, levels=colnames(gameDf)[28:59])
  questionDf$ageCut <- factor(questionDf$ageCut)
  questionDf$country = factor(questionDf$country, levels=c("US", "Germany", "Poland", "Greece", "Japan", "Korea", "Singapore", "India", "Saudi.Arabia", "South.Africa", "Nigeria", "Brazil", "Argentina", "Colombia", "Chile", "Mexico"))
  questionDf$region = factor(questionDf$region, levels=c("US", "Confucian", "West.South.Asia", "Latin.America", "Protestant.Europe", "Islamic", "Catholic.Europe", "Orthodox.Europe"))
  questionDf$native = factor(questionDf$native)
  questionDf$accessibility = factor(questionDf$accessibility)
  questionDf$hardcore = factor(questionDf$hardcore)
  
  return(questionDf)
}

# Read countryLevelDf for poststratification
read.ground.truth <- function() {
    # Read the ground truth data from the Xbox user base data for poststratification
    #  
    # Args: None
    # 
    # Returns:
    #   countryLevelDf: a dataframe that include the countries included in the analysis 
    #   and their population as well as the proportion
    countryLevelDf <- read_csv("demographic.csv")
    countryLevelDf$UserCount <- as.numeric(countryLevelDf$UserCount)
  
    # Make the country name consistent with the individual survey dataframe
    # The country name, gender, and age values should be consistent with the survey data
    countryLevelDf <- countryLevelDf %>% 
        dplyr::filter(Country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Germany", "Greece", "India", "Japan", "Korea", "Mexico", "Nigeria", "Poland", "Saudi Arabia", "Singapore", "South Africa", "United States")) %>%
        dplyr::rename(ageCut = AgeGroup, country = Country, gender = Gender) %>%
        dplyr::mutate(ageCut = case_when(ageCut == "18 - 24" ~ "18 - 24",
                                ageCut == "25 - 34" ~ "25 - 34",
                                ageCut == "35 - 44" ~ "35 - 44",
                                ageCut == "45 - 55" ~ "45 - 55",
                                ageCut == "> 55" ~ "> 55"))
    countryLevelDf$country[countryLevelDf$country == "United States"] <- "US"
    countryLevelDf$country[countryLevelDf$country == "Saudi Arabia"] <- "Saudi.Arabia"
    countryLevelDf$country[countryLevelDf$country == "South Africa"] <- "South.Africa"
    countryLevelDf$country <- factor(countryLevelDf$country)
    countryLevelDf$ageCut <- factor(countryLevelDf$ageCut)
    countryLevelDf$gender <- factor(countryLevelDf$gender)
    countryLevelDf$region = factor(countryLevelDf$region)
    
    # Filter out the nonbinary data due to lack of data that would affect the analysis
    countryLevelDf <- countryLevelDf %>% 
        dplyr::filter(ageCut != "" & ageCut != "Unknown" & !is.na(ageCut)) %>%
        dplyr::filter(gender != "" & gender != "Unknown" & !is.na(gender) & gender %in% c("Male", "Female")) 
    
    # Calculate the percentage of age+gender group by country
    countryLevelDf <- countryLevelDf %>% group_by(country) %>%
        dplyr::mutate(population=sum(UserCount, na.rm=TRUE)) %>%
        dplyr::mutate(UserCount.per=UserCount/population) %>% 
        dplyr::select(-population)

    return(countryLevelDf)
}

read.hofstde <- function() {
    # Read the hofstede's cultural dimension data for pairwise correlation analysis
    # Find the pairwise correlation between countries based on the Hofstede's framework
    # correlation based on six metrics: power.distance, individualism, masculinity, uncertainty
    # long.term.orientation, and indulgence
    # Args: None
    #
    # Returns:
    #   hofstedeCorr: a dataframe that include the pairwise correlation between countries
    hofstedeDf <- read.csv("hofstede.csv") # Source: https://hi.hofstede-insights.com/national-culture
    hofstedeDf <- hofstedeDf %>% 
        dplyr::select(-X) %>%
        dplyr::mutate(power.distance = power.distance / 100,
                    individualism = individualism / 100,
                    masculinity = masculinity / 100,
                    uncertainty = uncertainty / 100, 
                    long.term.orientation = long.term.orientation / 100,
                    indulgence = indulgence / 100)  # scale the hofstede score to 0 - 1
    corrMatrix <- cor(t(hofstedeDf[, -1]))  # remove the first column with countries
  
    corrMatrix[upper.tri(corrMatrix, diag = TRUE)] <- NA
    rownames(corrMatrix) <- colnames(corrMatrix) <- hofstedeDf$country
    corrMatrix <- na.omit(reshape::melt(t(corrMatrix)))
    corrMatrix <- corrMatrix[ order(corrMatrix$X1, corrMatrix$X2), ]
    hofstedeCorr <- corrMatrix %>% select(X1, X2, value) %>% dplyr::rename(countryA=X1, countryB=X2, hofstede.corr=value)
    hofstedeCorr$countryA <- as.character(hofstedeCorr$countryA)
    hofstedeCorr$countryB <- as.character(hofstedeCorr$countryB)
    hofstedeCorr <- transform(hofstedeCorr, countryA = pmin(countryA, countryB), countryB=pmax(countryA, countryB))
    return(hofstedeCorr)
}



##########################################################################
######################   Helper Functions   ##############################
##########################################################################

find_mode <- function(x) {
  # Find the mode of a given vector (including multiple modes)
  # Reference: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
  return(u)
}


getAnnotator <- function(df, label) {
  # Get the annotator for a specific label
  # e.g. how do the Polish annotators label "control_complexity"
  annotator <- df %>% filter(labeler == "Yes" & label == {{ label }}) %>%
    count(game, answer) %>%
    group_by(game) %>%
    summarize(majority.vote = mean(answer[which(n==max(n))]))
  return(annotator)
}