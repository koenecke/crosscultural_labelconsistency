# Fig. 5 An evaluation of F1 scores across 14 countries from logistic regression models
# Find the output: ./figures/Figure5.pdf
# Import library and set theme
library(tidyverse)
library(caret)
library(MLmetrics)
library(pROC)
library(rpart)
# source("./utils.R")

REMOVED_GAME <- ""

df <- read.csv("longData.csv")
inconsistentLabels <- read.csv("./data/inconsistent_labels.csv") %>% 
                            dplyr::rename("Game"="game", "Label"="label")
consistentLabels <- read.csv("./data/consistent_labels.csv") %>% 
                            dplyr::rename("Game"="game", "Label"="label")

inconsistent <- inconsistentLabels %>% left_join(df, by=c("Game", "Label")) 
consistent <- consistentLabels %>% left_join(df, by=c("Game", "Label")) 

print("########## Countries: ##########")
print(unique(inconsistent$Country))

# split consistent and inconsistent labels into train and test sets 
# stratified sampling due to imbalanced classes among different countries
split = function(dataset) {
    # remove countries and game with too few samples
    dataset <- dataset %>% dplyr::filter(!Game %in% c(REMOVED_GAME), !Country %in% c("Saudi Arabia", "India")) 
    dataset$Country <- factor(dataset$Country)
    dataset$Game <- factor(dataset$Game)
    dataset$Label <- factor(dataset$Label)
    dataset$Value <- factor(dataset$Value)
    dataset$Gender <- factor(dataset$Gender)
    dataset$AgeGroup <- factor(dataset$AgeGroup)

    dataset$id <- 1:nrow(dataset)
    train <- dataset %>% dplyr::sample_frac(0.70)
    test <- dplyr::anti_join(dataset, train, by="id")
    return(list(train, test))
}

# split the data into train and test sets in the US versus non-US
set.seed(123) 
inconsistentSplit <- split(inconsistent)
consistentSplit <- split(consistent)
trainInconsistent <- inconsistentSplit[[1]]
testInconsistent <- inconsistentSplit[[2]]
trainConsistent <- consistentSplit[[1]]
testConsistent <- consistentSplit[[2]]
trainInconsistentUS <- trainInconsistent %>% dplyr::filter(Country == "United States")
trainInconsistentnonUS <- trainInconsistent %>% dplyr::filter(Country != "United States")
trainConsistentUS <- trainConsistent %>% dplyr::filter(Country == "United States")
trainConsistentnonUS <- trainConsistent %>% dplyr::filter(Country != "United States")
print(nrow(trainInconsistentUS))
print(nrow(trainInconsistentnonUS))
print(nrow(trainConsistentUS))
print(nrow(trainConsistentnonUS))


# train the model for consistent and inconsistent labels in the US and non-US
print("########## Model Training ##########")
logit.US.consistent <- glm(Value ~ Game + Label + AgeGroup + Gender, data=trainConsistentUS, family="binomial")
logit.US.inconsistent <- glm(Value ~ Game + Label + AgeGroup + Gender, data=trainInconsistentUS, family="binomial")
logit.nonUS.consistent <- glm(Value ~ Game + Label + AgeGroup + Gender, data=trainConsistentnonUS, family="binomial")
logit.nonUS.inconsistent <- glm(Value ~ Game + Label + AgeGroup + Gender, data=trainInconsistentnonUS, family="binomial")
countries <- unique(trainInconsistent$Country)
resDf <- data.frame(country=c(), Homogeneous=c(), Heterogeneous=c(), label=c())
# for each country, predict the test set and calculate the F1 score
# use the responses in that country as the test set
# use the model trained with inconsistent and consistent labels in the US and non-US to predict
for(country in countries) {
    # consistent models
    currTest <- testConsistent %>% dplyr::filter(Country == {{country}})
    currTest$estimate.US <- logit.US.consistent %>% predict(newdata = currTest, type="response")
    currTest$estimate.global <- logit.nonUS.consistent %>% predict(newdata = currTest, type="response")
    currTest$US.pred <- factor(ifelse(currTest$estimate.US > 0.5, 1, 0))
    currTest$global.pred <- factor(ifelse(currTest$estimate.global > 0.5, 1, 0))

    resDf <- rbind(
        data.frame(country=country,
                   Homogeneous = F1_Score(y_true=currTest$Value, y_pred=currTest$US.pred),
                   Heterogeneous = F1_Score(y_true=currTest$Value, y_pred=currTest$global.pred),
                   label="Consistently Conceptualized"),
        resDf
    )

    # inconsistent models
    currTest <- testInconsistent %>% dplyr::filter(Country == {{country}})
    currTest$estimate.US <- predict(logit.US.inconsistent, newdata = currTest, type="response")
    currTest$estimate.global <- predict(logit.nonUS.inconsistent, newdata = currTest, type="response")
    currTest$US.pred <- factor(ifelse(currTest$estimate.US > 0.5, 1, 0))
    currTest$global.pred <- factor(ifelse(currTest$estimate.global > 0.5, 1, 0))

    resDf <- rbind(
        data.frame(country=country,
                   Homogeneous = F1_Score(y_true=currTest$Value, y_pred=currTest$US.pred),
                   Heterogeneous = F1_Score(y_true=currTest$Value, y_pred=currTest$global.pred),
                   label="Inconsistently Conceptualized"),
        resDf
    )
}

countryOrder <- resDf %>%
                    pivot_longer(c(Homogeneous, Heterogeneous), names_to = "condition", values_to = "F1") %>%
                    dplyr::filter(condition == "Heterogeneous" & label == "Consistently Conceptualized") %>% 
                    arrange(F1) 

g <- resDf %>% 
        pivot_longer(c(Homogeneous, Heterogeneous), names_to = "condition", values_to = "F1") %>% 
        ggplot(aes(x=factor(country, levels=countryOrder$country), y=F1, shape=label, group=interaction(condition, label), color=condition)) +
        geom_point(size=3) +
        geom_line(aes(linetype=label)) +
        ylim(c(0,1)) +
        labs(x="", color="Training Data", linetype="Label Conceptualization", shape="Label Conceptualization") +
        theme_bw() +
        theme(legend.position = "bottom",
                legend.text=element_text(size=11),
                legend.title=element_text(size=12),
                legend.margin = margin(),
                legend.spacing = unit(0.04, "cm"),
                legend.box = 'vertical', 
                axis.title.x=element_blank(),
                axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1, size=13),
                axis.title.y=element_text(size=14),
                plot.title = element_text(size=18),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
ggsave("./figures/Figure5.pdf", g, width=10, height=6)


##########################################################################
#############################   Figure 13   ##############################
##########################################################################
# A similar plot compared to Figure 5, but with the stratified F1 score as sanity check
statifiedSplit = function(dataset) {
    # Sample an equal number of observations from each category
    # This is to make sure that the F1 score is not dominated by the majority class
    censusData <- readGroundTruth("./data/demographic.csv")
    freq <- table(dataset$Country)
    sampleSize <- min(freq)
    categories <- unique(dataset$Country)
    results <- data.frame(matrix(ncol=length(colnames(dataset)), nrow=0))
    for(cat in names(freq)) {
        results <- rbind(results, 
                         dataset %>% filter(Country == {{ cat }}) %>%
                             sample_n(sampleSize))
    }

    dataset <- results %>% 
                left_join(censusData, by=c("Country", "Gender", "AgeGroup")) %>% 
                dplyr::mutate(samplesize=UserCount.per * 1000) %>%
                group_by(Country, Gender, AgeGroup) %>%
                sample_n(min(samplesize, n()), replace=FALSE) %>% 
                ungroup() 

    dataset$id <- 1:nrow(dataset)
    train <- dataset %>% dplyr::sample_frac(0.70)
    test  <- dplyr::anti_join(dataset, train, by = 'id')
    return(list(train=train, test=test))
}

repInconsistentSplit <- statifiedSplit(inconsistent)
repConsistentSplit <- statifiedSplit(consistent)
repTrainConsistent <- repConsistentSplit$train
repTestConsistent <- repConsistentSplit$test
repTrainInconsistent <- repInconsistentSplit$train
repTestInconsistent <- repInconsistentSplit$test
repTrainInconsistentUS <- repTrainInconsistent %>% dplyr::filter(Country == "United States")
repTrainInconsistentnonUS <- repTrainInconsistent %>% dplyr::filter(Country != "United States")
repTrainConsistentUS <- repTrainConsistent %>% dplyr::filter(Country == "United States")
repTrainConsistentnonUS <- repTrainConsistent %>% dplyr::filter(Country != "United States")

print("########## Stratified Model Training ##########")
logit.US.consistent <- glm(Value ~ Game + Label + AgeGroup + Gender, data=repTrainConsistentUS, family="binomial")
logit.US.inconsistent <- glm(Value ~ Game + Label + AgeGroup + Gender, data=repTrainInconsistentUS, family="binomial")
logit.nonUS.consistent <- glm(Value ~ Game + Label + AgeGroup + Gender, data=repTrainConsistentnonUS, family="binomial")
logit.nonUS.inconsistent <- glm(Value ~ Game + Label + AgeGroup + Gender, data=repTrainInconsistentnonUS, family="binomial")
countries <- unique(trainInconsistent$Country)
resDf <- data.frame(country=c(), Homogeneous=c(), Heterogeneous=c(), label=c())
# for each country, predict the test set and calculate the F1 score
# use the responses in that country as the test set
# use the model trained with inconsistent and consistent labels in the US and non-US to predict
for(country in countries) {
    # consistent models
    currTest <- testConsistent %>% dplyr::filter(Country == {{country}})
    currTest$estimate.US <- logit.US.consistent %>% predict(newdata = currTest, type="response")
    currTest$estimate.global <- logit.nonUS.consistent %>% predict(newdata = currTest, type="response")
    currTest$US.pred <- factor(ifelse(currTest$estimate.US > 0.5, 1, 0))
    currTest$global.pred <- factor(ifelse(currTest$estimate.global > 0.5, 1, 0))

    resDf <- rbind(
        data.frame(country=country,
                   Homogeneous = F1_Score(y_true=currTest$Value, y_pred=currTest$US.pred),
                   Heterogeneous = F1_Score(y_true=currTest$Value, y_pred=currTest$global.pred),
                   label="Consistently Conceptualized"),
        resDf
    )

    # inconsistent models
    currTest <- testInconsistent %>% dplyr::filter(Country == {{country}})
    currTest$estimate.US <- predict(logit.US.inconsistent, newdata = currTest, type="response")
    currTest$estimate.global <- predict(logit.nonUS.inconsistent, newdata = currTest, type="response")
    currTest$US.pred <- factor(ifelse(currTest$estimate.US > 0.5, 1, 0))
    currTest$global.pred <- factor(ifelse(currTest$estimate.global > 0.5, 1, 0))

    resDf <- rbind(
        data.frame(country=country,
                   Homogeneous = F1_Score(y_true=currTest$Value, y_pred=currTest$US.pred),
                   Heterogeneous = F1_Score(y_true=currTest$Value, y_pred=currTest$global.pred),
                   label="Inconsistently Conceptualized"),
        resDf
    )
}

countryOrder <- c("Mexico", "Nigeria", "Brazil", "United States", "Singapore", "Colombia", "Chile",
                    "South Africa", "Poland", "Japan", "Argentina", "Germany", "Korea", "Greece") 

g <- resDf %>% 
        pivot_longer(c(Homogeneous, Heterogeneous), names_to = "condition", values_to = "F1") %>% 
        ggplot(aes(x=factor(country, levels=countryOrder), y=F1, shape=label, group=interaction(condition, label), color=condition)) +
        geom_point(size=3) +
        geom_line(aes(linetype=label)) +
        ylim(c(0,1)) +
        labs(x="", color="Training Data", linetype="Label Conceptualization", shape="Label Conceptualization") +
        theme_bw() +
        theme(legend.position = "bottom",
                legend.text=element_text(size=11),
                legend.title=element_text(size=12),
                legend.margin = margin(),
                legend.spacing = unit(0.04, "cm"),
                legend.box = 'vertical', 
                axis.title.x=element_blank(),
                axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1, size=13),
                axis.title.y=element_text(size=14),
                plot.title = element_text(size=18),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
ggsave("./figures/Figure13.pdf", g, width=10, height=6)

print("########## Done ##########")