# Fig. 11-12. A heatmap of the standard deviations across gender and age of post-stratified label annotation estimates for each game (anonymized) and label
# Reference: Juan Lopez-Martin, Justin H. Phillips, and Andrew Gelman, https://bookdown.org/jl5522/MRP-case-studies/ 
# This code will take a long time to run because MCMC is a computationally intensive algorithm
# This script is largely the same as Figure10_Poststratified_CI.R, but it post-stratifies by age group instead of country
# Find the output: ./figures/Figure11.pdf, ./figures/Figure12.pdf
library(tidyverse)
library(rstanarm)
source("./utils.R")

REMOVED_GAME <- ""
DATA_FILE <- ""
DEMOGRAPHIC_FILE <- ""

df <- read.csv(DATA_FILE) %>%
        dplyr::filter(!Country %in% c("India", "Saudi Arabia"), Game != {{ REMOVED_GAME }}) 
groundTruth <- readGroundTruth(path=DEMOGRAPHIC_FILE) %>% 
        dplyr::filter(!Country %in% c("India", "Saudi Arabia")) 


LABELS <- unique(df$Label)
GAMES <- unique(df$Game)

# This is the placeholder final data frame where we store:
resAge <- data.frame(AgeGroup = c(), 
                    AgeGroup_Num = c(), 
                    model_age_sd=c(), 
                    model_age_pref=c(), 
                    game=c(), 
                    label=c())

resGender <- data.frame(Gender = c(), 
                    Gender_Num = c(), 
                    model_gender_sd=c(), 
                    model_gender_pref=c(), 
                    game=c(), 
                    label=c())

for(GAME in GAMES) {
    for(LABEL in LABELS) {
        print(paste0("Game: ", GAME, " Label: ", LABEL))
        
        curr <- df %>% dplyr::filter(Game == {{ GAME }} & Label == {{ LABEL }})

        # fit a bayesian model using stan_glmer
        fit <- stan_glmer(Value ~ Gender + (1 | Country) + (1|AgeGroup),
                    family = binomial(link = "logit"),
                    data = curr,
                    prior = normal(0, 1, autoscale = TRUE),
                    prior_covariance = decov(scale = 0.50),
                    adapt_delta = 0.99,
                    refresh = 0,
                    seed = 1010)
        # post-stratify 
        ageDf <- data.frame(
                    AgeGroup = rev(unique(curr$AgeGroup)),
                    AgeGroup_Num = 1:5,
                    model_age_sd = rep(-1, 5),
                    model_age_pref = rep(-1, 5))    
        # join the ageDf dataframe with the dataframe dataframe
        joinDf <- ageDf %>% 
                    dplyr::select(AgeGroup, AgeGroup_Num) %>% 
                    left_join(groundTruth, by="AgeGroup")
        
        for(i in 1:length(levels(as.factor(joinDf$AgeGroup_Num)))) {
            poststratAge <- joinDf[joinDf$AgeGroup_Num == i, ]
            posteriorProbAge <- rstanarm::posterior_epred(
                fit,
                newdata = poststratAge, 
                draws = 1000
            )
            poststratProbAge <- posteriorProbAge %*% poststratAge$UserCount / sum(poststratAge$UserCount)
            #This is the estimate for popn in state:
            ageDf$model_age_pref[i] <- round(mean(poststratProbAge), 4)
            ageDf$model_age_sd[i] <- round(sd(poststratProbAge), 4)
        }
        ageDf$game = GAME
        ageDf$label = LABEL
        resAge <- rbind(resAge, ageDf)
        
        print(paste0("Game: ", GAME, " Label: ", LABEL))
        genderDf <- data.frame(
                    Gender = rev(unique(curr$Gender)), 
                    Gender_Num = 1:2, 
                    model_gender_sd = rep(-1, 2), 
                    model_gender_pref = rep(-1, 2))
    
        joinDf <- genderDf %>% 
                    dplyr::select(Gender, Gender_Num) %>% 
                    left_join(groundTruth, by="Gender")
    
        for(i in 1:length(levels(as.factor(joinDf$Gender_Num)))) {
            poststratGender <- joinDf[joinDf$Gender_Num == i, ]
            posteriorProbGender <- posterior_epred(
                fit,
                newdata = poststratGender, 
                draws = 1000
            )
            poststratProbGender <- posteriorProbGender %*% poststratGender$UserCount / sum(poststratGender$UserCount)
            genderDf$model_gender_pref[i] <- round(mean(poststratProbGender), 4)
            genderDf$model_gender_sd[i] <- round(sd(poststratProbGender), 4)
        }
        genderDf$game = GAME
        genderDf$label = LABEL
        
        resGender <- rbind(resGender, genderDf)
    }
}

# Temporarily save the data frame to a csv file due to long running time
write.csv(resGender, "Figure11.csv")
write.csv(resAge, "Figure12.csv")

############################## Plotting ##############################
plotTile <- function(tileDf, outPath) {
    # Ensure the order and formatting on the final plot 
    tileDf <- tileDf %>% mutate(Label = factor(as.character(Label), levels=sort(unique(tileDf$Label)))) 
    tileDf$label <- str_wrap(tileDf$Label, width=60)

    plt <- tileDf %>% 
        ggplot(aes(x=factor(game, level=c("1", "2", "3", "4", "5", "6", "7", "8","9","10","11", "12","13","14")), 
                    y=factor(label, levels=rev(sort(unique(tileDf$label)))), fill=stdev)) +
        geom_tile() +
        geom_text(aes(label = round(stdev, 3)), color="white", size=6) +
        labs(x="Games (Anonymized)", y="") +
        scale_fill_gradient(high = "#132B43", low = "#56B1F7") +
        labs(fill="Standard Deviation") +
        theme_bw() +  # plot formatting 
        theme(legend.position = "bottom",
                legend.title=element_text(size=18),
                legend.key.width= unit(4, 'cm'),
                legend.text = element_text(size=17),
                axis.text.y = element_text(size=16, hjust=0), 
                axis.text.x = element_text(angle =0, vjust = 1, hjust = 1, size=22),
                axis.ticks.x=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.x=element_text(size=24))
    ggsave(outPath, plt, height=15, width=18)
}


genderTileDf <- resAge %>% group_by(game, label) %>%
    rename("Label" = "label") %>%
    summarize(stdev=sd(model_age_pref, na.rm=TRUE)) %>%
    ungroup() 

ageTileDf <- resAge %>% group_by(game, label) %>%
    rename("Label" = "label") %>%
    summarize(stdev=sd(model_age_pref, na.rm=TRUE)) %>%
    ungroup() 

plotTile(genderTileDf, "Figure11.png")
plotTile(ageTileDf, "Figure12.png")
