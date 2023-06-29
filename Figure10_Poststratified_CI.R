# Fig. 10. A heatmap of the standard deviations across countries of post-stratified label annotation estimates for each game (anonymized) and label
# Reference: Juan Lopez-Martin, Justin H. Phillips, and Andrew Gelman, https://bookdown.org/jl5522/MRP-case-studies/ 
# This code will take a long time to run because MCMC is a computationally intensive algorithm
# The total running time is around 10 hours
# Find the output: ./figures/Figure10.pdf
library(tidyverse)
library(rstanarm)
source("./utils.R")

REMOVED_GAME <- ""
DEMOGRAPHIC_FILE <- ""

df <- read.csv("./longData.csv") %>%
        dplyr::filter(!Country %in% c("India", "Saudi Arabia"), Game != {{ REMOVED_GAME }}) 
groundTruth <- readGroundTruth(path=DEMOGRAPHIC_FILE) %>% 
        dplyr::filter(!Country %in% c("India", "Saudi Arabia")) 


LABELS <- unique(df$Label)
GAMES <- unique(df$Game)

# This is the placeholder final data frame where we store:
# country, number of participants in each country, model standard deviation, model mean, game, label
res <- data.frame(Country = c(), 
                    Country_Num = c(), 
                    model_country_sd=c(), 
                    model_country_pref=c(), 
                    game=c(), 
                    label=c())
# Each iteration of this loop will fit a model for a single game and label combination
# and then post-stratify the model to estimate the population mean and standard deviation for each country
# Each iteration of this loop will take a long time to run because MCMC is a computationally intensive algorithm
# Each iteration is about 1.5 minutes (11*28*1.5 = 594 minutes = 9.9 hours)
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
        countryDf <- data.frame(
                    Country = rev(unique(df$Country)),
                    Country_Num = 1:14,
                    model_country_sd = rep(-1, 14),
                    model_country_pref = rep(-1, 14))    
        # join the countryDf dataframe with the dataframe dataframe
        joinDf <- countryDf %>% 
                    dplyr::select(Country, Country_Num) %>% 
                    left_join(groundTruth, by="Country")
        
        for(i in 1:length(levels(as.factor(joinDf$Country_Num)))) {
            poststratCountry <- joinDf[joinDf$Country_Num == i, ]
            posteriorProbCountry <- rstanarm::posterior_epred(
                fit,
                newdata = poststratCountry, 
                draws = 1000
            )
            poststratProbCountry <- posteriorProbCountry %*% poststratCountry$UserCount / sum(poststratCountry$UserCount)
            #This is the estimate for popn in state:
            countryDf$model_country_pref[i] <- round(mean(poststratProbCountry), 4)
            countryDf$model_country_sd[i] <- round(sd(poststratProbCountry), 4)
        }

        countryDf$game = GAME
        countryDf$label = LABEL
        res <- rbind(res, countryDf)
    }
}

write.csv(res, "Figure10.csv")

############### Plotting ###############
tileDf <- res %>% group_by(game, label) %>%
    rename("Label" = "label") %>%
    summarize(stdev=sd(model_country_pref, na.rm=TRUE)) %>%
    ungroup() 

# Ensure the order and formatting on the final plot 
tileDf <- tileDf %>% mutate(Label = factor(as.character(Label), levels=sort(unique(tileDf$Label)))) 
tileDf$label <- str_wrap(tileDf$Label, width=60)

plt <- tileDf %>%  # ensure the plot is ordered correctly with anonymized games
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

ggsave("./figures/Figure10.pdf", plt, height=15, width=18)