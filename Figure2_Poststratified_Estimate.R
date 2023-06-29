# Fig. 2. Post-stratified estimates that a certain action game would be annotated with the label "high replayability" or "zen"
# Find the output: ./figures/Figure2.pdf
# Find the output: ./figures/Figure7.pdf
# This script is used to generate Figure 2 and Figure 7 in the paper
# Import library and set theme
library(tidyverse)
library(rstanarm)
library(gridExtra)
source("./utils.R")

# Replace anonymized games with relevant survey data
REMOVED_GAME <- ""
GAME <- ""

df <- read.csv("longDataset.csv") %>% 
        dplyr::filter(!Country %in% c("India", "Saudi Arabia"), Game != {{ REMOVED_GAME }}) # Remove India and Saudi Arabia due to small sample size

groundTruth <- readGroundTruth(path="./data/demographic.csv") %>% # underlying demographic is proprietary and not provided
        dplyr::filter(!Country %in% c("India", "Saudi Arabia")) 

postEstimateCountry <- function(data, model) {
    countryDf <- data.frame(Country = rev(unique(data$Country)), 
                        Country_Num = 1:14, 
                        model_country_sd = rep(-1, 14), 
                        model_country_pref = rep(-1, 14))
    demographicsDf <- countryDf %>% 
        dplyr::select(Country, Country_Num) %>% 
        left_join(groundTruth, by="Country")

    for(i in 1:length(levels(as.factor(demographicsDf$Country_Num)))) {
        poststratCountry <- demographicsDf[demographicsDf$Country_Num == i, ]
        posteriorProbCountry <- rstanarm::posterior_epred(
            model,
            newdata = poststratCountry, 
            draws = 1000
        )
        poststratProbCountry <- posteriorProbCountry %*% poststratCountry$UserCount / sum(poststratCountry$UserCount)
        countryDf$model_country_pref[i] <- round(mean(poststratProbCountry), 4)
        countryDf$model_country_sd[i] <- round(sd(poststratProbCountry), 4)
    }
    return(countryDf)
}

postEstimateAge <- function(data, model) {
    ageDf <- data.frame(AgeGroup = rev(unique(data$AgeGroup)), 
                        AgeGroup_Num = 1:5, 
                        model_age_sd = rep(-1, 5), 
                        model_age_pref = rep(-1, 5))

    demographicsDf <- ageDf %>% 
        dplyr::select(AgeGroup, AgeGroup_Num) %>% 
        left_join(groundTruth, by="AgeGroup")

    for(i in 1:length(levels(as.factor(demographicsDf$AgeGroup_Num)))) {
        poststratAge <- demographicsDf[demographicsDf$AgeGroup_Num == i, ]
        posteriorProbAge <- rstanarm::posterior_epred(
            model,
            newdata = poststratAge, 
            draws = 1000
        )
        poststratProbAge <- posteriorProbAge %*% poststratAge$UserCount / sum(poststratAge$UserCount)
        ageDf$model_age_pref[i] <- round(mean(poststratProbAge), 4)
        ageDf$model_age_sd[i] <- round(sd(poststratProbAge), 4)
    }
    return(ageDf)
}

postEstimateGender <- function(data, model) {
    genderDf <- data.frame(Gender = rev(unique(data$Gender)),
                            Gender_Num = 1:2, 
                            model_age_sd = rep(-1, 2), 
                            model_age_pref = rep(-1, 2))
    demographicsDf <- genderDf %>%
        dplyr::select(Gender, Gender_Num) %>%
        left_join(groundTruth, by="Gender")
    
    for(i in 1:length(levels(as.factor(demographicsDf$Gender_Num)))) {
        poststratGender <- demographicsDf[demographicsDf$Gender_Num == i, ]
        posteriorProbGender <- rstanarm::posterior_epred(
            model,
            newdata = poststratGender, 
            draws = 1000
        )
        poststratProbGender <- posteriorProbGender %*% poststratGender$UserCount / sum(poststratGender$UserCount)
        genderDf$model_gender_pref[i] <- round(mean(poststratProbGender), 4)
        genderDf$model_gender_sd[i] <- round(sd(poststratProbGender), 4)
    }
    return(genderDf)
}

# Fit a stan_glmer model
# Reference: https://bookdown.org/jl5522/MRP-case-studies/introduction-to-mister-p.html
hr <- df %>% dplyr::filter(Label == "high_replayability"  & Game == {{ GAME }})
zen <- df %>% dplyr::filter(Label == "is zen."  & Game == {{ GAME }})
print("### Model training ###")
modelHr <- rstanarm::stan_glmer(Value ~ (1 | Country) + (1|AgeGroup) + Gender,
                                    family = binomial(link = "logit"),
                                    data = hr,
                                    prior = normal(0, 1, autoscale = TRUE),
                                    prior_covariance = decov(scale = 0.50),
                                    adapt_delta = 0.99,
                                    refresh = 0,
                                    seed = 1010)
modelZen <- rstanarm::stan_glmer(Value ~ (1 | Country) + (1|AgeGroup) + Gender,
                                    family = binomial(link = "logit"),
                                    data = zen,
                                    prior = normal(0, 1, autoscale = TRUE),
                                    prior_covariance = decov(scale = 0.50),
                                    adapt_delta = 0.99,
                                    refresh = 0,
                                    seed = 1010)

# Post-stratified estimates
# high replayability model (Hr)
print("### Post-stratified estimating ###")
countryDfHr <- postEstimateCountry(data=hr, model=modelHr)
countryDfZen <- postEstimateCountry(data=zen, modelZen)
ageDfHr <- postEstimateAge(data=hr, model=modelHr)
ageDfZen <- postEstimateAge(data=zen, modelZen)
genderDfHr <- postEstimateGender(data=hr, model=modelHr)
genderDfZen <- postEstimateGender(data=zen, modelZen)

# Plot
print("### Plotting ###")
g.gender <- rbind(
                genderDfHr %>% dplyr::mutate(Label = "Has high \nreplayability."), 
                genderDfZen %>% dplyr::mutate(Label = "Is zen.")
            ) %>%
            dplyr::mutate(title="Gender") %>%
            ggplot(aes(x=Gender, y=model_gender_pref, colour=Label, group=Label, fill=Label, shape=Label)) + 
            geom_point(size=3) +
            geom_line() +
            geom_ribbon(aes(ymin=pmax(0, model_gender_pref - model_gender_sd), ymax=pmin(1, model_gender_pref + model_gender_sd)), alpha=.3, linetype=0) +
            scale_fill_manual(values=c("#2ca25f", "#FF5733"), name="fill") +
            scale_color_manual(values=c("#238b45", "#900C3F")) +
            geom_hline(yintercept=0.5, linetype="dashed") + 
            ylab("Estimated Probability of the \nSubgroup Annotating Game with Labels") +
            facet_grid(. ~ title) +
            ylim(c(0,1)) + theme(legend.position = "none", axis.title.x=element_blank(), strip.text.x = element_text(size=14),
                                axis.text.x=element_text(margin=margin(29), angle = 45, vjust = 1.5, hjust = 1, size=15),
                                axis.title.y=element_text(size=13)) 

g.age <- rbind(
    ageDfHr %>% dplyr::mutate(Label = "Has high \nreplayability."), 
    ageDfZen %>% dplyr::mutate(Label = "Is zen.")
    ) %>%
        dplyr::mutate(title="Age") %>%
        ggplot(aes(x=factor(AgeGroup, levels=c("18 - 24", "25 - 34", "35 - 44", "45 - 55", "> 55")), y=model_age_pref, colour=Label, group=Label, fill=Label, shape=Label)) + 
        geom_point(size=3) +
        geom_line() +
        geom_ribbon(aes(ymin=pmax(0, model_age_pref - model_age_sd), ymax=pmin(1, model_age_pref + model_age_sd)), alpha=.3, linetype=0) +
        scale_fill_manual(values=c("#2ca25f", "#FF5733"), name="fill") +
        scale_color_manual(values=c("#238b45", "#900C3F")) +
        geom_hline(yintercept=0.5, linetype="dashed") +
        facet_grid(. ~ title) +
        ylim(c(0,1)) +theme(legend.position = "none", 
                            strip.text.x = element_text(size=14),
                            axis.text.y=element_blank(),
                            axis.ticks.y=element_blank(),
                            axis.title.y=element_blank(), 
                            axis.title.x=element_blank(),
                            axis.text.x=element_text(margin=margin(30), angle = 45, vjust = 1.5, hjust = 1, size=15))

# arrange country order in the plot
countryOrder <- countryDfHr %>% dplyr::mutate(Label = "Has high \nreplayability.") %>% arrange(model_country_pref) 

g.country <- rbind(
    countryDfHr %>% mutate(Label = "Has high \nreplayability."), 
    countryDfZen %>% mutate(Label = "Is zen.")
    ) %>%
        dplyr::mutate(title="Country") %>%
        ggplot(aes(x=factor(Country, levels=countryOrder$Country), y=model_country_pref, colour=Label, group=Label, fill=Label, shape=Label)) + 
        geom_point(size=3) +
        geom_line() +
        geom_ribbon(aes(ymin=pmax(0, model_country_pref - model_country_sd), ymax=pmin(1, model_country_pref + model_country_sd)), alpha=.3, linetype=0) +
        scale_fill_manual(values=c("#2ca25f", "#FF5733"), name="Label") +
        scale_color_manual(values=c("#238b45", "#900C3F")) +
        geom_hline(yintercept=0.5, linetype="dashed") +
        facet_grid(. ~ title) +
        labs(x="", fill="Label") +
        ylim(c(0,1)) +
        theme_bw() +
        theme(axis.text.y=element_blank(),
                strip.text.x = element_text(size=14),
                axis.ticks.y=element_blank(), 
                axis.title.y=element_blank(),
                axis.title.x=element_blank(),
                axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1, size=15),
                legend.title=element_text(size=12),
                legend.text=element_text(size=11), 
                plot.title = element_text(size=18),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())

p <- grid.arrange(
  g.gender, g.age, g.country,
  widths = c(4, 4, 9)
)

ggsave(plot = p, filename="./figures/Figure2.pdf", width = 15, height = 6)


#################### Plot for Figure 7 ####################
# This plot only shows the raw data, not the poststratified estimates
g.gender <- df %>% filter(Label %in% c("high_replayability", "is zen."), Game=={{ GAME }}) %>%
  group_by(Label, Gender) %>% 
  summarize(mean=mean(Value), stdev=sd(Value)/sqrt(length(Value))) %>%  
  mutate(title="Gender") %>%
  ggplot(aes(x=Gender, y=mean, colour=Label, group=Label, fill=Label, shape=Label)) + 
  geom_point(size=3) +
  geom_line() +
  geom_ribbon(aes(ymin=pmax(0, mean - stdev), ymax=pmin(1, mean + stdev)), alpha=.3, linetype=0) +
  scale_fill_manual(values=c("#EEA47FFF", "#00539CFF"), name="fill") +
  scale_color_manual(values=c("#b54c17", "#114355")) +
  geom_hline(yintercept=0.5, linetype="dashed") + 
  ylab("Proportion of the Subgroup \nAnnotating a Game with Labels") +
  facet_grid(. ~ title) +
  ylim(c(0,1)) + theme(legend.position = "none", axis.title.x=element_blank(), strip.text.x = element_text(size=14),
                       axis.text.x=element_text(margin=margin(29), angle = 45, vjust = 1.5, hjust = 1, size=15),
                       axis.title.y=element_text(size=13))   

g.age <- df %>% filter(Label %in% c("high_replayability", "is zen."), Game=={{ GAME }}) %>%
  group_by(Label, AgeGroup) %>% 
  summarize(mean=mean(Value), stdev=sd(Value)/sqrt(length(Value))) %>%
  mutate(title="Age") %>%  
  ggplot(aes(x=factor(AgeGroup, levels=c("18 - 24", "25 - 34", "35 - 44", "45 - 55", "> 55")), y=mean, colour=Label, group=Label, fill=Label, shape=Label)) + 
  geom_point(size=3) +
  geom_line() +
  geom_ribbon(aes(ymin=pmax(0, mean - stdev), ymax=pmin(1, mean + stdev)), alpha=.3, linetype=0) +
  scale_fill_manual(values=c("#EEA47FFF", "#00539CFF"), name="fill") +
  scale_color_manual(values=c("#b54c17", "#114355")) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  facet_grid(. ~ title) +
  ylim(c(0,1)) +theme(legend.position = "none", 
                      strip.text.x = element_text(size=14),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.title.y=element_blank(), 
                      axis.title.x=element_blank(),
                      axis.text.x=element_text(margin=margin(30), angle = 45, vjust = 1.5, hjust = 1, size=15))



countryOrder <- df %>% filter(Label %in% c("high_replayability", "is zen."), Game=={{ GAME }}) %>%
  group_by(Label, Country) %>% 
  summarize(mean=mean(Value), stdev=sd(Value)/sqrt(length(Value))) %>%
  filter(Label=="high_replayability") %>% 
  arrange(mean) 

g.country <- df %>% filter(Label %in% c("high_replayability", "is zen."), Game=={{ GAME }}) %>%
  group_by(Label, Country) %>% 
  summarize(mean=mean(Value), stdev=sd(Value)/sqrt(length(Value))) %>%
  mutate(title="Country") %>%
  mutate(Label = ifelse(Label == "high_replayability",  "Has high \nreplayability.", "Is zen.")) %>%
  ggplot(aes(x=factor(Country, levels=countryOrder$Country), y=mean, colour=Label, group=Label, fill=Label, shape=Label)) + 
  geom_point(size=3) +
  geom_line() +
  geom_ribbon(aes(ymin=pmax(0, mean - stdev), ymax=pmin(1, mean + stdev)), alpha=.3, linetype=0) +
  scale_fill_manual(values=c("#EEA47FFF", "#00539CFF"), name="Label") +
  scale_color_manual(values=c("#b54c17", "#114355")) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  facet_grid(. ~ title) +
  ylim(c(0,1)) +theme(axis.text.y=element_blank(),
                      strip.text.x = element_text(size=14),
                      axis.ticks.y=element_blank(), 
                      axis.title.y=element_blank(),
                      axis.title.x=element_blank(),
                      axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1, size=15),
                      legend.title=element_text(size=12),
                      legend.text=element_text(size=11))

p <- grid.arrange(
  g.gender, g.age, g.country,
  widths = c(4, 4, 9)
)

ggsave(plot=p, filename="./figures/Figure7.pdf", width = 15, height = 6)