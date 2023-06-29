# Fig. 3. cultural distance index and country pair annotation correlation
# Find the output: ./figures/Figure3.pdf
# Import library and set theme
library(tidyverse)
library(reshape2)
library(ggpubr)
options(dplyr.summarise.inform = FALSE)

REMOVED_GAME <- ""

# remove low data country and games
df <- read.csv("longData.csv") %>% 
        dplyr::filter(!Country %in% c("India", "Saudi Arabia"), Game != {{ REMOVED_GAME }})
postDf <- read.csv("./data/poststratified.csv")

print("### Processing Consistent and Inconsistent Labels ###")
# process inconsistent labels 
# Create a dataframe to store all inconsistent labels based on the poststratified results
inconsistent <- postDf %>% 
    group_by(Country, game, label) %>%  
    summarize(mean=mean(model_country_pref)) %>%
    ungroup() %>% 
    group_by(game, label) %>% # get the mean value and standard deviation for each country's game/label's mean value
    summarize(sdv=sd(mean), country_mean=mean(mean), min.value=min(mean), max.value=max(mean)) %>%
    ungroup() %>% # criteria for inconsistent labels (too much standard deviations)
    dplyr::filter(sdv > 0.05) %>%
    dplyr::filter(min.value < 0.5, max.value>0.5) 

# Create a dataframe to store all consistent labels
consistent <- postDf %>% 
    anti_join(inconsistent, by=c("game", "label")) %>%
    group_by(Country, game, label) %>%  
    summarize(mean=mean(model_country_pref)) %>%
    ungroup() %>% 
    group_by(game, label) %>% # get the mean value and standard deviation for each country's game/label's mean value
    summarize(sdv=sd(mean), country_mean=mean(mean), min.value=min(mean), max.value=max(mean)) %>%
    ungroup() 

inconsistent$game <- factor(inconsistent$game)
inconsistent$label <- factor(inconsistent$label)
consistent$game <- factor(consistent$game)
consistent$label <- factor(consistent$label)

# write the consistent and inconsistent labels to csv files
write.csv(inconsistent, "./data/inconsistent_labels.csv")
write.csv(consistent, "./data/consistent_labels.csv")

# read Hofstede's cultural distance index
hofstedeDf <- read.csv("./data/hofstede.csv")
hofstedeDf <- hofstedeDf %>% 
    dplyr::select(-X) %>%
    dplyr::mutate(power.distance = power.distance / 100,
                    individualism = individualism / 100,
                    masculinity = masculinity / 100,
                    uncertainty = uncertainty / 100,
                    long.term.orientation = long.term.orientation / 100,
                    indulgence = indulgence / 100) %>%
  dplyr::select(country, uncertainty, long.term.orientation) # only select significant two dimensions


# Check Hofstede index significance
reg_df <- postDf %>% 
  left_join(hofstedeDf,by=c('Country'='country')) %>%
  group_by(Country, game, label) %>%  
  summarize(Value=mean(model_country_pref))
model <- glm(Value ~ Game + Label + power.distance + individualism + masculinity + uncertainty + long.term.orientation + indulgence, 
             data=reg_df, family = 'binomial')
summary(model)

# Calculating CDI
mat <- as.matrix(hofstedeDf[, c("uncertainty", "long.term.orientation")])
rownames(mat) <- hofstedeDf$country
euclidian <- dist(mat, method="euclidean")
hofstedeCorr <- melt(as.matrix(euclidian), varnames = c("countryA", "countryB"))
hofstedeCorr <- hofstedeCorr %>% filter(countryA != countryB) %>% rename("hofstede.corr"="value")

# combine the two dataframes of consistent and inconsistent labels + CDI
# fullDf contains both consistent and inconsistent game label pairs
fullDf <- postDf %>% 
    left_join(
        inconsistent %>% 
            dplyr::mutate(inconsistent=1) %>% # create a column to note the inconsistent labels
            dplyr::select(game, label, inconsistent),
        by=c("game", "label")
    ) %>% 
    dplyr::rename("Game" = "game", "Label"="label")

consistentDf <- fullDf %>% dplyr::filter(is.na(inconsistent)) %>% mutate(inconsistent=0)
consistentDf$Game <- factor(consistentDf$Game)
consistentDf$Label <- factor(consistentDf$Label)
inconsistentDf <- fullDf %>% filter(inconsistent == 1)
inconsistentDf$Game <- factor(inconsistentDf$Game)
inconsistentDf$Label <- factor(inconsistentDf$Label)

####################### Calculate Label Pair Correlation #######################
############################ Inconsistent Labels ###############################
# Create constant variables to maintain order of games and labels
INCONSISTENT_VECTOR_LENGTH <- nrow(inconsistent)
GAME_ORDER <-  unique(inconsistentDf$Game)
LABEL_ORDER <- unique(inconsistentDf$LABEL)
COUNTRIES <- unique(inconsistentDf$Country)
labelSpace <- data.frame(matrix(ncol=0, nrow=INCONSISTENT_VECTOR_LENGTH))

# populate labelSpace, which has the INCONSISTENT_VECTOR_LENGTH number of rows
# each column will be populated in this for loop
# each column is a country's model_country_pref for each game/label pair
for(country in COUNTRIES) {
  curr <- inconsistentDf %>% filter(Country == {{ country }}) %>% arrange(Game, Label)
  v <- c(curr$model_country_pref)
  labelSpace <- cbind(labelSpace, as.data.frame(v))
  names(labelSpace)[names(labelSpace) == "v"] <- country
}

# with the labelSpace dataframe, calculate the correlation between each country's game/label pair using cor()
n <- ncol(labelSpace)
cor_matrix <- matrix(NA, n, n)
colnames(cor_matrix) <- colnames(labelSpace)
rownames(cor_matrix) <- colnames(labelSpace)
for (i in 1:n) {
  for (j in 1:n) {
    cor_matrix[i, j] <- cor(labelSpace[, i], labelSpace[, j])
  }
}

labelCorr <- melt(as.matrix(cor_matrix), varnames = c("countryA", "countryB"))
labelCorr <- labelCorr %>% filter(countryA != countryB) %>% rename("label.corr"="value")

combined.inconsistent <- hofstedeCorr %>% left_join(labelCorr, by=c("countryA", "countryB")) %>% na.omit() %>% mutate(consistent=1)

############################ Consistent Labels #################################
CONSISTENT_VECTOR_LENGTH <- nrow(consistent)
labelSpace <- data.frame(matrix(ncol=0, nrow=CONSISTENT_VECTOR_LENGTH))

for(country in COUNTRIES) {
  curr <- consistentDf %>% filter(Country == {{ country }}) %>% arrange(Game, Label)
  v <- c(curr$model_country_pref)
  labelSpace <- cbind(labelSpace, as.data.frame(v))
  names(labelSpace)[names(labelSpace) == "v"] <- country
}

n <- ncol(labelSpace)
cor_matrix <- matrix(NA, n, n)
colnames(cor_matrix) <- colnames(labelSpace)
rownames(cor_matrix) <- colnames(labelSpace)
for (i in 1:n) {
  for (j in 1:n) {
    cor_matrix[i, j] <- cor(labelSpace[, i], labelSpace[, j])
  }
}

labelCorr <- melt(as.matrix(cor_matrix), varnames = c("countryA", "countryB"))
labelCorr <- labelCorr %>% filter(countryA != countryB) %>% rename("label.corr"="value")

combined.consistent <- hofstedeCorr %>% left_join(labelCorr, by=c("countryA", "countryB")) %>% na.omit() %>% mutate(consistent=0)

# Check correlation coefficients
print("### Showing Coefficients between CDI and Label Correlations ###")
consistentCorr <- rbind(combined.consistent, combined.inconsistent) %>% filter(consistent==0)
inconsistentCorr <- rbind(combined.consistent, combined.inconsistent) %>% filter(consistent==1)

print(summary(lm(consistentCorr$hofstede.corr ~ consistentCorr$label.corr)))
print(summary(lm(inconsistentCorr$hofstede.corr ~ inconsistentCorr$label.corr)))

print("### Plotting Correlation ###")
p <- rbind(combined.consistent, combined.inconsistent) %>% 
        dplyr::mutate(consistent = ifelse(consistent==1, "Inconsistently Conceptualized", "Consistently Conceptualized")) %>%
        dplyr::mutate(consistent=factor(consistent)) %>%
        ggplot(aes(x=hofstede.corr, label.corr, color=consistent, shape=consistent)) +
        geom_point() +
        geom_smooth(method="lm", level=0.99) + 
        ylab("Country Pair Annotation Correlation") +
        xlab("Country Pair Cultural Distance Index") +
        labs(title = "") +
        theme_bw() +
        theme(legend.position="bottom",
                axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size=12), 
                axis.text.y = element_text(angle = 0, vjust = 1, hjust = 1, size=12),
                axis.title.y = element_text(size=16),
                axis.title.x = element_text(size=16),
                legend.text=element_text(size=14), 
                legend.title=element_blank(), 
                plot.title = element_text(size=18),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()) 

ggsave("./figures/Figure3.pdf", p, width=8, height=8)