# Figure 9:  The distribution of the Spearman correlations (between country pairs) 
# of ordinal label rankings in a specific game
# Find the output: ./figures/Figure9.pdf
library(tidyverse)

POSTSTRATIFIED_DATA_PATH <- ""
REMOVED_GAME <- ""
df <- read.csv(POSTSTRATIFIED_DATA_PATH)

# Find the consistent labels and inconsistent labels from poststratification (Figure 10)
inconsistentLabelDf <- read.csv("./data/inconsistent_labels.csv") %>% mutate(inconsistent=1)
consistentLabelDf <- read.csv("./data/consistent_labels.csv") %>% mutate(inconsistent=0)
INCONSISTENT_LABELS <- inconsistentLabelDf
CONSISTENT_LABELS <- consistentLabelDf

# rankDf is the dataframe where we store the spearman correlation between: 
# the ranks of the labels/games between two countries
rankDf <- data.frame(Country1=c(), Country2=c(), game=c(), corr=c())
COUNTRIES <- unique(df$Country)
LABELS <- unique(df$label)
GAMES <- c("")  # can't share this data

VISITED = c()
for(GAME in GAMES) {
  for(COUNTRY_1 in COUNTRIES) {
    # for each country and game, we rank the likelihood of the labels
    # since we set levels=LABELS, the labels are ranked in the order of LABELS
    # then we can compare the correlation between the ranks of two countries
    df.c1 <- df %>% filter(game == { GAME }, Country == { COUNTRY_1 }) %>%
      arrange(factor(label, levels=LABELS)) %>%
      mutate(rank=rank(model_country_pref))

    # v.c1 is the game, label vector of ranks for country 1
    v.c1 <- df.c1$rank
    for(COUNTRY_2 in COUNTRIES) {
      # if the countries are the same, the correlation would be perfect=1
      if(COUNTRY_2 %in% VISITED) next;
      df.c2 <- df %>% filter(game == { GAME }, Country == { COUNTRY_2 }) %>%
        arrange(factor(label, levels=LABELS)) %>%
        mutate(rank=rank(model_country_pref))

      # v.c2 is the game, label vector of ranks for country 2
      v.c2 <- df.c2$rank

      # compare the correlation between the label vectors
      rank.corr <- cor(v.c1, v.c2, method = c("spearman"))
      # append the data to the rankDf for visualization later
      rankDf <- rbind(data.frame(Country1=COUNTRY_1, Country2=COUNTRY_2, game=GAME, corr=rank.corr), rankDf)
    }
    VISITED <- c(VISITED, COUNTRY_1)
  }
}

rankDf <- rankDf %>% filter(Country1 != Country2)

rankDf %>% filter(game != {{ REMOVED_GAME }}) %>% ggplot(aes(Country1, Country2, fill= corr)) + 
  geom_tile() + facet_wrap(~game, scales = "free", ncol=5) + 
  scale_fill_gradient(limits = c(-1,1))

plt <- ggplot(rankDf, aes(corr)) + 
  geom_histogram(color=("black"), fill="grey", binwidth = 0.03) +
  labs(x="Spearman Correlation", y="Frequency") +
  theme(axis.text.x = element_text(angle =0, vjust = 1, hjust = 1, size=12),)

ggsave("./figures/Figure9.pdf", plt, width=10)