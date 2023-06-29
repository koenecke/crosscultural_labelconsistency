# Fig. 4. The translation difference between English and native languages for four countries
# (Brazil, Germany, Mexico, and Poland) 
# For an action game’s inconsistently conceptualized label (“makes me feel amused or laugh”)
# Find the output: ./figures/Figure4.pdf
# Import packages
library(tidyverse)
library(dplyr)
library(caret)
library(stargazer)
library(caret)
library(MatchIt)
library(lmtest)
library(sandwich)
library(ggpubr)
library(cobalt)
library(DescTools)

RAW_DATA_PATH <- "rawData.csv"
REMOVED_GAME <- ""
INCONSISTENT_LABELS_PATH <- "./data/inconsistent_labels.csv"
GAME <- "" 

############### Data Preparation ################
df <- read.csv(RAW_DATA_PATH) %>%
    dplyr::filter(game != {{ REMOVED_GAME }}) %>%
    dplyr::filter(!(country %in% c("Saudi.Arabia", "India")))

inconsistentLabels <- read.csv(INCONSISTENT_LABELS_PATH)

# clean the data
df <- df %>% dplyr::mutate(
    eng_lang = ifelse(language == "English", 1, 0),
    male = ifelse(GENDER == "Male", 1, 0), # need to do this to do MatchIt
    eng_freq_bin = ifelse(english_frequency %in% c("Often", "Always"), 1, 0),
    experience = ifelse(PC.exp >= 5 | Console.exp >= 5, 1, 0)
)
df$experience <- rowMeans(df[c("PC.exp", "Console.exp")])

# subset to countries with bilingual speaker responses
df <- df %>%
    dplyr::filter(eng_freq_bin == 1) %>%
    dplyr::filter(country %in% c(
        "Argentina", "Brazil", "Chile", "Colombia",
        "Germany", "Greece", "Korea", "Mexico", "Poland"
    ))

############## Matching ################
match_sigs <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(match_sigs) <- c(
    "game", "country", "matched_n", "label", "coef", "se",
    "pval", "cluster_se", "cluster_pval", "diff_lower95ci", "diff_upper95ci"
)

df <- df %>% dplyr::filter(game == {{ GAME }})

# These four countries are the countries that will get a match
# Other countries would have too few observations to match for language
# More specifically, MatchIt will return "Error: No units were matched."
countries <- c("Brazil", "Germany", "Mexico", "Poland")
loveplots <- list()
# matchData <- list()
fig4 <- data.frame()
i <- 1
for (country in countries) {
    currDf <- df %>% dplyr::filter(country == {{ country }})
    m.out <- matchit(eng_lang ~ AGE + male + experience,
        data = currDf,
        distance = "logit",
        caliper = .2,
        mahvars = ~ AGE + male + experience
    )
    m.data <- match.data(m.out, data = currDf, distance = "mahalanobis")

    # For Figure 8
    # make the individual plots here
    plt <- love.plot(m.out, binary = "std") + ggtitle(paste0(country, ", n=", nrow(m.data))) + theme(axis.text = element_text(size = 12))
    loveplots[[i]] <- plt
    i <- i + 1
    
    # check significance
    out.mod <- glm(comedic ~ eng_lang, data = m.data, weights = weights, family="binomial")
    
    # For Figure 4
    eng.vers <- m.data %>% filter(country == {{ country }}, eng_lang == 1)
    noneng.vers <- m.data %>% filter(country == {{ country }}, eng_lang == 0)
    n <- nrow(eng.vers)
    eng.avg <- mean(eng.vers$comedic)
    eng.std <- sd(eng.vers$comedic)
    noneng.avg <- mean(noneng.vers$comedic)
    noneng.std <- sd(noneng.vers$comedic)
    avgdiff <- MeanDiffCI(eng.vers$comedic, noneng.vers$comedic)

    row <- c(country, n, eng.avg, eng.std, noneng.avg, noneng.std, avgdiff)
    fig4 <- rbind(fig4, row)
}

# Formatting for Figure 4
colnames(fig4) <- c(
    "country", "count.per.arm", "english.avg", "english.std",
    "nonenglish.avg", "nonenglish.std", "meandiff", "lwr.ci", "upr.ci"
)

fig4 <- fig4 %>%
    mutate_at(
        c(
            "english.avg", "english.std", "nonenglish.avg", "nonenglish.std",
            "meandiff", "lwr.ci", "upr.ci"
        ), as.numeric
    ) %>%
    mutate(
        eng.se = qnorm(.975) * english.std / sqrt(length(count.per.arm)),
        eng.lower = english.avg - qnorm(.975) * english.std,
        eng.upper = english.avg + qnorm(.975) * english.std,
        noneng.se = qnorm(.975) * nonenglish.std / sqrt(length(count.per.arm)),
        noneng.lower = nonenglish.avg - qnorm(.975) * nonenglish.std,
        noneng.upper = nonenglish.avg + qnorm(.975) * nonenglish.std
    )

plot.df <- fig4 %>%
    dplyr::select(country, english.avg, nonenglish.avg) %>%
    gather(language, average, english.avg:nonenglish.avg) %>%
    mutate(
        label = ifelse(language == "english.avg", "English", "Local"),
        granular_language = ifelse(country == "Brazil" & language == "nonenglish.avg", "Portuguese",
            ifelse(country == "Germany" & language == "nonenglish.avg", "German",
                ifelse(country == "Mexico" & language == "nonenglish.avg", "Spanish",
                    ifelse(country == "Poland" & language == "nonenglish.avg", "Polish", "English")
                )
            )
        )
    )

# Final formatting for Figure 4
bilingual.plot <- ggplot(plot.df, aes(x = country, y = average, color = label, shape = label)) +
    geom_point(aes(color = label, shape = label), position = position_dodge(0.5), size = 3, stroke = 4) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20), axis.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20), legend.title = element_text(size = 20), legend.position = "bottom",
        plot.margin = margin(10, 50, 10, 10),
        title = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 22),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
    ) +
    scale_color_manual(values = c(
        "English" = "black",
        "Local" = "purple"
    ), name = "Survey Language") +
    scale_shape_manual(values = c(
        "English" = 4,
        "Local" = 5
    ), name = "Survey Language") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0, 1), expand = c(0, 0)) +
    labs(y = "Annotation Share for \n'makes me feel amused or laugh'", x = "Country of Bilingual Respondents") +
    geom_hline(yintercept = .5, linetype = "dashed", size = 2.5) +
    facet_grid(cols = vars(country), scales = "free_x")

ggsave(paste0("./figures/Figure4.pdf"), bilingual.plot)

# Figure 8
balplots <- ggarrange(loveplots[[1]], loveplots[[2]], loveplots[[3]], loveplots[[4]], ncol = 2, nrow = 2)
ggsave(paste0("./figures/Figure8.pdf"), balplots)