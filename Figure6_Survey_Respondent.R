# Fig. 6. The distribution of survey respondents across gender, age, and country demographic subgroups. 
# Bars correspond to percentage of group distribution; 
# corresponding respondent counts are indicated above bars.
# Find the output: ./figures/Figure6.pdf
# Import library and set theme
library(tidyverse)
library(gridExtra)

theme_set(theme_bw() + theme(
  plot.title = element_text(size=18),
  axis.text.y = element_text(size=14), 
  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=14), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()))

# Read the survey data
df <- read.csv("dataset.csv") 
# plot gender distribution
gender.plt <- df %>% 
  dplyr::mutate(GENDER = ifelse(GENDER=="Another gender (please specify)", "Non-Binary", GENDER)) %>% # rename the nonbinary column name 
  dplyr::distinct(GENDER, Response.ID) %>% 
  dplyr::count(GENDER) %>% 
  dplyr::mutate(prop=n/sum(n)) %>% 
  dplyr::mutate(title="Gender") %>%
  ggplot(aes(x=reorder(GENDER, prop), y=prop)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=scales::comma(round(n), accuracy=1)), vjust = -0.8, size=6) +
    facet_grid(. ~ title) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) +
    theme(legend.position = "none", 
            axis.title.x=element_blank(), 
            axis.title.y=element_text(size=13),
            strip.text.x = element_text(size=14),
            axis.text.x=element_text(margin=margin(29), angle = 45, vjust = 1.5, hjust = 1, size=15)) 

# plot age distribution
age.plt <- df %>% 
  dplyr::distinct(ageCut, Response.ID) %>% 
  dplyr::count(ageCut) %>% 
  dplyr::mutate(prop=n/sum(n)) %>% 
  dplyr::mutate(title="Age") %>%
  ggplot(aes(x=ageCut, y=prop)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=scales::comma(round(n), accuracy=1)), vjust = -0.8, size=6) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) +
    facet_grid(. ~ title) +
    theme(legend.position = "none", 
                      strip.text.x = element_text(size=14),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.title.y=element_blank(), 
                      axis.title.x=element_blank(),
                      axis.text.x=element_text(margin=margin(55), angle = 45, vjust = 2.5, hjust = 1, size=15))

# plot country distribution
country.plt <- df %>% 
    dplyr::distinct(country, Response.ID) %>% 
    dplyr::count(country) %>% 
    dplyr::mutate(prop=n/sum(n)) %>% 
    dplyr::mutate(title="Age") %>%
    ggplot(aes(x=country, y=prop)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=scales::comma(round(n), accuracy=1)), vjust = -0.8, size=4) +
        facet_grid(. ~ title) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
        theme(axis.text.y=element_blank(),
                      strip.text.x = element_text(size=14),
                      axis.ticks.y=element_blank(), 
                      axis.title.y=element_blank(),
                      axis.title.x=element_blank(),
                      axis.text.x=element_text(margin=margin(25), angle = 45, vjust = 1.2, hjust = 1, size=15),
                      legend.title=element_text(size=12),
                      legend.text=element_text(size=11))

p <- grid.arrange(
  gender.plt, age.plt, country.plt,
  widths = c(4, 4, 9)
)

ggsave(plot=p, filename="./figures/Figure6.pdf", width = 15, height = 7)