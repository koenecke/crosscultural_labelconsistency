# Auditing Cross-Cultural Consistency of Human-Annotated Labels for Recommendation Systems

The repository contains code for figure reproduction of "Auditing Cross-Cultural Consistency of Human-Annotated Labels for Recommendation Systems", accepted at FAccT'23. Underlying data for figures are proprietary and not provided.

Please cite: Rock Yuren Pang*†, Jack Cenatempo*, Franklyn Graham, Bridgette Kuehn, Maddy Whisenant, Portia Botchway, Katie Stone Perez, and Allison Koenecke†. 2023. Auditing Cross-Cultural Consistency of Human-Annotated Labels for Recommendation Systems. In 2023 ACM Conference on Fairness, Accountability, and Transparency (FAccT ’23), June 12–15, 2023, Chicago, IL, USA. ACM, New York, NY, USA, 23 pages. https://doi.org/10.1145/3593013.3594098

For any inquiries, please contact koenecke@cornell.edu.


## Data Preparation

Our survey data is in wide format initially. Each row in the original Alchemer survey platform output is by user (a user can select multiple games per session, and each game can have multiple labels). To pivot the survey data so that each row is by user and game, run the below command to generate `longData.csv` in the working directory:

```
chmod +x ./run.sh
./run.sh
```

## Figures

Below is a list of R Script to reproduce the figures in the paper, followed by a brief description. 
* Figure 2: `Figure2_Poststratified_Estimate.R` The post-stratified estimates of "high replayability"  and "zen".
* Figure 3: `Figure3_Hofstede.R`. The relationship between a pair of countries' CDI and Survey Responses.
* Figure 4: `Figure4_translation.R` The translation differences may contribute to the differences across estimated annotation rates by country). Note that this R script can also generate Figure 8.
* Figure 5: `Figure5_F1.R`An evaluation of F1 scores across 14 countries from logistic regression models trained on either homogeneous and heterogeneous dataset. Note that you can also generate Figure 13 from the same script.
* **Appendix**
* Figure 6 `Figure_6_Survey_Respondent.R` The descriptive statistics of the survey respondent data.
* Figure 7: `Figure2_Poststratified_Estimate.R` The raw difference for "high replayability" and "zen" labels.
* Figure 8: `Fugure4_translation.R` The detailed Matching results for Figure 4 language translation difference.
* Figure 9: `Figure9_Spearman_Correlation.R` The Spearman correlation of the Label Ranking between 2 country pairs.
* Figure 10: `Figure10_Poststratified_CI.R` A heatmap of the standard deviations across countries of post-stratified label annotation estimates.
* Figure 11: `Figure11_12_Poststratified_CI_Age_Gender.R` A heatmap of the standard deviations across gender of post-stratified label annotation estimates.
* Figure 12: `Figure11_12_Poststratified_CI_Age_Gender.R` A heatmap of the standard deviations across age of post-stratified label annotation estimates.
* Figure 13: `Figure5_F1.R` An evaluation of F1 scores across 14 countries from logistic regression using stratified samples.

The code follows the [gentzkow-shapiro R style guides](https://github.com/gslab-econ/lab-manual/wiki/Code).
