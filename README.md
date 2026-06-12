# In_2L_t_behavior_Drosophila
Containing the data and scripts for the 2026 publication on In_2L_t's impact on D. Melanogaster behavior
We have included the scripts used for analyzing the fly position data, constructing activity and startle response phenotypes, statistical analysis, and figure creation.
We have also included the raw position data annotated with the fly genotypes, in addition to phenotype data that is organized at the individual level. We have included the simplified data objects used to reconstruct the figures used in the publications. 
Additionaly, we include the script for our re-analysis of the Lee 2017 along with the relevant data from this publication.

Data processing pipeline:

Scripts 01-03. – Inputs exp[1-3]behaviordataRAW, output exp[1-3]behaviordataPROCESSED
Reads in the locomotion data from the DART, and estimates the baseline speed of individual flies before the first stimilu, their duration of startle response after each stimulus, and their magnitude of startle response after each stimilu. Saves individual, stimuli specific data. 
Script 04. – Inputs exp[1-3]behaviordataPROCESSED, output activity.startle.phenotyes.FINAL and figuredataFINAL
Combines the three locomotion phenotypes, finds average phenotypes at the individual level, organizes the data for analysis, and saves data ready for modeling and figure creation
Script 05. Inputs sleepexp[1-3]RAW , output sleepdataFINAL
Reads in the sleep data from DART, organizes the sleep phenotypes at the individual level. Combines data across experiments, and saves the output in a form ready for modeling.
Scripts 06-08. Inputs exp[1-3]positionsRAW, output Input exp[1-3]positionsPROCESSED
Reads in the fly position data from the DART, and calculates the proportion of time the flies spend in each region of the holding tube. This position phenotype data is saved at the indivual level
Script 09. Inputs exp[1-3]positionsPROCESSED, output positionsFINAL
Combines the three experiments worth of position phenotypes, and saves into a file format used for both modeling and figure creation
Script 10. Inputs activity.startle.phenotyes.FINAL, sleepdataFINAL, and positionsFINAL
Performs the modeling analysis of the paper, and outputs results in summary tables. First, the script loads in the individual level for the 6 phenotypes being considered, and performs final data organization prior to modeling. Next, the script contructs a series of 8 models for each trait: A null model, sex model, inversion genotype model, and interaction model for flies of both sexes at 25C, and a null model, temperature model, inversion genotype model, and interaction model for female flies at all temperatures. Models are compared, and the summary statistics of the likelihood ratio tests are saved for each model comparison. The summary statistics for the sex-specific and temperature-specific model comparisons are saved as .csv files.
Script 11. Inputs activity.startle.phenotyes.FINAL, sleepdataFINAL, and positionsFINAL
Performs the t.test analysis of differences across groups in the data. Similarly to before, the individual level data for the 6 phenotypes is loaded again, and merged into one data object. T.tests are performed across every genotype group, within each temperature, within each sex category, and within each phenotype. Summary statistics of each test are aggregated, and then multi-testing correction is applied. For phenotype-environment conditions where modeling revealed a significant effect, t.test summary statistics are manually observed and incorporated into the manuscript. 
Script 12. Inputs dgrpreftable.gz  & foraging.csv
This script loads in the foraging data from Lee et al., 2017, as well as the inversion genotype data for the DGRP lines, to append the inversion status to the Lee dataset. Then, the script checks if In(2L)t presence alters the foraging behavior observed, and reports the results
Script 13-15. Inputs, figuredataFINAL & positionsFINAL
Creatures the publications figures. Loads in the individual level phenotype data, and creates one layer a) that finds the mean and 95% confidence intervals for the given phenotype across sex, genotype, and temperature groups, and a second layer b) that shows the raw phenotype data. Both layers are graphed together, to illustrate differences in phenotype across experimental groups, as well as distributions within the raw data. Significant differences are marked by the inclusion of brackets, with asterixis often added in later by image editing. 
<img width="468" height="630" alt="image" src="https://github.com/user-attachments/assets/e9fff1ae-ab4f-4951-b96d-743ddb7b281d" />
