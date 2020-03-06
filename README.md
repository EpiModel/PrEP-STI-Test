# STI Screening during PrEP Care among Men who Have Sex with Men in the United States

This repository contains the analysis scripts and tables for the following study:

> [insert citation]

<img src="https://github.com/EpiModel/PrEP-STI-Test/blob/updates_branch/Figures/Figure_1.png">

## Abstract

### Background

The U.S. Centers for Disease Control and Prevention (CDC) recommends comprehensive sexually transmitted infection (STI) screening every 3–6 months for men who have sex with men (MSM) using HIV preexposure prophylaxis (PrEP). The gaps between clinical practice and these recommendations by region have not been quantified.

### Methods

We collected data between 2017 and 2019 on STI screening among MSM across the U.S., stratified by current, prior, and never PrEP use. Poisson regression models with robust error variance were used to assess factors, including residence in the Southeastern U.S., associated with exposure site-specific STI screening during PrEP care.

### Results

Of 3259 HIV-negative MSM, 19% were currently using PrEP, 6% had used PrEP in the past, and 75% had never used PrEP ([Table 1](https://github.com/EpiModel/PrEP-STI-Test/blob/updates_branch/Figures/Table_1.pdf)). Among ever PrEP users, 87%, 78%, 57%, and 64% were consistently (always or sometimes) screened for STIs by blood sample, urine sample/urethral swab, rectal swab, or pharyngeal swab, respectively, during PrEP care ([Table 2](https://github.com/EpiModel/PrEP-STI-Test/blob/updates_branch/Figures/Table_2.pdf)). PrEP users in the Southeast were significantly less likely to be consistently screened for urogenital [adjusted prevalence ratio (aPR): 0.86, 95% confidence interval (CI): 0.76, 0.98] and rectal STIs (aPR: 0.76, 95% CI: 0.62, 0.93) during PrEP care ([Table 5](https://github.com/EpiModel/PrEP-STI-Test/blob/updates_branch/Figures/Table_5.pdf)).

### Conclusions

Significant gaps exist between CDC recommendations for STI screening during PrEP care and current clinical practice, particularly for rectal and pharyngeal exposure sites that can harbor asymptomatic infections and for MSM in Southeastern states where the STI burden is substantial.

## Data

We used data from ARTnet - an anonymous cross-sectional online survey of HIV-related risk behaviors, testing, and use of prevention services among MSM in the United States. MSM were recruited from the American Mens’ Internet Survey (AMIS) Survey, so the dataset also includes variables from AMIS.

Additional documentation on ARTnet and information to accessing the data can be found [here](https://github.com/EpiModel/ARTnetData). Code to install the “ARTnetData” package can be found below, but it may require a [Github Personal Access Token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line) since it is a private repository.

```r
install.packages("remotes")
remotes::install_github("EpiModel/ARTnetData")
```

## Code Organization

The analysis script `analysis.R` is the script used for analysis. This includes code used to prepare the ARTnet data for analysis, exploratory analyses, and code for bivariable and multivariable regression models. 