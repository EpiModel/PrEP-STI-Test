## STI Screening during PrEP Care among Men who Have Sex with Men in the United States

-------

This repository contains the analysis scripts and tables for the following study:

> [insert citation]

<img src="https://https://github.com/EpiModel/PrEP-STI-Test/tree/updates_branch/Figures/Figure_1.png">


### Abstract

-------

#### Background

The U.S. Centers for Disease Control and Prevention (CDC) recommends comprehensive sexually transmitted infection (STI) screening for men who have sex with men (MSM) using HIV preexposure prophylaxis (PrEP) every 3–6 months. The regional gaps between clinical practice and these recommendations have not been quantified.

#### Methods

We collected data between 2017 and 2019 on STI screening among MSM across the U.S., stratified by current, non-current, and never PrEP use. Multivariable models were used to estimate predictors of exposure site-specific STI screening during PrEP care, including residence in “Deep South” states of the Southeastern U.S.

#### Results

Of 3259 HIV-negative MSM, 19% were currently using PrEP, 6% had used PrEP in the past, and 75% had never used PrEP (Table 1).  Among ever PrEP users, 87%, 78%, 57%, and 64% were consistently (always or sometimes) screened for STIs by blood sample, urine sample/urethral swab, rectal swab, or pharyngeal swab, respectively, during PrEP care (Table 2). PrEP users in the Deep South were significantly less likely to be consistently screened for urogenital [adjusted odds ratio (aOR): 0.51, 95% confidence interval (CI): 0.32, 0.82] and rectal STIs (aOR: 0.51, 95% CI: 0.33, 0.79) during PrEP care (Table 5).

#### Conclusions

We found major gaps between CDC recommendations for STI screening during PrEP care and current clinical practice, particularly for rectal and pharyngeal exposure sites that harbor asymptomatic infections and for MSM in Deep South states where the STI burden is substantial.

### Data

------

We used data from ARTnet - an anonymous cross-sectional online survey of HIV-related risk behaviors, testing, and use of prevention services among MSM in the United States. MSM were recruited from the American Mens’ Internet Survey (AMIS) Survey, so the dataset also includes variables from AMIS.

Additional documentation on ARTnet and information to accessing the data can be found [here](https://github.com/EpiModel/ARTnetData). Code to install the “ARTnetData” package can be found below, but it may require a [Github Personal Access Token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line) since it is a private repository.

```r
install.packages("remotes")
remotes::install_github("EpiModel/ARTnetData")
```

### Code Organization

------

The analysis script `analysis.R` is the script used for analysis. This includes code used to prepare the ARTnet data for analysis, exploratory analyses, and code for bivariable and multivariable regression models. 