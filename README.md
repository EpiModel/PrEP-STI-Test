# HIV and STI Screening among Men who Have Sex with Men in the United States

This analysis evaluates self-reported STI screening among men who have sex with men (MSM) who:

1.  currently use pre-exposure prophylaxis (PrEP),
2.  have used PrEP but currently do not, and
3.  have never used PrEP.

We investigate whether PrEP use leads to increased HIV and all-site STI testing, and if there are differences by geography, demographics, or risk behavior. Among MSM who are currently using PrEP, we assess providers’ adherence to Centers for Disease Control and Prevention Guidelines for PrEP, which recommends testing for asymptomatic STIs every 3-6 months through the collection of pharyngeal, rectal, and urine specimens.

## Data

We use data from ARTnet - an anonymous cross-sectional online survey of HIV-related risk behaviors, testing, and use of prevention services among MSM in the United States. MSM were recruited from the American Mens’ Internet Survey (AMIS) Survey, so the dataset also includes variables from AMIS.

Additional documentation on ARTnet and information to accessing the data can be found [here](https://github.com/EpiModel/ARTnetData). Code to install the “ARTnetData” package can be found below, but it may require a [Github Personal Access Token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line) since it is a private repository.

```r
install.packages("remotes")
remotes::install_github("EpiModel/ARTnetData")
```