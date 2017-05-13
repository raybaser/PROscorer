
<!-- README.md is generated from README.Rmd. Please edit that file -->
PROscorer README
================

Overview
--------

**PROscorer** provides functions to accurately and reliably score commonly used patient-reported outcome (PRO) measures, quality of life (QoL) questionnaires, and other psychometric and psychological instruments. It is intended to be an extensible repository of up-to-date, well-documented, and open-source scoring functions for these types of instruments (which I will collectively and somewhat imprecisely refer to as "PRO measures" from here forward).

Each function in the PROscorer package scores a different PRO measure. Functions are named using the initials of the PRO measure. For example, the `fsfi` function scores the Female Sexual Function Index (FSFI).

PROscorer also comes with a vignette containing detailed descriptions of each of the instruments scored by PROscorer (see [PROscorer Instrument Descriptions](https://cran.r-project.org/web/packages/PROscorer/vignettes/instrument-descriptions.html)). The purpose of including these instrument descriptions, complete with references, is to help improve the descriptions of PRO measures in protocols, grants, and published results PROscorer. In most cases, the descriptions can be used in research documents with little or no editing.

To minimize the possibility of scoring errors and other bugs, each PROscorer function is composed of simpler, well-tested "helper" functions from the [PROscorerTools](https://CRAN.R-project.org/package=PROscorerTools) package. This reliance on a small set of simple functions that have been thoroughly tested ensures that the underlying code base of 'PROscorer' functions is bug-free, and that the scoring functions produce reliable, consistent, and accurate results.

PROscorer, together with the PROscorerTools package, is a system to facilitate the incorporation of PRO measures into research studies and clinical settings in a scientifically rigorous and reproducible manner. The overarching goals of the PROscorer and PROscorerTools packages are to draw attention to PRO scoring and reporting best-practices and to help eliminate inaccurate and inconsistent scoring.

The current version is still somewhat developmental, since the formal unit testing system is still immature for some functions, and not yet in place for others. Please use with caution at this time, and feel free to contact me with questions or suggestions. More scoring functions are currently in development and will be added in future updates, including more functions for the EORTC family of PRO measures.

Installation and Usage
----------------------

Install the stable version of PROscorer from CRAN:

``` r
install.packages("PROscorer")
```

Load PROscorer into your R workspace with the following:

``` r
library(PROscorer)
```

As an example, we will use the `makeFakeData` function from the PROscorerTools package to make fake item responses to the EORTC QLQ-C30 quality of life questionnaire. The created data set (named "dat") has an "id" variable, plus responses to 30 items (named "q1", "q2", etc.) from 20 imaginary respondents. There are also missing responses ("NA") scattered throughout.

``` r
dat <- PROscorerTools::makeFakeData(n = 20, nitems = 30, values = 1:4, id = TRUE)
dat
```

Below we will use the `qlq_c30` function to score the fake responses in "dat". We will save the scores from the EORTC QLQ-C30 questionnaire in a data frame named "c30scores".

``` r
c30scores <- qlq_c30(dat, 'q')
c30scores
```

The first argument to `qlq_c30` took our data frame, "dat". With the second argument, we needed to tell the `qlq_c30` function how to find our items in "dat". Since our items are all named with the prefix "q" plus the item number, we gave this quoted prefix to the second argument. These arguments actually have names, but in most cases you don't have to explicitly use the names. Below gives the same results, but explicitly uses the argument names.

``` r
c30scores <- qlq_c30(df = dat, iprefix = 'q')
c30scores
```

Specifically, the first argument is named `df` (for **d**ata **f**rame) and the second is named `iprefix` (for **i**tem prefix).

If you want to merge your scores back into your main data frame with the item responses, there are several different ways to do so. For example, assuming you have not changed the order of `dat` or `dat_scored`, you can do the following:

``` r
dat_scored <- data.frame(dat, c30scores)
dat_scored
```

For more information on the `qlq_c30` function, you can access its "help" page by typing `?qlq_c30` into R.

Resources for More Information
------------------------------

-   You can access the "help" page for "PROscorer" package by typing `?PROscorer` into R.

-   For more detailed information on 'PROscorer', including the problems it is intended to address, its design philosophy, and planned future updates, please see the [Introduction to PROscorer vignette](https://cran.r-project.org/web/packages/PROscorer/vignettes/intro-to-PROscorer.html).

-   You might find the [other PROscorer vignettes](https://CRAN.R-project.org/package=PROscorer) helpful, too. You can access them from the [main PROscorer webpage on CRAN](https://CRAN.R-project.org/package=PROscorer).

-   The underlying code base of the [PROscorer](https://CRAN.R-project.org/package=PROscorer) package is built from functions from the [PROscorerTools package](https://CRAN.R-project.org/package=PROscorerTools).

-   If you need to score PRO measures from the the FACT (Functional Assessment of Cancer Therapy) and FACIT (Functional Assessment of Chronic Illness Therapy) family of measures, please see the [FACTscorer package](https://CRAN.R-project.org/package=FACTscorer).
