
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hsstools

<!-- badges: start -->
<!-- badges: end -->

The hsstools R package contains a collection of functions to use during
the data wrangling / analysis process for the Human Security Survey
(Iraq + South Sudan). Its initial purpose was to expedite the creation
of formatted contingency tables, and the largest part of the functions
serve this purpose. However, a number of additional functions have been
added with the purpose of automating the analysis process. These
functions broadly fall under the umbrella of data cleaning, data
auditing, and data formatting.

The package borrows a couple of ideas from the existing koboloadeR
package, but most things are implemented in a more simplified manner and
tailored more specifically to the current HSS data cleaning/analysis
process. It might be worth checking the [KoboloadeR
documentation](https://unhcr.github.io/koboloadeR/docs/) to see what it
has to offer.

Most functions are also made to work alongside the now obsolete
Stata-based data cleaning and -analysis process.

## Installation

You can install the hsstools package from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("ALKS/hsstools")
```

## Purpose of scripts & tools

The functions in this package are made to automate or expedite a number
of time-consuming steps in the data cleaning and -wrangling process.

The package currently offers functions for the following steps:

-   Loading encrypted (Iraq) data;
-   Splitting ‘select multiple’ questions
-   Updating variable and value labels;
-   Recoding ‘select other’ answer options;
-   Calculating survey durations;
-   Automatic translation of Arabic text;
-   Creating contingency tables for survey questions;
-   Applying English or Arabic text labels to questions + response
    options;
-   Exporting contingency tables as .csv or formatted .docx file;
-   Validating the accuracy/precision of data (audit);

## The package structure

Since this started out as a collection of functions to solve a number of
unrelated issues, structuring it as a package was a convenient way that
all work remained together. Additionally the package structure makes
sure that any dependencies are installed with it and that the package +
documentation is self-contained. All documentation for user-facing
functions can be accessed from within R using ?function_name.

## Depedencies

The hsstools package relies on a number of other packages. These package
dependencies will be downloaded and installed automatically with the
hsstools package. Additional packages installed are:

    #> dplyr,
    #> flextable,
    #> forcats,
    #> janitor,
    #> lubridate,
    #> magrittr,
    #> officer,
    #> purrr,
    #> questionr,
    #> stringr,
    #> tidyr,
    #> writexl,
    #> haven,
    #> Hmisc,
    #> readr,
    #> readxl
