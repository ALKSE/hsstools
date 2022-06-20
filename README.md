
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hsstools

<!-- badges: start -->
<!-- badges: end -->

This R package contains a collection of functions to use during the data
wrangling / analysis process for the Human Security Survey. Its initial
purpose was to expedite the creation of formatted frequency /
contingency tables, and the largest part of the functions serve this
purpose. However, a number of additional functions exist at the moment
to automate some steps earlier in the data cleaning process.

The package provides some functionality similar to the existing
koboloadeR package, but in a more simplified manner and tailored more
specifically to the current HSS data cleaning/analysis process. It might
be worth checking the KoboloadeR documentation to see what it has to
offer.

Most functions are made to work alongside the current Stata-based data
cleaning and -analysis process.

## Installation

You can install the hsstools package from [GitHub](https://github.com/)
with:

<!-- change github directory to pax-owned account -->

``` r
# install.packages("devtools")
devtools::install_github("RenRMT/hsstools")
```

## Usage

The core of the data table script consists of three groups of functions:

-   `hss_table_*()` : to create a contingency table with all required
    data for select-one or select-multiple questions.
-   `hss_format_*()` : to apply the desired formatting for either
    select-one or select-multiple tables.
-   `hss_label()` : to apply question labels & response labels.

A contingency table can be created for a specified question and
cross-variable using the `hss_table_*()` functions. Afterward, the
appropriate formatting and labeling can be applied with the
`hss_label()` and `hss_format_*()` functions.

These three functions can be used individually, but they can also be
applied to a list of questions. You can use the `hss_write_*()` and
`hss_export_*()` functions to write an entire list of tables to a CSV or
DOCX file. To create a list of question/variable names to write, use the
`hss_create_question_list()` function. `hss_write_*()` will call the
table, formatting and labelling functions for each question in the
question list.

## Example workflow using the hsstools package.

An example workflow using the hsstools package to write CSV and
(formatted) DOCX files for all survey questions contained in an HSS
dataset.

``` r
# -------------------------------------------------------------------------
# library(hsstools)
library(devtools)
load_all()

df_path <- "path/to/dataset"
dict_path <- "path/to/xlsform"

# load the dataframe containing the survey questions
df <- haven::read_dta(df_path)
# create dictionary objects for questions/variables and responses/values based on
# the XLS form for the survey.
dict_var <- hss_create_dict(dict_path, type = "var")
dict_val <- hss_create_dict(dict_path, type = "val")
# create a question list based on the XLS form.
questions <- hss_create_question_list(dict_path)


# Excel output ------------------------------------------------------------
# create a list of tables based on the specified dataframe, using the question 
# names from the 'questions' object. Set the disaggregation variable with group, 
# and percentages with percent. set the amount of (significant) digits to show 
# for percentages.
table_list <- hss_write_tables(
  df,
  questions,
  group = "gender",
  percent = TRUE,
  digits = 3
)

# export the generated list of tables to the specified path.
hss_export_tables(table_list, "file/to/save.csv")

# Formatted docx output ---------------------------------------------------
# create a list of tables based on the specified dataframe, using the question 
# names from the 'questions' object. Set the disaggregation variable with group, 
# and percentages with percent. set the amount of (significant) digits to show 
# for pecentages, and the language for the questionand response labels.
formatted_list <- hss_write_formatted(
  df,
  questions,
  group = "gender",
  percent = TRUE,
  digits = 1,
  lang = "en"
)

# export the generated list of tables to the specified path.
hss_export_formatted(formatted_list, "file/to/save.docx")
```

## See also

For an introduction to functions related to creating and exporting
contingency tables see vignette(“generating-tables”). For an
introduction to functions related to data cleaning and wrangling see
vignette(“data-cleaning”).

## Depedencies

The hsstools package relies on a number of other packages. These package
dependencies will be downloaded and installed automatically with the
hsstools package. Additional packages installed are:

    #> dplyr,
    #> flextable,
    #> forcats,
    #> lubridate,
    #> magrittr,
    #> officer,
    #> purrr,
    #> questionr,
    #> stringr,
    #> tidyr
