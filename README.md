
<!-- README.md is generated from README.Rmd. Please edit that file -->

# keser-i2b2

This is a package to:

- Generate Embedding

- Evaluate Embedding

- Get Evaluation Plots

## Installation

You can install the development version from GitHub with:

``` r
install.packages("remotes")
remotes::install_github("celehs/KESER-i2b2")
library(KESER.i2b2)
```

If you can’t install it, alternative opntion is to download the repo and
use `devtools::load_all()` to load the package.

``` r
install.packages("devtools")
setwd("D:\\Projects\\harvard\\keser-i2b2")     # Set to your path
devtools::load_all()    
```

## Usage

Make sure you have the following files available:

- Co-occurrence matrix file, such as
  `rpdr_code_cooccurrence_victor_2019.csv`

- Co-occurrence matrix dict file (optional), such as
  `UPMC_AD_wordindex_mapping_datainfo.csv`

**Note:** The format of files can be `.csv`, `.parquet` or `.Rdata`.

Then, You can tune the dimensions on variable `dims`.

### Set up Parameters

For the file paths, change it base on your file locations.

``` r
CO_file <- "dungeon//data//AD_cooccurence_result_1019.parquet"  # Co-occurrence File: .csv/.parquet/.Rdata
dims <- seq(200, 1000, 200)                                         # Dimension Setting
out_dir <- NULL                                                     # Output folder setting -  If NULL All Outputs Will Be At: working_dir/output
```

### Generate Embedding & Evaluation

If your Co-occurrence matrix file has code pairs, a dictionary is not
needed:

``` r
summary <- get_eval_embed(CO_file, dims, out_dir)
```

Otherwise, if there’s no code pairs but index, a dictionary
`CO_dict_file` is needed:

``` r
summary <- get_eval_embed(CO_file, dims, out_dir, CO_dict_file = "dungeon//data//UPMC_AD_wordindex_mapping_datainfo.csv")    # Replace your dict file here 
```

The output of `get_eval_embed` is a list includes meta-data, embedding &
evaluation results. The list will also be saved as `.Rdata`. It may take
hours to run depending on your data size and hardware.

#### Generate Plot Report

``` r
get_report(summary, plot_val = "auc", knit_format = "html")
```

`get_report` will not return anything but creates the plot report under
`out_dir` folder. If `out_dir` is `NULL` then it will create a folder
called **output** in your working directly.

#### Retrieve Embedding

Let’s say you picked the best `dim` from the plots, and the value is
**200**. the way the retrieve it is:

``` r
my_embed <- summary[["summary"]][["200"]][["embedding"]]
View(my_embed)
```

#### Retrieve Evaluation Table

For the same case above, the way to retrieved evaluation table is:

``` r
my_eval <- summary[["summary"]][["200"]][["evaluation"]]
View(my_eval)
```

#### Check Results

The evaluation plots are saved as “Summary-XX-YY-ZZ.html”, and the
original summary data are saved as “Summary-XX-YY-ZZ.Rdata”, where XX
stands for starting dimension, YY stands for ending dimension and ZZ
stands for Step.

You can choose a proper dimension and use the corresponding embedding
for the next steps.

See the [Documentation](https://celehs.github.io/KESER-i2b2/) to learn
more.
