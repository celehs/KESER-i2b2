
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

- Hierarchy file, such as `MultiAxialHierarchy.csv`

- Relation pairs file, such as `AllRelationPairsWithNull.Rdata`

- Input data file, such as `rpdr_code_cooccurrence_victor_2019.csv`

Then, You can tune the dimensions on varaible `dims`, each dimension may
take 26-30 mins to run.

### Set up Parameters

For the file paths, change it base on your file locations.

``` r
library(keser.i2b2)
CO_file <- "dungeon//data//rpdr_code_cooccurrence_victor_2019.csv"  # Co-occurrence File: .csv/.parquet/.Rdata
HAM_file <- "dungeon//data//MultiAxialHierarchy.csv"                # Multi-axial Hierarchy File: .csv/.parquet/.Rdata  
ARP_file <- "dungeon//data//AllRelationPairsWithNull.Rdata"         # All Relation Pairs File: .csv/.parquet/.Rdata  
dims <- seq(200, 1000, 200)                                         # Dimension Setting
out_dir <- NULL                                                     # Output folder setting -  If NULL All Outputs Will Be At: working_dir/output
data_type <- 1                                                      # Input Data Type Setting, Codi Only:1, Codi & CUI: 2 
```

### Generate Embedding & Evaluation

``` r
summary <- get_eval_embed(CO_file, HAM_file, ARP_file, dims, out_dir)
```

The output of `get_eval_embed` is a list includes meta-data, embedding &
evaluation results. The list will also be saved as `.Rdata`.

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
