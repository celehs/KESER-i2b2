
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

If you can’t install it, alternative option is to download the repo and
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

- Co-occurrence matrix code frequency counts file (optional), such as
  `UPMC_AD_code_freq.csv`

**Note:** The format of files can be `.csv`, `.parquet` or `.Rdata`.

### Input File Data Structure Examples

#### Co-occurrence matrix file (with codes):

    #> # A tibble: 6 x 3
    #>   i         j           count
    #>   <chr>     <chr>       <dbl>
    #> 1 CCS-PCS:1 CCS-PCS:1    6318
    #> 2 CCS-PCS:1 CCS-PCS:10     14
    #> 3 CCS-PCS:1 CCS-PCS:100    40
    #> 4 CCS-PCS:1 CCS-PCS:101     8
    #> 5 CCS-PCS:1 CCS-PCS:102    13
    #> 6 CCS-PCS:1 CCS-PCS:103    12

The first two columns are the names of code. The third column is the
count of the code pair.

#### Co-occurrence matrix file (with index):

    #>      i    j count
    #> 1 1856 3004     1
    #> 2  671 8332    10
    #> 3 4737 4967    79
    #> 4  572 9233     1
    #> 5 1438 5749     3
    #> 6 1165 9479     1

The first two columns are the location indexes of code. The third column
is the count of the code pair.

#### Co-occurrence matrix dict file:

    #> # A tibble: 6 x 2
    #>   code        WordIndex
    #>   <chr>           <dbl>
    #> 1 CCS-PCS:1           1
    #> 2 CCS-PCS:10          2
    #> 3 CCS-PCS:100         3
    #> 4 CCS-PCS:101         4
    #> 5 CCS-PCS:102         5
    #> 6 CCS-PCS:103         6

The first column is the name of code, the second column is the location
index. Only use when the input is **Co-occurrence matrix file with
index**.

#### Co-occurrence matrix code frequency counts file:

    #> # A tibble: 6 x 2
    #>   feature_id  feature_freq
    #>   <chr>              <dbl>
    #> 1 CCS-PCS:1           1577
    #> 2 CCS-PCS:10           759
    #> 3 CCS-PCS:100        10303
    #> 4 CCS-PCS:101         5978
    #> 5 CCS-PCS:102         5258
    #> 6 CCS-PCS:103         1517

The first column is the name of code, the second column is the frequency
counts.

### Set up Parameters

For the file paths, change it base on your file locations. You can tune
the dimensions on variable `dims`. The output files (`.Rdata` file &
report file) will be saved at `out_dir`. If `out_dir = NULL`, it will
create a folder called `output` and put it there.

``` r
CO_file <- "dungeon//data//AD_cooccurence_result_1019.parquet"  # Co-occurrence File: .csv/.parquet/.Rdata
dims <- seq(200, 1000, 200)                                         # Dimension Setting
out_dir <- NULL                                                     # Output folder setting -  If NULL All Outputs Will Be At: working_dir/output
```

### Generate Embedding & Evaluation

If your Co-occurrence matrix file has code pairs, a dictionary is not
needed:

``` r
summary <- get_eval_embed(CO_file = CO_file, 
                          dims = dims, 
                          out_dir = out_dir)
```

Otherwise, if there’s no code pairs but index, a dictionary
`CO_dict_file` is needed:

``` r
CO_dict_file <- "dungeon//data//UPMC_AD_wordindex_mapping_datainfo.csv"   # Replace your dict file here 
summary <- get_eval_embed(CO_file = CO_file, 
                          dims = dims, 
                          out_dir = out_dir, 
                          CO_dict_file = CO_dict_file)   
```

Moreover, to further reduce the time cost, a code frequency counts file
can be used to remove codes with low frequency. The default cutoff is
set to **1000**. To change the cutoff, please refer to documents of
function `get_eval_embed`.

``` r
freq_file <- "dungeon//data//UPMC_AD_code_freq.csv"
summary <- get_eval_embed(CO_file = CO_file, 
                          dims = dims, 
                          out_dir = out_dir, 
                          CO_dict_file = CO_dict_file, 
                          freq_file = freq_file)
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

#### Summary Object

The summary object includes `meta_data` and `summary`:

``` r
print(names(summary))
```

`summary$meta_data` has the meta information. Inside the
`summary$summary`, the next level is dimension value. Inside each
dimension, there are `evaluation` and `embedding`.

``` r
print(summary$meta_data)
print(names(summary$summary$`100`))
```

Let’s say you picked want to pick `dim=100` from the summary. this is
the way to retrieve it:

``` r
my_embed <- summary$summary$`100`$embedding
my_evaluation <- summary$summary$`100`$evaluation
View(my_embed)
View(my_evaluation)
```

#### Retrieve Best Embedding

By default the best embedding is picking from the best `roc` with
weighted relative (`weighted.rela`). You can change the setting in the
function, please refer to the function documentation for more info.

``` r
best <- get_best_dim(summary)
best_dim <- best$dim
best_embed <- best$embedding
View(best_embed)
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
