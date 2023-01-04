#' Generate & Evaluate Embedding
#' 
#' \code{get_eval_embed} acts as embedding generation & evaluation from co-occurrence data file.
#' It returns a list of summary including meta-data, evaluation and embedding itself. 
#' 
#' @param CO_file Co-occurrence data file with format \code{.csv}, \code{.parquet} or \code{.Rdata}.
#' The data should be a table with 3 columns index1, index2, count:
#' \itemize{
#' \item{\code{index1}}: Shows the index of code1.
#' \item{\code{index2}}: Shows the col index of code2.
#' \item{\code{count}}: Shows the counts for certain pair.
#' }
#' @param freq_file Frequency count file with format \code{.csv}, \code{.parquet} or \code{.Rdata}.
#' The data should be a table with 4 columns index, code, description, freq_count:
#' \itemize{
#' \item{\code{index}}: Shows the index of code.
#' \item{\code{code}}: Shows the name of code.
#' \item{\code{description}}: Shows the description text of code.
#' \item{\code{freq_count}}: Shows the frequency count of code.
#' }
#' @param dims A vector of numeric values for dimension, by default is \code{seq(100, 1000, 100)}.
#' @param out_dir Output folder, if \code{NULL} then by default set to your_working_directory/output.
#' @param freq_min The frequency counts cutoff for code filtering. If the counts are less than \code{freq_min}, it’ll be filtered
#' out. By default is \code{1000}.
#' @param threshold Integer number, the threshold to get SPPMI matrix, by default is \code{10}.
#' @param normalize \code{TRUE} or \code{FALSE}, to normalize embedding or not. By default is \code{True}.
#' @return A list of information of meta-data, embedding & evaluation result. It will 
#' be saved in \code{out_dir} as \code{.Rdata} file. 
#' 
#' @export
get_eval_embed <- function(CO_file, 
                           freq_file,
                           dims = seq(100, 1000, 100),
                           out_dir = NULL,
                           freq_min = 1000,
                           threshold = 10,
                           normalize = TRUE) {
      
  
  # Get Summary
  ################################################################################
  CO_file <- "D:\\Documents\\Projects\\harvard\\keser-i2b2\\dungeon\\data\\AD_cooccurence_result_1019.csv"
  freq_file <- "D:\\Documents\\Projects\\harvard\\keser-i2b2\\dungeon\\data\\AD_freq_file.csv"
  
  
  # Set Up Output Folder
  out_dir <- ifelse(is.null(out_dir), file.path(getwd(), "output"), path_chk(out_dir))
  dir.create(out_dir, showWarnings = FALSE)
  cat(paste0("\nOutput Folder: ", out_dir))
  
  # Load Data
  cat("\nLoading data...")
  CO_idx <- read_file(path_chk(CO_file))
  freq <- read_file(path_chk(freq_file))
  
  # Check Input Files
  if (any(colnames(CO_idx) != c("index1", "index2", "count"))) {
    stop("Invalid Column names for CO_file, should be: index1, index2, count")
  }
  if (any(colnames(freq) != c("index", "code", "description", "freq_count"))) {
    stop("Invalid Column names for CO_file, should be: index1, code, description, freq_count")
  }
  
  # Map CO from index to codes
  cat("\nMapping CO codes...")
  CO <- map_CO(CO_idx, freq)

  # Change CO Column Names
  colnames(CO) <- c("V1", "V2", "V3")
  
  # Check CO data
  cat("\nChecking CO data...")
  CO <- chk_CO(CO)
  
  # Clear Codes (Remove codes less than a certain frequency)
  cat("\nClearing codes ...")
  CO <- clear_CO(CO, freq, freq_min)
  
  # Check Memory
  cat("\nMemory check...\n")
  memory_chk(CO)
  
  # Get Time
  start_t <- Sys.time()
  
  # Filter Dimensions
  dims <- dims[which(dims <= DIM_MAX)]
  max_dim <- max(dims)
  
  # Run
  cat("\n-------------------------------------------------------------------------\n")
  cat("\n")
  cat(paste0("Dimensions Setting:\n", paste(dims, collapse=", "), "  (Dimension greater than ",  DIM_MAX, " will be ignore)"))
  cat(paste0("\n\nNote: It may take minutes to run depending on data size and your hardware.\n"))
  
  # Generate SPPMI & SVD from cooc
  #########################################################################
  
  # Get Unique Values for First two Columns of CO
  CO_unique <- unique(c(CO$V1, CO$V2))
  
  # Obtain The Roll Up Dictionary For LOINC Codes:
  # cat("\nGetting rollup dict...\n")
  # code_LPcode <- get_rollup_dict(CO_unique, MAH)   # No needed
  
  # Calculate SPPMI
  cat("\nCalculating SPPMI...")
  t <- Sys.time()
  SPPMI <- getSPPMI(CO, data.frame(feature_id = CO_unique), threshold = threshold)
  t_cost <- round(as.numeric(difftime(Sys.time(), t, units = "mins")), 2)
  cat(paste0("\n(", t_cost, " mins)\n"))
  
  cat("\nCalculating SVD...")
  t <- Sys.time()
  SVD <- getSVD(SPPMI, dim = max_dim + 1000)
  t_cost <- round(as.numeric(difftime(Sys.time(), t, units = "mins")), 2)
  cat(paste0("\n(", t_cost, " mins)\n"))
  #########################################################################
  
  # Embedding Generation & Calculation
  n_dims <- length(dims)
  summary <- lapply(1:n_dims, function(i) {
    
    dim <- dims[i]
    cat("-----------------------------------------------------------------------\n")
    cat(paste0("[", i, "/", n_dims, "] ", "Calculating for Dimension: ", dim))
    
    # Get Time Per Dim
    start_sub_t <- Sys.time()
    
    # Get Embedding
    #########################################################################
    cat("\nGetting embedding...")
    embed = get_embed(SVD, dim, normalize = normalize)
    #########################################################################
    
    
    # Embed Generation Evaluation
    #########################################################################
    # Evaluate Embedding
    cat("\nEvaluating...")
    ans = Evaluate_codi(embed, AllRelationPairs = ARP, normalize = !normalize)  # codi only, if embedding normalized, evaluation should not be normalized again.

    #########################################################################
    
    
    # Get Time Cost Per Dim
    end_sub_t <- Sys.time()
    time_cost <- round(as.numeric(difftime(end_sub_t, start_sub_t, units = "mins")), 2)
    cat(paste0("\nTime cost: ", time_cost, " mins."))
    cat("\n-----------------------------------------------------------------------\n")
    
    return(list("evaluation"=ans, "embedding"=embed))
  })
  
  # Get Time Cost
  end_t <- Sys.time()
  time_cost <- round(as.numeric(difftime(end_t, start_t, units = "mins")), 2)
  cat(paste0("\nFinished - Total Time cost: ", time_cost, " mins."))
  cat("\n-------------------------------------------------------------------------\n")
  
  
  # Save Summary Data
  dim_str <- strsplit(paste(dims, collapse = ","), ",")[[1]]
  names(summary) <- dim_str
  summary_file <- paste0("summary-", dims[1], "-", dims[length(dims)], 
                         "-", dims[2]-dims[1],'.Rdata')
  summary <- list("meta_data" = list("CO_file" = CO_file,
                                 "dims" = dims,
                                 "out_dir" = out_dir,
                                 "summary_file" = summary_file),
                  "summary" = summary)
  save(summary, file = file.path(out_dir, summary_file))
  return(summary)
  ################################################################################
}


#' Generate Plot Report
#' 
#' \code{get_report} acts as plot report generation from summary returned by \code{get_eval_embed}.
#' It returns \code{NULL} but generates plots in \code{out_dir}, classified by similarity & relation.
#' Report format can be chosen in parameter \code{knit_format}.
#' 
#' @param summary A list of summary infomation generated by \code{get_eval_embed}.
#' @param plot_val Metric for evaluation (Columns in summary evaluation dataframes). 
#' Options could be: #"pairs", "auc", "cut/0.01", "cut/0.05", "cut/0.1", "TPR/0.01", "TPR/0.05" or "TPR/0.1".
#' @param knit_format Format of plot report, "pdf" or "html".
#' @param labels Labels to be used in plots, options are the row names of summary evaluation dataframes.
#' @param split_patterns Pattern to split lablels, normally not changed.
#' @export
get_report <- function(summary,
                       plot_val = "auc",
                       knit_format = "html",
                       split_patterns = list("Similarity" = "(sim)", 
                                              "Relation" = "(rela)")) {
  
  # Param Check
  vals <- c("pairs", "auc", "cut/0.01", "cut/0.05", "cut/0.1", "TPR/0.01", "TPR/0.05", "TPR/0.1")
  if (!(plot_val %in% vals)) stop(paste0("Invalid plot_val: ", plot_val, " \nValid values: ", paste(vals, collapse = ' ')))
  
  # Rmd Files
  rmd_file <- list("html" = "summary_html.Rmd",
                   "pdf" =  "summary_pdf.Rmd") 
  
  # Other Variables
  summary_file <- summary[["meta_data"]][["summary_file"]]
  CO_file <- summary[["meta_data"]][["CO_file"]]
  out_dir <- summary[["meta_data"]][["out_dir"]]
  
  # Generate Output file
  dims <- summary[["meta_data"]][["dims"]]
  output_file <- paste0("summary-", dims[1], "-", dims[length(dims)],
                        "-", dims[2]-dims[1], '.', knit_format)
  rmd_file <- system.file("rmd", rmd_file[[knit_format]], package = PKG.NAME)
  out_file <- file.path(out_dir, output_file)
  rmarkdown::render(rmd_file, output_file = out_file)
  cat("\nOutput sumamry file saved as: ", out_file, "\n")
}


#' Get Best Embedding from Summary 
#' 
#' @param summary the summary object from output of function \code{get_eval_embed}.
#' @param metric Metric for evaluation (Columns in summary evaluation dataframes). 
#' Options could be: #"pairs", "auc", "cut/0.01", "cut/0.05", "cut/0.1", "TPR/0.01", "TPR/0.05" or "TPR/0.1".
#' By default is auc.
#' @param by The evaluation object, options can be seems in plot summary file (the legend). By default is \code{weighted.rela}.
#' 
#' @export
get_best_dim <- function(summary,
                         metric = "auc",
                         by = "weighted.rela") {
  
  best_dim <- get_plot_data(summary, col = metric) %>% 
    dplyr::arrange(dplyr::desc(!!rlang::sym(by))) %>%
    dplyr::filter(dplyr::row_number() == 1) %>% dplyr::pull(dims) %>% min()
  
  best_embed <- summary[["summary"]][[as.character(best_dim)]][["embedding"]]
  return(list("dim" = best_dim, "embedding" = best_embed))
}
 





