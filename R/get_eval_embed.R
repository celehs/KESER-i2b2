#' Generate & Evaluate Embedding
#' 
#' \code{get_eval_embed} acts as embedding generation & evaluation from co-occurrence data file.
#' It returns a list of summary including meta-data, evaluation and embedding itself. 
#' 
#' @param CO_file Co-ccurrence data file with format \code{.csv}, \code{.parquet} or \code{.Rdata}.
#' The data should be a table with 3 columns V1, V2, V3:
#' \itemize{
#' \item{\code{V1}}: Shows the row id (code id) or row code pair.
#' \item{\code{V2}}: Shows the col id (code id) or row code pair..
#' \item{\code{V3}}: Shows the counts for certain pair.
#' }
#' Note: If V1 & V2 are code ids other than code pairs, a mapping dict offering information 
#' from id to code pair is needed. Please see \code{CO_dict_file} for details.
#' @param ARP_file All relation pairs data file with format \code{.csv}, \code{.parquet} or \code{.Rdata}.
#' @param dims A vector of numeric values for dimension.
#' @param out_dir Output folder, if \code{NULL} then by default set to your_working_directory/output.
#' @param CO_dict_file A data file with two columns, where:
#' \itemize{
#' \item{\code{Column 1}: Shows the name of code pair.}
#' \item{\code{Column 2}: Shows the corresponding id number.}
#' }
#' File format can be \code{.csv}, \code{.parquet} or \code{.Rdata}.
#' @param HAM_file Multi-axial hierarchy data file with format \code{.csv}, \code{.parquet} or \code{.Rdata}.
#' By default it's set to \code{NULL} and read the default file within the package. 
#' If a file name is offered, it'll read it instead and replace the default file
#' @param data_type If data Does not contain CUI codes, set as \code{1}. Otherwise, set as \code{2}.
#' @return A list of infomation of meta-data, embedding & evaluation result. It will 
#' be saved in \code{out_dir} as \code{.Rdata} file. 
#' 
#' @export
get_eval_embed <- function(CO_file, 
                           ARP_file, 
                           dims,
                           CO_dict_file = NULL,
                           out_dir = NULL, 
                           HAM_file = NULL,
                           data_type = 1) {
      
  
  # Get Summary
  ################################################################################
  
  # Set Up Output Folder
  out_dir <- ifelse(is.null(out_dir), file.path(getwd(), "output"), out_dir)
  dir.create(out_dir, showWarnings = FALSE)
  
  # Load Data
  cat("Loading data...\n")
  CO <- read_file(CO_file)
  if (is.null(HAM_file)) MAH <- read_file(file.path("inst", "MAH.Rdata")) else MAH <- read_file(HAM_file)
  ARP <- read_file(ARP_file)
  
  # Check & Map CO
  if (class(CO[[1]]) == "integer") {
    if (is.na(CO_dict_file)) stop("Please provide CO dict.") else {
      
      # Map CO If CO Dict Passed
      CO_dict <- read_file(CO_dict_file)
      cat("\nMapping CO codes from CO dict...\n")
      CO <- map_CO(CO, CO_dict)
    }
  }
  
  # Change CO Column Names
  colnames(CO) <- c("V1", "V2", "V3")
  
  # Align Codes
  cat("Aligning codes...")
  CO <- CO %>% dplyr::mutate(across(c("V1", "V2"), stringr::str_replace, "-PCS", ""))  
  
  # Get Time
  start_t <- Sys.time()
  
  # Run
  cat("\n-------------------------------------------------------------------------\n")
  cat("\n")
  cat(paste0("Dimensions Setting:\n", paste(dims, collapse=", ")))
  cat(paste0("\n\nTotal Time Cost Estimation: ", round(length(dims) * 30 / 60, 2), " Hours\n"))
  
  # Generate SPPMI from cooc
  #########################################################################
  
  # Get Unique Values for First two Columns of CO
  CO_unique <- unique(c(CO$V1, CO$V2))
  
  # Obtain The Roll Up Dictionary For LOINC Codes:
  cat("\nGetting rollup dict...")
  code_LPcode = get_rollup_dict(CO_unique, MAH)
  
  # Calculate SPPMI
  cat("\nCalculating SPPMI...")
  SPPMI = getSPPMI(CO, data.frame(feature_id = CO_unique), code_LPcode)
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
    embed = getembedding(SPPMI, dim)
    #########################################################################
    
    
    # Embed Generation Evaluation
    #########################################################################
    # Evaluate Embedding
    cat("\nEvaluating...")
    if (data_type == 1) {
      ans = Evaluate_codi(embed, AllRelationPairs = ARP)  # data_type == 1: codi only
    } else if (data_type == 2) {
      ans = Evaluate(embed, AllRelationPairs = ARP)       # data_type == 2: codi & CUI
    } else stop("Invalid Value: 'data_type' should be 1 or 2.")
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
                                 "HAM_file" = HAM_file,
                                 "ARP_file" = ARP_file,
                                 "dims" = dims,
                                 "out_dir" = out_dir,
                                 "summary_file" = summary_file),
                  "summary" = summary)
  
  save(summary, file = file.path(out_dir, summary_file))
  return(summary)
  ################################################################################
}





# Get Plotting Output File
################################################################################

################################################################################
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
                       labels = c("PheCode-PheCode(sim)", "RXNORM-RXNORM(sim)", "LAB-LAB(sim)",
                                   "PheCode-PheCode(rela)", "PheCode-RXNORM(rela)",
                                   "PheCode-LAB(rela)"),
                       split_patterns = list("Similarity" = "(sim)", 
                                              "Relation" = "(rela)")) {
  
  # Rmd Files
  rmd_file <- list("html" = "summary_html.Rmd",
                   "pdf" =  "summary_pdf.Rmd") 
  
  # Set Up Output Folder
  out_dir <- summary[["meta_data"]][["out_dir"]]
  dir.create(out_dir, showWarnings = FALSE)
  
  # Other Variables
  summary_file <- summary[["meta_data"]][["summary_file"]]
  CO_file <- summary[["meta_data"]][["CO_file"]]
  HAM_file <- summary[["meta_data"]][["HAM_file"]]
  ARP_file <- summary[["meta_data"]][["ARP_file"]]
  
  # Generate Output file
  dims <- summary[["meta_data"]][["dims"]]
  output_file <- paste0("summary-", dims[1], "-", dims[length(dims)],
                        "-", dims[2]-dims[1], '.', knit_format)
  rmarkdown::render(file.path("Rmd", rmd_file[[knit_format]]), output_file = file.path(out_dir, output_file))
  cat("\nOutput sumamry file saved as: ", file.path(out_dir, output_file))
}
  
 





