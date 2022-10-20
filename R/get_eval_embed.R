
# Parameter Setting
################################################################################
# CO_file <- "dungeon//rpdr_code_cooccurrence_victor_2019.csv"  # Co-occurrence File: .csv/.parquet/.Rdata
# HAM_file <- "dungeon//MultiAxialHierarchy.csv"                # Multi-axial Hierarchy File: .csv/.parquet/.Rdata  
# ARP_file <- "dungeon//AllRelationPairsWithNull.Rdata"         # All Relation Pairs File: .csv/.parquet/.Rdata  
# dims <- seq(100, 200, 100)                           # Dimension Setting
# out_dir <- NULL                                      # Output folder setting -  If NULL All Outputs Will Be At: working_dir/output
# data_type <- 1                                       # Input Data Type Setting, Codi Only:1, Codi & CUI: 2 
# 
# 
# func_file <- "get_eval_embed_funcs.R"                # Functions File
################################################################################
get_eval_embed <- function(CO_file, 
                           HAM_file, 
                           ARP_file, 
                           dims,
                           out_dir = NULL, 
                           data_type = 1) {
      
  
  # Get Summary
  ################################################################################
  
  # Set Up Output Folder
  out_dir <- ifelse(is.null(out_dir), file.path(getwd(), "output"), out_dir)
  dir.create(out_dir, showWarnings = FALSE)
  
  # Load Data
  cat("Loading data...")
  CO <- read_file(CO_file)
  MAH <- read_file(HAM_file)
  ARP <- read_file(ARP_file)
  
  
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
  n_dims <- length(dims)
  summary <- lapply(1:n_dims, function(i) {
    
    dim <- dims[i]
    cat("-----------------------------------------------------------------------\n")
    cat(paste0("[", i, "/", n_dims, "] ", "Calculating for Dimension: ", dim))
    
    # Get Time Per Dim
    start_sub_t <- Sys.time()
    
    # Generate embedding from cooc
    #########################################################################
    # Get Unique Values for First two Columns of CO
    CO_unique <- unique(c(CO$V1, CO$V2))
    
    # Obtain The Roll Up Dictionary For LOINC Codes:
    cat("\nGetting rollup dict...")
    code_LPcode = get_rollup_dict(CO_unique, MAH)
    
    # Calculate SPPMI
    cat("\nCalculating SPPMI...")
    SPPMI = getSPPMI(CO, data.frame(feature_id = CO_unique), code_LPcode)
    
    # Get Embedding
    cat("\nGetting embedding...")
    embed = getembedding(SPPMI, dim)
    #########################################################################
    
    
    # Embed Generation Evaluation
    #########################################################################
    # Evaluate Embedding
    cat("\nEvaluating...")
    if (data_type == 1) {
      ans = Evaluate_tmp(embed, AllRelationPairs = ARP)
    } else if (data_type == 2) {
      ans = Evaluate(embed, AllRelationPairs = ARP)
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
# plot_val <- "auc"                                    # Metric for Evaluation (Columns in Summary.Rds Dataframes), Could be: #pairs, auc, cut/0.01, cut/0.05, cut/0.1, TPR/0.01, TPR/0.05, TPR/0.1
# knit_format <- "html"                                # Format of Plotting File - Should be "html" or "pdf"
# labels <- c("PheCode-PheCode(sim)", "RXNORM-RXNORM(sim)", "LAB-LAB(sim)",
#             "PheCode-PheCode(rela)", "PheCode-RXNORM(rela)",
#             "PheCode-LAB(rela)")                     # Labels(pairs) to Include in Plots - If NULL Then Include All Labels
# split_patterns <- list("Similarity" = "(sim)", 
#                        "Relation" = "(rela)")        # Patterns to Split Pairs into Groups

################################################################################
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
  
 





