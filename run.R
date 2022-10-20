# source("R/get_eval_embed_funcs.R")
# source("R/get_eval_embed.R")

CO_file <- "dungeon//rpdr_code_cooccurrence_victor_2019.csv"  # Co-occurrence File: .csv/.parquet/.Rdata
HAM_file <- "dungeon//MultiAxialHierarchy.csv"                # Multi-axial Hierarchy File: .csv/.parquet/.Rdata  
ARP_file <- "dungeon//AllRelationPairsWithNull.Rdata"         # All Relation Pairs File: .csv/.parquet/.Rdata  
dims <- seq(200, 400, 200)                                   # Dimension Setting
out_dir <- NULL                                               # Output folder setting -  If NULL All Outputs Will Be At: working_dir/output
data_type <- 1                                                # Input Data Type Setting, Codi Only:1, Codi & CUI: 2 

# summary <- get_eval_embed(CO_file, HAM_file, ARP_file, dims, out_dir)

get_report(summary, plot_val = "auc", knit_format = "pdf")