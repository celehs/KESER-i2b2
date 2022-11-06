
# Global Params
PKG.NAME <- "KESER.i2b2"
DIM_MAX <- 2000                                              # Maximum limit of embedding dimension
ARP_CODI_IDX <- 1:9                                          # id range for codi items in all relation pairs file
RAM_USAGE_PER_CODE <- 20 / 23000                             # 23k unique codes use up 20gb of ram
VALID_CODES <- c("LOINC", "RXNORM", "CCS", "PheCode")        # Valid codes in CO  