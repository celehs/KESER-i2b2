
# fill 0's for validation set if no rows there
align_embed <- function(embed_train, embed_valid) {
  
  # turn matrix into data.frame
  embed_train <- as.data.frame(embed_train)
  embed_valid <- as.data.frame(embed_valid)
  
  # sort rows by alphabetic order of codes
  rows_train <- sort(rownames(embed_train))
  rows_valid <- rownames(embed_valid)
  embed_train <- embed_train[rows_train, ]  
  
  # make sure all codes in validation embedding are also in training embedding
  diff1 <- setdiff(rows_valid, rows_train)
  if (length(diff1) != 0 ) {
    stop(paste0("Validation embedding has unrecognized codes in training embedding: ",
                paste0(diff1, collapse = ", ")))
  }
  
  # fill non-existing codes as 0 in validation embedding
  diff2 <-setdiff(rows_train, rows_valid)
  m <- length(diff2)
  if (m > 0) {
    adding <- as.data.frame(matrix(rep(0, ncol(embed_valid) * m), nrow = m))
    colnames(adding) <- colnames(embed_valid)
    rownames(adding) <- diff2
    embed_valid <- rbind(embed_valid, adding)
  }
  embed_valid <- embed_valid[rows_train, ]   #sort rows by alphabetic order of codes (use training embedding codes)
  
  # turn data frame back to matrix
  embed_train <- data.matrix(embed_train)
  embed_valid <- data.matrix(embed_valid)
  
  return(list("embed_train" = embed_train, "embed_valid" = embed_valid))
}


# get cosine matirx for code-pairs
get_cosine <- function(interest, cos_matrix, codes = NULL) {
  if (is.null(codes)) codes <- setdiff(rownames(cos_matrix), interest)
  cosine <- sapply(codes, function(code) {
    cos_matrix[interest, code]
  },  simplify = TRUE, USE.NAMES = TRUE)
  cosine <- as.data.frame(cosine)
  return(cosine)
}


# remove parents/children for interested pheocde
rm_family = function(codeselect, phecode){
  idx = grep("PheCode:", codeselect)
  if(length(idx)==0){
    return(codeselect)
  }else{
    idx1 = grep(phecode, codeselect)
    idx2 = which(sapply(codeselect, function(code) grepl(code, phecode)))
    idx = union(idx1, idx2)
    if(length(idx)==0){
      return(codeselect)
    }else{
      return(codeselect[setdiff(1:length(codeselect),idx)])
    }
  }
}



# get residuals
GetRes = function(lasso_fit_lambda, embed, phecode_lst, cosd = NULL){
  x = lasso_fit_lambda
  res_part = lapply(phecode_lst, function(phecode){
    beta_list = x[[which(names(x)==phecode)]]
    idx = which(rownames(beta_list)%in%rownames(embed))
    idy = which(rownames(embed)==phecode)
    if(length(idx)==0){
      tmp = rep(norm(embed[which(rownames(embed)==phecode),],'2'), ncol(beta_list))
      names(tmp) = colnames(beta_list)
      return(tmp)
    }else if(length(idx)==1){
      beta_list = beta_list[idx,]
      idbeta = which(rownames(embed)==names(beta_list))
      tmp = t(t(embed[idbeta,]))%*%t(beta_list) #dd=0
    }else{
      beta_list = beta_list[idx,]
      idbeta = match(rownames(beta_list),rownames(embed))
      tmp = t(embed[idbeta,])%*%beta_list
    }
    tmp = tmp-embed[which(rownames(embed)==phecode),]
    tmp = apply(tmp, 2, norm, '2') # res = |Y-\hat Y|
    names(tmp) = colnames(beta_list)
    return(tmp)
  })
  names(res_part) = names(lasso_fit_lambda)
  return(res_part)
}


# get cosine cutoffs by quantile of sampling
get_cosine_cutff <- function(cosine_similarity, num=10000, cutoff=0.95) {
  set.seed(0)
  codes <- rownames(cosine_similarity)
  phecode_vec <- codes[grepl("PheCode", codes, fixed = TRUE)]
  rxnorm_vec <- codes[grepl("RXNORM", codes, fixed = TRUE)]
  loinc_vec <- codes[grepl("LOINC", codes, fixed = TRUE)]
  ccs_vec <- codes[grepl("CCS", codes, fixed = TRUE)]
  other_vec <- setdiff(codes, phecode_vec) %>% setdiff(rxnorm_vec) %>% setdiff(loinc_vec) %>% setdiff(ccs_vec)
  
  phecode_cutoff<- lapply(seq(1, num), function(x) {
    abs(cosine_similarity[sample(phecode_vec, 1), sample(phecode_vec, 1)])
  }) %>% unlist() %>% sort() %>% quantile(cutoff)
  
  rxnorm_cutoff <- lapply(seq(1, num), function(x) {
    abs(cosine_similarity[sample(phecode_vec, 1), sample(rxnorm_vec, 1)])
  }) %>% unlist() %>% sort() %>% quantile(cutoff)
  
  loinc_cutoff <- lapply(seq(1, num), function(x) {
    abs(cosine_similarity[sample(phecode_vec, 1), sample(loinc_vec, 1)])
  }) %>% unlist() %>% sort() %>% quantile(cutoff)
  
  ccs_cutoff <- lapply(seq(1, num), function(x) {
    abs(cosine_similarity[sample(phecode_vec, 1), sample(ccs_vec, 1)])
  }) %>% unlist() %>% sort() %>% quantile(cutoff)
  
  other_cutoff <- lapply(seq(1, num), function(x) {
    abs(cosine_similarity[sample(phecode_vec, 1), sample(other_vec, 1)])
  }) %>% unlist() %>% sort() %>% quantile(cutoff)
  
  cutoffs <- c(phecode_cutoff, rxnorm_cutoff, loinc_cutoff, ccs_cutoff, other_cutoff)
  names(cutoffs) <- c("phecode", "rxnorm", "loinc", "ccs", "other")
  return(cutoffs)
}


# get filtered codes by cosine cutoffs
filter_by_cosine <- function(phecode, cosine_similarity, cutoffs) {
  codes <- rownames(cosine_similarity)
  phecode_vec <- codes[grepl("PheCode", codes, fixed = TRUE)]
  rxnorm_vec <- codes[grepl("RXNORM", codes, fixed = TRUE)]
  loinc_vec <- codes[grepl("LOINC", codes, fixed = TRUE)]
  ccs_vec <- codes[grepl("CCS", codes, fixed = TRUE)]
  other_vec <- setdiff(codes, phecode_vec) %>% setdiff(rxnorm_vec) %>% setdiff(loinc_vec) %>% setdiff(ccs_vec)
  
  phecode_filtered <- lapply(phecode_vec, function(x) {
    if (phecode != x & abs(cosine_similarity[phecode, x]) >= cutoffs["phecode"]) return(x)
  }) %>% unlist()
  
  rxnorm_filtered <- lapply(rxnorm_vec, function(x) {
    if (abs(cosine_similarity[phecode, x]) >= cutoffs["rxnorm"]) return(x)
  }) %>% unlist()
  
  loinc_filtered <- lapply(loinc_vec, function(x) {
    if (abs(cosine_similarity[phecode, x]) >= cutoffs["loinc"]) return(x)
  }) %>% unlist()
  
  ccs_filtered <- lapply(ccs_vec, function(x) {
    if (abs(cosine_similarity[phecode, x]) >= cutoffs["ccs"]) return(x)
  }) %>% unlist()
  
  other_filtered <- lapply(other_vec, function(x) {
    if (abs(cosine_similarity[phecode, x]) >= cutoffs["other"]) return(x)
  }) %>% unlist()
  
  return(c(phecode_filtered, rxnorm_filtered, loinc_filtered, ccs_filtered, other_filtered))
}

