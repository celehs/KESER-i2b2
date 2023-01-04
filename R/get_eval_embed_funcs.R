
# Functions To Obtain SPPMI from cooc
#########################################################################
#' Find Two level LP Codes for LOINC Codes
#' 
#' @param codelist A triple-column dataframe, colnames are V1, V2, V3
#' \itemize{
#' \item{\code{V1: Shows the row code pair.}}
#' \item{\code{V2: Shows the col code pair.}}
#' \item{\code{V3: Shows the counts for certain pair.}}
#' }
#' @param MAH: Multi-axial hierarchy dataframe.
#' @return A dataframe.
#' @keywords internal
get_rollup_dict = function(codelist, MAH){
  # codelist is a vector including LOINC codes
  # MAH is a data.frame loaded from a csv called MultiAxialHierarchy.csv
  a = unique(codelist)
  a = a[grep("LOINC",a)]
  dict_full = data.frame(feature_id = a, code = gsub("LOINC:","",a))
  dict_full$group="LOINC"
  rm(a)
  MAH1 = as.data.frame(MAH)
  MAH1 = MAH1[which(MAH1$IMMEDIATE_PARENT!="LP29693-6" & 
                      MAH1$IMMEDIATE_PARENT!="LP248770-2"),
              c("CODE","IMMEDIATE_PARENT")]
  MAH1 = MAH1[!duplicated(MAH1),]
  MAH1 = na.omit(MAH1)
  # these two codes are removed because they are not a group level, 
  ## see details in the MultiAxialHierarchy.csv
  dict_full = merge(dict_full, MAH1, all.x = TRUE,
                    by.x = "code", by.y = "CODE")
  colnames(dict_full) = gsub("IMMEDIATE_PARENT", "first", colnames(dict_full))
  dict_full = merge(dict_full, MAH1, all.x = TRUE,
                    by.x = "first", by.y = "CODE")
  colnames(dict_full) = gsub("IMMEDIATE_PARENT", "second", colnames(dict_full))
  dict_full = na.omit(dict_full)
  return(dict_full)
}


#' roll up data
#' @keywords internal
find_rollup = function(x, rolluplist){
  i = which(rolluplist[,1]==x)
  if(length(i) == 0){
    return(NA)
  }else{
    y = as.character(rolluplist[i,2])
    return(paste("LOINC:",y,sep = ""))
  }
}

#' roll up data 2
#' @keywords internal
find_rollup2 = function(x, rolluplist){
  i = which(rolluplist[,2]==x)
  if(length(i) == 0){
    return(NA)
  }else if(length(i) == 1){
    y = as.character(rolluplist[i,3])
    return(paste("LOINC:",y,sep = ""))
  }else{
    # after check, one code only has one LP-code
    y = as.character(rolluplist[i[1],3])
    return(paste("LOINC:",y,sep = ""))
  }
}

#' Arrange codes
#' 
#' let the "first col (V1) id" < "second col (V2) id"
#' and add the row with same V1 and V2
#' @keywords internal
code_arrange = function(data){
  name_list = union(data$V1[!duplicated(data$V1)],
                    data$V2[!duplicated(data$V2)])
  data = data %>% dplyr::mutate(No1 = match(data$V1,name_list),
                         No2 = match(data$V2,name_list))
  data$NoV1 = apply(data[,4:5],1,min)
  data$NoV2 = apply(data[,4:5],1,max)
  data$V1 = name_list[data$NoV1]
  data$V2 = name_list[data$NoV2]
  data = data[,1:3]
  data_arrange = aggregate(V3~V1+V2, data = data, sum)
  return(data_arrange)
}

#' Split codes
#' @keywords internal
code_split = function(data){
  a = sapply(data$V1, function(s){
    x = strsplit(s,":")[[1]]
    if(length(x)==2){
      return(x)
    }else{
      return(c(NA,NA))
    }})
  b = sapply(data$V2, function(s){
    x = strsplit(s,":")[[1]]
    if(length(x)==2){
      return(x)
    }else{
      return(c(NA,NA))
    }})
  data[,4:5] = t(a); data[,6:7] = t(b)
  colnames(data) = paste("V",1:7,sep="")
  return(data)
}

#' Roll-up codes
#' @keywords internal
code_rollup = function(data,rolluplist,viewproc=TRUE){
  idx1 = which(data$V4 == "LOINC")
  idx2 = which(data$V6 == "LOINC")
  idx = intersect(idx1,idx2)
  data1 = data.frame(
    V1 = data$V2[idx1],
    V2 = sapply(data$V5[idx1],find_rollup,rolluplist = rolluplist),
    V3 = data$V3[idx1]
  )
  data2 = data.frame(
    V1 = data$V1[idx2],
    V2 = sapply(data$V7[idx2],find_rollup,rolluplist = rolluplist),
    V3 = data$V3[idx2]
  )
  data3 = data.frame(
    V1 = sapply(data$V5[idx],find_rollup,rolluplist = rolluplist),
    V2 = sapply(data$V7[idx],find_rollup,rolluplist = rolluplist),
    V3 = data$V3[idx]
  )
  data_rollup = rbind(data[,1:3], data1, data2, data3)
  data_rollup = na.omit(data_rollup)
  
  data = data_rollup
  idx1 = which(grepl("LOINC:LP",data$V1))
  idx2 = which(grepl("LOINC:LP",data$V2))
  idx = intersect(idx1,idx2)
  data1 = data.frame(
    V1 = data$V2[idx1],
    V2 = sapply(gsub("LOINC:","",data$V1[idx1]),find_rollup2,rolluplist = rolluplist),
    V3 = data$V3[idx1]
  )
  data2 = data.frame(
    V1 = data$V1[idx2],
    V2 = sapply(gsub("LOINC:","",data$V2[idx2]),find_rollup2,rolluplist = rolluplist),
    V3 = data$V3[idx2]
  )
  data3 = data.frame(
    V1 = sapply(gsub("LOINC:","",data$V1[idx]),find_rollup2,rolluplist = rolluplist),
    V2 = sapply(gsub("LOINC:","",data$V2[idx]),find_rollup2,rolluplist = rolluplist),
    V3 = data$V3[idx]
  )
  data_rollup = rbind(data, data1, data2, data3)
  data_rollup = na.omit(data_rollup)
  
  return(data_rollup)
}

#' Roll-up data
#' @keywords internal
roll_up = function(data,rolluplist,viewproc=TRUE){
  # viewproc: this function takes a long time, use viewproc=TRUE to view where it is
  #data = code_arrange(data)
  #if(viewproc==TRUE)
  #  cat("===finish code arrangement===","\n")
  if(viewproc==TRUE)
    cat("\n    - Spliting data...")
  data = code_split(data)
  
  if(viewproc==TRUE)
    cat("\n    - Rolling up data...")
  data = code_rollup(data,rolluplist,viewproc=viewproc)
  if(viewproc==TRUE)
    cat("\n    - Arranging data...")
  data = code_arrange(data)
  return(data)
}



#' Get SPPMI matrix
#' 
#' Obtain SPPMI Matrix from COOC Matrix in Triple Form.
#' 
#' @param cooc Should be a triple form of cooc.
#' @param dict A data.frame.
#' @param code_LPcode The output of \code{get_rollup_dict(unique(c(data$V1, data$V2)))}
#' @param threshold Integer number, the threshold to get SPPMI matrix, by default is 10.
#' @return A data matrix.
#' @keywords internal
getSPPMI = function(cooc, dict, code_LPcode = NULL, threshold = 10){
  colnames(cooc) <- c("feature1","feature2","ct_A")
  id <- which(cooc$feature1%in%dict$feature_id & cooc$feature2%in%dict$feature_id)
  cooc <- cooc[id,]
  colnames(cooc) <- paste("V",1:3,sep="")
  # data = roll_up(cooc,code_LPcode)   # No Needed
  data <- cooc
  colnames(data) <- c("feature1","feature2",'ct_A')
  
  feature.list <- union(data$feature1[!duplicated(data$feature1)],
                       data$feature2[!duplicated(data$feature2)])
  
  cooccur.matrix.A <- matrix(NA, length(feature.list), length(feature.list))
  data <- data %>% dplyr::mutate(feature1_No = match(data$feature1,feature.list),
                          feature2_No = match(data$feature2,feature.list))
  inp.mtx <- as.matrix(data[,c('feature1_No','feature2_No','ct_A')])
  cooccur.matrix.A[inp.mtx[,1:2]]<- inp.mtx[,'ct_A']
  cooccur.matrix.A[inp.mtx[,c(2,1)]]<- inp.mtx[,'ct_A']
  cooccur.matrix.A[which(is.na(cooccur.matrix.A) == T)] = 0
  cooccur.matrix.A.1 = cooccur.matrix.A
  rm(cooccur.matrix.A)
  cooccur.matrix.A.1[which(cooccur.matrix.A.1 < threshold)] = 0
  idx = which(feature.list%in%c("LOINC:NA","LOINC:"))
  if(length(idx)>0){
    cooccur.matrix.A.1 = cooccur.matrix.A.1[-idx,-idx]
    feature.list = setdiff(feature.list,c("LOINC:NA","LOINC:"))
  }
  
  cooccur.list.A.1 = apply(cooccur.matrix.A.1, 1, sum)
  total.A.1 = sum(cooccur.list.A.1)
  PMI = cooccur.matrix.A.1 * total.A.1
  rm(cooccur.matrix.A.1)
  temp = matrix(cooccur.list.A.1) %*% t(matrix(cooccur.list.A.1))
  PMI = PMI / temp
  rm(temp)
  PMI = log(PMI) 
  
  
  SPPMI = matrix(0, length(feature.list), length(feature.list))
  rownames(SPPMI) = colnames(SPPMI) = feature.list
  k = 1
  idx = which(PMI > log(k))
  SPPMI[idx] = PMI[idx] - log(k)
  #a = which(apply(SPPMI,1,sum)!=0)
  #SPPMI = SPPMI[a,a]
  return(SPPMI)
}
#########################################################################





# Embed Generation Evaluation (by AUC)
#########################################################################
#' Getting randomized svd from SPPI.
#' 
#' @param SPPMI The SPPMI matrix obtained from \code{getSPPMI}.
#' @param dim specifies the target rank of the low-rank decomposition used in rSVD, suggested to set as highest dimension + 1000.
#' @return A list of matrix.
#' \itemize{
#' \item{\code{d} Array, singular values.}
#' \item{\code{u} Array, left singular vectors.}
#' \item{\code{v} Array, right singular vectors.}
#' }
#' @keywords internal
getSVD <- function(SPPMI, dim) {
  a <- which(rowSums(SPPMI) != 0)
  SPPMI <- SPPMI[a,a]
  set.seed(1)
  fit.A <- rsvd::rsvd(SPPMI, k = dim)
  rownames(fit.A$u) <- rownames(SPPMI)
  return(fit.A)
}


#' Getting embedding from svd
#' 
#' @inheritParams Evaluate_codi
#' @param mysvd The svd result.
#' @param d Dimension of the final embedding.
#' @return A dataframe of embedding.
#' @keywords internal
get_embed = function(mysvd, d = 2000, normalize = TRUE){
  id = which(sign(mysvd$u[1,])==sign(mysvd$v[1,]))
  id = id[1:min(d,length(id))]
  embed = mysvd$u[,id]%*%diag(sqrt(mysvd$d[id]))
  if(normalize){
    embed = embed/apply(embed,1,norm,'2')
  }
  rownames(embed) = rownames(mysvd$u)
  return(embed)
}

ROC_id = function(embed, Boot){
  n = min(nrow(Boot$pairs_h), nrow(Boot$pairs_null))
  y = c(rep(1, n), rep(0, n))
  id1 = Boot$pairs_h$id1; id2 = Boot$pairs_h$id2; num = length(id1)
  p1 = unlist(lapply(1:num, function(i){
    return(sum(embed[id1[i],]*embed[id2[i],]))
  }))
  id1 = Boot$pairs_null$id1; id2 = Boot$pairs_null$id2; num = length(id1)
  p2 = unlist(lapply(1:num, function(i){
    return(sum(embed[id1[i],]*embed[id2[i],]))
  }))
  p = c(p1[1:n],p2[1:n])
  
  roc0 = pROC::roc(y, p)
  cut0 = as.numeric(quantile(p2,0.99))
  cut1 = as.numeric(quantile(p2,0.95))
  cut2 = as.numeric(quantile(p2,0.9))
  
  tpr0 = roc0$sensitivities[which(roc0$thresholds>cut0)[1]]
  tpr1 = roc0$sensitivities[which(roc0$thresholds>cut1)[1]]
  tpr2 = roc0$sensitivities[which(roc0$thresholds>cut2)[1]]
  return(c('#pairs'=num,'auc'=roc0$auc,'cut/0.01'=cut0[1],'cut/0.05'=cut1[1],'cut/0.1'=cut2[1],
           'TPR/0.01'=tpr0,'TPR/0.05'=tpr1,'TPR/0.1'=tpr2))
}

evalu_part = function(embed, pairs){
  if(nrow(pairs)==0){
    myauc = c('#pairs'=0,'auc'=NA,'cut/0.01'=NA,'cut/0.05'=NA,'cut/0.1'=NA,
              'TPR/0.01'=NA,'TPR/0.05'=NA,'TPR/0.1'=NA)
    return(myauc)
  }
  Boot = list()
  Boot$pairs_h = data.frame(id1 = match(pairs$code1, rownames(embed)),
                            id2 = match(pairs$code2, rownames(embed)))
  Boot$pairs_h = na.omit(Boot$pairs_h)
  Boot$pairs_null = data.frame(id1 = match(pairs$null_code1, rownames(embed)),
                               id2 = match(pairs$null_code2, rownames(embed)))
  Boot$pairs_null = na.omit(Boot$pairs_null)
  if(nrow(Boot$pairs_h)==0|nrow(Boot$pairs_null)==0){
    myauc = c('#pairs'=0,'auc'=NA,'cut/0.01'=NA,'cut/0.05'=NA,'cut/0.1'=NA,
              'TPR/0.01'=NA,'TPR/0.05'=NA,'TPR/0.1'=NA)
    # YO comment: Why return a list other than just myacu as a vector?
    # return(list(ans = myauc, name = NA, 
    #             type = NA))
    return(myauc)    # YO Modified - Should the return be consistent as a vector other than a list?
  }
  return(ROC_id(embed, Boot))
}

get_type = function(AllRelationPairs){
  pairs = lapply(AllRelationPairs, function(x){ 
    x = x[,c('type',"pair","RELA","id")]
    x = x[!duplicated(x),]
  })
  pairs = do.call("rbind",pairs)
  pairs = pairs[!duplicated(pairs),]
  tn = lapply(1:28, function(i){
    if(sum(pairs$id==i)==0) return(c(type = "NA", name = "NA"))
    if(i<=9){
      name = unique(pairs$pair[which(pairs$id==i)])
      type = ifelse(i<9,unique(pairs$type[which(pairs$id==i)]),"rm")
      name = paste(name,"(",ifelse(i<=3,"sim",ifelse(i<=8,"rela","")),")",sep="")
    }else{
      name = ifelse(i<=27, pairs$RELA[which(pairs$id==i)], "CUI-code")
      type = unique(pairs$type[which(pairs$id==i)])
      name = paste(name,"(",ifelse(type=="similarity","sim","rela"),")",sep="")
    }
    stopifnot(length(type)==1&length(name)==1)
    return(c(type = type, name = name))
  })
  tn = do.call("rbind", tn)
  return(list(type = tn[,1], name = tn[,2]))
}

#' Evaluate The Embedding (Codi & CUI)
#' 
#' 
#' @param embed Embedding, should be normed, rownames are code names.
#' @param AllRelationPairs All relation pairs dataframe.
#' @param evatype Can be "train", "test", all the other character would be ignored.
#' \itemize{
#' \item{If \code{evatype="train"}, choose first \code{(prop * 100)}\% pairs as training pairs.}
#' \item{If \code{evatype="test"}, choose last \code{(prop * 100)}\% pairs as test pairs.}
#' \item{Otherwise, using all pairs to evaluate the embedding.}
#' }
#' @param prop There are 3 scenarios:
#' @param normalize \code{TRUE} or \code{TFALSE}, to normalize embedding or not.
#' @details
#' \itemize{
#' \item{\code{wei_auc}: The weighted auc of similar pairs, related pairs and CUI-CUI pairs.}
#' \item{\code{anstable}: The The auc of all subtype pairs.}
#' \item{\code{fulltable}: The full information of subtype pairs, including cut, TPR, etc...}
#' }
#' 
#' @return A list.
#' @keywords internal
Evaluate = function(embed, AllRelationPairs, evatype = "all", prop = 0.3, normalize = TRUE){
  if(normalize){
    embed = embed/apply(embed,1,norm,'2')
  }
  tn = get_type(AllRelationPairs)
  if(evatype=="train"){
    AllRelationPairs = lapply(AllRelationPairs, function(x){
      return(x[which(x$training<=prop),])
    })
  }else if(evatype=="test"){
    AllRelationPairs = lapply(AllRelationPairs, function(x){
      return(x[which(x$training>prop),])
    })
  }
  fulltable = lapply(1:3, function(k){
    anslist = lapply(1:28, function(i){
      pairs = AllRelationPairs[[k]]
      pairs = pairs[which(pairs$id==i),]
      return(evalu_part(embed, pairs))
    })
    ans = do.call("rbind", lapply(anslist, function(x) x))
    ans = as.data.frame(ans)
    rownames(ans) = tn$name
    pairtype = tn$type
    id = which(pairtype=="similarity")
    ans = rbind(ans, apply(ans[id,],2,weighted.mean,ans[id,1]))
    ans[nrow(ans),1] = sum(ans[id,1])
    rownames(ans)[nrow(ans)] = "weighted.sim"
    id = which(pairtype=="related")
    ans = rbind(ans, apply(ans[id,],2,weighted.mean,ans[id,1]))
    ans[nrow(ans),1] = sum(ans[id,1])
    rownames(ans)[nrow(ans)] = "weighted.rela"
    return(ans)
  })
  anstable = do.call(cbind, lapply(fulltable,function(x) x[,1:2]))
  colnames(anstable) = c("CUI-CUI","auc","CUI-codi","auc","codi-codi","auc")
  names(fulltable) = c("CUI-CUI","CUI-codi","codi-codi")
  wei_auc = as.matrix(tail(anstable,2))
  wei_auc = cbind(wei_auc, all = apply(wei_auc[,c(1,3,5)],1,sum))
  wei_auc = cbind(wei_auc, wei.auc = c(weighted.mean(wei_auc[1,c(2,4,6)],wei_auc[1,c(1,3,5)]),
                                       weighted.mean(wei_auc[2,c(2,4,6)],wei_auc[2,c(1,3,5)])))
  wei_auc = cbind(wei_auc, ave.auc = c(mean(wei_auc[1,c(2,4,6)]),
                                       mean(wei_auc[2,c(2,4,6)])))
  return(list(wei_auc = as.matrix(wei_auc),
              anstable = as.matrix(anstable),
              fulltable = as.data.frame(fulltable)))
}

#' Evaluate The Embedding (Codi Only)
#' 
#' @inheritParams Evaluate
#' @param labels A character vector of labels in evaluations summary. Values can be 
#' \code{"PheCode-PheCode(sim)", "RXNORM-RXNORM(sim)", "LAB-LAB(sim)", 
#' "PheCode-PheCode(rela)", "PheCode-RXNORM(rela)", "PheCode-CCS(rela)", "PheCode-LAB(rela)"} .
#' If \code{NULL} then all labels will be used.
#' @return A list.
#' @keywords internal
Evaluate_codi = function(embed, AllRelationPairs, evatype = "all", prop = 0.3, normalize = TRUE, labels=NULL){
  
  if(normalize){
    embed = embed/apply(embed,1,norm,'2')
  }
  tn = get_type(AllRelationPairs)
  if(evatype=="train"){
    AllRelationPairs = lapply(AllRelationPairs, function(x){
      return(x[which(x$training<=prop),])
    })
  }else if(evatype=="test"){
    AllRelationPairs = lapply(AllRelationPairs, function(x){
      return(x[which(x$training>prop),])
    })
  }
  k = "code_code"
  anslist = lapply(ARP_CODI_IDX, function(i){
    pairs = AllRelationPairs[[k]]
    pairs = pairs[which(pairs$id==i),]
    return(evalu_part(embed, pairs))
  })
  ans = do.call("rbind", lapply(anslist, function(x) x))
  ans = as.data.frame(ans)
  rownames(ans) = tn$name[ARP_CODI_IDX]
  pairtype = tn$type[ARP_CODI_IDX]
  ans <- ans[which(pairtype %in% c("similarity", "related")), ]   # YO - filter out rows with "similarity" & "related"
  if (!is.null(labels)) ans <- ans[labels, ]                      # YO - filter out rows by labels, by default no filtering
  id = which(pairtype=="similarity")
  ans = rbind(ans, apply(ans[id,],2,weighted.mean,ans[id,1]))
  ans[nrow(ans),1] = sum(ans[id,1])
  rownames(ans)[nrow(ans)] = "weighted.sim"
  id = which(pairtype=="related")
  ans = rbind(ans, apply(ans[id,],2,weighted.mean,ans[id,1]))
  ans[nrow(ans),1] = sum(ans[id,1])
  rownames(ans)[nrow(ans)] = "weighted.rela"
  return(ans)
}
#########################################################################


# Functions To plot
#########################################################################

#' Function to get plot data
#' @keywords internal
get_plot_data <- function(summary, col="auc", dim_name = "dims", labels = NULL) {
  l <- lapply(names(summary[["summary"]]), function(d) {
    summary[["summary"]][[d]][["evaluation"]][[col]]
  })
  df <- do.call(rbind.data.frame, l)
  colnames(df) <- row.names(summary[["summary"]][[1]][["evaluation"]])
  row.names(df) <- names(summary[["summary"]])
  df <- df %>% t() %>% stats::na.omit() %>% t() %>% as.data.frame()  # delete NA Columns
  df[[dim_name]] <- as.numeric(row.names(df))
  
  # filter out columns if labels specified
  if (!is.null(labels)) {
    df <- df %>% dplyr::select(dplyr::one_of(append(dim_name, labels)))
  }
  
  return(df)
} 

#' Function to split plot data by group
#' @keywords internal
split_data <- function(plot_data, include.others = TRUE, dim_name = "dims",
                       patterns=list("Similarity" = "(sim)", "Relation" = "(rela)")) {
  
  # get groups by pattern
  cols <- colnames(plot_data)
  splits <- lapply(names(patterns), function(p) {
    append(dim_name, cols[grepl(patterns[[p]], cols, fixed = TRUE)])
  })
  names(splits) <- names(patterns)

  # get other group
  if (include.others) {
    other <- cols
    for (s in splits) {
      other <- setdiff(other, s)
    }
    if (length(other) > 0) {
      splits[["Other"]] <- append(dim_name, other)
    } else {splits[["Other"]] <- c()}
  }
 
  # get split data list
  out <- lapply(splits, function(s) {
    plot_data[, s]
  })
  return(out)
}

#' Function to reshape plot data
#' @keywords internal
reshape_data <- function(data, x = "dims", y = "vals", group = "pairs") {
  out <- data.frame()
  for (col in setdiff(colnames(data), x)) {
    tmp <- data[, c(x, col)]
    tmp[, y] <- col
    colnames(tmp) <- c(x, y, group)
    out <- rbind(out, tmp)
  }
  row.names(out) <- 1:nrow(out)
  return(out)
}

#' Function to plot scatter-line by group
#' @keywords internal 
plt <- function(data, x="dims", y="auc", group = "pairs", method = "plotly") {
  
  # rename column names
  colnames(data) <- gsub("/|#", "_", colnames(data))
  y <- gsub("/|#", "_", y)
  
  m <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 100,
    pad = 4
  )
  
  # hovertemplate <- paste(
  #   "<b>%{text}</b><br><br>",
  #   "%{xaxis.title.text}: %{x}<br>",
  #   "%{yaxis.title.text}: %{y}",
  #   "<extra></extra>"
  # )
  
  hovertemplate <- paste(
    "<b>%{text}</b>: %{y}",
    "<extra></extra>"
  )
  
  if (method == "plotly") {
    return(
      data %>% plotly::group_by(!!group) %>%
        plotly::plot_ly(x = as.formula(paste0('~', x)), 
                        y = as.formula(paste0('~', y)), 
                        type = "scatter",
                        color = as.formula(paste0('~', group)),
                        text = as.formula(paste0('~', group)),
                        mode = "lines+markers",
                        hovertemplate = hovertemplate) %>%
        plotly::layout(autosize = F, width = 900, height = 500, margin = m,
                       legend = list(orientation = 'h', y = -0.3),
                       hovermode = "x unified")
    )

  } 
  
  if (method == "ggplot") {
    return(
      data %>% ggplot2::ggplot(
        ggplot2::aes_string(x = x, y = y, group = group, color = group)
      ) + ggplot2::geom_line() + ggplot2::geom_point()
    )
  }
 
}
#########################################################################


# Functions - Utility
#########################################################################
#' Functions to read data file
#' 
#' @param file_name Data format can be \code{.csv}, \code{.parquet} or \code{.Rdata}.
#' @return A DataFrame.
#' @export
read_file <- function(file_name) {
  ext <- strsplit(tolower(file_name), split = "\\.")[[1]][-1]
  if (ext == "csv") return(readr::read_csv(file_name, show_col_types = FALSE))
  if (ext == "parquet") {
    # tryCatch(suppressWarnings(library(arrow)),
    #          error = function() {
    #            message("Package 'arrow' Not Found. To install, ",
    #                    "try: install.packages(\"arrow\")")
    #          })
    return(arrow::read_parquet(file_name, show_col_types = FALSE) %>% as.data.frame())
  }
  if (ext == "rdata") {
    var <- load(file_name)
    err_msg <- paste0("Number of variable in '", file_name, "' NOT equal to 1!")
    if (length(var) != 1) stop(err_msg) else return(get(var)) 
  }
}


#' Functions to map CO codes from CO dict
#' 
#' @keywords internal
map_CO <- function(CO, freq) {
  code_dict <- freq %>% dplyr::select(index, code)
  index1 <- CO %>% dplyr::left_join(code_dict, by = c("index1" = "index"))
  index2 <- CO %>% dplyr::left_join(code_dict, by = c("index2" = "index"))
  CO_new <- data.frame(cod1 = index1[["code"]], code2 = index2[["code"]], count = CO[["count"]])
  index1_NA <- index1[["index1"]][is.na(CO_new[["code1"]])]
  index1_NA <- index2[["index2"]][is.na(CO_new[["code2"]])]
  NAs <- append(index1_NA, index1_NA)
  if (length(NAs) != 0) {
    stop(paste0("\nCode Pairs Not Found for below index:\n", NAs))
  }
  return(CO_new)
}


#' Function to get unique codes in CO
#' @keywords internal
get_unique_codes <- function(CO) {
  return(unique(append(CO[[1]], CO[[2]]))) 
}

#' Function to remove CO codes by frequency cutoff
#' 
#' @keywords internal
clear_CO <- function(CO, freq, freq_min=1000) {
  cat(paste0("\nRules: Codes with frequency counts less than ", freq_min, " will be remove from CO.\n"))
  colnames(CO) <- c("V1", "V2", "V3")
  codes_co <- get_unique_codes(CO)
  codes_rm <- freq %>% dplyr::filter(freq_count < freq_min) %>% 
    dplyr::select(code) %>% dplyr::pull() %>% unique() %>% intersect(codes_co)
  cat(paste0("\n", length(codes_rm), "/", length(codes_co), " unique codes in CO will be removed."))
  CO_clear <- CO %>% dplyr::filter(!(V1 %in% codes_rm) & !(V2 %in% codes_rm))
  return(CO_clear)
}

#' Function to check memory

#' @keywords internal
memory_chk <- function(CO) {
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    total_ram <- as.numeric(gsub("\r","",gsub("TotalVisibleMemorySize=","",system('wmic OS get TotalVisibleMemorySize /Value',intern=TRUE)[3])))/1024/1024
    free_ram <- as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024
  } else if (os == "Linux") {
    total_ram <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo ", intern=TRUE))/1024/1024
    free_ram <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo ", intern=TRUE))/1024/1024
  } else if (os == "Darwin") {
    ram_txt <- stringr::str_extract_all(system("top -l 1 | grep PhysMem", intern=TRUE), "[0-9]+[a-zA-Z]{1}")[[1]]
    ram <- lapply(ram_txt, function(x) {
      num <- as.numeric(stringr::str_extract(x, "[0-9]+"))
      unit <- stringr::str_extract(toupper(x), "[A-Z]+")
      if ("G" %in% unit) {
        return(num)
      } else if ("M" %in% unit) {
        return(num / 1024)
      } else if ("K" %in% unit) {
        return(num / 1024 / 1024)
      } else (return(NA))
    }) %>% unlist()
    total_ram <- ram[1] + ram[3]
    free_ram <- ram[1]
  } else {
    cat("\n Unrecognized OS: ", os, ". Memory check ignored.\n")
    return(NULL)
  }
 
  cat(paste0("\nSystem OS: ", os))
  est_ram <- length(get_unique_codes(CO)) * RAM_USAGE_PER_CODE
  cat(paste0("\nTotal Physical RAM: ", total_ram, " Gb.\n"))
  cat(paste0("Free Physical RAM: ", free_ram, " Gb.\n"))
  cat(paste0("Estimated RAM Usage: ", est_ram, " Gb.\n"))
  if (est_ram > total_ram) stop("Total RAM not enough, please upgrade your physical RAM.")
  if (est_ram > free_ram) stop("Usable RAM not enough, please free up your memory.")
}

#' Function to check path
#'
#' @param path An full or relative path.
#' @keywords internal
#' @return A full path of input path.
path_chk <- function(path) {
  full_dir <- file.path(getwd(), path)
  if (dir.exists(dirname(full_dir))) {
    return(full_dir)
  } else if(dir.exists(dirname(path))) {
    return(path)
  } else {
    stop(paste0("Path or File Not Found: ", path), ". Please try entering the absolute full path.")
  }
}

#' Function to check CO data
#' @keywords internal
chk_CO <- function(CO) {
  # cat("\n\n- Codes ending with '-PCS' will be cleared as: 'Code-PCS' to 'Code'\n")
  # CO <- CO %>% dplyr::mutate(dplyr::across(c("V1", "V2"), stringr::str_replace, "-PCS", ""))
  cat("- Valid codes: ",  paste0(VALID_CODES, collapse=", "), "\n")
  cat("- Examples: LOINC:15429, RXNORM:248656, PheCode:426.92, CCS:143\n")
  CO_Unique_codes <- get_unique_codes(CO)
  valid_codes <- lapply(VALID_CODES, function(x) {
    # CO_Unique_codes[!is.na(stringr::str_extract(CO_Unique_codes, paste0(x, ":([0-9]+$|[0-9]+.[0-9]+$)")))]
    CO_Unique_codes[!is.na(stringr::str_extract(CO_Unique_codes, paste0(x, ":.")))]
  })
  names(valid_codes) <- VALID_CODES
  invalid_codes <- CO_Unique_codes
  cat("\nCodes summary (", length(CO_Unique_codes), "unique codes ):")
  for (code in VALID_CODES) {
    cat(paste0("\n# of ", code, ": ", length(valid_codes[[code]])))
    invalid_codes <- setdiff(invalid_codes, valid_codes[[code]])
  }
  cat(paste0("\n# of invalid codes: ", length(invalid_codes), "\n"))
  if (length(invalid_codes) != 0) {
    cat(paste0("\nInvalid codes: "))
    cat(invalid_codes)
    cat("\n\n")
  }
  return(CO)
}
#########################################################################