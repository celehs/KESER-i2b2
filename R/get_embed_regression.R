#' Generate & Evaluate Embedding
#' 
#' \code{get_embed_regression} acts as embedding regression to select related codes for interested PheCodes.
#' It returns a list of summary objects.
#' @param embed_train Tranning embedding for regression.
#' @param embed_valid Validation embedding for regression.
#' @param phecodes Interested phecodes.
#' @param dim Dimension used for embedding, for AIC/BIC calculation.
#' @param lambda_vec Lambda candidates for glmnet, it's very data specific. 
#' By default: \code{c(seq(1, 51, 1) \* 1e-6, seq(60, 1000, 50) \* 1e-6)}
#' @param alpha Alpha value for glmnet, by defaut is \code{0.25}
#' @param custom_dict Dictionary file for codes mapping. If not offered, the internal dictionary will be used.
#' Data structure:
#' \itemize{
#' \item{\code{code}}: codes like PheCode:714.1
#' \item{\code{desc}}: descriptions like rheumatoid arthritis
#' }
#' @return A list of information including:
#' \itemize{
#' \item{\code{summary_data}}: Regression summary of selected codes, beta's, cosine values and code description.
#' \item{\code{Nlist}}: Number of non-zero beta's over lambda.
#' \item{\code{min_lambdas}}: The best lambda of mininmun AIC + Testing Residual for interested Phecodes.
#' \item{\code{eval_plots}}: Plots of Residuals over log(lambda) for interested Phecodes. 
#' \item{\code{wordcloud_plots}}: Word cloud plots for selected features magnified by cosine values. 
#' }
#' 
#' @export


get_embed_regression <- function(embed_train, embed_valid, phecodes, dim,
                                 lambda_vec = c(seq(1,51,1)*1e-6,seq(60,1000,50)*1e-6),
                                 alpha = 0.25, custom_dict = NULL) {

  # load user's dict if applicable
  if (!is.null(custom_dict)) {
    OurDict <- read_file(custom_dict)
    if (colnames(OurDict) != c("code", "desc")) {
      stop("Invalid dictionary file - Data columns should be: 'code', 'desc'!")
    }
  }

  
  # Step 0 - align training & testing embeddings
  aligned_embeds <- align_embed(embed_train, embed_valid)
  embed <- aligned_embeds$embed_train
  embed_test <- aligned_embeds$embed_valid
  
  
  # step 1 - only on trainning embed
  cosine_similarity = embed %*% t(embed)
  embed = embed[which(!is.na(rownames(embed))),]
  
  lambda_vec = sort(lambda_vec, decreasing = TRUE)
  best_lambda = lambda_vec
  # cosine_cutoffs <- get_cosine_cutff(cosine_similarity, num = 10000, cutoff = 0.95)  # get cosine cutoffs for upper 95% cosine values out of 10000 samples
  lasso_phe_lst = lapply(phecodes, function(phecode){
    idx = which(rownames(embed)==phecode)
    
    # filtered_codes <- filter_by_cosine(phecode, cosine_similarity, cosine_cutoffs) # filter codes by cosine cutoffs
    filtered_codes <- setdiff(rownames(embed), phecode)  # NOT filter codes by cosine cutoffs
    
    idselect = which(rownames(embed) %in% filtered_codes)
    codeselect = rownames(embed)[idselect]
    codeselect = rm_family(codeselect, phecode)
    idselect = which(rownames(embed)%in%codeselect)
    if(length(idselect)==0){
      return(numeric())
    }
    x = t(embed[idselect,]*abs(cosine_similarity[idx,idselect]))
    x = x[,order(cosine_similarity[idx,idselect], decreasing = TRUE)]
    best_model <- glmnet::glmnet(x = x, 
                         y = embed[idx,], alpha = alpha, lambda = best_lambda,
                         intercept = FALSE,
                         standardize = FALSE)
    tmp = coef(best_model)
    colnames(tmp) = best_lambda
    tmp = tmp*abs(cosine_similarity[idx,match(rownames(tmp),colnames(cosine_similarity))]) #x = embed
    # tmp = coef(best_model) -> x = embed * cosine
    return(tmp)
  })
  names(lasso_phe_lst) = phecodes
  lasso_fit_lambda= lasso_phe_lst
  

  # Step 2 - get residuals
  trainres = GetRes(lasso_fit_lambda, embed, phecodes)
  testres = GetRes(lasso_fit_lambda, embed_test, phecodes)
  
  # see how many beta's are nonzero
  # the only input is lasso_fit_lambda from lasso_getbeta.R
  x = lasso_fit_lambda
  N_part = lapply(phecodes, function(phecode){
    beta = x[[which(names(x)==phecode)]]
    N_beta = apply(beta, 2, function(x) sum(x!=0))
    names(N_beta) = colnames(beta)
    return(N_beta)
  })
  names(N_part) = phecodes
  Nlist <- N_part
  

  # step 3 - get min_lambdas and plots of residuals over lambdas
  min_lambdas <- c()
  eval_plots <- list()
  for(j in 1:length(trainres)){
    code = names(trainres)[j]
    lambdalist = as.numeric(names(trainres[[j]]))
    trainres_tmp = log(trainres[[j]])
    testres_tmp = log(testres[[j]])
    N_tmp = Nlist[[j]]
    aic = trainres_tmp + N_tmp/dim # 1000 = dim of embedding
    bic = trainres_tmp + N_tmp*log(dim)/(dim*2) # 2000 = 2 * dim of embedding
    df = data.frame(lambda = rep(lambdalist, 4),
                    logres = c(aic, bic, testres_tmp, testres_tmp+aic),
                    type = rep(c("AIC", "BIC", "Test Set", "Test Set + AIC"), each=length(N_tmp)))
    dt = data.frame(x = lambdalist, 
                    y = rep(c(-1.9,-2.1),length(N_tmp))[1:length(N_tmp)], 
                    text = as.character(N_tmp))
    
    p1 = ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = log(lambda), y = logres, 
                                         group = type, colour = type, 
                                         linetype = type, shape = type)) + 
      ggplot2::geom_line() + 
      ggplot2::geom_point() + 
      ggplot2::scale_linetype_manual(values = c(2,2,2,1)) +
      ggplot2::scale_shape_manual(values = c(39,39,39,16)) +
      ggplot2::scale_color_manual(values = c("#c64646", "#c4c646", "#4077ad","#51c646")) +
      ggplot2::geom_hline(ggplot2::aes(yintercept=min(testres_tmp+aic)), 
                 colour="#080808", linetype="dotted") +
      ggplot2::geom_vline(ggplot2::aes(xintercept=log(lambdalist[which.min(testres_tmp+aic)])), 
                 colour="#080808", linetype="dotted") +
      ggplot2::annotate('text', x = log(dt$x), y = dt$y, label = dt$text, size = 1.75) +
      ggplot2::ggtitle(paste(code,": ",OurDict$desc[which(OurDict$code==code)]))
    plot(p1)
    eval_plots[[j]] <- p1

    min_lambda <- df %>% dplyr::filter(type == "Test Set + AIC") %>% 
      dplyr::slice(which.min(logres)) %>% dplyr::select(lambda)
    min_lambdas <- append(min_lambdas, min_lambda)
  }
  names(eval_plots) <- names(trainres) 
  names(min_lambdas) <- names(trainres)


  # step 4 - get summary data for min.lambda selected by Test Set + AIC
  summary_data <- lapply(phecodes, function(phecode) {
    beta <- lasso_fit_lambda[[phecode]][, as.character(min_lambdas[[phecode]])]
    cosine <- get_cosine(phecode, cosine_similarity)
    description <- OurDict %>% dplyr::filter(code %in% setdiff(rownames(cosine_similarity), phecode))
    
    beta <- as.data.frame(list(codes = names(beta), beta = beta))
    cosine <- as.data.frame(list(codes = rownames(cosine), cosine = cosine))
    description <- as.data.frame(list(codes = description$code, description = description$desc))
    
    summary_data <- beta %>% dplyr::left_join(cosine, by="codes") %>% dplyr::left_join(description, by="codes")
    return(summary_data)
  })
  names(summary_data) <- phecodes
  

  # step 5 - get word cloud based on cosine
  wordcloud_plots <- lapply(phecodes, function(x) {
    wordcloud2::wordcloud2(data = summary_data[[x]] %>% dplyr::filter(beta != 0) %>%
                             dplyr::mutate(word = dplyr::case_when(!is.na(description) ~ description, TRUE ~ codes)) %>%
                             dplyr::select(word, cosine) %>%
                             dplyr::rename(freq = cosine), size = .5)
  })
  names(wordcloud_plots) <- phecodes
  
  return(list(summary_data = summary_data, Nlist = Nlist, min_lambdas = min_lambdas, 
              eval_plots = eval_plots, wordcloud_plots = wordcloud_plots))
}



