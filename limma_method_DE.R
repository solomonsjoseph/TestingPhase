#' Limma Method
#'
#' This function analyzes differential expression, returns significant genes.
#' @param log_dat Logged gene expression data
#' @param anal_des provides the experimental design for the DE analysis
#' @param batch batch data
#' @param con_list conditon data
#' @return dds stores the results of the DE analysis using limma method
#' @import limma
#'
#' @export

limma_method <- function(log_dat, anal_des, batch, cond_list) {
  
  # Check if log_dat contains only numeric values
  if (!all(sapply(log_dat, is.numeric))) {
    anal_des[is.na(anal_des)] <- 0
  }
  
  # build formula:
  mod_formula <- as.formula(paste0(" ~ ", batch, " + ", paste(cond_list, collapse = " + ")))
  des <- model.matrix(mod_formula, data = anal_des)
  colnames(des)[1] <- "Intercept"
  
  # Fit the linear model
  fit <- lmFit(log_dat, design = des)
  
  # contrasts for batch
  contrasts <- list(levels=des)
  contrasts <- append(contrasts, paste(batch, levels(anal_des[, batch])[-1], sep = "", collapse = " + "))
  
  # contrasts for conditions
  for (con in cond_list) {
    contrasts <- append(contrasts, paste(con, levels(anal_des[, con])[-1], sep = "", collapse = " + "))
  }
  
  # Create the contrast matrix
  contrast.matrix <- makeContrasts(contrasts=unlist(contrasts[-1]), levels=des)
  
  # do.call(makeContrasts, myargs)
  fit <- contrasts.fit(fit, contrast.matrix)
  
  # Apply empirical Bayes moderation to the standard errors
  fit2 <- eBayes(fit)
  
  # Extract top differentially expressed genes
  lim <- topTable(fit2, number = Inf)
  
  return(lim)
}
