globalVariables(c("chosen"))

#' Differential Expression Analysis
#'
#' This function runs DE analysis on a count matrix in the se object
#' @param se SummarizedExperiment object
#' @param method DE analysis method option
#' @param batch Batch sample metadata column
#' @param conditions Sample metadata columns for additional analysis covariates
#' @param assay_to_analyze Assay for DE analysis
#' @return A named list of of two matrices.
#' @return res features the DE analysis results.
#' @return volcano features a subset of the DE analysis results for plotting.
#' @import SummarizedExperiment
#' @import DESeq2
#' @import scran
#'
#' @export
DE_analyze <- function(se, method, batch, conditions, assay_to_analyze) {
  assay_dat <- assays(se)[[assay_to_analyze]]
  rownames(assay_dat) <- names(se)
  analysis_design <- as.data.frame(colData(se)[c(conditions, batch)])
  
  if (method == "DESeq2") {
    
    # method_res <- DESeq2_method_DE(assay_dat, analysis_design)
    
    # Check if the assay contains counts (e.g. non negative integer data), otherwise throw an error
    if (any(!data %in% 0:max(data)) || any(data < 0)) {
      stop("Error: The data does contains negative integer counts.")
    }
    dds <- DESeqDataSetFromMatrix(
      countData = data,
      colData = analysis_design,
      design = stats::as.formula(
        paste(
          " ~ ",
          paste(colnames(analysis_design),
                collapse = "+"
          )
        )
      )
    )
    method_res <- DESeq(dds)
    
  }
  if (method == "limma") {
    # method_res <- limma_method_DE(assay_dat, analysis_design,
    #                               batch, conditions)
    
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
    method_res <- topTable(fit2, number = Inf)
    
  }
  return(list(method_res = method_res))
}


#' Returns summary table for p-values of explained variation
#'
#' @param DE_res DE analysis results output from DE_analyze()
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#'
#' @export
pval_summary <- function(DE_res) {
  
  pval_table <- rbind(summary(results(DE_res$dds)[, 'pvalue']))
  
  row_count <- 1
  for (i in seq_len(length(resultsNames(DE_res$dds)))) {
    if (resultsNames(DE_res$dds)[i] == 'Intercept') {
      next
    }else if (i == length(resultsNames(DE_res$dds))) {
      next
    }else {
      pval_table <- rbind(pval_table, summary(
        results(DE_res$dds, name = resultsNames(DE_res$dds)[i])$pvalue))
      rownames(pval_table)[row_count + 1] <- resultsNames(DE_res$dds)[i]
      row_count <- row_count + 1
    }
  }
  
  rownames(pval_table)[1] <- "Batch"
  
  return(list(pval_table = pval_table))
}


#' Covariate P-value Plotter
#' This function allows you to plot covariate p-values of explained variation
#' @param DE_res DE analysis results output from DE_analyze()
#' @import reshape2
#' @import ggplot2
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#'
#' @export
covariate_pval_plotter <- function(DE_res) {
  pval_table <- c()
  covar_list <- c()
  for (i in seq_len(length(resultsNames(DE_res$dds)))) {
    if (resultsNames(DE_res$dds)[i] == 'Intercept') {
      next
    }else if (i == length(resultsNames(DE_res$dds))) {
      next
    }else {
      pval_table <- cbind(pval_table,
                          results(DE_res$dds,
                                  name = resultsNames(DE_res$dds)[i])$pvalue)
      covar_list <- c(covar_list, resultsNames(DE_res$dds)[i])
    }
  }
  colnames(pval_table) <- covar_list
  covar_boxplot <- ggplot(subset(melt(data.table::as.data.table(pval_table),
                                      id.vars = NULL),
                                 variable %in% covar_list),
                          aes(x = variable, y = value, fill = variable)) +
    geom_violin(width = 0.8) +
    geom_boxplot(width = 0.1) +
    coord_flip() +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "P-Values") +
    labs(title =
           "Distribution of Covariate Effects (P-Values) Across Genes") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  return(list(covar_boxplot = covar_boxplot))
}


#' This function allows you to plot batch p-values of explained variation
#' @param DE_res DE analysis results output from DE_analyze()
#' @import reshape2
#' @import ggplot2
#' @importFrom data.table data.table
#' @return List of explained variation by batch and condition
#'
#' @export
batch_pval_plotter <- function(DE_res) {
  batch_boxplot <- ggplot(
    data = melt(data.table::as.data.table(results(DE_res$dds)$pvalue),
                id.vars = NULL),
    aes(x = variable, y = value, fill = variable)) +
    geom_violin(width = 0.8) +
    geom_boxplot(width = 0.1) +
    scale_color_manual(values = "#56B4E9", aesthetics = "fill") +
    coord_flip() +
    scale_x_discrete(name = "", labels = "Batch") +
    scale_y_continuous(name = "P-Values") +
    labs(title = "Distribution of Batch Effect (P-Values) Across Genes") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  return(list(batch_boxplot = batch_boxplot))
}
