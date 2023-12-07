#' DESeq2 method
#'
#' This function analyzes differential expression, returns significant genes.
#' @param log_dat Logged gene expression data
#' @param anal_des provides the experimental design for the DE analysis
#' @return dds stores the results of the DE analysis using limma method
#' @import DESeq2
#'
#' @export

DESeq2_method_DE <- function(dat, anal_des) {
  
  # Check if the assay contains counts (e.g. non negative integer data)
  # if (any(!dat %in% 0:max(dat)) || any(dat < 0)) {
  #   stop("Error: The data does contains negative integer counts.")
  # }
  
  # Ensure that the data contains only non-negative integers
  if (any(dat < 0)) {
    stop("Error: The data contains negative counts.")
  }
  if (any(dat != floor(dat))) {
    stop("Error: The data contains non-integer counts.")
  }
  
  # Center and scale numeric variables in the analysis design
  numeric_vars <- sapply(anal_des, is.numeric)
  if (any(numeric_vars)) {
    anal_des[numeric_vars] <- scale(anal_des[numeric_vars])
  }
  
  colnames(dat) <- rownames(anal_des)
  dat[is.na(dat)] <- 0
  
  dds <- DESeqDataSetFromMatrix(
    countData = dat,
    colData = anal_des,
    design = stats::as.formula(
      paste(
        " ~ ",
        paste(colnames(anal_des),
              collapse = "+"
        )
      )
    )
  )
  dds <- DESeq(dds)
  
  # List all the coefficients in the model
  coefNames <- resultsNames(dds)
  
  # Initialize a list to store results for each coefficient
  listOfResults <- list()
  
  # Loop through each coefficient and extract results
  for (coef in coefNames) {
    listOfResults[[coef]] <- results(dds, name=coef)
  }
  dds <- listOfResults
  
  return(dds)
}
