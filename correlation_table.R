


get_corr <- function(df, ref_col, digits=3){

  clean_corr <- function(target_col){
    
    corr <- try(cor.test(df[[ref_col]], df[[target_col]]), silent=TRUE)
    #If the columns are too similar, we get an error, this is how we handle it
    if(identical(class(corr), "try-error")){
      return(list("question"=target_col, "correlation"=NA, "t"=NA, "p"=NA))
    }
    return(list(
      "question"   = target_col,
      "correlation"= round(unname(corr$estimate), digits),
      "t"          = round(unname(corr$statistic), digits),
      "p"          = round(unname(corr$p.value), digits)
    ))  
  }
  
  lst <- vector("list")
  for(target_col in colnames(df[colnames(df) != ref_col])){
    lst[[target_col]] <- clean_corr(target_col)
  }
  
  ret <- do.call(rbind.data.frame, lst)
  rownames(ret) <- NULL
  return(ret)
}

#Alternate version
get_corr <- function(df, ref_col, digits=3){
  
  clean_corr <- function(target_col){
    
    corr <- try(cor.test(df[[ref_col]], df[[target_col]]), silent=TRUE)
    #If the columns are too similar, we get an error, this is how we handle it
    if(identical(class(corr), "try-error")){
      return(list("question"=target_col, "correlation"=NA, "t"=NA, "p"=NA))
    }
    
    return(list(
      "question"   = target_col,
      "correlation"= round(unname(corr$estimate), digits),
      "t"          = round(unname(corr$statistic), digits),
      "p"          = round(unname(corr$p.value), digits)
    ))  
  }
  
  # Over the column names which are not the reference column,
  # calculate and clean the correlation
  lst <- lapply(colnames(df[colnames(df) != ref_col]), clean_corr)
  ret <- do.call(rbind.data.frame, lst)
  
  return(ret)
}
