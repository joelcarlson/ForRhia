###
### Example usage at bottom of file!
###
get_t_test <- function(df_est, df_new, cond_col, digits=3){
  
    clean_t_test <- function(column, digits=3){
      #Currently this function takess a column, and a dataset (not passed in),
      #calculates the t-test, and returns the values in clean format. 
      # After the function we loop over each column name to spit out a table of 
      # all t-test values with the given column based on a conditional
      t_test <- try(t.test(df_est[[column]], df_new[[column]]), silent=TRUE)
      #If the columns are too similar, we get an error, this is how we handle it
      if(identical(class(t_test), "try-error")){
        return(list("question"=column, NA, NA, NA, NA, NA, NA))
      }
      #Return all the values of interest in a list
      list("question" = column,
           "established_mean"  = round(unname(t_test$estimate[1]),digits),
           "new_mean"  = round(unname(t_test$estimate[2]),digits),
           "mean_diff"= round(unname(diff(t_test$estimate)),digits),
           "p"        = round(t_test$p.value,digits),
           "lower"    = round(t_test$conf.int[1],digits),
           "upper"    = round(t_test$conf.int[2],digits)
      )
    }

    lst <- lapply(colnames(df_est[colnames(df_est) != cond_col]), clean_t_test)
    ret <- do.call(rbind.data.frame, lst)
    return(ret)
    }

#Example usage
data(mtcars)
dat <- mtcars
dat$condition_var <- ifelse(dat$mpg > median(dat$mpg), "above", "below") 
dat_est <- filter(dat, condition_var == "above")
dat_new <- filter(dat, condition_var == "below")
get_t_test(dat_est, dat_new, cond_col="condition_var")