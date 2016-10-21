outlierKD <- function(df, var) {
  var_name <- eval(substitute(var),eval(df))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "\n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "\n")
  cat("Mean of the outliers:", round(mo, 2), "\n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "\n")
  cat("Mean if we remove outliers:", round(m2, 2), "\n")
  response <- readline(prompt="Do you want to remove outliers and to remove the rows? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df[as.character(substitute(var))] <- invisible(var_name)
    df<-na.omit(df)	
    assign(as.character(as.list(match.call())$df), df, envir = .GlobalEnv)
    cat("Outliers successfully removed\n")
    return(invisible(df))
  } else{
    cat("Outliers were not removed\n")
    return(invisible(var_name))
  }
  
}
#Load the File into a Variable
df1<-FINAL.DATASET.VERSION.3.1
#Call the Function with your dataframe and Attribute whose values you want to remove Outliers From.
# Here I have removed the Outliers from "TARGET_D", which is my dependent variable in Linear Regression.
outlierKD(df1, TARGET_D)
