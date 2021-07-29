# Basic information about the dataset
df_info <- function(df){
  dim <- dim(df)
  numeric_var <- sum(unlist(lapply(df, is.numeric)))
  var_types <- unlist(lapply(df, class))
  cat_var <- sum(var_types %in% c("factor", "character"))
  missing_any <- any(is.na(df))
  info <- data.frame(
    instances = dim[[1]],
    variables = dim[[2]],
    numeric = numeric_var,
    categorical = cat_var,
    missing_data = ifelse(missing_any == TRUE, "present", "absent")
  )
  
  return(info)
}