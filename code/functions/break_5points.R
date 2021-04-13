
break_points <- function(df, colname, round, minimum){
  # Create the 5 numeric break points to legends
  # 
  # df = data.frame
  # colname = column name
  # round = number of digits
  # minimum = lower value in legent
  library(rlang)
  max_val <- df %>% ungroup() %>% select({{ colname }}) %>% max(na.rm = TRUE)
  
  c(
    round(minimum, round),
    round(minimum + ((max_val - minimum) / 4), round),
    round(minimum + ((max_val - minimum) * 2 / 4), round),
    round(minimum + ((max_val - minimum) * 3 / 4), round),
    round(max_val, round)
  )
}
