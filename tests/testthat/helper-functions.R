#helper-functions.R
#round_n
round_n <- function(x, n=3) {
  if (!is.na(x)) {
    if (x >= 1000 | x<=0.0005) {
      # if x>=1000, convert value to scientific notation
      formatC(x, format = "e", digits = 1)
    } else { # else, round the value to 3 decimal places
      format(round(x, n), nsmall = 3)
    }
  } else {
    return(NA)
  }
}
round_n <- Vectorize(round_n)