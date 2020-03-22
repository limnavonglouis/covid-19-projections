
clean_timeserie <- function(time_serie, start) {
  i = 1
  int = time_serie[i]
  while (int <= start) {
    time_serie <- time_serie[-1]
    int = time_serie[i]
  }
  return(as.numeric(time_serie))
}