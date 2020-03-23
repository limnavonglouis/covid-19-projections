
read_data_from_github <- function(url) {
  
  text_base <- read_html(url)
  csv <- html_text(text_base)
  data <- read.table(text = csv, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  
  return(data)
}

