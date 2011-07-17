isMts <- function(data){
  if (class(data)[1] == "mts")return(TRUE) else return(FALSE)
}