example <- '65,8;65,9;66;72,1;72,2;73,7;74;75;75;75,8;76;77,3;77,5;78;78,2;78,7;78,9;79,2;79,4;79,8;79,9;80,1;80,2;80,6;81;82,2;82,2;82,3;82,6;83,9;84,5;85,6;85,9;86,4;87,2;87,3;87,3;87,3;87,6;88;89;89,2;89,3;89,6;89,8;90,8;92,7;93,2;95,7;96,4'

getDataType <- function(data) {
  dataList <- unlist(strsplit(data,";"))
  size <- length(dataList)
  if (size == 0)
    return(NULL)
  
  dataTypes <- list()
  dataType <- NULL

  for(i in 1:size)
    dataTypes[[i]] <- !is.na(as.integer(dataList[[i]]))
  qualitativa <- grepl('[A-Za-z]', dataList)
  
  if (TRUE %in% qualitativa)
    dataType <- 'Qualitativas'
  else if (FALSE %in% dataTypes)
    dataType <- 'Continuas'
  else
    dataType <- 'Discretas'
      
  dataType
}
