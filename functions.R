dictionary_clean = function(dictionary_partition){
  var_index = grep("^D ", dictionary_partition)
  vars = dictionary_partition[var_index]
  # look at varying length vars
  vars = strsplit(vars, "\\s+")
  l = unlist(lapply(vars, length))
  vars2 = lapply(vars, function(x) x[1:4])
  varsDat = do.call(rbind.data.frame, c(vars2, stringsAsFactors=F))
  names(varsDat) = c("data", "label", "size", "begin")
  # remove fillers
  varsDat = varsDat[-which(varsDat$label=="FILLER"),]
  # character to numeric
  varsDat$size = as.integer(varsDat$size)
  varsDat$begin = as.integer(varsDat$begin)
  return(varsDat)
}