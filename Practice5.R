diamonds5<-readRDS("diamonds5.rds")

diamonds5%>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep='')
