diamonds4 <- readRDS("diamonds4.rds")
diamonds4%>%
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = T)

