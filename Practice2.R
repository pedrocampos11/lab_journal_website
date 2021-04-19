library(tidyverse)
diamonds3 <- readRDS("diamonds3.rds")

diamonds3 %>% 
  pivot_wider(names_from  = "dimension",
              values_from = "measurement") %>% 
  head(n = 5)
