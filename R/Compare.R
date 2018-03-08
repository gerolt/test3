Compare <- function(data,group,y,order='mean'){
  if(!require(dplyr)){install.packages("dplyr");library(dplyr)}
  return(
    data %>%
      group_by_at(group) %>%
      summarise(mean = mean(get(y), na.rm = T),
                sd = sd(get(y), na.rm = T),
                length = length(get(y))) %>%
      arrange_at(order, desc)
  )
}
