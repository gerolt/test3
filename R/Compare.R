Compare <- function(data,y,by){
  if(!require(data.table)){install.packages("data.table");library(data.table)}
  data <- data.table(data)
  data[,
       list(
         mean = mean(get(y)),
         std = sd(get(y)),
         length = length(get(y))
       ),
       by = by
       ]
}
