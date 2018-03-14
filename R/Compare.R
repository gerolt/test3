Compare <- function(data,y,by){
  if(!require(data.table)){install.packages("data.table");library(data.table)}
  data <- data.table(data)
  data[,
       list(
         #mean = mean('price'),
         std = sd(y)
       ),
       by = by
       ]
}
