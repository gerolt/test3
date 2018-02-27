Summary <- function(data){
  # Loading Package
  if(!require(dplyr)){install.packages("devtools");library(dplyr)}
  reserved_words <- c('if', 'else', 'while', 'function', 'for', 'in', 'next',
                      'break',' TRUE', 'FALSE','NULL', 'Inf', 'NaN', 'NA')
  # Warning Message
  if(names(data) %in% reserved_words %>% sum != 0){
    stop('변수명은 예약어로 사용할 수 없습니다.\n 예약어 : if, else, while, function, for, in, next,
         break, TRUE, FALSE, NULL, Inf, NaN, NA')
  }
  Card <- function(x) unique(x) %>% length()
  NofNA <- function(x) is.na(x) %>% sum()
  GetMode <- function(x, order=1, type='value'){
    # mode count
    count_tab <- sort(table(x), decreasing = T)
    mode_count <- count_tab[order]
    # mode value
    value_tab <- names(count_tab)
    mode_value <- value_tab[order]
    if(type=='value') {
      result <- mode_value
    } else if(type=='count'){
      result <- mode_count
    }
    names(result) <- NULL
    return(result)
  }
  # Split
  con <- data[sapply(data, is.number)]
  if(length(con)==0) con$idx <- 1:nrow(con)
  cat <- data[!sapply(data, is.number)]
  if(length(cat)==0) cat$idx <- as.character(1:nrow(cat))
  # Continuous
  countdat <- sapply(con, length) %>% tbl_df()
  nadat <- sapply(con, NofNA) %>% tbl_df()
  carddat <- sapply(con, Card) %>% tbl_df()
  mindat <- sapply(con, min, na.rm=T) %>% tbl_df()
  maxdat <- sapply(con, max, na.rm=T) %>% tbl_df()
  mediandat <- sapply(con, median, na.rm=T) %>% tbl_df()
  meandat <- sapply(con, mean, na.rm=T) %>% tbl_df()
  q1dat <- sapply(con, quantile, 0.25, na.rm=T) %>% tbl_df()
  q3dat <- sapply(con, quantile, 0.75, na.rm=T) %>% tbl_df()
  stddat <- sapply(con, sd, na.rm=T) %>% tbl_df()
  # Categorical
  countdf <- sapply(cat, length) %>% tbl_df()
  nadf <- sapply(cat, NofNA) %>% tbl_df()
  carddf <- sapply(cat, Card) %>% tbl_df()
  mode1df <- sapply(cat, GetMode, order=1, type='value') %>% tbl_df()
  mode1cdf <- sapply(cat, GetMode, order=1, type='count') %>% tbl_df()
  mode2df <- sapply(cat, GetMode, order=2, type='value') %>% tbl_df()
  mode2cdf <- sapply(cat, GetMode, order=2, type='count') %>% tbl_df()
  # result
  con_df <- cbind(names(con), countdat, nadat, nadat/countdat, carddat, carddat/countdat,
                  mindat, q1dat, meandat, mediandat, q3dat, maxdat, stddat)
  cat_df <- cbind(names(cat), countdf, nadf, nadf/countdf, carddf, carddf/countdf,
                  mode1df, mode1cdf, mode1cdf/countdf,
                  mode2df, mode2cdf, mode2cdf/countdf)
  names(con_df) <-
    c('Variable','n', 'Missing', 'Missing.R', 'Cardinality', 'Cardinality.R',
      'Minimum', 'Q1', 'Mean', 'Median', 'Q3', 'Maximum', 'Std')
  names(cat_df) <-
    c('Variable','n', 'Missing', 'Missing.R', 'Cardinality', 'Cardinality.R',
      'Mode1', 'Mode1.C', 'Mode1.R', 'Mode2', 'Mode2.C', 'Mode2.R')
  invisible(list(Categorical = cat_df, Continuous = con_df))
  }
#yes
