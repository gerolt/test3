Summary <- function(data){
  # Warning Message
  reserved_words <- c('if', 'else', 'while', 'function', 'for', 'in', 'next',
                      'break',' TRUE', 'FALSE','NULL', 'Inf', 'NaN', 'NA')
  if(sum(names(data) %in% reserved_words) != 0){
    stop('변수명은 예약어로 사용할 수 없습니다.\n 예약어 : if, else, while, function, for, in, next,
         break, TRUE, FALSE, NULL, Inf, NaN, NA')
  }
  Card <- function(x) length(table(x))
  NofNA <- function(x) sum(is.na(x))
  # Split
  con <- data[sapply(data, is.number)]
  if(length(con)==0) con$idx_number <- 1:nrow(con)
  cat <- data[!sapply(data, is.number)]
  if(length(cat)==0) cat$idx_number <- as.character(1:nrow(cat))
  # Quantitative
  con_n <- sapply(con, length)
  con_na <- sapply(con, NofNA)
  con_card <-sapply(con, Card)
  con_dat <- data.frame(
    stringsAsFactors = F,
    names(con), con_n,  con_na,
    con_na/con_n, con_card, con_card/con_n,
    sapply(con, min, na.rm=T), sapply(con, max, na.rm=T),
    sapply(con, median, na.rm=T), sapply(con, mean, na.rm=T),
    sapply(con, quantile, 0.25, na.rm=T), sapply(con, quantile, 0.75, na.rm=T),
    sapply(con, sd, na.rm=T)
  )
  # Qualitative
  cat_n <- sapply(cat, length)
  cat_na <- sapply(cat, NofNA)
  cat_card <-sapply(cat, Card)
  catm1c <- sapply(cat, GetMode, order=1, type='count')
  catm2c <- sapply(cat, GetMode, order=2, type='count')
  cat_dat <- data.frame(
    stringsAsFactors = F,
    names(cat),
    cat_n, cat_na, cat_na/cat_n, cat_card, cat_card/cat_n,
    sapply(cat, GetMode, order=1, type='value'),
    catm1c, catm1c/cat_n,
    sapply(cat, GetMode, order=2, type='value'),
    catm2c, catm2c/cat_n
  )
  names(con_dat) <-
    c('Variable','n', 'Missing', 'Missing.R', 'Cardinality', 'Cardinality.R',
      'Minimum', 'Q1', 'Mean', 'Median', 'Q3', 'Maximum', 'Std')
  names(cat_dat) <-
    c('Variable','n', 'Missing', 'Missing.R', 'Cardinality', 'Cardinality.R',
      'FirstMode', 'FirstMode.C', 'FirstMode,R', 'SecondMode', 'SecondMode.C',
      'SecondMode.R')
  cat_dat$FS.R <- (cat_dat$FirstMode.C + cat_dat$SecondMode.C) / cat_dat$n
  return(list(Qualitative = cat_dat, Quantitative = con_dat))
  }
