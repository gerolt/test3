Summary <- function(data){
  if(!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}

  # Warning Message
  reserved_words <- c('if', 'else', 'while', 'function', 'for', 'in', 'next',
                      'break',' TRUE', 'FALSE','NULL', 'Inf', 'NaN', 'NA')
  if(sum(names(data) %in% reserved_words) != 0){
    stop('변수명은 예약어로 사용할 수 없습니다.\n 예약어 : if, else, while,
         function, for, in, next, break, TRUE, FALSE, NULL, Inf, NaN, NA')
  }
  Card <- function(x) length(table(x))
  NofNA <- function(x) sum(is.na(x))
  # Split
  con <- data[sapply(data, is.number)]
  cat <- data[!sapply(data, is.number)]

  # Quantitative
  message('Calculating quantitative data..')
  CalcCon <- function(x){
    if(length(x)==0){
      message('No Quantitative data')
      result <- NA
    } else{
      con_n <- sapply(x, length)
      con_na <- sapply(x, NofNA)
      con_card <-sapply(x, Card)
      result <- data.frame(
        stringsAsFactors = F,
        names(x), con_n,  con_na,
        con_na/con_n, con_card, con_card/con_n,
        sapply(x, min, na.rm=T), sapply(x, Quantile, 0.25),
        sapply(x, median, na.rm=T), sapply(x, mean, na.rm=T),
        sapply(x, Quantile, 0.75), sapply(x, max, na.rm=T),
        sapply(x, sd, na.rm=T)
      )
      names(result) <-
        c('Variable','n', 'Missing', 'Missing.R','Cardinality','Cardinality.R',
          'Minimum', 'Q1', 'Mean', 'Median', 'Q3', 'Maximum', 'Std')
    }
    return(result)
  }
  con_dat <- CalcCon(con)

  # Qualitative
  message('Calculating qualitative data..')
  CalcCat <- function(x){
    if(length(x)==0){
      message('No Qualitative data')
      result <- NA
    } else{
      cat_n <- sapply(x, length)
      cat_na <- sapply(x, NofNA)
      cat_card <-sapply(x, Card)
      catm1c <- sapply(x, GetMode, order=1, type='count')
      catm2c <- sapply(x, GetMode, order=2, type='count')
      result <- data.frame(
        stringsAsFactors = F,
        names(x),
        cat_n, cat_na, cat_na/cat_n, cat_card, cat_card/cat_n,
        sapply(x, GetMode, order=1, type='value'),
        catm1c, catm1c/cat_n,
        sapply(x, GetMode, order=2, type='value'),
        catm2c, catm2c/cat_n
      )
      names(result) <-
        c('Variable','n', 'Missing', 'Missing.R','Cardinality','Cardinality.R',
          'FirstMode', 'FirstMode.C','FirstMode,R','SecondMode','SecondMode.C',
          'SecondMode.R')
      result$FS.R <- (result$FirstMode.C + result$SecondMode.C) / result$n
    }
    return(result)
  }
  cat_dat <- CalcCat(cat)

  output <- list(Qualitative = cat_dat, Quantitative = con_dat)
  message('Done')

  # missing comment
  cat('Missig Problem : "',
      sum(c(output$Quantitative$Missing.R>=0.3,
            output$Qualitative$Missing.R>=0.3))/length(data),
      '% " (The number of variables which missing ratio is over than 30%)'
  )
  misspbm <- c(
    output$Quantitative[which(output$Quantitative$Missing.R>=0.3),]$Variable,
    output$Qualitative[which(output$Qualitative$Missing.R>=0.3),]$Variable
  )
  misspbm <- if(length(misspbm)==0) misspbm <- NA else misspbm
  cat('\nMissing PBM Variables : "',
      paste(misspbm, collapse = ', '),'(', length(table(misspbm)), ')',
      '" (Names of variables which missing ratio is over than 30%)'
  )

  # cardinality comment
  cat('\n\nCardinality Problem : "',
      sum(c(output$Qualitative$Cardinality>=20,
            output$Quantitative$Cardinality.R<0.2))/length(data),
      '% " (The number of variables which has cardinality problem)'
  )
  cardpbm <- na.omit(unique(c(
    output$Quantitative[which(output$Qualitative$Cardinality>=20),]$Variable,
    output$Qualitative[which(output$Quantitative$Cardinality.R<0.2),]$Variable
  )))
  cardpbm <- if(length(cardpbm)==0) cardpbm <- NA else cardpbm
  cat('\nCardinality PBM Variables : "',
      paste(cardpbm, collapse = ', '),'(', length(table(cardpbm)), ') "'
  )
  cat('\nCardinality PBM(Qualitative) : Cardinality is more than 20') ##
  cat('\nCardinality PBM(Quantitative) : Cardinality Ratio is less than 20%') ##

  # Qualitative Plot
  correction <- ifelse(max(output$Qualitative$Cardinality)>20,
                       max(output$Qualitative$Cardinality),20)
  output$Qualitative.Plot <-
    ggplot(output$Qualitative) +
    geom_bar(
      aes(x=reorder(Variable, -output$Qualitative$Missing.R), y=Missing.R),
      stat = 'identity', fill='dodgerblue3', alpha=0.8
    ) +
    geom_bar(
      aes(x=Variable, y=-Cardinality/correction),
      stat = 'identity', fill='deeppink4',alpha=0.8
    ) +
    scale_y_continuous(
      name = 'Missing Ratio(%)',
      breaks = c(0,0.5,1),
      limits = c(-1,1),
      labels = c(0, 0.5,1)*100,
      sec.axis = sec_axis(
        ~.*correction, name = 'Cardianlity',
        breaks = c(-1,-0.5,0) * correction,
        labels = c(-1,-0.5,0) * -correction
      )
    ) +
    labs(x='Variables') +
    geom_hline(yintercept = 0, size=1.3, linetype='dashed')
  # Qualitative Plot
  output$Quantitative.Plot <-
    ggplot(output$Quantitative) +
    geom_bar(
      aes(x=reorder(Variable, -output$Quantitative$Missing.R), y=Missing.R),
      stat = 'identity', fill='dodgerblue3', alpha=0.8
    ) +
    geom_bar(
      aes(x=Variable, y=-Cardinality.R),
      stat = 'identity', fill='deeppink4',alpha=0.8
    ) +
    scale_y_continuous(
      name = 'Missing Ratio(%)',
      breaks = c(0,0.5,1),
      limits = c(-1,1),
      labels = c(0,0.5,1)*100,
      sec.axis = sec_axis(
        ~., name = 'Cardianlity Ratio(%)',
        breaks = c(0,-0.5,-1),
        labels = c(0,-0.5,-1)*100
      )
    ) +
    labs(x='Variables') +
    geom_hline(yintercept = 0, size=1.3, linetype='dashed')

  invisible(output)
  }
