dashboard <- function(){
  # Importing packages
  if(!require(shiny)){install.packages("shiny");library(shiny)}
  if(!require(shinydashboard)){install.packages("shinydashboard");library(shinydashboard)}
  if(!require(dplyr)){install.packages("dplyr");library(dplyr)}
  if(!require(plotly)){install.packages("plotly");library(plotly)}
  if(!require(ggplot2)){install.packages("ggplot2");library(ggplot2)}
  if(!require(gmodels)){install.packages("gmodels");library(gmodels)}
  if(!require(ggmosaic)){install.packages("ggmosaic");library(ggmosaic)}

  # names of data frame
  dfnamesft <- function(){
    dfnames <- ls(envir=.GlobalEnv) %>% sapply(get) %>% sapply(is.data.frame)
    dfnames <- names(dfnames[dfnames==T])
    return(dfnames)
  }
  dfnames <- dfnamesft()

  header <- dashboardHeader(
    title = 'Data Summary'
  )
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem('Data', tabName = 'menu_data'),
      menuItem('Plot', tabName = 'menu_plot'),
      absolutePanel(
        bottom = 20, right = 20, width = 300,
        draggable = TRUE,
        style = "opacity: 0.70;color: #FF0000",
        wellPanel(
          selectInput('sel_posi', 'Position(hist ONLY)', c('stack', 'identity', 'dodge')),
          htmlOutput('selectUIs'),
          sliderInput('sli_binw', 'Number of bin width(hist ONLY)', min = 0.1, max=2, step = 0.1, value = 0.2),
          htmlOutput('selectUIw')
        )
      )
    )
  )
  body <- dashboardBody(
    tabItems(
      tabItem(
        tabName = 'menu_data',
        fluidPage(
          column(
            width = 2,
            selectInput('sel_data','Data',dfnames),
            box(
              title = 'Row, Colum of Plots', solidHeader = T, collapsible = T,width = 13,
              numericInput('num_plotrow', 'Number of row', value = 7),
              numericInput('num_plotcol', 'Number of col', value = 8)
            ),
            box(
              title = 'Plot Parameters', solidHeader = T, collapsible = T,width = 13,
              selectInput('sel_ptype', 'Plot type', c('point', 'boxplot', 'histogram', 'mosaic')),
              htmlOutput('selectUIx'),
              htmlOutput('selectUIy'),
              htmlOutput('selectUIc'),
              htmlOutput('selectUIf'),
              htmlOutput('selectUIfacet'),
              radioButtons('radio_axis','Facet axis',c('col','row'),inline = T),
              sliderInput('sli_alpha', 'alpha', min = 0, max=1, value = 1),
              submitButton("Apply Changes")
            )
          ),
          tabBox(
            width = 10,
            tabPanel(
              title = 'Data',
              fluidPage(
                dataTableOutput('dt_raw')
              )
            ),
            tabPanel(
              title = 'Summary',
              fluidPage(
                fluidRow(
                  dataTableOutput('sum_cat')
                ),
                fluidRow(
                  dataTableOutput('sum_con')
                )
              )
            ),
            tabPanel(
              title = 'Plots',
              fluidPage(
                plotOutput('plot1',height="1000px")
              )
            ),
            tabPanel(
              title = 'Detailed Plot',
              fluidPage(
                plotlyOutput('detailplot',height="800px")
              )
            ),
            tabPanel(
              title = 'Cross Table',
              fluidPage(
                verbatimTextOutput('crosstab')
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'menu_plot',
        fluidRow(
          tabBox(
            width = 7,
            tabPanel('one', 'tab_one1'),
            tabPanel('two', 'tab_two1')
          )
        )
      )
    )
  )
  ui <- dashboardPage(header = header, sidebar = sidebar, body = body)
  server <- function(input, output){
    data.r <- reactive({
      return(get(input$sel_data))
    })
    summary.r <- reactive({
      return(Summary(data.r()))
    })
    output$dt_raw <- renderDataTable(data.r(),
                                     options = list(
                                       pageLength = 12,
                                       scrollX = T
                                     )
    )
    output$sum_cat <- renderDataTable(
      summary.r()[['Categorical']],
      options = list(
        pageLength = 12,
        scrollX = T
      )
    )
    output$sum_con <- renderDataTable(
      summary.r()[['Continuous']],
      options = list(
        pageLength = 12,
        scrollX = T
      )
    )
    output$plot1 <- renderPlot({
      idx_cat <- summary.r()$Categorical[summary.r()$Categorical$Cardinality<11,] %>% rownames()
      idx_con <- summary.r()$Continuous[summary.r()$Continuous$n>summary.r()$Continuous$Cardinality,] %>% rownames()
      par(mfrow=c(input$num_plotrow,input$num_plotcol))
      for(i in idx_cat){
        barplot(table(data.r()[[i]]), main = i)
      }
      for(j in idx_con){
        hist(data.r()[[j]], main = j, xlab = '')
      }
    })
    varnames.r <- reactive({
      return(names(data.r()))
    })
    varnames_num.r <- reactive({
      return(varnames.r()[sapply(data.r(), is.number)])
    })
    varnames_cha.r <- reactive({
      return(varnames.r()[!sapply(data.r(), is.number)])
    })
    facet.r <- reactive({
      if(input$sel_facet=='NULL') result <- NULL else if(input$radio_axis=='col'){
        result <- facet_grid(paste('~', input$sel_facet, sep = ''))
      } else if(input$radio_axis=='row'){
        result <- facet_grid(paste(input$sel_facet,'~.', sep = ''))
      }
      return(result)
    })
    output$selectUIx <- renderUI({
      selectInput('sel_x', 'X variable', c(1,varnames.r()), varnames.r()[1])
    })
    output$selectUIy <- renderUI({
      selectInput('sel_y', 'Y variable', c(1,varnames.r()), varnames.r()[2])
    })
    output$selectUIc <- renderUI({
      selectInput('sel_c', 'Color', c('NULL',varnames.r()), 'NULL')
    })
    output$selectUIf <- renderUI({
      selectInput('sel_f', 'Fill', c('NULL',varnames.r()), 'NULL')
    })
    output$selectUIfacet <- renderUI({
      selectInput('sel_facet', 'Facet', c('NULL',varnames_cha.r()), 'NULL')
    })
    output$selectUIs <- renderUI({
      selectInput('sel_s', 'Shape(point ONLY)', c('NULL',varnames_cha.r()), 'NULL')
    })
    output$selectUIw <- renderUI({
      selectInput('sel_w', 'Weight(mosaic ONLY)', c('NULL',varnames_cha.r()), 'NULL')
    })
    ptype.r <- reactive({
      switch (input$sel_ptype,
              'point' = geom_point(aes_string(x=input$sel_x, y=input$sel_y, shape=input$sel_s)
                                   , alpha=input$sli_alpha),
              'boxplot' = geom_boxplot(aes_string(x=input$sel_x, y=input$sel_y), alpha=input$sli_alpha),
              'histogram' = geom_histogram(aes_string(x=input$sel_x),
                                           position = input$sel_posi, alpha=input$sli_alpha,
                                           binwidth = input$sli_binw),
              'mosaic' = geom_mosaic(aes_(weight = input$sel_w, x = product(input$sel_x, input$sel_y)))
      )
    })
    output$detailplot <- renderPlotly({
      g <- ggplot(data.r()) + ptype.r() + aes_string(color=input$sel_c, fill=input$sel_c) +
        facet.r()
      ggplotly(g)
    })
    output$crosstab <- renderPrint({
      a <- CrossTable(data.r()[[input$sel_x]], data.r()[[input$sel_y]], dnn = c(input$sel_x, input$sel_y))
    })
  }
  return(shinyApp(ui, server))
}

