#### IMPORTING LIBRARIES ####

library(plotly)
library(rmarkdown)
library(tinytex)
library(knitr)
library(ggplot2)
library(gridExtra)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinyalert)
library(flexdashboard)
library(misty)
library(shinydashboard)
library(correlation)
library(DataExplorer)
library(funtimes)
library(Kendall)
library(nlme)
library(forecast)
library(imputeTS)
library(tidyverse)
library(DT)
library(tseries)
require(gtools)
library(ggcorrplot)
library(psych)
library(readit)
library(FactoMineR)
library(lavaan)
library(Conigrave)
library(Amelia )
library(Boruta)
library(gimme)
library(apaTables)
library(iml)
library(lattice)
library(pastecs)
require(lme4)
require(arm)
require(multilevel)
library(h2o) # a java-based implementation of random forest
require(psych)
require (Hmisc)
library(data.table)
library(writexl)
library(patchwork)
library(ranger)   # a c++ implementation of random forest
library(AmesHousing)
library(rsample)   # for resampling procedures
library(caret)
library(tree)
library(randomForest)
library(party)  # ctree
library(MASS) # messes up your select in some analysis
library(rpart)
library(rpart.plot)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(MuMIn)
library(behaviorchange)
library(metaforest)
library(shinybusy)

library(shinymanager)
#remotes::install_github("MatthieuStigler/matPkg")
#library(matPkg)
# indSEM
library(gimme)
library(tidyr)
library(tidyverse)
library(magrittr)
library(readit)
# DV Change
library (changepoint)
library(trend)
library(EnvCpt)
library(shiny)
library(stringr)
library(crayon)
#devtools::install_github("ricardo-bion/ggradar",
                      #   dependencies = TRUE)
library(ggradar)
library(fmsb)


#devtools::install_github("ramnathv/rCharts")
library(rCharts)
# API
library(plumber)
library(httr)
library(jsonlite)


## explanation table data
explanation_table = read.csv(normalizePath("Data/explanation_table.csv"))

#### UI SETUP ####

source('UI.R')
source("api_functions.R")

ui <- UI

#### SERVER SETUP ####

server <- function(input, output, session) {

  source("functions.R")

  ## SETTING UP INPUT SIDEBAR AND GLOBAL VARIABLES ####
  ### reading file ####
  # reading the file
  values <- reactiveValues(df_data = NULL)

  observeEvent(input$data_file, {
    values$df_data <- read.csv(input$data_file$datapath)
  })

  observeEvent(input$mgLogin, {
    showModal(modalDialog(
      tags$h2("Please enter your username and password for you MindGrapher account."),
      textInput("username", "Username"),
      textInput("password", "Password"),
      footer=tagList(
        actionButton("mg_submit", "Login"),
        modalButton("Cancel")
      )
    ))
  })


  observeEvent(input$mg_submit, {
    # get data using API
    show_modal_spinner(
      spin = "cube-grid",
      color = "#F28C28",
      text = "Please wait while we get your data..."
    )
    login_details <- api_login(username = input$username, password = input$password)
    user_details <- get_user_details(login_details$userId, login_details$token)
    user_answers <- get_user_answers(login_details$userId, login_details$token)
    data_reshaped <- get_reshaped_data(user_details, user_answers)
    values$df_data <- data_reshaped
    removeModal()
  })

  file_data <- reactive({
    values$df_data
  })


  # selecting dependent variable
    output$dependent_variable <- renderUI({
        selectInput(inputId = "dependent_variable",
                    label = "What outcome would you like to focus on?",
                    choices = colnames(file_data()),
                    selected = colnames(file_data())[3],
                    multiple = FALSE)
    })

    # selecting independent variables
    output$independent_variables <- renderUI({
        selectInput(inputId = "independent_variables",
                    label = "What processes would you like focus on?",
                    choices = colnames(file_data()),
                    multiple = TRUE)
    })

    # selecting individual
    output$individual <- renderUI({
        selectInput(inputId = "individual",
                    label = "Select Individual",
                    choices = unique(file_data()$ID),
                    multiple = FALSE)
    })

    # updating select inputs
    observeEvent( input$raw_execute, {
        updateSelectInput(session,
                        "dependent_variable",
                        choices = colnames(file_data()),
                        selected = isolate(colnames(file_data())[3]))
        updateSelectInput(session,
                        "independent_variables",
                        choices = colnames(file_data())[!(colnames(file_data()) %in% c(colnames(file_data())[3]))],
                        selected = isolate(input$independent_variables))
        updateSelectInput(session,
                        "individual",
                        choices = unique(file_data()$ID),
                        selected = isolate(input$individual))
    })

  ### setting up empty spaces outputs ####
  # needs to be different output IDs otherwise the app crashes
  output[["empty_space1"]] <- renderUI({
    HTML("  ")
  })
  output[["empty_space2"]] <- renderUI({
    HTML("  ")
  })
  output[["empty_space3"]] <- renderUI({
    HTML("  ")
  })
  output[["empty_space4"]] <- renderUI({
    HTML("  ")
  })
  output[["empty_space5"]] <- renderUI({
    HTML("  ")
  })
  output[["empty_space6"]] <- renderUI({
    HTML("  ")
  })
  output[["empty_space7"]] <- renderUI({
    HTML("  ")
  })
  output[["empty_space8"]] <- renderUI({
    HTML("  ")
  })
  output[["empty_space9"]] <- renderUI({
    HTML("  ")
  })
  output[["empty_space10"]] <- renderUI({
    HTML("  ")
  })

  # Summary
    ## Raw data tab

    observeEvent(input$raw_execute, {
      req(input$individual, input$dependent_variable, input$independent_variables)
      focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables)
      print(lapply(focus_data, class))
       # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
      ind <- input$individual
      full_data <- file_data()
      output$raw_data <- renderUI({
        shiny::validate(
          need(length(focus_data[,3] %>% na.omit()) > 2,
               "Sorry, you don't have enough data to do this analysis."))
          tagList(
            br(),
            h3(paste0('Raw data for ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
            datatable(focus_data,
                extensions = c('Buttons', 'FixedHeader','FixedColumns'),
                options = list(dom = 'Blfrtip', scrollX = TRUE,
                                buttons = c('csv', 'excel', 'print'), rownames = FALSE)),
            br(),
            h3(paste0('Summary for ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
            DT::datatable(get_variable_stat(focus_data, full_data),
                    extensions = c('Buttons', 'FixedHeader','FixedColumns'),
                    options = list(dom = 'Blfrtip', scrollX = TRUE,
                                    buttons = c('csv', 'excel', 'print'), rownames = FALSE)),
            br(),
            h3(paste0('Standardized data for ', as.character(ind), '( Z score / scaled value )'), style = 'text-align: center; background: #fcc38b'),
            DT::datatable(standardise_df(focus_data),
                    extensions = c('Buttons', 'FixedHeader','FixedColumns'),
                    options = list(dom = 'Blfrtip', scrollX = TRUE,
                                    buttons = c('csv', 'excel', 'print'), rownames = FALSE))
          )
        })
    })

  ## Norms tab

  observeEvent(input$norms_execute, {
    req(input$individual, input$dependent_variable, input$independent_variables)
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
    ind <- input$individual
    full_data <- file_data()
    output$radar_plot <- renderPlotly({
      plot_radar(get_variable_stat(focus_data, full_data))
    })
    output$norms <- renderUI({
      tagList(
        br(),
        h3(paste0('Norms for ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
        shiny::validate(
          need(length(focus_data[,3] %>% na.omit()) > 2,
               "Sorry, you don't have enough data to do this analysis.")),
        plotlyOutput("radar_plot", height = "100%")
      )
    })
  })

  ## Trends tab
    output$trends_outcome <- renderUI({
      req(input$individual, input$dependent_variable, input$independent_variables)
      focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables)
        selectInput(inputId = "outcome",
                    label = "Select outcome variable",
                    choices = colnames(focus_data),
                    selected = colnames(focus_data)[3],
                    multiple = FALSE)
    })
    output$trends_process <- renderUI({
      req(input$individual, input$dependent_variable, input$independent_variables)
      focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables)
        selectInput(inputId = "process",
                    label = "Select process variable",
                    choices = colnames(focus_data),
                    selected = colnames(focus_data)[4],
                    multiple = FALSE)
    })
    observe({
        updateSelectInput(session,
                        "outcome",
                        choices = colnames(file_data()),
                        selected = isolate(colnames(file_data())[3]))
        updateSelectInput(session,
                        "process",
                        choices = colnames(file_data())[!(colnames(file_data()) %in% c(colnames(file_data())[3]))],
                        selected = isolate(input$independent_variables[1]))
    })

    observeEvent(input$trends_execute, {
        req(input$individual, input$dependent_variable, input$independent_variables)
      focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
        ind <- input$individual
        outcome <- input$outcome
        process <- input$process
        full_data <- file_data()
        output$trend_plot <- renderPlotly({
            PlotData <- full_data %>%
                filter(ID %in% ind) %>%
                dplyr::select(Time, outcome, process) %>%
                na_interpolation

            ggplot(data = PlotData, aes(x = Time)) +
                geom_line(aes(y = get(outcome), colour = outcome)) +
                geom_line(aes(y = get(process), colour = process)) +
                scale_colour_manual("",
                                    breaks = c(outcome, process),
                                    values = c("red", "blue")
                ) +
                xlab(" ") +
                scale_y_continuous("Intensity", limits = c(0,100)) +
                labs(title="Outcome and process over time")+
                theme(plot.title=element_text(hjust=0.5))
        })
        output$trends <- renderUI({
            tagList(
                br(),
                h3(paste0('Trends for ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
                shiny::validate(
                  need(length(full_data[,outcome] %>% na.omit()) > 2,
                       "Sorry, you don't have enough data to do this analysis.")),
                plotlyOutput("trend_plot", height = "100%")
            )
        })
    })

  # Analysis

  ## Linear-Importance tab

    observeEvent(input$linear_execute, {
        req(input$individual, input$dependent_variable, input$independent_variables)
      focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
        ind <- input$individual
        dep_var <- input$dependent_variable

        output$linear_plot <- renderPlot({
          results = run_arima_full_dataset(focus_data, model = "automate", lagged=0)
          arima_summary <- format_arima(results, focus_data, file_data())
            if (is.null(arima_summary)) {
                invisible()
            }else{
                forest_plot <- plot_forest(arima_summary)
                forest_plot
            }
        })
        output$linear <- renderUI({
            req(input$linear_execute)
            tagList(
                br(),
                h3(paste0('Importance of Process for Client ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
                h4(paste0("Determinent of ", as.character(dep_var)), style = "text-align: left"),
                h4("Determinent Norm Percentile", style = "text-align: right"),
                shiny::validate(
                  need(length(focus_data[,3] %>% na.omit()) > 2,
                       "Sorry, you don't have enough data to do this analysis.")),
                plotOutput("linear_plot")
            )
        })
    })

  ## Well-being chart tab

  observeEvent(input$wellbeingchart_execute, {
    req(input$individual, input$dependent_variable, input$independent_variables)
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables)
    ind <- input$individual
    dep_var <- input$dependent_variable

    output$wellbeingchart <- renderPlot({
      results = run_arima_full_dataset(focus_data, model = "automate", lagged=0)
      arima_summary <- format_arima(results, focus_data, file_data())
      if (is.null(arima_summary)) {
        invisible()
      }else{
        plot_wellbeingchart(arima_summary, dep_var)
      }
    })
    output$wellbeingchart_tab <- renderUI({
      req(input$wellbeingchart_execute)
      tagList(
        br(),
        h3(paste0('Well-being Mountain for Client ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
        h4("The higher the process is on the mountain chart, the more important it is to your well-being. Bigger font means the process has a significant influence, whereas a smaller font means we have not established if the process is significant or not."),
        plotOutput("wellbeingchart")
      )
    })
  })

  ## I-Boruta tab

  observeEvent(input$boruta_execute, {
    req(input$individual, input$dependent_variable, input$independent_variables)
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables)# setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
    ind <- input$individual
    set.seed(8675309)
    output$boruta_plot <- renderPlotly({
      boruta_results <- run_boruta(focus_data)$stat_results
        p <- boruta_results %>%
        mutate(decision = factor(decision,
                                levels = c("Confirmed", "Tentative", "Rejected"))) %>%
        ggplot(aes(x = meanImp,
                    y = reorder(variable, meanImp),
                    fill = decision,
                    text = paste("Decision:", decision,
                                "\nmeanImp:",round(meanImp, 3)))) +
        geom_col() +
        scale_fill_manual(values = c("Confirmed" = "#87bc45",
                                    "Tentative" = "#edbf33",
                                    "Rejected" = "#ea5545")) +
        labs(x = "Mean Importance", y = "", fill = "Decision") +
        theme_minimal()
        ggplotly(p, tooltip = c("text"))
    })
    output$boruta <- renderUI({
        tagList(
          br(),
          h3(paste0('Importance of Process for Client ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
          shiny::validate(
            need(length(focus_data[,3] %>% na.omit()) > 2,
                 "Sorry, you don't have enough data to do this analysis.")),
          plotlyOutput("boruta_plot")
        )
    })
  })

  ## Change tab

    observeEvent(input$change_execute, {
        req(input$individual, input$dependent_variable, input$independent_variables)
      focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
        ind <- input$individual
        dep_var <- input$dependent_variable

        output$change_scatterplot <- renderPlot({
          ggplot(focus_data, aes_string(x = "Time", y = dep_var)) +
              geom_point(size = 3) +
              geom_smooth() +
              scale_color_brewer(palette = "Set1")
        })

        output$change_pointmeanplot <- renderPlot({
          TimeSeriesData <- na_interpolation(focus_data[,3])
          fit_changepointMean <- cpt.mean(TimeSeriesData)
          plot(fit_changepointMean)
        })
        output$change_pointvarianceplot <- renderPlot({
          TimeSeriesData <- na_interpolation(focus_data[,3])
          fit_changepointVariance<- cpt.var(TimeSeriesData)
          plot(fit_changepointVariance)
       })
       output$dv_change <- renderUI({
         shiny::validate(
           need(length(focus_data[,3] %>% na.omit()) > 2,
                "Sorry, you don't have enough data to do this analysis."))

        TimeSeriesData <- na_interpolation(focus_data[,3])
        notrend_test_linear <- notrend_test(TimeSeriesData)
        notrend_test_linear_statistic <- as.character(round(notrend_test_linear$statistic, 2))
        notrend_test_linear_pval <- as.character(round(notrend_test_linear$p.value, 3))

        notrend_test_any <- notrend_test(TimeSeriesData, test = "MK", factor.length = "adaptive.selection")
        notrend_test_any_statistic <- as.character(round(notrend_test_any$statistic, 2))
        notrend_test_any_pval <- as.character(round(notrend_test_any$p.value, 3))

        box_test <- Box.test(TimeSeriesData, type = "Box-Pierce", lag = 1)
        box_test_statistic <- as.character(round(box_test$statistic, 2))
        box_test_pval <- as.character(round(box_test$p.value, 3))

        tagList(
            br(),
            h3(paste0('Change Analysis for Client ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
            fillCol(height = 100, flex = c(2,2,2),
              fillRow(
                HTML(paste("<b>Test for Linear Change</b>", "<br>Test Value:", notrend_test_linear_statistic, "<br>P-Value: ", notrend_test_linear_pval)),
                HTML(paste("<b>Test for any trend</b>", "<br>Test Value:", notrend_test_any_statistic, "<br>P-Value: ", notrend_test_any_pval)),
                HTML(paste("<b>Test for Change in Variance</b> ", "<br>Test Value:", box_test_statistic, "<br>P-Value: ", box_test_pval))
              )
            ),
            br(),
            plotOutput("change_scatterplot"),
            plotOutput("change_pointmeanplot"),
            plotOutput("change_pointvarianceplot")
        )
      })
    })

  # Advanced Analysis (these need boruta_tab and arima_summary)

  ## Decision Tree tab

  dt_rv <- reactiveValues(dat=NULL, type=NULL)

  output$dt_header <- renderUI({
    ind <- input$individual
    dep_var <- input$dependent_variable
    ind_vars <- input$independent_variables
    dt_rv$individual <- ind
    dt_rv$dependent_variable <- dep_var
    dt_rv$independent_variables <- ind_vars
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
        tagList(
            br(),
            h3(paste0('Decision Tree for Client ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
            br(),
            shiny::validate(
              need(length(focus_data[,3] %>% na.omit()) > 2,
                   "Sorry, you don't have enough data to do this analysis.")),
            p("This analysis requires results from the Linear and Machine Learning Analysis."),
            p("1. Click on the 'Prune Variables' button to get the choices from the Linear and Machine Learning Analysis."),
            p("2. Then select your choices."),
            p("3. Click on 'Plot' button to see the plot."),
            br()
        )
    })

  observeEvent(input$dtVar_prune, {
    req(input$individual, input$dependent_variable, input$independent_variables)
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
    ind <- input$individual
    dep_var <- input$dependent_variable
    ind_vars <- input$independent_variables
    full_data <- file_data()

    set.seed(8675309)
    boruta_tab <- run_boruta(focus_data)$stat_results %>%
        mutate(decision = factor(decision,
                                # ordering the decision for better visualization
                                levels = c("Confirmed",
                                            "Tentative",
                                            "Rejected")))
    arima_summary_tab <- format_arima(run_arima_full_dataset(focus_data, model = "automate", lagged=0), focus_data, full_data)

    output$confirmed_UI <- renderUI({
        choices = boruta_tab$variable[boruta_tab$decision=="Confirmed"]
        pickerInput("confirmed","Confirmed",
                    choices = choices,
                    options = list(
                    `style` = HTML("color: #FFFFFF") ,
                    `width` = '85%',
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Select None",
                    `select-all-text` = "Select All",
                    `none-selected-text` = ifelse(length(choices)==0,
                                                    "No variable",
                                                    paste(length(choices), "Options")),
                    `selected-text-format`= "count",
                    `count-selected-text` = "{0} Selected"
                    ),
                    multiple = TRUE)
    })

    output$tentative_UI <- renderUI({
        choices = boruta_tab$variable[boruta_tab$decision=="Tentative"]
        pickerInput("tentative","Tentative",
                    choices = choices,
                    options = list(
                    `style` = HTML("color: #FFFFFF") ,
                    `width` = '85%',
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Select None",
                    `select-all-text` = "Select All",
                    `none-selected-text` = ifelse(length(choices)==0,
                                                    "No variable",
                                                    paste(length(choices), "Options")),
                    `selected-text-format`= "count",
                    `count-selected-text` = "{0} Selected"
                    ),
                    multiple = TRUE)
    })

    output$significant_UI <- renderUI({
        arima_sig <- arima_summary_tab %>%
          mutate(Decision = ifelse((L < 0 & U < 0) |
                                    (L > 0 & U > 0), "Significant", "Insignificant")) %>%
          dplyr::filter(Decision == "Significant") %>%
          dplyr::select(variable) %>%
          as.vector()
        choices = arima_sig$variable
        pickerInput("significant","Significant in ARIMA",
                    choices = choices,
                    options = list(
                    `style` = HTML("color: #FFFFFF") ,
                    `width` = '85%',
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Select None",
                    `select-all-text` = "Select All",
                    `none-selected-text` = ifelse(length(choices)==0,
                                                    "No variable",
                                                    paste(length(choices), "Options")),
                    `selected-text-format`= "count",
                    `count-selected-text` = "{0} Selected"
                    ),
                    multiple = TRUE)
    })

    output$dependent_UI <- renderUI({
        pickerInput("dependent","Dependent",
                    choices = colnames(focus_data)[!colnames(focus_data) %in% boruta_tab$variable],
                    selected = dep_var,
                    options = list(
                    `style` = HTML("color: #FFFFFF"),
                    `width` = '85%',
                    `actions-box` = TRUE,
                    `none-selected-text` = "Select One"
                    ),
                    multiple = F)
    })

    output$selectCP <- renderUI({
        isolate({
        sliderInput("cp","Complexity Parameter",
                    min=0.0001, max=0.3, value = 0.001, step = 0.001)
        })
    })

    output$dt_execute <- renderUI({
      actionButton("dtVar_execute", "Plot",
                 class = "run-button",
                 icon = icon("play-circle"),
                 style="width:30%; position: relative;")
    })

    observeEvent(input$dtVar_execute, {
      req(input$dependent, input$cp)
      reactive_cp <- reactive({
        input$cp
      })
      output$decisionTree <- renderPlot({
          isolate({
            if(is.null(input$confirmed) &
               is.null(input$tentative) &
               is.null(input$significant)){
              choices = boruta_tab$variable[boruta_tab$decision=="Confirmed"]
              Treedata_OBT5 <- focus_data %>%
                dplyr::select(dep_var, choices)
              formula_rpart <- as.formula(paste(dep_var, "~."))
              understandable.model <- rpart(formula_rpart,
                                            Treedata_OBT5, cp = reactive_cp())
              rpart.plot(understandable.model,
                         main = paste("Decision Tree with", reactive_cp(), "complexity"),
                         type = 5,
                         clip.right.labs = FALSE,
                         under = TRUE, branch.lty = 3)
            }else{
              select_dep_var = input$dependent
              select_ind_vars = unique(c(input$confirmed, input$tentative, input$significant))
              Treedata_OBT5 <- focus_data %>%
                dplyr::select(select_dep_var, select_ind_vars)
              formula_rpart <- as.formula(paste(select_dep_var, "~."))
              understandable.model <- rpart(formula_rpart,
                                            Treedata_OBT5, cp = reactive_cp())
              rpart.plot(understandable.model,
                         main = paste("Decision Tree with", reactive_cp(), "complexity"),
                         type = 5,
                         clip.right.labs = FALSE,
                         under = TRUE, branch.lty = 3)
            }
          })
      })
    })
  })



  ## IndSem tab

  indsem_rv <- reactiveValues(dat=NULL, type=NULL)

  output$indsem_header <- renderUI({
    ind <- input$individual
    dep_var <- input$dependent_variable
    ind_vars <- input$independent_variables
    indsem_rv$individual <- ind
    indsem_rv$dependent_variable <- dep_var
    indsem_rv$independent_variables <- ind_vars
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
        tagList(
            br(),
            h3(paste0('IndSEM for Client ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
            br(),
            shiny::validate(
              need(length(focus_data[,3] %>% na.omit()) > 2,
                   "Sorry, you don't have enough data to do this analysis.")),
            p("This analysis requires results from the Linear and Machine Learning Analysis."),
            p("1. Click on the 'Prune Variables' button to get the choices from the Linear and Machine Learning Analysis."),
            p("2. Then select your choices. Note: you must select at least 2 choices."),
            p("3. Click on 'Plot' button to see the plot."),
            br()
        )
    })

  observeEvent(input$indsemVar_prune, {
    req(input$individual, input$dependent_variable, input$independent_variables)
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
    ind <- input$individual
    dep_var <- input$dependent_variable
    ind_vars <- input$independent_variables
    full_data <- file_data()

    set.seed(8675309)
    boruta_tab_indsem <- run_boruta(focus_data)$stat_results %>%
        mutate(decision = factor(decision,
                                # ordering the decision for better visualization
                                levels = c("Confirmed",
                                            "Tentative",
                                            "Rejected")))
    arima_summary_tab_indsem <- format_arima(run_arima_full_dataset(focus_data, model = "automate", lagged=0), focus_data, full_data)

    output$SEMconfirmed_UI <- renderUI({
        choices = boruta_tab_indsem$variable[boruta_tab_indsem$decision=="Confirmed"]
        pickerInput("confirmedSEM","Confirmed",
                    choices = choices,
                    #selected = choices,
                    options = list(
                    `style` = HTML("color: grey") ,
                    `width` = '85%',
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Select None",
                    `select-all-text` = "Select All",
                    `none-selected-text` = ifelse(length(choices)==0,
                                                    "No variable",
                                                    paste(length(choices), "Options")),
                    `selected-text-format`= "count",
                    `count-selected-text` = "{0} Selected"
                    ),
                    multiple = TRUE)
    })

    output$SEMtentative_UI <- renderUI({
        choices = boruta_tab_indsem$variable[boruta_tab_indsem$decision=="Tentative"]
        pickerInput("tentativeSEM","Tentative",
                    choices = choices,
                    options = list(
                    `style` = HTML("color: grey") ,
                    `width` = '85%',
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Select None",
                    `select-all-text` = "Select All",
                    `none-selected-text` = ifelse(length(choices)==0,
                                                    "No variable",
                                                    paste(length(choices), "Options")),
                    `selected-text-format`= "count",
                    `count-selected-text` = "{0} Selected"
                    ),
                    multiple = TRUE)
    })

    output$SEMsignificant_UI <- renderUI({
        arima_sig <- arima_summary_tab_indsem %>%
          mutate(Decision = ifelse((L < 0 & U < 0) |
                                    (L > 0 & U > 0), "Significant", "Insignificant")) %>%
          dplyr::filter(Decision == "Significant") %>%
          dplyr::select(variable) %>%
          as.vector()
        choices = arima_sig$variable
        pickerInput("significantSEM","Significant in ARIMA",
                    choices = choices,
                    #selected = choices,
                    options = list(
                    `style` = HTML("color: grey") ,
                    `width` = '85%',
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Select None",
                    `select-all-text` = "Select All",
                    `none-selected-text` = ifelse(length(choices)==0,
                                                    "No variable",
                                                    paste(length(choices), "Options")),
                    `selected-text-format`= "count",
                    `count-selected-text` = "{0} Selected"
                    ),
                    multiple = TRUE)
    })

    output$SEMdependent_UI <- renderUI({
        pickerInput("dependentSEM","Dependent",
                    choices = colnames(focus_data)[!colnames(focus_data) %in% boruta_tab_indsem$variable],
                    selected = dep_var,
                    options = list(
                    `style` = HTML("color: grey") ,
                    `width` = '85%',
                    `actions-box` = TRUE,
                    `none-selected-text` = "Select One"
                    ),
                    multiple = F)
    })

    output$indsem_execute <- renderUI({
      actionButton("indsemVar_execute", "Plot",
                   class = "run-button",
                   icon = icon("play-circle"),
                   style="width:30%; position: relative;")
    })

    observeEvent(input$indsemVar_execute, {
      req(input$dependentSEM)
      output$SEMplot <- renderPlot({
        withProgress(message = 'Loading Plot...', {
          incProgress(1/4)
          isolate({
            if(is.null(input$confirmedSEM) &
               is.null(input$tentativeSEM) &
               is.null(input$significantSEM)){
              SinglePerson <- list()
              SinglePerson[[1]] <- focus_data
              SinglePerson[[1]] <- subset(focus_data, select = -c(ID, Time))
              data_list_fewer <- SinglePerson
              for(i in 1:length(SinglePerson)){
                data_list_fewer[[i]] <- data_list_fewer[[i]] %>%
                  dplyr::select(dep_var, ind_vars)
              }
              indOut <- indSEM(data = data_list_fewer)
              plot(indOut$plots[[1]])

            }else{
              dep_var = input$dependentSEM
              ind_vars = unique(c(input$confirmedSEM, input$tentativeSEM, input$significantSEM))
              SinglePerson <- list()
              SinglePerson[[1]] <- focus_data
              SinglePerson[[1]] <- subset(focus_data, select = -c(ID, Time))
              data_list_fewer <- SinglePerson
              for(i in 1:length(SinglePerson)){
                data_list_fewer[[i]] <- data_list_fewer[[i]] %>%
                  dplyr::select(dep_var, ind_vars)
              }
              incProgress(2/4)
              indOut <- indSEM(data = data_list_fewer)
              incProgress(3/4)
              plot(indOut$plots[[1]])
              incProgress(4/4)
            }
          })
        })

        #}
      })
  })
  })



  ## Lagged Linear Analysis tab

  output$linear_lag_input <- renderUI({
    selectInput("linear_lag", label = "Select time series lag",
                choices = c(1, 2, 3))
  })

  output$linear_lag_header <- renderUI({
    ind <- input$individual
        tagList(
            br(),
            h3(paste0('Lagged Linear Analysis for Client ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
            br()
        )
    })

  observeEvent(input$lagged_execute, {
    req(input$linear_lag, input$individual, input$dependent_variable, input$independent_variables)
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
    ind <- input$individual
    dep_var <- input$dependent_variable
    ind_vars <- input$independent_variables
    full_data <- file_data()
    lag <- as.numeric(input$linear_lag)



    output$lagged_linear_plot <- renderPlot({
      results = run_arima_full_dataset(focus_data, model = "automate", lagged=lag)
      arima_summary_lagged <- format_arima(results, focus_data, full_data)
        if (is.null(arima_summary_lagged)) {
            invisible()
        }else{
            lag_forest_plot <- plot_lagged_forest(arima_summary_lagged, lag)
            lag_forest_plot
        }
    })

    output$lagged_linear <- renderUI({
        tagList(
            br(),
            h3(paste0('Importance of Process for Client ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
            shiny::validate(
              need(length(focus_data[,3] %>% na.omit()) > 2,
                   "Sorry, you don't have enough data to do this analysis.")),
            plotOutput("lagged_linear_plot")
        )
    })

  })

  ## ITM

  output$itm_header <- renderUI({
    ind <- input$individual
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
        tagList(
            br(),
            h3(paste0('Information Theoretic Modelling for Client ', as.character(ind)), style = 'text-align: center; background: #fcc38b'),
            br(),
            shiny::validate(
              need(length(focus_data[,3] %>% na.omit()) > 2,
                   "Sorry, you don't have enough data to do this analysis."))
        )
    })

  output[["ITM_execute"]] <- renderUI({
    isolate({
      actionButton("plotITM", "Execute Information Theoretic Modelling",
                   class = "run-button",
                   icon = icon("play-circle"),
                   style="width:100%!important;
                   position: relative;
                   margin-bottom: -10px",
                   align = "center")
    })
  })

  observeEvent(input$plotITM, {
    req(input$individual, input$dependent_variable, input$independent_variables)
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
    dep_var <- input$dependent_variable

    DR1 <- information_theoretic_modelling(focus_data)
    DR1_df <- get_itm_df(DR1)

    output$boruta_itm <- renderPlot({
      get_itm_boruta_graph(DR1_df, dep_var)
    })

    output$itm_forest_plot <- renderPlot({
      get_itm_ci_graph(DR1_df, dep_var)
    })

    output$itm_imp <- renderUI({
      get_ordered_imp_vars(DR1_df)
    })

    output$itm_notimp <- renderUI({
      get_ordered_notimp_vars(DR1_df)
    })
  })


  # REPORT

  observeEvent(input$generate_report, {
    req(input$individual, input$dependent_variable, input$independent_variables)
    focus_data <- get_focus_data(file_data(), input$individual, input$dependent_variable, input$independent_variables) # setting the common structure of the dataframe so that all functions can assume this structure to extract dep_var and ind_vars
    ind <- input$individual
    dep_var <- input$dependent_variable
    ind_vars <- input$independent_variables
    full_data <- file_data()
    arima_summary_report <- format_arima(run_arima_full_dataset(focus_data, model = "automate", lagged=0), focus_data, full_data)
    TimeSeriesData <- na_interpolation(focus_data[,3])

    notrend_test <- notrend_test(TimeSeriesData)
    anytrend_test <- notrend_test(TimeSeriesData, test = "MK",
                                 factor.length = "adaptive.selection")
    variance_box_test <- Box.test(TimeSeriesData, type = "Box-Pierce", lag = 1)
    if ( notrend_test$p.value < 0.05 |
         anytrend_test$p.value < 0.05 |
         variance_box_test$p.value < 0.05) {
      change_text = "Well-being changed over this time period."
    } else {
      change_text = "Well-being did not change over this time period."
    }

    stats <- get_variable_stat(focus_data, full_data)
    min_percentile = min(stats$X..mean)
    max_percentile = max(stats$X..mean)
    min_perc_var = stats %>%
      filter(X..mean == min(X..mean)) %>%
      dplyr::select(Variable)
    max_perc_var = stats %>%
      filter(X..mean == max(X..mean)) %>%
      dplyr::select(Variable)
    max_exp <- explanation_table %>%
      filter(Variablename == as.character(max_perc_var)) %>%
      dplyr::select(Explanation)
    min_exp <- explanation_table %>%
      filter(Variablename == as.character(min_perc_var)) %>%
      dplyr::select(Explanation)
    sp_1 <- paste0("This person is in the ",
                 round(max_percentile, 0),
                 "th percentile for ",
                 max_perc_var,
                 ". ",
                 max_perc_var,
                 " means ",
                 max_exp,
                 ". This means this person does this more than ",
                 round(max_percentile, 0),
                 " percent of people.\n\n")

    sp_2 = paste0("This person is in the ",
                 round(min_percentile, 0),
                 "th percentile for ",
                 min_perc_var,
                 ". ",
                 min_perc_var,
                 " means ",
                 min_exp,
                 ". This means this person does this less than ",
                 round(100-min_percentile, 0),
                 " percent of people.")

  output$dv_scatterPlot_report <- renderPlotly({
      scatter_change <- ggplot(focus_data, aes_string(x = "Time", y = as.character(dep_var))) +
                      geom_point(size = 1.5) +
                      geom_point(data = focus_data[(focus_data[,3] == min(focus_data[,3], na.rm = T)), ], color="#87bc45",
                                size=4) +
                      geom_point(data = focus_data[(focus_data[,3] == max(focus_data[,3], na.rm = T)), ], color="orange",
                                size=4) +
                      geom_smooth() +
                      scale_color_brewer(palette = "Set1") +
                      theme_minimal() +
                      theme(plot.background = element_rect(colour = "#909090", fill=NA, size=2))
      ggplotly(scatter_change, tooltip = c("text"))
  })

  output$radar_plot_norm_between_report <- renderPlotly({
      spider_plot <- plot_radar(get_variable_stat(focus_data, full_data))
  })

  output$linear_imp_report_plot <- renderPlotly({
    linear_imp <- plot_linear_imp_report(arima_summary_report, as.character(dep_var))
  })

  output$full_report <- renderUI({
      tagList(
      h1(paste0("Client ", as.character(ind), " Report")),

      h3(paste0("Change in ", as.character(dep_var))),
      p(paste0("This graph shows how much the person has changed in ",
              as.character(dep_var),
              " over time")),
      plotlyOutput("dv_scatterPlot_report"),
      p(change_text),
      get_change_analysis_summary(focus_data),

      h3("Normative"),
      p("This plot presents how much the person engages in different behaviors, compared to the average."),
      plotlyOutput("radar_plot_norm_between_report", height = "100%"),
      p(sp_1),
      p(sp_2),

      h3("Key correlates of well-being"),
      get_linear_analysis_summary(arima_summary_report),
      plotlyOutput("linear_imp_report_plot"),
      br(),
      p(paste0("Processes in grey are generally less important to your well-being than processes in green. ",
            "Processes on the right increase the ", colnames(focus_data)[3],
            ". Processes on the left decrease the ", colnames(focus_data)[3], ". ",
            linear_imp_report(arima_summary_report, focus_data))),

      h3("Potential targets for change"),
      p("Taking into account norm percentiles (in what percentile the person sits away from the group mean):"),
      target_of_change(arima_summary_report),
      br(),
      br(),
    )
  })

  output$report <- downloadHandler(
    filename = function() return("report.pdf"),

    content = function(file) {
      src <- normalizePath("report.Rmd")
      src3 <- normalizePath("www/target.png")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)
      file.copy(src3, "target.png", overwrite = TRUE)
      out <- rmarkdown::render("report.Rmd",
                                output_format = "pdf_document",
                                run_pandoc = TRUE,
                                params = list(dep_var = as.character(dep_var),
                                             focus_data = focus_data,
                                             arima_summary = arima_summary_report,
                                             file_data = full_data,
                                             explanation_tb = explanation_table))
      file.rename(out, file)
    }
  )

  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit(normalizePath('report.rmd'), quiet = TRUE)))
  })

  })
  ## PROJECT INFORMATION

}

shinyApp(ui, server)
