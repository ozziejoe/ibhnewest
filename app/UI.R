UI <- fluidPage(
  
  # style and title of page 
  navbarPage(title=div(img(src="mindgrapher_logo.png",
                           width = "50",
                           height="50")),
             collapsible = TRUE,
             theme = shinytheme("simplex"),
             
             id = "page-nav",
             
             # css style of the navigation bar
             tags$head(HTML('<link rel="icon", 
                              href="logo.png",
                              type = "image/png" />')),

             tags$head(tags$style(HTML(
               "#page-nav > li:first-child { display: none }"))),
             
             tags$head(
               tags$style(HTML(' .navbar {
                          height: 70px;
                          min-height:70px !important;
                          font-size: 15px;
                          text-align:center;
                          margin-left: 8px;
                        }
                      
                      .navbar .navbar-nav > li > a{
                          color: grey !important;
                      }
                      
                      .navbar .navbar-nav > .active > a{
                          color: #F28C28 !important;
                      }
                      
                      .navbar .navbar-nav > li > a:hover,
                      .navbar .navbar-nav > li > a:focus,
                      .navbar .navbar-nav > .active > a:hover,
                      .navbar .navbar-nav > .active > a:focus {
                          color: #F28C28 !important;
                      }
                      
                      .nav-tabs > li > a {
                        color: grey !important;
                      }
                      
                      .nav-tabs > .active > a {
                        color: #F28C28 !important;
                      }
                      
                      .nav-tabs > li > a:hover, 
                      .nav-tabs > .active > a:hover {
                        color: #F28C28 !important;
                        background-color: #ededed !important;
                      }
                      
                      .navbar-nav > li > a, .navbar-brand {
                            padding-top: 10px !important; 
                            padding-bottom: 10px !important;
                            height: 70px;
                            text-align:center;
                            margin-left: -8px;
                      }
                      .navbar .navbar-nav {
                        margin-top: 10px !important;
                      }
                      .navbar-nav > li {
                        margin-right: 50px !important;
                        margin-left: -60px !important;
                      }
                      .container-fluid {
                        padding-left: 0px;
                        padding-right: 0px;
                      }
                      .navbar-nav > li >
                        a[data-value=" "] {
                        }
                      .navbar-nav > li >
                        a[data-value="Summary"] {
                        padding-top: 20px !important;
                        }
                      .navbar-nav > li >
                        a[data-value="Analysis"] {
                        padding-top: 20px !important;
                        }
                      .navbar-nav > li >
                        a[data-value="Advanced Analysis"] {
                        padding-top: 20px !important;
                        }
                      .navbar-nav > li >
                        a[data-value="Report"] {
                        padding-top: 20px !important;
                        }
                      .navbar-nav > li >
                        a[data-value="Project Information"] {
                        padding-top: 20px !important;
                        }
                      .nav-tabs-custom .nav-tabs li.active {
                        border-top-color: #F28C28;
                      }
                               '
                               ))),
             
             header = tagList(
               useShinydashboard()
             ),
             
             # Upload file 
             #column(3,
                    
                    sidebarPanel(
                      style = 'height: 100vh;',
                      width = 3,
                      "",
                      tags$head(tags$style(".progress-bar{background-color:#F28C28;}")),
                      div(style = "height: 70px;",
                        fileInput("data_file", "Choose CSV File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                      )),
                      
                      div(strong("OR"), br(), "Get your MindGrapher data", br()),
                      actionButton("mgLogin", "Login",
                                   style="width:70%;"),
                      div(br()),
                      uiOutput("dependent_variable"), # dependent variable selection
                      uiOutput("independent_variables"), # independent variables selection 
                      uiOutput("individual"), # individual selection

                    ),
             
             # home panel

             tabPanel(div(img(src="mindgrapher_text.png",
                              width = "190",
                              height="40"), " "), 
                      mainPanel(
                        h1("Welcome to the IBH MindGrapher"),
                        h3("Personalising treatment based on sophisticated and evidence based methods"),
                        br(),
                        fluidRow(
                          tabBox(
                            side = "right",
                            title = "HOW TO USE:",
                            width = 12,
                            tabPanel(
                              "Step 3",
                              "Navigate through the dashboard using the navigation bar up the top. Click on 'Plot' buttons within each page to generate the analysis and visualisations."
                            ),
                            tabPanel(
                              "Step 2",
                              "Select your dependent, independent, and individual variables.",
                              br(),
                              br(),
                              "Your dependent variable will be the outcome you would like to focus on.",
                              br(),
                              br(),
                              "Your independent variables are the processes you would like to focus on.",
                              br(),
                              br(),
                              "If you have uploaded a dataset with data from multiple individuals, you can select the individual you would like to focus on."
                            ),
                            tabPanel(
                              "Step 1",
                              "Use the 'Browse' button on the left hand side sidebar to upload a CSV file containing your data.",
                              br(),
                              br(),
                              strong("OR"),
                              br(),
                              br(),
                              "Use the 'Login' button to upload your data from your MindGrapher account."
                            ),
                          ),
                        ),
                      fluidRow(
                        box(
                          title = "How do these methods help me personalise treatment?", status = "success",
                          "Idionomic methods...",
                          br(),
                          br(),
                          "Exploring different models to reduce limitations..."
                        ),
                        box(
                          title = "How to use: video tutorial", status = "warning",
                          "embed a visual tutorial video..."
                        )
                      )
                      )
                      ),
             
             # tab panels 
             
             tabPanel("Summary", 
                      mainPanel(
                             style = "margin: 0 10px; font-size: 17px",
                             add_busy_spinner(spin = "fading-circle", position = "top-left"),
                        tabsetPanel( id = "view_tabset",
                          tabPanel("Raw data",
                                    br(),
                                    actionButton("raw_execute", "Plot Raw Data",
                                          class = "run-button",
                                          icon = icon("play-circle"),
                                          style="width:30%; position: center;"),
                                   uiOutput('raw_data')
                          ),
                          tabPanel("Norms between people",
                                    br(),
                                    actionButton("norms_execute", "Plot Trend",
                                          class = "run-button",
                                          icon = icon("play-circle"),
                                          style="width:30%; position: center;"),
                                    uiOutput("norms")),
                          tabPanel("Trends",
                                   br(),
                                   inputPanel(
                                     uiOutput("trends_outcome"), 
                                     uiOutput("trends_process"),
                                   ),
                                   actionButton("trends_execute", "Plot Trend",
                                            class = "run-button",
                                            icon = icon("play-circle"),
                                            style="width:30%; position: center;"),
                                   uiOutput("trends")
                            ))
             )),
             
             tabPanel("Analysis",
                      mainPanel(
                             style = "margin: 0 10px; font-size: 17px",
                             add_busy_spinner(spin = "fading-circle", position = "top-left"),
                             tabsetPanel(
                               tabPanel("Linear-Importance Analysis", # forest plot
                                          br(),
                                          actionButton("linear_execute", "Plot Linear",
                                                    class = "run-button",
                                                    icon = icon("play-circle"),
                                                    style="width:30%; position: relative;"),
                                          uiOutput("linear")),
                               tabPanel("Well-being Mountain",
                                        br(),
                                        actionButton("wellbeingchart_execute", "Plot Chart",
                                                     class = "run-button",
                                                     icon = icon("play-circle"),
                                                     style="width:30%; position: relative;"),
                                        uiOutput("wellbeingchart_tab")),
                               tabPanel("Machine Learning Importance Analysis", #i-boruta
                                        br(),
                                        actionButton("boruta_execute", "Plot Boruta",
                                                    class = "run-button",
                                                    icon = icon("play-circle"),
                                                    style="width:30%; position: relative;"),
                                        uiOutput("boruta")), 
                               tabPanel("Change Analysis", # DV change texts and plots
                                        br(),
                                        actionButton("change_execute", "Plot Change",
                                                    class = "run-button",
                                                    icon = icon("play-circle"),
                                                    style="width:30%; position: relative;"),
                                        uiOutput("dv_change"))) 
                             )),
             
             tabPanel("Advanced Analysis",  
                      mainPanel(
                             style = "margin: 0 10px; font-size: 17px",
                             add_busy_spinner(spin = "fading-circle", position = "top-left"),
                             tabsetPanel(
                               tabPanel("Decision Tree", # decision tree 
                                        uiOutput("dt_header"),
                                        actionButton("dtVar_prune", "Prune Variables",
                                                        class = "run-button",
                                                        icon = icon("play-circle"),
                                                        style="width:30%; position: relative;"),
                                        br(),
                                        br(),
                                        fillCol(height = 400, flex = c(2,2,2), 
                                                fillRow(
                                                  uiOutput("confirmed_UI"),
                                                  uiOutput("tentative_UI"),
                                                  uiOutput("selectCP"),
                                                ),
                                                fillRow(
                                                  uiOutput("significant_UI"),
                                                  uiOutput("dependent_UI"),
                                                  uiOutput("empty_space1")
                                                ),
                                                fillRow(
                                                  uiOutput("dt_execute")
                                                ),
                                                fillRow(
                                                  plotOutput("decisionTree", height=600)
                                                )
                                        )),
                               tabPanel("IndSEM", # IndSEM
                                        uiOutput("indsem_header"),
                                        actionButton("indsemVar_prune", "Prune Variables",
                                                        class = "run-button",
                                                        icon = icon("play-circle"),
                                                        style="width:30%; position: relative;"),
                                        br(),
                                        br(),
                                        fillCol(height = 400, flex = c(2,2,2),
                                                fillRow(
                                                  uiOutput("SEMconfirmed_UI"), 
                                                  uiOutput("SEMtentative_UI"),
                                                  uiOutput("empty_space2")
                                                ),
                                                fillRow(
                                                  uiOutput("SEMsignificant_UI"), 
                                                  uiOutput("SEMdependent_UI"),
                                                  uiOutput("empty_space3")
                                                ),
                                                fillRow(
                                                  uiOutput("indsem_execute")
                                                ),
                                                fillRow(
                                                  plotOutput("SEMplot", height=700)
                                                )
                                        )),
                               tabPanel("Lagged Linear Analysis",
                                        uiOutput("linear_lag_header"),
                                        uiOutput("linear_lag_input"),
                                        actionButton("lagged_execute", "Plot Lagged Analysis",
                                                        class = "run-button",
                                                        icon = icon("play-circle"),
                                                        style="width:30%; position: relative;"),
                                        uiOutput("lagged_linear")
                                        ),
                               tabPanel("Information Theoretic Modelling",
                                        uiOutput("itm_header"),
                                        p("This analysis might take a while after you click execute."),
                                        fillCol(height = 100, flex = c(2,2,2),
                                                fillRow(
                                                  uiOutput("ITM_execute"),
                                                  uiOutput("empty_space4")
                                                )
                                        ),
                                        plotOutput('boruta_itm'),
                                        br(),
                                        plotOutput('itm_forest_plot')
                                        )
                             ))), 
             
             tabPanel("Report",
                      mainPanel(
                        style = "margin: 0 10px; font-size: 17px",
                        add_busy_spinner(spin = "fading-circle", position = "top-left"),
                        fillCol(height = 100, flex = c(2,2,2),
                                                fillRow(
                                                  actionButton("generate_report", "Generate Report",
                                                        class = "run-button",
                                                        icon = icon("play-circle"),
                                                        style="width:100%; position: relative;"),
                                                  uiOutput("empty_space5"),
                                                  downloadButton("report", "Download report")
                                                )
                                        ),
                        uiOutput("full_report"),
                      ),
             ),
             
             tabPanel("Project Information",
                      mainPanel(
                        h3("Authors"),
                        p("Joseph Ciarrochi, Saaz Sahdra, and Steven C. Hayes"),
                        br(),
                        h3("Project Information"),
                        p("Reconciling idiographic and nomothetic methods"),
                        p("Supported by IBH, a non-profit organisation."),
                        tags$a(href="https://www.ibh.com/", "Click here for IBH website.")
                      ))
  )
)

