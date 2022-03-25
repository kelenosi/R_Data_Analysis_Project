
dashboardPage(
  dashboardHeader(title ='Vehicle Insurance Claims',titleWidth = 275),
  dashboardSidebar(
    sidebarUserPanel("R Data Analysis Project" ),
    sidebarMenu(
      menuItem("EDA", tabName = "eda", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("ROC", tabName = "roc" , icon = icon("list-alt")),
      menuItem("Prediction", tabName = "predict" , icon = icon("search"))
    )),
   
  dashboardBody(tabItems(
    tabItem(tabName = 'eda',
            sidebarLayout(
              sidebarPanel(width = 2,
                selectInput(inputId ="cat",label='Categorical Variables',
                choices= names(df_auto[sapply(df_auto,class) == "factor"])[-1],
                selected= names(df_auto[sapply(df_auto,class) == "factor"])[1]),
               
                br(),
          
                selectInput("num", "Numeric Variables",
                choices= names(df_auto[sapply(df_auto,class) != "factor"]),
                selected=names(df_auto[sapply(df_auto,class) != "factor"])[1])
              ),
              mainPanel(width = 10,
               tabsetPanel(type = "tabs",
                           
                 tabPanel("Plots",
                          fluidRow(
                            column(6, plotOutput("plot_bar")),
                            column(6, plotOutput("plot_box"))),
                          
                          br(),
                          
                          fluidRow(
                            column(6, plotOutput("plot_point")),
                            column(6, plotOutput("plot_hist")))
                          ),
                 tabPanel("Summary",verbatimTextOutput("summary")),
                 tabPanel("Table",
                          fluidRow(
                            column(6, tableOutput("table"))),
                          br(),
                          fluidRow(
                            column(6,
                             box(title = "Glossary of Terms",
                                 status = 'primary',
                                 width = 12, height = 260,
                                 helpText("exposure = proportion of time exposed
                                          to risk"),
                                 helpText("num_clm = number of claims"),
                                 helpText("clm_amt = claim amount"),
                                 helpText("freq = num_clm/exposure -> number
                                          of claims per unit of exposure"),
                                 helpText("severity = clm_amt/num_clm -> amount 
                                          per claim"),
                                 helpText("pure_prem = clm_amt/exposure -> amount
                                          per unit of exposure"),
                                 helpText("base_level = level with the most
                                          exposure"))) 
                                )))
            ))),
          
    tabItem(tabName = 'data', dataTableOutput("data")),
    
    tabItem(tabName = 'roc',
            sidebarLayout(
              sidebarPanel(width = 4,
                    sliderInput("cutoff",
                     "Discrimination Threshold:",
                        value = .08,
                         min = .01,
                         max = .30)),
              mainPanel(width = 8,
                  wellPanel(style= "min-width: 800px;max-width: 800px;
                            overflow:auto",
                    plotOutput("plot_roc"),
                    br(),
                    tableOutput("c_matrix")))
            )),
    tabItem(tabName = 'predict',
            sidebarLayout(
            sidebarPanel(width = 4,
             selectInput(inputId ="agecat",label='Policyholder Age (1 youngest):',
             choices= levels(model_final$model$agecat),
             selected= levels(model_final$model$agecat)[1]),
             br(),
             selectInput(inputId ="area",label='Area of Residence:',
             choices= levels(model_final$model$area),
             selected= levels(model_final$model$area)[1]),
             br(),
             selectInput(inputId ="veh_age",label='Vehicle Age: (1 new):',
             choices= levels(model_final$model$veh_age),
             selected= levels(model_final$model$veh_age)[1]),
             br(),
             selectInput(inputId ="veh_body",label='Vehicle Body Type:',
             choices= levels(model_final$model$veh_body),
             selected= levels(model_final$model$veh_body)[1]),
             br(),
             sliderInput("exposure",
                         "Exposure (time at risk):",
                         value = .50,
                         min = .01,
                         max = 1.0)),
             
             mainPanel(width = 8,
                wellPanel(style= "min-width: 800px;max-width: 800px;
                            overflow:auto",
                box(title = "Prediction Result",
                    status = 'primary',
                    solidHeader = TRUE,
                    width = 4, height = 260,
                    div(h5('Estimated Probability of a Claim:')),
                    verbatimTextOutput("value", placeholder = TRUE),
                    br(),
                    actionButton('calc','Calculate', icon = icon('calculator'))),
                br(),
                box(title = "Model Explanation",
                    status = 'primary',
                    solidHeader = FALSE,
                    width = 6, height = 275,
                    helpText("The following model will predict the probability
                             of an insured having a claim based on the Age 
                             group of the policyholder,area of residence, 
                             vehicle age and vehicle body type."),
                    helpText("The prediction is based on a logistic regression
                             model with an exposure offset term
                             that produced the lowest AIC. Modeling counts 
                             requires correction for the number exposed to
                             risk. When policies have exposure to risk, the
                             probability of a claim is proportionally reduced
                             by the time at risk.")))
    )))
)
))


