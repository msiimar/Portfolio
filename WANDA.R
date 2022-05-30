# Welcome to the Water Anomaly Detection Application, aka WANDA!
# WANDA uses time series decomposition from the Anomalize package. 
# Data is decomposed with the seasonal trend decomposition Loess (STL) algorithm.
# Anomalies are detected within the decomposed data using the interquartile range method (IQR).
# The parameters for tuning (sensitivity and maximum number of anomalies) are controlled by the user.

####################################
# Libraries              # Version #
library(anomalize)       # 0.2.2   #
library(ggthemes)        # 4.2.4   #
library(lubridate)       # 1.8.0   #
library(openxlsx)        # 4.2.5   #
library(rio)             # 0.5.29  #
library(shiny)           # 1.7.1   #
library(shinyscreenshot) # 0.2.0   #
library(shinythemes)     # 1.2.0   #
library(tibbletime)      # 0.1.6   #
library(tidyverse)       # 1.3.1   #
####################################

# SHINY APPLICATION BEGINS:
shinyApp(
  ##### USER INTERFACE SIDE #####
  ui <-  fluidPage(
    titlePanel(title = "Water Anomaly Detection Application",
               windowTitle = "WANDA"),
    #themeSelector(), # uncomment this line and run the app to see drop down of different themes to select
    theme = shinytheme("spacelab"), # this is the theme that is currently used
    # Side Bar Panel:
    sidebarPanel(
      style = "position:fixed;width:300px;height:80vh;overflow-y: auto;", # cascading style sheets
      width = 3,
      # Select File
      fileInput(inputId = "uploaded_file",
                accept = c(".xlsx", ".csv"),
                NULL,
                placeholder = "No file selected...",
                buttonLabel = "Select File",
                label = "Upload File:",
                multiple = FALSE), # Change this line to be TRUE to upload multiple files
      # Insert account number(s):
      selectizeInput("account", label = "Insert Account Number(s):", choices = NULL, options = NULL, multiple = TRUE),
      # Code for bill code checkboxes:
      checkboxGroupInput("billcode", "Select Bill Code(s):",
                         choices = c("IJ0" = "IJ0", "IJ1" = "IJ1", "J00" = "J00", "WG1" = "WG1")
                         ), # ends checkboxGroupInput
      # Date Input:
      dateRangeInput("dates", "Date Range:", start = "2014-01-01"),
      # Sensitivity Slider:
      sliderInput("sensitivity", "Detection Sensitivity:", .01, .15, .01, value = .05),
      # Maximum Number of Anomalies Slider:
      sliderInput("maximum_number", "Maximum Number of Anomalies:", .01, 1, .01, value = .15),
      # Filter action button on the sidebar menu:
      actionButton("filter", "Apply Selections", class = "btn-info", icon = icon("filter")),
      br(), # insert space between buttons
      br(), # insert space between buttons
      # Run Anomaly Detection button on sidebar menu:
      actionButton("run_models", "Run Anomaly Detection", class = "btn-danger", icon = icon("dragon")),
      br(), # insert space between buttons
      br(), # insert space between buttons
      # Download Table button on sidebar menu:
      downloadButton("download_table", "Download Table Results"),
      br(), # insert space between buttons
      br(), # insert space between buttons
      # Download Graphs button on sidebar menu:
      actionButton("download_components_graph", "Download Components Graph", icon = icon("chart-line")),
      br(), # insert space between buttons
      br(), # insert space between buttons
      actionButton("download_overall_graph", "Download Overall Graph", icon = icon("chart-line"))
    ),
    # Main Panel:
    mainPanel(
      # Tab Panel
      tabsetPanel(
        # Uploaded Data tab:
        tabPanel("Uploaded Data",
                 dataTableOutput("contents")),
        # Data to Analyze tab:
        tabPanel("Data to Analyze", dataTableOutput("model_data")),
        # Table Results tab:
        tabPanel("Table Results",
                 # Drop down menu that appears at the top of the Table Results tab:
                 selectizeInput("account_billcode", label = "Select Account.Billcode Combination:", choices = NULL, multiple = FALSE),
                 # Output table from running the anomaly detection model:
                 dataTableOutput("model_table")
        ),
        # Graph Results tab:
        tabPanel("Graph Results", 
                 plotOutput("season_trend_rem_graph", width = "70%", height = "70%"),
                 plotOutput("recomposed_graph", width = "70%", height = "70%")),
        # Help tab with text instructions:
        tabPanel("Help", 
                 h3("Select File"),
                 h5("Upload the data file. The program will read .xlsx or .csv files.
                    Within the data file, the program relies on looking for certain column titles, 
                    such as year, month, account, billcode, and cyaconsum to run correctly. For example, the program uses the year and month columns
                    to create a new date column, where the default day is the first of each month; this is the default behavior, given that no day value
                    is provided. The program requires a date column to run the time series decomposition and recomposition."),
                 h3("Account Number"),
                 h5("The account number field can be used to select one or many accounts to analyze. 
                    If no account number is inserted, the program will analyze all of the accounts data set.
                    'If you select nothing, you select everything.'"),
                 h3("Bill Code"),
                 h5("Notice how by default none of the bill code options are checked, think of this as, 'If you select nothing, you select everything.'
                    The program will only run for accounts containing at least one of the four bill code options"),
                 h3("Date Range"),
                 h5("A start and end date can be specified. When selecting a date range, the program will analyze all of the data within that range.
                    The program can run using only a date range, without providing any account or bill code information.
                    The default start date is January 1, 2014.
                    The date column for the program is created when the data file is uploaded; it is created from the year and month columns.
                    When the R programming language creates a date column using only year and month information, the default for the day value is 1,
                    in this case meaning the first of the month. When selecting certain date ranges, keep in mind that the program requires a minimum number of values
                    to correctly detect anomalies and run. Narrowing the date range may reduce the number of values the program requires."),
                 h3("Detection Sensitivity"),
                 h5("The user is able to specifiy the sensitivity with which the program will detect anomalies.
                    The scale for the sensitivity is 0.01 to 0.015; these values are percentages.
                    The default sensitivity is 0.05.
                    The effect of changing the sensitivity value can be seen on the second graph (on the Graph Results tab) in the form of the thickness of the gray band.
                    The higher the sensitivity, the narrower the gray band; the lower the sensitivity, the wider the gray brand.
                    In other words, selecting lower sensitivity values will make it harder for values to be anomalies because the gray band is wider, 
                    while selecting higher sensitivity values will make it easier for values to be anomalies because the gray band becomes narrower. Note that the gray band
                    is the seasonality."),
                 h3("Maximum Number of Anomalies"),
                 h5("The maximum number of anomalies takes a value between 0.01 and 1.
                    The default value is 0.05.
                    The value determines the number of data points that are allowed to be labeled as anomalies.
                    For example, sometimes there may be numerous values outside of the gray band, but the user may only want to see the most
                    egregious cases labeled as anomalies. Selecting a lower number for maximum number of anomalies will result in fewer
                    data points being labeled as anomalies outside of the gray band.
                    The program will label the most 'anomalous' points first.
                    If the user wants to see every data point outside of the gray band labeled as an anomaly, they should select
                    1, meaning 100% of the data points outside the band will be labeled as anomalies."),
                 h3("Apply Selections"),
                 h5("Clicking on the Apply Selections button will use the information specified from inserting the account number, bill code, and date range. The changes to the data can be seen on the Data to Analyze tab. You will need to click the
                    Apply Selections button to show the updates. The Uploaded Data tab is not affected by the selections. If the user plans to run the anomaly detection
                    program without making any selections, they must still press the Apply Selections button. To ensure the Apply Selections button has worked, please
                    view the data on the Data to Analyze tab because the Apply Selections button updates the data table on that tab. The data that appears on the Data to
                    Analyze tab is the data that will be fed into the anomaly detection model."),
                 h3("Run Anomaly Detection"),
                 h5("Pressing the Run Anomaly Detection button runs the model and analyze the data.
                    The button should be the last action the program user performs.
                    If no parameters, such as account number, bill code, or date range are specified,
                    or no values are changed for Detection Sensitivity or Maximum Number of Anomalies,
                    then pressing the Run Anomaly Detection button will analyze the entire data set with the defaults
                    for Detection Sensitivity and Maximum Number of Anomalies. Please be sure to press the Apply Selections button before pressing the Run Anomaly
                    Detection Button. If you only adjust the Detection Sensitivity or Maximum Number of Anomalies, you only need to press the Run Anomaly Detection Button
                    to apply the changes, you do not need to press the Apply Selections button."),
                 h3("Download Table"),
                 h5("The Download Table button will download the data from the Table Results tab as an Excel file."),
                 h3("Download Components Graph"),
                 h5("The Download Components Graph button will download the upper graph from the Graph Results tab as a png file."),
                 h3("Download Overall Graph"),
                 h5("The Download Overall Graph button will download the lower graph from the Graph Results tab as a png file."),
                 h3(" "),
        ),       # this parenthesis ends the Help tabPanel
      )          # this parenthesis ends the tabsetPanel
    )            # this parenthesis ends the mainPanel
  ),             # this parenthesis ends the fluidPage
  
  ##### SERVER SIDE #####
  server = function(input, output, session) {
    file <- reactiveValues()
    # Select File button:
    observeEvent(input$uploaded_file,{
      isolate({
        import(input$uploaded_file$datapath) %>%
          mutate(date = make_date(year, month)) %>% # Creates date column by using year and month columns
          relocate(date, .before = account) ->> file # Save file as a global variable
      }) # ends isolate
      # Clean file while uploading the file and select the relevant columns (part of Select File button)
      updateSelectizeInput(session, "account", choices = file$account, server = TRUE)
      output$contents <- renderDataTable({
        file %>%
          filter(billcode == "WG1" | # filter data when file is imported include only the four bill codes specified
                   billcode == "IJ0" |
                   billcode == "IJ1" |
                   billcode == "J00") %>%
          dplyr::select(date, account, billcode, group, location, cyaconsum) # Only these columns will appear in the rendered table
      }) # ends renderDataTable for file upload
    }) # ends observeEvent for uploaded_file
    # Apply Selections button:
    observeEvent(input$filter,{
      if(is.null(input$billcode)){c("IJ0", "IJ1", "J00", "WG1") ->> billcode2} # global variable
      else{input$billcode ->> billcode2} # global variable
      if(is.null(input$account)){unique(file$account) ->> account2} # global variable
      else{input$account ->> account2} # global variable
      file %>%
        filter(account %in% account2) %>%
        filter(billcode %in% billcode2) %>%
        filter(date >= lubridate::ymd(input$dates[1]) & date <= lubridate::ymd(input$dates[2])) %>%
        dplyr::select(date, account, billcode, group, location, cyaconsum) ->> model_data # global variable
      output$model_data <- renderDataTable({
        model_data
      }) # ends renderDataTable for model_data
    }) # ends observeEvent for Apply Selections button
    # Run Anomaly Detection button:
    observeEvent(input$run_models, {
      df_list <- split(model_data, f = list(model_data$account, model_data$billcode), drop = TRUE)
      messed_up <- NULL # messed_up collects the models that could not run due to there not being enough data points to detect the period
      models <<- lapply(df_list, function(account) {
        account %>% as_tibble() -> account # save account to use for the anomaly detection model
        tryCatch({
          # Time Series Decomposition using the anomalize and tibbletime libraries
          ts_decomposition = account %>%
            time_decompose(cyaconsum, method = "stl", merge = TRUE, frequency = "auto", trend = "auto") %>%
            anomalize(remainder, method = "iqr", alpha = input$sensitivity,  max_anoms = input$maximum_number) %>%
            time_recompose()
          list(
            ts_decomposition = ts_decomposition,
            # Visualize observed, seasonality, trend, and remainder
            season_trend_rem_graph = ts_decomposition %>%
              plot_anomaly_decomposition(color_no = "royal blue", color_yes = "darkorange1", ncol = 1, size_circles = 4) +
              ggtitle(paste0("Account: ",
                            unique(account$account),
                            "  ",
                            "Billcode: ",
                            unique(account$billcode),
                            "  ",
                            "Location: ",
                            str_to_title(unique(account$location))),
                      paste(subtitle = "Components Graph Created On:", Sys.Date(), sep = " ")) +
              geom_line(color = "royalblue", size = .65) +
              theme(
                plot.subtitle = element_text(size = 11, color = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 16),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 14),
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 14),
                strip.text = element_text(size = 20, color = "white",),
                strip.background = element_rect(fill = "black")
              ), # ends the theme for season_trend_rem_graph
            # Visualize anomalies with seasonality and trend
            recomposed_graph = ts_decomposition %>%
              plot_anomalies(time_recomposed = TRUE, fill_ribbon = "gray92", color_no = "royal blue", size_circles = 4, color_yes = "darkorange1") +
              geom_line(color = "royal blue", size = .65) + 
              #geom_smooth(se = FALSE, color = "black", size = .5) + # optional line to add
              ggtitle(paste0("Account: ",
                             unique(account$account),
                             "  ",
                             "Billcode: ",
                             unique(account$billcode),
                             "  ",
                             "Location: ",
                             str_to_title(unique(account$location))),
                      paste(subtitle = "Overall Graph Created On:", Sys.Date(), sep = " ")) +
              ylab("Water Consumption in KGAL") +
              xlab("") +
              theme(
                plot.subtitle = element_text(size = 11, color = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 16),
                axis.title.y = element_text(size = 16),
                plot.title = element_text(size = 14),
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 14),
              ), # ends the theme for recomposed_graph
            # This code creates the data that appears on the Table Results tab:
            tsd_output = ts_decomposition %>%
              dplyr::select(account, billcode, group, location, date, cyaconsum, anomaly) %>% 
              arrange(desc(anomaly),desc(date))
          )}, # ends the tryCatch
          # This code collects the accounts and bill codes for which the model does not have enough data points to run:
          error = function(e){
            messed_up <<- c(messed_up, paste0(account$account[1], ".", account$billcode[1])); # global variable
            paste0(account$account[1], ".",  account$billcode[1])
          }) %>%
          return()
      }) # ends the lapply for the models
      updateSelectizeInput(session, "account_billcode", choices = names(df_list), server = TRUE)
      # Place results on Table Results tab:
      output$model_table <<- renderDataTable({cbind(data.frame(models[[input$account_billcode]]$tsd_output))})
      # Place the graphs on the Results Graphs tab:
      # output$season_trend_rem_graph <- renderPlot({models[[input$account_billcode]]$season_trend_rem_graph}, width = 850, height = 550)
      # output$recomposed_graph <- renderPlot({models[[input$account_billcode]]$recomposed_graph}, width = 850, height = 531.25) # golden ratio
      output$season_trend_rem_graph <- renderPlot({models[[input$account_billcode]]$season_trend_rem_graph}, width = 645, height = 600)
      output$recomposed_graph <- renderPlot({models[[input$account_billcode]]$recomposed_graph}, width = 645, height = 450)
    }) # ends the observeEvent for the Anomaly Detection button
    # Save the Graph Results:
    observeEvent(input$download_components_graph, {
      screenshot(id = "season_trend_rem_graph", scale = 1, filename = "WANDA_Components_Graph")
    }) # ends the observeEVent for the components_graph download
    observeEvent(input$download_overall_graph, {
      screenshot(id = "recomposed_graph", scale = 1, filename = "WANDA_Overall_Graph")
    }) # ends the observeEVent for the recomposed_graph download
    # Save the Table Results:
    output$download_table <- downloadHandler(
      filename = function(){
        "WANDA_Table_Results.xlsx"
        }, # ends function for filename
      content = function(file){
        openxlsx::write.xlsx(cbind(data.frame(models[[input$account_billcode]]$tsd_output)), file = file, asTable = TRUE, overwrite = TRUE)
        }) # end of downloadHandler for download_table
} # ends the function for the server side
) # ends the shiny App

##### END #####

