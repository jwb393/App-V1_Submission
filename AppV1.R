# GLOBAL FUNCTION TO RUN AT STARTUP
global = function(){   
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(shiny)
  library(shinyWidgets)
  library(shinycssloaders)
  library(tidyr)
  library(purrr)
  library(broom)
}

ui = function(){
  jp_solar <- read.csv('jp_solar.csv')
  muni_list <- unique(jp_solar$muni_code)
  muni_choices <- c("All", as.character(muni_list))
  
  fluidPage(theme = "style.css",
            div(style = "padding: 1px 0px; width: '100%'",
                titlePanel(title = "", windowTitle = "Japanese Municipalities Solar Installations")
            ),
            navbarPage(
              title = div(span("Japanese Municipalities Solar Installations",
                               style = "position: relative; top: 50%; transform: translateY(-50%);")),
              tabPanel("Total Solar Rate",
                       sidebarPanel(
                         pickerInput("municipality1Picker", "Filter to one municipality:", choices = muni_choices, selected = "All", multiple = FALSE),
                         radioButtons("ci1Picker", "Toggle Confidence Interval:", choices = c("On" = "on", "Off" = "off")),
                         radioButtons("outlier1Picker", "Filter to outlier only or all graphs:", choices = c("On" = "on", "Off" = "off")),
                         textOutput("meanText1")
                       ),
                       mainPanel(
                         plotOutput("total_sr_by_muni_plot") %>% withSpinner()
                       )
              ),
              tabPanel("Installs Per Month",
                       sidebarPanel(
                         pickerInput("municipality2Picker", "Filter to one municipality:", choices = muni_choices, selected = "All", multiple = FALSE),
                         radioButtons("ci2Picker", "Toggle Confidence Interval:", choices = c("On" = "on", "Off" = "off")),
                         radioButtons("outlier2Picker", "Filter to outlier only or all graphs:", choices = c("On" = "on", "Off" = "off")),
                         textOutput("meanText2")
                       ),
                       mainPanel(
                         plotOutput("sr_by_muni_plot") %>% withSpinner()
                       )
              )
            )
  )
}

server = function(input, output, session){
  jp_solar <- read.csv('jp_solar.csv')
  jp_solar$date <- as.Date(jp_solar$date)
  jp_solar$muni_code <- as.character(jp_solar$muni_code)
  
  monthly_solar_total <- jp_solar %>%
    filter(date <= '2017-03-25') %>%
    mutate(total_solar_rate = solar_under_10kw / pop * 1000)
  
  monthly_solar_total <- monthly_solar_total %>%
    arrange(date) %>%
    mutate(month_num = as.integer(as.factor(date)))
  
  # STEP 2: Fit Linear Model
  # Model solar_rate as a function of time
  modelMonth <- lm(solar_rate ~ month_num, data = monthly_solar_total)
  modelTotal <- lm(total_solar_rate ~ month_num, data = monthly_solar_total)
  
  # STEP 3: Summarize Model
  model_summary_month <- summary(modelMonth)
  model_summary_total <- summary(modelTotal)
  tidy_model_month <- tidy(modelMonth, conf.int = TRUE)
  tidy_model_total <- tidy(modelTotal, conf.int = TRUE)
  
  summary_stats_month <- monthly_solar_total %>%
    group_by(date) %>%
    summarize(
      mean_solar_rate = mean(solar_rate, na.rm = TRUE),
      sd_solar_rate = sd(solar_rate, na.rm = TRUE),
      se_solar_rate = sd_solar_rate / sqrt(n()),
      ci_lower = mean_solar_rate - 1.96 * se_solar_rate,
      ci_upper = mean_solar_rate + 1.96 * se_solar_rate,
      median_solar_rate = median(solar_rate, na.rm = TRUE),
      iqr_solar_rate = IQR(solar_rate, na.rm = TRUE)
    )
  
  summary_stats_total <- monthly_solar_total %>%
    group_by(date) %>%
    summarize(
      mean_solar_rate = mean(total_solar_rate, na.rm = TRUE),
      sd_solar_rate = sd(total_solar_rate, na.rm = TRUE),
      se_solar_rate = sd_solar_rate / sqrt(n()),
      ci_lower = mean_solar_rate - 1.96 * se_solar_rate,
      ci_upper = mean_solar_rate + 1.96 * se_solar_rate,
      median_solar_rate = median(total_solar_rate, na.rm = TRUE),
      iqr_solar_rate = IQR(total_solar_rate, na.rm = TRUE)
    )
  
  monthly_solar <- reactive({
    data <- monthly_solar_total
    
    if (!is.null(input$municipality1Picker) && input$municipality1Picker != "All") {
      data <- data %>% filter(muni_code == input$municipality1Picker)
    }
    
    if (input$outlier1Picker == "on") {
      data <- data %>% filter(muni_code != "4362")
    }
    
    data
  })
  
  monthly_solar_2 <- reactive({
    data <- jp_solar %>%
      filter(date <= '2017-03-25')
    
    if (!is.null(input$municipality2Picker) && input$municipality2Picker != "All") {
      data <- data %>% filter(muni_code == input$municipality2Picker)
    }
    
    if (input$outlier2Picker == "on") {
      data <- data %>% filter(muni_code != "4362")
    }
    
    data
  })
  
  output$total_sr_by_muni_plot <- renderPlot({
    p <- ggplot() +
      geom_line(data = monthly_solar(), 
                aes(x = date, y = total_solar_rate, group = muni_code, color = as.factor(muni_code)),
                alpha = 0.4)
    
    if (input$ci1Picker == "on") {
      p <- p +
        geom_line(data = summary_stats_total, aes(x = date, y = mean_solar_rate, color = "Mean Solar Rate"), size = 1) +
        geom_ribbon(data = summary_stats_total, aes(x = date, ymin = ci_lower, ymax = ci_upper, fill = "95% CI"), alpha = 0.5)
    }
    
    p + labs(
      title = "Total Solar Rate Over Time by Municipality",
      x = "Date",
      y = "Total Solar Rate",
      color = "Municipal Code")
  })
  
  output$sr_by_muni_plot <- renderPlot({
    p <- ggplot() +
      geom_line(data = monthly_solar_2(), 
                aes(x = date, y = solar_rate, group = muni_code, color = as.factor(muni_code)),
                alpha = 0.4)
    
    if (input$ci2Picker == "on") {
      p <- p +
        geom_line(data = summary_stats_month, aes(x = date, y = mean_solar_rate, color = "Mean Solar Rate"), size = 1) +
        geom_ribbon(data = summary_stats_month, aes(x = date, ymin = ci_lower, ymax = ci_upper, fill = "95% CI"), alpha = 0.5)
    }
    
    p + labs(
      title = "Solar Installation Rate Over Time by Municipality",
      x = "Date",
      y = "Solar Installation Rate",
      color = "Municipal Code")
  })
  output$meanText2 <- renderText({

    if (input$ci2Picker == "on") {
      beta <- tidy_model_month$estimate[2]
      p_val <- tidy_model_month$p.value[2]
      sprintf("Beta: %.4f \n p-value: %.4f\n", beta,p_val)
    }
  })
  
  output$meanText1 <- renderText({
    
    if (input$ci1Picker == "on") {
      beta <- tidy_model_total$estimate[2]
      p_val <- tidy_model_total$p.value[2]
      sprintf("Beta: %.4f \n p-value: %.4f\n", beta,p_val)
    }
  })
}

shinyApp(ui = ui, server = server, onStart = global)