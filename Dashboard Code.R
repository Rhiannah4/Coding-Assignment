#install packages

#   install.packages("readxl")
#   install.packages("googlesheets4")
#   install.packages("janitor")
#    install.packages("tidyverse")
#   install.packages("lme4")
#   install.packages("emmeans")
#   install.packages("dplyr")
#    install.packages("esquisse")
#   install.packages("effectsize")
#   install.packages("shinydashboard")
#   install.packages("kableExtra")
#   install.packages("shiny")
#   install.packages("ggplot2")
#   install.packages("readr")
#   install.packages ("rsconnect")

#run packages 

library(ggplot2)
library(readxl)
library(googlesheets4)
library(janitor)
library(tidyverse)
library(lme4)
library(emmeans)
library(dplyr)
library(esquisse)
library(effectsize)
library(shinydashboard)
library(shiny)
library(kableExtra)
library(readr)
library(rsconnect)


# set wd 

setwd("~/Library/CloudStorage/OneDrive-Personal/Teesside University/Semester 2/R Studio/Work Area/Assessment")

# import data 

data <- read_csv("Assessment Data.csv")
view(data)


# clean & tidy

data <- clean_names(data)
view(data)

colnames(data)

data$id <- as.factor(data$id)  
#data$date <- as.Date(data$date,format="%m-%d-%Y") #date being an issue
data$altitude_code<- as.factor(data$altitude_code)
data$gd<- as.factor(data$gd)
data$replication<- as.factor(data$replication)

view(data)

data <- data %>%           #renamed altitude levels for clarity
  mutate(altitude_code = recode(altitude_code, 
                                `1` = "Sea (0-1000m)", 
                                `2` = "Low (1000-2000m)", 
                                `3` = "Medium (>2000m)"))

data <- data %>%           #renamed game day for clarity
  mutate(gd = recode(gd, 
                     `0` = "MD", 
                     `1` = "MD-1", 
                     `2` = "MD-2"))

#wrangle
data <- data %>%     #removed heart rate data as was very sparse with lots of missing values
  select(-c(hr_zone3time, hr_zone4time, hr_zone5time, hr_zone6time, h_rzone_trimp))
view(data)



#SHINY  DASHBOARD
# UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Matchday Altitude Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Boxplot Analysis", tabName = "boxplot", icon = icon("chart-bar")),
      menuItem("Lollipop Graph", tabName = "lollipop", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # 📌 Boxplot Tab ----
      tabItem(tabName = "boxplot",
              selectInput("view_option", "View Option:", 
                          choices = c("Single Metric", "Both Metrics"), 
                          selected = "Single Metric"),
              
              selectInput("metric", "Select Metric:", 
                          choices = c("HSR per min" = "hsr", "Soreness" = "soreness"), 
                          selected = "hsr"),
              
              fluidRow(
                box(plotOutput("altitude_plot"), width = 12)
              ),
              
              # Conditional Summary Box (Only for Both Metrics)
              conditionalPanel(
                condition = "input.view_option == 'Both Metrics'",
                box(
                  title = "What Impact does Altitude have on Matchday HSR (m/min) and Soreness?",
                  width = 12,
                  textOutput("summary_text")
                )
              )
      ),
     
              
      # 📌 Lollipop Graph Tab ----
      tabItem(tabName = "lollipop",
              selectInput("view_option_lollipop", "View Option:", 
                          choices = c("Single Metric", "Both Metrics"), 
                          selected = "Single Metric"),
              
              selectInput("lollipop_metric", "Select Metric:", 
                          choices = c("HSR per min" = "hsr", "Soreness" = "soreness"), 
                          selected = "hsr"),
              
              # Single Metric Lollipop Plot
              conditionalPanel(
                condition = "input.view_option_lollipop == 'Single Metric'",
                box(plotOutput("lollipop_plot"), width = 12)
              ),
              
              # Both Metrics Lollipop Plots Side by Side
              conditionalPanel(
                condition = "input.view_option_lollipop == 'Both Metrics'",
                fluidRow(
                  box(plotOutput("lollipop_plot_hsr"), width = 6),
                  box(plotOutput("lollipop_plot_soreness"), width = 6)
                ),
                # Conditional Summary Box for Lollipop Graph
                box(
                  title = "What Impact does Altitude have on Matchday HSR (m/min) and Soreness Across Individuals?",
                  width = 12,
                  textOutput("summary_text_lollipop")
                )
              )
      )
    )
  )
)

# Server ----
server <- function(input, output) {
  
  # Convert data to long format for boxplot
  soreness_1_long <- soreness_1 %>%
    pivot_longer(cols = c(mean_hs_rmin, mean_soreness_1), names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = recode(Metric, mean_hs_rmin = "HSR (m/min)", mean_soreness_1 = "Soreness (1-10)"))
  
  output$altitude_plot <- renderPlot({
    if (input$view_option == "Single Metric") {
      metric_column <- ifelse(input$metric == "hsr", "mean_hs_rmin", "mean_soreness_1")
      metric_label <- ifelse(input$metric == "hsr", "Matchday Mean High-Speed Running (m/min)", "Matchday Mean Soreness")
      
      ggplot(soreness_1, aes(x = altitude_code, y = .data[[metric_column]], fill = altitude_code)) +
        geom_boxplot(outlier.shape = NA) +  
        geom_point(color = "red", size = 2, alpha = 0.8, position = position_nudge(x = 0)) +  
        scale_fill_manual(values = c(Sea = "#011925", Low = "#418cdd", Medium = "#c3a871")) +
        scale_x_discrete(labels = c("Sea" = "Sea (0-1000m)", "Low" = "Low (1000-2000m)", "Medium" = "Medium (>2000m)")) + 
        theme_classic() + theme(legend.position = "none") +
        labs(
          x = "Altitude Category",
          y = metric_label,  
          fill = "Altitude Level",
          title = paste("Impact of Altitude on", metric_label)
        )
    } else {
      ggplot(soreness_1_long, aes(x = altitude_code, y = Value, fill = altitude_code)) +
        geom_boxplot(outlier.shape = NA) +  
        geom_point(color = "red", size = 2, alpha = 0.8, position = position_nudge(x = 0)) +  
        facet_wrap(~ Metric, scales = "free") +
        scale_fill_manual(values = c(Sea = "#011925", Low = "#418cdd", Medium = "#c3a871")) +
        scale_x_discrete(labels = c("Sea" = "Sea (0-1000m)", "Low" = "Low (1000-2000m)", "Medium" = "Medium (>2000m)")) + 
        theme_classic() + theme(legend.position = "none") +
        labs(
          x = "Altitude Category",
          y = "",  
          fill = "Altitude Level",
          title = "Impact of Altitude on Matchday HSR (m/min) and Soreness"
        )
    }
  })
  
  # Compute individual differences for Lollipop Graph
  individual_differences <- soreness_1 %>%
    group_by(id) %>%
    summarize(
      hsr_diff = mean(mean_hs_rmin[altitude_code != "Sea"], na.rm = TRUE) - mean(mean_hs_rmin[altitude_code == "Sea"], na.rm = TRUE),
      soreness_diff = mean(mean_soreness_1[altitude_code != "Sea"], na.rm = TRUE) - mean(mean_soreness_1[altitude_code == "Sea"], na.rm = TRUE)
    ) %>%
    pivot_longer(cols = c(hsr_diff, soreness_diff), names_to = "Metric", values_to = "Difference") %>%
    mutate(Metric = recode(Metric, hsr_diff = "HSR per min", soreness_diff = "Soreness"))
  
  render_lollipop <- function(metric_label) {
    plot_data <- individual_differences %>%
      filter(Metric == metric_label, !is.na(Difference))
    
    ggplot(plot_data, aes(x = Difference, y = reorder(id, Difference))) +
      geom_rect(aes(xmin = -1, xmax = 1, ymin = -Inf, ymax = Inf), fill = "gray80", alpha = 0.3) +
      geom_segment(aes(xend = 0, yend = id), color = "black") +
      geom_point(size = 4, color = "red") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      theme_classic() + theme(legend.position = "none") +
      labs(x = "Difference from Sea Level (0-1000m) to Altitude (>1000m)", y = "Player ID",  title = ifelse(metric_label == "HSR per min", 
                                                                                                            "Individual Differences in Matchday HSR (m/min) from Sea Level to Altitude", 
                                                                                                            paste("Individual Differences in", metric_label, "from Sea Level to Altitude")))
  }
  
  # Render Single Lollipop Graph
  output$lollipop_plot <- renderPlot({
    render_lollipop(ifelse(input$lollipop_metric == "hsr", "HSR per min", "Soreness"))
  })
  
  # Render Both Lollipop Graphs Side by Side
  output$lollipop_plot_hsr <- renderPlot({ render_lollipop("HSR per min") })
  output$lollipop_plot_soreness <- renderPlot({ render_lollipop("Soreness") })
  
  # Create summary text for findings (Boxplot)
  output$summary_text <- renderText({
    if (input$view_option == "Both Metrics") {
      return("The Boxplots show no significant differences in matchday high-speed running (m/min) across the altitude categories, suggesting that altitude had minimal impact on matchday running output. However, next-day soreness ratings were lower at medium altitudes (>2000m), showing a drop from a ~6/10 to a 3/10, indicating players experienced greater muscle soreness. At moderate altitude there is an average increase in soreness of 3.15 AU on the Likert Scale, which is greater than the smallest worthwhile change. Furthermore, moderate altitude appears to increase sorenesss somewhere between 2.23 and 4.02 units. This increased soreness may reflect greater muscle stress due to lower atmospheric pressure and reduced oxygen availability. These findings highlight the importance of enhanced recovery strategies - such as extended recovery time - when playing at higher altitudes. Overall, the data suggests that while altitude may not drastically alter match-day running performance, it can influence fatigue and recovery.")
    } else {
      metric_label <- ifelse(input$metric == "hsr", "HSR per min", "Soreness")
      return(paste("The analysis for", metric_label, "at different altitudes suggests the impact of altitude on the metric. Further detailed analysis is required to draw conclusions on the impact at different altitude levels."))
    }
  })
  
  # Create summary text for findings (Lollipop)
  output$summary_text_lollipop <- renderText({
    if (input$view_option_lollipop == "Both Metrics") {
      return("When exploring individual responses, 4 players demonstrated an increase in matchday HSR (m/min) and soreness at altitude. In comparison, 6 players reported reduced HSR (m/min) at altitude, of which 2 showed a decrease in soreness. Notably, the other 4 players demonstrated mismatched trends with an increase in soreness with a reduction in HSR at altitude. Moreover, it is important to note no player increased in HSR (m/min) or reported less soreness outside of the smallest worthwhile change (+1) whereas the magnitude of HSR(m/min) decrease and soreness increase was much larger. This highlights the individual variability in adaptation to altitude and the importance of monitoring both internal and external load, further supporting the importance of obtaining soreness data at altitude.")
    } else {
      metric_label <- ifelse(input$lollipop_metric == "hsr", "HSR per min", "Soreness")
      return(paste("The analysis of", metric_label, "individual differences at different altitudes suggests significant changes in performance and discomfort. Further exploration may be needed to assess the implications of these changes at different altitudes."))
    }
  })
}

# Run App ----
shinyApp(ui = ui, server = server)



