#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(ggforce)
library(plotly)
library(dplyr)
library(zoo)
library(tidyverse)




# ----------------------------------------------------------------------------
# Method to Cache api pull. Not Currently Used as pull anonyminized CSV Data
# ----------------------------------------------------------------------------
# download_new_activities <- function(token, prev_activitiy_df, previous_pulled_df, PARAMS, GROUPBY){
#   new_activity_pull <- ofCloudGetActivities(token)
#   if (length(previous_pulled_df >0)){
#     activities_to_pull <- setdiff(new_activity_pull$id, prev_activitiy_df$id)
#     # print(activities_to_pull)
#     common_ids <- intersect(new_activity_pull$id, prev_activitiy_df$id)
#     
#     new_activity_pull$modified_at <- as.POSIXct(new_activity_pull$modified_at)
#     prev_activitiy_df$modified_at <- as.POSIXct(prev_activitiy_df$modified_at)
#     
#     # Find updated activities
#     updated_activities <- new_activity_pull %>%
#       filter(id %in% common_ids) %>%
#       inner_join(prev_activitiy_df, by = "id", suffix = c(".new", ".old")) %>%
#       filter(modified_at.new > modified_at.old) %>%
#       select(id, modified_at = modified_at.new) #rename to match new_activities columns
#     
#     activities_to_pull <- c(activities_to_pull, updated_activities$id)
#   } else{
#     activities_to_pull <- new_activity_pull$id
#   }
#   
#   if (length(activities_to_pull)>0){
#     
#     
#     
#     pulled_data <- ofCloudGetStatistics(
#       token, params = PARAMS, groupby = GROUPBY, filters = list(name = "activity_id",
#                                                                 comparison = "=",
#                                                                 values = activities_to_pull)
#     )
#     combined_df <- rbind(previous_pulled_df, pulled_data)
#     return(combined_df)
#   }
#   else{
#     return(previous_pulled_df)
#   }
# }
# 





function(input, output, session) {
# 
  options(warn = -1)
  df <- reactiveVal(data.frame())
  
  observe({
    # activities  <- ofCloudGetActivities(token)
    # 
    # temp_df<- ofCloudGetStatistics(
    #   token,
    #   params = c("athlete_name", "position_name", "dynamic_movement_load",
    #              "total_distance", "time_length","baseball_swing_count", "baseball_swing_maxpl", "baseball_throw_count", "baseball_throw_maxpl",   "total_player_load", "softball_sprint", "throw_pl_per_throw","throw_load_per_throw_scaled", "swing_load_per_swing", "date", "weighted_dist",
    #              "period_id", "period_name", "activity_name", "start_time", "end_time",  "day_name", "baseball_throw_count"),
    #   groupby = c("athlete", "period", "activity"),
    #   filters = list(name = "activity_id",
    #                  comparison = "=",
    #                  values = activities$id))
    # print("Pull Complete")
    df(read_csv("Softball_Data.csv"))

    updateSelectInput(session, "selected_player",
                      choices = unique(df()$athlete_name))
    updateSelectInput(session, "selected_player_ac",
                      choices = unique(df()$athlete_name),
                      selected =unique(df()$athlete_name))
    
    print("updated_selector")
    })
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  player_load_thresholds <- reactive({
    athlete_sum_load <- df() %>%
      group_by(athlete_name, date,position_name, day_name)  %>%
      summarise(sum_dml = sum(dynamic_movement_load, na.rm = TRUE),
                sum_wd = sum(weighted_dist, na.rm = TRUE),
                sum_sprint = sum(softball_sprint, na.rm = TRUE),
                sum_throw = sum(baseball_throw_maxpl, na.rm = TRUE),
                sum_swing = sum(baseball_swing_maxpl, na.rm = TRUE)) 
    
    quantiles <- athlete_sum_load %>%
      group_by(athlete_name) %>%
      summarise(
        tq1 = quantile(sum_throw, .20),
        tq3 = quantile(sum_throw, .8),
        sq1 = quantile(sum_swing, .25),
        sq3 = quantile(sum_swing, .75),
        llq1 = quantile(sum_wd, .25),
        llq3 = quantile(sum_wd, .75),
        dmlq1 = quantile(sum_dml, .25),
        dmlq3 = quantile(sum_dml, .75)

      )
    
    return(quantiles)
  })
  
  q_level = .25
  
  rolling_df <- reactive({
    rolling_df <- df()  %>% 
      select(athlete_name, date, softball_sprint, weighted_dist,
             baseball_throw_maxpl, baseball_swing_maxpl, dynamic_movement_load, baseball_throw_count) %>%
      group_by(athlete_name, date) %>%
      summarise(sprint_avg_yds = mean(softball_sprint, na.rm = TRUE),
                sprint_total_yds = sum(softball_sprint, na.rm = TRUE),
                lower_load_avg = mean(weighted_dist, na.rm = TRUE),
                lower_load_sum = sum(weighted_dist, na.rm = TRUE),
                throw_avg = mean(baseball_throw_maxpl, na.rm = TRUE),
                throw_total = sum(baseball_throw_maxpl, na.rm = TRUE),
                swing_avg = mean(baseball_swing_maxpl, na.rm = TRUE),
                swing_total = sum(baseball_swing_maxpl, na.rm = TRUE),
                dml_sum = sum(dynamic_movement_load, na.rm = TRUE),
                throw_count_sum = sum(baseball_throw_count, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        date = as.Date(date, format= "%d/%m/%Y")
        ) %>%
      group_by(athlete_name) %>%
      arrange(date) %>%
      mutate(
        rolling_throw_lower = map_dbl(seq_along(throw_total), ~quantile(throw_total[1:min(.,90)][throw_total>0], q_level, na.rm = TRUE)),
        rolling_throw_upper = map_dbl(seq_along(throw_total), ~quantile(throw_total[1:min(.,90)][throw_total>0], 1-q_level, na.rm = TRUE)),
        rolling_swing_lower = map_dbl(seq_along(swing_total), ~quantile(swing_total[1:min(.,90)][swing_total>0], q_level, na.rm = TRUE)),
        rolling_swing_upper = map_dbl(seq_along(swing_total), ~quantile(swing_total[1:min(.,90)][swing_total>0], 1-q_level, na.rm = TRUE)),
        rolling_ll_lower = map_dbl(seq_along(lower_load_sum), ~quantile(lower_load_sum[1:min(.,90)][lower_load_sum>50], q_level, na.rm = TRUE)),
        rolling_ll_upper = map_dbl(seq_along(lower_load_sum), ~quantile(lower_load_sum[1:min(.,90)][lower_load_sum>50], 1-q_level, na.rm = TRUE)),
        rolling_sprint_lower = map_dbl(seq_along(sprint_total_yds), ~quantile(sprint_total_yds[1:min(.,90)][sprint_total_yds>0], q_level, na.rm = TRUE)),
        rolling_sprint_upper = map_dbl(seq_along(sprint_total_yds), ~quantile(sprint_total_yds[1:min(.,90)][sprint_total_yds>0], 1-q_level, na.rm = TRUE)),
        rolling_dml_lower = map_dbl(seq_along(dml_sum), ~quantile(dml_sum[1:min(.,90)][dml_sum>0], q_level, na.rm = TRUE)),
        rolling_dml_upper = map_dbl(seq_along(dml_sum), ~quantile(dml_sum[1:min(.,90)][dml_sum>0], 1-q_level, na.rm = TRUE)),
        rolling_tc_lower = map_dbl(seq_along(throw_count_sum), ~quantile(throw_count_sum[1:min(.,90)][dml_sum>0], q_level, na.rm = TRUE)),
        rolling_tc_upper = map_dbl(seq_along(throw_count_sum), ~quantile(throw_count_sum[1:min(.,90)][dml_sum>0], 1-q_level, na.rm = TRUE))
      ) %>%
      mutate(
        across(starts_with("rolling_"), 
               ~ rollmean(.x, k = 20, fill = NA, align = "right"), 
               .names = "smooth_{.col}")
      ) %>%
      ungroup()

    
    })
  
  
  output$testing_rolling_table <- renderDT(rolling_df())
  output$testing_filtered_table <- renderDT(filtered_df() %>%
                                              select(date, athlete_name, lower_load_sum, sprint_total_yds, throw_total, swing_total,dml_sum)%>%
                                              rename(
                                                `Date` = date,
                                                `Athlete Name` = athlete_name,
                                                `Lower Body Load` = lower_load_sum,
                                                `Sprint Yards` = sprint_total_yds,
                                                `Throw Load` = throw_total,
                                                `Swing Load` = swing_total,
                                                `Dynamic Movement Load` = dml_sum
                                              )%>%
                                              mutate_if(is.numeric, round, 0))
  filtered_df <- reactive({
   filtered_df <- df()  %>% 
      filter(athlete_name == input$selected_player)%>%
     select(athlete_name, date, softball_sprint, weighted_dist,
            baseball_throw_maxpl, baseball_swing_maxpl, dynamic_movement_load) %>%
     group_by(athlete_name, date) %>%
     summarise(sprint_avg_yds = mean(softball_sprint, na.rm = TRUE),
               sprint_total_yds = sum(softball_sprint, na.rm = TRUE),
               lower_load_avg = mean(weighted_dist, na.rm = TRUE),
               lower_load_sum = sum(weighted_dist, na.rm = TRUE),
               throw_avg = mean(baseball_throw_maxpl, na.rm = TRUE),
               throw_total = sum(baseball_throw_maxpl, na.rm = TRUE),
               swing_avg = mean(baseball_swing_maxpl, na.rm = TRUE),
               swing_total = sum(baseball_swing_maxpl, na.rm = TRUE),
               dml_sum = sum(dynamic_movement_load, na.rm = TRUE)) %>%
     ungroup() %>%
     { 
       date_range <- seq(min(.$date), max(.$date), by = "day")
       complete_df <- data.frame(date = date_range)
       merged_df <- merge(complete_df, ., by = "date", all.x = TRUE)
       merged_df[is.na(merged_df)] <- 0 
       merged_df 
     } %>%
     mutate(
       athlete_name = input$selected_player,
       date = as.Date(date, format= "%d/%m/%Y")
     ) %>%
     group_by(athlete_name) %>%
     arrange(date) %>%
     mutate(
       rolling_throw_lower = map_dbl(seq_along(throw_total), ~quantile(throw_total[1:min(.,90)][throw_total>0], q_level, na.rm = TRUE)),
       rolling_throw_upper = map_dbl(seq_along(throw_total), ~quantile(throw_total[1:min(.,90)][throw_total>0], 1-q_level, na.rm = TRUE)),
       rolling_swing_lower = map_dbl(seq_along(swing_total), ~quantile(swing_total[1:min(.,90)][swing_total>0], q_level, na.rm = TRUE)),
       rolling_swing_upper = map_dbl(seq_along(swing_total), ~quantile(swing_total[1:min(.,90)][swing_total>0], 1-q_level, na.rm = TRUE)),
       rolling_ll_lower = map_dbl(seq_along(lower_load_sum), ~quantile(lower_load_sum[1:min(.,90)][lower_load_sum>50], q_level, na.rm = TRUE)),
       rolling_ll_upper = map_dbl(seq_along(lower_load_sum), ~quantile(lower_load_sum[1:min(.,90)][lower_load_sum>50], 1-q_level, na.rm = TRUE)),
       rolling_sprint_lower = map_dbl(seq_along(sprint_total_yds), ~quantile(sprint_total_yds[1:min(.,90)][sprint_total_yds>0], q_level, na.rm = TRUE)),
       rolling_sprint_upper = map_dbl(seq_along(sprint_total_yds), ~quantile(sprint_total_yds[1:min(.,90)][sprint_total_yds>0], 1-q_level, na.rm = TRUE)),
       rolling_dml_lower = map_dbl(seq_along(dml_sum), ~quantile(dml_sum[1:min(.,90)][dml_sum>0], q_level, na.rm = TRUE)),
       rolling_dml_upper = map_dbl(seq_along(dml_sum), ~quantile(dml_sum[1:min(.,90)][dml_sum>0], 1-q_level, na.rm = TRUE))
     ) %>%
     mutate(
       across(starts_with("rolling_"), 
              ~ rollmean(.x, k = 20, fill = NA, align = "right"), 
              .names = "smooth_{.col}")
     ) %>%
     ungroup()%>%
     filter(date >= input$date_range[1] & date <= input$date_range[2]) 

   
  
  })
  
  
  
  
  filtered_df_only_session <- reactive({
    filtered_df <- df()  %>%
      filter(athlete_name == input$selected_player,
             date >= input$date_range[1] & date <= input$date_range[2]) %>% 
      select(athlete_name, date, softball_sprint, weighted_dist,
             baseball_throw_maxpl, baseball_swing_maxpl) %>%
      group_by(athlete_name, date) %>%
      summarise(sprint_avg_yds = mean(softball_sprint, na.rm = TRUE),
                sprint_total_yds = sum(softball_sprint, na.rm = TRUE),
                lower_load_avg = mean(weighted_dist, na.rm = TRUE),
                lower_load_sum = sum(weighted_dist, na.rm = TRUE),
                throw_avg = mean(baseball_throw_maxpl, na.rm = TRUE),
                throw_total = sum(baseball_throw_maxpl, na.rm = TRUE),
                swing_avg = mean(baseball_swing_maxpl, na.rm = TRUE),
                swing_total = sum(baseball_swing_maxpl, na.rm = TRUE))%>%
      left_join(player_load_thresholds(), by = "athlete_name") %>%
      mutate(
        throw_category = case_when(
          throw_total > tq3 ~ "high",
          throw_total >= tq1 ~ "medium",
          TRUE ~ "low"
        ),
        swing_category = case_when(
          swing_total > sq3 ~ "high",
          swing_total >= sq1 ~ "medium",
          TRUE ~ "low"
        ),
        wd_category = case_when(
          lower_load_sum > llq3 ~ "high",
          lower_load_sum >= llq1 ~ "medium",
          TRUE ~ "low"
        )

      ) %>%
      select(-tq1, -tq3, -sq1, -sq3, -llq1, -llq3, -dmlq1, -dmlq3) %>%
      mutate(date = as.Date(date, format = "%d/%m/%Y"))
  })
    
  output$player_name_title <- renderText({
    req(input$selected_player)  
    paste(input$selected_player, "Load") 
  })
  
  output$test_ll_table <- renderTable({ 
    df_longer <- filtered_df()%>%
      select(athlete_name, date, lower_load_sum, rolling_ll_lower, rolling_ll_upper) %>%
      pivot_longer(cols = c(lower_load_sum, rolling_ll_lower, rolling_ll_upper),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(date = as.Date(date, format = "%d/%m/%Y")) 

  })
  
  output$lower_load_range_plot <- renderPlotly({
    df_ll_temp <- filtered_df() %>% select(athlete_name, date, lower_load_sum, smooth_rolling_ll_lower, smooth_rolling_ll_upper)
    plot_ly(df_ll_temp,
            x = ~date) %>%
      
      # Green shading below rolling_ll_lower
      add_ribbons(ymin = 0, ymax = ~smooth_rolling_ll_lower,
                  fillcolor = "green", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("Low Flag Bound: 0 - ", round(smooth_rolling_ll_lower, 0),  sep = ""),
                  hoverinfo = "text") %>%
      
      # Yellow shading between rolling_ll_lower and rolling_ll_upper
      add_ribbons(ymin = ~smooth_rolling_ll_lower, ymax = ~smooth_rolling_ll_upper,
                  fillcolor = "yellow", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("Med Flag Bound: ", round(smooth_rolling_ll_lower, 0), " - ", round(smooth_rolling_ll_upper, 0), sep = ""),
                  hoverinfo = "text") %>%
      
      # Red shading above rolling_ll_upper
      add_ribbons(ymin = ~smooth_rolling_ll_upper, ymax = max(df_ll_temp$lower_load_sum, na.rm = TRUE),
                  fillcolor = "red", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("High Flag Bound: ", round(smooth_rolling_ll_upper, 0), "+ ",  sep = ""),
                  hoverinfo = "text") %>%
      
      # Line for lower_load_sum
      add_trace(y = ~lower_load_sum, type = 'scatter', mode = 'lines', name = "Lower Load Sum",
                line = list(color = "black"),
                text = ~paste("Lower Body Load: ", round(lower_load_sum, 0),  sep = ""),
                hoverinfo = "text") %>%
      layout(
        title = "Lower Body Load",
        legend = list(orientation = "h",   
                      x = 0.5,             
                      y = -0.2,            
                      xanchor = "center",  
                      yanchor = "top"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Lower Body Load"),
        hovermode = "x unified"
      )  
    
    
    
    
  })
  output$throw_range_plot <- renderPlotly({
    df_throw_temp <- filtered_df() %>% select(athlete_name, date,  throw_total, smooth_rolling_throw_lower, smooth_rolling_throw_upper)
    plot_ly(df_throw_temp,
            x = ~date) %>%
      
      # Green shading below rolling_ll_lower
      add_ribbons(ymin = 0, ymax = ~smooth_rolling_throw_lower,
                  fillcolor = "green", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("Low Flag Bound: 0 - ", round(smooth_rolling_throw_lower, 0),  sep = ""),
                  hoverinfo = "text") %>%
      
      # Yellow shading between rolling_ll_lower and rolling_ll_upper
      add_ribbons(ymin = ~smooth_rolling_throw_lower, ymax = ~smooth_rolling_throw_upper,
                  fillcolor = "yellow", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("Med Flag Bound: ", round(smooth_rolling_throw_lower, 0), " - ", round(smooth_rolling_throw_upper, 0), sep = ""),
                  hoverinfo = "text") %>%
      
      # Red shading above rolling_ll_upper
      add_ribbons(ymin = ~smooth_rolling_throw_upper, ymax = max(df_throw_temp$throw_total, na.rm = TRUE),
                  fillcolor = "red", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("High Flag Bound: ", round(smooth_rolling_throw_upper, 0), "+ ",  sep = ""),
                  hoverinfo = "text") %>%
      
      # Line for lower_load_sum
      add_trace(y = ~throw_total, type = 'scatter', mode = 'lines', name = "Throw Load",
                line = list(color = "black"),
                            text = ~paste("Throw  Load: ", round(throw_total, 0),  sep = ""),
                            hoverinfo = "text")%>%
      layout(
        title = "Throw Load",
        legend = list(orientation = "h",  
                      x = 0.5,            
                      y = -0.2,           
                      xanchor = "center",  
                      yanchor = "top"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Throw Load"),
        hovermode = "x unified"
      )  
    
    
    
    
  })
  output$swing_load_range_plot <- renderPlotly({
        df_swing_temp <- filtered_df() %>% select(athlete_name, date, swing_total,smooth_rolling_swing_lower,smooth_rolling_swing_upper)
    plot_ly(df_swing_temp,
            x = ~date) %>%
      
      # Green shading below rolling_ll_lower
      add_ribbons(ymin = 0, ymax = ~smooth_rolling_swing_lower,
                  fillcolor = "green", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("Low Flag Bound: 0 - ", round(smooth_rolling_swing_lower, 0),  sep = ""),
                  hoverinfo = "text") %>%
      
      # Yellow shading between rolling_ll_lower and rolling_ll_upper
      add_ribbons(ymin = ~smooth_rolling_swing_lower, ymax = ~smooth_rolling_swing_upper,
                  fillcolor = "yellow", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("Med Flag Bound: ", round(smooth_rolling_swing_lower, 0), " - ", round(smooth_rolling_swing_upper, 0), sep = ""),
                  hoverinfo = "text") %>%
      
      # Red shading above rolling_ll_upper
      add_ribbons(ymin = ~smooth_rolling_swing_upper, ymax = max(df_swing_temp$swing_total, na.rm = TRUE),
                  fillcolor = "red", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("High Flag Bound: ", round(smooth_rolling_swing_upper, 0), "+ ",  sep = ""),
                  hoverinfo = "text") %>%
      
      # Line for lower_load_sum
      add_trace(y = ~swing_total, type = 'scatter', mode = 'lines', name = "Swing Load",
                line = list(color = "black"),
                text = ~paste("Swing  Load: ", round(swing_total, 0),  sep = ""),
                hoverinfo = "text")%>%
      layout(
        title = "Swing Load",
        legend = list(orientation = "h",  
                      x = 0.5,             
                      y = -0.2,            
                      xanchor = "center",  
                      yanchor = "top"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Swing Load"),
        hovermode = "x unified"
      )  
    
    
  })
  
  output$sprint_load_range_plot <- renderPlotly({
    df2 <- filtered_df() %>% select(athlete_name, date, sprint_total_yds, smooth_rolling_sprint_lower, smooth_rolling_sprint_upper)
    plot_ly(df2,
            x = ~date) %>%
      
      # Green shading below rolling_ll_lower
      add_ribbons(ymin = 0, ymax = ~smooth_rolling_sprint_lower,
                  fillcolor = "green", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("Low Flag Bound: 0 - ", round(smooth_rolling_sprint_lower, 0),  sep = ""),
                  hoverinfo = "text") %>%
      
      # Yellow shading between rolling_ll_lower and rolling_ll_upper
      add_ribbons(ymin = ~smooth_rolling_sprint_lower, ymax = ~smooth_rolling_sprint_upper,
                  fillcolor = "yellow", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("Med Flag Bound: ", round(smooth_rolling_sprint_lower, 0), " - ", round(smooth_rolling_sprint_upper, 0), sep = ""),
                  hoverinfo = "text") %>%
      
      # Red shading above rolling_ll_upper
      add_ribbons(ymin = ~smooth_rolling_sprint_upper, ymax = max(df2$sprint_total_yds, na.rm = TRUE),
                  fillcolor = "red", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("High Flag Bound: ", round(smooth_rolling_sprint_upper, 0), "+ ",  sep = ""),
                  hoverinfo = "text") %>%
      
      # Line for lower_load_sum
      add_trace(y = ~sprint_total_yds, type = 'scatter', mode = 'lines', name = "Sprint Yards",
                line = list(color = "black"),
                            text = ~paste("Sprint Yards: ", round(sprint_total_yds, 0),  sep = ""),
                            hoverinfo = "text")%>%
      layout(
        title = "Sprint Yards",
        legend = list(orientation = "h",   
                      x = 0.5,             
                      y = -0.2,            
                      xanchor = "center",  
                      yanchor = "top"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Sprint Yards"),
        hovermode = "x unified"
      )  
    
    
  })
  
  output$dml_load_range_plot <- renderPlotly({
    df2 <- filtered_df() %>% select(athlete_name, date, dml_sum, smooth_rolling_dml_lower, smooth_rolling_dml_upper)
    plot_ly(df2,
            x = ~date) %>%
      
      # Green shading below rolling_ll_lower
      add_ribbons(ymin = 0, ymax = ~smooth_rolling_dml_lower,
                  fillcolor = "green", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("Low Flag Bound: 0 - ", round(smooth_rolling_dml_lower, 0),  sep = ""),
                  hoverinfo = "text") %>%
      
      # Yellow shading between rolling_ll_lower and rolling_ll_upper
      add_ribbons(ymin = ~smooth_rolling_dml_lower, ymax = ~smooth_rolling_dml_upper,
                  fillcolor = "yellow", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("Med Flag Bound: ", round(smooth_rolling_dml_lower, 0), " - ", round(smooth_rolling_dml_upper, 0), sep = ""),
                  hoverinfo = "text") %>%
      
      # Red shading above rolling_ll_upper
      add_ribbons(ymin = ~smooth_rolling_dml_upper, ymax = max(df2$dml_sum, na.rm = TRUE),
                  fillcolor = "red", opacity = 0.3, line = list(width = 0), showlegend = FALSE,
                  text = ~paste("High Flag Bound: ", round(smooth_rolling_dml_upper, 0), "+ ",  sep = ""),
                  hoverinfo = "text") %>%
      
      # Line for lower_load_sum
      add_trace(y = ~dml_sum, type = 'scatter', mode = 'lines', name = "DML",
                line = list(color = "black"),
                text = ~paste("Sprint Yards: ", round(dml_sum, 0),  sep = ""),
                hoverinfo = "text")%>%
      layout(
        title = "Dynamic Movement Load",
        legend = list(orientation = "h",  
                      x = 0.5,             
                      y = -0.2,            
                      xanchor = "center",  
                      yanchor = "top"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Dynamic Movement Load"),
        hovermode = "x unified"
      )  
    
    
  })
  
  #Player Swing Load Graph filtered_df()$swing_total)
  output$player_swing_load <- renderPlotly({
    throw_load_fig <- plot_ly(filtered_df(),
                              x = ~date,
                              y = ~swing_total,
                              type = "scatter",
                              name = paste(input$selected_player,"Swing Load"),
                              mode = 'lines', text = paste0("Swing Load", ": ", round(filtered_df()$swing_total, 0)),
                              hoverinfo = "text") %>%
      layout(
        title = "Swing Load",
        legend = list(orientation = "h",   
                      x = 0.5,             
                      y = -0.2,            
                      xanchor = "center",  
                      yanchor = "top"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Swing Load"),
        hovermode = "x unified"
      )              
      avg_throw <- mean(filtered_df_only_session()$throw_total, na.rm = TRUE)
      throw_load_fig <- throw_load_fig %>%
        add_trace(y = rep(avg_throw, nrow(filtered_df())), 
                  type = "scatter",
                  mode = "lines",
                  name = paste(input$selected_player,"Average Daily Swing Load"),
                  x = filtered_df()$date,  
                  hoverinfo = "text", 
                  text = paste0("Avg Swing Load: ", round(avg_throw, 0)) 
        )
    
    return(throw_load_fig)
  })
  
  

  player_role <- reactive({
    temp_df <- df()  %>% filter(athlete_name == input$selected_player)
    
  })
  
  output$isPitcher <- reactive({
    player_role() == "Pitcher"
  })
  outputOptions(output, "isPitcher", suspendWhenHidden = FALSE)


  #Player Throw  Load Graph


  output$player_throw_load <- renderPlotly({
      
      throw_load_fig <- plot_ly(filtered_df(),
                                x = ~date,
                                y = ~throw_total,
                                type = "scatter",
                                name = paste(input$selected_player,"Throw Load"),
                                mode = 'lines', text = paste0("Throw Load", ": ", round(filtered_df()$throw_total, 0)),
                                hoverinfo = "text") %>%
        layout(
          title = "Throw Load",
          legend = list(orientation = "h",   
                        x = 0.5,             
                        y = -0.2,            
                        xanchor = "center",  
                        yanchor = "top"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Throw Load"),
          hovermode = "x unified"
        )              
        avg_throw <- mean(filtered_df_only_session()$throw_total, na.rm = TRUE)
        throw_load_fig <- throw_load_fig %>%
          add_trace(y = rep(avg_throw, nrow(filtered_df())),
                    type = "scatter",
                    mode = "lines",
                    name = paste(input$selected_player,"Average Daily Throw Load"),
                    x = filtered_df()$date,  
                    hoverinfo = "text",
                    text = paste0("Avg Throw Load: ", round(avg_throw, 0))
          )
      
      return(throw_load_fig)
  
  })

  # Lower Body Load


    output$player_lower_load <- renderPlotly({
      
      throw_load_fig <- plot_ly(filtered_df(),
                                x = ~date,
                                y = ~lower_load_sum,
                                type = "scatter",
                                name = paste(input$selected_player,"Lower Load Load"),
                                mode = 'lines', text = paste0("Lower Load Load", ": ", round(filtered_df()$lower_load_sum, 0)),
                                hoverinfo = "text") %>%
        layout(
          title = "Lower Load Load",
          legend = list(orientation = "h",   
                        x = 0.5,            
                        y = -0.2,           
                        xanchor = "center",  
                        yanchor = "top"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Lower Load Load"),
          hovermode = "x unified"
        )              
        avg_throw <- mean(filtered_df_only_session()$lower_load_sum, na.rm = TRUE)
        throw_load_fig <- throw_load_fig %>%
          add_trace(y = rep(avg_throw, nrow(filtered_df())), # Replicate the mean for all dates
                    type = "scatter",
                    mode = "lines",
                    name = paste(input$selected_player,"Average Daily Lower Load Load"),
                    x = filtered_df()$date,  # Use existing dates
                    hoverinfo = "text", # Add hoverinfo
                    text = paste0("Avg Lower Load Load: ", round(avg_throw, 0)) # Custom hover text
          )
      
      return(throw_load_fig)
})
    
    
    
    # Sprinting

    
    output$player_sprint <- renderPlotly({
      
      throw_load_fig <- plot_ly(filtered_df(),
                                x = ~date,
                                y = ~sprint_total_yds,
                                type = "scatter",
                                name = paste(input$selected_player,"Sprint yards"),
                                mode = 'lines', text = paste0("Sprint yards", ": ", round(filtered_df()$lower_load_sum, 0)),
                                hoverinfo = "text") %>%
        layout(
          title = "Sprint yards",
          legend = list(orientation = "h",   
                        x = 0.5,            
                        y = -0.2,            
                        xanchor = "center",  
                        yanchor = "top"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Sprint Yards"),
          hovermode = "x unified"
        )              
        avg_throw <- mean(filtered_df_only_session()$sprint_total_yds, na.rm = TRUE)
        throw_load_fig <- throw_load_fig %>%
          add_trace(y = rep(avg_throw, nrow(filtered_df())),
                    type = "scatter",
                    mode = "lines",
                    name = paste(input$selected_player,"Average Sprint Yards"),
                    x = filtered_df()$date,  
                    hoverinfo = "text", 
                    text = paste0("Avg Sprint yards: ", round(avg_throw, 0))
          )
      
      return(throw_load_fig)
        })
  
    output$player_dml <- renderPlotly({
      
      throw_load_fig <- plot_ly(filtered_df(),
                                x = ~date,
                                y = ~dml_sum,
                                type = "scatter",
                                name = paste(input$selected_player,"DML"),
                                mode = 'lines', text = paste0("DML", ": ", round(filtered_df()$lower_load_sum, 0)),
                                hoverinfo = "text") %>%
        layout(
          title = "Dynamic Movement Load",
          legend = list(orientation = "h",   
                        x = 0.5,             
                        y = -0.2,            
                        xanchor = "center",  
                        yanchor = "top"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Dynamic Movement Load"),
          hovermode = "x unified"
        )              
        avg_throw <- mean(filtered_df_only_session()$sprint_total_yds, na.rm = TRUE)
        throw_load_fig <- throw_load_fig %>%
          add_trace(y = rep(avg_throw, nrow(filtered_df())), # Replicate the mean for all dates
                    type = "scatter",
                    mode = "lines",
                    name = paste(input$selected_player,"Average Dynamic Movement Load"),
                    x = filtered_df()$date,  
                    hoverinfo = "text", 
                    text = paste0("Avg DML: ", round(avg_throw, 0)) 
          )
  
      return(throw_load_fig)
      })
    
    
    # Player Flagging
    
    flagging_df <- reactive({
      print("FLAG FLAG ")
      flagging_df<- rolling_df()%>%
        mutate(
          throw_category = case_when(
            throw_total > smooth_rolling_throw_upper ~ "High",
            throw_total >= smooth_rolling_throw_lower ~ "Medium",
            TRUE ~ "Low"
          ),
          swing_category = case_when(
            swing_total > smooth_rolling_swing_upper ~ "High",
            swing_total >= smooth_rolling_swing_lower ~ "Medium",
            TRUE ~ "Low"
          ),
          ll_category = case_when(
            lower_load_sum  > smooth_rolling_ll_upper  ~ "High",
            lower_load_sum  >= smooth_rolling_ll_lower  ~ "Medium",
            TRUE ~ "Low"
          ),
          dml_category = case_when(
            dml_sum > smooth_rolling_dml_upper ~ "High",
            dml_sum >= smooth_rolling_dml_lower ~ "Medium",
            TRUE ~ "Low"
          ),
          sprint_category = case_when(
            sprint_total_yds > smooth_rolling_sprint_upper ~ "High",
            sprint_total_yds >= smooth_rolling_sprint_lower ~ "Medium",
            TRUE ~ "Low"
          ),
          Catcher_category = case_when(
            throw_count_sum > smooth_rolling_tc_upper ~ "High",
            throw_count_sum >= smooth_rolling_tc_lower ~ "Medium",
            TRUE ~ "Low"
          )
        ) %>%
        select(date, athlete_name, throw_category, swing_category, ll_category, dml_category, sprint_category, Catcher_category )
    })
    
    
    
    output$load_flag <- renderDT({
      datatable(
        flagging_df() %>% filter(date == input$input_date_flag) %>%
          select(date, athlete_name, ll_category, sprint_category, throw_category, swing_category, dml_category, Catcher_category) %>%
          rename(`Date` = date,
                 `Athlete Name` = athlete_name,
                 `Lower Body Load Flag` = ll_category,
                 `Sprint Yard Flag` = sprint_category,
                 `Throw Load Flag` = throw_category,
                 `Swing Load Flag` = swing_category,
                 `DML Flag` = dml_category,
                 `Catcher Load Flag` = Catcher_category
                 ),
                options = list(pageLength = 20)
        )%>%
      formatStyle(
        c("Lower Body Load Flag", "Sprint Yard Flag", "Throw Load Flag", "Swing Load Flag", "DML Flag", "Catcher Load Flag"),  # Column name to format
        backgroundColor = styleEqual(
          c('Low', 'Medium', 'High'),  
          c('green', 'yellow', 'red')  
        ),
        color = styleEqual(
          c('Low', 'Medium', 'High'),
          c('white', 'black', 'white')
        )
      )
      })
   
    
    ##### Individual Day Reports
    filtered_daily_report_pos <- reactive({
      data.frame(df()) %>%
        filter(date == input$input_date_day_summary, position_name != "Pitcher") %>%
        group_by(period_name, start_time) %>%
        summarise(
          avg_sprint = mean(softball_sprint, na.rm = TRUE),
          avg_lower_load = mean(weighted_dist, na.rm = TRUE),
          avg_dynamic_movement_load = mean(dynamic_movement_load, na.rm = TRUE),
          avg_throw_load = mean(baseball_throw_maxpl, na.rm = TRUE),
          avg_swing_load = mean(baseball_swing_maxpl, na.rm = TRUE),
          avg_swing_intensity = mean(swing_load_per_swing, na.rm = TRUE),
          avg_throw_intensity = mean(throw_load_per_throw_scaled, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(start_time) %>%  
        mutate(
          period_instance = paste(period_name,"*_*", start_time), 
          period_instance = factor(period_instance, levels = period_instance)  
        )
    })
    
    filtered_daily_report_pos_player_summary <- reactive({
      data.frame(df() ) %>%
        filter(date == input$input_date_day_summary, position_name != "Pitcher") %>%
        group_by(athlete_name, date) %>%
        summarise(
          sum_sprint = sum(softball_sprint, na.rm = TRUE),
          sum_lower_load = sum(weighted_dist, na.rm = TRUE),
          sum_dynamic_movement_load = sum(dynamic_movement_load, na.rm = TRUE),
          sum_throw_load = sum(baseball_throw_maxpl, na.rm = TRUE),
          sum_swing_load = sum(baseball_swing_maxpl, na.rm = TRUE),
          swing_intensity = mean(swing_load_per_swing, na.rm = TRUE),
          throw_intensity = mean(throw_load_per_throw_scaled, na.rm = TRUE),
          .groups = "drop"
        ) 
    })
    
    
    output$Temp_table <- renderTable(filtered_daily_report_pos_player_summary())
    
    
    output$day_swing_load <- renderPlotly(
      plot_ly(filtered_daily_report_pos(),
              x = ~period_instance,
              y = ~avg_swing_load,
              type = "bar"
               ) %>%
        layout(
          title = "Team's Average Swing Load by Period",
          xaxis = list(title = "Drill",
            tickvals = unique(filtered_daily_report_pos()$period_instance), 
            ticktext = sub("\\*_\\*.*", "", unique(filtered_daily_report_pos()$period_instance)), 
            tickangle = -45
          ),
          yaxis = list(title = "Average Swing Load"),
          barmode = "group",
          showlegend = FALSE
        )
    )
      

    
    output$day_lower_load <-  renderPlotly(
      plot_ly(filtered_daily_report_pos(),
              x = ~period_instance,
              y = ~avg_lower_load,
              type = "bar"
      ) %>%
        layout(
          title = "Team's Average Lower Body Load by Period",
          xaxis = list(title = "Drill",
            tickvals = unique(filtered_daily_report_pos()$period_instance), 
            ticktext = sub("\\*_\\*.*", "", unique(filtered_daily_report_pos()$period_instance)),
            tickangle = -45
          ),
          yaxis = list(title = "Average Lower Body Load"),
          barmode = "group",
          showlegend = FALSE
        )
    )
    
    output$sprint_load <-  renderPlotly(
      plot_ly(filtered_daily_report_pos(),
              x = ~period_instance,
              y = ~avg_sprint,
              type = "bar"
      ) %>%
      layout(
        title = "Team's Average Sprint Yards by Period",
        xaxis = list(title = "Drill",
          tickvals = unique(filtered_daily_report_pos()$period_instance), 
          ticktext = sub("\\*_\\*.*", "", unique(filtered_daily_report_pos()$period_instance)), 
          tickangle = -45
        ),
        yaxis = list(title = "Average Sprint Yards"),
        barmode = "group",
        showlegend = FALSE
      )
    )

    output$day_throw_load <- renderPlotly(
      plot_ly(filtered_daily_report_pos(),
              x = ~period_instance,
              y = ~avg_throw_load,
              type = "bar"
      ) %>%
        layout(
          title = "Team's Average Throw Load by Period",
          xaxis = list(title = "Drill",
            tickvals = unique(filtered_daily_report_pos()$period_instance), 
            ticktext = sub("\\*_\\*.*", "", unique(filtered_daily_report_pos()$period_instance)),
            tickangle = -45
          ),
          yaxis = list(title = "Average Throw Load"),
          barmode = "group",
          showlegend = FALSE
        )
    )

    
    weekly_thresholds <- reactive({
      data.frame(df() ) %>%
        group_by(athlete_name, date) %>%
        summarise(
          sum_sprint = sum(softball_sprint, na.rm = TRUE),
          sum_lower_load = sum(weighted_dist, na.rm = TRUE),
          sum_dynamic_movement_load = sum(dynamic_movement_load, na.rm = TRUE),
          sum_throw_load = sum(baseball_throw_maxpl, na.rm = TRUE),
          sum_swing_load = sum(baseball_swing_maxpl, na.rm = TRUE),
          sum_swing_intensity = sum(swing_load_per_swing, na.rm = TRUE),
          sum_throw_intensity = sum(throw_load_per_throw_scaled, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        group_by(athlete_name) %>%
        summarise(
          threshold_sprint = quantile(sum_sprint, 0.75, na.rm = TRUE) + 1.5 * IQR(sum_sprint, na.rm = TRUE),
          threshold_lower_load = quantile(sum_lower_load, 0.75, na.rm = TRUE) + 1.5 * IQR(sum_lower_load, na.rm = TRUE),
          threshold_dynamic_movement_load = quantile(sum_dynamic_movement_load, 0.75, na.rm = TRUE) + 1.5 * IQR(sum_dynamic_movement_load, na.rm = TRUE),
          threshold_throw_load = quantile(sum_throw_load, 0.75, na.rm = TRUE) + 1.5 * IQR(sum_throw_load, na.rm = TRUE),
          threshold_swing_load = quantile(sum_swing_load, 0.75, na.rm = TRUE) + 1.5 * IQR(sum_swing_load, na.rm = TRUE),
          threshold_throw_intensity = quantile(sum_throw_intensity, 0.75, na.rm = TRUE) + 1.5 * IQR(sum_throw_intensity, na.rm = TRUE),
          .groups = "drop"
        )%>%
        rename(
          Athlete = athlete_name,
          `Lower Body Load` = threshold_lower_load,
          `Sprint Distance` = threshold_sprint,
          `Swing Load` = threshold_swing_load,
          `Throw Load` = threshold_throw_load,
          `Throw Intensity` = threshold_throw_intensity
        )
    })
    
    
    
    output$weekly_table_output <- renderDT({
      temp_df <- filtered_daily_report_pos_player_summary() %>%
        select(athlete_name,
               sum_lower_load,
               sum_sprint,
               sum_swing_load,
               sum_throw_load,
               throw_intensity) %>%
        mutate(across(where(is.numeric), round, 0)) %>%
        rename(
          Athlete = athlete_name,
          `Lower Body Load` = sum_lower_load,
          `Sprint Distance` = sum_sprint,
          `Swing Load` = sum_swing_load,
          `Throw Load` = sum_throw_load,
          `Throw Intensity` = throw_intensity
        )
 
       datatable(temp_df)   
    })
    
    output$download_player_report <- downloadHandler(
      filename = "Position_Report.pdf",
      content = function(file) {
        # req(pitchers_weekly_data())
        
        tempReport <- file.path(tempdir(), "Player_Position_Daily_Report.Rmd")
        file.copy("Reports/Player_Position_Daily_Report.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(filtered_daily_report_pos_player_summary = filtered_daily_report_pos_player_summary(),
                       filtered_daily_report_pos = filtered_daily_report_pos(),
                       activity_date = input$input_date_day_summary) 
        
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
    
    
  
    
        ###### Pitchers Plot

    
    
    season_start <- as.Date("2025-01-13")
    current_week_offset <- interval(season_start, Sys.Date()) %/% weeks(1)
    
    if (Sys.Date() < season_start) {
      current_week_offset <- -interval(Sys.Date(), season_start) %/% weeks(1)
    }
    
    updateSliderInput(session, "week_number", value = current_week_offset+1, max = current_week_offset+1)
    updateSliderInput(session, "week_number2", value = current_week_offset+1, max = current_week_offset+1)
    
    
    output$date_range <- renderPrint({
      selected_week <- input$week_number
      
      first_day_of_week <- season_start + weeks(selected_week - 1) 
      
      last_day_of_week <- first_day_of_week + days(6)
      
      paste0("Selected Date Range: ", first_day_of_week, " to ", last_day_of_week)
    })
    output$date_range2 <- renderPrint({
      selected_week <- input$week_number2
      
      first_day_of_week <- season_start + weeks(selected_week - 1) 
      
      last_day_of_week <- first_day_of_week + days(6)
      
      paste0("Selected Date Range: ", first_day_of_week, " to ", last_day_of_week)
    })
    
    pitchers_day_data <- reactive({
      
      selected_week <- input$week_number
      
      first_day_of_week <- season_start + weeks(selected_week - 1)
      last_day_of_week <- first_day_of_week + days(6)
      
      return_df <- df() %>% filter(date >= first_day_of_week, date <= last_day_of_week, grepl("Pitcher", position_name)) %>%
        group_by(athlete_name,day_name) %>%
        summarise(sum_dynamic_movement_load = sum(dynamic_movement_load, na.rm = TRUE))
      return(return_df)
    })
    
    
    output$pitcher_daily_dynamic_load <- renderPlot(
      ggplot(pitchers_day_data(), aes(x = factor(day_name, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = sum_dynamic_movement_load)) + 
        geom_bar(stat = "identity") +
        facet_wrap(~ athlete_name, ncol = 1, axes = "all_x") +   # Use facet_grid
        scale_x_discrete(drop = FALSE)
        # facet_wrap(~athlete_name,  ncol = 1)
    )
    
    
    pitchers_weekly_data <- reactive({
      selected_week <- input$week_number
      
      first_day_of_week <- season_start + weeks(selected_week - 1)
      
      df2 <- df()  %>%
        mutate(week_number = as.numeric(interval(season_start, date) %/% weeks(1)) + 1)
      
      filtered_df <- df2 %>%
        filter(week_number <= selected_week, grepl("Pitcher", position_name))
      
      result_df <- filtered_df %>%
        group_by(athlete_name, week_number) %>%
        summarise(sum_dynamic_movement_load = sum(dynamic_movement_load, na.rm = TRUE))
      
      return(result_df)
    })
    
    output$pitcher_table <- renderTable({
      pitchers_weekly_data()
    })
 
    
    output$Weekly_pitcher_load <- renderPlot(
      ggplot(pitchers_weekly_data(), aes(x = week_number, y = sum_dynamic_movement_load)) + 
        geom_bar(stat = "identity") + 
        facet_wrap(~ athlete_name, ncol = 1,axes = "all_x")
    )
    
    
    
    output$pitcher_weekly_plot <- renderUI({
      pitchers_temp <- df() %>% filter(grepl("Pitcher", position_name))
      pitchers <-   unique(pitchers_temp$athlete_name)
      
      plot_outputs <- lapply(pitchers, function(player) {
        card(
          height = 500,
          full_screen = FALSE,
          card_body(
        plotlyOutput(outputId = paste0("plot_", player, "_weekly"))
          ),
        id = paste0("card_", gsub(" ", "_", player), "_weekly"))
      })
 
      
      do.call(tagList, plot_outputs)

    })
    
    observe({
        pitchers_temp <- df() %>% filter(grepl("Pitcher", position_name, ignore.case = TRUE))
        pitchers <-   unique(pitchers_temp$athlete_name)

      
      for (player in pitchers) {
        local({
          player_name <- player  # Capture the current value of player
          output[[paste0("plot_", player_name, "_weekly")]] <- renderPlotly({
            # Sample plot (replace with actual data/graphing logic)
            plot_ly(
              data = pitchers_weekly_data() %>% filter(athlete_name == player_name),
              x = ~week_number,
              y = ~sum_dynamic_movement_load,
              type = 'bar',
              text = ~paste("Week", week_number, ":", round(sum_dynamic_movement_load,0)),  # Custom hover text
              hoverinfo = 'text',
              textposition = 'none'
            ) %>%
              layout(
                title = paste(player_name, "Weekly Load"),
                xaxis = list(title = "Week Number"),
                yaxis = list(title= "Dynamic Movement Load")
              )
            
       
          })
        })
      }
    })

    output$pitcher_daily_plot <- renderUI({
      pitchers_temp <- df() %>% filter(grepl("Pitch", position_name))
      pitchers <-   unique(pitchers_temp$athlete_name)
      
      plot_outputs <- lapply(pitchers, function(player) {
        card(
          height = 500,
          full_screen = FALSE,
          card_body(
            plotlyOutput(outputId = paste0("plot_", player, "_daily"))
          )
      )
      })

      do.call(tagList, plot_outputs)
    })
    
    observe({
      pitchers_temp <- df() %>% filter(grepl("Pitcher", position_name, ignore.case = TRUE))
      pitchers <-   unique(pitchers_temp$athlete_name)
      
      
      for (player in pitchers) {
        local({
          player_name <- player  
          output[[paste0("plot_", player_name, "_daily")]] <- renderPlotly({
         

            player_data <- pitchers_day_data() %>% filter(athlete_name == player_name)
            plot_ly(data = player_data,
                    x = ~factor(day_name, 
                                levels = c("Monday",
                                           "Tuesday",
                                           "Wednesday",
                                           "Thursday", 
                                           "Friday", 
                                           "Saturday", 
                                           "Sunday")),
                    y = ~sum_dynamic_movement_load,
                    type = 'bar',
                    text = ~paste( day_name, "Load:", round(sum_dynamic_movement_load,0)),  
                    hoverinfo = 'text',
                    textposition = 'none'
            ) %>%
              layout(
                title = paste(player_name, "Daily Load"),
                xaxis = list(title = "Day of week", type = "category"),
                yaxis = list(title= "Dynamic Movement Load")
              )
                
           
          })
        })
      }
    })
    
    
    
    
    
    # Pitcher Load PDF
    output$download_pitcher_report <- downloadHandler(
      filename = "Pitcher_report.pdf",
      content = function(file) {

        tempReport <- file.path(tempdir(), "PitcherReport.Rmd")
        file.copy("Reports/PitcherReport.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(pitcher_data = pitchers_weekly_data(), pitchers_day_data = pitchers_day_data(), week_number = input$week_number) # Pass the data
        
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
    
    
    
    

#    Acute:Chronic WorkLoad
    observeEvent(input$select_all, {
      updateSelectizeInput(session, "selected_player_ac", 
                           selected = unique(df()$athlete_name))
    })
    
    calculate_rolling_average <- function(df,days, athlete_col, date_col, stats_cols) {
      
      
      df <- df  %>%
        arrange(!!sym(athlete_col), !!sym(date_col)) %>% # Ensure data is ordered by athlete and date
        group_by(!!sym(athlete_col)) %>%
        mutate(across(all_of(stats_cols),
                      ~ rollapply(.x, width = days, FUN = mean, align = "right", fill = NA, partial = FALSE),
                      .names = paste0("{.col}_rolling_", days))) %>%
        ungroup()
      
      return(df)
    }
    
    ac_workload <- reactive({
      df2 <- df()  %>% select("athlete_name", "date","position_name", "dynamic_movement_load", "weighted_dist", "baseball_throw_maxpl", "softball_sprint","baseball_swing_maxpl") %>%
        group_by(athlete_name, date) %>%
        summarise(sum_dml = sum(dynamic_movement_load, na.rm = TRUE),
                  sum_wd = sum(weighted_dist, na.rm = TRUE),
                  sum_sprint = sum(softball_sprint, na.rm = TRUE),
                  sum_throw = sum(baseball_throw_maxpl, na.rm = TRUE),
                  sum_swing = sum(baseball_swing_maxpl, na.rm = TRUE)) %>%
        mutate(date = as.Date(date, format = "%d/%m/%Y"))
      
      
      complete_dates <- df2 %>%
        group_by(athlete_name) %>%
        summarise(min_date = min(date)-28, max_date = max(date)) %>%
        ungroup() %>%
        mutate(date_range = purrr::map2(min_date, max_date, seq, by = "day")) %>%
        unnest(date_range) %>%
        rename(date = date_range)
      
      athlete_data_complete <- complete_dates %>%
        left_join(df2, by = c("athlete_name", "date")) %>%
        mutate(sum_dml = replace_na(sum_dml, 0),
               sum_wd = replace_na(sum_wd, 0),
               sum_sprint = replace_na(sum_sprint, 0),
               sum_throw = replace_na(sum_throw, 0),
               sum_swing = replace_na(sum_swing, 0)) %>%
        select(athlete_name,date, sum_dml, sum_wd, sum_swing, sum_throw, sum_sprint)
      
      day28 <-calculate_rolling_average(athlete_data_complete, 28, "athlete_name", "date", c("sum_dml", "sum_wd", "sum_swing", "sum_throw", "sum_sprint"))
      day4 <-calculate_rolling_average(athlete_data_complete, 4, "athlete_name", "date",  c("sum_dml", "sum_wd", "sum_swing", "sum_throw", "sum_sprint"))
      
      ac_df <- left_join(day4, day28, by = c("athlete_name", "date")) %>%
        mutate(ac_dml = sum_dml_rolling_4/sum_dml_rolling_28,
               ac_lower_load = sum_wd_rolling_4/sum_wd_rolling_28,
               ac_throw = sum_throw_rolling_4/sum_throw_rolling_28,
               ac_swing = sum_swing_rolling_4/sum_swing_rolling_28,
               ac_sprint = sum_sprint_rolling_4/sum_sprint_rolling_28) %>%
        select(athlete_name, date, ac_dml, ac_lower_load, ac_throw, ac_swing, ac_sprint)%>%
        mutate(date = as.Date(date, format = "%d/%m/%Y"))
      return(ac_df)
    })
    
    ac_df_output <- reactive({
      ac_workload() %>%
        filter(date >= input$ac_date_range[1] & date <= input$ac_date_range[2]) %>%
        filter(athlete_name %in% input$selected_player_ac)
    })
    
    output$ac_table <- renderDT(ac_df_output() %>%
                                  select(date, athlete_name, ac_lower_load, ac_sprint, ac_throw, ac_swing, ac_dml)%>%
                                  rename(`Date` = date,
                                         `Athlete Name` = athlete_name,
                                         `A:C Lower  Ratio` = ac_lower_load,
                                         `A:C Sprint  Ratio` = ac_sprint,
                                         `A:C Throw  Ratio` = ac_throw,
                                         `A:C Swing  Ratio` = ac_swing,
                                         `A:C DML Ratio` = ac_dml)%>%
                                  mutate_if(is.numeric, round, 3)
                                  )
    
    
    output$ac_lower_load_plot <- renderPlotly({
      throw_load_fig <- plot_ly(ac_df_output(),
                                x = ~date,
                                y = ~ac_lower_load,
                                color = ~athlete_name,
                                type = "scatter",
                                mode = 'lines')  %>%
        layout(
          title = "Lower Load Load A:C Ratio",
          legend = list(orientation = "h",  
                        x = 0.5,            
                        y = -0.2,            
                        xanchor = "center",  
                        yanchor = "top"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "A:C Ratio"),
          hovermode = "x unified"
        )

      return(throw_load_fig)
    })
    
    output$ac_dml_load_plot <- renderPlotly({
      throw_load_fig <- plot_ly(ac_df_output(),
                                x = ~date,
                                y = ~ac_dml,
                                color = ~athlete_name,
                                type = "scatter",
                                mode = 'lines') %>%
        layout(
          title = "Dynamic Movement Load A:C Ratio",
          legend = list(orientation = "h",   
                        x = 0.5,            
                        y = -0.2,           
                        xanchor = "center",  
                        yanchor = "top"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "A:C Ratio"),
          hovermode = "x unified"
        )
      return(throw_load_fig)
    })
    output$ac_throw_load_plot <- renderPlotly({
      throw_load_fig <- plot_ly(ac_df_output(),
                                x = ~date,
                                y = ~ac_throw,
                                color = ~athlete_name,
                                type = "scatter",
                                mode = 'lines') %>%
        layout(
          title = "Throw Load A:C Ratio",
          legend = list(orientation = "h",   
                        x = 0.5,            
                        y = -0.2,            
                        xanchor = "center", 
                        yanchor = "top"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "A:C Ratio"),
          hovermode = "x unified"
        )
   
      return(throw_load_fig)
    })
    output$ac_swing_load_plot <- renderPlotly({
      throw_load_fig <- plot_ly(ac_df_output(),
                                x = ~date,
                                y = ~ac_swing,
                                color = ~athlete_name,
                                type = "scatter",
                                mode = 'lines') %>%
        layout(
          title = "Swing Load A:C Ratio",
          legend = list(orientation = "h",  
                        x = 0.5,            
                        y = -0.2,           
                        xanchor = "center", 
                        yanchor = "top"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "A:C Ratio"),
          hovermode = "x unified"
        )
      return(throw_load_fig)
    })
    output$ac_sprint_load_plot <- renderPlotly({
      throw_load_fig <- plot_ly(ac_df_output(),
                                x = ~date,
                                y = ~ac_sprint,
                                color = ~athlete_name,
                                type = "scatter",
                                mode = 'lines') %>%
        layout(
          title = "Sprint Load A:C Ratio",
          legend = list(orientation = "h",   
                        x = 0.5,             
                        y = -0.2,            
                        xanchor = "center", 
                        yanchor = "top"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "A:C Ratio"),
          hovermode = "x unified"
        )
      return(throw_load_fig)
    })
    
    
    
    
    
    
    
    
    ## Report Info
    
    
    #---- Positional Player
    
 

pitchers_report_df <- reactive({
  
  selected_week <- input$week_number2
  
  # Calculate the start date of the selected week
  first_day_of_week <- season_start + weeks(selected_week - 1) 
  
  # Calculate the last day of the selected week
  last_day_of_week <- first_day_of_week + days(6)
  
  return_df <- df() %>% filter(date >= first_day_of_week, date <= last_day_of_week, grepl("Pitcher", position_name)) %>%
    group_by(athlete_name, date) %>%
    summarise(
      sum_dml = sum(dynamic_movement_load, na.rm = TRUE),
      sum_lower = sum(weighted_dist, na.rm = TRUE)
    ) %>% filter(sum_dml >100) %>%
    group_by(date) %>%
    summarise(
      avg_dml = mean(sum_dml, na.rm = TRUE),
      avg_lower = mean(sum_lower, na.rm = TRUE) 
    ) %>% mutate_if(is.numeric, ~ round(.x, 0))
  print(return_df)
  return(return_df%>% mutate(date = as.character(date))%>%
           bind_rows(summarise(return_df, date = "Total", avg_dml = sum(avg_dml), avg_lower = sum(avg_lower)))
  )
})

players_report_df <- reactive({
  
  selected_week <- input$week_number2
  
  # Calculate the start date of the selected week
  first_day_of_week <- season_start + weeks(selected_week - 1)
  
  # Calculate the last day of the selected week
  last_day_of_week <- first_day_of_week + days(6)
  
  return_df <- df() %>% filter(date >= first_day_of_week, date <= last_day_of_week, !grepl("Pitcher", position_name)) %>%
    group_by(athlete_name, date) %>%
    summarise(
      sum_throw = sum(baseball_throw_maxpl, na.rm = TRUE),
      sum_swing = sum(baseball_swing_maxpl, na.rm = TRUE),
      sum_lower = sum(weighted_dist, na.rm = TRUE),
      sum_sprint = sum(softball_sprint, na.rm = TRUE)
    ) %>%
    group_by(date) %>%
    summarise(
      avg_throw = mean(sum_throw, na.rm = TRUE),
      avg_swing = mean(sum_swing, na.rm = TRUE),
      avg_lower = mean(sum_lower, na.rm = TRUE),
      avg_sprint = mean(sum_sprint, na.rm = TRUE)
    )%>% mutate_if(is.numeric, ~ round(.x, 0))
  return(return_df%>% mutate(date = as.character(date))%>%
           bind_rows(summarise(return_df, date = "Total", avg_throw = sum(avg_throw), avg_swing = sum(avg_swing), avg_lower = sum(avg_lower), avg_sprint = sum(avg_sprint)))
  )
})

output$pitcher_report_table <- renderDT(pitchers_report_df()   %>% rename(
  `Average Dynamic Movement Load` = avg_dml,
  `Average Lower Body Load` = avg_lower )
)
output$player_pos_report_table <- renderDT(players_report_df()%>%
                                             select(date, avg_lower, avg_sprint, avg_swing, avg_throw)%>%
                                             rename(
                                               `Date` = date,
                                               `Average Lower Body Load` = avg_lower,
                                               `Average Sprint Yards` = avg_sprint,
                                               `Average Swing Load` = avg_swing,
                                               `Average Throw Load` = avg_throw
                                             )

)

}




  
