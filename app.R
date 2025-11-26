# Load necessary libraries
library(shiny)
library(dplyr)
library(tidyverse)
library(highcharter) 
library(urbnmapr)

#Data Sets

#Timeline dataset
timeline_df <- read.csv("opium_timeline.csv") 

death_df_race = read_csv("opioid_death_demographics_NVP.csv") |>
  filter(Measure %in% 
           c('Drug Deaths - American Indian/Alaska Native',
             'Drug Deaths - Asian',
             'Drug Deaths - Black',
             'Drug Deaths - Hawaiian/Pacific Islander',
             'Drug Deaths - Hispanic',
             'Drug Deaths - Multiracial',
             'Drug Deaths - White')) |>
  mutate(Measure = as.factor(substr(Measure,15,100)))

df = read_csv("census_by_race_3.csv") |>
  mutate(Formatted_race = as.factor(Formatted_race))



#This is used for the Map
my_data = read.csv("Opiod_data.csv")


#This formats data for the map
prep_data = function(my_data) {
  
  df_clean = my_data |> 
    mutate(
      Location = str_to_title(trimws(Location)),
      Location = str_replace(Location,
                             "^Dist\\.? Of Columbia$",
                             "District Of Columbia"),
      Value    = as.numeric(Data)
    ) |> 
    filter(
      !str_detect(Location, regex("^United States$", ignore_case = TRUE)),
      !is.na(Value)
    )
  
  # Mean per state-year
  state_year = df_clean |> 
    group_by(Location, TimeFrame) |> 
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
  
  years = sort(unique(state_year$TimeFrame))
  
  states_sf = get_urbn_map("states", sf = TRUE) |>
    mutate(Location = state_name)
  
  list(
    state_year = state_year,
    states_sf  = states_sf,
    years      = years,
    vmin       = floor(min(state_year$Value, na.rm = TRUE)),
    vmax       = ceiling(max(state_year$Value, na.rm = TRUE))
  )
}

data_objects = prep_data(my_data)

state_year = data_objects$state_year
states_sf  = data_objects$states_sf
all_years  = data_objects$years
vmin       = data_objects$vmin
vmax       = data_objects$vmax


label_safe_palette = c(
  "#e5f5f9",
  "#99d8c9",
  "#2ca25f",
  "#006d2c"
)

death_rate_pie_chart <- function(input_state) {
  death_df_race |>
    filter(State %in% c(input_state), Year == 2021) |>
    hchart(type = "pie", hcaes(x = Measure, y = Value, group = State )) |>
    hc_title(
      text = "",
      align = "center",
      style = list(color = 'darkblue', fontWeight = "bold", fontSize = "18px")) |>
    hc_tooltip(
      pointFormat = "{point.y} deaths per 100,000 <br/> ({point.percentage:.1f}%)",
      useHTML = TRUE,
      style = list(fontSize ="13px") ) |>
    hc_legend(enabled = FALSE) |> 
    hc_add_theme(hc_theme_flat()) |>
    hc_colors(c("#f1c40f", "#e67e22", "#e74b3c","#9b59b6", "#34495e","#2fcc71","#3498db")) |>
    hc_plotOptions(
      pie = list(
        size = 200,                # Force the pie to be exactly 200px diameter
        center = list("50%", "50%"), # Force the pie to be exactly in the middle
        dataLabels = list(
          enabled = TRUE,           # Turn them on
          distance = -30,           # Negative value moves text INSIDE the slice
          format = "{point.percentage:.0f}%", # Show just the number (e.g., "45%")
          style = list(
            fontSize = "12px", 
            textOutline = "none",   # Remove the white glow/stroke
            color = "white"         # White text is usually best on dark slices
          ),
          filter = list(
            property = "percentage",
            operator = ">",
            value = 5
          ) # Disable labels to prevent auto-resizing logic
        ) # Disable labels to prevent auto-resizing logic
      )
    ) |>
    hc_chart(
      marginTop = 50,     # Reserve exactly 50px for the title area
      marginBottom = 20,  # Reserve exactly 20px for the bottom
      spacingTop = 0,
      spacingBottom = 0
    )
}

population_pie_chart <- function(input_state, display_state)
{
  df |>
    filter(State %in% c(input_state)) |>
    group_by(Formatted_race, State, Color) |>
    summarise(total_population = sum(Population)) |>
    hchart(type = "pie", hcaes(x = Formatted_race, y = total_population, group = State,color = Color )) |>
    hc_title(
      text = display_state,
      align = "center",
      style = list(color = '#7f7f7f', fontWeight = "bold", fontSize = "18px")) |>
    hc_tooltip(
      pointFormat = "Total Population: {point.y} <br/>({point.percentage:.1f}%)",
      useHTML = TRUE,
      style = list(fontSize ="13px")) |>
    hc_legend(enabled = FALSE) |> 
    hc_add_theme(hc_theme_flat()) |>
    hc_colors(c("#f1c40f", "#2fcc71", "#e74b3c","#9b59b6", "#34495e","#2fcc71","#3498db")) |>
    hc_plotOptions(
      pie = list(
        size = 200,                # Force the pie to be exactly 200px diameter
        center = list("50%", "50%"), # Force the pie to be exactly in the middle
        dataLabels = list(
          enabled = TRUE,           # Turn them on
          distance = -30,           # Negative value moves text INSIDE the slice
          format = "{point.percentage:.0f}%", # Show just the number (e.g., "45%")
          style = list(
            fontSize = "12px", 
            textOutline = "none",   # Remove the white glow/stroke
            color = "white"         # White text is usually best on dark slices
          ),
          filter = list(
            property = "percentage",
            operator = ">",
            value = 5
          ) # Disable labels to prevent auto-resizing logic
        )
      )
      
    ) |>
    hc_chart(
      marginTop = 50,     # Reserve exactly 50px for the title area
      marginBottom = 20,  # Reserve exactly 20px for the bottom
      spacingTop = 0,
      spacingBottom = 0
    )
}


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            /* Styling for common elements */
            .well { background-color: #ecf0f1; border: none; box-shadow: 0 4px 6px rgba(0,0,0,0.1); border-radius: 8px;}
            h3 { color: #7F7F7F; font-weight: 600; } 

            /* Styling for action buttons */
            .btn-chart-select {
                margin-top: 5px;
                margin-bottom: 5px;
                width: 100%; 
                background-color: #4B8BBE;
                color: white;
                border: none;
                border-radius: 4px;
                padding: 10px 15px;
                font-weight: 500;
                transition: background-color 0.3s ease, transform 0.1s ease;
                text-align: left;
            }
            .btn-chart-select:hover {
                background-color: #4B8BBE;
                transform: translateY(-1px);
                box-shadow: 0 2px 5px rgba(0, 0, 0, 0.2);
                color: white; 
            }
            .btn-chart-select:focus, .btn-chart-select:active:focus, .btn-chart-select:active {
                background-color: #4B8BBE;
                color: white;
                outline: none;
                box-shadow: 0 0 0 0.25rem rgba(75,139,190,1.0); 
            }
            /* Styling for the sidebar panel */
            .well.sidebar-panel-content {
                padding: 15px;
                margin: 0;
            }
            /* Style to ensure chart content has top spacing */
            .shiny-tab-content > .active {
                padding-top: 20px; 
            }
              .member-info {
                 display: flex;
                 align-items: center;
                 margin-bottom: 15px;
                 padding: 10px;
                 border-bottom: 1px solid #eee;
             }
             .member-img {
                 /* --- UPDATED SIZE HERE --- */
                 width: 80px;
                 height: 80px;
                 border-radius: 50%;
                 margin-right: 15px;
                 object-fit: cover;
                 border: 2px solid #4B8BBE;
             }
             .pie-charts {
             background-color: #ecf0f1;
             }
             @keyframes fadeIn {
  0% {
    opacity: 0; /* Start completely transparent */
  }
  100% {
    opacity: 1; /* End fully opaque */
  }
}

.fade-in-arrow {
  /* Apply the animation */
  animation-name: fadeIn;
  /* Duration of the animation */
  animation-duration: 2s; 
  /* Delay before the animation starts (optional) */
  animation-delay: 0.5s; 
  /* Keep the final state (fully visible) */
  animation-fill-mode: forwards; 
  /* Initially hide the element until animation starts */
  opacity: 0; 
}

.tight-p-container p {
            margin-top: 1px;     
            margin-bottom: 1px;
            text-align: center;

        }
        "))
  ),
  
  # Apply the bslib theme directly to fluidPage for styling
  theme = bslib::bs_theme(bootswatch = "flatly", primary = "#7F7F7F"), 
  
  # --- Main Application Header ---
  div(
    h1(strong("Opioid Crisis in the US")),
    p("Analyzing death statistics and trends across the country."),
    img(
      src = "https://www.utsa.edu/_files/images/logos/ut-san-antonio.svg",
      style = "position: absolute; top: 9px; right: 30px; width: 300px;" 
    )
  ),
  hr(),
  
  # --- Main Sidebar Layout ---
  sidebarLayout(
    sidebarPanel(
      div(class = "well sidebar-panel-content", 
          h4(""),
          selectInput("main_view_selector", 
                      "Select a Section:",
                      choices = c(
                        "1. Introduction" = "intro",
                        "2. History" = "history",
                        "3. US Map" = "us_map", 
                        "4. Deaths Across the Country" = "deaths",
                        "5. Demographics" = "characteristics",
                        "6. Policies" = "policies"
                      ),
                      selected = "intro"
          ),
          hr(),
          
          conditionalPanel(
            condition = "input.main_view_selector == 'us_map'",
              sliderInput(
                inputId = "year",
                label   = "Year",
                min     = min(all_years),
                max     = max(all_years),
                value   = min(all_years),
                step    = 1,
                sep     = "",
                animate = animationOptions(interval = 800, loop = TRUE)
              ),
              helpText("Use the slider or Play button to animate through years.")
            ),
          
          
          conditionalPanel(
            # Condition checks the selected value of the main navigation input
            condition = "input.main_view_selector == 'deaths'",
            h4("Categories"),
            actionButton("top5_btn", 
                         "Top 5 States by Average Death Rate", 
                         class = "btn-chart-select"),
            actionButton("top5Line_btn", 
                         "Top 5 States by Death Rate Across Time", 
                         class = "btn-chart-select"),
            actionButton("bottom5_btn", 
                         "Bottom 5 States by Average Death Rate", 
                         class = "btn-chart-select"),
            actionButton("bottom5Line_btn", 
                         "Bottom 5 States by Death Rate Across Time", 
                         class = "btn-chart-select")
          ),
        
          conditionalPanel(
            condition = "input.main_view_selector == 'characteristics'",
            h4("Ranked States"),
            actionButton("top3char_btn", 
                         "Top 3 States by Race", 
                         class = "btn-chart-select"),
            actionButton("bottom3char_btn", 
                         "Bottom 3 States by Race", 
                         class = "btn-chart-select")
          ),
          
          conditionalPanel(
            condition = "input.main_view_selector == 'policies'",
            h4("Policy Filters")
          )
      )
    ),
    
    mainPanel(

      tabsetPanel(
        id = "main_content_tabs",
        type = "hidden", 
        tabPanel("intro",
                 h3("DA-6233-902-Fall 2025-Data Analytics - Group 1"),
                 
                 div(class = "member-info",
                     tags$img(
                       src = "472E972F-A591-4D3A-A064-EEE19AB678B2.jpeg",
                       class = "member-img",
                       alt = "Valerie Ceciliano"
                     ),
                     p("Valerie Ceciliano")
                     
                 ),
                 div(class = "member-info",
                     tags$img(
                       src = "Picture1.png",
                       class = "member-img",
                       alt = "Marisa Flores"
                     ),
                     p("Marisa Flores")
                 ),
                 
                 div(class = "member-info",
                     tags$img(
                       src = "IMG_20231116_125244021.jpg",
                       class = "member-img",
                       alt = "Mark Hertzfeld II"
                     ),
                     p("Mark Hertzfeld II")
                 ),
                 
                 div(class = "member-info",
                     tags$img(
                       src = "1763748509654~2.jpg",
                       class = "member-img",
                       alt = "Chris Serrano"
                     ),
                     p("Chris Serrano")
                 )
        ),
        tabPanel("history",
                 highchartOutput("timeline", height = "600px")
        ),
        
        tabPanel("us_map",
                 plotOutput("usMap", height = "600px")
                 
        ),
        
        tabPanel("deaths",
                 highchartOutput("stateChart", height = "600px")
                 
        ),
        
        tabPanel("characteristics",
                 class = "pie-charts",
                 fluidRow(
                   column(12, highchartOutput("sharedLegend", height = "100px"))
                 ),  
                 # --- Top Row (3 Charts) ---
                 
                 
                 hr(),
                 fluidRow(
                   
                   column(width = 4, highchartOutput("aPop", height = "300px")),
                   column(width = 4, highchartOutput("bPop", height = "300px")),
                   column(width = 4, highchartOutput("cPop", height = "300px"))
                 ),
                 fluidRow(
                   column(width = 4,style = "text-align: center;", tags$img(src = "dotted_arrow.png",style = "height: 75px;", class = "fade-in-arrow" )),
                   column(width = 4,style = "text-align: center;",tags$img(src = "dotted_arrow.png",style = "height: 75px;", class = "fade-in-arrow" )),
                   column(width = 4,style = "text-align: center;", tags$img(src = "dotted_arrow.png",style = "height: 75px;" , class = "fade-in-arrow" ))
                   ),
                 # --- Bottom Row (3 Charts) ---
                 fluidRow(
                   column(width = 4, highchartOutput("aDeath", height = "300px")),
                   column(width = 4, highchartOutput("bDeath", height = "300px")),
                   column(width = 4, highchartOutput("cDeath", height = "300px"))
                 ),
                 fluidRow(
                   width = 12,
                   tags$div(class = "tight-p-container",
                            p("Sources"),
                            p("U.S. Census Bureau population estimates for July 1, 2021"),
                            p("Americas Health Rankings - 2023 Annual Report")
                   )
                 )
        ),
        
        tabPanel("policies",
                 h3("Policies and Interventions"),
                 highchartOutput("deathmap", height = "400px")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  currentChartSelection <- reactiveVal("top5")
  currentPieChartSelection <- reactiveVal("top3char")
  
  observeEvent(input$main_view_selector, {
    updateTabsetPanel(
      session = session, 
      inputId = "main_content_tabs", 
      selected = input$main_view_selector 
    )
  })
  

  observeEvent(input$top5_btn, {
    currentChartSelection("top5")
  })
  observeEvent(input$top5Line_btn, {
    currentChartSelection("top5Line")
  })
  observeEvent(input$bottom5_btn, {
    currentChartSelection("bottom5")
  })
  observeEvent(input$bottom5Line_btn, {
    currentChartSelection("bottom5Line")
  })
  
  observeEvent(input$top3char_btn, {
    currentPieChartSelection("top3char")
  })
  observeEvent(input$bottom3char_btn, {
    currentPieChartSelection("bottom3char")
  })
  
  #this sections is for the navigation buttons for deaths across us
  output$chart_buttons <- renderUI({
    selection <- currentChartSelection()
    
    base_class <- "btn-chart-select"
    active_class <- paste(base_class, "btn-chart-select-active")
    
    top5_class <- if (selection == "top5") active_class else base_class
    top5Line_class <- if (selection == "top5Line") active_class else base_class
    bottom5_class <- if (selection == "bottom5") active_class else base_class
    bottom5Line_class <- if (selection == "bottom5Line") active_class else base_class
    
    div(
      actionButton("top5_btn", "top5bottom5", class = top5_class),
      actionButton("top5Line_btn", "top5bottom5", class = top5Line_class),
      actionButton("bottom5_btn", "top5bottom5", class = bottom5_class),
      actionButton("bottom5Line_btn", "top5bottom5", class = bottom5Line_class),
    )
  })
  
  output$chart_pie_buttons <- renderUI({
    selection <- currentPieChartSelection()
    
    base_class <- "btn-chart-select"
    active_class <- paste(base_class, "btn-chart-select-active")
    
    top3char_class <- if (selection == "top3char") active_class else base_class
    bottom3char_class <- if (selection == "bottom3char") active_class else base_class
    
  })
  
  output$timeline <- renderHighchart({

    #JS Functions for Timeline
    
    format_bce_labels <- JS("function() {
    var year = this.value;
    if (year < 0) {
        
        return (year * -1) + ' BCE';
    }
    return year
}")
    
    js_dim_other_labels <- JS("function() {
  var chart = this.series.chart;
  var currentPoint = this;
  chart.series[0].points.forEach(function(point) {
    if (point !== currentPoint && point.dataLabel) {
      point.dataLabel.attr({
        opacity: 0.2
      });
    }
  });
}")
    
    js_restore_labels <- JS("function() {
  var chart = this.series.chart;
  chart.series[0].points.forEach(function(point) {
    if (point.dataLabel) {
      point.dataLabel.attr({
        opacity: 1
      });
    }
  });
}")
    
  
    timeline_df |>
      mutate(Year = Date) |>
      hchart("timeline", hcaes(x = Year, name = Event)) |>
      hc_add_theme(hc_theme_flat()) |>
      hc_yAxis(
        visible = FALSE) |>
      hc_xAxis(
        type = "datetime", 
        labels = list(
          formatter = format_bce_labels
        )
      ) |>
      hc_title(text = "<b>The History of Opium</b>") |>
      hc_chart(
        zoomType = 'x'
      )  |>
      
      hc_plotOptions(
        series = list(
          animation = list(duration = 2000), 
          dataLabels = list(
            allowOverlap = TRUE,
            format = '<span style="color:{point.color}; font-weight:bold;">{point.name}</span>',
            style = list(textOutline = 'none', transition = 'opacity 0.3s ease-out') # Add transition for smooth effect
          ),
          point = list(
            events = list(
              mouseOver = js_dim_other_labels,
              mouseOut = js_restore_labels
            )
          )
        )
      )  |>
      hc_tooltip(
        formatter = JS("function() {
    var shouldShow = this.point.show_tooltip;

    if (shouldShow === 'N') {
      return false;
    }

    var htmlContent = 
      '<div style=\"width: 350px; padding: 5px;\">' +
        
        '<div style=\"float: left; margin-right: 10px;\">' +
          '<img src=\"' + this.point.Image + '\" width=\"120\" style=\"border-radius: 5px; box-shadow: 2px 2px 5px #aaa;\">' +
        '</div>' +
        '<div style=\"overflow: hidden;\">' +
          '<span style=\"font-size: 16px; font-weight: bold;\">' + this.point.Event + '</span><br/>' +
          '<span style=\"font-size: 12px; color: #555;\">' + this.point.Significance + '</span>' +
        '</div>' +
      '<div style=\"clear: both;\"></div>' +
      '</div><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/>';
    return htmlContent;
  }"),
        hideDelay = 10,
        enabled = TRUE,
        trigger = "click",
        headerFormat = "",
        useHTML = TRUE,
        style = list(pointerEvents = 'auto'))
  })
  
# Top 5 / Bottom 5 Section
  selected_chart = "top5"
  
  output$stateChart <- renderHighchart({
    
    selected_chart <- currentChartSelection()
    
     if (selected_chart == "top5") {
      
      df_trends = my_data |> 
        mutate(
          Location = str_to_title(trimws(Location)),
          Location = str_replace(Location, "^Dist\\. Of Columbia$", "District Of Columbia"),
          Value    = parse_number(Data)
        ) |> 
        filter(
          !str_detect(Location, regex("^United States$", ignore_case = TRUE)),
          !is.na(Value)
        )
      
      # Top 5 states by average death rate
      top5_ranked_data = df_trends |> 
        group_by(Location) |> 
        summarise(
          avg_death_rate = mean(Value, na.rm = TRUE),
          .groups = "drop"
        ) |> 
        arrange(desc(avg_death_rate)) |> 
        slice_head(n = 5)
      
      top5_states_ranked = top5_ranked_data$Location
      
      df_top5 = df_trends |> 
        filter(Location %in% top5_states_ranked)   # <-- fixed here
      
      series_data = top5_ranked_data |> 
        transmute(
          name  = Location,
          y     = avg_death_rate
        ) |> 
        list_parse2() 
      
      bar_chart = highchart() |> 
        hc_chart(type = "column") |> 
        hc_title(text = "Top 5 Locations by Average Opioid Death Rate") |> 
        hc_subtitle(text = "Average deaths per 100k (1999-2023)") |> 
        hc_xAxis(
          type = "category",
          title = list(text = "State"),
          labels = list(style = list(fontSize = "12px"))
        ) |> 
        hc_yAxis(
          min = 0,
          title = list(text = "Deaths per 100k"),
          gridLineDashStyle = "Dash"
        ) |> 
        hc_plotOptions(column = list(
          colorByPoint = TRUE,
          borderRadius = 5,
          pointPadding = 0.05,
          groupPadding = 0.08,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("function(){ return Highcharts.numberFormat(this.y, 1); }"),
            style = list(fontWeight = "bold")
          )
        )) |> 
        hc_add_series(
          name = "Avg Death Rate",
          data = series_data,
          showInLegend = FALSE
        ) |> 
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<span style='font-size:13px'><b>{point.key}</b></span><br/>",
          pointFormat = "Average deaths: <b>{point.y:.1f}</b> per 100k"
        ) |> 
        hc_exporting(enabled = TRUE) |> 
        hc_credits(enabled = FALSE) |> 
        hc_add_theme(hc_theme_flat())
      
      bar_chart
      
    
    } 
    else if (selected_chart == "bottom5") {
      df_trends = my_data |> 
        mutate(
          Location = str_to_title(trimws(Location)),
          Location = str_replace(Location, "^Dist\\. Of Columbia$", "District Of Columbia"),
          Value    = parse_number(Data)
        ) |> 
        filter(
          !str_detect(Location, regex("^United States$", ignore_case = TRUE)),
          !is.na(Value)
        )
      
      # Average death rate by state across all years
      location_avg = df_trends |> 
        group_by(Location) |> 
        summarise(
          avg_death_rate = mean(Value, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Bottom 5 states (lowest average death rate)
      bottom5_locations = location_avg |> 
        arrange(avg_death_rate) |> 
        slice(1:5)
      
      series_data_bar = bottom5_locations |> 
        transmute(
          name = Location,
          y    = avg_death_rate
        ) |> 
        list_parse2()
      
      bar_chart_bottom5 = highchart() |> 
        hc_chart(type = "column") |> 
        hc_title(text = "Bottom 5 Locations by Average Opioid Death Rate") |> 
        hc_subtitle(text = "Average deaths per 100k (1999-2023)") |> 
        hc_xAxis(
          type = "category",
          title = list(text = "State"),
          labels = list(style = list(fontSize = "12px"))
        ) |> 
        hc_yAxis(
          min = 0,
          title = list(text = "Deaths per 100k"),
          gridLineDashStyle = "Dash"
        ) |> 
        hc_plotOptions(column = list(
          colorByPoint = TRUE,     
          borderRadius = 5,
          pointPadding = 0.05,
          groupPadding = 0.08,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("function(){ return Highcharts.numberFormat(this.y, 1); }"),
            style = list(fontWeight = "bold")
          )
        )) |> 
        hc_add_series(
          name = "Avg Death Rate",
          data = series_data_bar,
          showInLegend = FALSE
        ) |> 
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<span style='font-size:13px'><b>{point.key}</b></span><br/>",
          pointFormat = "Average deaths: <b>{point.y:.1f}</b> per 100k"
        ) |> 
        hc_exporting(enabled = TRUE) |> 
        hc_credits(enabled = FALSE) |> 
        hc_add_theme(hc_theme_flat())
      
      bar_chart_bottom5
    }
    
    else if (selected_chart == "top5Line") {
      df_trends = my_data |> 
        mutate(
          Location = str_to_title(trimws(Location)),
          Location = str_replace(Location, "^Dist\\. Of Columbia$", "District Of Columbia"),
          Value    = parse_number(Data)   
        ) |>
        filter(
          !str_detect(Location, regex("^United States$", ignore_case = TRUE)),
          !is.na(Value)
        )
      
      
      top5_ranked_data = df_trends |>
        group_by(Location) |>
        summarise(
          avg_death_rate = mean(Value, na.rm = TRUE),
          .groups = "drop"
        ) |>
        arrange(desc(avg_death_rate)) |>
        slice_head(n = 5)
      
      top5_states = top5_ranked_data$Location
      
      
      df_top5 = df_trends |>
        filter(Location %in% top5_states)
      
      national_avg = df_trends |> 
        group_by(TimeFrame) |> 
        summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      
      
      y_min = floor(min(df_top5$Value, na.rm = TRUE))
      y_max = ceiling(max(df_top5$Value, na.rm = TRUE))
      
      # build chart
      line_chart = highchart() |> 
        hc_title(text = "Opioid Overdose Death Rates — Top 5 States (1999–2023)") |> 
        hc_subtitle(text = "Each line shows a state's trend; solid black = national average") |> 
        hc_xAxis(title = list(text = "Year"))|> 
        hc_yAxis(title = list(text = "Deaths per 100k"), min = y_min, max = y_max)
      
      top5_states_ranked = top5_ranked_data$Location   
      
      for (st in top5_states_ranked) {   
        st_data = df_top5 |> 
          filter(Location == st) |> 
          arrange(TimeFrame)
        
        last_year = max(st_data$TimeFrame, na.rm = TRUE)
        
        line_chart = line_chart |> 
          hc_add_series(
            data  = st_data,
            type  = "line",
            hcaes(x = TimeFrame, y = Value),
            name  = st,
            dataLabels = list(
              enabled = TRUE,
              formatter = JS(sprintf(
                "function(){ if(this.x === %d) return '%s'; return null; }",
                last_year, st
              )),
              align = "left", x = 5, y = 0
            )
          )
      }
      
      line_chart = line_chart |> 
        hc_add_series(
          data = national_avg,
          type = "line",
          hcaes(x = TimeFrame, y = Value),
          name = "National Avg",
          color = "black",
          lineWidth = 3,
          marker = list(enabled = FALSE)
        ) |> 
        hc_tooltip(
          shared = FALSE,      
          useHTML = TRUE,
          formatter = JS(
            "function() {
         return '<b>' + this.series.name + '</b><br/>' +
                'Year: ' + this.x + '<br/>' +
                'Rate: ' + Highcharts.numberFormat(this.y, 1) + ' per 100k';
       }"
          )
        ) |> 
        hc_legend(align = "center", verticalAlign = "bottom") |> 
        hc_exporting(enabled = TRUE) |> 
        hc_add_theme(hc_theme_flat())
      
      line_chart
    }
    
    else if (selected_chart == "bottom5Line")
    {
      df_trends = my_data |>
        mutate(
          Location = str_to_title(trimws(Location)),
          Location = str_replace(Location, "^Dist\\. Of Columbia$", "District Of Columbia"),
          Value    = parse_number(Data)   # numeric overdose rate
        ) |>
        filter(
          !str_detect(Location, regex("^United States$", ignore_case = TRUE)),
          !is.na(Value)
        )
      
      
      bottom5_ranked_data = df_trends |>
        group_by(Location) |>
        summarise(
          avg_death_rate = mean(Value, na.rm = TRUE),
          .groups = "drop"
        ) |>
        arrange(avg_death_rate) |>      # ascending → lowest first
        slice_head(n = 5)
      
      bottom5_states = bottom5_ranked_data$Location
      
      
      df_bottom5 = df_trends |>
        filter(Location %in% bottom5_states)
      
      
      national_avg = df_trends |>
        group_by(TimeFrame) |>
        summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
      
      
      y_min = floor(min(df_bottom5$Value, na.rm = TRUE))
      y_max = ceiling(max(df_bottom5$Value, na.rm = TRUE))
      
      line_chart_bottom5 = highchart() |>
        hc_title(text = "Opioid Overdose Death Rates — Bottom 5 States (1999–2023)") |>
        hc_subtitle(text = "Each line shows a state's trend; solid black = national average") |>
        hc_xAxis(title = list(text = "Year")) |>
        hc_yAxis(title = list(text = "Deaths per 100k"), min = y_min, max = y_max)
      
      bottom5_states_ranked = bottom5_ranked_data$Location   
      
      for (st in bottom5_states_ranked) {
        
        st_data = df_bottom5 |>
          filter(Location == st) |>
          arrange(TimeFrame)
        
        last_year = max(st_data$TimeFrame, na.rm = TRUE)
        
        line_chart_bottom5 = line_chart_bottom5 |>
          hc_add_series(
            data  = st_data,
            type  = "line",
            hcaes(x = TimeFrame, y = Value),
            name  = st,
            dataLabels = list(
              enabled = TRUE,
              formatter = JS(sprintf(
                "function(){ if(this.x === %d) return '%s'; return null; }",
                last_year, st
              )),
              align = "left", x = 5, y = 0
            )
          )
      }
      
      line_chart_bottom5 = line_chart_bottom5 |>
        hc_add_series(
          data = national_avg,
          type = "line",
          hcaes(x = TimeFrame, y = Value),
          name = "National Avg",
          color = "black",
          lineWidth = 3,
          marker = list(enabled = FALSE)
        ) |>
        hc_tooltip(
          shared   = FALSE,      
          useHTML  = TRUE,
          formatter = JS(
            "function() {
         return '<b>' + this.series.name + '</b><br/>' +
                'Year: ' + this.x + '<br/>' +
                'Rate: ' + Highcharts.numberFormat(this.y, 1) + ' per 100k';
       }"
          )
        ) |>
        hc_legend(align = "center", verticalAlign = "bottom") |>
        hc_exporting(enabled = TRUE) |>
        hc_add_theme(hc_theme_flat())
      
      line_chart_bottom5
      
    }
    else {
      # Fallback case 
      highchart() |> hc_title(text = "Select a chart view.")
    }
  })
  
#Map Section
  output$usMap = renderPlot({
    
    df_year = state_year |> 
      filter(TimeFrame == input$year)
    
    map_data_sf = states_sf |> 
      left_join(df_year, by = "Location")
    
    ggplot(map_data_sf) +
      geom_sf(aes(fill = Value), color = "white", linewidth = 0.3) +
      geom_sf_text(
        aes(label = ifelse(is.na(Value), "", sprintf("%.1f", Value))),
        size = 2.8,
        color = "black",          # readable everywhere
        fontface = "bold"
      ) +
      scale_fill_gradientn(
        colors = label_safe_palette,
        limits = c(vmin, vmax),
        name = "Deaths per 100k",
        na.value = "grey90"
      ) +
      labs(
        title    = "Opioid Overdose Death Rate by State",
        subtitle = paste("Year:", input$year),
        caption  = "Source: CDC WONDER",
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        legend.position = "right"
      )
  })


 
# this section calls the death rate pie function for each state
output$aDeath <- renderHighchart({
 
  selected_pie_chart <- currentPieChartSelection()
  
  if (selected_pie_chart == "top3char") {
    
    death_rate_pie_chart("WV")
    
  }
  else if (selected_pie_chart == "bottom3char") {
      
    death_rate_pie_chart("NE")
  }
})

output$bDeath <- renderHighchart({
  
  selected_pie_chart <- currentPieChartSelection()
  
  if (selected_pie_chart == "top3char") {
    
    death_rate_pie_chart("DC")
      
    
  }
  else if(selected_pie_chart == "bottom3char")
  {
    death_rate_pie_chart("SD")
  }
})
output$cDeath <- renderHighchart({
  
  selected_pie_chart <- currentPieChartSelection()
  
  if (selected_pie_chart == "top3char") {
    
    death_rate_pie_chart("MD")
      
    
  }
  else if (selected_pie_chart == "bottom3char") {
  
    death_rate_pie_chart("HI")
    }
  
})

# this section is for the reusable legend
output$sharedLegend <- renderHighchart({
  selected_pie_chart <- currentPieChartSelection()
  
    death_df_race |>
      filter(State %in% c('NE'), Year == 2021) |>
      mutate(Value = 1) |>
      hchart(type = "pie", hcaes(x = Measure, y = Value, group = State)) |>
      hc_tooltip(enabled = FALSE) |> # Better way to disable tooltip completely
      hc_add_theme(hc_theme_flat()) |>
      hc_colors(c("#f1c40f", "#e67e22", "#e74b3c","#9b59b6", "#34495e","#2fcc71","#3498db")) |>
      hc_plotOptions(
        pie = list(
          size = 0,                # Hide the pie slices
          showInLegend = TRUE,     # <--- CRITICAL: Force legend items to show
          dataLabels = list(enabled = FALSE),
          center = list("50%", "50%") # Center alignment (doesn't matter much if size is 0)
        )
      ) |>
      hc_legend(enabled = TRUE,
                verticalAlign = "top",  # Moves it to the top of the container
                align = "center",       # Centers it horizontally
                layout = "horizontal") |> # Ensure legend global switch is on
      hc_chart(height = 160) |>
      hc_title(text = "Racial Demographics by State",
               align = "center")|>
      hc_subtitle(text = "The charts on the top represent the total populatoin of the state for 2021. The charts on the bottom represent the death rate per 100,000 people for 2021")
  
})

# this section calls the population pie function for each state
output$aPop <- renderHighchart({
  
  selected_pie_chart <- currentPieChartSelection()
  
  if (selected_pie_chart == "top3char") {
    population_pie_chart("West Virginia","West Virginia")
  }
  
else if (selected_pie_chart == "bottom3char") {
  population_pie_chart("Nebraska","Nebraska")
}
})

output$bPop <- renderHighchart({
  
  selected_pie_chart <- currentPieChartSelection()
  
  if (selected_pie_chart == "top3char") {
    
    population_pie_chart('Washington D.C.',"Washington D.C.")
        
  }
  else if (selected_pie_chart == "bottom3char") {
    population_pie_chart("South Dakota","South Dakota")
  }
})

output$cPop <- renderHighchart({
  
  selected_pie_chart <- currentPieChartSelection()
  
  if (selected_pie_chart == "top3char") {
    
    population_pie_chart("Maryland","Maryland")
  }
  else if (selected_pie_chart == "bottom3char") {
    
    population_pie_chart("Hawaii","Hawaii")
  }
})

}

# Run the application
shinyApp(ui = ui, server = server)


