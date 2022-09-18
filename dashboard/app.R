library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# system("python ../scripts/whoop.py")
# system("Rscript --vanilla ../scripts/rules.R")

header <- dashboardHeader(
    title = "Jake's Health Kit"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Daily Strain", tabName = "daily_strain", 
                 icon = icon("chart-simple"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "daily_strain",
            h2("Daily Strain"),
            fluidRow(
                box(
                    infoBoxOutput("today"),
                    infoBoxOutput("last_10_day_average")
                )
            ),
            fluidRow(
                box(
                    title = "Daily Strain vs. Average",
                    plotOutput("daily_strain_plot")
                )
            )
        )
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { 
    get_cycle_data <- function() {
        cycles <- read_csv("../data/cycles.csv")
        
        day_start <- unlist(
            str_split(cycles$during, ",")
        )[grepl("\\[", unlist(str_split(cycles$during, ",")))]
        day_start <- str_remove_all(day_start, "\\[|\\'")
        day_start <- as_date(day_start, format = "%Y-%m-%dT%H:%M:%OSZ")
        
        cycles <- cycles %>% 
            mutate(day_start = day_start,
                   dotw = weekdays(day_start))
        day_start <- NULL
        
        cycles_sum <- cycles %>% 
            group_by(dotw) %>% 
            slice_head(n = 10) %>% 
            summarise(avg_strain = mean(day_strain),
                      avg_score = mean(scaled_strain),
                      avg_kj = mean(day_kilojoules))
        
        cycles <- cycles %>%
            head(10) %>% 
            left_join(cycles_sum, by = "dotw") %>% 
            select(day_start, 
                   day_strain,
                   scaled_strain,
                   day_kilojoules,
                   avg_strain,
                   avg_score,
                   avg_kj,
                   dotw) %>% 
            mutate(avg_strain = avg_strain * 1000,
                   day_strain = day_strain * 1000)
        
        return(cycles)
    }
    
    get_cycle_average <- function() {
        cycle_average <- read_csv("../data/cycles.csv") %>% 
            head(10) %>% 
            summarise(avg_strain = mean(day_strain) * 1000)
    }
    
    output$daily_strain_plot <- renderPlot({
        cycles <- get_cycle_data()
        
        ggplot(cycles, aes(x = day_start)) +
            geom_bar(aes(y = day_strain), stat = "identity") + 
            geom_errorbar(aes(ymin = avg_strain, ymax = avg_strain)) + 
            geom_text(aes(label = dotw, y = 1)) + 
            geom_text(aes(label = percent((day_strain - avg_strain) / avg_strain, 
                                          accuracy = 0.01), 
                          y = day_strain - 0.5)) +
            geom_text(aes(label = round(avg_strain, 2), y = avg_strain + 0.5)) + 
            geom_text(aes(label = round(day_strain, 2), y = day_strain + 0.5)) + 
            theme(panel.background = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  panel.grid.major.y = element_line(color = "light gray"))
    })
    
    output$last_10_day_average <- renderInfoBox({
        ten_day_avg <- get_cycle_average()
        infoBox(
            "Last 10 Day Average",
            round(ten_day_avg$avg_strain[1], 2),
            icon = icon("gauge")
        )
    })
    
    output$today <- renderInfoBox({
        today <- get_cycle_data() %>% 
            head(1)
        
        infoBox(
            "Today's Strain",
            round(today$day_strain[1], 2),
            icon = icon("calendar-day")
        )
    })
}

shinyApp(ui, server)