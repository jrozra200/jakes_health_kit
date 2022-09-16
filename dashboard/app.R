library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)

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
                   avg_kj)
        
        return(cycles)
    }
    
    daily_strain_plot <- renderPlot({
        cycles <- get_cycle_data()
        
        ggplot(cycles, aes(x = day_start)) +
            geom_bar(aes(y = day_strain), stat = "identity") + 
            geom_errorbar(aes(ymin = avg_strain, ymax = avg_strain))
    })
}

shinyApp(ui, server)