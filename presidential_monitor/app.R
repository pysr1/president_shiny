# load the required packages
library(shiny)
require(shinydashboard)
library(tidyverse)
# library for getting wikipedia page view data
library(pageviews)
#dates
library(lubridate)
library(billboarder)

# define a function to get a subjects weekly pageviews

get_subjects = function(term){
    # call the function very similar to how shown in documentation
    # select the only the date and views column
    
    pageviews = article_pageviews(project = "en.wikipedia", article = term
                                  , start = as.Date('2018-09-01'), end = Sys.Date()
                                  , user_type = c("user"), platform = c("desktop", "mobile-web")) %>% select(date, views)
    #return a dataframe
    return(pageviews)
}

# define a function that calls get_subjects and aggregates the dataframe to weekly frequency
# this helps to smooth the series and seperate the signal from the noise

get_weekly_subjects = function(term){
    # call get_subjects with the term
    df = get_subjects(term)
    df = df %>% 
        # group by week
        group_by(week=floor_date(date, "weeks")) %>%
        # calcualte total traffic
        summarize(Weekly_Traffic =sum(views))
    # create a column with the same name as the term
    df[,term] = df$Weekly_Traffic
    #The last week will never be complete so don't want to include it since we are taking a sum
    n = length(df$Weekly_Traffic) - 1
    return(df %>% head(n) %>% select(-Weekly_Traffic))
}

get_daily_subjects = function(term){
    # call get_subjects with the term
    df = get_subjects(term)
    df = df %>% 
        # group by week
        group_by(date) %>%
        rename(Date = date) %>% 
        # calcualte total traffic
        summarize(Weekly_Traffic =sum(views))
    # create a column with the same name as the term
    df[,term] = df$Weekly_Traffic
    #The last week will never be complete so don't want to include it since we are taking a sum
    return(df  %>% select(-Weekly_Traffic))
}


candidates = c(
    "Cory Booker",
    "Pete Buttigieg",
    "Julian_Castro",
    #"John Delaney",
    "Tulsi Gabbard",
    "Kirsten Gillibrand",
    "Mike Gravel",
    "Kamala Harris",
    "John Hickenlooper",
    "Jay Inslee",
    "Amy Klobuchar",
    #"Wayne Messam",
    "Joe Biden",
    "Beto O'Rourke",
    #"Tim Ryan",
    "Bernie Sanders",
    "Eric Swalwell",
    "Elizabeth Warren",
    "Marianne Williamson",
    "Andrew Yang",
    "Donald Trump",
    "Bill Weld"
)

# the map function takes the list of subjects and call the get_weekly_subjects function on each element of the list
# the reduce function uses left joins to create a subjects

subjects = map(candidates, get_daily_subjects) %>% reduce(left_join, by = "Date")




#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Presidential Monitoring")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("All Candidates Time Series", tabName = "plot_line", icon = icon("dashboard")),
        menuItem("All Candidates Bar Chart", tabName = "plot_line2", icon = icon("dashboard")),
        menuItem("Visit my linkedin", icon = icon("send",lib='glyphicon'), 
                 href = "https://www.linkedin.com/in/bcrocke1/")
    )
)




server <- function(input, output) {
    output$plot <- renderPlot({
            subjects %>% gather(key, value, -Date) %>% 
            mutate(value = log10(value)) %>% 
            ggplot(aes(Date, value))+
            geom_smooth(method = "loess", se = T)+
            geom_line(alpha = 0.3)+
            labs( x = "Date", y= "Log10 Weekly Searchs")+
            facet_wrap(~key)+#, scales = "free_y")+
            theme_classic()
        
        
    })
    output$plot2 <- renderBillboarder({
        agg = subjects %>% gather(key, value, -Date) %>% group_by(key) %>% summarise(value = mean(value, na.rm = T)) %>% 
            arrange(desc(value)) %>% rename('Average Wikipedia Page Views' = value)
        billboarder() %>% 
            bb_barchart(data = agg, rotated = T) %>% 
            bb_legend(show = FALSE) %>% 
            bb_title("Average Daily Wikipedia Page Views")
        
    })
}

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "plot_line",
                plotOutput("plot", height = 700)),    
        
        
        tabItem(tabName = "plot_line2",
                billboarderOutput("plot2", height = 700))   
    )  
)  



ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)

shinyApp(ui, server)