library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(dygraphs)
library(plotly)
library(openxlsx)
library(lubridate)

####################################################
## INITIAL SETUP AND FORMATTING                   ##
####################################################

# read in the movies sheet, then create a copy (to prevent having to reload it each time)
data <- read.xlsx("data/Media Stats Tracking.xlsx", sheet=3, startRow=1)
df <- data

# convert the urls to urls using HTML code (it will be escapes in the rendering of the table)
for (i in 1:nrow(df)) {
    if (df[i, "web_address"] != "Not Available") {
        
        # set the values in the row
        val = df[i, "web_address"]
        movie_title <- df[i, "title"]
        
        # convert the url to HTML
        df[i, "web_address"] <- paste0('<a href="', val,'" target="_blank">', movie_title,'</a>')
    }
}

# this is for the filter for the date break down visual
df_limited <- df %>% select('day', 'month', 'year')

# this is to filter the variables for the heatmap
df_limited_2 <- df %>% select('year', 'month', 'day', 'release_date', 'genre', 'runtime', 'rating', 
                              'imdb_score', 'personal_score')

####################################################
## R SHINY APP CODE                               ##
####################################################

# create the layout
ui <- fluidPage(
    
    # add a theme
    theme = shinytheme("lumen"),
    
    # set up the tabs
    navbarPage(title="Zach's Movie Habits",
        
        ####### The About Panel #######
        tabPanel("About the Project",
                 
            # Add the image
            img(src="film-reel.png", height=272, width=500),
            br(),
            br(),
    
            # add in all the text with html formating
            p("This project is an examination of the movie data that I have collected since 2013. The basic format 
            of the data is a record for each movie that I watched to completion during the year, with the features 
            as listed: year:"),
                tags$ul(tags$li(em("year"), ": The year that I watched the movie"),
                tags$li(em("month"), ": The month that I watched the movie"),
                tags$li(em("day"), ": The day that I watched the movie"),
                tags$li(em("title"), ": The title of the film"),
                tags$li(em("release_year"), ": The year of release for the film"),
                tags$li(em("personal_score"), ": My personal rating for the film on a 1-5 scale (Note: not all the movies have a rating)")),
                p("At the conclusion of each year, I compile the data into an Excel document with all of the data.
            Since the data is lacking some information about the movies, I created a web scraping tool in R to pull 
            the IMDB page for the movie using RSelenium (part 1), then I updated the movie table with extra features 
            using rvest (part 2), as listed:"),
                tags$ul(tags$li(em("director"), ": the director of the film"),
                tags$li(em("genre"), ": the genres of the film. This takes the first two listed genres (if there is only one then it 
                takes just that genre), and combines them into a hyphenated genre (ex: Crime-Drama)."),
                tags$li(em("runtime"), ": The runtime of the film in minutes"),
                tags$li(em("rating"), ": The rating of the film. Some movies were made prior to the creation of the MPAA, so they 
                will have different ratings than the standard ones"),
                tags$li(em("imdb_score"), ": The average score of the film taken from IMDB"),
                tags$li(em("web_address"), ": The IMDB URL for the movie")),
            p("Once the final table was constructed, it was used to generate the data exploration that is present in 
            this Shiny App. The purpose of the app is to look at my personal movie watching habits over the course of 
            the 6 years I have collected data. When the next year concludes, I will add the data for 2019. I will 
            hopefully be able to keep adding subsequent years to see if my habits around watching movies changes over 
            the course of time."),
            p("The R source code for the web-scraping components can be found on my ", a('GitHub', 
            href="https://github.com/ZachZs/PersonalMovieData", target="_blank"), "."),
            br(),
            br(),
            br()
        ),
        
        ####### GRAPHS #######
        tabPanel("Visualizations",
            
            # bar plot of the movies per release year
            sidebarLayout(
                
                # create a side panel with a years range
                sidebarPanel(width=3,
                    p("This panel allows you to alter the range of years for the graph to the right."),
                    sliderInput("all_movies_slider",
                        label = "Range of Years",
                        value = c(min(df$release_date), max(df$release_date)),
                        min = min(df$release_date),
                        max = max(df$release_date)
                    )
                ),
                
                # create the main panel of the histogram
                mainPanel(
                    plotlyOutput("all_movies")
                )
            ),
            
            br(),
            br(),
            
            # plot of day/month/year of movies with the count
            sidebarLayout(
                
                # create a side bar with the variable selector for day/month/year
                sidebarPanel(width=3,
                    p("This panel allows you to select which variable (Day, Month, Year) to use to create the plot"),
                    varSelectInput("var1",
                        "Time Breakdown Selection",
                        df_limited
                    )
                ),
                
                # create a main panel with the plot
                mainPanel(
                    plotlyOutput("time_plot")
                )
            ),
            
            br(),
            br(),
            
            # heatmap plot of selectable variables
            fluidRow(
                
                # create columns for the description and the two variable selection
                column(width=2,
                    p("This panel allows you to select the two variables to compare in the heatmap")
                ),
                column(width=3,
                    varSelectInput("heat_var1",
                        "Variable 1",
                        df_limited_2,
                        selected="year"
                    )
                ),
                column(width=3,
                    varSelectInput("heat_var2",
                        "Variable 2",
                        df_limited_2,
                        selected="month"
                    )
                )
            ),
            
            # create row for the heat map
            fluidRow(
                plotlyOutput("heatmap_plot")
            ),
            
            br(),
            br()
            
        ),
        
        ####### DATA TABLE #######
        tabPanel("Data Table",
            
            # create a row for the date range selection 
            fluidRow(column(width = 4),
                dateRangeInput("dateRange",
                    label="Time Frame of Movies Watched",
                    min="2013-01-01",
                    max="2020-01-01",
                    start="2013-01-01",
                    end=NULL,
                    format = "mm/dd/yy",
                    separator = " - "
                )
            ),
            
            # create a row for the plot
            fluidRow(
                plotlyOutput("dateGraph")
            ),
            
            # create a row for the table
            fluidRow(
                dataTableOutput("dataTable")
            )
        )
    )
)


# create the server function
server <- function(input, output) {

##### create an output for the movies per release year #####
    
    # this creates a subset of the years based on the input slider
    all_movies_reac <- reactive({
        
        # create a table of number of movies per release year
        df_dy <- df %>% 
            group_by(release_date, title) %>%
            group_by(release_date) %>% 
            count() %>%
            rename("count" = "n") %>%
            rename("release_date" = "release_date")
        
        # subset the data
        df_dy2 <- subset(df_dy, df_dy$release_date <= input$all_movies_slider[2] & df_dy$release_date >= input$all_movies_slider[1])
        
        return(df_dy2)
    })
    
    # this takes the subset, and creates the plotly graph
    output$all_movies <- renderPlotly({
        plot_ly(
            all_movies_reac(),
            x = ~release_date,
            y = ~count,
            type = "bar",
            hovertemplate = paste('Release Year: %{x}<extra></extra>',
                '<br>Count of Movies: %{y}'),
            marker = list(color = ~count
                          )
        ) %>%
        layout(title = "Movies By Release Year",
               xaxis = list(title = "Release Year"),
               yaxis = list(title = "Number of Movies"),
               plot_bgcolor='transparent',
               paper_bgcolor='transparent'
        )
    })
    
##### CREATE AN OUTPUT FOR THE TIME BREAKDOWN PLOT #####
    
    # this creates a subset of the time based on the selection
    movie_time <- reactive({

        # group the data by the selected variable
        df_time <- df %>% 
            group_by(!!input$var1) %>%
            count() %>% 
            rename("count" = "n")
        
            # get the name of the variable chosen
            names <- colnames(df_time)
            names <- names[1]
        
            # rename the variable chosen to time to be universal
            df_time <- df_time %>% 
                rename("time" = names)
        
        return(df_time)
    })
    
    # this is the output graph
    output$time_plot <- renderPlotly({
        plot_ly(
            movie_time(),
            x = ~time,
            y = ~count,
            type = "bar",
            hovertemplate = paste(input$var1, '%{x}<extra></extra>',
                                  '<br>Count of Movies: %{y}'),
            marker = list(color = ~count
            )
        ) %>% 
        layout(title = paste("Movies Watched by", input$var1),
            xaxis = list(title = paste("", input$var1), tickmode='linear'),
            yaxis = list(title = "Number of Movies"),
            plot_bgcolor='transparent',
            paper_bgcolor='transparent'
        )
    })
    
##### CREATE AN OUTPUT FOR THE HEATMAP #####

    # get the counts of the variables selected
    heat_reac <- reactive({

        # group the data by the selected variables
        df_heat <- df %>%
            group_by(!!input$heat_var1, !!input$heat_var2) %>%
            count() %>%
            rename("fill_count" = "n")

        # get the name of the variable chosen
        names <- colnames(df_heat)

        # rename the variable chosen to Var1 or Var2 to be universal
        df_heat <- df_heat %>%
            rename("Var1" = names[1], "Var2" = names[2])
        
        return(df_heat)
    })

    # this is the output graph
    output$heatmap_plot <- renderPlotly({
        plot_ly(
            heat_reac(),
            x = ~Var1,
            y = ~Var2,
            z = ~fill_count,
            type = "heatmap",
            colorbar = list(title = "Count"),
            colors = "Oranges", #YlGn
            hovertemplate = paste(input$heat_var1, '%{x}',
                                  '<br>', input$heat_var2, '%{y}<extra></extra>',
                                  '<br>Count of Movies: %{z}')
        ) %>% 
        layout(title = paste(input$heat_var1, " vs. ", input$heat_var2),
            xaxis = list(title = paste("", input$heat_var1)),
            yaxis = list(title = paste("", input$heat_var2)),
            plot_bgcolor='transparent',
            paper_bgcolor='transparent'
        )
    })

##### CREATE AN OUTPUT FOR THE DATE GRAPH #####
    
    # filter the df based on the date range
    date_graph <- reactive({
        
        # combine the month/day/year into one string, and make it the date format
        df$date_watched <- as.Date(paste(df$year, df$month, df$day, sep="-"), format = "%Y-%m-%d")
        
        # filter the data for the selected range
        df_range <- df %>%
            filter(date_watched >= input$dateRange[1],  date_watched <= input$dateRange[2]) %>% 
            group_by(date_watched) %>%
            count() %>% 
            rename("count" = "n")

        return(df_range)
    })

    # render the graph
    output$dateGraph <- renderPlotly({
        plot_ly(
            date_graph(),
            x = ~date_watched,
            y = ~count,
            type = "bar",
            hovertemplate = paste('%{x}',
                                  '<br>Movie Count:', '%{y}<extra></extra>'),
            marker = list(color = ~count
            )
        ) %>% 
        layout(title = "Number of Movies Watched",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Number of Movies"),
               plot_bgcolor='transparent',
               paper_bgcolor='transparent'
        )
        
    })
    
 ##### CREATE AN OUTPUT FOR THE DATE DATA TABLE #####
    
    # filter the df based on the date range
    date_range <- reactive({
        
        # combine the month/day/year into one string, and make it the date format
        df$date_watched <- as.Date(paste(df$year, df$month, df$day, sep="-"), format = "%Y-%m-%d")
        
        # filter the data for the selected range
        df_range <- df %>%
            filter(date_watched >= input$dateRange[1],  date_watched <= input$dateRange[2])
        
        #rename the cols
        colnames(df_range) <- c("Year", "Month", "Day", "Title", "Release Date", "Director", "Genre", "Runtime (min)",
                                "Rating", "IMDB Score", "Personal Score", "IMDB Link", "Date Watched")
        
        # reorder the columns and drop the day/month/year cols
        df_range <- subset(df_range, select=c(13, 4:12))
        
        #return the dd
        return(df_range)
    })
    
    # renderthe table, and don't excape the html code
    output$dataTable <- renderDataTable(
        date_range(),
        options = list(pageLength = 100,
                       lengthMenu = c(100, 250, 500, 1000)
        ), 
        escape=FALSE
    )

}

# Run
shinyApp(ui = ui, server = server)

