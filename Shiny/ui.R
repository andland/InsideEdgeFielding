# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Fielding Plays over Average with Inside Edge Data (2012-2013)"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("pos", "Position",positions,"All"),
    
    selectInput("team", "Team",teams,"All"),
    
    sliderInput("innings", "Number of Innings Played", 0, max(full.dat$Innings), c(800,2856), step = 1,
                round = FALSE, format = "#,##0.#####", locale = "us",
                ticks = TRUE, animate = FALSE),
    
    radioButtons("type", "Rate or Count Statistic",
                 c("Plays Per Season" = "rate", "Total Plays" = "count")),
    
    p("My blog - ",
      a("Statistically Significant", href="http://andland.github.io")
    ),
    
    p("Follow me - ",
      a("@andland", href="http://twitter.com/andland")
    ),
    
    p("Data from ",
      a("Fangraphs", href="http://www.fangraphs.com/leaders.aspx?pos=all&stats=fld&lg=all&qual=0&type=3&season=2013&month=0&season1=2012&ind=0&team=0&rost=0&age=0&filter=&players=0")
    )
  ),
  
  # Show a plot 
  mainPanel(
    tabsetPanel(
      tabPanel('Leaderboard',
               dataTableOutput("leaderboard"))
    )
#     plotOutput("playsPlot"),
#     h3(HTML("<br>")),
#     tableOutput("view")
    #     plotOutput("namePlot"),
    #     h4(htmlOutput("notes"))
  )
))
