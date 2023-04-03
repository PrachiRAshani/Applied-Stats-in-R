library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(stringr)
library(here)

netflix <- readRDS("netflix_data.Rds")
netflix$genre <- str_to_title(netflix$genre) 

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Netflix Originals", tabName = "netflix_originals", icon = icon("film")),
    menuItem("Data Exploration", icon = icon("search")),
    menuItem("About", icon = icon("address-card"))
  )
)

body <- dashboardBody(
  chooseSliderSkin("Flat"),
  tabItems(
    tabItem(tabName = "netflix_originals",
            fluidRow(
              br(),
              column(width = 12, box(
                title = HTML('<span class="fa-stack fa-lg" style="color:#FF0000">
                                        <i class="fa fa-square fa-stack-2x"></i>
                                        <i class="fa fa-mouse-pointer fa-inverse fa-stack-1x"></i>
                                        </span> <span style="font-weight:bold;font-size:20px">
                                          Please select a movie</span>'),
                width = 4,
                "Select a genre and movie, a comparison against movies selected in the boxplot area will then be shown!",
                br(),
                br(),
                selectInput("genre", "Genre", selected = "documentary", choices = sort(unique(netflix$genre))),
                uiOutput("filtered_movies"),
                

                ),
                box(
                  title = HTML('<span class="fa-stack fa-lg" style="color:#FF0000">
                                        <i class="fa fa-square fa-stack-2x"></i>
                                        <i class="fa fa-chart-bar fa-inverse fa-stack-1x"></i>
                                        </span> <span style="font-weight:bold;font-size:20px">
                                          Movie Ratings - IMDB Scores by Genre</span>'),
                  plotOutput("plotted_ratings", brush = "plot_brush"),
                  width = 8
              )
            )
          ),
          fluidRow(
            br(),
            column(width = 12, offset = 0, box(
                  title = HTML('<span class="fa-stack fa-lg" style="color:#FF0000">
                                        <i class="fa fa-square fa-stack-2x"></i>
                                        <i class="fa fa-info fa-inverse fa-stack-1x"></i>
                                        </span> <span style="font-weight:bold;font-size:20px">
                                          Choice comparison\n
                               [Runtime - 1:<30mins, 2: 30-60mins, 3: 1-2hours, 4: >2hours]</span>'),
                  width = 7,
                  hr(),
                  DT::dataTableOutput("database"),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }"),
                  hr(),
                  tags$style("#slider_rows {font-size:30px;}"),
                  setSliderColor("indianred",1),
                  tagList(
                    tags$style(type = 'text/css', '#big_slider .irs-grid-text {font-size: 12px}
                                .irs-grid-pol {display: none;}
                                .irs-min {display: none;}
                                .irs-max {display: none;}
                                .irs-single {font-size: 12px}'), 
                    div(id = 'big_slider',
                        sliderInput("slider_rows", "max Rows:", min = 1, max = 15, value = 8)
                    ) #div close
                  ) #taglist close
              ),
              box(
                title = HTML('<span class="fa-stack fa-lg" style="color:#FF0000">
                                        <i class="fa fa-square fa-stack-2x"></i>
                                        <i class="fa fa-chart-bar fa-inverse fa-stack-1x"></i>
                                        </span> <span style="font-weight:bold;font-size:20px">
                                          IMDB score by language</span>'),
                hr(),
                plotOutput("language_plot"),
                width = 5
              )
            ),
          )
    )
  ),
  tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '
  )))
)


dashboardPage(
  skin = "black",
  dashboardHeader(title = "ASAR: Group 3"),
  sidebar,
  body
)