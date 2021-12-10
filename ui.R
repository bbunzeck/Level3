#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(shinydashboardPlus)
library(shinycssloaders)

options(spinner.type = 6)

# Define UI for application that draws a histogram
header <- dashboardHeader(
    title = "Tokens and their context words",
    titleWidth = 350,
    dropdownMenuOutput("tokendata")
)

sidebar <- dashboardSidebar(
    width = 150,
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("About", tabName = "description", icon = icon("info"))
    ),
    hr(),
    selectInput("lemma", "Lemma",
                dir(here::here("data", "wwmx"))),
    selectInput("medoid", "N of medoid",
                seq(8), selected = 1),
    strong("Noise cues", style ="margin-left:1em;"),
    materialSwitch("noise", label = "Ignore", status="success",
                   right = TRUE, value = TRUE),
    # strong("Tokens transparency", style ="margin-left:1em;"),
    # materialSwitch("alpha", label = "Scale",
    #                right = TRUE, value = TRUE, status = "primary"),
    hr(),
    h3("Define relevance", style="margin-left:1em;"),
    numericInput("minfreq", "Frequency",
                min = 0, value = 2, step = 1),
    sliderInput("recall", "Recall",
                min = 0, max = 1, value = 0.5, step = 0.1,
                ticks = FALSE),
    switchInput("operator", onLabel = "AND", offLabel = "OR",
                onStatus = "success", offStatus = "warning"),
    sliderInput("precision", "Precision",
                min = 0, max = 1, value = 0.5, step = 0.1,
                ticks = FALSE)
    )

tsne_panel <- tabPanel(
    "t-SNE",
    fluidRow(
      column(width = 6,
             box(title = "Tokens", width = NULL,
                 status = "primary", solidHeader = TRUE, collapsible = TRUE,
                 plotlyOutput("tokens")),
             box(title = "Contexts", width = NULL,
                 status = "primary", solidHeader = TRUE, collapsible = TRUE,
                 DT::dataTableOutput("contexts"))
             ),
      column(width = 6,
             box(title = "FOCs (type level vectors)", width = NULL,
                 status = "success", solidHeader = TRUE, collapsible = TRUE,
                 plotlyOutput("focs"), id = "focs_box"),
             box(title = "FOCs by recall and precision", width = NULL,
                 status = "success", solidHeader = TRUE, collapsible = TRUE,
                 plotlyOutput("cor"))
      )
    )
)

body <- dashboardBody(
  tags$style(HTML("
  .target {
  border : solid;
  border-color : black;
  border-width : 1px;
  border-radius : 5px;
  padding : 1px;
  }
                  ")),
  tabItems(
    tabItem(
      tabName = "dashboard",
      fluidRow(
        shinydashboard::box(
          h4(htmlOutput("medoid")), width = 10, background = "yellow"),
        shinydashboard::box(
          column(12, align = "center",
                 actionBttn("previous", "", icon=icon("arrow-alt-circle-left"), color = "warning", style="material-flat"),
                 actionBttn("nextmodel", "", icon=icon("arrow-alt-circle-right"), color = "warning", style="material-flat")
                 ),
          width = 2, status = "warning")
      ),
      fluidRow(
        tabBox(selected = "t-SNE",
               width = 12, height = "100%",
               tsne_panel,
               tabPanel("Heatmap",
                        withSpinner(plotlyOutput("heatmap_or", height = "100%"), color = "#00a65a", hide.ui = FALSE)),
               tabPanel("HDBSCAN structure",
                        fluidRow(
                          # box(title = "MP densities", status = "primary",
                          #     plotOutput("mp")),
                          box(title = "Eps values", status = "primary",
                              withSpinner(plotOutput("eps"))),
                          box(title = "Proportion of noise per model",
                              status = "danger",
                              withSpinner(plotOutput("bars"), color = "#dd4b39")))),
               tabPanel("Messages", fluidRow(
                 box(title = "Split relevance",
                     width = 12,
                     background = "olive",
                     htmlOutput("splitrelevance"))
               ))
        )
      )
    ),
    desc_panel
  )
  
)

dashboardPage(header, sidebar, body, skin = "green")