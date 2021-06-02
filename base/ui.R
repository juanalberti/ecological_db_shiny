library(shinydashboard)
library(RMySQL)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(cowplot)
library(forcats)

source("../utils.R")

shinyUI(dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    menu
  ),
  dashboardBody(fluidRow(uiOutput("dxe"),
                         uiOutput("todo"),
                         uiOutput("dxy"),
                         uiOutput("dxv"),
                         uiOutput("status"))
  )
))
