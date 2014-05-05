
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinyExt)
library(shinyIncubator)
library(gridBase)
library(plotrix)
library(rworldmap)
library(WriteXLS)
library(xlsx)
library(wordcloud)
library(googleVis)
library(ggplot2)
library(ggmap)
suppressPackageStartupMessages(library(googleVis))

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Email Clustering"),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(id ="tabs1", position = "left",
                tabPanel("Classification", plotOutput("spam") ), 
                tabPanel("Clustering - wss",plotOutput("wss")),
                tabPanel("Clustering - cluster sizes",plotOutput("cluster")),
                tabPanel("Data Mining - top authors",plotOutput("topauth")),
                tabPanel("Data Mining - top topics",plotOutput("toptopics")),
                tabPanel("Data Mining - Frequent words",plotOutput("freqterms"))
    )
)))
