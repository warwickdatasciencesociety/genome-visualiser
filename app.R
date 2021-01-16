options(repos = BiocManager::repositories())
library(tidyverse)
library(shiny)
library(Gviz)
library(GenomicRanges)
library(readr)
library(shinyalert)
library(shinyjs)
library(shinydashboard)
library(imager)
library(readxl)
disease <- read.csv(file = "diseaseworkbook1.csv")
diseasetb <- as_tibble(disease)
diseases <- diseasetb$Disease
snpdesc1 <- read_file('text/snpdesc1.txt')
snpdesc2 <- read_file('text/snpdesc2.txt')
cpgdesc <- read_file('text/cpgdesc.txt')
hiw1 <- read_file('text/hiw1.txt')
hiw2 <- read_file('text/hiw2.txt')
hiw3 <- read_file('text/hiw3.txt')

header <- dashboardHeader(title = 'Visualisation of CpG Islands and SnP sites for different diseases', titleWidth = 700)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("home")),
    menuItem("What is a SnP site?", tabName = "snp", icon = icon("dna")),
    menuItem("What is a CpG Island?", tabName = "cpg", icon = icon("dna")),
    menuItem("How It Works", tabName = 'hiw', icon = icon("question-circle"))
  ))

body <- dashboardBody(
  tabItems(
    tabItem( tabName = "Home",
             
             fluidRow(
               
               box(width = 12, shinyjs::hidden(p(id = "text1", "Processing...")), plotOutput('plot')),
               box(title = "Choose a Disease:", solidHeader = TRUE, status = "success",
                   selectInput("disease", "",
                               diseases
                   ),
                   actionButton('run', 'Run'),
                   useShinyalert(),
                   shinyjs::hidden(p(id = "text1", "Processing...")),
                   
               ),
               
               box(title = ("Description of the disease"), solidHeader = TRUE, status = "success",
                   shinyjs::hidden(p(id = "text1", "Processing...")),
                   textOutput('description'),
                   uiOutput('tab'))
             )
    ),
    tabItem(tabName = "snp",
            h2('What is a SnP site?'),
            br(),
            p(snpdesc1),
            HTML('<center><img src="snppic.png" width="600"></center>'),
            p(snpdesc2),
            br(),
            p("A. Wolf, R. Caselli, E. Reiman and J. Valla, Neurobiology of Aging, 2013, 34, 1007-1017. ")
    ),
    tabItem(tabName = "cpg",
            h2('What is a CpG Island?'),
            br(),
            p(cpgdesc),
            HTML('<center><img src="cpgislandpic.png" width="600"></center>'),
    ),
    
    tabItem(tabName = "hiw",
            h2("How does it work?"),
            br(),
            p(hiw1),
            HTML('<center><img src="hiwpic1.png" width="600"></center>'),
            p(hiw2),
            HTML('<center><img src="hiwpic2.png" width="600"></center>'),
            p(hiw3),
            HTML('<center><img src="hiwpic3.png" width="600"></center>'),
            
    )
            
  ))

ui <- dashboardPage(header, sidebar, body,
                    useShinyjs(), skin = "green")

server <- function(input, output) {plotReady <- reactiveValues(ok = FALSE)
shinyalert("Welcome!", "To start select a disease and press run.", type = "info")
observeEvent(input$run, {
  
  shinyjs::hide("run")
  shinyjs::show("text1")
  shinyjs::disable("disease")
  plotReady$ok <- FALSE 
})

chosendisease <- eventReactive(input$run, {input$disease}) 
observeEvent(input$cancel, {
  shinyjs::show('hide')
})
output$plot <- renderPlot({
  req(input$run != 0)
  chr <- diseasetb[[which(diseasetb$Disease == chosendisease()), 2]]
  gen <- 'hg19'
  from <- diseasetb[[which(diseasetb$Disease == chosendisease()), 3]]
  to <- diseasetb[[which(diseasetb$Disease == chosendisease()), 4]]

  row <- which(diseasetb$Disease == chosendisease())
  listdis <- readRDS(paste("Plots/",row, "plot.rds", sep="" ))
  plotTracks(listdis, 
             from = from, to = to, showTitle = TRUE)
  
  plotReady$ok <- TRUE
  
  if (plotReady$ok) {
    shinyjs::show("run")
    shinyjs::hide("text1")
    shinyjs::enable("disease")
  }
  
})
output$description <- renderText({
  req(input$run != 0)
  diseasetb[[which(diseasetb$Disease == chosendisease()), 5]]
})

output$tab <- renderUI({
  req(input$run != 0)
  url <- a("Click here for more information on the disease", 
           href= diseasetb[[which(diseasetb$Disease == chosendisease()), 6]])
  tagList(url)
})
}
shinyApp(ui, server)


#Used to create rds files with disease data (takes approx 3hr30)
#for(i in 1:length(diseasetb[1])){
#  chr <- diseasetb[[i, 2]]
#  gen <- 'hg19'
#  from <- diseasetb[[i, 3]]
#  to <- diseasetb[[i, 4]]
#  axTrack <- GenomeAxisTrack()
#  idxTrack <- IdeogramTrack(genome=gen, chromosome=chr, fontsize = 15, fontcolor = "black")
#  
#  snpLocations <-  UcscTrack(genome = gen, chromosome = chr, 
#                             track = "snp151Common", from = from, to = to,
#                             trackType = "AnnotationTrack", 
#                             start = "chromStart", end = "chromEnd", 
#                             id = "name", feature = "func", 
#                             strand = "strand", shape = "box", 
#                             stacking = "dense", fill = "black",
#                             name = "SNPs", background.title = "blue", fontsize = 20, background.panel = "#B9CAFF")
#  
#  cpgIslands <- UcscTrack(genome = gen, chromosome = chr, 
#                          track = "cpgIslandExt", from = from, to = to,
#                          trackType = "AnnotationTrack", 
#                          start = "chromStart", end = "chromEnd", 
#                          id = "name", shape = "box", fill = "#006400", 
#                          name = "CpG Islands", background.title = "red", fontsize = 20, background.panel = "#FFB9B9")
#  plot <- list(idxTrack, axTrack, snpLocations, cpgIslands)
#}
