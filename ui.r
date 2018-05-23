library(shiny)
library(shinydashboard)
library(plotrix)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(skin = "blue",
                dashboardHeader(title = 'Large Numbers'),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Overview", tabName = "Overview", icon = icon("th")),
                    menuItem("Law of Large Numbers", icon = icon("bar-chart"), tabName = "largeNumber")
                  )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "Overview",
                            fluidRow(
                              column(3,
                                     img(src="psu.PNG", width = 70)),
                              column(9,
                                     h1("About:"))),
                            fluidPage(
                              h2("Law of Large Numbers"),
                              h3("This app is designed to examine the Law of Large Numbers for means and proportions.
                                 The Law of Large Numbers tells us that averages or proportions are likely 
                                 to be more stable when there are more trials while sums or counts are likely to be more variable.
                                 This does not happen by compensation for a bad run of luck since independent trials have no memory."),
                              h3 ("Students could learn the law of large number through picking the different sample size and different simulations. "),
                              h2("Instructions:"),
                              h3("1. Pick a population from one of the continuous types (left-skewed; right-skewed; symmetric; or bimodal) or one of the discrete examples (rolls of an astragalus; random songs from ipod shuffle; 
                                 or accident occurrence)."),
                              h3("2. Use the sliders to adjust the parameters of the population model you chose."),
                              h3("3. Use the sliders to decide how many times you will repeat the chance process you will run (i.e. #of trials or sample size) and how many sample paths you will observe 
                                 (i.e. # of repetitions of the whole simulation.)"),
                              h3("4. Observe whether the plots for the averages and sums converge or diverge from their expected values"),
                              h2("Acknowledgement:"),
                              h4("This app was originally developed and coded by Zibin Gao and Caihui Xiao. "),
                              h4("The app was modified and recoded by Yingjie(Chelsea) Wang in February 2018."), 
                              h4("Special thanks to Jingling Feng and team member Qichao Chen for help in the development of the app.")
                                 )),
                    
                    tabItem(tabName = "largeNumber",
                            tags$head(
                              tags$style(HTML("
                                              input[type=\"number\"] {
                                              width: 60px;
                                              }
                                              "))
                              ),
                            h4(
                              wellPanel(
                                fluidRow(
                                  column(3,
                                         selectInput("popDist", "Population Type",
                                                     list( "Left-skewed" = "leftskewed",
                                                           "Right-skewed" = "rightskewed",
                                                           "Symmetric" = "symmetric",
                                                           "Bimodal" = "bimodal",
                                                           "Astragalus"="astragalus",
                                                           "IPod Shuffle"="ipodshuffle",
                                                           "Accident Rate"= "poisson"
                                                     )
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.popDist=='leftskewed'",
                                           sliderInput("leftskew", " Skewness:",min = 1, max = 10, value = 1, step= 0.1)
                                         ),
                                         conditionalPanel(
                                           condition="input.popDist=='rightskewed'",
                                           sliderInput("rightskew", "Skewness:",min = 1, max = 10, value = 1, step= 0.1)
                                         ),
                                         conditionalPanel(
                                           condition="input.popDist=='symmetric'",
                                           sliderInput("inverse","Peakedness:", min = 0.5, max = 10, value = 1, step= 0.1)
                                         ),
                                         conditionalPanel(
                                           condition="input.popDist=='bimodal'",
                                           
                                           sliderInput("prop","% under right mode:",min = 0,max = 1,value = 0.2)
                                         ),
                                         conditionalPanel(
                                           condition="input.popDist=='poisson'",
                                           
                                           sliderInput("poissonmean","Mean:", min = 0.1, max = 10, value = 1, step= 0.1)
                                         ),
                                         
                                         #iPod shuffle 
                                         conditionalPanel(
                                           condition="input.popDist == 'ipodshuffle'",
                                           column(width = 7, offset = 0, 
                                                  radioButtons(
                                                    "ptype", "Category to follow:",
                                                    list("Jazz",
                                                      "Rock",
                                                      "Country",
                                                      "Hip-hop"),
                                                    selected ="Jazz"),
                                           
                                           column(4,offset = 0, 
                                                 
                                                  numericInput("s1", "Jazz", 1,
                                                               min = 0, max = 100, step = 1, width = '10%'),
                                                  numericInput("s2", "Rock", 1,
                                                               min = 0, max = 200, step = 1),
                                                  numericInput("s3", "Country", 1,
                                                               min = 0, max = 200, step = 1),
                                                  numericInput("s4", "Hip-hop", 1,
                                                               min = 0, max = 200, step = 1))
                                           ),
                                           column(width = 5,
                                                  helpText('P of Jazz'),
                                                  verbatimTextOutput("Jazz_percent"),
                                                  helpText('P of Rock '),
                                                  verbatimTextOutput("Rock_percent")),
                                           column(width = 5,
                                                  helpText('P of Country'),
                                                  verbatimTextOutput("Country_percent"),
                                                  helpText('P of Hip-hop'),
                                                  verbatimTextOutput("Hiphop_percent"))
                                         )
                                         
                                  ),
                                  column(3,
                                         #left skewed
                                         conditionalPanel(
                                           condition ="input.popDist == 'leftskewed'", 
                                           sliderInput("leftpath",
                                                       "# of paths",
                                                       min = 1,
                                                       max = 5,
                                                       value = 1),
                                           sliderInput("leftsize",
                                                       "sample size (n)",
                                                       min = 10,
                                                       max = 1000,
                                                       value = 100)
                                           
                                         ),
                                         conditionalPanel(
                                           condition = "input.popDist == 'rightskewed'", 
                                           
                                           # choose the number of sample means
                                           sliderInput("rightpath",
                                                       "# of paths",
                                                       min = 1,
                                                       max = 5,
                                                       value = 1),
                                           # choose the number of sample means
                                           sliderInput("rightsize",
                                                       "sample size (n)",
                                                       min = 10,
                                                       max = 1000,
                                                       value = 100)
                                         ),
                                         
                                         conditionalPanel(
                                           condition= "input.popDist == 'symmetric'",
                                           
                                           #choose the number of sample means
                                           sliderInput("sympath",
                                                       "# of paths",
                                                       min = 1,
                                                       max = 5,
                                                       value = 1),
                                           #choose the number of sample means
                                           sliderInput("symsize",
                                                       "sample size (n)",
                                                       min = 10,
                                                       max = 1000,
                                                       value = 100)
                                           
                                         ),
                                         conditionalPanel(
                                           condition= "input.popDist == 'astragalus'",
                                           
                                           sliderInput("aspath",
                                                       '# of paths', 
                                                       min = 1, 
                                                       max = 5, 
                                                       value = 1),
                                           sliderInput("assize",
                                                       "# of Trials",
                                                       min = 1,
                                                       max = 1000,
                                                       value = 100)
                                         ),
                                         conditionalPanel(
                                           condition= "input.popDist == 'bimodal'",
                                           
                                           #choose the number of sample means
                                           sliderInput("bipath",
                                                       "# of paths",
                                                       min = 1,
                                                       max = 5,
                                                       value = 1),
                                           #choose the number of sample means
                                           sliderInput("bisize",
                                                       " sample size (n)",
                                                       min = 10,
                                                       max = 1000,
                                                       value = 100)
                                           
                                         ),
                                         conditionalPanel(
                                           condition= "input.popDist == 'poisson'", 
                                           
                                           #choose the number of sample means
                                           sliderInput("poissonpath",
                                                       "# of paths",
                                                       min = 1,
                                                       max = 5,
                                                       value = 1),
                                           #choose the number of sample means
                                           sliderInput("poissonsize",
                                                       "sample size (n)",
                                                       min = 10,
                                                       max = 1000,
                                                       value = 100)
                                         ),
                                         conditionalPanel(
                                           condition= "input.popDist == 'ipodshuffle'",
                                           # sliderInput("draws", 
                                           #             label = "# of Songs in Playlist:", 
                                           #             value = 1,
                                           #             min = 1, max = 100, step = 1),
                                           sliderInput("ipodpath", label = "# of paths:",
                                                       min = 1, max = 5, value = 1, step = 1),
                                           sliderInput("ipodsize", label = "sample size (n)", 
                                                       min = 10, max = 1000, value = 100, step = 1)
                                         )
                                  ),
                                  column(6,
                                         conditionalPanel(
                                           condition ="input.popDist == 'leftskewed'", 
                                           plotOutput('plotleft1')),
                                         
                                         conditionalPanel(
                                           condition = "input.popDist == 'rightskewed'", 
                                           plotOutput('plotright1')),
                                         conditionalPanel(
                                           condition= "input.popDist == 'symmetric'",
                                           plotOutput('plotsymmetric1')),
                                         conditionalPanel(
                                           condition= "input.popDist == 'astragalus'",
                                           plotOutput("pop")),
                                         conditionalPanel(
                                           condition= "input.popDist == 'bimodal'",
                                           plotOutput('plotbiomodel1')),
                                         conditionalPanel(
                                           condition= "input.popDist == 'poisson'", 
                                           plotOutput('poissonpop')),
                                         conditionalPanel(
                                           condition="input.popDist == 'ipodshuffle'",
                                           conditionalPanel(
                                             condition="input.ptype== 'Jazz'",
                                             plotOutput("Plot1")),
                                           conditionalPanel(
                                             condition="input.ptype == 'Rock'",
                                             plotOutput('Plot2')),
                                           conditionalPanel(
                                             condition="input.ptype == 'Country'",
                                             plotOutput('Plot3')),
                                           conditionalPanel(
                                             condition="input.ptype == 'Hip-hop'",
                                             plotOutput('Plot4'))
                                           
                                         )
                                  )
                                )
                                
                              )
                            ),
                            br(),
                            fluidRow(
                              column(6,
                                     conditionalPanel(
                                       condition ="input.popDist == 'leftskewed'", 
                                       plotOutput('plotleft2')),
                                     conditionalPanel(
                                       condition = "input.popDist == 'rightskewed'", 
                                       plotOutput('plotright2')),
                                     conditionalPanel(
                                       condition= "input.popDist == 'symmetric'",
                                       plotOutput('plotsymmetric2')),
                                     conditionalPanel(
                                       condition= "input.popDist == 'astragalus'",
                                       plotOutput("line2")),
                                     conditionalPanel(
                                       condition= "input.popDist == 'bimodal'",
                                       plotOutput('plotbiomodel2')),
                                     conditionalPanel(
                                       condition= "input.popDist == 'poisson'", 
                                       plotOutput('plotpoisson1')),
                                     conditionalPanel(
                                       condition="input.popDist =='ipodshuffle'",
                                       conditionalPanel(
                                         condition="input.ptype== 'Jazz'",
                                         plotOutput("Plot01")),
                                       conditionalPanel(
                                         condition="input.ptype=='Rock' ",
                                         plotOutput("Plot02")),
                                       conditionalPanel(
                                         condition="input.ptype=='Country'",
                                         plotOutput("Plot03")),
                                       conditionalPanel(
                                         condition="input.ptype=='Hip-hop'",
                                         plotOutput("Plot04"))
                                     )
                              ),
                              column(6,
                                     conditionalPanel(
                                       condition ="input.popDist == 'leftskewed'", 
                                       plotOutput('plotleft3')),
                                     conditionalPanel(
                                       condition = "input.popDist == 'rightskewed'", 
                                       plotOutput('plotright3')),
                                     conditionalPanel(
                                       condition= "input.popDist == 'symmetric'",
                                       plotOutput('plotsymmetric3')),
                                     conditionalPanel(
                                       condition= "input.popDist == 'astragalus'",
                                       plotOutput("line1")),
                                     conditionalPanel(
                                       condition= "input.popDist == 'bimodal'",
                                       plotOutput('plotbiomodel3')),
                                     conditionalPanel(
                                       condition= "input.popDist == 'poisson'", 
                                       plotOutput('plotpoisson2')),
                                     conditionalPanel(
                                       condition="input.popDist =='ipodshuffle'",
                                       conditionalPanel(
                                         condition="input.ptype== 'Jazz'",
                                         plotOutput("Plot10")),
                                       conditionalPanel(
                                         condition="input.ptype=='Rock' ",
                                         plotOutput("Plot20")),
                                       conditionalPanel(
                                         condition="input.ptype=='Country'",
                                         plotOutput("Plot30")),
                                       conditionalPanel(
                                         condition="input.ptype=='Hip-hop'",
                                         plotOutput("Plot40"))))))))))
