#
# This is the application logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(markdown)
library(ggplot2)
library(dplyr)
library(haven)


#artnet <- readRDS("artnet4shiny.rda")
#artnetLong <- readRDS("ARTNet-long.rda")
#rawdata <- read_sav("ART-Net_Raw.sav")
#artnetLong4shiny <- readRDS("artnetlong4shiny.rda")

 artnet<-readRDS("C:/Users/ramya/Box Sync/ART-Net/Products/NetStats/NetStats/Shiny Output/artnet4shiny.rda")
 artnetLong4shiny<-readRDS("C:/Users/ramya/Box Sync/ART-Net/Products/NetStats/NetStats/Shiny Output/artnetlong4shiny.rda")
 artnetLong<-readRDS("C:/Users/ramya/Box Sync/ART-Net/Data/Cleaned/output/ARTNet-long.rda")
 rawdata<-read_sav("C:/Users/ramya/Box Sync/ART-Net/Data/Cleaned/input/ART-Net_Raw.sav")



rawdata$age.cat<-rep(NA, nrow(rawdata))
rawdata$age.cat[rawdata$AGE2 >= 15 & rawdata$AGE2 <=24] <- "15-24"
rawdata$age.cat[rawdata$AGE2 >= 25 & rawdata$AGE2 <= 34] <- "25-34"
rawdata$age.cat[rawdata$AGE2 >= 35 & rawdata$AGE2 <= 44] <- "35-44"
rawdata$age.cat[rawdata$AGE2 >= 45 & rawdata$AGE2 <= 54] <- "45-54"
rawdata$age.cat[rawdata$AGE2 >= 55 & rawdata$AGE2 <= 65] <- "55-65"



rawdata$city[rawdata$city==""]<-"NA"


artnetLong$age.cat<-rep(NA, nrow(artnetLong))
artnetLong$age.cat[artnetLong$age >= 15 & artnetLong$age <=24] <- "15-24"
artnetLong$age.cat[artnetLong$age >= 25 & artnetLong$age <= 34] <- "25-34"
artnetLong$age.cat[artnetLong$age >= 35 & artnetLong$age <= 44] <- "35-44"
artnetLong$age.cat[artnetLong$age >= 45 & artnetLong$age <= 54] <- "45-54"
artnetLong$age.cat[artnetLong$age >= 55 & artnetLong$age <= 65] <- "55-65"

artnet$age.cat<-rep(NA, nrow(artnet))
artnet$age.cat[artnet$age >= 15 & artnet$age <=24] <- "15-24"
artnet$age.cat[artnet$age >= 25 & artnet$age <= 34] <- "25-34"
artnet$age.cat[artnet$age >= 35 & artnet$age <= 44] <- "35-44"
artnet$age.cat[artnet$age >= 45 & artnet$age <= 54] <- "45-54"
artnet$age.cat[artnet$age >= 55 & artnet$age <= 65] <- "55-65"


#CITY
artnetLong$city2[artnetLong$city2 == "zOther1" |
                artnetLong$city2 == "zOther2" |  
                artnetLong$city2 == "zOther3" |
                artnetLong$city2 == "zOther4" |
                artnetLong$city2 == "zOther5" |
                artnetLong$city2 == "zOther6" |
                artnetLong$city2 == "zOther7" |
                artnetLong$city2 == "zOther8" |
                artnetLong$city2 == "zOther9" ] <- "Other"


#DIVISION LABELS
artnet$DIVCODE[artnet$DIVCODE==1]<-"New England"
artnet$DIVCODE[artnet$DIVCODE==2]<-"Middle Atlantic"
artnet$DIVCODE[artnet$DIVCODE==3]<-"East North Central"
artnet$DIVCODE[artnet$DIVCODE==4]<-"West North Central"
artnet$DIVCODE[artnet$DIVCODE==5]<-"South Atlantic"
artnet$DIVCODE[artnet$DIVCODE==6]<-"East South Central"
artnet$DIVCODE[artnet$DIVCODE==7]<-"West South Central"
artnet$DIVCODE[artnet$DIVCODE==8]<-"Mountain"
artnet$DIVCODE[artnet$DIVCODE==9]<-"Pacific"

artnetLong$DIVCODE[artnetLong$DIVCODE==1]<-"New England"
artnetLong$DIVCODE[artnetLong$DIVCODE==2]<-"Middle Atlantic"
artnetLong$DIVCODE[artnetLong$DIVCODE==3]<-"East North Central"
artnetLong$DIVCODE[artnetLong$DIVCODE==4]<-"West North Central"
artnetLong$DIVCODE[artnetLong$DIVCODE==5]<-"South Atlantic"
artnetLong$DIVCODE[artnetLong$DIVCODE==6]<-"East South Central"
artnetLong$DIVCODE[artnetLong$DIVCODE==7]<-"West South Central"
artnetLong$DIVCODE[artnetLong$DIVCODE==8]<-"Mountain"
artnetLong$DIVCODE[artnetLong$DIVCODE==9]<-"Pacific"

#URBAN RURAL CLASSIFICATION
artnet$NCHS_2013[artnet$NCHS_2013==1]<-"Large Central Metro"
artnet$NCHS_2013[artnet$NCHS_2013==2]<-"Large Fringe Metro"
artnet$NCHS_2013[artnet$NCHS_2013==3]<-"Medium Metro"
artnet$NCHS_2013[artnet$NCHS_2013==4]<-"Small Metro"
artnet$NCHS_2013[artnet$NCHS_2013==5]<-"Micropolitan Metro"
artnet$NCHS_2013[artnet$NCHS_2013==6]<-"Noncore"


artnetLong$NCHS_2013[artnetLong$NCHS_2013==1]<-"Large Central Metro"
artnetLong$NCHS_2013[artnetLong$NCHS_2013==2]<-"Large Fringe Metro"
artnetLong$NCHS_2013[artnetLong$NCHS_2013==3]<-"Medium Metro"
artnetLong$NCHS_2013[artnetLong$NCHS_2013==4]<-"Small Metro"
artnetLong$NCHS_2013[artnetLong$NCHS_2013==5]<-"Micropolitan Metro"
artnetLong$NCHS_2013[artnetLong$NCHS_2013==6]<-"Noncore"

#HIV STATUS CLASSIFICATION
artnet$hiv[artnet$hiv==0]<-"HIV Negative"
artnet$hiv[artnet$hiv==1]<-"HIV Positive"

artnetLong$hiv[artnetLong$hiv==0]<-"HIV Negative"
artnetLong$hiv[artnetLong$hiv==1]<-"HIV Positive"


#SURVEY STATUS CLASSIFICATION
rawdata$Vstatus[rawdata$Vstatus=="Disqualified"]<-"Disqualified"
rawdata$Vstatus[rawdata$Vstatus=="Complete"]<-"Complete"
rawdata$Vstatus[rawdata$Vstatus=="Partial"]<-"Partial"


# UI-------------------------------------------------
ui <- fluidPage(

  navbarPage("ART-Net",
             theme = shinytheme("flatly"),
             tabPanel("About",
                      column(1),
                      column(8,
                             includeMarkdown("intro.md")
                      )
             ),
             tabPanel("Descriptive (T1)",
                      sidebarLayout(
                        # Input(s)
                        sidebarPanel(
                          titlePanel(h3("Inputs", align="center")),
                          selectInput(inputId ="x1",
                                      label = "Stratifications",
                                      choices = c(
                                        "Age Category"="age.cat",
                                        "City"="city",
                                        "Education Level"= "education",
                                        "HIV Status"="hiv",
                                        "Race Category"= "race.cat",
                                        "Division"= "DIVCODE",
                                        "Urbanicity (County)"= "NCHS_2013"
                                      ),
                                      selected = "age.cat")
                        ),


                        mainPanel(
                          fluidRow(
                            h3("Summary Data Table", align="left"),
                            h4("ART-NET Survey Completion", align="left"),
                            column(4,
                                   tableOutput("table1"))
                          )
                        )
                      )
             ),


             navbarMenu("Partnership Degree (T2)",
                        tabPanel("Ongoing Partnerships",
                               sidebarLayout(
                                 # Input(s)
                                 sidebarPanel(
                                   titlePanel(h3("Inputs", align="center")),
                                   # Select Variables
                                   selectInput(inputId ="y2",
                                               label = "Variable",
                                               choices = c(
                                                 "Number of Ongoing AI or OI Sexual Partners"= "totdegree",
                                                 "Number of Ongoing AI Partners"= "allaionlydegree",
                                                 "Number of Ongoing OI Partners"="alloionlydegree"
                                               ),
                                               selected = "totdegree"),

                                   # Select Stratifications
                                   selectInput(inputId ="x2",
                                               label = "Stratifications",
                                               choices = c(
                                                 "Age Category"="age.cat",
                                                 "City"="city",
                                                 "HIV Status"="hiv",
                                                 "Race Category"= "race.cat",
                                                 "Division"="DIVCODE",
                                                 "Urbanicity (County)"= "NCHS_2013"
                                               ),
                                               selected = "age.cat")
                                 ),
                               
                                 mainPanel(
                                   fluidRow(
                                     h2("All (Main & Casual) Partnerships", align="center"),
                                     column(4,
                                            h4("Total Degree- Main & Casual Partnerships", align="center"),
                                            tableOutput("table2all")),
                                     column(8,
                                            h4("Density Plot- Main & Casual Partnerships", align="center"),
                                            plotOutput(outputId="densityplot2all" ))),
                                   br(),
                                   br(),
                                   fluidRow(
                                     h2("Main Partnerships", align="center"),
                                     column(4,
                                            h4("Total Degree- Main Partnerships", align="center"),
                                            tableOutput("table2main")),
                                     column(8,
                                            h4("Density Plot- Main Partnerships", align="center"),
                                            plotOutput(outputId="densityplot2main"))
                                   ),
                                   br(),
                                   br(),
                                   h2("Casual Partnerships", align="center"),
                                   fluidRow(
                                     column(4,
                                            h4("Total Degree- Casual Partnerships", align="center"),
                                            tableOutput("table2casual")),
                                     column(8,
                                            h4("Density Plot- Casual Partnerships", align="center"),
                                            plotOutput(outputId="densityplot2casual")))
                                   )
                                 )
                               ),

                        
                              tabPanel("One-Off Partnerships",
                               sidebarLayout(
                                 # Input(s)
                                 sidebarPanel(
                                   titlePanel(h3("Inputs", align="center")),
                                   # Select Variables
                                   selectInput(inputId ="y2b",
                                               label = "Variable",
                                               choices = c(
                                                 "Number of AI or OI Sexual Partners"= "rate.oo.aioi.part",
                                                 "Number of AI Partners"= "rate.oo.ai.part",
                                                 "Number of OI Partners"="rate.oo.oi.part"
                                               ),
                                               selected = "rate.oo.aioi.part"),

                                   # Select Stratifications
                                   selectInput(inputId ="x2b",
                                               label = "Stratifications",
                                               choices = c(
                                                 "Age Category"="age.cat",
                                                 "City"="city",
                                                 "HIV Status"="hiv",
                                                 "Race Category"= "race.cat",
                                                 "Division"="DIVCODE",
                                                 "Urbanicity (County)"= "NCHS_2013"
                                               ),
                                               selected = "age.cat")
                                   ),

                                 mainPanel(
                                   fluidRow(
                                     h2("One-Off Partnerships", align="center"),
                                     column(4,
                                            h4("Rate- One-Off Partnerships", align="center"),
                                            tableOutput("table2oneoff")),
                                     column(8,
                                            h4("Density Plot-One-Off Partnerships", align="center"),
                                            plotOutput(outputId="densityplot2oneoff" )))

                          )
                        )
                  )        
                
             ),

             tabPanel("Partnership Duration (T3)",
                      sidebarLayout(
                        # Input(s)
                        sidebarPanel(
                          titlePanel(h3("Inputs", align="center")),
                          #Select Variables
                          selectInput(inputId ="y3",
                                      label = "Variable",
                                      choices = c(
                                        "Duration"= "duration"
                                      ),
                                      selected = "duration"),
                          #Select Main or Casual
                          selectInput(inputId ="x3",
                                      label = "Stratifications",
                                      choices = c(
                                        "Age Category"="age.cat",
                                        "City"="city2",
                                        "HIV Status"="hiv",
                                        "Race Category"= "race.cat",
                                        "Division"="DIVCODE",
                                        "Urbanicity (County)"= "NCHS_2013"
                                      ),
                                      selected = "age.cat")

                        ),

                        mainPanel(
                          fluidRow(
                            h2("All (Main & Casual) Partnerships", align="center"),
                            column(5,
                                   h4("Duration in Weeks- Main & Casual Partnerships", align="center"),
                                   tableOutput("table3all")),
                            column(7,
                                   h4("Density Plot- Main & Casual Partnerships", align="center"),
                                   plotOutput(outputId="densityplot3all" ))
                          ),
                          br(),
                          br(),
                          fluidRow(
                            h2("Main Partnerships", align="center"),
                            column(5,
                                   h4("Duration in Weeks- Main Partnerships", align="center"),
                                   tableOutput("table3main")),
                            column(7,
                                   h4("Density Plot- Main Partnerships", align="center"),
                                   plotOutput(outputId="densityplot3main"))
                          ),
                          br(),
                          br(),
                          fluidRow(
                            h2("Casual Partnerships", align="center"),
                            column(5,
                                   h4("Duration in Weeks- Casual Partnerships", align="center"),
                                   tableOutput("table3casual")),
                            column(7,
                                   h4("Density Plot- Casual Partnerships", align="center"),
                                   plotOutput(outputId="densityplot3casual")))
                        
                      )
                  )
             ),



             tabPanel("Partnership Mixing (T4)",
                      sidebarLayout(
                        # Input(s)
                        sidebarPanel(
                          titlePanel(h3("Inputs", align="center")),
                          selectInput(inputId ="x4",
                                      label = "Stratifications",
                                      choices = c(
                                        "Race"="partraces",
                                        "HIV Status"="partstatuses",
                                        "Age"= "partages",
                                        "Age Difference"="edgeagediff",
                                        "Age Difference of Square Roots" = "sqrtedgeagediff"
                                      ),
                                      selected = "partraces")
                        ),

                        mainPanel(
                          fluidRow(
                            h2("All Partnerships", align="left"),
                            column(12,
                                   #h4("All Partnerships", align="left"),
                                   tableOutput("table4all"))
                          ),
                          br(),
                          fluidRow(
                            h2("Main Partnerships", align="left"),
                            column(12,
                                   #h4("Main Partnerships", align="left"),
                                   tableOutput("table4main"))
                          ),
                          br(),
                          fluidRow(
                            h2("Casual Partnerships", align="left"),
                            column(12,
                                   #h4("Casual Partnerships", align="left"),
                                   tableOutput("table4casual"))
                          ),
                          br(),
                          fluidRow(
                            h2("One-Off Partnerships", align="left"),
                            column(12,
                                   #h4("One-Off Partnerships", align="left"),
                                   tableOutput("table4oneoff"))

                          )
                        )
                      )
             )





  )
)


#SERVER----------------------------------------------------
shinyserver<-(function(input, output) {

#DATA INPUTS-----------------------------
  
  datasetInputx1 <- reactive({
    switch(input$x1,
           "Age Category"=artnet$age.cat,
           "city"=artnet$city,
           "Education Level"= artnet$education,
           "HIV Status"=artnet$hiv,
           "Race Category"= artnet$race.cat,
           "Division"= artnet$DIVCODE,
           "Urbanicity (County)"=artnet$NCHS_2013
    )
  })

  datasetInputx2<-reactive({
    switch(input$x2,
           "Age Category"=artnet$age.cat,
           "City"= artnet$city,
           "HIV Status"=artnet$hiv,
           "Race Category"= artnet$race.cat,
           "Division"=artnet$DIVCODE,
           "Urbanicity (County)"=artnet$NCHS_2013
    )
  })
  
  datasetInputx2b<-reactive({
    switch(input$x2b,
           "Age Category"=artnet$age.cat,
           "City"= artnet$city,
           "HIV Status"=artnet$hiv,
           "Race Category"= artnet$race.cat,
           "Division"=artnet$DIVCODE,
           "Urbanicity (County)"=artnet$NCHS_2013
    )
  })

  datasetInputy2<- reactive({
    switch(input$y2,
           "Number of Ongoing AI or OI Sexual Partners"= artnet$totdegree,
           "Number of Ongoing AI Partners"= artnet$allaionlydegree,
           "Number of Ongoing OI Partners"=artnet$alloionlydegree
    )
  })
  
  datasetInputy2b <-reactive({
    switch(input$y2b,
           "Number of AI or OI Sexual Partners"= artnet$rate.oo.aioi.part,
           "Number of AI Partners"= artnet$rate.oo.ai.part,
           "Number of OI Partners"= artnet$rate.oo.oi.part
    )  
    })

  
  datasetInputx3<-reactive({
    switch(input$x3,
           "Age Category"=artnetLong$age.cat,
           "City"= artnetLong$city2,
           "HIV Status"=artnetLong$hiv,
           "Race Category"= artnetLong$race.cat,
           "Division"=artnetLong$DIVCODE,
           "Urbanicity (County)"=artnetLong$NCHS_2013

    )
  })

  datasetInputy3<- reactive({
    switch(input$y3,
           "Duration"=artnetLong$duration
    )
  })

  datasetInputx4<-reactive({
    switch(input$x4,
           "Race"= artnetLong4shiny$partraces,
           "HIV Status"=artnetLong4shiny$partstatuses,
           "Age"= artnetLong4shiny$partages,
           "Age Difference"= artnetLong4shiny$edgeagediff,
           "Age Difference of Square Roots" = artnetLong4shiny$sqrtedgeagediff
    )
  })


#OUTPUT TABLES----------------------------------

#TABLE 1----------------------------------------
  
  #SUMMARY OUTPUT TABLE 1a
  #artnet= data.frame(artnet)
  
  table1a<-reactive(
    artnet %>%
      group_by_(input$x1) %>%
      summarise(n=n()
      ) %>%
      mutate(freq= n/sum(n)*100) %>%
      setNames(c(" ","n","%"))
  )

  #SUMMARY OUTPUT TABLE 1b
  table1b<-reactive(
    artnet %>%
      group_by(input$x1) %>%
      summarise(n=n()
      ) %>%
      mutate(freq= n/sum(n)*100) %>%
      setNames(c(" ","n","%"))
  )
  
  # OUTPUT TABLE TAB 1
  output$table1<-renderTable({rbind(table1a(),table1b())})


#TABLE 2---------------------------------------------

  #SUMMARY OUTPUT TABLE 2 ALL
  table2every<-reactive(
    artnet %>%
      group_by_(input$x2) %>%
      filter(maintotdegree==1 | castotdegree==1) %>%
      summarise_at(vars(input$y2),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )
  
  #OVERALL STATISTICS ALL
  table2everytot<-reactive(
    artnet %>%
      group_by(input$x2) %>%
      filter(maintotdegree==1 | castotdegree==1) %>%
      summarise_at(vars(input$y2),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  #SUMMARY OUTPUT TABLE 2 MAIN
  table2amain<-reactive(
    artnet %>%
      group_by_(input$x2) %>%
      filter(mainaionlydegree==1 | mainoionlydegree==1) %>%
      summarise_at(vars(input$y2),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  #OVERALL STATISTICS MAIN
  table2amaintot<-reactive(
    artnet %>%
      group_by(input$x2) %>%
      filter(mainaionlydegree==1 | mainoionlydegree==1) %>%
      summarise_at(vars(input$y2),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  #SUMMARY OUTPUT TABLE 2 CASUAL
  table2bcasual<-reactive(
    artnet %>%
      group_by_(input$x2) %>%
      filter(casaionlydegree==1 | casoionlydegree==1) %>%
      summarise_at(vars(input$y2),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  #OVERALL STATISTICS CASUAL
  table2bcasualtot<-reactive(
    artnet %>%
      group_by(input$x2) %>%
      filter(casaionlydegree==1 | casoionlydegree==1) %>%
      summarise_at(vars(input$y2),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  
  
#TABLE 2 ONE-OFF-----------------------------------------  
  
  #SUMMARY OUTPUT TABLE 2 ONEOFF
  table2coneoff<-reactive(
    artnet %>%
      group_by_(input$x2b) %>%
      summarise_at(vars(input$y2b),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  #OVERALL STATISTICS ONE OFF TOTAL
  table2coneofftot<-reactive(
    artnet %>%
      group_by(input$x2b) %>%
      filter() %>%
      summarise_at(vars(input$y2b),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )
  
  
  #Bind Table Rows Tab 2
  output$table2all<-renderTable({rbind(table2every(),table2everytot())})
  output$table2main<-renderTable({rbind(table2amain(),table2amaintot())})
  output$table2casual<-renderTable({rbind(table2bcasual(),table2bcasualtot())})
  output$table2oneoff<-renderTable({rbind(table2coneoff(),table2coneofftot())})


#TABLE 3----------------------------------------------

  #SUMARY OUTPUT TABLE 3-ALL
  table3alls<-reactive(
    artnetLong %>%
      group_by_(input$x3) %>%
      filter((p_ONGOING == 1 & duration < 2150) & (ptype==1 | ptype==2) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
      summarise_at(vars(input$y3),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  #OVERALL STATISTICS
  table3alltot<-reactive(
    artnetLong %>%
      group_by(input$x3) %>%
      filter((p_ONGOING == 1 & duration < 2150) & (ptype==1 | ptype==2) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
      summarise_at(vars(input$y3),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )



  #SUMMARY OUTPUT TABLE 3a-MAIN
  table3a<-reactive(
    artnetLong %>%
      group_by_(input$x3) %>%
      filter((p_ONGOING == 1 & duration < 2150) & (ptype==1) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
      summarise_at(vars(input$y3),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  #OVERALL STATISTICS
  table3atot<-reactive(
    artnetLong %>%
      group_by(input$x3) %>%
      filter((p_ONGOING == 1 & duration < 2150) & (ptype==1) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
      summarise_at(vars(input$y3),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  #SUMMARY OUTPUT TABLE 3b-CASUAL
  table3b<-reactive(
    artnetLong %>%
      group_by_(input$x3) %>%
      filter((p_ONGOING == 1 & duration < 2150) & (ptype==2) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1))%>%
      summarise_at(vars(input$y3),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  #OVERALL STATISTICS
  table3btot<-reactive(
    artnetLong %>%
      group_by(input$x3) %>%
      filter((p_ONGOING == 1 & duration < 2150) & (ptype==2) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
      summarise_at(vars(input$y3),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
  )

  #Bind Table Rows Tab 3
  output$table3all<-renderTable({rbind(table3alls(),table3alltot())})
  output$table3main<-renderTable({rbind(table3a(),table3atot())})
  output$table3casual<-renderTable({rbind(table3b(),table3btot())})

  
# #TABLE 3 TEST----------------------------------------------
#   
#   #SUMARY OUTPUT TABLE 3-ALL
#   table3alls<-reactive(
#     artnetLong %>%
#       group_by_(input$x3) %>%
#       filter((p_ONGOING == 1 & duration < 2150) & (ptype==1 | ptype==2) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
#       summarise(vars(duration),
#                    funs(#n=n(),
#                         mean(.,na.rm = TRUE),
#                         sd(.,na.rm=TRUE),
#                         median(.,na.rm = TRUE)
#                    )) %>%
#       setNames(c(" ", "n", "Mean", "SD", "Median"))
#   )
#   
#   #OVERALL STATISTICS
#   table3alltot<-reactive(
#     artnetLong %>%
#       group_by(input$x3) %>%
#       filter((p_ONGOING == 1 & duration < 2150) & (ptype==1 | ptype==2) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
#       summarise_at(vars(duration),
#                    funs(#n=n(),
#                         mean(.,na.rm = TRUE),
#                         sd(.,na.rm=TRUE),
#                         median(.,na.rm = TRUE)
#                    )) %>%
#       setNames(c(" ", "n", "Mean", "SD", "Median"))
#   )
#   
#   
#   
#   #SUMMARY OUTPUT TABLE 3a-MAIN
#   table3a<-reactive(
#     artnetLong %>%
#       group_by_(input$x3) %>%
#       filter((p_ONGOING == 1 & duration < 2150) & (ptype==1) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
#       summarise_at(vars(input$y3),
#                    funs(n=n(),
#                         mean(.,na.rm = TRUE),
#                         sd(.,na.rm=TRUE),
#                         median(.,na.rm = TRUE)
#                    )) %>%
#       setNames(c(" ", "n", "Mean", "SD", "Median"))
#   )
#   
#   #OVERALL STATISTICS
#   table3atot<-reactive(
#     artnetLong %>%
#       group_by(input$x3) %>%
#       filter((p_ONGOING == 1 & duration < 2150) & (ptype==1) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
#       summarise_at(vars(input$y3),
#                    funs(n=n(),
#                         mean(.,na.rm = TRUE),
#                         sd(.,na.rm=TRUE),
#                         median(.,na.rm = TRUE)
#                    )) %>%
#       setNames(c(" ", "n", "Mean", "SD", "Median"))
#   )
#   
#   #SUMMARY OUTPUT TABLE 3b-CASUAL
#   table3b<-reactive(
#     artnetLong %>%
#       group_by_(input$x3) %>%
#       filter((p_ONGOING == 1 & duration < 2150) & (ptype==2) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1))%>%
#       summarise_at(vars(input$y3),
#                    funs(n=n(),
#                         mean(.,na.rm = TRUE),
#                         sd(.,na.rm=TRUE),
#                         median(.,na.rm = TRUE)
#                    )) %>%
#       setNames(c(" ", "n", "Mean", "SD", "Median"))
#   )
#   
#   #OVERALL STATISTICS
#   table3btot<-reactive(
#     artnetLong %>%
#       group_by(input$x3) %>%
#       filter((p_ONGOING == 1 & duration < 2150) & (ptype==2) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
#       summarise_at(vars(input$y3),
#                    funs(n=n(),
#                         mean(.,na.rm = TRUE),
#                         sd(.,na.rm=TRUE),
#                         median(.,na.rm = TRUE)
#                    )) %>%
#       setNames(c(" ", "n", "Mean", "SD", "Median"))
#   )
#   
#   #Bind Table Rows Tab 3
#   output$table3all<-renderTable({rbind(table3alls(),table3alltot())})
#   output$table3main<-renderTable({rbind(table3a(),table3atot())})
#   output$table3casual<-renderTable({rbind(table3b(),table3btot())})  
#   
  

#TABLE 4------------------------------------------------------------

  
  table4allpart <- reactive({
    if (input$x4=="edgeagediff" | input$x4=="sqrtedgeagediff")
    {
      artnetLong4shiny %>%
        summarise_at(vars(input$x4),
                     funs(n=n(),
                          mean(.,na.rm = TRUE),
                          sd(.,na.rm=TRUE),
                          median(.,na.rm = TRUE)
                     )) %>%
        setNames(c("n", "Mean","Standard Deviation", "Median"))
    }
    else if (input$x4=="partraces")
    {
      artnetLong4shiny %>%
      group_by_(input$x4)%>%  
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ","n","%"))
    }
    else if (input$x4=="partages")
    {
      artnetLong4shiny %>%
        group_by_(input$x4)%>%  
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ","n","%"))
    }
    else
    {
      artnetLong4shiny %>%
      group_by_(input$x4)%>%  
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ","n","%"))
    }
  })
  
  
  table4allparttotal <- reactive({
    if (input$x4=="edgeagediff" | input$x4=="sqrtedgeagediff")
    {
    }
    else if (input$x4=="partraces")
    {
      artnetLong4shiny %>%
        group_by(input$x4)%>%  
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ","n","%"))
    }
    else if (input$x4=="partages")
    {
      artnetLong4shiny %>%
        group_by(input$x4)%>%  
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ","n","%"))
    }
    else
    {
      artnetLong4shiny %>%
        group_by(input$x4)%>%  
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ","n","%"))
    }
  })
  

  
  table4mains <- reactive({
    if (input$x4=="edgeagediff" | input$x4=="sqrtedgeagediff")
    {
      artnetLong4shiny %>%
        filter(ptype==1)%>%
        summarise_at(vars(input$x4),
                     funs(n=n(),
                          mean(.,na.rm = TRUE),
                          sd(.,na.rm=TRUE),
                          median(.,na.rm = TRUE)
                     )) %>%
        setNames(c("n", "Mean","Standard Deviation", "Median"))
    }
    else if (input$x4=="partraces")
    {
      artnetLong4shiny %>%
        group_by(input$x4) %>%
        filter(ptype==1)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else if (input$x4=="partages")
    {
      artnetLong4shiny %>%
        group_by(input$x4) %>%
        filter(ptype==1)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else
    {
      artnetLong4shiny %>%
        group_by(input$x4) %>%
        filter(ptype==1)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
  })
  
  
  table4maintotal <- reactive({
    if (input$x4=="edgeagediff" | input$x4=="sqrtedgeagediff")
    {
    }
    else if (input$x4=="partraces")
    {
      #partracesmaintot<-
      artnetLong4shiny %>%
        group_by_(input$x4) %>%
        filter(ptype==1)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else if (input$x4=="partages")
    {
      artnetLong4shiny %>%
        group_by_(input$x4) %>%
        filter(ptype==1)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else
    {
      #partstatusesmaintot<-
      artnetLong4shiny %>%
        group_by_(input$x4) %>%
        filter(ptype==1)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
  })
  
  
  table4casuals <- reactive({
    if (input$x4=="edgeagediff" | input$x4=="sqrtedgeagediff")
    {
      artnetLong4shiny %>%
        filter(ptype==2)%>%
        summarise_at(vars(input$x4),
                     funs(n=n(),
                          mean(.,na.rm = TRUE),
                          sd(.,na.rm=TRUE),
                          median(.,na.rm = TRUE)
                     )) %>%
        setNames(c("n", "Mean","Standard Deviation", "Median"))
    }
    else if (input$x4=="partraces")
    {
      artnetLong4shiny %>%
        group_by(input$x4) %>%
        filter(ptype==2)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else if (input$x4=="partages")
    {
      artnetLong4shiny %>%
        group_by(input$x4) %>%
        filter(ptype==2)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else
    {
      artnetLong4shiny %>%
        group_by(input$x4) %>%
        filter(ptype==2)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
  })
  
  
  table4casualtotal <- reactive({
    if (input$x4=="edgeagediff" | input$x4=="sqrtedgeagediff")
    {
    }
    else if (input$x4=="partraces")
    {
      artnetLong4shiny %>%
        group_by_(input$x4) %>%
        filter(ptype==2)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else if (input$x4=="partages")
    {
      artnetLong4shiny %>%
        group_by_(input$x4) %>%
        filter(ptype==2)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else
    {
      artnetLong4shiny %>%
        group_by_(input$x4) %>%
        filter(ptype==2)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
  })
  
  
  table4oneoffs <- reactive({
    if (input$x4=="edgeagediff" | input$x4=="sqrtedgeagediff")
    {
      artnetLong4shiny %>%
        filter(ptype==3)%>%
        summarise_at(vars(input$x4),
                     funs(n=n(),
                          mean(.,na.rm = TRUE),
                          sd(.,na.rm=TRUE),
                          median(.,na.rm = TRUE)
                     )) %>%
        setNames(c("n", "Mean","Standard Deviation", "Median"))
    }
    else if (input$x4=="partraces")
    {
      artnetLong4shiny %>%
        group_by(input$x4) %>%
        filter(ptype==3)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else if (input$x4=="partages")
    {
      artnetLong4shiny %>%
        group_by(input$x4) %>%
        filter(ptype==3)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else
    {
      artnetLong4shiny %>%
        group_by(input$x4) %>%
        filter(ptype==3)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
  })
  
  
  table4oneofftotal <- reactive({
    if (input$x4=="edgeagediff" | input$x4=="sqrtedgeagediff")
    {
    }
    else if (input$x4=="partraces")
    {
      #partracesoneofftot<-
      artnetLong4shiny %>%
        group_by_(input$x4) %>%
        filter(ptype==3)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else if (input$x4=="partages")
    {
      artnetLong4shiny %>%
        group_by_(input$x4) %>%
        filter(ptype==3)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
    else
    {
      #partstatusesoneofftot<-
      artnetLong4shiny %>%
        group_by_(input$x4) %>%
        filter(ptype==3)%>%
        summarise(n=n()) %>%
        mutate(freq= n/sum(n)*100) %>%
        setNames(c(" ", "n","%"))
    }
  })
  
  
  #Bind Table Rows Tab 4
  
  output$table4all<-renderTable({rbind(table4allpart(),table4allparttotal())})
  output$table4main<-renderTable({rbind(table4mains(),table4maintotal())})
  output$table4casual<-renderTable({rbind(table4casuals(),table4casualtotal())})
  output$table4oneoff<-renderTable({rbind(table4oneoffs(),table4oneofftotal())})  
  
  
  
#TABLE 2 DENSITY PLOTS-----------------------------------------------------------------  

  #CREATE DENSITY PLOT TAB 2-ALL
  t2every<-
    artnet %>%
    filter(maintotdegree==1 | castotdegree==1)
  output$densityplot2all<-renderPlot({
    ggplot(data=t2every, aes_string(x=input$y2, group=input$x2, color=input$x2))+
      geom_density(size=1) + theme_bw() + labs(y="Density",
                                               x="Number of Ongoing Partners",
                                               color=if(input$x2=="age.cat"){print ("Age Category")}
                                               else if(input$x2=="race.cat"){print ("Race Category")}
                                               else if(input$x2=="city"){print ("City")}
                                               else if(input$x2=="DIVCODE"){print("Division")}
                                               else if(input$x2=="hiv"){print ("HIV Status")}
                                               else if(input$x2=="NCHS_2013"){print ("Urbanicity (County)")})
  })


  #CREATE DENSITY PLOT TAB 2-MAIN
  t2main<-
    artnet %>%
    filter(mainaionlydegree==1 | mainoionlydegree==1)
  output$densityplot2main<-renderPlot({
    ggplot(data=t2main, aes_string(x=input$y2, group=input$x2, color=input$x2))+ 
      geom_density(size=1) + theme_bw() + labs(y="Density",
                                               x="Number of Ongoing Partners",
                                               color=if(input$x2=="age.cat"){print ("Age Category")}
                                               else if(input$x2=="race.cat"){print ("Race Category")}
                                               else if(input$x2=="city"){print ("City")}
                                               else if(input$x2=="DIVCODE"){print("Division")}
                                               else if(input$x2=="hiv"){print ("HIV Status")}
                                               else if(input$x2=="NCHS_2013"){print ("Urbanicity (County)")})
  })


  #CREATE DENSITY PLOT TAB 2-CASUAL
  t2casual<-
    artnet %>%
    filter(casaionlydegree==1 | casoionlydegree==1)
  output$densityplot2casual<-renderPlot({
    ggplot(data=t2casual, aes_string(x=input$y2, group=input$x2, color=input$x2))+ 
      geom_density(size=1) + theme_bw() + labs(y="Density",
                                               x="Number of Ongoing Partners",
                                               color=if(input$x2=="age.cat"){print ("Age Category")}
                                               else if(input$x2=="race.cat"){print ("Race Category")}
                                               else if(input$x2=="city"){print ("City")}
                                               else if(input$x2=="DIVCODE"){print("Division")}
                                               else if(input$x2=="hiv"){print ("HIV Status")}
                                               else if(input$x2=="NCHS_2013"){print ("Urbanicity (County)")})
  })

#CREATE DENSITY PLOT TAB 2-ONE OFF ALL
  t2oneoff<-
    artnet %>%
  filter()
  output$densityplot2oneoff<-renderPlot({
    ggplot(data=t2oneoff, aes_string(x=input$y2b, group=input$x2b, color=input$x2b)) +
      geom_density(size=1) + theme_bw() + labs(y="Density",
                                               x="Rate of One-Off Partners",
                                               color=if(input$x2b=="age.cat"){print ("Age Category")}
                                               else if(input$x2b=="race.cat"){print ("Race Category")}
                                               else if(input$x2b=="city"){print ("City")}
                                               else if(input$x2b=="DIVCODE"){print("Division")}
                                               else if(input$x2b=="hiv"){print ("HIV Status")}
                                               else if(input$x2b=="NCHS_2013"){print ("Urbanicity (County)")})
  })

#TABLE 3 PLOTS-----------------------------------------------
      
#CREATE DENSITY PLOT TAB 3-Every
  t3every<-
    artnetLong %>%
    filter(ptype==1 | ptype==2 & p_RAI==1 & p_IAI==1 & p_ROI==1 & p_IOI==1)
  output$densityplot3all<-renderPlot({
    ggplot(data=t3every, aes_string(x=input$y3, group=input$x3, color=input$x3))+
      geom_density(size=1) + theme_bw() + labs(y="Density",
                                               x= "Duration",
                                               color=if(input$x3=="age.cat"){print ("Age Category")}
                                               else if(input$x3=="race.cat"){print ("Race Category")}
                                               else if(input$x3=="city"){print ("City")}
                                               else if(input$x3=="DIVCODE"){print("Division")}
                                               else if(input$x3=="hiv"){print ("HIV Status")}
                                               else if(input$x3=="NCHS_2013"){print ("Urbanicity (County)")})
  })


#FILTERED DENSITY PLOT TAB 3- MAIN
  t3main<-
    artnetLong %>%
    filter(ptype==1 & p_RAI==1 & p_IAI==1 & p_ROI==1 & p_IOI==1)
  output$densityplot3main<-renderPlot({
    ggplot(data=t3main, aes_string(x=input$y3, group=input$x3, color=input$x3))+
      geom_density(size=1) + theme_bw()+ labs(y="Density",
                                              x="Duration",
                                              color=if(input$x3=="age.cat"){print ("Age Category")}
                                              else if(input$x3=="race.cat"){print ("Race Category")}
                                              else if(input$x3=="city"){print ("City")}
                                              else if(input$x3=="DIVCODE"){print("Division")}
                                              else if(input$x3=="hiv"){print ("HIV Status")}
                                              else if(input$x3=="NCHS_2013"){print ("Urbanicity (County)")})
  })



#CREATE DENSITY PLOT TAB 3-Casual
  t3casual<-
    artnetLong %>%
    filter(ptype==2 & p_RAI==1 & p_IAI==1 & p_ROI==1 & p_IOI==1)
  output$densityplot3casual<-renderPlot({
    ggplot(data=t3casual, aes_string(x=input$y3, group=input$x3, color=input$x3))+
      geom_density(size=1) + theme_bw()+ labs(y="Density",
                                              x="Duration",
                                              color=if(input$x3=="age.cat"){print ("Age Category")}
                                              else if(input$x3=="race.cat"){print ("Race Category")}
                                              else if(input$x3=="city"){print ("City")}
                                              else if(input$x3=="DIVCODE"){print("Division")}
                                              else if(input$x3=="hiv"){print ("HIV Status")}
                                              else if(input$x3=="NCHS_2013"){print ("Urbanicity (County)")})
  })



})


# Create a Shiny app object
shinyApp(ui = ui, server = shinyserver)

