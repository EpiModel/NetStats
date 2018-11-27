#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(haven)

#setwd("C:/Users/ramya/Box Sync/ETN-001/Data/Cleaned")
#artnet < -readRDS("../../../../Data/Cleaned/ARTNet-vars.rda")
#artnet <- readRDS("C:/Users/ramya/Box Sync/ETN-001/Products/NetStats/NetStats/artnet4shiny.rda")
#rawdata <- read_sav("C:/Users/ramya/Box Sync/ETN-001/Data/Cleaned/input/ART-Net_Raw.sav")
artnet <- readRDS("../../../../Products/NetStats/NetStats/artnet4shiny.rda")
artnetLong <- readRDS("../../../../Data/Cleaned/output/ARTNet-long.rda")
rawdata <- read_sav("../../../../Data/Cleaned/input/ART-Net_Raw.sav")
artnetLong4shiny <- readRDS("../../../../Products/NetStats/NetStats/artnetlong4shiny.rda")

rawdata$age.cat<-rep(NA, nrow(rawdata))
rawdata$age.cat[rawdata$AGE2 >= 15 & rawdata$AGE2 <=24] <- "15-24"
rawdata$age.cat[rawdata$AGE2 >= 25 & rawdata$AGE2 <= 29] <- "25-29"
rawdata$age.cat[rawdata$AGE2 >= 30 & rawdata$AGE2 <= 39] <- "30-39"
rawdata$age.cat[rawdata$AGE2 >= 40 & rawdata$AGE2 <= 49] <- "40-49"
rawdata$age.cat[rawdata$AGE2 >= 50 & rawdata$AGE2 <= 59] <- "50-59"
rawdata$age.cat[rawdata$AGE2 >= 60 & rawdata$AGE2 <= 65] <- "60-65"
rawdata$age.cat[rawdata$AGE2 >= 66] <- "66+"


rawdata$city[rawdata$city==""]<-"NA"



artnetLong$age.cat<-rep(NA, nrow(artnetLong))
artnetLong$age.cat[artnetLong$age >= 15 & artnetLong$age <=24] <- "15-24"
artnetLong$age.cat[artnetLong$age >= 25 & artnetLong$age <= 29] <- "25-29"
artnetLong$age.cat[artnetLong$age >= 30 & artnetLong$age <= 39] <- "30-39"
artnetLong$age.cat[artnetLong$age >= 40 & artnetLong$age <= 49] <- "40-49"
artnetLong$age.cat[artnetLong$age >= 50 & artnetLong$age <= 59] <- "50-59"
artnetLong$age.cat[artnetLong$age >= 60 & artnetLong$age <= 65] <- "60-65"
artnetLong$age.cat[artnetLong$age >= 66] <- "66+"

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



# UI
ui <- fluidPage(

  navbarPage("ART-NET Data",
             tabPanel("Characteristics of Individuals",
                      sidebarLayout(
                        # Input(s)
                        sidebarPanel(
                          titlePanel(h3("Inputs", align="center")),
                          # # Select Variables
                          # selectInput(inputId ="y1",
                          #             label = "Variable",
                          #             choices = c(
                          #               "Survey Completion Status"= "Vstatus"
                          #             ),
                          #             selected = "Vstatus"),
                          # Select Stratifications
                          selectInput(inputId ="x1",
                                      label = "Stratifications",
                                      choices = c(
                                        "Age Category"="age.cat",
                                        "City"="city",
                                        "Education Level"= "education",
                                        "HIV Status"="hiv",
                                        "Race Category"= "race.cat",
                                        "Region/Division"= "DIVCODE",
                                        "Urbanicity (County)"= "NCHS_2013"
                                      ),
                                      selected = "age.cat")
                        ),


                        # Main panel for displaying Outputs
                        mainPanel(
                          #header and summary of distribution
                          h3("Summary Data Table", align="center"),
                          h4("ART-NET Survey Completion", align="center"),
                          tableOutput("table1a"),
                          tableOutput("table1b"),
                          tableOutput("table1")
                          #scatter plot
                          #h3("Plot", align="center"),
                          #plotOutput (outputId = "scatterplot")
                        )
                      )
             ),


             tabPanel("Mean Partnership Degree",
                      sidebarLayout(
                        # Input(s)
                        sidebarPanel(
                          titlePanel(h3("Inputs", align="center")),
                          # Select Variables
                          selectInput(inputId ="y2",
                                      label = "Variable",
                                      choices = c(
                                        "Number of Ongoing AI or OI Sexual Partners"= "totdegree",
                                        "Number of Ongoing AI Partners"= "aionlydegree",
                                        "Number of Ongoing OI Partners"="oionlydegree"
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
                                        "Region/Division"="DIVCODE",
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
                                   plotOutput(outputId="densityplot2all" ))
                          ),
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
                                   plotOutput(outputId="densityplot2casual"))
                          )
                        )
                      )
             ),

             tabPanel("Partnership Duration",
                      sidebarLayout(
                        # Input(s)
                        sidebarPanel(
                          titlePanel(h3("Inputs", align="center")),
                          # Select Variables
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
                                        "Region/Division"="DIVCODE",
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
                                   plotOutput(outputId="densityplot3casual"))
                          )
                        )
                      )
             ),



             tabPanel("Partnership Level Charactersitics (Mixing)",
                      sidebarLayout(
                        # Input(s)
                        sidebarPanel(
                          titlePanel(h3("Inputs", align="center")),
                          # Select Variables
                          selectInput(inputId ="y4",
                                      label = "Variable",
                                      choices = c(
                                        "Acts per Week"= "p_total.acts.week"
                                      ),
                                      selected = "p_total.acts.week"),
                          # Select Stratifications
                          selectInput(inputId ="x4",
                                      label = "Stratifications",
                                      choices = c(
                                        "Race"="partraces",
                                        "HIV Status"="partstatuses",
                                        "Age"="edgeagediff"
                                      ),
                                      selected = "partraces")

                        ),

                        mainPanel(
                          fluidRow(
                            h2("All Partnerships", align="center"),
                            column(5,
                                   h4("All Partnerships", align="center"),
                                   tableOutput("table4all")),
                            column(7,
                                   h4("Density Plot- All Partnerships", align="center"),
                                   plotOutput(outputId="densityplot4all" ))
                          ),
                          br(),
                          br(),
                          fluidRow(
                            h2("Main Partnerships", align="center"),
                            column(5,
                                   h4("Main Partnerships", align="center"),
                                   tableOutput("table4main")),
                            column(7,
                                   h4("Density Plot- Main Partnerships", align="center"),
                                   plotOutput(outputId="densityplot4main"))
                          ),
                          br(),
                          br(),
                          fluidRow(
                            h2("Casual Partnerships", align="center"),
                            column(5,
                                   h4("Casual Partnerships", align="center"),
                                   tableOutput("table4casual")),
                            column(7,
                                   h4("Density Plot- Casual Partnerships", align="center"),
                                   plotOutput(outputId="densityplot4casual"))
                          )
                        )
                      )
             )




             # tabPanel("Partnership Level Charactersitics (Mixing)",
             #          sidebarLayout(
             #            # Input(s)
             #            sidebarPanel(
             #              titlePanel(h3("Inputs", align="center")),
             #              # Select Variables
             #              selectInput(inputId ="y4",
             #                          label = "Variable",
             #                          choices = c(
             #                            "Acts per Week"= "p_total.acts.week"
             #                          ),
             #                          selected = "p_total.acts.week"),
             #              # Select Stratifications
             #              selectInput(inputId ="x4",
             #                          label = "Stratifications",
             #                          choices = c(
             #                            "Race"="race",
             #                            "HIV Status"="HIVSTATUS",
             #                            "Age"="Age"
             #                          ),
             #                          selected = "race")
             #
             #            ),
             #
             #
             #
             #            # Main panel for displaying Outputs
             #            mainPanel(
             #              #header and summary of distribution
             #              h3("Summary Data Table", align="center"),
             #              tableOutput("table4a"),
             #              tableOutput("table4b"),
             #              tableOutput("table4"),
             #              #density plot
             #              h3("Density Plot", align="center"),
             #              plotOutput (outputId = "densityplot4"))
             #          )
             #
             # )




  )
)



# Server
shinyserver<-(function(input, output) {


  # observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, {
  #   # event will be called when histdata changes, which only happens once, when it is initially calculated
  #   showModal(modalDialog(
  #     title = "Landing Page",
  #     h1('Landing Page'),
  #     p('Theoretically you can put whatever content you want in here')
  #   ))
  # })


  datasetInputx1 <- reactive({
    switch(input$x1,
           "Age Category"=artnet$age.cat,
           "city"=artnet$city,
           "Education Level"= artnet$education,
           "HIV Status"=artnet$hiv,
           "Race Category"= artnet$race.cat,
           "Region/Division"= artnet$DIVCODE,
           "Urbanicity (County)"=artnet$NCHS_2013
    )
  })

  # datasetInputy1<- reactive({
  #   switch(input$y1,
  #          "Survey Completion Status"= rawdata$Vstatus
  #   )
  # })

  datasetInputx2<-reactive({
    switch(input$x2,
           "Age Category"=artnet$age.cat,
           "City"= artnet$city,
           "HIV Status"=artnet$hiv,
           "Race Category"= artnet$race.cat,
           "Region/Division"=artnet$DIVCODE,
           "Urbanicity (County)"=artnet$NCHS_2013
    )
  })

  datasetInputy2<- reactive({
    switch(input$y2,
           "Number of Ongoing AI or OI Sexual Partners"= artnet$totdegree,
           "Number of Ongoing AI Partners"= artnet$aionlydegree,
           "Number of Ongoing OI Partners"=artnet$oionlydegree
    )
  })


  datasetInputx3<-reactive({
    switch(input$x3,
           "Age Category"=artnetLong$age.cat,
           "City"= artnetLong$city2,
           "HIV Status"=artnetLong$hiv,
           "Race Category"= artnetLong$race.cat,
           "Region/Division"=artnetLong$DIVCODE,
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
             "Age"= artnetLong4shiny$edgeagediff
      )
    })


    datasetInputy4<- reactive({
      switch(input$y4,
             "Acts per Week"= artnetLong4shiny$p_total.acts.week
      )
    })



  })






  # #SUMMARY OUTPUT TABLE 1a
  # table1a<-reactive(
  #   rawdata %>%
  #     #filter(rawdata$Vstatus=="Complete") %>%
  #     group_by_(input$x1) %>%
  #     summarise_at(vars(input$x1),
  #                  funs(n=n()
  #                       #"%"= n()/nrow(rawdata)*100
  #                       #"%"= nrow(rawdata[which(rawdata$Vstatus=="Complete"),])/nrow(rawdata)*100
  #                  )) %>%
  #     mutate(freq= n/sum(n)*100) %>%
  #     setNames(c(" ","n","%"))
  # )
  #
  #
  # #SUMMARY OUTPUT TABLE 1b
  # table1b<-reactive(
  #   rawdata %>%
  #     #filter(rawdata$Vstatus=="Complete") %>%
  #     group_by(input$x1) %>%
  #     summarise_at(vars(input$x1),
  #                  funs(n=n()
  #                  )) %>%
  #     mutate(freq= n/sum(n)*100) %>%
  #     setNames(c(" ","n","%"))
  # )


  #SUMMARY OUTPUT TABLE 1a
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
  #add_row_names(table1b)<-"Total"

  # OUTPUT TABLE TAB 1
  output$table1<-renderTable({rbind(table1a(),table1b())})






  #SUMMARY OUTPUT TABLE 2 ALL
  table2every<-reactive(
    artnet %>%
      group_by_(input$x2) %>%
      filter(maintotdegree==1 | castotdegree==1) %>%
      summarise_at(vars(input$y2),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #L CONF INTERVAL
                        #mean(.,na.rm = TRUE)- qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #U CONF INTERVAL
                        #mean(.,na.rm = TRUE)+ qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
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
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #L CONF INRERVAL
                        #mean(.,na.rm = TRUE)- qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #U CONF INTERVAL
                        #mean(.,na.rm = TRUE)+ qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
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
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #L CONF INRERVAL
                        #mean(.,na.rm = TRUE)- qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #U CONF INTERVAL
                        #mean(.,na.rm = TRUE)+ qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
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
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #L CONF INRERVAL
                        #mean(.,na.rm = TRUE)- qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #U CONF INTERVAL
                        #mean(.,na.rm = TRUE)+ qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
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
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #L CONF INRERVAL
                        #mean(.,na.rm = TRUE)- qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #U CONF INTERVAL
                        #mean(.,na.rm = TRUE)+ qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
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
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #L CONF INRERVAL
                        #mean(.,na.rm = TRUE)- qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #U CONF INTERVAL
                        #mean(.,na.rm = TRUE)+ qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
  )


  #Bind Table Rows Tab 2
  output$table2all<-renderTable({rbind(table2every(),table2everytot())})
  output$table2main<-renderTable({rbind(table2amain(),table2amaintot())})
  output$table2casual<-renderTable({rbind(table2bcasual(),table2bcasualtot())})



  #SUMARY OUTPUT TABLE 3-ALL
  table3alls<-reactive(
    artnetLong %>%
      group_by_(input$x3) %>%
      filter((p_ONGOING == 1 & duration < 2150) & (ptype==1 | ptype==2) & (p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
      summarise_at(vars(input$y3),
                   funs(n=n(),
                        mean(.,na.rm = TRUE),
                        sd(.,na.rm=TRUE),
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
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
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
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
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
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
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
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
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
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
                        #q1=quantile(.,probs=.25,na.rm=TRUE),
                        median(.,na.rm = TRUE)
                        #q3=quantile(.,probs=.75,na.rm=TRUE),
                        #IQR(.,na.rm = TRUE),
                        #min(.,na.rm=TRUE),
                        #max(.,na.rm=TRUE)
                   )) %>%
      setNames(c(" ", "n", "Mean", "SD", "Median"))
    #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
  )

  #Bind Table Rows Tab 3
  output$table3all<-renderTable({rbind(table3alls(),table3alltot())})
  output$table3main<-renderTable({rbind(table3a(),table3atot())})
  output$table3casual<-renderTable({rbind(table3b(),table3btot())})


  #SUMARY OUTPUT TABLE 4-ALL
  table4alls<-reactive(
    artnetLong4shiny %>%
      group_by_(input$x4) %>%
      #filter((p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
      summarise(n=n()
      ) %>%
      mutate(freq= n/sum(n)*100) %>%
      setNames(c(" ","n","%"))
    #   summarise_at(vars(input$y4),
    #                funs(n=n(),
    #                     mean(.,na.rm = TRUE),
    #                     sd(.,na.rm=TRUE),
    #                     median(.,na.rm = TRUE)
    #                )) %>%
    #   setNames(c(" ", "n", "Mean", "SD", "Median"))
    # #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
  )

  #OVERALL STATISTICS
  table4alltot<-reactive(
    artnetLong4shiny %>%
      group_by(input$x4) %>%
      #filter((p_RAI==1 | p_IAI==1 | p_ROI==1 | p_IOI==1)) %>%
      summarise(n=n()
      ) %>%
      mutate(freq= n/sum(n)*100) %>%
      setNames(c(" ","n","%"))
    #   summarise_at(vars(input$y4),
    #                funs(n=n(),
    #                     mean(.,na.rm = TRUE),
    #                     sd(.,na.rm=TRUE),
    #                     median(.,na.rm = TRUE)
    #                )) %>%
    #   setNames(c(" ", "n", "Mean", "SD", "Median"))
    # #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
  )



  #SUMMARY OUTPUT TABLE 4a-MAIN
  table4a<-reactive(
    artnetLong4shiny %>%
      group_by_(input$x4) %>%
      filter((ptype==1)) %>%
      summarise(n=n()
      ) %>%
      mutate(freq= n/sum(n)*100) %>%
      setNames(c(" ","n","%"))
    #   summarise_at(vars(input$y4),
    #                funs(n=n(),
    #                     mean(.,na.rm = TRUE),
    #                     sd(.,na.rm=TRUE),
    #                     #q1=quantile(.,probs=.25,na.rm=TRUE),
    #                     median(.,na.rm = TRUE)
    #                     #q3=quantile(.,probs=.75,na.rm=TRUE),
    #                     #IQR(.,na.rm = TRUE),
    #                     #min(.,na.rm=TRUE),
    #                     #max(.,na.rm=TRUE)
    #                )) %>%
    #   setNames(c(" ", "n", "Mean", "SD", "Median"))
    # #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
  )

  #OVERALL STATISTICS
  table4atot<-reactive(
    artnetLong4shiny %>%
      group_by(input$x4) %>%
      filter((ptype==1)) %>%
      summarise(n=n()
      ) %>%
      mutate(freq= n/sum(n)*100) %>%
      setNames(c(" ","n","%"))
    #   summarise_at(vars(input$y4),
    #                funs(n=n(),
    #                     mean(.,na.rm = TRUE),
    #                     sd(.,na.rm=TRUE),
    #                     #q1=quantile(.,probs=.25,na.rm=TRUE),
    #                     median(.,na.rm = TRUE)
    #                     #q3=quantile(.,probs=.75,na.rm=TRUE),
    #                     #IQR(.,na.rm = TRUE),
    #                     #min(.,na.rm=TRUE),
    #                     #max(.,na.rm=TRUE)
    #                )) %>%
    #   setNames(c(" ", "n", "Mean", "SD", "Median"))
    # #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
  )

  #SUMMARY OUTPUT TABLE 4b-CASUAL
  table4b<-reactive(
    artnetLong4shiny %>%
      group_by_(input$x4) %>%
      filter((ptype==2))%>%
      summarise(n=n()
      ) %>%
      mutate(freq= n/sum(n)*100) %>%
      setNames(c(" ","n","%"))
    #   summarise_at(vars(input$y4),
    #                funs(n=n(),
    #                     mean(.,na.rm = TRUE),
    #                     sd(.,na.rm=TRUE),
    #                     #q1=quantile(.,probs=.25,na.rm=TRUE),
    #                     median(.,na.rm = TRUE)
    #                     #q3=quantile(.,probs=.75,na.rm=TRUE),
    #                     #IQR(.,na.rm = TRUE),
    #                     #min(.,na.rm=TRUE),
    #                     #max(.,na.rm=TRUE)
    #                )) %>%
    #   setNames(c(" ", "n", "Mean", "SD", "Median"))
    # #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
  )

  #OVERALL STATISTICS
  table4btot<-reactive(
    artnetLong4shiny %>%
      group_by(input$x4) %>%
      filter((ptype==2)) %>%
      summarise(n=n()
      ) %>%
      mutate(freq= n/sum(n)*100) %>%
      setNames(c(" ","n","%"))
    #   summarise_at(vars(input$y4),
    #                funs(n=n(),
    #                     mean(.,na.rm = TRUE),
    #                     sd(.,na.rm=TRUE),
    #                     #q1=quantile(.,probs=.25,na.rm=TRUE),
    #                     median(.,na.rm = TRUE)
    #                     #q3=quantile(.,probs=.75,na.rm=TRUE),
    #                     #IQR(.,na.rm = TRUE),
    #                     #min(.,na.rm=TRUE),
    #                     #max(.,na.rm=TRUE)
    #                )) %>%
    #   setNames(c(" ", "n", "Mean", "SD", "Median"))
    # #setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
  )

  #Bind Table Rows Tab 4
  output$table4all<-renderTable({rbind(table4alls(),table4alltot())})
  output$table4main<-renderTable({rbind(table4a(),table4atot())})
  output$table4casual<-renderTable({rbind(table4b(),table4btot())})



  # #SUMMARY OUTPUT TABLE 4
  # table4a<-reactive(
  #   artnet %>%
  #     group_by_(input$x41,input$x42) %>%
  #     summarise_at(vars(input$y4),
  #                  funs(n=n(),
  #                       mean(.,na.rm = TRUE),
  #                       sd(.,na.rm=TRUE),
  #                       q1=quantile(.,probs=.25,na.rm=TRUE),
  #                       median(.,na.rm = TRUE),
  #                       q3=quantile(.,probs=.75,na.rm=TRUE),
  #                       IQR(.,na.rm = TRUE),
  #                       #L CONF INRERVAL
  #                       #mean(.,na.rm = TRUE)- qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
  #                       #U CONF INTERVAL
  #                       #mean(.,na.rm = TRUE)+ qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
  #                       min(.,na.rm=TRUE),
  #                       max(.,na.rm=TRUE)
  #                  )) %>%
  #     setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
  # )
  #
  # #OVERALL STATISTICS
  # table4b<-reactive(
  #   artnet %>%
  #     group_by(input$x41,input$x42) %>%
  #     summarise_at(vars(input$y4),
  #                  funs(n=n(),
  #                       mean(.,na.rm = TRUE),
  #                       sd(.,na.rm=TRUE),
  #                       q1=quantile(.,probs=.25,na.rm=TRUE),
  #                       median(.,na.rm = TRUE),
  #                       q3=quantile(.,probs=.75,na.rm=TRUE),
  #                       IQR(.,na.rm = TRUE),
  #                       #L CONF INRERVAL
  #                       #mean(.,na.rm = TRUE)- qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
  #                       #U CONF INTERVAL
  #                       #mean(.,na.rm = TRUE)+ qnorm(.975)*(sd(.,na.rm =TRUE)/sqrt(n())),
  #                       min(.,na.rm=TRUE),
  #                       max(.,na.rm=TRUE)
  #                  )) %>%
  #     setNames(c(" ", "n", "Mean", "SD", "Q1", "Median", "Q3", "IQR", "Minimum", "Maximum"))
  # )
  #
  #
  # #Bind Table Rows Tab 4
  # output$table4<-renderTable({rbind(table4a(),table4b())
  # })



  # # Create scatterplot object the plotOutput function is expecting
  # output$scatterplot <- renderPlot({
  #   ggplot(data = rawdata, aes_string(x = input$x1,
  #                                     y = input$y1)) +
  #     geom_point()
  #   #geom_point(aes(color=1))
  # })


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

  #CREATE DENSITY PLOT TAB 4-Every
  t4every<-
    artnetLong4shiny
  output$densityplot4all<-renderPlot({
    ggplot(data=t4every, aes_string(x=input$y4, group=input$x4, color=input$x4))+
      geom_density(size=1) + theme_bw() + labs(y="Density",
                                               x= "Duration",
                                               color=if(input$x4=="edgeagediff"){print ("Age")}
                                               else if(input$x4=="partraces"){print ("Race")}
                                               else if(input$x4=="partstatuses"){print ("HIV Status")})
  })





  #FILTERED DENSITY PLOT TAB 4- MAIN
  t4main<-
    artnetLong4shiny
  output$densityplot4main<-renderPlot({
    ggplot(data=t4main, aes_string(x=input$y4, group=input$x4, color=input$x4))+
      geom_density(size=1) + theme_bw()+ labs(y="Density",
                                              x="Duration",
                                              color=if(input$x4=="edgeagediff"){print ("Age")}
                                              else if(input$x4=="partraces"){print ("Race")}
                                              else if(input$x4=="partstatuses"){print ("HIV Status")})
  })



  #CREATE DENSITY PLOT TAB 4-Casual
  t4casual<-
    artnetLong4shiny
  output$densityplot4casual<-renderPlot({
    ggplot(data=t4casual, aes_string(x=input$y4, group=input$x4, color=input$x4))+
      geom_density(size=1) + theme_bw()+ labs(y="Density",
                                              x="Duration",
                                              color=if(input$x4=="edgeagediff"){print ("Age")}
                                              else if(input$x4=="partraces"){print ("Race")}
                                              else if(input$x4=="partstatuses"){print ("HIV Status")})
  })





  #
  #   #CREATE DENSITY PLOT TAB 4
  #   output$densityplot4<-renderPlot({
  #     ggplot(data=artnet, aes_string(x=input$y4, group=input$x4, color=input$x4))+
  #       geom_density(size=1) + theme_bw()
  #   })

}
)

# Create a Shiny app object
shinyApp(ui = ui, server = shinyserver)

