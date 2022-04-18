# Title: Data Explanatory Shiny App Server
# Date Commence: 2 December 2021
# By: Hafizuddin
# Maintainer: 
# Objective: To serve as the server of the data explanatory shiny web app
# Version: 1.0
# Date Modified:
# Input Data: 
# Output: 

# R version 4.0.4 (2021-02-15)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] data.table_1.14.0     gridExtra_2.3         dplyr_1.0.7           DataExplorer_0.8.2    ggpubr_0.4.0          ggplot2_3.3.5        
# [7] shinycssloaders_1.0.0 DT_0.19               shinyWidgets_0.6.1    shinyjs_2.0.0         shinydashboard_0.7.1  shiny_1.6.0          
# 
# loaded via a namespace (and not attached):
#   [1] sass_0.4.0        tidyr_1.1.3       jsonlite_1.7.2    carData_3.0-4     bslib_0.3.0       cellranger_1.1.0  yaml_2.2.1        pillar_1.6.2     
# [9] backports_1.2.1   glue_1.4.2        digest_0.6.27     promises_1.2.0.1  ggsignif_0.6.3    colorspace_2.0-2  cowplot_1.1.1     plyr_1.8.6       
# [17] htmltools_0.5.2   httpuv_1.6.2      pkgconfig_2.0.3   broom_0.7.9       haven_2.4.3       purrr_0.3.4       xtable_1.8-4      scales_1.1.1     
# [25] openxlsx_4.2.4    later_1.3.0       rio_0.5.27        tibble_3.1.4      generics_0.1.0    farver_2.1.0      car_3.0-11        ellipsis_0.3.2   
# [33] cachem_1.0.6      withr_2.4.2       magrittr_2.0.1    crayon_1.4.1      readxl_1.3.1      mime_0.11         evaluate_0.14     fansi_0.5.0      
# [41] rstatix_0.7.0     forcats_0.5.1     foreign_0.8-81    tools_4.0.4       hms_1.1.0         lifecycle_1.0.0   stringr_1.4.0     munsell_0.5.0    
# [49] zip_2.2.0         networkD3_0.4     compiler_4.0.4    jquerylib_0.1.4   tinytex_0.33      rlang_0.4.11      grid_4.0.4        htmlwidgets_1.5.3
# [57] crosstalk_1.1.1   igraph_1.2.6      labeling_0.4.2    rmarkdown_2.10    gtable_0.3.0      abind_1.4-5       curl_4.3.2        reshape2_1.4.4   
# [65] R6_2.5.1          knitr_1.33        fastmap_1.1.0     utf8_1.2.2        stringi_1.7.4     parallel_4.0.4    Rcpp_1.0.7        vctrs_0.3.8      
# [73] tidyselect_1.1.1  xfun_0.25  

# library
library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(ggpubr)
library(dplyr)
library(plotly)

server = function(input, output, session){
  # body side
  # reactive values
  rv = reactiveValues(tab = NULL, data = NULL, dd = NULL)
  
  
  # read file
  observeEvent(input$file,{
    rmPop = F
    if(grepl('rds$',input$file$datapath,ignore.case = T)){
      rv$data = readRDS(input$file$datapath)
      rmPop = T
    }
    if(grepl('csv$',input$file$datapath,ignore.case = T)){
      rv$data = read.csv(input$file$datapath)
      rmPop = T
    }
    if(rmPop){
      removeModal()
    }
    # browser()
  })
  
  # update party
  observeEvent(input$Parties,{
    if(!is.null(rv$data)){
      rv$dd = input$Parties
    }
  })
  
  # update select input
  observe({
    if(!is.null(rv$tab)){
      switch (rv$tab,
              'tab1' = updateSelectInput(session, "Parties", label = "Choose Party", choices = unique(rv$data$Candidate.Party)),
              'tab2' = updateSelectInput(session, "Parties", label = "Choose Coalition", choices = unique(rv$data$Coalition)),
              'tab3' = updateSelectInput(session, "Parties", label = "Choose Gender", choices = unique(rv$data$Gender)),
              'tab4' = updateSelectInput(session, "Parties", label = "Choose Seat", choices = unique(rv$data$Seat.ID)),
              'tab5' = ,
              uiOutput('blank')
      )      
    }

  })
  
  # main page
  output$mainpage = renderUI({
    # show file input
    if(is.null(rv$data)){
      data = read.csv('data/Election-Results-2018 - Parlimen_Results_By_Candidate.csv')
      data$Votes.for.Candidate = as.integer(gsub(',','',data$Votes.for.Candidate))
      data$Total.Votes.Cast = as.integer(gsub(',','',data$Total.Votes.Cast))
      data$X..of.total.Votes = as.double(gsub('%','',data$X..of.total.Votes))
      data$Gender = gsub('^L$','MALE',data$Gender)
      data$Gender = gsub('^P$','FEMALE',data$Gender)
      rv$data = data
      rv$tab = 'NULL'
    }else{rv$tab = input$sidebar}
    # browser()
    switch (rv$tab,
      'tab1' = uiOutput('pg1'),
      'tab2' = uiOutput('pg2'),
      'tab3' = uiOutput('pg3'),
      'tab4' = uiOutput('pg4'),
      uiOutput('blank')
    )
  })
  
  output$blank = renderUI({})
  
  output$pg1 = renderUI({
    if(!is.null(rv$data)){
      # read data
      if(is.null(rv$dd)){
        party = unique(rv$data$Candidate.Party)[1]
      }else{
        party = rv$dd
      }
      # get data for value box
      party_df = rv$data %>% filter(Candidate.Party==party)
      coalition = party_df$Coalition[1]
      seat_contested = nrow(party_df)
      seat_win = nrow(party_df %>% filter(Candidate.Win==1))
      winning_rate = paste0(round(100*seat_win/seat_contested,2),'%')
      coalition_win = nrow(rv$data %>% filter(Coalition==party_df$Coalition[1]) %>% filter(Candidate.Win==1))
      overall_seats = length(unique(rv$data$Seat.ID))
      # get data for others
      lost_df = party_df %>% filter(Candidate.Win==0)
      lost_df = lost_df[order(lost_df$X..of.total.Votes, decreasing = T),]
      won_df = party_df %>% filter(Candidate.Win==1)
      won_df = won_df[order(won_df$X..of.total.Votes,decreasing = F),]
      
      # browser()
      # frontend
      tagList(
        fluidRow(
          div(valueBox("Party",value = party, width = '100%'),
              class = 'col-md-4 col-lg-6'),
          div(valueBox("Coalition",value = coalition, width = '100%'),
              class = 'col-md-4 col-lg-6'),
          div(valueBox("Seats Contested",value = paste0(seat_contested,'/',overall_seats), width = '100%',color="yellow"),
              class = 'col-md-4 col-lg-3'),
          div(valueBox("Seats Win",value = paste0(seat_win,'/',overall_seats), width = '100%',color="yellow"),
              class = 'col-md-4 col-lg-3'),
          div(valueBox("Winning Rate",value = winning_rate, width = '100%',color="green"),
              class = 'col-md-4 col-lg-3'),
          div(valueBox("Coalition Seats Win",value = paste0(coalition_win,'/',overall_seats), width = '100%',color="green"),
              class = 'col-md-4 col-lg-3')
        ),
        box(title = 'Highest Total Votes (Lost)', status = 'danger',solidHeader = T,
            fluidRow(
              taskItem(paste(lost_df$Seat.ID[1],lost_df$Candidate.Name[1]),value=lost_df$X..of.total.Votes[1]),
              taskItem(paste(lost_df$Seat.ID[2],lost_df$Candidate.Name[2]),value=lost_df$X..of.total.Votes[2]),
              taskItem(paste(lost_df$Seat.ID[3],lost_df$Candidate.Name[3]),value=lost_df$X..of.total.Votes[3]),
              taskItem(paste(lost_df$Seat.ID[4],lost_df$Candidate.Name[4]),value=lost_df$X..of.total.Votes[4]),
              taskItem(paste(lost_df$Seat.ID[5],lost_df$Candidate.Name[5]),value=lost_df$X..of.total.Votes[5]),
              tags$head(tags$style(HTML('.row{margin:10px}li{list-style:none}'))),
            ),width = 6),
        box(title = 'Lowest Total Votes (Won)', status = 'success',solidHeader = T,
            fluidRow(
              taskItem(paste(won_df$Seat.ID[1],won_df$Candidate.Name[1]),value=won_df$X..of.total.Votes[1]),
              taskItem(paste(won_df$Seat.ID[2],won_df$Candidate.Name[2]),value=won_df$X..of.total.Votes[2]),
              taskItem(paste(won_df$Seat.ID[3],won_df$Candidate.Name[3]),value=won_df$X..of.total.Votes[3]),
              taskItem(paste(won_df$Seat.ID[4],won_df$Candidate.Name[4]),value=won_df$X..of.total.Votes[4]),
              taskItem(paste(won_df$Seat.ID[5],won_df$Candidate.Name[5]),value=won_df$X..of.total.Votes[5]),
              tags$head(tags$style(HTML('.row{margin:10px}li{list-style:none}'))),
            ),width = 6),
      )           
    }
 

  })
  
  
  output$pg2 = renderUI({
    # read data
    if(is.null(rv$dd)){
      coalition = unique(rv$data$Coalition)[1]
    }else{
      coalition = rv$dd
    }
    # get data for value box
    coalition_df = rv$data %>% filter(Coalition==coalition)
    seat_contested = nrow(coalition_df)
    seat_win = nrow(coalition_df %>% filter(Candidate.Win==1))
    winning_rate = paste0(round(100*seat_win/seat_contested,2),'%')
    overall_seats = length(unique(rv$data$Seat.ID))
    # get data for pie chart
    # browser()
    contested_df = as.data.frame(table(coalition_df$Candidate.Party))
    won_df = as.data.frame(table(coalition_df$Candidate.Party[coalition_df$Candidate.Win==1]))
    contested_fig = plot_ly(contested_df,labels=~Var1,values=~Freq,type='pie')
    won_fig = plot_ly(won_df,labels=~Var1,values=~Freq,type='pie')
    # frontend
    tagList(
      fluidRow(
        div(valueBox("Coalition",value = coalition, width = '100%'),
            class = 'col-md-4 col-lg-3'),
        div(valueBox("Seats Contested",value = paste0(seat_contested,'/',overall_seats), width = '100%',color="yellow"),
            class = 'col-md-4 col-lg-3'),
        div(valueBox("Seats Win",value = paste0(seat_win,'/',overall_seats), width = '100%',color="yellow"),
            class = 'col-md-4 col-lg-3'),
        div(valueBox("Winning Rate",value = winning_rate, width = '100%',color="green"),
            class = 'col-md-4 col-lg-3'),
      ),
      box(title = 'Parties Contested', status = 'danger',solidHeader = T,
          fluidRow(
            renderPlotly(contested_fig),
          ),width = 6),
      box(title = 'Parties Won', status = 'success',solidHeader = T,
          fluidRow(
            renderPlotly(won_fig),
          ),width = 6),
    )
  })  
  
  output$pg3 = renderUI({
    # read data
    if(is.null(rv$dd)){
      gender = unique(rv$data$Gender)[1]
    }else{
      gender = rv$dd
    }
    # get data for value box
    gender_df = rv$data %>% filter(Gender==gender)
    seat_contested = nrow(gender_df)
    seat_win = nrow(gender_df %>% filter(Candidate.Win==1))
    winning_rate = paste0(round(100*seat_win/seat_contested,2),'%')
    overall_seats = length(unique(rv$data$Seat.ID))
    # get data for others
    lost_df = gender_df %>% filter(Candidate.Win==0)
    lost_df = lost_df[order(lost_df$X..of.total.Votes, decreasing = T),]
    won_df = gender_df %>% filter(Candidate.Win==1)
    won_df = won_df[order(won_df$X..of.total.Votes,decreasing = F),]
    # frontend
    tagList(
      fluidRow(
        div(valueBox("Gender",value = gender, width = '100%'),
            class = 'col-md-4 col-lg-3'),
        div(valueBox("Seats Contested",value = paste0(seat_contested,'/',overall_seats), width = '100%',color="yellow"),
            class = 'col-md-4 col-lg-3'),
        div(valueBox("Seats Win",value = paste0(seat_win,'/',overall_seats), width = '100%',color="yellow"),
            class = 'col-md-4 col-lg-3'),
        div(valueBox("Winning Rate",value = winning_rate, width = '100%',color="green"),
            class = 'col-md-4 col-lg-3'),
      ),
      
      box(title = 'Highest Total Votes (Lost)', status = 'danger',solidHeader = T,
          fluidRow(
            taskItem(paste(lost_df$Seat.ID[1],lost_df$Candidate.Name[1]),value=lost_df$X..of.total.Votes[1]),
            taskItem(paste(lost_df$Seat.ID[2],lost_df$Candidate.Name[2]),value=lost_df$X..of.total.Votes[2]),
            taskItem(paste(lost_df$Seat.ID[3],lost_df$Candidate.Name[3]),value=lost_df$X..of.total.Votes[3]),
            taskItem(paste(lost_df$Seat.ID[4],lost_df$Candidate.Name[4]),value=lost_df$X..of.total.Votes[4]),
            taskItem(paste(lost_df$Seat.ID[5],lost_df$Candidate.Name[5]),value=lost_df$X..of.total.Votes[5]),
            tags$head(tags$style(HTML('.row{margin:10px}li{list-style:none}'))),
          ),width = 6),
      box(title = 'Lowest Total Votes (Won)', status = 'success',solidHeader = T,
          fluidRow(
            taskItem(paste(won_df$Seat.ID[1],won_df$Candidate.Name[1]),value=won_df$X..of.total.Votes[1]),
            taskItem(paste(won_df$Seat.ID[2],won_df$Candidate.Name[2]),value=won_df$X..of.total.Votes[2]),
            taskItem(paste(won_df$Seat.ID[3],won_df$Candidate.Name[3]),value=won_df$X..of.total.Votes[3]),
            taskItem(paste(won_df$Seat.ID[4],won_df$Candidate.Name[4]),value=won_df$X..of.total.Votes[4]),
            taskItem(paste(won_df$Seat.ID[5],won_df$Candidate.Name[5]),value=won_df$X..of.total.Votes[5]),
            tags$head(tags$style(HTML('.row{margin:10px}li{list-style:none}'))),
          ),width = 6),
    )
  })  
  
  output$pg4 = renderUI({
    # read data
    if(is.null(rv$dd)){
      seat = unique(rv$data$Seat.ID)[1]
    }else{
      seat = rv$dd
    }
    # browser()
    # get data for value box
    seat_df = rv$data %>% filter(Seat.ID==seat)
    seat_df = seat_df[order(seat_df$X..of.total.Votes,decreasing = T),]
    fig = plot_ly(seat_df,labels=~Candidate.Party,values=~X..of.total.Votes,type='pie')
    
    tagList(
      fluidRow(
        div(valueBox("Seat ID",value = seat, width = '100%'),
            class = 'col-md-4 col-lg-3'),
        div(valueBox("Seat Name",value = seat_df$Seat.Name[1], width = '100%',color="yellow"),
            class = 'col-md-4 col-lg-3'),
        div(valueBox("Winning Party",value = seat_df$Candidate.Party[1], width = '100%',color="yellow"),
            class = 'col-md-4 col-lg-3'),
        div(valueBox("Total Votes",value = seat_df$X..of.total.Votes[1], width = '100%',color="green"),
            class = 'col-md-4 col-lg-3'),
      ),
      
      box(title = 'Party Total Votes', status = 'danger',solidHeader = T,
          fluidRow(
            renderPlotly(fig),
          ),width = 6),
      box(title = 'Candidates Total Votes', status = 'success',solidHeader = T,
          fluidRow(
            taskItem(paste(seat_df$Candidate.Name[1],'(',seat_df$Candidate.Party[1],')'),value=seat_df$X..of.total.Votes[1]),
            taskItem(paste(seat_df$Candidate.Name[2],'(',seat_df$Candidate.Party[2],')'),value=seat_df$X..of.total.Votes[2]),
            taskItem(paste(seat_df$Candidate.Name[3],'(',seat_df$Candidate.Party[3],')'),value=seat_df$X..of.total.Votes[3]),
            taskItem(paste(seat_df$Candidate.Name[4],'(',seat_df$Candidate.Party[4],')'),value=seat_df$X..of.total.Votes[4]),
            taskItem(paste(seat_df$Candidate.Name[5],'(',seat_df$Candidate.Party[5],')'),value=seat_df$X..of.total.Votes[5]),
            tags$head(tags$style(HTML('.row{margin:10px}li{list-style:none}'))),
          ),width = 6),
    )
  })  
  

}