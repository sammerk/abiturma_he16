library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(shinyBS)
library(feather)
library(svgPanZoom)
library(gridSVG)
library(scrypt)




# Ip-Test ################################################################################

ipID <- renderPrint(print(input$ipid))

# Data Import ############################################################################

kursdata <- read.table("data/kursdata_fr16_b_demo.csv", sep = ";", header = TRUE, na.strings = c("NA"))       # Import of Data
#likertdata1 <- read.table("data/likertdata_fr16_2.csv", sep = ";", header = T, na.strings = c("NA"))
freitextdata <- read.table("data/freitextdata_fr16_demo.csv", sep = ";", header = T, na.strings = c("NA"))
likertdata1 <- read.table("data/likertdata_fr16_2_demo.csv", sep = ";", header = T, na.strings = c("NA"))
pw_data <- read_feather("Stuff/kl_pw.feather")


##########################################################################################
# Custom Functions ###
##########################################################################################

# Function for mean (jitter plot panel) ##################################################

stat_sum_single <- function(fun, geom="point", ...) {                     
  stat_summary(fun.y=fun, colour="#DC5C40", geom=geom, size = 3.0, ...)
}




# Functions for usage tracking of jitter Navbarpage #######################################

fields <- c("qualdim", "darstell", "scaling", "groupin", "ipid")   # names of fields to track

outputDir <- "responses_jitter"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}


# Functions for usage tracking of likert Navbarpage #####################################

fields_l <- c("likertfragen", "ipid", "groupinl")   # names of fields to track
outputDir_l <- "responses_likert"

saveData_l <- function(data_l) {
  data_l <- t(data_l)
  # Create a unique file name
  fileName_l <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data_l))
  # Write the file to the local system
  write.csv(
    x = data_l,
    file = file.path(outputDir_l, fileName_l), 
    row.names = FALSE, quote = TRUE
  )
}

loadData_l <- function() {
  # Read all the files into a list
  files_l <- list.files(outputDir_l, full.names = TRUE)
  data_l <- lapply(files_l, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data_l <- do.call(rbind, data_l)
  data_l
}


# Functions for usage tracking of freitext Navbarpage ####################################

fields_f <- c("sort_freitexte", "ipid")   # names of fields to track
outputDir_f <- "responses_freitext"

saveData_f <- function(data_f) {
  data_f <- t(data_f)
  # Create a unique file name
  fileName_f <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data_f))
  # Write the file to the local system
  write.csv(
    x = data_f,
    file = file.path(outputDir_f, fileName_f), 
    row.names = FALSE, quote = TRUE
  )
}

loadData_f <- function() {
  # Read all the files into a list
  files_f <- list.files(outputDir_f, full.names = TRUE)
  data_f <- lapply(files_f, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data_f <- do.call(rbind, data_f)
  data_f
}



# Functions for usage tracking of qualidim2 Navbarpage ################################################################
fields_q2 <- c("qualdim2", "ipid")   # names of fields to track
outputDir_q2 <- "responses_qualdim2"

saveData_q2 <- function(data_q2) {
  data_q2 <- t(data_q2)
  # Create a unique file name
  fileName_q2 <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data_q2))
  # Write the file to the local system
  write.csv(
    x = data_q2,
    file = file.path(outputDir_q2, fileName_q2), 
    row.names = FALSE, quote = TRUE
  )
}

loadData_q2 <- function() {
  # Read all the files into a list
  files_q2 <- list.files(outputDir_f, full.names = TRUE)
  data_q2 <- lapply(files_q2, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data_q2 <- do.call(rbind, data_q2)
  data_q2
}




#######################################################################################################################
####                            #######################################################################################
####                            #######################################################################################
####       Shiny internal       #######################################################################################
####                            ####################################################################################### 
####                            ####################################################################################### 
####                            ####################################################################################### 
#######################################################################################################################

shinyServer(function(input, output, session) {
  
  #####################################################################################################################
  # Test Fingerprinting                                                                                    ############
  #####################################################################################################################
  
  #output$testtext <- renderText(paste("     fingerprint: ", input$fingerprint, "     ip: ", input$ipid))
  
  
  #####################################################################################################################
  # User Import                                                                                            ############
  #####################################################################################################################
  
  user <- reactive({
    return(input$username)

  })

  #####################################################################################################################
  # Log in Backend                                                                                       ############
  #####################################################################################################################
  
  ## Login Feedback for user with busyindicator
  
  # reactive value of valid login
  
  login_true <- reactive({verifyPassword(as.character(pw_data[pw_data$Login == user(),"Passwort_scrypted"]), 
                               as.character(input$passw)) == TRUE
    })  
  
  observeEvent(input$loginBtn, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("loginBtn", {
      if (login_true() == FALSE) {
        stop("Login Passwort Kombi ist falsch!")
      }
    })
  })
  
  
  ## Hide further tabs before sucessful login
  
  observe({
    hide(selector = c("#navbarpage li a[data-value=einzelfragen]", 
                      "#navbarpage li a[data-value=qualdim_v1]",
                      "#navbarpage li a[data-value=qualdim_v2]",
                      "#navbarpage li a[data-value=freitext_antw]"))
  })
  
  observeEvent(input$loginBtn, {
    if(login_true() == TRUE)
      show(selector = c("#navbarpage li a[data-value=einzelfragen]", 
                        "#navbarpage li a[data-value=qualdim_v1]",
                        "#navbarpage li a[data-value=qualdim_v2]",
                        "#navbarpage li a[data-value=freitext_antw]"))
  })
  
  #####################################################################################################################
  # Freitext Backend                                                                                       ############
  #####################################################################################################################
  
  ##  Freitextdaten subsetten  ########################################################################################
  
  freitextdata2 <- 
    eventReactive(input$gofreitext, { 
      
      if(input$sort_freitexte == "abst"){
        freitextdata1 <-
          freitextdata%>%
          filter(kursleiterin == user())%>%
          arrange(desc(score))
      }
      
      
      if(input$sort_freitexte == "nsort" | is.null(input$sort_freitexte) == TRUE){
        freitextdata1 <-
          freitextdata%>%
          filter(kursleiterin == user())
      }
      
      
      return(freitextdata1)
      
      
    })
  
  ## Debug DF
  #output$freitextdata_debug <-  DT::renderDataTable({freitextdata2()})
  
  freitextdata3 <- 
    reactive({ 
      freitextdata1 <-
        freitextdata%>%
        filter(kursleiterin == user(),
               ftk != "data/freitextbilder_questor4/")
    })
  
  
  ## 
  max_freitextplots <- 70
  
  
  
  ## Freitextplotschleife #############################################################################################
  
  freitextplotgone <- eventReactive(input$gofreitext,{  
    
    plot_output_list <- lapply(1:length((freitextdata2()$ftk)), function(i) {
      plotname <- paste("freitextplot", i, sep="")
      plotOutput(plotname, height = 100)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  
  
  for (i in 1:max_freitextplots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    
    local({
      my_i <- i
      plotname <- paste("freitextplot", my_i, sep="")
      
      output[[plotname]] <- renderImage({
        path1 <- as.character(freitextdata2()$ftk[my_i])
        list(src = path1,
             alt = paste("")
        )
        
      })
      
      
    })
  }
  
  ## Rendering FreitextUI #############################################################################################
  output$freitextplots <- renderUI({
    freitextplotgone()
  })
  
  ## Usage tracking freitextplot  #####################################################################################
  
  # Whenever a field is filled, aggregate all form data
  formData_f <- reactive({
    data_f <- sapply(fields_f, function(x) paste(input[[x]], collapse = "-"))
    
    data_f$systtime <- paste(Sys.time())
    data_f$user <- user()
    data_f
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$gofreitext, {
    
    saveData_f(formData_f())
  })
  
  
  #####################################################################################################################
  # Qualitätsdimensionen Backend                                                                           ############
  #####################################################################################################################
  
  # Create reactive dataset for Plot of Quality Dimensions with grouping-variables ####################################
  kursdata1 <- 
    reactive({ kursdata2 <- 
      
      kursdata%>%
      filter(variable %in% input$qualdim)%>%
      mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"),
             mggroup     =  paste("Dein Kurs in ", Kursort, "\n", "Kursbeginn: ", Kursbeginn, ", ", Uhrzeit, sep = ""),
             mgxgmgroup  =  ifelse(kursleiterin == user(), paste("Dein Kurs in ", Kursort, ", Kursbeginn: ", "\n", 
                                                                 Kursbeginn, ", ", Uhrzeit, sep = "")
                                   , "abiturma gesamt"),
             variable    =  factor(variable))
    return(kursdata2)
    })
  
  # Kursanzahl extrahieren für adaptive plotheight ####################################################################  
  n_lev_mg <- eventReactive(input$golikert, {nlevdata <- kursdata1()%>%      #Count of teached courses by actual user
    filter(kursleiterin == user())
  return(nlevels(as.factor(nlevdata$kurs)))})
  
  
  # Main Jitter-Plot ##################################################################################################
  
  
  qualdimplotgone <- eventReactive(input$goqualdim,{     
    
    # Data fetching ###################################################################################################
    kursdata3 <- kursdata1()  
    
    # Alerts ###################################################################################################
    if(is.null(input$qualdim) == TRUE | is.null(input$darstell) == TRUE | "noscale" %in% input$scaling) {
      createAlert(session, "jitteralert1", "JitterAlert1", title = "Bitte Auswahl treffen!",
                  content = "<ul>Bitte wähle
                  <li>mindestens eine <b>Qualitätsdimension</b></li>
                  <li>mindestens eine <b>Darstellungsoption</b> und</li>
                  <li>genau eine <b>Skalierungsoption</b> aus.</li>
                  Drücke dann nochmals den <i>Plot-Button.</i>
                  </ul>", append = FALSE)
    }
    
    if(is.null(input$qualdim) == FALSE & is.null(input$darstell) == FALSE &  !"noscale" %in% input$scaling) {
      closeAlert(session, "JitterAlert1")
    }
    
    
    # Setting up plot-data and plot-variables in dependence of input$groupin and input$scaling ########################
    
    #       |       | no group   |   gmean   | kurse     | gmean & kurse |
    #       |-------|------------|:---------:|-----------|---------------|
    #       | raw   | #jitter_10 | #jitter_1 | #jitter_2 | #jitter_3     |
    #       | grand | #jitter_11 | #jitter_4 | #jitter_5 | #jitter_6     |
    #       | group | #jitter_12 | #jitter_7 | #jitter_8 | #jitter_9     |
    #     
    # Plotsyntax if there are no scaling and no grand mean grouping options active
    p <- ggplot(kursdata1()[kursdata1()$kursleiterin == user() & kursdata1()$variable%in%input$qualdim,],         # jitter_2
                aes(x=variable, y= value))  + geom_blank() + facet_wrap(~  gmgroup, ncol = 3) 
    
    
    # Plotsyntax if there are GRANDmean scaling and no grand mean grouping options active      
    if ("std" %in% input$scaling)
      p <- ggplot(kursdata3[kursdata3$kursleiterin == user() & kursdata3$variable%in%input$qualdim,],       # jitter_11
                  aes(x=variable, y= value.std))  + geom_blank()    + facet_wrap(~  gmgroup, ncol = 3)  
    
    # Plotsyntax if there are GROUPmean ( = person-mean) scaling and no grand mean grouping options active  # jitter_12
    if ("gstd" %in% input$scaling)
      p <- ggplot(kursdata3[kursdata3$kursleiterin == user() & kursdata3$variable%in%input$qualdim,], 
                  aes(x=variable, y= value.pstd))  + geom_blank()    + facet_wrap(~  gmgroup, ncol = 3) 
    
    # Plotsyntax if only GRAND mean grouping options active                                                 # jitter_1
    if ("gmean" %in% input$groupin)
      p <- ggplot(kursdata3[kursdata3$variable%in%input$qualdim,], 
                  aes(x=variable, y= value))  + geom_blank()    + facet_wrap(~  gmgroup, ncol = 3)   
    
    
    # Plotsyntax if there are GRAND mean scaling AND grand mean grouping options active                     # jitter_4
    if ("gmean" %in% input$groupin & "std" %in% input$scaling)
      p <- ggplot(kursdata1(), 
                  aes(x=variable, y= value.std))  + geom_blank()    + facet_wrap(~  gmgroup, ncol = 3)   
    
    # Plotsyntax if there are GROUP mean scaling AND grand mean grouping options active                     # jitter_7
    if ("gmean" %in% input$groupin & "gstd" %in% input$scaling)
      p <- ggplot(kursdata3[kursdata3$variable%in%input$qualdim,], 
                  aes(x=variable, y= value.pstd))  + geom_blank()    + facet_wrap(~  gmgroup, ncol = 3)   
    
    # Grouping options (not grand mean)
    if ("kurse" %in% input$groupin)
      p <- p + facet_wrap(~ mggroup, ncol = 3)   
    
    if ("kurse" %in% input$groupin & "gmean" %in% input$groupin)
      p <- p + facet_wrap(~ mgxgmgroup, ncol = 3)   
    
    # Setting up geoms in dependence of input$darstell ################################################################
    
    if("boxplot" %in% input$darstell)
      p <- p + geom_boxplot(width = 0.35, colour = "#3997FA", fill = NA, outlier.shape = NA, size = 1.1)
    
    
    if("jitter" %in% input$darstell)
      p <- p + geom_jitter(shape = 1, position = position_jitter(w= 0.05, h = .0), alpha = 1/2, size = 2)
    
    
    if("ci" %in% input$darstell)
      p <- p + stat_summary(fun.data = "mean_cl_boot", colour = "#DC5C40", size = 1.3, geom = "errorbar", width = .08)
    
    if("mean" %in% input$darstell)
      p <- p + stat_sum_single(mean)
    
    
    
    
    # Axis labels in dependence of input$scalin #######################################################################
    
    if("std" %in% input$scaling)
      p <- p + geom_hline(yintercept = 0, colour = "#A51E37") +
        #annotate("text", x=0.6, y = -0.3, label="abiturma gesamt = 0", colour = "#A51E37", hjust = 0) + 
        ylab("Mittelwert abiturma = 0 (je Dimension)") +
        xlab("") +
        theme(axis.title.y = element_text(size = rel(1), colour = "#A51E37"),
              strip.text.x = element_text(size = 11),
              axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 11)) + 
        coord_cartesian(ylim=c(-4, 3))
    
    if("gstd" %in% input$scaling)
      p <- p + geom_hline(yintercept = 0, colour = "#A51E37") +
        #annotate("text", x=0.6, y = -0.55, label="Mittelwert je\nTeilnehmerIn = 0", colour = "#A51E37", hjust = 0, vjust = 0) + 
        ylab("Mittelwert je\nTeilnehmerIn = 0") + 
        xlab("") +
        theme(axis.title.y = element_text(size = rel(1), colour = "#A51E37"),
              strip.text.x = element_text(size = 11),
              axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 11)) + 
        coord_cartesian(ylim=c(-4, 3)) 
    
    
    if("raw" %in% input$scaling)
      p <- p + theme(axis.title.y = element_text(size = rel(1)),
                     strip.text.x = element_text(size = 11),
                     axis.text.x  = element_text(angle = 55, hjust = 1, colour = "black", size = 11)) + 
        ylim(1,7) +  labs(x = "", y = "1 = trifft überhaupt nicht zu...\n...7 = trifft vollständig zu")
    
    if("noscale" %in% input$scaling )
      p <- p + theme(axis.text.y = element_blank())
    
    p <- p + theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank())
    p
    })
  
  # Adaptive Plotheight for Jitter-Plot ###############################################################################
  
  n_lev_mg <- eventReactive(input$goqualdim, {nlevdata <- kursdata1()%>%                # Count of teached courses by actual user
    filter(kursleiterin == user())
  return(nlevels(as.factor(nlevdata$kurs)))})
  
  
  height_jitter <- eventReactive(input$goqualdim, {nlevmg <- n_lev_mg()                 # Calculate height in dependece of n_lev_mg()
  
  ifelse(isolate("kurse" %in% input$groupin & "gmean" %in% input$groupin),
         return(360*ceiling((as.numeric(nlevmg)+1)/3)),
         ifelse(isolate("kurse" %in% input$groupin & !"gmean" %in% input$groupin),
                return(360*ceiling((as.numeric(nlevmg))/3)),
                return(360))
  )
  })
  
  # Call of qualidimplotgone() mit Actionbutton  ######################################################################
  output$qualdimplot <- renderPlot({
    qualdimplotgone()
  }, res = 72)    
  
  # Rendering des Qualdimplots mit adaptiver Plothöhe  ################################################################
  output$jitter.plot.ui <- renderUI({
    plotOutput("qualdimplot", height = as.numeric(height_jitter()))
  })
  
  # Usage tracking jitterplot #########################################################################################
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) paste(input[[x]], collapse = "-"))
    
    data$systtime <- paste(Sys.time())
    data$user <- user()
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$goqualdim, {
    
    saveData(formData())
  })
  
  
  ##  debug non zero grand means
  
  
  #       output$debugjitterdata <- 
  #         
  # 
  #       DT::renderDataTable({
  #         
  #         kursdata1()%>%
  #           group_by(gmgroup, variable)%>%
  #           summarize(mw = mean(value.std, na.rm = T))
  #       })
  #       
  #     output$jitterplot2 <- renderPlot({
  #       ggplot(kursdata1(), 
  #              aes(x=variable, y= value.std))  + geom_blank()    + facet_wrap(~  gmgroup, ncol = 3) + stat_sum_single(mean)
  #     })
  
  #####################################################################################################################
  # Backend Feedbackforms                                                                                  ############
  #####################################################################################################################
  
  ## Feedback for Qualdim 1 page
  observeEvent(input$qualdim1_fb_btn, {
    reset("qualdim1_fb_inf")
    reset("qualdim1_fb_finf")
    reset("qualdim1_fb_fazit")
  })
  
  #####################################################################################################################
  # Einzelfragen Backend                                                                                   ############
  #####################################################################################################################
  
  # Variablenstring für subsetting definieren                        ##################################################
  
  items_lern <- c(
    "Du hast im Kurs etwas Nützliches gelernt.", #le1
    "Dein Interesse am Mathematik-Abistoff ist durch den Kurs gestiegen.", #le2
    "Du hast die Inhalte des Kurses verstanden.", #le3
    "Du fandest den Kurs herausfordernd und wurdest zum Denken angeregt.") #le4
  items_enth <- c(
    "Der/die Kursleiter/in unterrichtet mit Begeisterung.", #en1
    "Der/die Kursleiter/in ist dynamisch und unterrichtet voller Energie.", #en2
    "Der/die Kursleiter/in lockert den Unterricht durch Humor auf.", #en3
    "Der/die Kursleiter/in hält Dein Interesse während des Kurses durch seinen/ihren Unterrichtsstil aufrecht.") #en4
  items_orga <- c(
    "Die Erklärungen des Kursleiters/ der Kursleiterin sind verständlich.", #or1
    "Der/die Kursleiter/in ist gut vorbereitet und erklärt die Inhalte sorgfältig.", #or2
    "Du hast im Kurs einen Überblick über alle Abi-relevanten Themen erhalten.", #or3
    "Du hast im Kurs die Bearbeitung Abi-relevanter Aufgabentypen geübt."   , #or4
    "Du hast durch den Kurs Wissenslücken schließen können.") #or5
  items_grup <- c(
    "Die Kursteilnehmer/innen werden ermutigt, Fragen zu stellen.", #gi1
    "Die Kursteilnehmer/innen werden eingeladen, eigene Ideen und Lösungswege mitzuteilen.", #gi2
    "Die Kursteilnehmer/innen wurden ermutigt, Fragen zu stellen.", #gi3
    "Die Kursteilnehmer/innen werden ermutigt, eigene Lösungswege zu formulieren und/oder die vorgetragenen Lösungen kritisch zu hinterfragen.") #gi4
  items_indi <- c(
    "Der/die Kursleiter/in ist den Teilnehmenden gegenüber stets freundlich.", #ir1
    "Der/die Kursleiter/in gibt den Teilnehmenden das Gefühl, jederzeit um Hilfe bitten zu können.", #ir2
    "Der/die Kursleiter/in interessiert sich aufrichtig für die Teilnehmenden.") #ir3
  items_empf <- c(
    "Würdest Du den Kursleiter/die Kursleiterin einem Freund/einer Freundin weiterempfehlen?", #em1
    "Hättest Du dir für diesen Kurs einen anderen Kursleiter/eine andere Kursleiterin gewünscht?", #em2
    "Kurz vor dem Abitur bieten wir einen weiterführenden Kurs an, um Euch bestmöglich auf die Prüfung vorzubereiten. Wirst Du den abiturma Kurs im Frühjahr wieder besuchen?") #em3
  
  
  
  # Create reactive dataset for likert panel and subset it accoding to input$likertfragen #############################
  
  likertdata3 <- 
    eventReactive(input$golikert, { 
      
      # Alerts ###################################################################################################
      if(input$likertfragen == "nolikert") {
        createAlert(session, "likertalert1", "LikertAlert1", title = "Bitte Auswahl treffen!",
                    content = "<ul>Bitte wähle aus, welche Einzelfragen dargestellt werden sollen.
                    Drücke dann nochmals den <i>Plot-Button.</i></ul>", append = FALSE)
      }
      
      if(input$likertfragen != "nolikert") {
        closeAlert(session, "LikertAlert1")
      }
      
      
      # Data   ###################################################################################################
      
      if("Lernerfolg" %in% input$likertfragen){
        # withProgress(message = "Bereite Daten auf", value = 0, {
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"))%>%
          filter(variable %in% items_lern)
        #setProgress(value = 1, detail = "fertig")
        return(likertdata2)
        #})
      }
      
      if("Organisation" %in% input$likertfragen){
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"))%>%
          filter(variable %in% items_orga)
        return(likertdata2)
      }
      
      if("Enthusiasmus" %in% input$likertfragen){
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"))%>%
          filter(variable %in% items_enth)
        return(likertdata2)
      }
      
      if("Interaktion" %in% input$likertfragen){
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"),
                 mggroup     =  paste("Dein Kurs in", Kursort, "/n", Kursbeginn, Uhrzeit))%>%
          filter(variable %in% items_grup)
        return(likertdata2)
      }
      
      if("Beziehung" %in% input$likertfragen){
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"),
                 mggroup     =  paste("Dein Kurs in", Kursort, "/n", Kursbeginn, Uhrzeit))%>%
          filter(variable %in% items_indi)
        return(likertdata2)
      }
      
      if("Weiterempfehlung" %in% input$likertfragen){
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"))%>%
          filter(variable %in% items_empf)
        return(likertdata2)
      }
      
      })
  
  ## Debug  #################################################################################################
  
  output$glimpse_likertdata3 <- renderPrint({summary(freitextdata3())})
  output$user_p <- renderPrint({user()})
  output$pw_conf <- renderPrint({input$likertfragen})
  

  
  
  # Create Einzelplots #################################################################################################
  
  # Farbpalette
  cbPalette <- c("#A51E37", "#D8516A", "#FF849D", "#F8F8F8", "#95C3DF", "#497793", "#002A46")
  
  # Einzelplot ohne grouping
  output$einzelplot <- renderSvgPanZoom({                    # Da keine reactives enthalten sind
    likertdata4 <- likertdata3()%>%                          # bzw. likertdata3() via input$golikert
      mutate(gmgroup = factor(gmgroup))%>%                   # isoliert ist, kann dies hier voll reaktiv sein
             #variable = ifelse(variable == "Der/die Kursleiter/in hält Dein Interesse während des Kurses durch seinen/ihren Unterrichtsstil aufrecht.",
             #                  "Der/die Kursleiter/in hält Dein Interesse während des Kurses\ndurch seinen/ihren Unterrichtsstil aufrecht.",
             #                  ifelse(variable == "Die Kursteilnehmer/innen werden ermutigt, eigene Lösungswege zu formulieren und/oder die vorgetragenen Lösungen kritisch zu hinterfragen.",
             #                         "Die Kursteilnehmer/innen werden ermutigt, eigene Lösungswege zu\nformulieren und/oder die vorgetragenen Lösungen kritisch zu hinterfragen.",
             #                         ifelse(variable == "Der/die Kursleiter/in gibt den Teilnehmenden das Gefühl, jederzeit um Hilfe bitten zu können.",
             #                                "Der/die Kursleiter/in gibt den Teilnehmenden das Gefühl,\njederzeit um Hilfe bitten zu können.", 
             #                                ifelse(variable == "Die Kursteilnehmer/innen werden eingeladen, eigene Ideen und Lösungswege mitzuteilen.",
             #                                       "Die Kursteilnehmer/innen werden eingeladen, eigene\nIdeen und Lösungswege mitzuteilen.", variable)))))%>%
      filter(gmgroup == "Deine Kurse",
             is.na(value) == FALSE)
    
    testp <- ggplot(likertdata4, aes(x=gmgroup)) + geom_bar(aes(fill = value), width = .3) + 
             coord_flip() + facet_wrap(~variable, ncol =1)   + 
             ggtitle("Deine Kurse Frühjahr '16") +
             # scale_colour_manual(values=cbPalette) +
             scale_fill_manual(limits = c("1 = trifft überhaupt nicht zu", "2","3" ,"4","5" ,"6","7 = trifft vollständig zu"),
                               labels = c("1 = trifft überhaupt nicht zu", "2","3" ,"4","5" ,"6","7 = trifft vollständig zu"), values=cbPalette) +
             guides(fill = guide_legend(nrow = 1)) +
             theme(legend.title=element_blank(), legend.position = "top",
                   axis.ticks = element_blank(), axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   strip.text.x = element_text(size = 11),
                   axis.title.x = element_blank(), axis.title.y = element_blank(),
                   plot.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank())
    svgPanZoom(testp, controlIconsEnabled = T)
  })
  
  
  # Vergleichsplot
  gmeaneinzelplotgone <- eventReactive(input$golikert, {   # Verleichsplot wird als reaktive Variable
    if("gmean" %in% input$groupinl){                       # erstellt, da er durch die if-Bedingung 
      likertdata5 <- likertdata3()%>%                      # selbst reaktiv ist.
        mutate(gmgroup = factor(gmgroup),              # isoliert ist, kann dies hier voll reaktiv sein
               variable = ifelse(variable == "Der/die Kursleiter/in hält Dein Interesse während des Kurses durch seinen/ihren Unterrichtsstil aufrecht.",
                                 "Der/die Kursleiter/in hält Dein Interesse während des Kurses\ndurch seinen/ihren Unterrichtsstil aufrecht.",
                                 ifelse(variable == "Die Kursteilnehmer/innen werden ermutigt, eigene Lösungswege zu formulieren und/oder die vorgetragenen Lösungen kritisch zu hinterfragen.",
                                        "Die Kursteilnehmer/innen werden ermutigt, eigene Lösungswege zu\nformulieren und/oder die vorgetragenen Lösungen kritisch zu hinterfragen.",
                                        ifelse(variable == "Der/die Kursleiter/in gibt den Teilnehmenden das Gefühl, jederzeit um Hilfe bitten zu können.",
                                               "Der/die Kursleiter/in gibt den Teilnehmenden das Gefühl,\njederzeit um Hilfe bitten zu können.", 
                                               ifelse(variable == "Die Kursteilnehmer/innen werden eingeladen, eigene Ideen und Lösungswege mitzuteilen.",
                                                      "Die Kursteilnehmer/innen werden eingeladen, eigene\nIdeen und Lösungswege mitzuteilen.", variable)))))%>%
        filter(gmgroup == "abiturma gesamt",
               is.na(value) == FALSE)
      
      return(ggplot(likertdata5, aes(x=gmgroup)) + geom_bar(aes(fill = value), width = .3) + 
               coord_flip() + facet_wrap(~variable, ncol =1) +  
               ggtitle("abiturma Frühjahr '16") +
               scale_fill_manual(limits = c("1 = trifft überhaupt nicht zu", "2","3" ,"4","5" ,"6","7 = trifft vollständig zu"),
                                 labels = c("1 = trifft überhaupt nicht zu", "2","3" ,"4","5" ,"6","7 = trifft vollständig zu"), values=cbPalette) +
               guides(fill = guide_legend(nrow = 1)) + 
               #scale_fill_manual(values=cbPalette) +
               theme(legend.title=element_blank(), legend.position = "top",
                     axis.ticks = element_blank(), axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     strip.text.x = element_text(size = 11),
                     axis.title.x = element_blank(), axis.title.y = element_blank(),
                     plot.background = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank()))    
    }
  })
  
  output$gmeaneinzelplot <- renderPlot({                   # Reaktiver Aufruf des Renderings
    gmeaneinzelplotgone()
  })
  
  
  # Usage tracking likertplot ###########################################################################################
  
  # Whenever a field is filled, aggregate all form data
  formData_l <- reactive({
    data_l <- sapply(fields_l, function(x) input[[x]])
    data_l$systtime <- paste(Sys.time())
    data_l$user <- user()
    data_l
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$golikert, {
    saveData_l(formData_l())
  })
  
  
  #####################################################################################################################
  # Qualdim2 Backend                                                                                       ############ 
  #####################################################################################################################
  
  qualdim2plotgone <- eventReactive(input$goqualdim2,{     
    
    # Data fetching ###################################################################################################
    
    kursdata8 <-
      kursdata%>%
      mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"),
             mggroup     =  paste("Dein Kurs in ", Kursort, "\n", "Kursbeginn: ", Kursbeginn, ", ", Uhrzeit, sep = ""),
             mgxgmgroup  =  ifelse(kursleiterin == user(), paste("Dein Kurs in ", Kursort, ", Kursbeginn: ", "\n", 
                                                                 Kursbeginn, ", ", Uhrzeit, sep = "")
                                   , "abiturma gesamt"),
             variable    =  factor(variable))
    
    
    
    # Alerts ###################################################################################################
    if(input$qualdim2 == "noqualdim2") {
      createAlert(session, "qualdim2alert1", "Qualdim2Alert1", title = "Bitte Auswahl treffen!",
                  content = "<ul>Bitte Auswahl treffen. Drücke dann nochmals den <i>Plot-Button.</i>
                  </ul>", append = FALSE)
    }
    
    if(input$qualdim2 != "noqualdim2") {
      closeAlert(session, "Qualdim2Alert1")
    }
    
    
    # Plot für Kursvergleich ###################################################################################
    if ("kurse" %in% input$qualdim2){
      p <- ggplot(kursdata8%>%filter(gmgroup == "Deine Kurse"),         
                  aes(x=variable, y= value, fill = mggroup)) + geom_boxplot(outlier.shape = NA) + 
        geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.2))
      
      p <- p + theme(axis.text.x  = element_text(angle = 55, hjust = 1, colour = "black", size = 11),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x = element_blank(),
                     legend.title=element_blank(),
                     legend.text = element_text(size = 11)) + 
        
        coord_cartesian(ylim=c(0.8, 7.2)) +  labs(x = "", y = "1 = trifft überhaupt nicht zu...\n...7 = trifft vollständig zu") + scale_y_continuous(breaks=c(1:7)) +
        guides(fill=guide_legend(
          keywidth=0.3,
          keyheight=0.6,
          default.unit="inch")
        )  
    }
    
    # Plot für Überblick ####################################################################################
    if ("ueber" %in% input$qualdim2){
      p <- ggplot(kursdata8%>%filter(gmgroup == "Deine Kurse"),         
                  aes(x=variable, y= value)) + geom_boxplot(width = 0.35, colour = "#3997FA", fill = NA, outlier.shape = NA, size = 1.1) + 
        geom_jitter(pch = 21, width = 0.1)
      
      p <- p + theme(axis.text.x  = element_text(angle = 55, hjust = 1, colour = "black", size = 11),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x = element_blank()) +
        coord_cartesian(ylim=c(0.8, 7.2)) +  labs(y = "1 = trifft überhaupt nicht zu...\n...7 = trifft vollständig zu", x = "") + scale_y_continuous(breaks=c(1:7))
    } 
    
    # Plot für Stärken ####################################################################################
    if ("staerken" %in% input$qualdim2){
      p <- ggplot(kursdata8%>%filter(gmgroup == "Deine Kurse"),         
                  aes(x=variable, y= value.pstd, fill = mggroup)) + geom_boxplot(outlier.shape = NA) + 
        geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.2)) +
        geom_hline(yintercept = 0, colour = "#A51E37")
      
      
      p <- p + theme(axis.text.x  = element_text(angle = 55, hjust = 1, colour = "black", size = 11),
                     axis.title.y  = element_text(colour = "#A51E37", size = rel(1.15)),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x = element_blank(),
                     legend.title=element_blank(),
                     legend.text = element_text(size = 11)) + 
        
        coord_cartesian(ylim=c(-4, 3)) +  labs(x = "", y = "Mittelwert je TeilnehmerIn = 0") + scale_y_continuous(breaks=c(-4:3)) +
        guides(fill=guide_legend(
          keywidth=0.3,
          keyheight=0.6,
          default.unit="inch")
        )  
    }      
    
    # Plot für gmean ####################################################################################
    if ("gmean" %in% input$qualdim2){
      p <- ggplot(kursdata8%>%filter(gmgroup == "Deine Kurse"),         
                  aes(x=variable, y= value.std)) + 
        stat_summary(fun.data = "mean_cl_boot", colour = "#DC5C40", size = 1.3, geom = "errorbar", width = .08)  + 
        stat_summary(fun.data = "mean_cl_boot", geom = "point", colour = "red", size = 3) +
        geom_jitter(pch = 21, width = 0.1) +
        geom_hline(yintercept = 0, colour = "#A51E37")
      
      p <- p + theme(axis.title.y  = element_text(colour = "#A51E37", size = rel(1.15)),
                     axis.text.x  = element_text(angle = 55, hjust = 1, colour = "black", size = 11),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x = element_blank(),
                     legend.title=element_blank(),
                     legend.text = element_text(size = 11)) + labs(x = "", y = "Mittelwert abiturma = 0 (je Dimension)") +
        guides(fill=guide_legend(
          keywidth=0.3,
          keyheight=0.6,
          default.unit="inch")
        )  
    }  
    
    p
    
    })
  
  
  # Call of qualidim2plotgone() & Interprethilfe  mit Actionbutton  ####################################################
  output$qualdim2plot <- renderPlot({
    qualdim2plotgone()
  }, res = 72)  
  
  
  # Reaktive Interpretationshilfe erstellen       ######################################################################
  
  # .md String nicht-reaktiv generieren
  
  
  #   output$interpretationshilfe.ui <- 
  #   #  observeEvent(input$goqualdim2, {
  #     renderUI({
  #    
  #     if("kurse" %in% input$qualdim2)
  #       interpret <- withMathJax(includeMarkdown("Stuff/Helpstuff/interpret_kurse.md"))
  #     if("ueber" %in% input$qualdim2)
  #       interpret <- withMathJax(includeMarkdown("Stuff/Helpstuff/interpret_ueber.md"))
  #     if("staerken" %in% input$qualdim2)
  #       interpret <- withMathJax(includeMarkdown("Stuff/Helpstuff/interpret_staerken.md"))
  #     if("gmean" %in% input$qualdim2)
  #       interpret <- withMathJax(includeMarkdown("Stuff/Helpstuff/interpret_gmean.md"))
  #     
  #     
  #     interpret
  #     })
  # })
  
  
  ## Debug qualdim2 tracking
  
  output$debug1 <- renderPrint({formData_q2()})
  output$debug2 <- renderPrint({formData_l()})
  
  
  
  #   # Help rendering              #########################################################################################
  #   
  #   output$darstell_help_im <- renderImage({
  #      
  #       filename_help_im <- normalizePath(file.path('./Stuff/Helpstuff/darstell_modal_im2.png'))
  #       
  #       # Return a list containing the filename and alt text
  #       list(src = filename_help_im,
  #            alt = paste("Image number"),
  #   
  #            height = "100%",
  #            style= "display: block; margin-left: auto; margin-right: auto;")
  #       
  #     }, deleteFile = FALSE)
  
  
  # Usage tracking qualdimplot2 #########################################################################################
  
  # Whenever a field is filled, aggregate all form data
  formData_q2 <- reactive({
    data_q2 <- sapply(fields_q2, function(x) paste(input[[x]], collapse = "-"))
    
    data_q2$systtime <- paste(Sys.time())
    data_q2$user <- user()
    data_q2
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$goqualdim2, {
    
    saveData_q2(formData_q2())
  })
  
  
  })





