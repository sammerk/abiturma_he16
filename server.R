library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(shinyBS)
library(feather)
library(scrypt)
library(forcats)




# Ip-Test ##################################################################################
Sys.setenv(TZ='GMT+4')
ipID <- renderPrint(print(input$ipid))

# Data Import ##############################################################################

#kursdata <- read.table("data/kursdata_fr16_b_demo.csv", sep = ";", header = TRUE, na.strings = c("NA"))       # Import of Data
kursdata <- data.table::fread("data/data_dynamic/kursdata_inkrementiert.csv", sep = ";", header = TRUE, na.strings = c("NA")) 

#freitextdata <- read.table("data/freitextdata_fr16_demo.csv", sep = ";", header = T, na.strings = c("NA"))
freitextdata <- data.table::fread("data/data_dynamic/freitextdata_inkrementiert.csv", sep = ";", header = TRUE, na.strings = c("NA"), stringsAsFactors = T) 

#likertdata1 <- read.table("data/likertdata_fr16_2_demo.csv", sep = ";", header = T, na.strings = c("NA"))
likertdata1 <-  data.table::fread("data/data_dynamic/likertdata_inkrementiert.csv", sep = ";", header = T, na.strings = c("NA"))

#pw_data2 <- read_feather("Stuff/kl_pw.feather")
pw_data <- tbl_df(data.table::fread("data/data_kl/data_pw_scrypted.csv", sep = ";", na.strings = "NA"))

##########################################################################################
# Custom Functions ###
##########################################################################################

# Function for mean (jitter plot panel) ##################################################

stat_sum_single <- function(fun, geom="point", ...) {                     
  stat_summary(fun.y=fun, colour="#DC5C40", geom=geom, size = 3.0, ...)
}




# Functions for usage tracking of jitter Navbarpage #######################################

fields <- c("qualdim", "darstell", "scaling", "groupin", "ipid", "q1_inf_stars", "q1_sic_stars")   # names of fields to track

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

fields_l <- c("likertfragen", "ipid", "groupinl", "likert_inf_stars", "likert_sic_stars")   # names of fields to track
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

fields_f <- c("sort_freitexte", "ipid", "frei_inf_stars")   # names of fields to track
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
fields_q2 <- c("qualdim2", "ipid", "q2_inf_stars", "q2_sic_stars")   # names of fields to track
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


# Function for Feedback recording of jitter navbarpage #####################################

fields_fb_q1 <- c("q1_inf_stars", "q1_sic_stars")   # names of fields to track
outputDir_fb_q1 <- "responses_fb_q1"

saveData_fb_q1 <- function(data_fb_q1) {
  data_fb_q1 <- t(data_fb_q1)
  # Create a unique file name
  fileName_fb_q1 <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data_fb_q1))
  # Write the file to the local system
  write.csv(
    x = data_fb_q1,
    file = file.path(outputDir_fb_q1, fileName_fb_q1), 
    row.names = FALSE, quote = TRUE
  )
}


# Function for Feedback recording of likert navbarpage #####################################

fields_fb_likert <- c("likert_inf_stars", "likert_sic_stars")   # names of fields to track
outputDir_fb_likert <- "responses_fb_likert"

saveData_fb_likert <- function(data_fb_likert) {
  data_fb_likert <- t(data_fb_likert)
  # Create a unique file name
  fileName_fb_likert <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data_fb_likert))
  # Write the file to the local system
  write.csv(
    x = data_fb_likert,
    file = file.path(outputDir_fb_likert, fileName_fb_likert), 
    row.names = FALSE, quote = TRUE
  )
}


# Function for Feedback recording of jitter v2 navbarpage #####################################

fields_fb_q2 <- c("q2_inf_stars", "q2_sic_stars")   # names of fields to track
outputDir_fb_q2 <- "responses_fb_q2"

saveData_fb_q2 <- function(data_fb_q2) {
  data_fb_q2 <- t(data_fb_q2)
  # Create a unique file name
  fileName_fb_q2 <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data_fb_q2))
  # Write the file to the local system
  write.csv(
    x = data_fb_q2,
    file = file.path(outputDir_fb_q2, fileName_fb_q2), 
    row.names = FALSE, quote = TRUE
  )
}


# Function for Feedback recording of logout navbarpage #####################################

fields_fb_logout <- c("glob_fb_likert_inf", "glob_fb_q1_inf", "glob_fb_q2_inf", "glob_fb_frei_inf", "glob_fb_erk", "glob_fb_abiturma")   # names of fields to track
outputDir_fb_logout <- "responses_fb_logout"

saveData_fb_logout <- function(data_fb_logout) {
  data_fb_logout <- t(data_fb_logout)
  # Create a unique file name
  fileName_fb_logout <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data_fb_logout))
  # Write the file to the local system
  write.csv(
    x = data_fb_logout,
    file = file.path(outputDir_fb_logout, fileName_fb_logout), 
    row.names = FALSE, quote = TRUE
  )
}

# Function for Feedback recording of freitextpage #####################################

fields_fb_frei <- c("frei_inf_stars")   # names of fields to track
outputDir_fb_frei <- "responses_fb_frei"

saveData_fb_frei <- function(data_fb_frei) {
  data_fb_frei <- t(data_fb_frei)
  # Create a unique file name
  fileName_fb_frei <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data_fb_frei))
  # Write the file to the local system
  write.csv(
    x = data_fb_frei,
    file = file.path(outputDir_fb_frei, fileName_fb_frei), 
    row.names = FALSE, quote = TRUE
  )
}


# Function for recording of helpbutton-clicks #####################################

fields_help <- c("darstellmodalbt", "jitterqualdimmodalbt", "scalemodalbt", "help_table_bt", "help_scale_bt", 
                 "help_gmean", "help_q2_grandmean", "help_darst_st", "help_q2_groupmean", "help_restq2")

outputDir_help <- "responses_help"

saveData_help <- function(data_help) {
  data_help <- t(data_help)
  # Create a unique file name
  fileName_help <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data_help))
  # Write the file to the local system
  write.csv(
    x = data_help,
    file = file.path(outputDir_help, fileName_help), 
    row.names = FALSE, quote = TRUE
  )
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
  # Hide loading message                                                                                   ############
  #####################################################################################################################  
  
  # Hide the loading message when the rest of the server function has executed
  # hide(id = "loading-content", anim = TRUE, animType = "fade")    
  # show("app-content")
  
  
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
   
   # Show hide fuctions of LoginAlerts and plots
   observeEvent(login_true(),{
     if(login_true() == F){
       hide("likert-plot-container")
       hide("qualdimplot1-container")
       hide("plot-container")
       hide("freitext-container")
       show("loginwarning")
       show("loginwarning2")
       show("loginwarning3")
       show("loginwarning4")}
     
     if(login_true() == T){
       hide("loginwarning") 
       hide("loginwarning2")
       hide("loginwarning3")
       hide("loginwarning4")}
     
     
   })
   
   observeEvent(input$gofreitext,{
     show("freitext-container")
     show("frei_form", anim = T, animType = "fade", time = 1)
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
          filter(kursleiterin == user(),
                 ftk != "data/freitextbilder_questor4/",
                 ftk != "rawdata_fruehjahr17_charge2f/freitextbilder/")%>%                         ### Aktualisieren 
          arrange(desc(score))
      }
      
      
      if(input$sort_freitexte == "nsort" | is.null(input$sort_freitexte) == TRUE){
        freitextdata1 <-
          freitextdata%>%
          filter(kursleiterin == user(),
                 ftk != "data/freitextbilder_questor4/",
                 ftk != "rawdata_fruehjahr17_charge2f/freitextbilder/")                            ### Aktualisieren 
      }
      
      
      return(freitextdata1)
      
      
    })

  ## 
  max_freitextplots <- 70
  
  
  
  ## Freitextplotschleife #############################################################################################
  
  freitextplotgone <- eventReactive(input$gofreitext,{  
    

    plot_output_list <- lapply(1:length((freitextdata2()$ftk)), function(i) {
      plotname <- paste("freitextplot", i, sep="")
      plotOutput(plotname, height = "165px")
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
             alt = paste("alt text"),
             width = "100%"
        )
        
      })
      
      
    })
  }
  
  ## Rendering FreitextUI #############################################################################################
  output$freitextplots <- renderUI({
    if(login_true() == T)
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
  
  
  ######################################################################################################################
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
  
  
  # Kursanzahl extrahieren für adaptive plotheight ####################################################################  
  n_lev_mg <- eventReactive(input$golikert, {nlevdata <- kursdata1()%>%      #Count of teached courses by actual user
    filter(kursleiterin == user())
  return(nlevels(as.factor(nlevdata$kurs)))})
  
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
  
 # # Qualdimplot as svg ###############################################################################
 # 
 # list_of_qualdimplot <- eventReactive(input$goqualdim, {
 #                              width_q1  <- session$clientData$output_qualdimplot_svg_width
 #                              height_q1 <- session$clientData$output_qualdimplot_svg_height
 #                              mysvgwidth_q1 <- width_q1/96
 #                              mysvgheight_q1 <- height_q1/96*ifelse("kurse" %in% input$groupin & "gmean" %in% input$groupin,
 #                                                                   ceiling((as.numeric(n_lev_mg())+1)/3),
 #                                                                 ifelse("kurse" %in% input$groupin & !"gmean" %in% input$groupin,
 #                                                                        ceiling((as.numeric(n_lev_mg()))/3),
 #                                                                        1
 #                                                                        )
 #                                                                   )
 #                              
 #                              # A temp file to save the output.
 #                              # This file will be removed later by renderImage
 #                              
 #                              outfile_q1 <- tempfile(fileext='.svg')
 #                              
 #                              #This actually save the plot in a image
 #                              ggsave(file=outfile_q1, plot=qualdimplotgone(), width=mysvgwidth_q1, height=mysvgheight_q1)
 #                              
 #                              # Return a list containing the filename
 #                              list(src = normalizePath(outfile_q1),
 #                                   contentType = 'image/svg+xml',
 #                                   width = width_q1,
 #                                   height = height_q1*ifelse("kurse" %in% input$groupin & "gmean" %in% input$groupin,
 #                                                             ceiling((as.numeric(n_lev_mg())+1)/3),
 #                                                             ifelse("kurse" %in% input$groupin & !"gmean" %in% input$groupin,
 #                                                                    ceiling((as.numeric(n_lev_mg()))/3),
 #                                                                    1
 #                                                                    )
 #                                                             ),
 #                                   alt = "My svg Histogram")
 # })
 # 
 # output$qualdimplot_svg <- renderImage({
 #     list_of_qualdimplot()
 # }) 
  
  # show qualdimplot1spinner
  
  observeEvent(input$goqualdim, {
    show("qualdimplot1-container")
    show("q1_star_wellpanel", anim = T, animType = "fade", time = 1)
  })
  
  # Call of qualidimplotgone() mit Actionbutton  ######################################################################
  output$qualdimplot <- renderPlot({
    if(login_true() == T)
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
  
  

  #####################################################################################################################
  # Backend Feedbackforms                                                                                  ############
  #####################################################################################################################
  
  ## Feedback for likert page ######################################################################################
  
  # Reset likert form ###########################################
  observeEvent(input$golikert, {
    show("likert_star_wellpanel", anim = T, animType = "fade", time = 1)
    reset("likert_inf_stars")
    reset("likert_sic_stars")
  })
  
  # Write Feedback likert ##################################
  
  # Whenever a field is filled, aggregate all form data
  formData_fb_likert <- reactive({
    data_fb_likert <- sapply(fields_fb_likert, function(x) input[[x]])
    data_fb_likert$systtime <- paste(Sys.time())
    data_fb_likert$user <- user()
    data_fb_likert
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$likert_fb_btn, {
    saveData_fb_likert(formData_fb_likert())
    reset("likert_inf_stars")
    reset("likert_sic_stars")
  })
  
  
  ## Feedback for Qualdim 1 page ######################################################################################
  
  # Reset q1 ###########################################
  observeEvent(input$qualdim1_fb_btn, {
    reset("q1_inf_stars")
    reset("q1_sic_stars")
  })
  observeEvent(input$goqualdim, {
    reset("q1_inf_stars")
    reset("q1_sic_stars")
  })
  
  # Write Feedback q1 ##################################
  
  # Whenever a field is filled, aggregate all form data
  formData_fb_q1 <- reactive({
    data_fb_q1 <- sapply(fields_fb_q1, function(x) input[[x]])
    data_fb_q1$systtime <- paste(Sys.time())
    data_fb_q1$user <- user()
    data_fb_q1
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$qualdim1_fb_btn, {
    saveData_fb_q1(formData_fb_q1())
  })
  
  
  ## Feedback for Qualdim 2 page ######################################################################################
  
  # Reset q2 ###########################################
  observeEvent(input$qualdim2_fb_btn, {
    reset("q2_form")
  })
   observeEvent(input$goqualdim2, {
     reset("q2_form")
   })
  
  # Write Feedback q2 ##################################
  
  # Whenever a field is filled, aggregate all form data
  formData_fb_q2 <- reactive({
    data_fb_q2 <- sapply(fields_fb_q2, function(x) input[[x]])
    data_fb_q2$systtime <- paste(Sys.time())
    data_fb_q2$user <- user()
    data_fb_q2
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$qualdim2_fb_btn, {
    saveData_fb_q2(formData_fb_q2())
  })
  
  
  ## Feedback for Freitext page ######################################################################################
  
  # Reset q2 ###########################################
  observeEvent(input$frei_fb_btn, {
    reset("frei_inf_stars")
  })
  observeEvent(input$gofreitext, {
    reset("frei_inf_stars")
  })
  
  # Write Feedback frei ##################################
  
  # Whenever a field is filled, aggregate all form data
  formData_fb_frei <- reactive({
    data_fb_frei <- sapply(fields_fb_frei, function(x) input[[x]])
    data_fb_frei$systtime <- paste(Sys.time())
    data_fb_frei$user <- user()
    data_fb_frei
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$frei_fb_btn, {
    saveData_fb_frei(formData_fb_frei())
  })
  
  
  # Write help button usage ##################################
  
  # Whenever a field is filled, aggregate all form data
  formData_help <- reactive({
    data_help <- sapply(fields_help, function(x) input[[x]])
    data_help$systtime <- paste(Sys.time())
    data_help$user <- user()
    data_help
  })
  
  # When the Submit button is clicked, save the form data   ## 
  # observeEvent(input$logout_btn, {                        ##  Code transfered to line ~ 855 where switchoff is iplemented 
  #   saveData_help(formData_help())                        ##    
  # })                                                      ##    
  
  
  
  ## Feedback for logout page ######################################################################################
  
  # Write Feedback logout ##################################
  
  # Whenever a field is filled, aggregate all form data
  formData_fb_logout <- reactive({
    data_fb_logout <- sapply(fields_fb_logout, function(x) input[[x]])
    data_fb_logout$systtime <- paste(Sys.time())
    data_fb_logout$user <- user()
    data_fb_logout
  })
  
  # When the Submit button is clicked, save the form data, terminate app and close window

     observeEvent(input$logout_btn, {
       saveData_help(formData_help())
       saveData_fb_logout(formData_fb_logout())
       js$closeWindow()
       stopApp()
  })
     
     # Save logut-form through all PlotButton (to track non-logout-people)
     
     observeEvent(input$golikert, {
       saveData_help(formData_help())
     })
     observeEvent(input$goqualdim, {
       saveData_help(formData_help())
     })
#     observeEvent(input$goqualdim2, {
#       saveData_help(formData_help())
#     })
     observeEvent(input$gofreitext, {
       saveData_help(formData_help())
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
    "Der/die Kursleiter/in ermutigt die Teilnehmenden, an den Diskussionen im Kurs teilzunehmen.", #gi1
    "Die Kursteilnehmer/innen werden eingeladen, eigene Ideen und Lösungswege mitzuteilen.", #gi2
    "Die Kursteilnehmer/innen werden ermutigt, Fragen zu stellen.", #gi3
    "Die Kursteilnehmer/innen werden ermutigt, eigene Lösungswege zu formulieren und/oder die vorgetragenen Lösungen kritisch zu hinterfragen.") #gi4
  items_indi <- c(
    "Der/die Kursleiter/in ist den Teilnehmenden gegenüber stets freundlich.", #ir1
    "Der/die Kursleiter/in gibt den Teilnehmenden das Gefühl, jederzeit um Hilfe bitten zu können.", #ir2
    "Der/die Kursleiter/in interessiert sich aufrichtig für die Teilnehmenden.") #ir3
  items_empf <- c(
    "Würdest Du den Kursleiter/die Kursleiterin einem Freund/einer Freundin weiterempfehlen?", #em1
    "Hättest Du dir für diesen Kurs einen anderen Kursleiter/eine andere Kursleiterin gewünscht?", #em2
    "Kurz vor dem Abitur bieten wir einen weiterführenden Kurs an, um Euch bestmöglich auf die Prüfung vorzubereiten. Wirst Du den abiturma Kurs im Frühjahr wieder besuchen?") #em3
  
  
  
  # Create reactive dataset for likert panel and subset it according to input$likertfragen #############################
  
  likertdata3 <- 
    eventReactive(input$golikert, { 
      # Alerts ###################################################################################################
      
      ## none-selection alert
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
       
        #})
      }
      
      if("Organisation" %in% input$likertfragen){
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"))%>%
          filter(variable %in% items_orga)
        
      }
      
      if("Enthusiasmus" %in% input$likertfragen){
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"))%>%
          filter(variable %in% items_enth)
        
      }
      
      if("Interaktion" %in% input$likertfragen){
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"),
                 mggroup     =  paste("Dein Kurs in", Kursort, "/n", Kursbeginn, Uhrzeit))%>%
          filter(variable %in% items_grup)%>%
          mutate(variable = fct_recode(variable,
                                       "Die Kursteilnehmer/innen werden ermutigt, eigene Lösungswege zu formulieren und/oder\ndie vorgetragenen Lösungen kritisch zu hinterfragen." = "Die Kursteilnehmer/innen werden ermutigt, eigene Lösungswege zu formulieren und/oder die vorgetragenen Lösungen kritisch zu hinterfragen."))
        
      }
      
      if("Beziehung" %in% input$likertfragen){
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"),
                 mggroup     =  paste("Dein Kurs in", Kursort, "/n", Kursbeginn, Uhrzeit))%>%
          filter(variable %in% items_indi)
        
      }
      
      if("Weiterempfehlung" %in% input$likertfragen){
        likertdata2 <- likertdata1%>%
          filter(Kursbeginn != "Herbst '15")%>%
          mutate(gmgroup     =  ifelse(kursleiterin == user(), "Deine Kurse", "abiturma gesamt"))%>%
          filter(variable %in% items_empf)
        
      }
      
    #  return(likertdata2)
      
      
      
       likertdata2b <- likertdata2%>%
          filter(is.na(value)==F)%>%
          group_by(value, gmgroup, variable)%>%
          summarize(Freq = n())%>%
          ungroup()%>%
          group_by(gmgroup, variable)%>%
          mutate(Freq_per = Freq/sum(Freq, na.rm = T)*100)%>%
          ungroup()
      
      
    if(!"gmean" %in% input$groupinl){
       likertdata2b <-likertdata2b%>%
       filter(gmgroup == "Deine Kurse")
       }
       
      return(likertdata2b)
       
      })
  

  ## reactive Data frame with info for plot title
  
  likertdata_with_userinfo <- eventReactive(input$golikert, { 
                               likertdata1%>%
                                  mutate(Kursbeginn = lubridate::ymd(Kursbeginn))%>%
                                  filter(kursleiterin == user())%>%
                                  filter(Kursbeginn == max(Kursbeginn, na.rm = T))
                                  
    })
    
  
  
  ## Debug  #################################################################################################
  
 output$glimpse_likertdata3 <- renderPrint({glimpse(likertdata_with_userinfo())})
 #output$user_p <- renderPrint({login_true()})
 #output$pw_conf <- renderPrint({input$likertfragen})
  

  
  
  # Create Einzelplots #################################################################################################
  
  # Colorpalette
  cbPalette <- c("#A51E37", "#D8516A", "#FF849D", "#D9D9D9", "#95C3DF", "#497793", "#002A46")
  
  # Einzelplot with reactive grouping
  einzelplot_s <- eventReactive(input$golikert, {                   
       likertplot <- ggplot(likertdata3(), aes(x=gmgroup, y = Freq_per, fill = value)) + geom_bar(stat='identity') + 
             coord_flip() + facet_wrap(~variable, ncol =1)   + 
             ggtitle(paste("Deine Kurse in ", likertdata_with_userinfo()$Kursort[1],"\n Kursbeginn: ", likertdata_with_userinfo()$Kursbeginn[1], sep = "")) +
             scale_fill_manual(limits = c("1 = trifft überhaupt nicht zu", "2","3" ,"4","5" ,"6","7 = trifft vollständig zu"),
                               labels = c("1 = trifft überhaupt nicht zu", "2","3" ,"4","5" ,"6","7 = trifft vollständig zu"), values = cbPalette) +
             guides(fill = guide_legend(nrow = 1)) +
             theme(legend.title=element_blank(), legend.position = "top",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   strip.text.x = element_text(size = 9, lineheight=1.1),
                   plot.background = element_blank(),
                   panel.background = element_rect(fill = '#FAFAFA'),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   plot.title = element_text(lineheight=1.1))
    
       likertplot
    
  })
  
  
  list_of_likertplot <- eventReactive(input$golikert, {
                              width_l  <- session$clientData$output_einzelplot_width
                              height_l <- session$clientData$output_einzelplot_height
                              mysvgwidth_l <- width_l/96*1.35
                              mysvgheight_l <- height_l/96*1.35*ifelse(input$likertfragen == "Beziehung",0.7,1)
                              
                              # A temp file to save the output.
                              # This file will be removed later by renderImage
                              
                              outfile_l <- tempfile(fileext='.svg')
                              
                              #This actually save the plot in a image
                              ggsave(file=outfile_l, plot=einzelplot_s(), width=mysvgwidth_l, height=mysvgheight_l)
                              
                              # Return a list containing the filename
                              list(src = normalizePath(outfile_l),
                                   contentType = 'image/svg+xml',
                                   width = width_l,
                                   height = height_l,
                                   alt = "My svg Histogram")
  })
  
  observeEvent(input$golikert,{
    if(login_true() == T)
    show("likert-plot-container")    # to override initial "hidden" value
  })

  
  output$einzelplot <- renderImage({
    list_of_likertplot()
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
  
  observeEvent(input$goqualdim2,{
    show("plot-container")
    show("q2_form", anim = T, animType = "fade", time = 1)
  })
  
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
      
      p <- p + theme(axis.text.x  = element_text(angle = 15, hjust = 1, colour = "black", size = 7),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x = element_blank(),
                     legend.title=element_blank(),
                     legend.text = element_text(size = 6, lineheight = 1.3),
                     axis.title.y  = element_text(colour = "#A51E37", size = 7, lineheight = 1.3)) + 
        
        coord_cartesian(ylim=c(0.8, 7.2)) +  labs(x = "", y = "1 = trifft überhaupt nicht zu...\n...7 = trifft vollständig zu") + scale_y_continuous(breaks=c(1:7)) +
        guides(fill=guide_legend(
          keywidth=0.15,
          keyheight=0.3,
          default.unit="inch")
        )  
    }
    
    # Plot für Überblick ####################################################################################
    if ("ueber" %in% input$qualdim2){
      p <- ggplot(kursdata8%>%filter(gmgroup == "Deine Kurse"),         
                  aes(x=variable, y= value)) + geom_boxplot(width = 0.35, colour = "#3997FA", fill = NA, outlier.shape = NA, size = 1.1) + 
        geom_jitter(pch = 21, width = 0.1)
      
      p <- p + theme(axis.text.x  = element_text(angle = 15, hjust = 1, colour = "black", size = 7),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x = element_blank(),
                     axis.title.y  = element_text(colour = "#A51E37", size = 7, lineheight = 1.3)) +
        coord_cartesian(ylim=c(0.8, 7.2)) +  labs(y = "1 = trifft überhaupt nicht zu...\n...7 = trifft vollständig zu", x = "") + scale_y_continuous(breaks=c(1:7))
    } 
    
    # Plot für Stärken ####################################################################################
    if ("staerken" %in% input$qualdim2){
      p <- ggplot(kursdata8%>%filter(gmgroup == "Deine Kurse"),         
                  aes(x=variable, y= value.pstd, fill = mggroup)) + geom_boxplot(outlier.shape = NA) + 
        geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.2)) +
        geom_hline(yintercept = 0, colour = "#A51E37")
      
      
      p <- p + theme(axis.text.x  = element_text(angle = 15, hjust = 1, colour = "black", size = 7),
                     axis.title.y  = element_text(colour = "#A51E37", size = 7),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x = element_blank(),
                     legend.title=element_blank(),
                     legend.text = element_text(size = 6, lineheight = 1.2)) + 
        
        coord_cartesian(ylim=c(-4, 3)) +  labs(x = "", y = "Mittelwert je TeilnehmerIn = 0") + scale_y_continuous(breaks=c(-4:3)) +
        guides(fill=guide_legend(
          keywidth=0.15,
          keyheight=0.3,
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
      
      p <- p + theme(axis.title.y  = element_text(colour = "#A51E37", size = 7),
                     axis.text.x  = element_text(angle = 15, hjust = 1, colour = "black", size = 7),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x = element_blank(),
                     legend.title=element_blank(),
                     legend.text = element_text(size = 7)) + labs(x = "", y = "Mittelwert abiturma = 0 (je Dimension)") +
        guides(fill=guide_legend(
          keywidth=0.1,
          keyheight=0.15,
          default.unit="inch")
        )  
    }  
    
    # Create .svg as tempfile ############################################################################
    # fetch clientdata
    width  <- session$clientData$output_qualdim2plot_width
    height <- session$clientData$output_qualdim2plot_height
    mysvgwidth <-  width/96
    mysvgheight <- height/96
    pixelratio <- session$clientData$pixelratio
    
    # A temp file to save the output.
    qualdimplot2_temp <- tempfile(fileext='.svg')
    
    # Generate the svg
    ggsave(file=qualdimplot2_temp, plot=p, width=mysvgwidth*pixelratio, height=mysvgheight*pixelratio)
    
    # Return a list containing the filename
    list(src = normalizePath(qualdimplot2_temp),
         contentType = 'image/svg+xml',
         width = width,
         height = height,
         alt = "My svg Histogram")
    })
  
  
  # Call of qualidim2plotgone() & Interprethilfe  mit Actionbutton  ####################################################
  output$qualdim2plot <- renderImage({
    if(login_true() == T)
    qualdim2plotgone()
  })  
  
  
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





