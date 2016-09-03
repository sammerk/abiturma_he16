library(shiny)
library(shinyBS)
library(shinyjs)
library(svgPanZoom)
library(gridSVG)
source("helpers.R")


#####################################################################################
# js Code for: Enter key = simulation of button press                    ############
#####################################################################################
jscode <- '
$(function() {
var $els = $("[data-proxy-click]");
$.each(
$els,
function(idx, el) {
var $el = $(el);
var $proxy = $("#" + $el.data("proxyClick"));
$el.keydown(function (e) {
if (e.keyCode == 13) {
$proxy.click();
}
});
}
);
});
'

#####################################################################################
# Calls für Fingerprint                                                  ############
#####################################################################################
inputUserid <- function(inputId, value='') {
  #   print(paste(inputId, "=", value))
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
  )
}

inputIp <- function(inputId, value=''){
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;"),
    singleton(tags$head(tags$script(src = "https://www.abiturma.de/unterrichtsfeedback_dateien/js/script.js", type='text/javascript'))), #$$
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://www.abiturma.de/unterrichtsfeedback_dateien/style.css")), #$$
    tags$head(tags$title("Unterrichtsfeedback")) #$$
  )
}

#####################################################################################
# ShinyUI                                                                ############
#####################################################################################

shinyUI(navbarPage("Unterrichtsfeedback",theme = "lumen.css", #$$
                   
                   ###########################################################################
                   
                   ############################################################################
                   tabPanel("Vorwort", 
                            fluidRow(class="splash",tags$div(id="splash")), #$$
                            fluidRow(
                              column(2),
                              column(7,
                                     includeMarkdown("Stuff/hallo4.md"))),
                            fluidRow(
                              useShinyjs(),
                              tags$style(appCSS), ## for busyindicator
                              tags$head(tags$script(HTML(jscode))), ## for "Enter to actionbtn-press simulation
                              column(2),
                              column(2,
                              textInput("username", "Benutzername"),
                                  
                                  withBusyIndicatorUI(
                                    actionButton(
                                      "loginBtn",
                                      "Log in",
                                      class = "btn-primary"
                                    )
                                  )
                                  ),
                              column(2,
                                     tagAppendAttributes(
                                        passwordInput("passw", "Passwort"),
                                        `data-proxy-click` = "loginBtn"
                                      )
                              )
                            
                             )),
                   
                   ###########################################################################   #    
                   tabPanel("Einzelfragen",          
                            sidebarLayout(
                              sidebarPanel(width = 2,
                                           inputIp("ipid"),
                                           inputUserid("fingerprint"),
                                           radioButtons("likertfragen", tags$strong("Auswahl"),
                                                        choices = c("nichts ausgewählt"    = "nolikert",
                                                                    sample(c("Dimension Lernerfolg"  = "Lernerfolg"      ,
                                                                           "Dimension Enthusiasmus"  = "Enthusiasmus"    ,
                                                                           "Dimension Organisation"  = "Organisation"    ,
                                                                           "Dimension Interaktion"   = "Interaktion"     ,
                                                                           "Dimension Beziehung"     = "Beziehung"       ))),
                                                        selected = "nichts ausgewählt"),
                                           hr(),
                                           checkboxGroupInput("groupinl", tags$strong("Vergleich"),
                                                              choices = sample(c("Mit allen abiturma-Kursen vergleichen" = "gmean"))),
                                           hr(),
                                           actionButton("golikert", "Plot!")
                              ),
                              
                              mainPanel(
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"
                                ),
                                column(6,
                                       bsAlert("likertalert1"),
                                       svgPanZoomOutput(outputId = "einzelplot")
                                ),
                                column(6,
                                       plotOutput("gmeaneinzelplot")
                                ),
                                column(12,
                                       verbatimTextOutput("pw_conf")
                                       ),

                                column(12,
                                       verbatimTextOutput("glimpse_likertdata3")
                                ),                                
                              
                              column(12,
                                     verbatimTextOutput("user_p"))
                              )
                              
                              
                   )),
                   
                   ###########################################################################       #
                   tabPanel("Qualitätsdimensionen_v1",          
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                checkboxGroupInput("qualdim", span(tags$strong("Qualitätsdimensionen")," (min. eine ausw.)",
                                                                   bsButton("jitterqualdimmodalbt", "?", 
                                                                            style = "inverse", size = "extra-small")),
                                                   choices = sample(c("Lernerfolg"             = "Lernerfolg"            , 
                                                                      "Enthusiasmus"           = "Enthusiasmus"          , 
                                                                      "Organisation"           = "Organisation"          , 
                                                                      "Gruppeninteraktion"    =  "Gruppeninteraktion"   , 
                                                                      "Individuelle Beziehung" = "Individuelle Beziehung"))),
                                hr(),
                                
                                checkboxGroupInput("darstell", span(tags$strong("Darstellung")," (min. eine ausw.)",
                                                                    bsButton("darstellmodalbt", "?", 
                                                                             style = "inverse", size = "extra-small")),
                                                   choices = sample(c("Mittelwert der Dimensionswerte"              = "mean", 
                                                                      "Dimensionswerte je TeilnehmerIn"          = "jitter", 
                                                                      "Quartile (exkl. Ausreisser)"                = "boxplot", 
                                                                      "95%-Konfidenzintervall" = "ci"))),
                                hr(),
                                
                                checkboxGroupInput("groupin", tags$strong("Vergleiche"),
                                                   choices = sample(c("Meine Kurse getrennt darstellen"       = "kurse", 
                                                                      "Mit allen abiturma-Kursen vergleichen" = "gmean"))),
                                hr(),
                                
                                radioButtons("scaling", span(tags$strong("Skalierung")," (eine ausw.)",
                                                             bsButton("scalemodalbt", "?", 
                                                                      style = "inverse", size = "extra-small")),
                                             choices = c("nichts ausgewählt"             = "noscale",
                                                       sample(c("Skala 1 (--) bis 7 (++)"        = "raw",
                                                                "Mittelw. abiturma = 0 (je Dimension)"        = "std",
                                                                "Mittelw. je Teilnehmer/In = 0" = "gstd"))),
                                             selected = "noscale"),
                                hr(),
                                
                                actionButton("goqualdim", "Plot!")
                                
                              ),
                              mainPanel(width = 9,
                                bsAlert("jitteralert1"),
                                h2("Dein Plot"),
                                
                                uiOutput("jitter.plot.ui"),
                                #DT::dataTableOutput("debugjitterdata"),
                                #plotOutput("jitterplot2"),
                                hr(),
                                bsModal("qualdimhelpjitter", "Auswahl der Qualitätsdimensionen", "jitterqualdimmodalbt", size = "small",
                                        includeMarkdown("Stuff/Helpstuff/qualdimhelpjitter2.md")),
                                bsModal("darstellhelp", "Auswahl der Darstellungen", "darstellmodalbt", size = "large",
                                        includeHTML("Stuff/Helpstuff/help_table1.html")),
                                bsModal("scalehelp", "Auswahl der Darstellungsoptionen", "scalemodalbt", size = "large",
                                        withMathJax(includeMarkdown("Stuff/Helpstuff/scalehelp7.md"))),
                                bsButton("help_table_bt", label = "Erläuterung der Darstellung"),
                                bsModal("help_table", "Erläuterung der Darstellung", "help_table_bt", size = "large",##
                                        includeHTML("Stuff/Helpstuff/help_table1.html")),
#                                 bsButton("help_table_bt2", label = "Erläuterung der Darstellung Image"),
#                                 bsModal("help_table2", "Erläuterung der Darstellung", "help_table_bt2", size = "large",
#                                         #includeHTML("Stuff/Helpstuff/darstell_modal_im2.html")),
#                                         #imageOutput("darstell_help_im")),
#                                         img(src = "img/darstell_modal_im2.png", width = 800, style = "display: block; margin-left: auto; margin-right: auto;")),
#                                         #tags$div(HTML(""))),
                                bsButton("help_scale_bt", label = "Erläuterung der Skalierungen"),
                                bsModal("help_scale", "Erläuterung der Skalierungen", "help_scale_bt", size = "large",
                                        withMathJax(includeMarkdown("Stuff/Helpstuff/scalehelp7.md")))
                                
                                
                                
                              )
                            
                   )),


                   
                   ###########################################################################
                   tabPanel("Qualitätsdimensionen_v2",
                        sidebarLayout(
                            sidebarPanel(width = 3,
                              selectInput("qualdim2", label = h3("Ich möchte ..."), 
                                         choices = c("bitte auswählen" = "noqualdim2",
                                                     sample(c("einen Überblick bekommen" = "ueber",
                                                              "etwas über meine Stärken und Schwächen erfahren" = "staerken",
                                                              "meine Kurse untereinander vergleichen" = "kurse",
                                                              "mich mit der Gesamtevaluation vergleichen" = "gmean"))), 
                                         selected = "noqualdim2"),
                              hr(),
                              actionButton("goqualdim2", "Plot!")
                            ),
                              
                            mainPanel(
                              
                              bsAlert("qualdim2alert1"),
                              plotOutput("qualdim2plot", height = "500px"),
                              #actionButton("interpretbutton", "Interpretationshilfe für diesen Plot anzeigen"),
                              #uiOutput("interpretationshilfe.ui")
                              conditionalPanel("input.qualdim2 == 'gmean'",
                                     bsButton("help_gmean", label = "Erläuterung dieser Darstellung"),
                                     bsModal("gmean_modal", "Erläuterung der Darstellung", "help_gmean", size = "large",
                                             includeHTML("Stuff/Helpstuff/help_table5.html")),
                                     bsButton("help_q2_grandmean", label = "Erläuterung dieser Skalierung"),
                                     bsModal("q2_grandmean_modal", "Erläuterung dieser Skalierung", "help_q2_grandmean", size = "large",
                                             includeMarkdown("Stuff/Helpstuff/scalehelp_q2_grandmean.md"))
                                     ),
                              
                              conditionalPanel("input.qualdim2 == 'staerken'",
                                     bsButton("help_darst_st", label = "Erläuterung dieser Darstellung"),
                                     bsModal("help_darst_st_modal", "Erläuterung der Darstellung", "help_darst_st", size = "large",
                                             includeHTML("Stuff/Helpstuff/help_table6.html")),
                                     bsButton("help_q2_groupmean", label = "Erläuterung dieser Skalierung"),
                                     bsModal("q2_groupmean_modal", "Erläuterung dieser Skalierung", "help_q2_groupmean", size = "large",
                                             includeMarkdown("Stuff/Helpstuff/scalehelp_q2_groupmean.md"))
                                     ),
                              conditionalPanel("input.qualdim2 == 'ueber' | input.qualdim2 == 'kurse'",#
                              bsButton("help_restq2", label = "Erläuterung dieser Darstellung"),
                              bsModal("urestq2_modal", "Erläuterung der Darstellung", "help_restq2", size = "large",
                                      includeHTML("Stuff/Helpstuff/help_table6.html")))
                              
                            )
                        )
                    ),
#############################################################################
tabPanel("Freitext-Antworten",
         sidebarLayout(
           sidebarPanel(width = 3,
                        radioButtons("sort_freitexte", "Freitexte sortieren ?",
                                     choices = c("nicht sortieren" = "nsort",
                                                 "absteigende Gesamtbewertung" = "abst"
                                     )
                        ),
                        actionButton("gofreitext", "Freitexte anzeigen")
           ),
           
           mainPanel(width = 9,
                     h3("Deine Freitexte (nur Winter/Frühjahr '16)"),
                     p("Bitte über Sortierung entscheiden. \n
                       Dann erscheinen einige Sekunden nach Drücken des \"Freitexte Anzeigen\" 
                       Buttons die Freitexte deiner KursteilnehmerInnen. \n
                       Hast du die Option \"absteigende Gesamtbewertung\" ausgewählt, werden die Freitexte nach den 
                       Bewertungen in den Qualitätsdimensionen sortiert: Oben stehen die Freitexte von TeilnehmerInnen die dich 
                       am besten bewertet haben."),
                     uiOutput("freitextplots")
                     )
           )
         )

   


                
#                    ###########################################################################
#                    tabPanel("Debug",
#                             fluidRow(verbatimTextOutput("debug1"),
#                                      verbatimTextOutput("debug2"))),
#                    
#                    
#                    
                   ###########################################################################
#                    tabPanel("Deine Rückmeldung",
#                             fluidRow(
#                               
#                               column(12, div(style = "height:100%;width:100%"),
#                                      tags$div(
#                                        HTML("<iframe src='https://www.soscisurvey.de/abiturma/?q=abiturma_fr_q2' 
#                                             style='border: none; width: 100%; height: 800px;'></iframe>")
#                                      )
#                               ))
#                             
#                             
#                    )
))
                   
                   