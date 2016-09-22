library(shiny)
library(shinyBS)
library(shinyjs)
source("helpers.R")


#####################################################################################
# js code for:                                                                      #
#     -enter key = simulation of button press                                       #                         
#     -action button = close app                                                    #
#     -reset of stratingInput                                                       #
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

shinyjs.closeWindow = function() { window.close(); }
'

#jsCodeR <-"shinyjs.reset_1 = function(params){$('.rating-symbol-foreground').css('width', params);}"
#####################################################################################
# CSS adaption for plot-spinner                                          ############
#####################################################################################

mycss <- "
#plot-container {
position: relative;
}
#loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: -1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
z-index: -2;
}
"



#####################################################################################
# Calls for Fingerprint                                                  ############
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
    #js für Fingerprint und ip-Adresse
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;"),
    
    # css und js für abiturmastyle
    singleton(tags$head(tags$script(src = "https://www.abiturma.de/unterrichtsfeedback_dateien/js/script.js", type='text/javascript'))), #$$
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://www.abiturma.de/unterrichtsfeedback_dateien/style.css")), #$$
    tags$head(tags$title("Unterrichtsfeedback")) #$$
  )
}

#####################################################################################
# Edit Radio button fucntion                                             ############
#####################################################################################

edit_rbutton=function(rb){
  
  for( i in 1:length(rb$children[[2]]$children[[1]])){
    value=as.numeric(rb$children[[2]]$children[[1]][[i]]$children[[1]]$children[[1]]$attribs$value)
    if(!is.na(value)&value>0){
      rb$children[[2]]$children[[1]][[i]]$children[[1]]$children[[2]]$children[[1]]=HTML(paste(rep('<i class="fa fa-star" aria-hidden="true"></i>',value),sep = "",collapse = ""))
    }
  }
  rb
  
}


#####################################################################################
# ShinyUI                                                                ############
#####################################################################################

shinyUI(
  tagList(
  useShinyjs(),
  navbarPage("Unterrichtsfeedback", id = "navbarpage", theme = "lumen.css", #$$
                   
############################################################################
# Vorwort                                                            ######
###########################################################################
                   tabPanel("Vorwort", 
                           # fluidRow(
                            fluidRow(class="splash",tags$div(id="splash"), #$$
                              column(7, offset = 1,
                                     includeMarkdown("Stuff/hallo4.md")),
                           
                              column(3, offset = 1, 
                              tags$style(appCSS), ## for busyindicator
                              tags$head(tags$script(HTML(jscode))), ## for "Enter to actionbtn-press simulation
                              h1("Login"),
                              textInput("username", "Benutzername"),
                              tagAppendAttributes(passwordInput("passw", "Passwort"),`data-proxy-click` = "loginBtn"),
                              withBusyIndicatorUI(actionButton("loginBtn","Log in", class = "btn-primary"))))),
                            
                   
###########################################################################
# Einzelfragen                                                       ######
###########################################################################
                   tabPanel(title = "Einzelfragen", value = "einzelfragen",
                            fluidRow(
                              column(2,
                              wellPanel(
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
                                 actionButton("golikert", "Plot!", icon = icon("bar-chart-o", lib = "font-awesome"))
                              )
                              ),
                              
                              column(7,
                                 tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"),
                                 p(div(HTML("<div id = 'loginwarning'; style='display: block;' class='btn-err'>
                                                          <div>
                                                            <i class='fa fa-exclamation-circle'></i>
                                                            <b>Error: </b>
                                                            <span class='btn-err-msg'>Login-Passwort Kombination ist falsch!</span>
                                                          </div>
                                                        </div>"))),
                                 bsAlert("likertalert1"),
                                 hidden(div(id = "likert-plot-container",
                                            tags$img(src = "spinner.gif",
                                                     id = "loading-spinner"),
                                 imageOutput("einzelplot")))
                                # verbatimTextOutput("glimpse_likertdata3")
                                ),
                              
                              column(3,
                                     hidden(
                                       div(id = "likert_star_wellpanel",
                                     wellPanel(
                                       
                                       h4("Bitte bewerte einige Plots"),
                                       h5(""),
                                       edit_rbutton(radioButtons("likert_inf_stars",
                                                                 div(HTML("<b>Informationsgehalt</b> des aktuellen Plots")),
                                                                 choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4))),
                                       edit_rbutton(radioButtons("likert_sic_stars",
                                                                 div(HTML("Meine <b>Interpretationssicherheit</b> bzgl. des 
                                                                                             aktuellen Plots")),
                                                                 choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4))),
                                       actionButton("likert_fb_btn", "Feedback abschicken", icon = icon("send", lib = "font-awesome"))
                                     ))
                                     ))
                   )),
                   
###########################################################################
# Qualitätsdimensionen_v1                                            ######
###########################################################################
                   tabPanel(title = "Qualitätsdimensionen_v1", value = "qualdim_v1",
                            fluidRow(
                              column(width = 3,
                              wellPanel(
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
                                
                                actionButton("goqualdim", "Plot!", icon = icon("bar-chart-o", lib = "font-awesome"))
                                
                              )),
                             column(width = 7,
                                p(div(HTML("<div id = 'loginwarning2'; style='display: block;' class='btn-err'>
                                                    <div>
                                           <i class='fa fa-exclamation-circle'></i>
                                           <b>Error: </b>
                                           <span class='btn-err-msg'>Login-Passwort Kombination ist falsch!</span>
                                           </div>
                                           </div>
                                           "))),
                                bsAlert("jitteralert1"),
                                h2("Dein Plot"),
                                #imageOutput("qualdimplot_svg"),
                                hidden(
                                  div(id = "qualdimplot1-container",
                                      tags$img(src = "spinner.gif",
                                               id = "loading-spinner"),
                                uiOutput("jitter.plot.ui"))),
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
                                bsButton("help_scale_bt", label = "Erläuterung der Skalierungen"),
                                bsModal("help_scale", "Erläuterung der Skalierungen", "help_scale_bt", size = "large",
                                        withMathJax(includeMarkdown("Stuff/Helpstuff/scalehelp7.md")))),
                               column(2, 
                                      hidden(div(id = "q1_star_wellpanel",
                                wellPanel(
                                h4("Bitte bewerte einige Plots!"),
                                edit_rbutton(radioButtons("q1_inf_stars",
                                                          div(HTML("<b>Informationsgehalt</b> des aktuellen Plots")),
                                                          choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4))),
                                edit_rbutton(radioButtons("q1_sic_stars",
                                                          div(HTML("Meine <b>Interpretationssicherheit</b> bzgl. des 
                                                                                             aktuellen Plots")),
                                                          choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4))),
                                actionButton("qualdim1_fb_btn", "Feedback abschicken", icon = icon("send", lib = "font-awesome"))
                                
                                
                                      )
                                ))
                                )
                               )
                             
                            
                   ),


                   
###########################################################################
# Qualitätsdimensionen_v2                                            ######
###########################################################################
                   tabPanel(title = "Qualitätsdimensionen_v2", value = "qualdim_v2",
                        fluidRow(
                            column(2,
                              wellPanel(selectInput("qualdim2", label = h3("Ich möchte ..."), 
                                         choices = c("bitte auswählen..." = "noqualdim2",
                                                     sample(c("einen Überblick bekommen" = "ueber",
                                                              "etwas über meine Stärken und Schwächen erfahren" = "staerken",
                                                              "meine Kurse untereinander vergleichen" = "kurse",
                                                              "mich mit der Gesamtevaluation vergleichen" = "gmean"))), 
                                         selected = "noqualdim2"),
                              hr(),
                              actionButton("goqualdim2", "Plot!")
                              )
                             ),
                              
                            column(8,
                              tags$head(tags$style(HTML(mycss))), # for plot spinner
                              p(div(HTML("<div id = 'loginwarning3'; style='display: block;' class='btn-err'>
                                                    <div>
                                         <i class='fa fa-exclamation-circle'></i>
                                         <b>Error: </b>
                                         <span class='btn-err-msg'>Login-Passwort Kombination ist falsch!</span>
                                         </div>
                                         </div>
                                         "))),
                              bsAlert("qualdim2alert1"),
                              hidden(
                              div(id = "plot-container",
                                  tags$img(src = "spinner.gif",
                                           id = "loading-spinner"),
                              imageOutput("qualdim2plot"))),
 
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
                             ),
                            column(2,
                                   hidden(
                                     div(
                                       id = "q2_form",
                                   wellPanel(
                                     h4("Bitte bewerte einige Plots!"),

                                       edit_rbutton(radioButtons("q2_inf_stars",
                                                                 div(HTML("<b>Informationsgehalt</b> des aktuellen Plots")),
                                                                 choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4))),
                                       edit_rbutton(radioButtons("q2_sic_stars",
                                                                 div(HTML("Meine <b>Interpretationssicherheit</b> bzgl. des 
                                                                          aktuellen Plots")),
                                                                 choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4))),
                                       actionButton("qualdim2_fb_btn", "Feedback abschicken", icon = icon("send", lib = "font-awesome"))))
                                     )
                                   )
                                  
                            )
                        ),


###########################################################################
# Freitext-Antworten                                                 ######
###########################################################################

tabPanel(title = "Freitext-Antworten", value = "freitext_antw",
         fluidRow(
           column(2,
                  wellPanel(
                        radioButtons("sort_freitexte", "Freitexte sortieren ?",
                                     choices = c("nicht sortieren" = "nsort",
                                                 "absteigende Gesamtbewertung" = "abst"
                                     )
                        ),
                        actionButton("gofreitext", "Freitexte anzeigen")
                  )
           ),
           
           
             column(8,
                  p(div(HTML("<div id = 'loginwarning4'; style='display: block;' class='btn-err'>
                                                     <div>
                                                       <i class='fa fa-exclamation-circle'></i>
                                                       <b>Error: </b>
                                                       <span class='btn-err-msg'>Login-Passwort Kombination ist falsch!</span>
                                                     </div>
                                                   </div>
                                     "))),
                  h3("Deine Freitexte (nur Herbst '16)"),
                  p("Bitte über Sortierung entscheiden. \n
                    Dann erscheinen einige Sekunden nach Drücken des \"Freitexte Anzeigen\" 
                    Buttons die Freitexte deiner KursteilnehmerInnen. \n
                    Hast du die Option \"absteigende Gesamtbewertung\" ausgewählt, werden die Freitexte nach den 
                    Bewertungen in den Qualitätsdimensionen sortiert: Oben stehen die Freitexte von TeilnehmerInnen die dich 
                    am besten bewertet haben."),
                  uiOutput("freitextplots")),
            column(2,
                   hidden(
                   div(
                      id = "frei_form",
                      wellPanel(
                      h4("Bitte bewerten!"),
                      
                      edit_rbutton(radioButtons("frei_inf_stars",
                                                div(HTML("<b>Informationsgehalt</b> der Freitexte")),
                                                choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4))),
                      actionButton("frei_fb_btn", "Feedback abschicken", icon = icon("send", lib = "font-awesome"))
                  ))) 
           )
              
           )),

   

###########################################################################
# Log-out Site                                                       ######
###########################################################################

tabPanel(title = "Deine Rückmeldung & Logout", value = "logout",
         
           fluidRow(
             column(8, offset = 2,
                     wellPanel(
                       h3("Dein Feedback"),
                       fluidRow(
                         column(6,
                       h4("Dein Feedback zu den Rückmeldeformaten"),
                       edit_rbutton(radioButtons("glob_fb_likert_inf",
                                                 div(HTML("Die <b>Informativität</b> der Einzelanworten bewerte ich&nbsp;...")),
                                                 choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4))),
                       hr(),
                       edit_rbutton(radioButtons("glob_fb_q1_inf",
                                                 div(HTML("Die <b>Informativität</b> der <b>freien</b> Darstellung der 
                                                              Qualitätsdimensionen (V1) bewerte ich&nbsp;...")),
                                                 choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4))),
                       hr(),
                       h5("Welche Erkenntnisse hast Du für Dich aus den eben gesichteten Rückmeldungen der Kursteilnehmer/innen 
                         gewinnen können?"),
                       tags$textarea(id="glob_fb_erk", rows=3, cols=90,"..."),
                       hr(),
                       h4("Dein Feedback zu abiturma"),
                       h5("Hier hast Du Platz für jede Art von Rückmeldungen. (Beispielsweise hinsichtlich unserer Kommunikation 
                        mit Dir, den Kursunterlagen, der Organisation vor Ort, Deinen Erfahrungen beim Unterrichten etc.)"),
                                tags$textarea(id="glob_fb_abiturma", rows=5, cols=90, "..."),
                                tags$br(),
                       tags$br(),
                       
                       actionButton("logout_btn", "Feedback abschicken & Logout", icon = icon("off", lib = "glyphicon"))
                       
                       ),        
                       
                      column(6,
                             h4(div(HTML("<font color='#FAFAFA'>Platzhalter inHGcol</font>"))),
                             edit_rbutton(radioButtons("glob_fb_frei_inf",
                                                       div(HTML("Die <b>Informativität</b> der Freitexte bewerte ich&nbsp;...")),
                                                       choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4))),
                             hr(),
                             edit_rbutton(radioButtons("glob_fb_q2_inf",
                                                       div(HTML("Die <b>Informativität</b> der <b>vorgegebenen</b> Darstellung der 
                                                              Qualitätsdimensionen (V2) bewerte ich&nbsp;...")),
                                                       choices = list("bitte bewerten" = 0,'one'=1,"two"=2, "three" = 3, "four" = 4)))              
 
                       )
                      
                       )
                      
             )
            
           ))
         )


))
)

                   
                   