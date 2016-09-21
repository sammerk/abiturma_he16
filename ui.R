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
                                 imageOutput("einzelplot")))),
                              
                              column(3,
                                     wellPanel(
                                       
                                       h4("Bitte gib uns hin und wieder Feedback zu einem Plot!"),
                                       h5(""),
                                       div(id = "likertform",
                                       tags$div(HTML('<div id="likertstars1" class="form-group shiny-input-radiogroup shiny-input-container">
  <label class="control-label" for="dist">Informativität des aktuell dargestellten Plot: </label>
                                                     <div class="shiny-options-group">
                                                     <div class="radio">
                                                     <label>
                                                     <input type="radio" name="dist" value="norm" checked="checked"/>
                                                     <span><i class="fa fa-star" aria-hidden="true"></i>
</span>
                                                     </label>
                                                     </div>
                                                     <div class="radio">
                                                     <label>
                                                     <input type="radio" name="dist" value="unif"/>
                                                     <span><i class="fa fa-star" aria-hidden="true"></i><i class="fa fa-star" aria-hidden="true"></i></span>
                                                     </label>
                                                     </div>
                                                     <div class="radio">
                                                     <label>
                                                     <input type="radio" name="dist" value="lnorm"/>
                                                     <span><i class="fa fa-star" aria-hidden="true"></i><i class="fa fa-star" aria-hidden="true"></i><i class="fa fa-star" aria-hidden="true"></i></span>
                                                     </label>
                                                     </div>
                                                     <div class="radio">
                                                     <label>
                                                     <input type="radio" name="dist" value="exp"/>
                                                     <span><i class="fa fa-star" aria-hidden="true"></i><i class="fa fa-star" aria-hidden="true"></i><i class="fa fa-star" aria-hidden="true"></i><i class="fa fa-star" aria-hidden="true"></i></span>
                                                     </label>
                                                     </div>
                                                     </div>
                                                     </div>')),
                                         selectInput("likert_fb_inf", div(HTML("<p>Den aktuell dargestellten Plot finde ich&nbsp;...</p>")),
                                                     c("bitte auswählen..." = NA,
                                                       "1 = gar nicht informativ " = 1,
                                                       "2" = 2,
                                                       "3" = 3,
                                                       "4" = 4,
                                                       "5" = 5,
                                                       "6 = sehr informativ" = 6)),
                                         selectInput("likert_fb_sic", div(HTML("<p>Bei der inhaltlichen Interpretation des aktuell dargestellten Plots bin 
                                                                               ich&nbsp;...</p>")),
                                                     c("bitte auswählen..." = NA,
                                                       "1 = sehr unsicher " = 1,
                                                       "2" = 2,
                                                       "3" = 3,
                                                       "4" = 4,
                                                       "5" = 5,
                                                       "6 = sehr sicher" = 6)),
                                         actionButton("likert_fb_btn", "Feedback abschicken", icon = icon("send", lib = "font-awesome"))
                                       )
                                     ))
                               
                              
                              
                              
                   )),
                   
###########################################################################
# Qualitätsdimensionen_v1                                            ######
###########################################################################
                   tabPanel(title = "Qualitätsdimensionen_v1", value = "qualdim_v1",
                            fluidRow(
                              column(width = 2,
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
                             column(width = 8,
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
                                wellPanel(
                                h4("Bitte gib uns hin und wieder Feedback zu einem Plot!"),
                                div(
                                  id = "form",
                                selectInput("qualdim1_fb_inf", div(HTML("<p>Den aktuell dargestellten Plot finde ich&nbsp;...</p>")),
                                            c("bitte auswählen..." = NA,
                                              "1 = gar nicht informativ " = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5" = 5,
                                              "6 = sehr informativ" = 6)),
                                selectInput("qualdim1_fb_sic", div(HTML("<p>Bei der inhaltlichen Interpretation des aktuell dargestellten Plots bin 
                                                                               ich&nbsp;...</p>")),
                                            c("bitte auswählen..." = NA,
                                              "1 = sehr unsicher " = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5" = 5,
                                              "6 = sehr sicher" = 6)),
                                actionButton("qualdim1_fb_btn", "Feedback abschicken", icon = icon("send", lib = "font-awesome"))
                                )
                                )
                               )
                             
                            
                   )),


                   
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
                                   wellPanel(
                                     h4("Bitte gib uns hin und wieder Feedback zu einem Plot!"),
                                     div(
                                       id = "form",
                                       selectInput("qualdim2_fb_inf", div(HTML("<p>Den aktuell dargestellten Plot finde ich&nbsp;...</p>")),
                                                   c("bitte auswählen ..." = NA,
                                                     "1 = gar nicht informativ " = 1,
                                                     "2" = 2,
                                                     "3" = 3,
                                                     "4" = 4,
                                                     "5" = 5,
                                                     "6 = sehr informativ" = 6)),
                                      # textInput("qualdim2_fb_finf", div(HTML("<p>Besonders informativ am aktuell dargestellten Plot finde ich&nbsp;...</p>")),#
                                      #           value = "..."),
                                      # textInput("qualdim2_fb_fazit", "Welche Erkenntnisse hast Du für Dich aus dem
                                      #           aktuell dargestellten Plot gewinnen können?",
                                      #           value = "..."),
                                      # 
                                      selectInput("qualdim2_fb_sic", div(HTML("<p>Bei der inhaltlichen Interpretation des aktuell dargestellten Plots bin 
                                                                               ich&nbsp;...</p>")),
                                                  c("bitte auswählen..." = NA,
                                                    "1 = sehr unsicher " = 1,
                                                    "2" = 2,
                                                    "3" = 3,
                                                    "4" = 4,
                                                    "5" = 5,
                                                    "6 = sehr sicher" = 6)),
                                       actionButton("qualdim2_fb_btn", "Feedback abschicken", icon = icon("send", lib = "font-awesome"))
                                     )
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
                  hidden(
                  div(id = "freitext-container",
                      tags$img(src = "spinner.gif",
                               id = "loading-spinner"),
                  uiOutput("freitextplots")))
                  ),
            column(2,
                  wellPanel(
                    h4("Bitte gib uns Feedback zu deiner Wahrnehmung der Freitexte!"),
                    div(
                      id = "form",
                      selectInput("frei_fb_inf", div(HTML("<p>Die Freitexte finde ich&nbsp;...</p>")),
                                  c("bitte auswählen..." = NA,
                                    "1 = gar nicht informativ " = 1,
                                    "2" = 2,
                                    "3" = 3,
                                    "4" = 4,
                                    "5" = 5,
                                    "6 = sehr informativ" = 6)),
                      actionButton("frei_fb_btn", "Feedback abschicken", icon = icon("send", lib = "font-awesome"))
                  )) 
           )
              
           )),

   

###########################################################################
# Log-out Site                                                       ######
###########################################################################

tabPanel(title = "Deine Rückmeldung & Logout", value = "logout",
         
           fluidRow(
             column(8, offset = 2,
                     wellPanel(
                       h3("Dein Feedback zu den Rückmeldeformaten"),
                       fluidRow(
                         column(6,
                       selectInput("glob_fb_likert_inf", div(HTML("<p>Die Einzelantworten fand ich&nbsp;...</p>")),
                                   c("..." = NA,
                                     "1 = gar nicht informativ " = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "5" = 5,
                                     "6 = sehr informativ" = 6), width = "80%"),
                       selectInput("glob_fb_q1_inf", div(HTML("<p>Die <b>freie</b> Darstellung der 
                                                              Qualitätsdimensionen (V1) fand ich&nbsp;...</p>")),
                                   c("..." = NA,
                                     "1 = gar nicht informativ " = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "5" = 5,
                                     "6 = sehr informativ" = 6), width = "80%")),
                       column(6,
                              selectInput("glob_fb_frei_inf", div(HTML("<p>Die Freitexte fand ich&nbsp;...</p>")),
                                          c("..." = NA,
                                            "1 = gar nicht informativ " = 1,
                                            "2" = 2,
                                            "3" = 3,
                                            "4" = 4,
                                            "5" = 5,
                                            "6 = sehr informativ" = 6), width = "80%"),                       
                             selectInput("glob_fb_q2_inf", div(HTML("<p>Die <b>vorgegebene</b> Darstellung der 
                                                              Qualitätsdimensionen (V2) fand ich&nbsp;...</p>")),
                                   c("..." = NA,
                                     "1 = gar nicht informativ " = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "5" = 5,
                                     "6 = sehr informativ" = 6), width = "80%")
                       )),
                       
                       fluidRow(column(12,
                      h5("Welche Erkenntnisse hast Du für Dich aus den eben gesichteten Rückmeldungen der Kursteilnehmer/innen 
                         gewinnen können?"),
                      tags$br(),
                      tags$textarea(id="glob_fb_erk", rows=3, cols=60,"..."),
                      tags$br())),

                     fluidRow(column(12),
                              column(12,
                     h3("Dein Feedback zu abiturma"),
                     h5("Hier hast Du Platz für jede Art von Rückmeldungen. (Beispielsweise hinsichtlich unserer Kommunikation 
                        mit Dir, den Kursunterlagen, der Organisation vor Ort, Deinen Erfahrungen beim Unterrichten etc.)"),
                     tags$br(),
                     tags$textarea(id="glob_fb_abiturma", rows=5, cols=60, "..."),
                     tags$br(),
                     hr(),
                     actionButton("logout_btn", "Feedback abschicken & Logout", icon = icon("off", lib = "glyphicon"))
             ))
           ))
         ))


))
)

                   
                   