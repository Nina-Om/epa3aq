rm(list=ls(all=T))

# list.of.packages <- c("ggplot2", "dplyr", "data.table", "DT", "readxl", "shiny", "shinythemes")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

# sink("pca.txt", append = T)
# sink()

library(dplyr)
library(shiny)
library(data.table)
library(ggplot2)
library(DT)
library(readxl)
library(shinythemes)
library(splus2R)
library(plotly)
library(Hmisc)
library(tinytex)
library(shinyFiles)
library(utils)
library(webshot)

source("checkparameter.R")

shinyUI(fluidPage(theme = shinythemes::shinytheme("cerulean"),
                  includeCSS("styles.css"),
               #   includeHTML("static.html"),
  navbarPage("Tier I Screening Aquatic Models for Pesticide Risk Assessment",
                # tags$head(tags$script(type="text/javascript", src = "code.js")),

             tabsetPanel(
               tabPanel("SCI-GROW",icon = icon("flask"),
                        fluidRow(
                          br(),
                          br(),
                          h4("SCI-GROW (Screening Concentration In GROund Water)"),
                          h6("SCI-GROW is a screening model for estimation of pesticide concentrations in vulnerable ground water.The SCI-GROW estimate is based on environmental fate properties of the pesticide (aerobic soil degradation half-life and linear adsorption coefficient normalized for soil organic carbon content), the maximum application rate, and existing data from small-scale prospective ground-water monitoring studies at sites with sandy soils and shallow ground water.")
                          ),
                          hr(),
                        sidebarLayout(
                                   sidebarPanel(tags$style(".well {background-color:#93CCEA;}"),
                                               checkparameterUI(id = "SCIGROW"),
                                               br(),
                                               br(),
                                               h5("2. Download Report"),
                                               br(),
                                               textInput("fname1", "Enter report file name (Optional):", "report.pdf"),
                                               br(),
                                               actionButton("submit1", label = "Download Report")
                                               ),
                        mainPanel(
                          h4("Results"),
                          hr(),
                          DT::dataTableOutput("print.s"),
                          br(),
                          br(),
                          div(plotlyOutput("plot.s"), align = "center"),
                          br(),
                          tags$p("Contact: Nina Omani (nina57@tamu.edu)")
                          ))
                        ),



               tabPanel("GENEEC", icon = icon("flask"),
                        fluidRow(
                          br(),
                          br(),
                          h4("GENEEC (GENeric Estimated Environmental Concentration)"),
                          h6("GENEEC2 is a screening model to predict environmental concentrations of pesticides in surface water for aquatic exposure assessments. GENEEC2 considers adsorption, incorporation of the pesticide at application, direct deposition of spray drift into the water body, and degradation of the pesticide. It is a single-event model. The GENEEC2 program is generic in that it does not consider differences in climate, soils, topography or crop in estimating potential pesticide exposure. GENEEC2 is expected to overestimate pesticide concentrations in surface water.")
                        ),
                        hr(),
                        sidebarLayout(
                          sidebarPanel(tags$style(".well {background-color:#93CCEA;}"),
                                       checkparameterUI(id = "GENEEC"),
                                       br(),
                                       br(),
                                       h5("2. Download Report"),
                                       br(),
                                       textInput("fname2", "Enter report file name (Optional):", "report.pdf"),
                                       br(),
                                       actionButton("submit2", label = "Download Report")
                          ),
                          mainPanel(
                            h4("Results"),
                            hr(),
                            DT::dataTableOutput("print.g"),
                            br(),
                            br(),
                            div(plotlyOutput("plot.g"), align = "center"),
                            tags$p("Contact: Nina Omani (nina57@tamu.edu)")
                          ))
               ),

               tabPanel("FIRST", icon = icon("flask"),
               fluidRow(
                 br(),
                 br(),
                 h4("FIRST (FQPA Index Reservoir Screening Tool)"),
                 h6("FIRST estimates peak values (acute) and long-term (chronic) average concentrations of pesticides in water. Like GENEEC, it is based upon the linked PRZM and EXAMS models and is a single-event process. As with the Tier II modeling for drinking water, FIRST uses an Index Reservoir watershed based on the Shipman City Lake in Illinois.")
               ),
               hr(),
               sidebarLayout(
                 sidebarPanel(tags$style(".well {background-color:#93CCEA;}"),
                              checkparameterUI(id = "FIRST"),
                              br(),
                              br(),
                              h5("2. Download Report"),
                              br(),
                              textInput("fname3", "Enter report file name (Optional):", "report.pdf"),
                              br(),
                              actionButton("submit3", label = "Download Report")
                 ),
                 mainPanel(
                   h4("Results"),
                   hr(),
                   DT::dataTableOutput("print.f"),
                   br(),
                   br(),
                   div(plotlyOutput("plot.f"), align = "center"),
                   br(),
                   tags$p("Contact: Nina Omani (nina57@tamu.edu)")
                 ))
             ),



 ## HELP #################################################################################
             navbarMenu("Help",
                        tabPanel("SCI-GROW",
                                 fluidRow(
                                   h3("Model SCI-GROW"),
                                   hr(),
                                   h4("Guides for Input Paramaters"),
                                   h6("Use maximum labeled rate and maximum number of applications per year. If Koc has more than 3 fold variation, use lowest Koc; otherwise use median Koc. If there are less than 4 metabolism values, use the mean maetabolism half life; otherwise, use the median half life.")
                                 )),
                        tabPanel("FIRST",
                                 fluidRow(
                        h3("Model FIRST"),
                      hr(),
                      h6("This is a program to estimate both acute and chronic Tier one, upper level (higher exposure) drinking water concentrations for food quality protection act (FQPA) assessments. It estimates pesticide concentrations in a vulnerable index reservoir located a high use area for the pesticide being assessed. The program considers reductions in dissolved concentration due to the percentage of the watershed which is cropped (percent cropped area), reduction in dissolved pesticide concentration due to adsorption of pesticide to soil or sediment, incorporation, degradation in soil before washoff to a water body, direct deposition of spray drift into the water body, and degradation of the pesticide within the water body. It is designed to mimic a PRZM-EXAMS simulation for a high exposure site."),
h6("Release notes version 1.1.1: This release was created primarily for the purpose
of standardizing the compiler versions in EFED. This version has been compiled with
Intel Fortran 10.0 along with Visual Studio 2005. No calulations-type changes were made
from version 1.1.0. In this version only grammar was corrected in the output text."),
hr(),
h4("Guides for Input Paramaters"),
h5("APPRAT: Application rate (lb/acre or kg/ha)"),
h6("The program assumes that rainfall and resulting runoff are sufficient to remove up to 8% of the pesticide from the portion of the 427 acres (172.8 hectares) of fields in the reservoir watershed where the crop is grown the portion of the chemical that is removed from the fields. In this way, flows into the reservoir and is dissolved in the reservoir water. The chemical concentation in the reservoir represents the part that is dissolved and not bound to field soil or to reservoir bottom sediments. The following information should be taken from the most current, accepted label for the use site in question."),
h5("PCA: Percent Cropped Area (used as a decimal)"),
h6("The amount of pesticide in the watershed available for washoff by rainfall into the reservoir is dependent on extent of the watershed on which the crop is grown. The program represents this area by a percent cropped area."),


verbatimTextOutput("pca2"),

h5("Koc and Kd: Organic carbon partition coefficient, and Soil adsorption coefficient"),
h6("The dissolved pesticide concentration in the reservoir is calculated by subtraction of the portion of the chemical that is bound to field soil, to field organic matter or to reservoir bottom or suspended sediment. This bound fraction is estimated by use of the soil/water equilibrium partition coefficient (Kd) or the organic carbon normalized soilwater equilibrium partition coefficient (Koc). See the FIRST program users manual for the appropriate Kd or Koc value to use."),
h5("METHAF: Aerobic metabolic soil halflife (in days)"),
h6("The dissolved pesticide concentration is also reduced by degradation in the field prior to a rainfallrunoff event this program assumes degradation by aerobic metabolism between applications as well as for two days after the last of the applications."),
h5("WETTED: flag to indicate the pesticide is wetted-in and runoff occurs one the day of application"),
h6("Some pesticide labels require that the pesticide be activated by 'wetting-in' either through rainfall or through irrigation. In this case, runoff to the reservoir is assumed to occur immediately rather than after two days."),
h5("METHOD: Method of application"),
h6("The dissolved pesticide concentration may be increased by deposition of spray drift either directly into the reservoir or into the streams that flow into the reservoir"),
verbatimTextOutput("method"),
h5("METHAF: Aerobic metabolic soil halflife (in days)"),
h6("The dissolved pesticide concentration is also reduced by degradation in the field prior to a rainfall /runoff event. This program assumes degradation by aerobic metabolism between applications as well as for two days after the last of the applications (if stable to aerobic soil metabolism or if data is unavailable, please enter zero)."),
h5("INCORP: Depth of incorporation (in)"),
h6("The dissolved pesticide concentration may be reduced by incorporation of the pesticide at the time of application. In this case, the following depths are suggested: "),
verbatimTextOutput("ncorp"),
h5("METHAP: Aerobic metabolic halflife in the reservoir (days)") ,
h6("The chronic drinking water pesticide concentration value is estimated using a degradation rate that is calculated by summing the individual aquatic degradation rates (the aerobic aquatic metabolic rate is assumed to include hydrolysis). Please enter zero for any that are stable or for which values are unavailable. If unavailable, the recommended EFED default is 2 times.")
)),

tabPanel("GENEEC",
         fluidRow(
         h3("Model GENEEC"),
         hr(),
         h6("This is a program to calculate both acute and chronic generic expected environmental concentration (GEEC) values. It considers reduction in dissolved pesticide concentration due to adsorption of pesticide to soil or sediment, incorporation, degradation in soil before washoff to a water body, direct deposition of spray drift into the water body, and degradation of the pesticide within the water body. It is designed to mimic a PRZM-EXAMS simulation."),
         hr(),
         h4("Guides for Input Paramaters"),

         h5("APPRAT: Application rate (lb/acre or kg/ha)"),
         h6(" The program assumes that rainfall and resulting runoff are sufficient to remove up to 10 percent of the pesticide from the 10 hectare treated agricultural field. The portion of the chemical which is removed from the field in this way, flows into the pond and is dissolved in the the pond water. The chemical concentation in the pond represents the part which is dissolved and not bound to field soil or to pond bottom sediments. The following information should be taken from the most current, accepted label for the use site in question."),
         h5("Koc and Kd: Organic carbon partition coefficient, and Soil adsorption coefficient"),
         h6("The dissolved pesticide concentration in the pond is calculated by subtraction of the portion of the chemical that is bound to field soil, to field organic matter or to pond bottom sediment. This bound fraction is estimated by use of the soil/water equilibrium partition coefficient (Kd) or the organic carbon normalized soilwater equilibrium partition coefficient (Koc). See the GENEEC program users manual for the appropriate Kd or Koc value to use."),
         h5("METHAF: Aerobic metabolic soil halflife (in days)"),
         h6("The dissolved pesticide concentration is also reduced by degradation in the field prior to a rainfall/runoff event this program assumes degradation by aerobic metabolism between applications as well as for two days after the final application."),
h5("WETTED: flag to indicate the pesticide is wetted-in and runoff occurs one the day of application"),
         h6("Some pesticide labels require that the pesticide be activated by 'wetting-in' either through rainfall or through irrigation. In this case, runoff to the pond is assumed to occur immediately rather than after two days."),
         h5("METHOD: Method of application"),
         h6("The dissolved pesticide concentration may be increased by deposition of spray drift either directly into the reservoir or into the streams that flow into the reservoir"),
         verbatimTextOutput("method2"),
         h5("METHAF: Aerobic metabolic soil halflife (in days)"),
         h6("The dissolved pesticide concentration is also reduced by degradation in the field prior to a rainfall/runoff event. This program assumes degradation by aerobic metabolism between applications as well as for two days after the last of the applications (if stable to aerobic soil metabolism or if data is unavailable, please enter zero)."),
         h5("INCORP: Depth of incorporation (in)"),
         h6("The dissolved pesticide concentration may be reduced by incorporation of the pesticide at the time of application. In this case, the following depths are suggested: "),
         verbatimTextOutput("ncorp2"),
         h5("METHAP: Aerobic metabolic halflife in the reservoir (days)"),
         h6("The chronic drinking water pesticide concentration value is estimated using a degradation rate that is calculated by summing the individual aquatic degradation rates (the aerobic aquatic metabolic rate is assumed to include hydrolysis). Please enter zero for any that are stable or for which values are unavailable. If unavailable, the recommended EFED default is 2 times."),
h5("AIRFLG: Flag to indicate aerial droplet size distribution"),
h6("The distribution of droplet sizes in pesticide spray (spray quality) impacts the distance of travel and the quantity of pesticide which will drift off-site with the wind."),
verbatimTextOutput("airflg"),
h5("GRNFLG: Flag to indicate ground sprayer type"),
h6("The amount of off-site spray drift from ground application depends on the sprayer configuration and the spray quality (droplet size distribution). Please enter the nozzle height above the crop or ground:", p("A: low boom ground sprayer (20 inches or less), or B: high boom ground sprayer (20 to 50 inches: efed default).")),
h5("ORCFLG: Flag to indicate type of orchard airblast application"),
h6("Airblast type (note: both airblast selections include a 3x safety factor)", p("A: orchards and dormant vineyards, or B: foliated vineyards")),
h5("SOL: Solubility (ppm)"),
h6("The dissolved pesticide concentration in a water body cannot exceed the solubility of the chemical."),
h5("HYDHAP: Hydrolysis halflife in the pond (days)"),
h6("Chronic generic EEC values are calculated by summing the individual aquatic degradation rates. The aerobi aquatic metabolic rate is assumed to include hydrolysis. Please enter zero for any which are stable or for which values are unavailable. If unavailable, recommended EFED default is 2x aerobic soil input value.")
)))
)
  )))

#   tabPanel("Document",
#            sidebarPanel(
#              selectInput(inputId='test',label=1,choices=1:5)
#            ),
#            mainPanel(
#              htmlOutput("inc")
#            )
# )
