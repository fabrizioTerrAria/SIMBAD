#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# TODO:
# - https://stackoverflow.com/questions/50650616/stream-system-output-to-shiny-front-end-continuously


library(shiny)
library(shinyjs)
library(shinyFeedback)
library(bs4Dash)
library(shinyWidgets)
library(dplyr)
library(ipc)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(highcharter)
library(reactable)
library(reactablefmtr)
library(htmltools)
library(fs)
library(shinyscreenshot)
library(password)
library(sendmailR)
library(ipc)
library(future)
library(promises)
library(uuid)
library(tippy)
plan(multisession)


source("global.R")
source("functions.R")
source("functionsUI.R")
source("functionsDB.R")
source("keycloak.R")


shinyApp(
    ui = dashboardPage(
        title = "SIMBAD",
        fullscreen = TRUE,
        help = NULL,
        dark = NULL,
        scrollToTop = FALSE,
        header = dashboardHeader(
            title = dashboardBrand(
                title = "SIMBAD",
                color = "secondary",
                href = "https://www.rse-web.it/",
                image = "RSE3.png",
                opacity = 1
            ),
            .list = "SIMplified emission-concentration model BAsed on DDM",
            skin = "light",
            status = "white",
            border = TRUE,
            fixed = TRUE,
            rightUi = tagList(
                tags$li(class = "dropdown", downloadBttn(outputId = "downloadUserGuide",label = "User Guide",style = "bordered",color = "primary")),
                userOutput("user")
            )
        ),
        sidebar = bs4DashSidebar(
            id = "sidebar",
            skin = "light",
            elevation = 3,
            minified = TRUE,
            sidebarMenu(
                id = "main_menu",
                sidebarHeader("Menu"),
                bs4Dash::menuItem(
                    text = "Scenario definition",
                    tabName = "scenario",
                    icon = icon("chart-line", lib = "font-awesome")
                ),
                bs4Dash::menuItem(
                    text = "Dashboard",
                    tabName = "dashboard",
                    icon = icon("dashboard", lib = "font-awesome")
                ),
                bs4Dash::menuItem(
                    text = "Emissions",
                    tabName = "emissions",
                    icon = icon("industry", lib = "font-awesome")
                ),
                bs4Dash::menuItem(
                    text = "Concentrations",
                    tabName = "concentrations",
                    icon = icon("wind", lib = "font-awesome")
                ),
                bs4Dash::menuItem(
                    text = "Health impacts",
                    tabName = "health",
                    icon = icon("lungs", lib = "font-awesome")
                ),
                sidebarHeader("User Menu"),
                bs4Dash::menuItem(
                	text = "Scenario configuration",
                	tabName = "scenarioConfig",
                	icon = icon("list-check", lib = "font-awesome")
                ),
                uiOutput('sidebar_admin'),
                uiOutput('menu_admin')
  
            )
        ),
        footer = dashboardFooter(
            
        	left = a(
        		target = "_blank",
        		HTML(paste0("Model development:&nbsp;",a(img(src='RSE.png',height='30',width='60'),href="https://www.rse-web.it",target = "_blank",id = "logo1")))
        	),
        	
        	right = a(
        		target = "_blank",
        		HTML(paste0("Platform development:&nbsp;",a(img(src='logo_Terraria_Ridimensionato.png',height='30',width='125'),href="https://www.terraria.com",target = "_blank",id = "logo2")))
        	),
           fixed = TRUE
        	
        ),
        body = dashboardBody(
            
            useShinyjs(),
            useShinyFeedback(),
            
            tags$head(tags$link(rel="shortcut icon", href="favicon.ico"),
            					
            					## funzione Javascript per aggiornare le mappe una volta massimizzato il box che le contiene
            					tags$script(
            						"$(function() {
							              $('[data-card-widget=\"maximize\"]').on('click', function() {
							                $('#emissionsMap').trigger('resize');
            									$('#mapEmiAbsPanel').insertAfter($('#emissionsMap'));
            									$('#concentrationsMap').trigger('resize');
            									$('#mapConcAbsPanel').insertAfter($('#concentrationsMap')); 

							              });
							            });
							            "
            						),
            					
            					## funzione JavaScript per creare i knob modificata rispetto a quella originale
            					tags$script(src = "jquery.knob.min.js"),
            						
            					includeCSS("styles.css")
            					
            					),
            
            tabItems(
                tabItem(tabName = "scenario",

                        fluidRow(
                            # column(width = 1),
                            column(width = 12,
                                fluidRow(
                                    column(width = 3, pickerInput(inputId = "scenarioChoice",label = "Create or Upload a scenario",width = "100%",choices = NULL)),
                                    column(width = 1, pickerInput(inputId = "scenarioYear",label = "Year",width = "100%",choices = seq(YEAR_STEPS[1],YEAR_STEPS[3]))),
                                    column(width = 3, textInput(inputId = "scenarioName",label = "Scenario name",placeholder = "Insert a scenario name...", width = '100%', value = NULL)),
                                    column(width = 5, textInput(inputId = "scenarioDescription",label = "Scenario description",placeholder = "[Optional] Insert a scenario description...", width = '100%', value = NULL )),
                                ),
                                hr(),
                                fluidRow(
                                  column(width = 12, id = "colBox",
                                         box(title = html("<b>Business As Usual Scenario</b>"), width = 12, collapsible = T, collapsed = T, align = 'center',
                                             
                                             fluidRow(
                                               column(width = 12,
                                                      div(id = "modifyBAUScenarioDiv", style = "padding-bottom: 15px",
                                                        actionBttn(inputId = "modifyBAUScenarioButton",label = "Edit BAU Scenario",style = "bordered",color = "success",icon = icon("sync-alt"))
                                                      )
                                               )
                                             ),
                                             hidden(
                                               div(id = "defineBAUScenarioDiv",
                                                   fluidRow(
                                                 
                                                     column(width = 3,
                                                           actionBttn(inputId = "returnDefaultScenarioButton",label = "Default BAU Scenario",style = "bordered",color = "success",icon = icon("backward"))
                                                     ),
                                                     column(width = 9,
                                                            prettyRadioButtons(
                                                              inputId = "defineBAUScenarioOptions",
                                                              label = "Choose an option:",
                                                              choices = SCENARIO_DEFINITION_CHOICES,
                                                              selected = SCENARIO_DEFINITION_CHOICES[3],
                                                              inline = TRUE
                                                            )
                                                     )
                                                     
                                                 )
                                                 
                                               )
                                               
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(width = 12, id = "colBox",
                                                      hidden(
                                                        div(id = "activityBAUCSVBox",
                                                            box(title = "BAU Scenario - Activity indicators file", width = 12, collapsible = F,
                                                                fileInput("actBAUReductionFile", "Upload activity CSV file",
                                                                          multiple = FALSE,
                                                                          accept = c(".csv"),
                                                                          width = "100%")
                                                            )
                                                        )
                                                      ),
                                                      hidden( 
                                                        div(id = "emissionBAUCSVBox",
                                                            box(title = "BAU Scenario - Emission reductions file", width = 12, collapsible = F,
                                                                fileInput("emiBAUReductionFile", "Upload emission reductions CSV file",
                                                                          multiple = FALSE,
                                                                          accept = c(".csv"),
                                                                          width = "100%")
                                                            )
                                                        )
                                                      ),
                                                      hidden(
                                                        # div nascosto: serve a creare i knob prima di modificarli lato server
                                                        # in questo modo la classe javascript di default viene caricata prima 
                                                        # di quella modificata per i valori negativi e positivi
                                                        div(updateBoxKnobInput("BAU - Emission reductions (% vs 2017)","Bred"))
                                                      ),
                                                      div(id = "emissionBAUREDBox",
                                                          uiOutput("emiBAUReductionBox")
                                                      )
                                               )
                                             )
                                         )
                                  )
                                ),
                                hr(),
                                
                                fluidRow(
                                  column(width = 12, id = "colBox",
                                         box(title = html("<b>Scenario</b>"), width = 12, collapsible = F,
                                             
                                             fluidRow(
                                               column(width = 12,
                                                      div(id = "defineScenarioDiv",
                                                          prettyRadioButtons(
                                                            inputId = "defineScenarioOptions",
                                                            label = "Choose an option:",
                                                            choices = SCENARIO_DEFINITION_CHOICES,
                                                            inline = TRUE
                                                          )
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 12,
                                                      div(id = "defineComputationDiv",
                                                          prettyRadioButtons(
                                                            inputId = "defineComputationOptions",
                                                            label = "Computation:",
                                                            choices = COMPUTATION_CHOICES,
                                                            inline = TRUE
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               # column(width = 1),
                                               column(width = 12, id = "colBox",
                                                      div(id = "activityCSVBox",
                                                          box(title = "Activity indicators file", width = 12, collapsible = F,
                                                              fileInput("actReductionFile", "Upload activity CSV file",
                                                                        multiple = FALSE,
                                                                        accept = c(".csv"),
                                                                        width = "100%")
                                                          )
                                                      ),
                                                      # hr(),
                                                      hidden( 
                                                        div(id = "emissionCSVBox",
                                                            box(title = "Emission reductions file", width = 12, collapsible = F,
                                                                fileInput("emiReductionFile", "Upload emission reductions CSV file",
                                                                          multiple = FALSE,
                                                                          accept = c(".csv"),
                                                                          width = "100%")
                                                            )
                                                        )
                                                      ),
                                                      # hr(),
                                                      hidden(
                                                        div(id = "emissionREDBox",
                                                            uiOutput("emiReductionBox")
                                                        )
                                                      )
                                               )
                                               
                                               # column(width = 1)
                                             )
                                         )
                                  )
                                )
                                
                            )
                        ),
                        hr(),
                        
                        fluidRow(
                            column(width = 4, align = 'left',
                                   hidden(
                                       div(id = "exportCsvDIV",
                                           downloadBttn(outputId = "exportCsvButton",label = "Export CSV",style = "bordered",color = "primary")
                                       )
                                   ),
                                   hidden(
                                       div(id = "downloadTemplateCsvDIV",
                                           downloadBttn(outputId = "downloadTemplateCsvButton",label = "Download CSV template",style = "bordered",color = "primary")
                                       )
                                   )
                            ),
                            column(width = 8, align = 'right',
                                   actionBttn(inputId = "runScenarioButton",label = "Run Scenario",style = "bordered",color = "success",icon = icon("sync-alt"))
                            )
                        )
                ),
                
                tabItem(tabName = "dashboard",
                        
                        fluidRow(
                            column(width = 12, uiOutput("scenarioChoiceDashboard")),
                        ),
                        
                        hr(),
                        
                        h4("Emissions"),
                        fluidRow(
                            column(width = 6, highchartOutput('emiGraph')),
                            column(width = 6, highchartOutput('deltaEmiGraph'))
                        ),
                        
                        br(),
                        hr(),
                        
                        h4("Concentrations"),
                        fluidRow(
                            column(width = 6, highchartOutput('concGraph')),
                            column(width = 6, highchartOutput('deltaConcGraph'))
                        ),
                        
                        br(),
                        hr(),
                        
                        h4("Health impacts"),
                        fluidRow(
                            column(width = 4, highchartOutput('hiaAvMortGraph')),
                            column(width = 4, highchartOutput('hiaAvYLLGraph')),
                            column(width = 4, highchartOutput('hiaCostsGraph'))
                        ),
                        
                        br(),
                        hr(),
                        
                
                        
                ),
                
                tabItem(tabName = "emissions",
                        
                        fluidRow(
                            # column(width = 1),
                            column(width = 3, uiOutput("scenarioChoiceEmissions")),
                            column(width = 3, uiOutput("scenarioNameEmissions")),
                            column(width = 6, uiOutput("scenarioDescriptionEmissions")),
                            # column(width = 1)
                        ),
                        hr(),
                        fluidRow(
                            column(width = 2, 
                                   pickerInput(
                                       inputId = "pollutantChoiceEmissions",
                                       label = "Choose a specie",
                                       # width = "100%",
                                       choices = PRECURSORS_DESC)
                            ),
                            column(width = 3, 
                                   pickerInput(
                                       inputId = "sectorChoiceEmissions",
                                       label = "Choose a sector",
                                       choices = SECTORS_DESC,
                                       # width = "100%"
                                   )
                            ),
                            column(width = 2, 
                                   pickerInput(
                                       inputId = "deltaChoiceEmissions",
                                       label = "Delta/Absolute values",
                                       choices = DELTA_ABS_CHOICHES,
                                       # width = "100%"
                                   )
                            ),
                            column(width = 2, 
                                   pickerInput(
                                       inputId = "densityChoiceEmissions",
                                       label = "Tot/Density values",
                                       choices = EMI_DENS_CHOICHES,
                                       # width = "100%"
                                   )
                            ),
                            column(width = 3, 
                                   pickerInput(
                                       inputId = "aggregationChoiceEmissions",
                                       label = "Spatial aggregation",
                                       choices = SPATIAL_AGGREGATION_CHOICES,
                                       # width = "100%"
                                   )
                            )
                        ),
                        fluidRow(
                            # column(width = 1),
                            column(width = 5, id = "colBox",
                                   box(title = "Emissions", width = 12, height = 700, collapsible = F,
                                       fluidRow(
                                           column(width = 7,
                                                  awesomeRadio(
                                                      inputId = "aggregationTableEmissions",
                                                      label = "Spatial aggregation",
                                                      choices = SPATIAL_AGGREGATION_CHOICES[1:2],
                                                      inline = TRUE
                                                  )
                                           ),
                                           column(width = 5, 
                                                  align = 'right',
                                           				fluidRow(
	                                                  # searchInput(inputId = "searchTableEmissions", placeholder = "Search",
	                                                  #             btnSearch = icon("search"),
	                                                  #             btnReset = NULL
	                                                  # )
                                           					downloadBttn(
                                           						outputId = "downloadTableEmissions",
                                           						label = "Download as CSV",
                                           						style = "bordered",
                                           						color = "primary",
                                           						size = "sm",
                                           						block = TRUE
                                           					)
                                           				)
                                           )
                                       ),
                                       reactableOutput('emissionsTable')
                                   ),
                            ),
                            column(width = 7, id = "colBox",
                                   box(title = "Emission map", width = 12, height = 700, collapsible = F, maximizable = T,
                                   		div(style = "height:100%",
                                   				
	                                   		 absolutePanel(id = "mapEmiAbsPanel",
	                                   		 							class = "absoluteEmiMapPanel",
	                                   		 							fixed = TRUE,
	                                   		 							draggable = FALSE,
	                                   		 							shinyWidgets::dropdownButton(
						                                   					label = "",
						                                   					size = "sm", width = "180",
						                                   					right = TRUE, up = FALSE,
						                                   					circle = FALSE, 
						                                   					status = "bttMap",
						                                   					icon = icon("gear"), 
						                                   					tooltip = tooltipOptions(title = "Map options!"),
						                                   					
						                                   					tags$div(class="mapmenu",
							                                   					tags$div("Set Min-Max values"),
							                                   					fluidRow(
							                                   						tags$div(class = "inline", 
							                                   										 uiOutput("minValEmiMapUI"),
							                                   										 uiOutput("maxValEmiMapUI")
							                                   										 ),
							                                   						actionBttn(inputId = "updateEmiMap",label = "Update Map",icon = icon("sync-alt"), block = TRUE, size = "sm")
							                                   					),
							                                   					br(),
							                                   					tags$div("Print map"),
							                                   					fluidRow(
							                                   						actionBttn(inputId = "printEmiMap",label = "Export Map",icon = icon("print"), block = TRUE, size = "sm")
							                                   					)
						                                   					)
						                                   				)
                                   		
	                                   		 ),
                                   				
                                   				leafletOutput('emissionsMap', height = "100%")
	                                 		)
                                   ),
                            )     
                            # column(width = 1)
                        )
                        
                ),
                
                tabItem(tabName = "concentrations",
                        
                        fluidRow(
                            # column(width = 1),
                            column(width = 3, uiOutput("scenarioChoiceConcentrations")),
                            column(width = 3, uiOutput("scenarioNameConcentrations")),
                            column(width = 6, uiOutput("scenarioDescriptionConcentrations")),
                            # column(width = 1)
                        ),
                        hr(),
                        fluidRow(
                            column(width = 2, 
                                pickerInput(
                                    inputId = "pollutantChoiceConcentrations",
                                    label = "Choose a specie",
                                    # width = "100%",
                                    choices = createSpecieNames())
                            ),
                            column(width = 3, 
                                   pickerInput(
                                       inputId = "deltaChoiceConcentrations",
                                       label = "Delta/Absolute values",
                                       choices = DELTA_ABS_CHOICHES,
                                       # width = "100%"
                                   )
                            ),
                            column(width = 7, 
                                   pickerInput(
                                       inputId = "aggregationChoiceConcentrations",
                                       label = "Spatial aggregation",
                                       choices = SPATIAL_AGGREGATION_CHOICES,
                                       # width = "100%"
                                   )
                            )
                        ),
                        fluidRow(
                            # column(width = 1),
                            column(width = 5, id = "colBox",
                                   box(title = "Air quality concentrations", width = 12, height = 700, collapsible = F,
                                       fluidRow(
                                           column(width = 7,
                                               awesomeRadio(
                                                   inputId = "aggregationTableConcentrations",
                                                   label = "Spatial aggregation",
                                                   choices = SPATIAL_AGGREGATION_CHOICES[1:2],
                                                   inline = TRUE
                                               )
                                           ),
                                           column(width = 5, 
                                                  align = 'right',
                                                  # searchInput(inputId = "searchTableConcentrations", placeholder = "Search",
                                                  #             btnSearch = icon("search"),
                                                  #             # btnReset = icon("remove")
                                                  #             )
                                           			 downloadBttn(
                                           			 	outputId = "downloadTableConcentrations",
                                           			 	label = "Download as CSV",
                                           			 	style = "bordered",
                                           			 	color = "primary",
                                           			 	size = "sm",
                                           			 	block = TRUE
                                           			 )
                                           )
                                       ),
                                       reactableOutput('concentrationsTable')
                                       ),
                            ),
                            column(width = 7, id = "colBox",
                                   box(title = "Air quality map", width = 12, height = 700, collapsible = F, maximizable = T,
                                   		div(style = "height:100%",
                                   				
                                   				absolutePanel(id = "mapConcAbsPanel",
                                   											class = "absoluteConcMapPanel",
                                   											fixed = TRUE,
                                   											draggable = FALSE,
                                   											shinyWidgets::dropdownButton(
                                   												label = "",
                                   												size = "sm", width = "180",
                                   												right = TRUE, up = FALSE,
                                   												circle = FALSE, 
                                   												status = "bttMap",
                                   												icon = icon("gear"), 
                                   												tooltip = tooltipOptions(title = "Map options!"),
                                   												
                                   												tags$div(class="mapmenu",
	                                   												tags$div("Set Min-Max values"),
	                                   												fluidRow(
	                                   													tags$div(class = "inline", 
	                                   																	 uiOutput("minValConcMapUI"),
	                                   																	 uiOutput("maxValConcMapUI")
	                                   													),
	                                   													actionBttn(inputId = "updateConcMap",label = "Update Map",icon = icon("sync-alt"), block = TRUE, size = "sm")
	                                   												),
	                                   												br(),
	                                   												tags$div("Print map"),
	                                   												fluidRow(
	                                   													actionBttn(inputId = "printConcMap",label = "Export Map",icon = icon("print"), block = TRUE, size = "sm")
	                                   												)
                                   												)
                                   											)
                                   											
                                   				),
                                   				
                                   				leafletOutput('concentrationsMap', height = "100%")
                                   				
                                       )
                                   )
                            )     
                            # column(width = 1)
                        )
                        
                ),
                
                tabItem(tabName = "health",
                        
                        fluidRow(
                            # column(width = 1),
                            column(width = 3, uiOutput("scenarioChoiceHealth")),
                            column(width = 3, uiOutput("scenarioNameHealth")),
                            column(width = 6, uiOutput("scenarioDescriptionHealth")),
                            # column(width = 1)
                        ),
                        hr(),
                        fluidRow(
                            column(width = 2, 
                                   pickerInput(
                                       inputId = "varChoiceHealth",
                                       label = "Choose a variable",
                                       # width = "100%",
                                       choices = HIA_VARS)
                            ),
                            column(width = 3, 
                                   pickerInput(
                                       inputId = "densityChoiceHealth",
                                       label = "Tot/Density values",
                                       choices = HIA_DENS_CHOICHES,
                                       # width = "100%"
                                   )
                            ),
                            column(width = 7, 
                                   pickerInput(
                                       inputId = "aggregationChoiceHealth",
                                       label = "Spatial aggregation",
                                       choices = SPATIAL_AGGREGATION_CHOICES,
                                       # width = "100%"
                                   )
                            )
                        ),
                        fluidRow(
                            # column(width = 1),
                            column(width = 5, id = "colBox",
                                   box(title = "Health Impact", width = 12, height = 700, collapsible = F,
                                       # reactableOutput('healthTable')
                                       fluidRow(
                                           column(width = 7,
                                                  awesomeRadio(
                                                      inputId = "aggregationTableHealth",
                                                      label = "Spatial aggregation",
                                                      choices = SPATIAL_AGGREGATION_CHOICES[1:2],
                                                      inline = TRUE
                                                  )
                                           ),
                                           column(width = 5, 
                                                  align = 'right',
                                                  # searchInput(inputId = "searchTableHealth", placeholder = "Search",
                                                  #             btnSearch = icon("search"),
                                                  #             # btnReset = icon("remove")
                                                  # ),
                                           			 downloadBttn(
                                           			 	outputId = "downloadTableHealth",
                                           			 	label = "Download as CSV",
                                           			 	style = "bordered",
                                           			 	color = "primary",
                                           			 	size = "sm",
                                           			 	block = TRUE
                                           			 )
	                                           			 
                                           )
                                       ),
                                       reactableOutput('healthTable')
                                   )
                            ),
                            column(width = 7, id = "colBox",
                                   box(title = "Health Impact map", width = 12, height = 700, collapsible = F, maximizable = T,
                                   		div(style = "height:100%",
                                   				
                                   				absolutePanel(id = "mapHIAAbsPanel",
                                   											class = "absoluteHIAMapPanel",
                                   											fixed = TRUE,
                                   											draggable = FALSE,
                                   											shinyWidgets::dropdownButton(
                                   												label = "",
                                   												size = "sm", width = "180",
                                   												right = TRUE, up = FALSE,
                                   												circle = FALSE, 
                                   												status = "bttMap",
                                   												icon = icon("gear"), 
                                   												tooltip = tooltipOptions(title = "Map options!"),
                                   												
                                   												tags$div(class="mapmenu",
	                                   												tags$div("Set Min-Max values"),
	                                   												fluidRow(
	                                   													tags$div(class = "inline", 
	                                   																	 uiOutput("minValHIAMapUI"),
	                                   																	 uiOutput("maxValHIAMapUI")
	                                   													),
	                                   													actionBttn(inputId = "updateHIAMap",label = "Update Map",icon = icon("sync-alt"), block = TRUE, size = "sm")
	                                   												),
	                                   												br(),
	                                   												tags$div("Print map"),
	                                   												fluidRow(
	                                   													actionBttn(inputId = "printHIAMap",label = "Export Map",icon = icon("print"), block = TRUE, size = "sm")
	                                   												)
                                   												)
                                   											)
                                   											
                                   				),
                                   				
                                   				leafletOutput('healthMap', height = "100%")
                                   				
                                   		)
                                   ),
                            )     
                            # column(width = 1)
                        )
                        
                        
                ),
                
                
                tabItem(tabName = "scenarioConfig",
                				
                				fluidRow(
                					
                					column(width = 12,
                								 
                								 div(class = "tab-header",
                								 		h4("Scenario List",style = "margin: 0px"),
                								 ),
                								 # br(),
                								 div(class = "tab-table",
                									reactableOutput('scenarioConfigTable')
                								 )
                					)
                					
                				)
                				
                ),
                
                
                tabItem(tabName = "userConfig",
                				
                				fluidRow(
                					
                					column(width = 12,
                								 
                								 div(class = "tab-header",
                								 		h4("Users List",style = "margin: 0px"),
                								 ),
                								 # br(),
                								 div(class = "tab-table",
                								 		reactableOutput('userConfigTable')
                								 )
                					)
                					
                				),
                				hr(),
                				fluidRow(
                					column(width = 12, align = 'right',
                								 actionBttn(inputId = "newUserButton",label = "Create a new user",style = "bordered",color = "success",icon = icon("user"))
                								 
                					)
                				
                				)
                				
                )
                
                
            )
            
            
        )
    ),


    server = function(input, output, session) {
    	
    	#######################################################################
    	# INITIALIZE APPLICATION
    	#######################################################################
    	
        
        if (!interactive()) sink(stderr(), type = "output")
        sf::sf_use_s2(FALSE)
        
        initializeDB()
        
        if (Sys.info()['sysname']=="Linux") {
          
        	username <- Sys.getenv("SHINYPROXY_USERNAME")
        	userList <- getListUsersWithGroups(keycloackUrl,realmName)
        	usergroup <- userList$group[userList$username == username]
        	
        } else {
          
        	username <- "f.ferrari@terraria.com"
        	usergroup <- "ADMIN"
        	
        }
				
        print(paste0(Sys.time(), " - SHINYPROXY_USERNAME: ", Sys.getenv("SHINYPROXY_USERNAME")))
        print(paste0(Sys.time(), " - SHINYPROXY_USERGROUPS: ", Sys.getenv("SHINYPROXY_USERGROUPS")))
        
        print(paste0(Sys.time(), " - Login by user: ", username))
        print(paste0(Sys.time(), " - User group: ", usergroup))
        
        output$sidebar_admin <- renderUI({
        
	        if (usergroup == "ADMIN") {
	        	
	        	sidebarHeader("Admin Menu")
	        	
	        }
        
        })
        
        output$menu_admin <- renderUI({
        	
        	if (usergroup == "ADMIN") {
        		
        			bs4Dash::menuItem(
        				text = "User configuration",
        				tabName = "userConfig",
        				icon = icon("users", lib = "font-awesome")
        			)
        		
        	}
        	
        })
        
        
        user_list <- reactiveValues(
        	
        	dataframe = getListUsersWithGroups(keycloackUrl,realmName),
        	value = NULL
        	
        )
        
        output$user <- renderUser({
            
        	firstName <- user_list$dataframe[user_list$dataframe$username == username,"firstName"]
        	lastName <- user_list$dataframe[user_list$dataframe$username == username,"lastName"]
        	name <- paste(firstName,lastName)
        	
          dashboardUser(
              name = name,
              image = "user-solid.svg",
              title = name,
              # subtitle = HTML(paste0(istitute,"<br>",username)),
              subtitle = username,
               column(
                  width = 12,
                  align = "center",
                  a(actionButton("logout", "Logout"), href ="/logout"),
              )
          )
            
        })
        
        output$downloadUserGuide <- downloadHandler(
            
            filename = "SIMBAD_UserGuide.pdf",
            content = function(file) {
                file.copy(paste0(USERGUIDEDIR,"/SIMBAD_UserGuide.pdf"), file)
            }
            
        )
        
        #######################################################################
        # SCENARIO DEFINITION
        #######################################################################
        
        scenario_list <- reactiveValues(
            
            # list = as.list(unique(c("Basecase",list.dirs(path = SCENDIR, full.names = F, recursive = F)))),
            list = as.list(scenarioListfromDB(username,usergroup)),
            dataframe = readScenarioFromDB(username,usergroup),
            fromOtherUser = getScenarioPublicFromDB(username,usergroup),
            value = NULL,
            precValue = NULL,
            timer = reactiveTimer(1000), 
            started = TRUE
                
        )
        
        scenBAU <- reactiveValues(readOnly = TRUE)
        
        observe({
            
            if (length(scenario_list$list) == 0) {
                scenList <- NULL
            } else {
                scenList <- scenario_list$list
            }
            
           updatePickerInput(
              inputId = "scenarioChoice",
              choices = list(
                  `-- Create a New scenario --` = c("NoSCEN"),
                  `Upload scenario` = scenList),
              selected = scenario_list$value,
              choicesOpt = list(
                  style = list("font-weight: bold;"),
                  # style = list("font-weight: bold;","color: firebrick !important; font-style: italic;")
                  subtext = scenario_list$fromOtherUser
                  )
              )
            
        })
        
        observeEvent(input$modifyBAUScenarioButton, {
          
          shinyjs::show(id = "defineBAUScenarioDiv", anim = T, animType = "slide")
          shinyjs::hide(id = "modifyBAUScenarioDiv", anim = T, animType = "slide")
          
          choice <- match(input$defineBAUScenarioOptions,SCENARIO_DEFINITION_CHOICES)
          
          if (choice == 1) {
            shinyjs::show(id = "activityBAUCSVBox", anim = T, animType = "slide")
            shinyjs::hide(id = "emissionBAUCSVBox", anim = T, animType = "slide")
            shinyjs::hide(id = "emissionBAUREDBox", anim = T, animType = "slide")
         } else if (choice == 2) {
            shinyjs::hide(id = "activityBAUCSVBox", anim = T, animType = "slide")
            shinyjs::show(id = "emissionBAUCSVBox", anim = T, animType = "slide")
         } else {
            shinyjs::hide(id = "activityBAUCSVBox", anim = T, animType = "slide")
            shinyjs::hide(id = "emissionBAUCSVBox", anim = T, animType = "slide")
            shinyjs::show(id = "emissionBAUREDBox", anim = T, animType = "slide")
         }
          
          scenBAU$readOnly <- FALSE
          
        })
        
        observeEvent(input$returnDefaultScenarioButton, {
          
          shinyjs::hide(id = "defineBAUScenarioDiv", anim = T, animType = "slide")
          shinyjs::show(id = "modifyBAUScenarioDiv", anim = T, animType = "slide")
          shinyjs::hide(id = "activityBAUCSVBox", anim = T, animType = "slide")
          shinyjs::hide(id = "emissionBAUCSVBox", anim = T, animType = "slide")
          shinyjs::show(id = "emissionBAUREDBox", anim = T, animType = "slide")
          
          scenBAU$readOnly <- TRUE
          
        })
        
        
        observeEvent(input$defineBAUScenarioOptions, {
          
          choice <- match(input$defineBAUScenarioOptions,SCENARIO_DEFINITION_CHOICES)
          
          if (choice == 1) {
            shinyjs::show(id = "activityBAUCSVBox", anim = T, animType = "slide")
            shinyjs::hide(id = "emissionBAUCSVBox", anim = T, animType = "slide")
            shinyjs::hide(id = "emissionBAUREDBox", anim = T, animType = "slide")
            # shinyjs::hide(id = "exportCsvDIV")
            # shinyjs::show(id = "downloadTemplateCsvDIV")
            # shinyjs::removeClass(id ="downloadTemplateCsvButton_bttn", class = "disabledButton")
          } else if (choice == 2) {
            shinyjs::hide(id = "activityBAUCSVBox", anim = T, animType = "slide")
            shinyjs::show(id = "emissionBAUCSVBox", anim = T, animType = "slide")
            shinyjs::hide(id = "emissionBAUREDBox", anim = T, animType = "slide")
            # shinyjs::hide(id = "exportCsvDIV")
            # shinyjs::show(id = "downloadTemplateCsvDIV")
            # shinyjs::removeClass(id ="downloadTemplateCsvButton_bttn", class = "disabledButton")
          } else {
            shinyjs::hide(id = "activityBAUCSVBox", anim = T, animType = "slide")
            shinyjs::hide(id = "emissionBAUCSVBox", anim = T, animType = "slide")
            shinyjs::show(id = "emissionBAUREDBox", anim = T, animType = "slide")
            # shinyjs::hide(id = "exportCsvDIV")
            # shinyjs::show(id = "downloadTemplateCsvDIV")
            # shinyjs::addClass(id ="downloadTemplateCsvButton_bttn", class = "disabledButton")
          }
          
        })
        
        
        
        observe({
          
          req(input$scenarioChoice)
          # req(input$defineBAUScenarioOptions)
          
          if (is.null(input$scenarioChoice) || (input$scenarioChoice == "NoSCEN"))  {
            
            shinyjs::hide(id = "defineBAUScenarioDiv", anim = T, animType = "slide")
            shinyjs::show(id = "modifyBAUScenarioDiv", anim = T, animType = "slide")
            
          } else {

            shinyjs::hide(id = "defineBAUScenarioDiv", anim = T, animType = "slide")
            shinyjs::hide(id = "modifyBAUScenarioDiv", anim = T, animType = "slide")
            shinyjs::hide(id = "activityBAUCSVBox", anim = T, animType = "slide")
            shinyjs::hide(id = "emissionBAUCSVBox", anim = T, animType = "slide")
            shinyjs::show(id = "emissionBAUREDBox", anim = T, animType = "slide")
            
          }
          
        })
        
        
        observe({
          
          req(input$scenarioChoice)
          req(input$defineScenarioOptions)
          
          if (is.null(input$scenarioChoice) || (input$scenarioChoice == "NoSCEN"))  {
            
            shinyjs::removeClass(id ="scenarioDescription", class = "disabledText")
            shinyjs::removeClass(id ="scenarioName", class = "disabledText")
            shinyjs::addClass(id ="deleteScenarioButton", class = "disabledButton")
            shinyjs::removeClass(id ="runScenarioButton", class = "disabledButton")
            shinyjs::show(id = "defineScenarioDiv", anim = T, animType = "slide")
            shinyjs::show(id = "defineComputationDiv", anim = T, animType = "slide")
            shinyjs::hide(id = "exportCsvDIV")
            shinyjs::show(id = "downloadTemplateCsvDIV")
            
            choice <- match(input$defineScenarioOptions,SCENARIO_DEFINITION_CHOICES)
            
            if (choice == 1) {
              shinyjs::show(id = "activityCSVBox", anim = T, animType = "slide")
              shinyjs::hide(id = "emissionCSVBox", anim = T, animType = "slide")
              shinyjs::hide(id = "emissionREDBox", anim = T, animType = "slide")
              shinyjs::hide(id = "exportCsvDIV")
              shinyjs::show(id = "downloadTemplateCsvDIV")
              shinyjs::removeClass(id ="downloadTemplateCsvButton_bttn", class = "disabledButton")
            } else if (choice == 2) {
              shinyjs::hide(id = "activityCSVBox", anim = T, animType = "slide")
              shinyjs::show(id = "emissionCSVBox", anim = T, animType = "slide")
              shinyjs::hide(id = "emissionREDBox", anim = T, animType = "slide")
              shinyjs::hide(id = "exportCsvDIV")
              shinyjs::show(id = "downloadTemplateCsvDIV")
              shinyjs::removeClass(id ="downloadTemplateCsvButton_bttn", class = "disabledButton")
            } else {
              shinyjs::hide(id = "activityCSVBox", anim = T, animType = "slide")
              shinyjs::hide(id = "emissionCSVBox", anim = T, animType = "slide")
              shinyjs::show(id = "emissionREDBox", anim = T, animType = "slide")
              shinyjs::hide(id = "exportCsvDIV")
              shinyjs::show(id = "downloadTemplateCsvDIV")
              shinyjs::addClass(id ="downloadTemplateCsvButton_bttn", class = "disabledButton")
            }
            
          } else {
            
            shinyjs::addClass(id ="scenarioDescription", class = "disabledText")
            shinyjs::addClass(id ="scenarioName", class = "disabledText")
            shinyjs::removeClass(id ="deleteScenarioButton", class = "disabledButton")
            shinyjs::addClass(id ="runScenarioButton", class = "disabledButton")
            shinyjs::hide(id = "defineScenarioDiv", anim = T, animType = "slide")
            shinyjs::hide(id = "defineComputationDiv", anim = T, animType = "slide")
            shinyjs::hide(id = "activityCSVBox", anim = T, animType = "slide")
            shinyjs::hide(id = "emissionCSVBox", anim = T, animType = "slide")
            shinyjs::show(id = "emissionREDBox", anim = T, animType = "slide")
            shinyjs::show(id = "exportCsvDIV")
            shinyjs::hide(id = "downloadTemplateCsvDIV")
            
          }
          
        })
          
        
        DATASCEN <- eventReactive({
          input$scenarioChoice 
          input$scenarioYear
          }, 
          {
            
            if (is.null(input$scenarioChoice) || (input$scenarioChoice == "NoSCEN"))  {
                
                SCENARIO_NAME <- NULL
                SCENARIO_DIR <- NULL
                SCENARIO_DSCR <- NULL
                SCENARIO_YEAR <- input$scenarioYear
                SCENARIO_RED <- matrix(0, nrow = length(PRECURSORS), ncol = length(SECTORS)-1)
                
                SCENARIO_BAUDIR <- getBAUScenarioDirectory(SCENARIO_YEAR)
                
                redPerc <- read.csv2(file.path(SCENARIO_BAUDIR,"emiReductionREF.csv"), sep = ";", dec = ".")
                redPerc <- redPerc[,2:length(SECTORS)]
                SCENARIO_BAURED1 <- as.matrix(redPerc)
                
                # redPerc <- read.csv2(file.path(SCENARIO_BAUDIR,"emiReductionQ.csv"), sep = ";", dec = ".")
                # redPerc <- redPerc[,2:length(SECTORS)]
                # SCENARIO_BAURED2 <- as.matrix(redPerc)

            } else {
                
                SCENARIO_NAME <- input$scenarioChoice
                SCENARIO_DIR <- file.path(SCENDIR,getScenarioDirectoryInDB(input$scenarioChoice))
                SCENARIO_DSCR <- getScenarioDescriptionInDB(input$scenarioChoice)
                SCENARIO_YEAR <- getScenarioYearInDB(input$scenarioChoice)
                
                redPerc <- read.csv2(file.path(SCENARIO_DIR,"emiReduction.csv"), sep = ";", dec = ".")
                redPerc <- redPerc[,2:length(SECTORS)]
                SCENARIO_RED <- as.matrix(redPerc)
                
                redPerc <- read.csv2(file.path(SCENARIO_DIR,"emiReductionREF.csv"), sep = ";", dec = ".")
                redPerc <- redPerc[,2:length(SECTORS)]
                SCENARIO_BAURED1 <- as.matrix(redPerc)
                
                # redPerc <- read.csv2(file.path(SCENARIO_DIR,"emiReductionQ.csv"), sep = ";", dec = ".")
                # redPerc <- redPerc[,2:length(SECTORS)]
                # SCENARIO_BAURED2 <- as.matrix(redPerc)
                
            }
            
            return(list(SCENARIO_NAME = SCENARIO_NAME,
                        SCENARIO_DIR = SCENARIO_DIR,
                        SCENARIO_YEAR = SCENARIO_YEAR,
                        SCENARIO_DSCR = SCENARIO_DSCR,
                        SCENARIO_RED = SCENARIO_RED,
                        SCENARIO_BAURED1 = SCENARIO_BAURED1
                        # SCENARIO_BAURED2 = SCENARIO_BAURED2
                        )
                   )
            
        }, ignoreNULL = FALSE)
        
        
        observe({
          
          req(input$scenarioChoice)  
          
          if (is.null(DATASCEN()$SCENARIO_NAME)) {
            
            if (!is.null(scenario_list$precValue)) {
              
              scenYearChoiches <- seq(YEAR_STEPS[1],YEAR_STEPS[3])
              scenYearSelected <- NULL
              
              updatePickerInput(
                inputId = "scenarioYear",
                choices = scenYearChoiches,
                selected = scenYearSelected
              )
              
            }
            
            scenario_list$precValue <- NULL
            
          } else {
            
            if (is.null(scenario_list$precValue)) {
              
              scenYearChoiches <- DATASCEN()$SCENARIO_YEAR
              scenYearSelected <- DATASCEN()$SCENARIO_YEAR
              
              updatePickerInput(
                inputId = "scenarioYear",
                choices = scenYearChoiches,
                selected = scenYearSelected
              )
              
            }
            
            scenario_list$precValue <- input$scenarioChoice
            
          }

        })
        
        observe({
          
        	req(input$scenarioChoice)  
        	
          value <- DATASCEN()$SCENARIO_DSCR
          
          if (is.null(value)) {
            updateTextInput(inputId = "scenarioDescription", value = "" )
          } else {
            updateTextInput(inputId = "scenarioDescription", value = value )
          }
          
        })
        
        observe({
        	
        	req(input$scenarioChoice)  
            
          value <-  DATASCEN()$SCENARIO_NAME
          
          if (is.null(value)) {
            updateTextInput(inputId = "scenarioName", value = "" )
          } else {
            updateTextInput(inputId = "scenarioName", value = value )
          }
            
        })
        
        
        output$emiBAUReductionBox <- renderUI({

          req(input$scenarioChoice)
          req(input$scenarioYear)
          
          title <- "BAU - Emission reductions (% vs 2017)"
          redPercBAU <- DATASCEN()$SCENARIO_BAURED1
          
          if (!is.null(DATASCEN()$SCENARIO_NAME) | scenBAU$readOnly) {
            readOnly <- TRUE
          } else {
            readOnly <- FALSE
          }
          
          updateBoxKnobInput(title,"BAUred",redPercBAU,readOnly)

        })
        

        output$emiReductionBox <- renderUI({
            
          req(input$scenarioChoice)
          
          title <- "Emission reductions (% vs BAU scenario)"
          redPerc <- DATASCEN()$SCENARIO_RED
          
          if (!is.null(DATASCEN()$SCENARIO_NAME)) {
              readOnly <- TRUE
          } else {
              readOnly <- FALSE
          }
          
          updateBoxKnobInput(title,"red",redPerc,readOnly)

        })
        
        
        observeEvent(input$defineScenarioOptions, {
            
          choice <- match(input$defineScenarioOptions,SCENARIO_DEFINITION_CHOICES)
          
          if (choice == 1) {
              shinyjs::show(id = "activityCSVBox", anim = T, animType = "slide")
              shinyjs::hide(id = "emissionCSVBox", anim = T, animType = "slide")
              shinyjs::hide(id = "emissionREDBox", anim = T, animType = "slide")
              shinyjs::hide(id = "exportCsvDIV")
              shinyjs::show(id = "downloadTemplateCsvDIV")
              shinyjs::removeClass(id ="downloadTemplateCsvButton_bttn", class = "disabledButton")
          } else if (choice == 2) {
              shinyjs::hide(id = "activityCSVBox", anim = T, animType = "slide")
              shinyjs::show(id = "emissionCSVBox", anim = T, animType = "slide")
              shinyjs::hide(id = "emissionREDBox", anim = T, animType = "slide")
              shinyjs::hide(id = "exportCsvDIV")
              shinyjs::show(id = "downloadTemplateCsvDIV")
              shinyjs::removeClass(id ="downloadTemplateCsvButton_bttn", class = "disabledButton")
          } else {
              shinyjs::hide(id = "activityCSVBox", anim = T, animType = "slide")
              shinyjs::hide(id = "emissionCSVBox", anim = T, animType = "slide")
              shinyjs::show(id = "emissionREDBox", anim = T, animType = "slide")
              shinyjs::hide(id = "exportCsvDIV")
              shinyjs::show(id = "downloadTemplateCsvDIV")
              shinyjs::addClass(id ="downloadTemplateCsvButton_bttn", class = "disabledButton")
          }
          
        },ignoreInit = F, ignoreNULL = F)

        
        output$downloadTemplateCsvButton = downloadHandler(
        
            filename = function() {
                
                choice <- match(input$defineScenarioOptions,SCENARIO_DEFINITION_CHOICES)
                
                if (choice == 1) {
                    return("actReduction.csv")
                } else {
                    return("emiReduction.csv")
                }
                
            },
            content = function(file) {
                
                choice <- match(input$defineScenarioOptions,SCENARIO_DEFINITION_CHOICES)
                
                if (choice == 1) {
                    
                    fileTodownload = "actReduction.csv"
                    reportTodownload <- read.csv2(file.path(TEMPLATESDIR,fileTodownload), sep = ";", dec = ".")
                    write.table(reportTodownload, file, row.names = F, quote = F, sep = ";", dec = ".")
                    
                } else {
                    
                    fileTodownload = "emiReduction.csv"
                    reportTodownload <- read.csv2(file.path(TEMPLATESDIR,fileTodownload), sep = ";", dec = ".")
                    write.table(reportTodownload, file, row.names = F, quote = F, sep = ";", dec = ".")
                    
                }
                
            }
        )
        
        
        output$exportCsvButton = downloadHandler(
            
            filename = function() {
                paste0(DATASCEN()$SCENARIO_NAME,"_","emiReduction.csv")
            },
            
            content = function(file) {
                fileTodownload = file.path(DATASCEN()$SCENARIO_DIR,"emiReduction.csv")
                reportTodownload <- read.csv2(fileTodownload, sep = ";", dec = ".")
                write.csv2(reportTodownload, file, row.names = F, quote = F)
            }
            
        )
            
        fileEmiRed <- reactiveValues(data = NULL)
        fileActRed <- reactiveValues(data = NULL)
        fileEmiBAURed <- reactiveValues(data = NULL)
        fileActBAURed <- reactiveValues(data = NULL)
        
        observe({
          
          file <- input$emiBAUReductionFile
          
          req(file)
          ext <- tools::file_ext(file$datapath)
          shiny::validate(need(ext == "csv", "Please upload a csv file"))
          
          tryCatch({
            
            redPercBAU <- read.csv2(file$datapath, sep = ";", dec = ".")
            redPercBAU <- redPercBAU[,2:(length(SECTORS))]
            fileEmiBAURed$data <- redPercBAU
            
          }, error=function(e) {
            
            show_alert(
              title = "Error !!",
              text = "An error occurred while uploading csv file!",
              type = "error",
              closeOnClickOutside = TRUE
            )
            
            fileEmiBAURed$data <- NULL
            shinyjs::reset('emiBAUReductionFile')
            
          })
          
        })
        
        
        
        observe({
            
            file <- input$emiReductionFile
            
            req(file)
            ext <- tools::file_ext(file$datapath)
            shiny::validate(need(ext == "csv", "Please upload a csv file"))
            
            tryCatch({
                
                redPerc <- read.csv2(file$datapath, sep = ";", dec = ".")
                redPerc <- redPerc[,2:(length(SECTORS))]
                fileEmiRed$data <- redPerc
                
            }, error=function(e) {
                
                show_alert(
                    title = "Error !!",
                    text = "An error occurred while uploading csv file!",
                    type = "error",
                    closeOnClickOutside = TRUE
                )
                
                fileEmiRed$data <- NULL
                shinyjs::reset('emiReductionFile')
                
            })
            
        })
        
        observe({
            
          file <- input$actReductionFile
          
          req(file)
          ext <- tools::file_ext(file$datapath)
          shiny::validate(need(ext == "csv", "Please upload a csv file"))
          
          tryCatch({
              
              actReductionFile <- file$datapath
              redPercEmi <- computeEmissionReduction(actReductionFile)
              fileActRed$data <- redPercEmi
              
          }, error=function(e) {
              
              show_alert(
                  title = "Error !!",
                  text = "An error occurred while uploading csv file!",
                  type = "error",
                  closeOnClickOutside = TRUE
              )
              
              fileActRed$data <- NULL
              shinyjs::reset('actReductionFile')
              
          })
            
            
        })
        
        observeEvent(input$runScenarioButton, {
        	
          print(paste0(Sys.time()," - Lancio lo scenario:", input$scenarioName))
          
          if (input$scenarioName == "") {
            
            sendAlert("Inserire il nome dello scenario!")
            return(NULL)
            
          }
          
          if (input$scenarioName %in% scenarioListAllfromDB()) {
            
            print(paste0(Sys.time()," - Nome dello scenario già presente"))
            
            sendAlert("Nome dello scenario già presente!")
            return(NULL)
            
          }
          
          if (!scenBAU$readOnly) {
            
            # Scenario BAU non custom
            choiceBAU <- match(input$defineBAUScenarioOptions,SCENARIO_DEFINITION_CHOICES)
            
            if (choiceBAU == 1) {
              
              # TODO
              # req(fileActRed$data)
              # redPercBAU <- as.matrix(fileActRed$data)
              
            } else if (choiceBAU == 2) {
              
              req(fileEmiBAURed$data)
              redPercBAU <- as.matrix(fileEmiBAURed$data)
              
            } else {
              
              redPercBAU <- c()
              
              for(j in 1:length(SECTORS)){
                for (i in 1:length(PRECURSORS)) {
                  redPercBAU <- c(redPercBAU,input[[paste0("BAUred",j,"-",i)]])
                }
              }
              
              redPercBAU <- matrix(redPercBAU, nrow = length(PRECURSORS), ncol = length(SECTORS)-1) # matrice delle riduzioni%
              
            }
            
          } else {
            
            redPercBAU <- matrix(0, nrow = length(PRECURSORS), ncol = length(SECTORS_ID))
            
          }
          
          
          choice <- match(input$defineScenarioOptions,SCENARIO_DEFINITION_CHOICES)
          
          if (choice == 1) {
              
              req(fileActRed$data)
              redPerc <- as.matrix(fileActRed$data)
              
          } else if (choice == 2) {
              
              req(fileEmiRed$data)
              redPerc <- as.matrix(fileEmiRed$data)
              
          } else {
              
              redPerc <- c()
              
              for(j in 1:length(SECTORS)){
                  for (i in 1:length(PRECURSORS)) {
                      redPerc <- c(redPerc,input[[paste0("red",j,"-",i)]])
                  }
              }
              
              redPerc <- matrix(redPerc, nrow = length(PRECURSORS), ncol = length(SECTORS)-1) # matrice delle riduzioni%
              
          }
          
          # resetto il menu
          updateTextInput(session,"scenarioName", value="")
          updateTextInput(session,"scenarioDescription", value="")
          updateTextInput(session,"scenarioYear", value=YEAR_STEPS[1])
          mapply(function(j,i) updateKnobInput(session,paste0("red",j,"-",i),value=0),
                 rep(1:(length(SECTORS)-1),each = length(PRECURSORS)),
                 rep(1:length(PRECURSORS),times = length(SECTORS)-1))
          mapply(function(j,i) updateKnobInput(session,paste0("BAUred",j,"-",i),value=0),
                 rep(1:(length(SECTORS)-1),each = length(PRECURSORS)),
                 rep(1:length(PRECURSORS),times = length(SECTORS)-1))
          
          
          tryCatch({
          		
          		newScenario <- input$scenarioName
          		newDescription <- input$scenarioDescription
          		directory <- UUIDgenerate()
          		newDir <- file.path(SCENDIR,directory)
          		year <- as.numeric(input$scenarioYear)
          		defaultBAU <- scenBAU$readOnly
          		dailyComputation <- ifelse(match(input$defineComputationOptions,COMPUTATION_CHOICES) == 1, FALSE, TRUE)
          		
          		progressFile <- file.path(newDir,"progress.txt")
          		statusFile <- file.path(newDir,"status.txt")
          		
          		print(paste0(Sys.time()," - Creo la directory: ", newDir))
          		dir.create(newDir)
          		write.table(0,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
          		write.table("Inizio calcolo",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
          		
          		addScenarioInDB(newScenario, directory, year, defaultBAU, dailyComputation, newDescription, username)
          		scenario_list$dataframe <- readScenarioFromDB(username,usergroup)
          		updateReactable("scenarioConfigTable", scenario_list$dataframe, page = current_page_Scen())
          		
          		scenario_list$started <- TRUE
          		scenario_list$timer <- reactiveTimer(1000)
          		
          		showModal(
          		  modalDialogFunction(FALSE, paste0("Scenario ", newScenario , " in esecuzione!"))
          		)
          		
          		fileEmiRed$data <- NULL
          		fileActRed$data <- NULL
          		
          		shinyjs::reset('emiReductionFile')
          		shinyjs::reset('actReductionFile')
              
          		future_promise({
          		  
          		  # EMISSIONS
          		  listEMI <- computeEmissions(newScenario,year,redPerc,!defaultBAU,redPercBAU,"online",TRUE,0,10)
          		  emissionsStackBAU <- listEMI[[1]]
          		  save(emissionsStackBAU, file = file.path(newDir,"emiRasterBAU.rda"))
          		  emissionsStack <- listEMI[[2]]
          		  save(emissionsStack, file = file.path(newDir,"emiRaster.rda"))
          		  shpEmixExtraction <- interpolateRaster(newScenario,emissionsStack,"sum","EMIX",TRUE,10,20)
          		  save(shpEmixExtraction, file = file.path(newDir,"emiShp.rda"))
          		  dat <- createEmiPlotData(emissionsStack, newScenario)
          		  save(dat, file = file.path(newDir,"emiPlot.rda"))

          		  # CONCENTRATIONS 
          		  listCONC <- computeScenario(newScenario,year,dailyComputation,redPerc,!defaultBAU,redPercBAU,"online",TRUE,20,80)
          		  concentrationsStackBAU <- listCONC[[1]]
          		  save(concentrationsStackBAU, file = file.path(newDir,"concRasterBAU.rda"))
          		  concentrationsStack <- listCONC[[2]]
          		  save(concentrationsStack, file = file.path(newDir,"concRaster.rda"))
          		  
          		  redPercBAU <- listCONC[[3]]
          		  redPercBAU <- cbind(PRECURSORS,redPercBAU)
          		  colnames(redPercBAU) <- c("PREC",SECTORS[1:(length(SECTORS))])
          		  write.csv2(redPercBAU, file = paste0(newDir,"/","emiReductionBAU.csv"), row.names = F, quote = F)
          		  
          		  redPercREF <- listCONC[[4]]
          		  redPercREF <- cbind(PRECURSORS,redPercREF)
          		  colnames(redPercREF) <- c("PREC",SECTORS[1:(length(SECTORS))])
          		  write.csv2(redPercREF, file = paste0(newDir,"/","emiReductionREF.csv"), row.names = F, quote = F)
          		  
          		  redPercP <- listCONC[[5]]
          		  redPercP <- cbind(PRECURSORS,redPercP)
          		  colnames(redPercP) <- c("PREC",SECTORS[1:(length(SECTORS))])
          		  write.csv2(redPercP, file = paste0(newDir,"/","emiReductionP.csv"), row.names = F, quote = F)
          		  
          		  redPercQ <- listCONC[[6]]
          		  redPercQ <- cbind(PRECURSORS,redPercQ)
          		  colnames(redPercQ) <- c("PREC",SECTORS[1:(length(SECTORS))])
          		  write.csv2(redPercQ, file = paste0(newDir,"/","emiReductionQ.csv"), row.names = F, quote = F)
          		  
          		  shpConcExtraction <- interpolateRaster(newScenario,concentrationsStack,"mean","CONC",TRUE,80,90)
          		  save(shpConcExtraction, file = file.path(newDir,"concShp.rda"))
          		  dat <- createBoxPlotData(concentrationsStack, newScenario)
          		  save(dat, file = file.path(newDir,"concBoxPlot.rda"))
          		  
          		  # HIA
          		  hiaStack <- computeHIA(newScenario,concentrationsStack,TRUE,90,95) #6.58
                save(hiaStack, file = file.path(newDir,"hiaRaster.rda"))
                shpHiaExtraction <- interpolateRaster(newScenario,hiaStack,"sum","HIA",TRUE,95,99) #3.07
                save(shpHiaExtraction, file = file.path(newDir,"hiaShp.rda"))
                dat <- createHIAPlotData(hiaStack, newScenario)
                save(dat, file = file.path(newDir,"hiaPlot.rda"))
                
                
            }, seed = NULL) %>%
              then(
                onFulfilled = function(value) {
                  
                  scenario_list$list <- as.list(scenarioListfromDB(username,usergroup))
                  scenario_list$value <- NULL

                  progressFile <- file.path(newDir,"progress.txt")
                  statusFile <- file.path(newDir,"status.txt")
                  
                  write.table("Ok",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
                  write.table(100,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
                  print(paste0(Sys.time()," - Calcolo finito per lo scenario: ", newScenario))
                  
                  redPerc <- cbind(PRECURSORS,redPerc)
                  colnames(redPerc) <- c("PREC",SECTORS[1:(length(SECTORS)-1)])
                  write.csv2(redPerc, file = file.path(newDir,"emiReduction.csv"), row.names = F, quote = F)
                  
                },
                onRejected = function(value) {
                  
                  sendAlert(paste0("An error occurred computing scenario: ", newScenario))
                  
                  progressFile <- file.path(newDir,"progress.txt")
                  statusFile <- file.path(newDir,"status.txt")
                  
                  write.table("Errore",statusFile,row.names = FALSE,col.names = FALSE,append = FALSE)
                  write.table(100,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
                  print(paste0(Sys.time(), " - Errore per lo scenario:", newScenario))
                  print(paste0(Sys.time(), " - Errore: ", value))
                  
                  redPerc <- cbind(PRECURSORS,redPerc)
                  colnames(redPerc) <- c("PREC",SECTORS[1:(length(SECTORS)-1)])
                  write.csv2(redPerc, file = file.path(newDir,"emiReduction.csv"), row.names = F, quote = F)
                  
                })

          }, error=function(e) {
              
            sendAlert(paste0("An error occurred computing scenario: ", newScenario))
            write.table(100,progressFile,row.names = FALSE,col.names = FALSE,append = FALSE)
            
            print(e)
            # unlink(newDir, recursive = TRUE)
              
          })
          
          # Return something other than the future so we don't block the UI
          NULL
        })
        
        #######################################################################
        # DASHBOARD
        #######################################################################
        
        output$scenarioChoiceDashboard <- renderUI({
            
          pickerInput(
              inputId = "scenarioChoiceDashboard",
              label = "Select one or more scenario",
              width = "100%",
              choices = list(
                  `Select scenario` = scenario_list$list),
              multiple = TRUE,
              selected = scenario_list$list,
              options = pickerOptions(multipleSeparator = " - "),
              choicesOpt = list(
              	subtext = scenario_list$fromOtherUser
              )
          )
            
        })
        
        DATASCEN_DASH <- eventReactive(input$scenarioChoiceDashboard, {
            
          req(input$scenarioChoiceDashboard)
          
          SCENARIO_NAME <- input$scenarioChoiceDashboard
          SCENARIO_DIR <- file.path(SCENDIR,getScenarioDirectoryInDB(input$scenarioChoiceDashboard))
          
          SCENARIO_EMIX = do.call("rbind",lapply(file.path(SCENARIO_DIR,"emiPlot.rda"), function(x) get(load((x)))))
          SCENARIO_CONC = do.call("rbind",lapply(file.path(SCENARIO_DIR,"concBoxPlot.rda"), function(x) get(load((x)))))
          SCENARIO_HIA = do.call("rbind",lapply(file.path(SCENARIO_DIR,"hiaPlot.rda"), function(x) get(load((x)))))
          
          return(list(SCENARIO_NAME = SCENARIO_NAME,
                      SCENARIO_DIR = SCENARIO_DIR,
                      SCENARIO_EMIX = SCENARIO_EMIX,
                      SCENARIO_CONC = SCENARIO_CONC,
                      SCENARIO_HIA = SCENARIO_HIA
          ))
            
        }, ignoreNULL = FALSE)
        
        output$emiGraph <- renderHighchart({ 
            
          scen <- input$scenarioChoiceDashboard
          dat <- DATASCEN_DASH()$SCENARIO_EMIX
          dat <- dat[dat$name %in% scen & dat$absDel == "absolute",]
         
          dat <- dat[match(dat$name,input$scenarioChoiceDashboard),]
          
          highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_yAxis(min = 0, title = list(text = "Emissions [ton]")) %>%
              hc_add_series_list(dat) %>%
              hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
              hc_title(
                  text = "Absolute values",
                  align = "center"
                  # style = list(color = "#22A884", useHTML = TRUE)
              )  
          # hc_add_theme(hc_theme_smpl())
            
        })
        
        output$deltaEmiGraph <- renderHighchart({ 
            
          scen <- input$scenarioChoiceDashboard
          dat <- DATASCEN_DASH()$SCENARIO_EMIX
          dat <- dat[dat$name %in% scen & dat$absDel == "delta",]
          dat <- dat[match(dat$name,input$scenarioChoiceDashboard),]
          
          highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_yAxis(title = list(text = "Emissions [ton]")) %>%
              hc_add_series_list(dat) %>%
              hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
              hc_title(
                  text = "Delta values (SCEN vs BAU)",
                  align = "center" 
                  # style = list(color = "#22A884", useHTML = TRUE)
              )  
          # hc_add_theme(hc_theme_smpl())
            
        })
        
        
        output$concGraph <- renderHighchart({ 
            
          scen <- input$scenarioChoiceDashboard
          dat <- DATASCEN_DASH()$SCENARIO_CONC
          dat <- dat[dat$name %in% scen & dat$absDel == "absolute",]
          
          highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_yAxis(min = 0, title = list(text = "Concentrations [\u00b5g/m\u00b3]")) %>%
              hc_add_series_list(dat) %>%
              hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
              hc_title(
                  text = "Absolute values",
                  align = "center"
                  # style = list(color = "#22A884", useHTML = TRUE)
              )  
          # hc_add_theme(hc_theme_smpl())
            
        })
        
        output$deltaConcGraph <- renderHighchart({ 
            
          scen <- input$scenarioChoiceDashboard
          dat <- DATASCEN_DASH()$SCENARIO_CONC
          dat <- dat[dat$name %in% scen & dat$absDel == "delta",]
          
          highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_yAxis(title = list(text = "Concentrations [\u00b5g/m\u00b3]")) %>%
              hc_add_series_list(dat) %>%
              hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
              hc_title(
                  text = "Delta values (SCEN vs BAU)",
                  align = "center" 
                  # style = list(color = "#22A884", useHTML = TRUE)
              )  
          # hc_add_theme(hc_theme_smpl())
            
        })
        
        
        output$hiaAvMortGraph <- renderHighchart({ 
            
          scen <- input$scenarioChoiceDashboard
          dat <- DATASCEN_DASH()$SCENARIO_HIA
          dat <- dat[dat$name %in% scen & dat$varName == "Avoided deaths",]
          dat$type <- "bar"
      

          highchart() %>%
              hc_xAxis(type = "category", visible = F) %>%
              hc_add_series_list(dat) %>%
              hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
              hc_title(
                  text = "Avoided deaths",
                  align = "center" 
                  # style = list(color = "#22A884", useHTML = TRUE)
              )  
          
        })
        
        output$hiaAvYLLGraph <- renderHighchart({ 
            
          scen <- input$scenarioChoiceDashboard
          dat <- DATASCEN_DASH()$SCENARIO_HIA
          dat <- dat[dat$name %in% scen & dat$varName == "Avoided YOLL",]
          dat$type <- "column"

          highchart() %>%
              hc_xAxis(type = "category", visible = F) %>%
              hc_add_series_list(dat) %>%
              hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
              hc_title(
                  text = "Avoided YOLL",
                  align = "center" 
                  # style = list(color = "#22A884", useHTML = TRUE)
              )
            
        })
        
        output$hiaCostsGraph <- renderHighchart({ 
            
          scen <- input$scenarioChoiceDashboard
          dat <- DATASCEN_DASH()$SCENARIO_HIA
          dat <- dat[dat$name %in% scen & dat$varName == "Health Costs",]
          # dat$value <- (dat$value * VOLY) / 10^6
          dat$type <- "packedbubble"

          highchart() %>%
              hc_xAxis(type = "category", visible = F) %>%
              hc_add_series_list(dat) %>%
              hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadCSV" )))) %>%
              hc_title(
                  text = "Avoided Health Costs",
                  align = "center" 
                  # style = list(color = "#22A884", useHTML = TRUE)
              ) %>%
              hc_tooltip(pointFormat = '{point.value: .2f} M\u20AC')	
            
        })
        
        
        #######################################################################
        # EMISSIONS
        #######################################################################
        
        output$scenarioChoiceEmissions <- renderUI({
            
          pickerInput(
              inputId = "scenarioChoiceEmissions",
              label = "Upload a scenario",
              width = "100%",
              choices = list(
                  `Upload scenario` = scenario_list$list),
              selected = scenario_list$value,
              choicesOpt = list(
                  # style = list("color: firebrick !important; font-style: italic;"),
                  subtext = scenario_list$fromOtherUser
              )
              
          )
            
        })

        DATASCEN_EMIX <- eventReactive(input$scenarioChoiceEmissions, {
            
          req(input$scenarioChoiceEmissions)
          
          shinyjs::addClass(id ="scenarioDescriptionEmissions", class = "disabledText")
          shinyjs::addClass(id ="scenarioNameEmissions", class = "disabledText")
          
          SCENARIO_NAME <- input$scenarioChoiceEmissions
          SCENARIO_DIR <- file.path(SCENDIR,getScenarioDirectoryInDB(input$scenarioChoiceEmissions))
          SCENARIO_DSCR <- getScenarioDescriptionInDB(input$scenarioChoiceEmissions)
          
          SCENARIO_EMIXRaster = get(load(file.path(SCENARIO_DIR,"emiRaster.rda")))
          SCENARIO_EMIXShp = get(load(file.path(SCENARIO_DIR,"emiShp.rda")))
          
          return(list(SCENARIO_NAME = SCENARIO_NAME,
                      SCENARIO_DIR = SCENARIO_DIR,
                      SCENARIO_DSCR = SCENARIO_DSCR,
                      SCENARIO_EMIXRaster = SCENARIO_EMIXRaster,
                      SCENARIO_EMIXShp = SCENARIO_EMIXShp
          ))
            
        }, ignoreNULL = FALSE)
        
        output$scenarioDescriptionEmissions <- renderUI({
            
          value <- DATASCEN_EMIX()$SCENARIO_DSCR
          # placeholder <- DATASCEN()$SCENARIO_PLCH
          textInput(inputId = "scenarioDescription",label = "Scenario description",placeholder = "[Optional] Insert a scenario description...", width = '100%', value = value )
            
        })
        
        output$scenarioNameEmissions <- renderUI({
            
          value <-  DATASCEN_EMIX()$SCENARIO_NAME
          textInput(inputId = "scenarioName",label = "Scenario name",placeholder = "Insert a scenario name...", width = '100%', value = value)
            
        })
        
        
        output$emissionsMap <- renderLeaflet({
            
          leaflet() %>%
              addTiles() %>%
              setView(lng = 12.6, lat = 42.1, zoom = 5.5) %>%
      				# addFullscreenControl() %>%
			      	addEasyprint(options = easyprintOptions(
			      		hidden = TRUE,
			      		hideControlContainer = FALSE,
			      		hideClasses = "absoluteEmiMapPanel",
			      		tileWait = 2000,
			      		exportOnly = TRUE,
			      		sizeModes = list("Custom Size"=list(
			      										 	width= 800,
			      										 	height= 800,
			      										 	name = "A custom landscape size tooltip",
			      										 	className= 'customEmiCssClass')
			      		))) 
        	
        })
        
        output$minValEmiMapUI <- renderUI({
	
        	if (aggrEMIX() < 4) {

        		data <- DATASCEN_EMIX()$SCENARIO_EMIXShp[[aggrEMIX()]]
        		shpEmix <- data[,c('NAME',pollEMIX())]

        		ifelse(isDensity(densityEMIX()) & !isPercentual(delta_absEMIX()), values <- round(as.numeric(shpEmix[,2] / st_area(shp[[aggrEMIX()]]) * 10^6), digits = 3), values <- shpEmix[,2])

        	} else {

        		data <- DATASCEN_EMIX()$SCENARIO_EMIXRaster
        		rasterEmix <- data[[pollEMIX()]]

        		ifelse(!isDensity(densityEMIX()), cellArea <- 1, cellArea <- (res(rasterEmix)[1] * res(rasterEmix)[2]) / 10^6)

        		values <- raster::values(rasterEmix) / cellArea

        	}

      		value <- min(values)
      		textInput(inputId = "minValEmiMap", label = "Min:", value = value )
        	
        })
        
        output$maxValEmiMapUI <- renderUI({

        	if (aggrEMIX() < 4) {
        		
        		data <- DATASCEN_EMIX()$SCENARIO_EMIXShp[[aggrEMIX()]]
        		shpEmix <- data[,c('NAME',pollEMIX())]
        		
        		ifelse(isDensity(densityEMIX()) & !isPercentual(delta_absEMIX()), values <- round(as.numeric(shpEmix[,2] / st_area(shp[[aggrEMIX()]]) * 10^6), digits = 3), values <- shpEmix[,2])
        		
        	} else {
        		
        		data <- DATASCEN_EMIX()$SCENARIO_EMIXRaster
        		rasterEmix <- data[[pollEMIX()]]
        		
        		ifelse(!isDensity(densityEMIX()), cellArea <- 1, cellArea <- (res(rasterEmix)[1] * res(rasterEmix)[2]) / 10^6)
        		
        		values <- raster::values(rasterEmix) / cellArea
        		
        	}
        	
        	value <- max(values)
        	textInput(inputId = "maxValEmiMap", label = "Max:", value = value )
        	
        })

        observeEvent(input$updateEmiMap, {
        		
        	poll <- input$pollutantChoiceEmissions

        	ifelse(!isDensity(densityEMIX()), units <- "[ton]", units <- "[ton/km\u00b2]")
        	if (isPercentual(delta_absEMIX())) units <- "[\u0025]"
        	title <- paste(poll, units)
        	
        	if (aggrEMIX() < 4) {
        		
        		data <- DATASCEN_EMIX()$SCENARIO_EMIXShp[[aggrEMIX()]]
        		shpEmix <- data[,c('NAME',pollEMIX())]
        		
        		ifelse(isDensity(densityEMIX()) & !isPercentual(delta_absEMIX()), values <- round(as.numeric(shpEmix[,2] / st_area(shp[[aggrEMIX()]]) * 10^6), digits = 3), values <- shpEmix[,2])

        		# verifico assegnazione corretta dei valori        		
        		if (is.null(input$minValEmiMap) | is.na(as.numeric(input$minValEmiMap)) ) {
        			updateTextInput(session, "minValEmiMap", value = min(values))
        			minVal <- min(values)
        			
        		} else {
        			
        			minVal <- as.numeric(input$minValEmiMap)
        			
        		}
        		
        		# if (is.null(input$maxValEmiMap) | as.numeric(input$maxValEmiMap) > max(values) | as.numeric(input$maxValEmiMap) < min(values) | is.na(as.numeric(input$maxValEmiMap)) ) {
        		if (is.null(input$maxValEmiMap) | is.na(as.numeric(input$maxValEmiMap)) ) {
        				
        			updateTextInput(session, "maxValEmiMap", value = max(values))
        			maxVal <- max(values)
        			
        		} else {
        			
        			maxVal <- as.numeric(input$maxValEmiMap)
        			
        		}
        		
        		if (isAbsolute(delta_absEMIX())) {
        			pal <- colorNumeric("YlOrRd", c(minVal,maxVal), na.color="transparent")
        		} else {
        			pal <- colorNumeric("YlOrRd", c(minVal,maxVal), na.color="transparent", reverse = T)
        		}
        		
        		labels <- sprintf("<strong>%s</strong><br/>%s: %g %s", shpEmix$NAME, poll, values, units) %>% lapply(htmltools::HTML)
        		
        		values[values <= minVal] <- minVal
        		values[values >= maxVal] <- maxVal
        		
        		shapefile <- shp[[aggrEMIX()]]
        		
        		updatePolyMap("emissionsMap",pal,labels,shapefile,values,c(minVal,maxVal),title,"customEmiCssClass","absoluteEmiMapPanel")
        		
        	} else {
        		
        		data <- DATASCEN_EMIX()$SCENARIO_EMIXRaster
        		rasterEmix <- data[[pollEMIX()]]
        		
        		ifelse(!isDensity(densityEMIX()), cellArea <- 1, cellArea <- (res(rasterEmix)[1] * res(rasterEmix)[2]) / 10^6)
        		
        		values <- raster::values(rasterEmix) / cellArea
        		
        		# verifico assegnazione corretta dei valori        		
        		if (is.null(input$minValEmiMap) | is.na(as.numeric(input$minValEmiMap)) ) {
        			
        			updateTextInput(session, "minValEmiMap", value = min(values))
        			minVal <- min(values)
        			
        		} else {
        			
        			minVal <- as.numeric(input$minValEmiMap)
        			
        		}
        		
        		if (is.null(input$maxValEmiMap) | is.na(as.numeric(input$maxValEmiMap)) ) {
        			
        			updateTextInput(session, "maxValEmiMap", value = max(values))
        			maxVal <- max(values)
        			
        		} else {
        			
        			maxVal <- as.numeric(input$maxValEmiMap)
        			
        		}
        		
        		values[values <= minVal] <- minVal + 0.000001
        		values[values >= maxVal] <- maxVal - 0.000001
        		
        		rasterMapEmix <- rasterEmix/cellArea
        		rasterMapEmix[rasterMapEmix <= minVal] <- minVal + 0.000001
        		rasterMapEmix[rasterMapEmix >= maxVal] <- maxVal - 0.000001
        		
        		# rasterMapEmix <- raster::projectRaster(rasterMapEmix,crs = "+init=epsg:3857")
        		
        		if (isAbsolute(delta_absEMIX())) {
        			pal <- colorNumeric("RdYlBu", values, na.color="transparent", reverse = T)
        		} else {
        			pal <- colorNumeric("RdYlBu", values, na.color="transparent")
        		}
        		
        		updateRasterMap("emissionsMap",pal,rasterMapEmix,values,c(minVal,maxVal),title,"customEmiCssClass","absoluteEmiMapPanel")
        		
        	}
        	
        })
        
        observeEvent(input$printEmiMap, {
        	
        	leafletProxy("emissionsMap") %>%
        		easyprintMap(sizeModes="customEmiCssClass")

        })
        
        aggrEMIX <- reactive({
          match(input$aggregationChoiceEmissions,c(SPATIAL_AGGREGATION_SHP,"Gridded"))
        })
        
        aggrEMIXTable <- reactive({
          match(input$aggregationTableEmissions,SPATIAL_AGGREGATION_SHP)
        })
        
        delta_absEMIX <- reactive({
          match(input$deltaChoiceEmissions,DELTA_ABS_CHOICHES)
        })
        
        densityEMIX <- reactive({
          match(input$densityChoiceEmissions,EMI_DENS_CHOICHES)
        })
        
        pollEMIX <- reactive({
          pollEMIX <- PRECURSORS[match(input$pollutantChoiceEmissions,PRECURSORS_DESC)]
          sectEMIX <- SECTORS_ID[match(input$sectorChoiceEmissions,SECTORS_DESC)]
          
          prefix <- ""
          if (delta_absEMIX() == 2) prefix <- "bau_"
          if (delta_absEMIX() == 3) prefix <- "delta_"
          if (delta_absEMIX() == 4) prefix <- "deltaP_"
          
          pollEMIX <- paste0(prefix,pollEMIX,"_",sectEMIX)
          pollEMIX
        })
        
        
        observe({

          poll <- input$pollutantChoiceEmissions
          
          ifelse(!isDensity(densityEMIX()), units <- "[ton]", units <- "[ton/km\u00b2]")
          if (isPercentual(delta_absEMIX())) units <- "[\u0025]"
          title <- paste(poll, units)
          
          if (aggrEMIX() < 4) {
              
              data <- DATASCEN_EMIX()$SCENARIO_EMIXShp[[aggrEMIX()]]
              shpEmix <- data[,c('NAME',pollEMIX())]
              
              ifelse(isDensity(densityEMIX()) & !isPercentual(delta_absEMIX()), values <- round(as.numeric(shpEmix[,2] / st_area(shp[[aggrEMIX()]]) * 10^6), digits = 3), values <- shpEmix[,2])
              
              if (isAbsolute(delta_absEMIX())) {
              	pal <- colorNumeric("YlOrRd", values, na.color="transparent")
              } else {
              	pal <- colorNumeric("YlOrRd", values, na.color="transparent", reverse = T)
              }
              
              labels <- sprintf("<strong>%s</strong><br/>%s: %g %s", shpEmix$NAME, poll, values, units) %>% lapply(htmltools::HTML)
              
              shapefile <- shp[[aggrEMIX()]]
              
              updatePolyMap("emissionsMap",pal,labels,shapefile,values,c(min(values),max(values)),title,"customEmiCssClass","absoluteEmiMapPanel")
              
          } else {
              
              data <- DATASCEN_EMIX()$SCENARIO_EMIXRaster
              rasterEmix <- data[[pollEMIX()]]
              
              ifelse(!isDensity(densityEMIX()), cellArea <- 1, cellArea <- (res(rasterEmix)[1] * res(rasterEmix)[2]) / 10^6)
              
              values <- raster::values(rasterEmix) / cellArea
              
              if (isAbsolute(delta_absEMIX())) {
              	pal <- colorNumeric("RdYlBu", values, na.color="transparent", reverse = T)
              } else {
              	pal <- colorNumeric("RdYlBu", values, na.color="transparent")
              }
              
              updateRasterMap("emissionsMap",pal,rasterEmix/cellArea,values,c(min(values, na.rm = T),max(values, na.rm = T)),title,"customEmiCssClass","absoluteEmiMapPanel")
              
          }
            
        }) 
        
        observe({
            
          click <- input$emissionsMap_click
          poll <- input$pollutantChoiceEmissions
          
          if (aggrEMIX() < 4) return(NULL)
          
          if (!is.null(click)) {
              
              data <- DATASCEN_EMIX()$SCENARIO_EMIXRaster
              rasterEmix <- data[[pollEMIX()]]
              
              ifelse(!isDensity(densityEMIX()), cellArea <- 1, cellArea <- (res(rasterEmix)[1] * res(rasterEmix)[2]) / 10^6)
              ifelse(!isDensity(densityEMIX()), units <- "[ton]", units <- "[ton/km\u00b2]")
              if (isPercentual(delta_absEMIX())) units <- "[\u0025]"
              # title <- paste(poll, units)
              
              xy <- data.frame(lon = click$lng, lat = click$lat)
              coordinates(xy) <- ~ lon + lat
              crs(xy) <- paste("+proj=longlat +datum=WGS84", sep="")
              
              emiValue <- raster::extract(rasterEmix, spTransform(xy, crs(rasterEmix))) / cellArea
              
              if (is.na(emiValue)) return(NULL)
              
              popup <- htmltools::HTML(sprintf("Long: %g Lat: %g<br/><strong>%s</strong>: %g %s", click$lng, click$lat, poll, round(emiValue, digits = 1), units))
              
              leafletProxy("emissionsMap") %>% 
                  clearPopups() %>% 
                  addPopups(click$lng, click$lat, popup = popup, 
                            options = popupOptions(closeOnClick = TRUE,
                                                   keepInView = FALSE)
                            )
              
          }
            
        }) 
        
        emissionsTable <- reactive({
          shpEmix <- DATASCEN_EMIX()$SCENARIO_EMIXShp[[aggrEMIXTable()]]
          data <- shpEmix[,c('NAME','POP_2011',pollEMIX())]
          
          ifelse(isDensity(densityEMIX()) & !isPercentual(delta_absEMIX()), data[,3] <- round(as.numeric(data[,3] / st_area(shp[[aggrEMIXTable()]]) * 10^6), digits = 3), data[,3] <- data[,3])
          
          if (isTruthy(input$searchTableEmissions)) {
              data <- data %>% 
                  filter(grepl(input$searchTableEmissions, NAME, ignore.case = T))
          } 
          
          data
        })
        
        output$downloadTableEmissions = downloadHandler(
        	
        	filename = function() {
        		paste0("exportEmissionTable.csv")
        	},
        	
        	content = function(file) {
        		
        		shpEmix <- emissionsTable()
        		poll <- PRECURSORS[match(input$pollutantChoiceEmissions,PRECURSORS_DESC)]
        		ifelse(!isDensity(densityEMIX()), units <- "[ton]", units <- "[ton/km\u00b2]")
        		if (isPercentual(delta_absEMIX())) units <- "[\u0025]"
        		title <- paste(poll, units)
        		names(shpEmix) <- c(input$aggregationTableEmissions,'Population',title)
        		
        		write.table(shpEmix, file, row.names = F, quote = F, dec = ".", sep = ";")
        		
        	}
        	
        )
        
        
        output$emissionsTable <- renderReactable({
            
          shpEmix <- emissionsTable()
          poll <- input$pollutantChoiceEmissions
          ifelse(!isDensity(densityEMIX()), units <- "[ton]", units <- "[ton/km\u00b2]")
          if (isPercentual(delta_absEMIX())) units <- "[\u0025]"
          title <- paste(poll, units)
          
          names(shpEmix) <- c(input$aggregationTableEmissions,'Population','POLL')
          
          reactable(shpEmix,
                    defaultSorted = 'POLL',
                    compact = TRUE,
                    pagination = TRUE,
          					paginationType = "simple",
          					wrap = FALSE,
                    # searchable = TRUE,
                    defaultPageSize = 15,
                    defaultColDef = colDef(headerClass = "header", align = "left"),
                    columns = list(
                        Population = colDef(
                            # name = poll,
                            # defaultSortOrder = "desc",
                            cell = function(value) {
                                width <- paste0(value * 100 / max(shpEmix$Population, na.rm = T), "%")
                                value <- format(value, big.mark = ",")
                                value <- format(value, width = 9, justify = "right")
                                bar <- div(
                                    class = "bar-chart",
                                    style = list(marginRight = "6px"),
                                    div(class = "bar", style = list(width = width, backgroundColor = "#3fc1c9"))
                                )
                                div(class = "bar-cell", span(class = "number", value), bar)
                            }
                        ),
                        POLL = colDef(
                            name = title,
                            defaultSortOrder = "desc",
                            align = "center",
                            cell = function(value) {
                                width <- paste0(value * 100 / ifelse(max(shpEmix$POLL)==0,100,max(shpEmix$POLL)), "%")
                                # value <- format(value, big.mark = ",")
                                value <- format(value, width = 9, justify = "right")
                                bar <- div(
                                    class = "bar-chart",
                                    style = list(marginRight = "6px"),
                                    div(class = "bar", style = list(width = width, backgroundColor = "#fc5185"))
                                )
                                div(class = "bar-cell", span(class = "number", value), bar)
                            }
                        )
                        
                    )
          )
            
        })
        
        
        
        #######################################################################
        # CONCENTRATIONS
        #######################################################################
        
        output$scenarioChoiceConcentrations <- renderUI({
            
          pickerInput(
              inputId = "scenarioChoiceConcentrations",
              label = "Upload a scenario",
              width = "100%",
              choices = list(
                  `Upload scenario` = scenario_list$list),
              selected = scenario_list$value,
              choicesOpt = list(
                  # style = c("color: firebrick !important; font-weight: bold;"))
                  # style = list("color: firebrick !important; font-style: italic;"),
                  subtext = scenario_list$fromOtherUser
              )
              
          )
            
        })
        
        DATASCEN_CONC <- eventReactive(input$scenarioChoiceConcentrations, {
            
          req(input$scenarioChoiceConcentrations)
          
          shinyjs::addClass(id ="scenarioDescriptionConcentrations", class = "disabledText")
          shinyjs::addClass(id ="scenarioNameConcentrations", class = "disabledText")
          
          SCENARIO_NAME <- input$scenarioChoiceConcentrations
          SCENARIO_DIR <- file.path(SCENDIR,getScenarioDirectoryInDB(input$scenarioChoiceConcentrations))
          SCENARIO_DSCR <- getScenarioDescriptionInDB(input$scenarioChoiceConcentrations)
          
          SCENARIO_CONCRaster = get(load(file.path(SCENARIO_DIR,"concRaster.rda")))
          SCENARIO_CONCShp = get(load(file.path(SCENARIO_DIR,"concShp.rda")))
          
          return(list(SCENARIO_NAME = SCENARIO_NAME,
                      SCENARIO_DIR = SCENARIO_DIR,
                      SCENARIO_DSCR = SCENARIO_DSCR,
                      SCENARIO_CONCRaster = SCENARIO_CONCRaster,
                      SCENARIO_CONCShp = SCENARIO_CONCShp
                      ))
            
        }, ignoreNULL = FALSE)
        
        output$scenarioDescriptionConcentrations <- renderUI({
            
          value <- DATASCEN_CONC()$SCENARIO_DSCR
          # placeholder <- DATASCEN()$SCENARIO_PLCH
          textInput(inputId = "scenarioDescription",label = "Scenario description",placeholder = "[Optional] Insert a scenario description...", width = '100%', value = value )
            
        })
        
        output$scenarioNameConcentrations <- renderUI({
            
          value <-  DATASCEN_CONC()$SCENARIO_NAME
          textInput(inputId = "scenarioName",label = "Scenario name",placeholder = "Insert a scenario name...", width = '100%', value = value)
            
        })
        
        observe({
          
          req(input$pollutantChoiceConcentrations)
          
          # se ho i superamenti visualizzo solo i valori assoluti
          
          if (getStat(input$pollutantChoiceConcentrations) == "# daily exceed") {
            
            updatePickerInput(
              inputId = "deltaChoiceConcentrations",
              choices = DELTA_ABS_CHOICHES[1]
            )
            
          } else {
            
            updatePickerInput(
              inputId = "deltaChoiceConcentrations",
              choices = DELTA_ABS_CHOICHES
            )
            
          }
          
        })
        
        observe({
          
          req(input$pollutantChoiceConcentrations)
          
          # se ho i superamenti ho solo le mappe comunali e grigliate
          
          if (getStat(input$pollutantChoiceConcentrations) == "# daily exceed") {
            
            updatePickerInput(
              inputId = "aggregationChoiceConcentrations",
              choices = SPATIAL_AGGREGATION_CHOICES[3:4]
            )
            
          } else {
            
            updatePickerInput(
              inputId = "aggregationChoiceConcentrations",
              choices = SPATIAL_AGGREGATION_CHOICES
            )
            
          }
          
        })
        
        
        output$concentrationsMap <- renderLeaflet({
        	
        	leaflet() %>%
        		addTiles() %>%
        		setView(lng = 12.6, lat = 42.1, zoom = 5.5) %>%
        		# addFullscreenControl() %>%
        		addEasyprint(options = easyprintOptions(
        			hidden = TRUE,
        			hideControlContainer = FALSE,
        			hideClasses = "absoluteConcMapPanel",
        			tileWait = 2000,
        			exportOnly = TRUE,
        			sizeModes = list("Custom Size"=list(
        											 	width= 800,
        											 	height= 800,
        											 	name = "A custom landscape size tooltip",
        											 	className= 'customConcCssClass')
        			))) 
        	
        })
        
        output$minValConcMapUI <- renderUI({
        	
        	if (aggrCONC() < 4) {
        		
        		data <- DATASCEN_CONC()$SCENARIO_CONCShp[[aggrCONC()]]
        		shpConc <- data[,c('NAME',poll())]
        		
        		values <- shpConc[,2]
        		
        	} else {
        		
        		data <- DATASCEN_CONC()$SCENARIO_CONCRaster
        		rasterConc <- data[[poll()]]
        		values <- raster::values(rasterConc)
        		
        	}

        	value <- min(values)
        	textInput(inputId = "minValConcMap", label = "Min:", value = value )
        	
        })
        
        output$maxValConcMapUI <- renderUI({
        	
        	if (aggrCONC() < 4) {
        		
        		data <- DATASCEN_CONC()$SCENARIO_CONCShp[[aggrCONC()]]
        		shpConc <- data[,c('NAME',poll())]
        		
        		values <- shpConc[,2]
        		
        	} else {
        		
        		data <- DATASCEN_CONC()$SCENARIO_CONCRaster
        		rasterConc <- data[[poll()]]
        		values <- raster::values(rasterConc)
        		
        	}
        	
        	value <- max(values)
        	textInput(inputId = "maxValConcMap", label = "Max:", value = value )
        	
        })
        
        observeEvent(input$updateConcMap, {
        	
        	units <- getSpecieUnit(input$pollutantChoiceConcentrations)
        	poll <- input$pollutantChoiceConcentrations

        	if (isPercentual(delta_abs())) units <- "[\u0025]"
        	title <- paste(poll, units)
        	
        	if (aggrCONC() < 4) {
        		
        		data <- DATASCEN_CONC()$SCENARIO_CONCShp[[aggrCONC()]]
        		shpConc <- data[,c('NAME',poll())]
        		values <- shpConc[,2]
        		
        		# verifico assegnazione corretta dei valori 
        		if (is.null(input$minValConcMap) | is.na(as.numeric(input$minValConcMap)) ) {
        			
        			updateTextInput(session, "minValConcMap", value = min(values))
        			minVal <- min(values)
        			
        		} else {
        			
        			minVal <- as.numeric(input$minValConcMap)
        			
        		}
        		
        		if (is.null(input$maxValConcMap) | is.na(as.numeric(input$maxValConcMap)) ) {
        			
        			updateTextInput(session, "maxValConcMap", value = max(values))
        			maxVal <- max(values)
        			
        		} else {
        			
        			maxVal <- as.numeric(input$maxValConcMap)
        			
        		}
        		
        		if (isAbsolute(delta_abs())) {
        			pal <- colorNumeric("YlOrRd", c(minVal,maxVal), na.color="transparent")
        		} else {
        			pal <- colorNumeric("YlOrRd", c(minVal,maxVal), na.color="transparent", reverse = T)
        		}
        		
        		labels <- sprintf("<strong>%s</strong><br/>%s: %g %s", shpConc$NAME, poll, values, units) %>% lapply(htmltools::HTML)
        		
        		values[values <= minVal] <- minVal
        		values[values >= maxVal] <- maxVal
        		
        		shapefile <- shp[[aggrCONC()]]
        		
        		updatePolyMap("concentrationsMap",pal,labels,shapefile,values,c(minVal,maxVal),title,"customConcCssClass","absoluteConcMapPanel")
        		
        	} else {
        		
        		data <- DATASCEN_CONC()$SCENARIO_CONCRaster
        		rasterConc <- data[[poll()]]
        		values <- raster::values(rasterConc)
        		
        		# verifico assegnazione corretta dei valori 
        		if (is.null(input$minValConcMap) | is.na(as.numeric(input$minValConcMap)) ) {
        			
        			updateTextInput(session, "minValConcMap", value = min(values))
        			minVal <- min(values)
        			
        		} else {
        			
        			minVal <- as.numeric(input$minValConcMap)
        			
        		}
        		
        		if (is.null(input$maxValConcMap) | is.na(as.numeric(input$maxValConcMap)) ) {
        			
        			updateTextInput(session, "maxValConcMap", value = max(values))
        			maxVal <- max(values)
        			
        		} else {
        			
        			maxVal <- as.numeric(input$maxValConcMap)
        			
        		}

        		values[values <= minVal] <- minVal + 0.000001
        		values[values >= maxVal] <- maxVal - 0.000001
        		
        		rasterMapConc <- rasterConc
        		rasterMapConc[rasterMapConc <= minVal] <- minVal + 0.000001
        		rasterMapConc[rasterMapConc >= maxVal] <- maxVal - 0.000001
        		
        		if (isAbsolute(delta_abs())) {
        			pal <- colorNumeric("RdYlBu", c(minVal,maxVal), na.color="transparent", reverse = T)
        		} else {
        			pal <- colorNumeric("RdYlBu", c(minVal,maxVal), na.color="transparent")
        		}
        		
        		updateRasterMap("concentrationsMap",pal,rasterMapConc,values,c(minVal,maxVal),title,"customConcCssClass","absoluteConcMapPanel")

        	}

        })
        
        observeEvent(input$printConcMap, {
        	
        	leafletProxy("concentrationsMap") %>%
        		easyprintMap(sizeModes="customConcCssClass")

        })

        aggrCONC <- reactive({
          match(input$aggregationChoiceConcentrations,c(SPATIAL_AGGREGATION_SHP,"Gridded"))
        })
        
        aggrTable <- reactive({
          match(input$aggregationTableConcentrations,SPATIAL_AGGREGATION_SHP)
        })
        
        delta_abs <- reactive({
          match(input$deltaChoiceConcentrations,DELTA_ABS_CHOICHES)
        })
        
        poll <- reactive({
          
          poll <- getSpecie(input$pollutantChoiceConcentrations)
          if (delta_abs() == 2) poll <- paste0("bau_",poll)
          if (delta_abs() == 3) poll <- paste0("delta_",poll)
          if (delta_abs() == 4) poll <- paste0("deltaP_",poll)
          if (getStat(input$pollutantChoiceConcentrations) == "# daily exceed") poll <- paste0(poll,"_dylExceed")
          
          poll
          
        })
        

        observe({
            
            units <- getSpecieUnit(input$pollutantChoiceConcentrations)
            poll <- input$pollutantChoiceConcentrations
            
            if (isPercentual(delta_abs())) units <- "[\u0025]"
            title <- paste(poll, units)
            
            if (aggrCONC() < 4) {
                
              data <- DATASCEN_CONC()$SCENARIO_CONCShp[[aggrCONC()]]
              shpConc <- data[,c('NAME',poll())]
              values <- shpConc[,2]
              
              if (isAbsolute(delta_abs())) {
              	pal <- colorNumeric("YlOrRd", values, na.color="transparent")
              } else {
              	pal <- colorNumeric("YlOrRd", values, na.color="transparent", reverse = T)
              }
              
              labels <- sprintf("<strong>%s</strong><br/>%s: %g %s", shpConc$NAME, poll, values, units) %>% lapply(htmltools::HTML)
              
              shapefile <- shp[[aggrCONC()]]
              
              updatePolyMap("concentrationsMap",pal,labels,shapefile,values,c(min(values),max(values)),title,"customConcCssClass","absoluteConcMapPanel")
                
            } else {
                
              data <- DATASCEN_CONC()$SCENARIO_CONCRaster
              rasterConc <- data[[poll()]]
              values <- raster::values(rasterConc)
              
              if (isAbsolute(delta_abs())) {
              	pal <- colorNumeric("RdYlBu", values, na.color="transparent", reverse = T)
              } else {
              	pal <- colorNumeric("RdYlBu", values, na.color="transparent")
              }
              
               updateRasterMap("concentrationsMap",pal,rasterConc,values,c(min(values, na.rm = T),max(values, na.rm = T)),title,"customConcCssClass","absoluteConcMapPanel")
                
            }
            
        }) 
        
        observe({
            
          click <- input$concentrationsMap_click
          units <- getSpecieUnit(input$pollutantChoiceConcentrations)
          poll <- input$pollutantChoiceConcentrations
          
          if (isPercentual(delta_abs())) units <- "[\u0025]"
          
          if (aggrCONC() < 4) return(NULL)
          
          if (!is.null(click)) {
              
              data <- DATASCEN_CONC()$SCENARIO_CONCRaster
              rasterConc <- data[[poll()]]
              
              xy <- data.frame(lon = click$lng, lat = click$lat)
              coordinates(xy) <- ~ lon + lat
              crs(xy) <- paste("+proj=longlat +datum=WGS84", sep="")

              concValue <- raster::extract(rasterConc, spTransform(xy, crs(rasterConc)))
              
              if (is.na(concValue)) return(NULL)
              
              popup <- htmltools::HTML(sprintf("Long: %g Lat: %g<br/><strong>%s</strong>: %g %s", click$lng, click$lat, poll, concValue, units))
              
              leafletProxy("concentrationsMap") %>% 
                  clearPopups() %>% 
                  addPopups(click$lng, click$lat, popup = popup, 
                            options = popupOptions(closeOnClick = TRUE,
                                                   keepInView = FALSE)
                            )
              
          }
            
        }) # e. of observeEvent
        
        concentrationsTable <- reactive({
          
          data <- DATASCEN_CONC()$SCENARIO_CONCShp[[aggrTable()]]
          data <- data[,c('NAME','POP_2011',poll())]
          
          if (isTruthy(input$searchTableConcentrations)) {
              data <- data %>% 
                  filter(grepl(input$searchTableConcentrations, NAME, ignore.case = T))
          } 
          
          data
        })
        
        output$downloadTableConcentrations = downloadHandler(
        	
        	filename = function() {
        		paste0("exportConcentrationsTable.csv")
        	},
        	
        	content = function(file) {
        		
        		shpConc <- concentrationsTable()
        		units <- getSpecieUnit(input$pollutantChoiceConcentrations)
        		poll <- SPECIES[match(input$pollutantChoiceConcentrations,SPECIES_DESC)]
        		
        		if (isPercentual(delta_abs())) units <- "[%]"
        		title <- paste(poll, units)
        		
        		names(shpConc) <- c(input$aggregationTableConcentrations,'Population',title)
        		
        		write.table(shpConc, file, row.names = F, quote = F, dec = ".", sep = ";", fileEncoding = "UTF-16LE")
        		
        	}
        	
        )
        
        output$concentrationsTable <- renderReactable({
            
          shpConc <- concentrationsTable()
          units <- getSpecieUnit(input$pollutantChoiceConcentrations)
          poll <- input$pollutantChoiceConcentrations
          
          if (isPercentual(delta_abs())) units <- "[\u0025]"
          title <- paste(poll, units)
          
          names(shpConc) <- c(input$aggregationTableConcentrations,'Population','POLL')
          
          reactable(shpConc,
                    defaultSorted = 'POLL',
                    compact = TRUE,
                    pagination = TRUE,
          					paginationType = "simple",
          					wrap = FALSE,
                    # searchable = TRUE,
                    defaultPageSize = 15,
                    defaultColDef = colDef(headerClass = "header", align = "left"),
                    columns = list(
                        Population = colDef(
                            # name = poll,
                            # defaultSortOrder = "desc",
                            cell = function(value) {
                                width <- paste0(value * 100 / max(shpConc$Population), "%")
                                value <- format(value, big.mark = ",")
                                value <- format(value, width = 9, justify = "right")
                                bar <- div(
                                    class = "bar-chart",
                                    style = list(marginRight = "6px"),
                                    div(class = "bar", style = list(width = width, backgroundColor = "#3fc1c9"))
                                )
                                div(class = "bar-cell", span(class = "number", value), bar)
                            }
                        ),
                        POLL = colDef(
                            name = title,
                            defaultSortOrder = "desc",
                            align = "center",
                            cell = function(value) {
                                width <- paste0(value * 100 / ifelse(max(shpConc$POLL)==0,100,max(shpConc$POLL)), "%")
                                # value <- format(value, big.mark = ",")
                                value <- format(value, width = 9, justify = "right")
                                bar <- div(
                                    class = "bar-chart",
                                    style = list(marginRight = "6px"),
                                    div(class = "bar", style = list(width = width, backgroundColor = "#fc5185"))
                                )
                                div(class = "bar-cell", span(class = "number", value), bar)
                            }
                        )

                    )
          )
            
        })

        
        #######################################################################
        # HEALTH
        #######################################################################
        
        output$scenarioChoiceHealth <- renderUI({
            
          pickerInput(
              inputId = "scenarioChoiceHealth",
              label = "Upload a scenario",
              width = "100%",
              choices = list(
                  `Upload scenario` = scenario_list$list),
              selected = scenario_list$value,
              choicesOpt = list(
                  # style = c("color: firebrick !important; font-weight: bold;"))
                  # style = list("color: firebrick !important; font-style: italic;"),
                  subtext = scenario_list$fromOtherUser
              )
              
          )
            
        })
        
        DATASCEN_HEALTH <- eventReactive(input$scenarioChoiceHealth, {
            
          req(input$scenarioChoiceHealth)
          
          shinyjs::addClass(id ="scenarioDescriptionHealth", class = "disabledText")
          shinyjs::addClass(id ="scenarioNameHealth", class = "disabledText")
          
          SCENARIO_NAME <- input$scenarioChoiceHealth
          SCENARIO_DIR <- file.path(SCENDIR,getScenarioDirectoryInDB(input$scenarioChoiceHealth))
          SCENARIO_DSCR <- getScenarioDescriptionInDB(input$scenarioChoiceHealth)
          
          SCENARIO_HIARaster = get(load(file.path(SCENARIO_DIR,"hiaRaster.rda")))
          SCENARIO_HIAShp = get(load(file.path(SCENARIO_DIR,"hiaShp.rda")))
          # SCENARIO_CONCShp = get(load(paste0(SCENARIO_DIR,"/","concShp.rda")))
          
          return(list(SCENARIO_NAME = SCENARIO_NAME,
                      SCENARIO_DIR = SCENARIO_DIR,
                      SCENARIO_DSCR = SCENARIO_DSCR,
                      SCENARIO_HIARaster = SCENARIO_HIARaster,
                      SCENARIO_HIAShp = SCENARIO_HIAShp
                      # SCENARIO_CONCShp = SCENARIO_CONCShp
          ))
            
        }, ignoreNULL = FALSE)
        
        output$scenarioDescriptionHealth <- renderUI({
            
          value <- DATASCEN_HEALTH()$SCENARIO_DSCR
          textInput(inputId = "scenarioDescription",label = "Scenario description",placeholder = "[Optional] Insert a scenario description...", width = '100%', value = value )
            
        })
        
        output$scenarioNameHealth <- renderUI({
            
            value <-  DATASCEN_HEALTH()$SCENARIO_NAME
            textInput(inputId = "scenarioName",label = "Scenario name",placeholder = "Insert a scenario name...", width = '100%', value = value)
            
        })
        
        output$healthMap <- renderLeaflet({
            
        	leaflet() %>%
        		addTiles() %>%
        		setView(lng = 12.6, lat = 42.1, zoom = 5.5) %>%
        		# addFullscreenControl() %>%
        		addEasyprint(options = easyprintOptions(
        			hidden = TRUE,
        			hideControlContainer = FALSE,
        			hideClasses = "absoluteHIAMapPanel",
        			tileWait = 2000,
        			exportOnly = TRUE,
        			sizeModes = list("Custom Size"=list(
        				width= 800,
        				height= 800,
        				name = "A custom landscape size tooltip",
        				className= 'customHIACssClass')
        			))) 
        	
        })
        
        output$minValHIAMapUI <- renderUI({
        	
        	if (aggrHIA() < 4) {
        		
        		colHIA <- match(input$varChoiceHealth,HIA_VARS) + 2
        		
        		data <- DATASCEN_HEALTH()$SCENARIO_HIAShp[[aggrHIA()]]
        		shpHIA <- data[,c(1:2,colHIA)]
        		
        		ifelse(densityHIA() == 1, values <- shpHIA[,3], values <- round(shpHIA[,3]/shpHIA[,2]*10^5, digits = 1))
        		
        	} else {
        		
        		colHIA <- match(input$varChoiceHealth,HIA_VARS)
        		popHIA <- length(HIA_VARS) + 1
        		
        		data <- DATASCEN_HEALTH()$SCENARIO_HIARaster
        		rasterHIA <- data[[colHIA]]
        		
        		if (densityHIA() == 1) {
        			
        			values <- raster::values(rasterHIA)

        		} else {
        			
        			population <- data[[popHIA]]
        			values <- round(raster::values(rasterHIA) / raster::values(population) * 10^5, digits = 1)

        		}        		
        	}
        	
        	value <- min(values, na.rm = T)
        	textInput(inputId = "minValHIAMap", label = "Min:", value = value )
        	
        })
        
        output$maxValHIAMapUI <- renderUI({
        	
        	if (aggrHIA() < 4) {
        		
        		colHIA <- match(input$varChoiceHealth,HIA_VARS) + 2
        		
        		data <- DATASCEN_HEALTH()$SCENARIO_HIAShp[[aggrHIA()]]
        		shpHIA <- data[,c(1:2,colHIA)]
        		
        		ifelse(densityHIA() == 1, values <- shpHIA[,3], values <- round(shpHIA[,3]/shpHIA[,2]*10^5, digits = 1))
        		
        	} else {
        		
        		colHIA <- match(input$varChoiceHealth,HIA_VARS)
        		popHIA <- length(HIA_VARS) + 1
        		
        		data <- DATASCEN_HEALTH()$SCENARIO_HIARaster
        		rasterHIA <- data[[colHIA]]
        		
        		if (densityHIA() == 1) {
        			
        			values <- raster::values(rasterHIA)
        			
        		} else {
        			
        			population <- data[[popHIA]]
        			values <- round(raster::values(rasterHIA) / raster::values(population) * 10^5, digits = 1)
        			
        		}        		
        	}
        	
        	value <- max(values, na.rm = T)
        	textInput(inputId = "maxValHIAMap", label = "Max:", value = value )
        	
        })
        
        observeEvent(input$updateHIAMap, {
        	
        	varHIA <- input$varChoiceHealth

        	if (aggrHIA() < 4) {
        		
        		colHIA <- match(input$varChoiceHealth,HIA_VARS) + 2
        		
        		data <- DATASCEN_HEALTH()$SCENARIO_HIAShp[[aggrHIA()]]
        		shpHIA <- data[,c(1:2,colHIA)]
        		
        		ifelse(densityHIA() == 1, values <- shpHIA[,3], values <- round(shpHIA[,3]/shpHIA[,2]*10^5, digits = 1))
        		ifelse(densityHIA() == 1, units <- "[inhabitants]", units <- "[per 100.000 inh]")
        		
        		# verifico assegnazione corretta dei valori 
        		if (is.null(input$minValHIAMap) | is.na(as.numeric(input$minValHIAMap)) ) {
        			
        			updateTextInput(session, "minValHIAMap", value = min(values, na.rm = T))
        			minVal <- min(values, na.rm = T)
        			
        		} else {
        			
        			minVal <- as.numeric(input$minValHIAMap)
        			
        		}
        		
        		if (is.null(input$maxValHIAMap) | is.na(as.numeric(input$maxValHIAMap)) ) {
        			
        			updateTextInput(session, "maxValHIAMap", value = max(values, na.rm = T))
        			maxVal <- max(values, na.rm = T)
        			
        		} else {
        			
        			maxVal <- as.numeric(input$maxValHIAMap)
        			
        		}
        		
        		
        		pal <- colorNumeric("YlOrRd", c(minVal,maxVal), na.color="transparent")
        		labels <- sprintf(paste0("<strong>%s</strong><br/>%s: %g ",units), shpHIA$NAME, varHIA, values) %>% lapply(htmltools::HTML)
        		
        		values[values <= minVal] <- minVal
        		values[values >= maxVal] <- maxVal
        		
        		shapefile <- shp[[aggrHIA()]]
        		title <- varHIA
        		
        		updatePolyMap("healthMap",pal,labels,shapefile,values,c(minVal,maxVal),title,"customHIACssClass","absoluteHIAMapPanel")
        		
        	} else {
        		
        		colHIA <- match(input$varChoiceHealth,HIA_VARS)
        		popHIA <- length(HIA_VARS) + 1
        		
        		data <- DATASCEN_HEALTH()$SCENARIO_HIARaster
        		rasterHIA <- data[[colHIA]]
        		
        		title <- varHIA
        		
        		if (densityHIA() == 1) {
        			
        			values <- raster::values(rasterHIA)
        			
        		} else {
        			
        			population <- data[[popHIA]]
        			values <- round(raster::values(rasterHIA) / raster::values(population) * 10^5, digits = 1)

        		}
        		
        		# verifico assegnazione corretta dei valori 
        		if (is.null(input$minValHIAMap) | is.na(as.numeric(input$minValHIAMap)) ) {
        			
        			updateTextInput(session, "minValHIAMap", value = min(values, na.rm = T))
        			minVal <- min(values, na.rm = T)
        			
        		} else {
        			
        			minVal <- as.numeric(input$minValHIAMap)
        			
        		}
        		
        		if (is.null(input$maxValHIAMap) | is.na(as.numeric(input$maxValHIAMap)) ) {
        			
        			updateTextInput(session, "maxValHIAMap", value = max(values, na.rm = T))
        			maxVal <- max(values, na.rm = T)
        			
        		} else {
        			
        			maxVal <- as.numeric(input$maxValHIAMap)
        			
        		}
        		
        		values[values <= minVal] <- minVal
        		values[values >= maxVal] <- maxVal
        		
        		pal <- colorNumeric("RdYlBu", c(minVal,maxVal), na.color="transparent", reverse = T)
        		
        		if (densityHIA() == 1) {
        			
        			rasterMapHIA <- rasterHIA
        			rasterMapHIA[rasterMapHIA <= minVal] <- minVal + 0.000001
        			rasterMapHIA[rasterMapHIA >= maxVal] <- maxVal - 0.000001
        			
        			updateRasterMap("healthMap",pal,rasterMapHIA,values,c(minVal,maxVal),title,"customConcCssClass","absoluteConcMapPanel")
        			
        		} else {
        			
        			rasterMapHIA <- rasterHIA/population*10^5
        			rasterMapHIA[rasterMapHIA <= minVal] <- minVal + 0.000001
        			rasterMapHIA[rasterMapHIA >= maxVal] <- maxVal - 0.000001
        			
        			updateRasterMap("healthMap",pal,rasterMapHIA,values,c(minVal,maxVal),title,"customHIACssClass","absoluteHIAMapPanel")
        			
        		}
        		
        	}
        	
        })
        
        observeEvent(input$printHIAMap, {
        	
        	leafletProxy("healthMap") %>%
        		easyprintMap(sizeModes="customHIACssClass")
        	
        })
        
        aggrHIA <- reactive({
          match(input$aggregationChoiceHealth, c(SPATIAL_AGGREGATION_SHP,"Gridded"))
        })
        
        aggrHIATable <- reactive({
          match(input$aggregationTableHealth, SPATIAL_AGGREGATION_SHP)
        })
        
        densityHIA <- reactive({
          match(input$densityChoiceHealth,HIA_DENS_CHOICHES)
        })
        
        observe({
            
          varHIA <- input$varChoiceHealth
          
          if (aggrHIA() < 4) {
              
              colHIA <- match(input$varChoiceHealth,HIA_VARS) + 2
              
              data <- DATASCEN_HEALTH()$SCENARIO_HIAShp[[aggrHIA()]]
              shpHIA <- data[,c(1:2,colHIA)]
              
              ifelse(densityHIA() == 1, values <- shpHIA[,3], values <- round(shpHIA[,3]/shpHIA[,2]*10^5, digits = 1))
              ifelse(densityHIA() == 1, units <- "[inhabitants]", units <- "[per 100.000 inh]")
              
              pal <- colorNumeric("YlOrRd", values, na.color="transparent")
              labels <- sprintf(paste0("<strong>%s</strong><br/>%s: %g ",units), shpHIA$NAME, varHIA, values) %>% lapply(htmltools::HTML)
              
              shapefile <- shp[[aggrHIA()]]
              title <- varHIA
              
              updatePolyMap("healthMap",pal,labels,shapefile,values,c(min(values),max(values)),title,"customHIACssClass","absoluteHIAMapPanel")
              
          } else {
              
              colHIA <- match(input$varChoiceHealth,HIA_VARS)
              popHIA <- length(HIA_VARS) + 1
              
              data <- DATASCEN_HEALTH()$SCENARIO_HIARaster
              rasterHIA <- data[[colHIA]]
              
              title <- varHIA
              
              if (densityHIA() == 1) {
                  
                  values <- raster::values(rasterHIA)
                  pal <- colorNumeric("RdYlBu", values, na.color="transparent", reverse = T)
                  
                  updateRasterMap("healthMap",pal,rasterHIA,values,c(min(values,na.rm = T),max(values,na.rm = T)),title,"customHIACssClass","absoluteHIAMapPanel")
                  
              } else {
                  
                  population <- data[[popHIA]]
                  values <- round(raster::values(rasterHIA) / raster::values(population) * 10^5, digits = 1)
                  
                  pal <- colorNumeric("RdYlBu", values, na.color="transparent", reverse = T)
                  
                  updateRasterMap("healthMap",pal,rasterHIA/population*10^5,values,c(min(values,na.rm = T),max(values,na.rm = T)),title,"customHIACssClass","absoluteHIAMapPanel")
                  
              }
              
                  
              
              
          }
            
        }) 
        
        observe({
            
          click <- input$healthMap_click
          varHIA <- input$varChoiceHealth
          colHIA <- match(input$varChoiceHealth,HIA_VARS)
          popHIA <- length(HIA_VARS) + 1
          
          if (aggrHIA() < 4) return(NULL)
          
          if (!is.null(click)) {
              
              data <- DATASCEN_HEALTH()$SCENARIO_HIARaster
              rasterHIA <- data[[colHIA]]
              population <- data[[popHIA]]
              
              xy <- data.frame(lon = click$lng, lat = click$lat)
              coordinates(xy) <- ~ lon + lat
              crs(xy) <- paste("+proj=longlat +datum=WGS84", sep="")

              xy <- spTransform(xy, crs(rasterEmix))

              ifelse(densityHIA() == 1, hiaValue <- round(raster::extract(rasterHIA, xy), digits = 1), hiaValue <- round(raster::extract(rasterHIA, xy) /  raster::extract(population, xy) * 10^5,digits = 1) )
              ifelse(densityHIA() == 1, units <- "[inhabitants]", units <- "[per 100.000 inh]")
              
              if (is.na(hiaValue)) return(NULL)
              
              popup <- htmltools::HTML(sprintf(paste0("Long: %g Lat: %g<br/><strong>%s</strong>: %g ",units), click$lng, click$lat, varHIA, hiaValue))
              
              leafletProxy("healthMap") %>% 
                  clearPopups() %>% 
                  addPopups(click$lng, click$lat, popup = popup, 
                            options = popupOptions(closeOnClick = TRUE,
                                                   keepInView = FALSE)
                            )
              
          }
            
        }) # e. of observeEvent
        
 
        healthTable <- reactive({
          
          shpHIA <- DATASCEN_HEALTH()$SCENARIO_HIAShp[[aggrHIATable()]]
          colHIA <- match(input$varChoiceHealth,HIA_VARS) + 2
          
          data <- shpHIA[,c(1:2,colHIA)]
          
          ifelse(densityHIA() == 1, data[,3] <- data[,3], data[,3] <- round(data[,3]/data[,2]*10^5, digits = 1))
          
          if (isTruthy(input$searchTableHealth)) {
              data <- data %>% 
                  filter(grepl(input$searchTableHealth, NAME, ignore.case = T))
          } 
          
          data
          
        })
        
        output$downloadTableHealth = downloadHandler(
        	
        	filename = function() {
        		paste0("exportHealthTable.csv")
        	},
        	
        	content = function(file) {
        		
        		var <- input$varChoiceHealth
        		
        		shpHIA <- healthTable()
        		ifelse(densityHIA() == 1, units <- "[inhabitants]", units <- "[per 100.000 inh]")
        		title <- paste(var, units)
        		
        		names(shpHIA) <- c(input$aggregationTableHealth,'Population',title)
        		
        		write.table(shpHIA, file, row.names = F, quote = F, dec = ".", sep = ";", fileEncoding = "UTF-16LE")
        		
        	}
        	
        )
        
        
        output$healthTable <- renderReactable({
            
          var <- input$varChoiceHealth
          
          shpHIA <- healthTable()
          ifelse(densityHIA() == 1, units <- "[inhabitants]", units <- "[per 100.000 inh]")
          
          names(shpHIA) <- c(input$aggregationTableHealth,'Population','VAR')
          
          reactable(shpHIA,
                    defaultSorted = 'VAR',
                    compact = TRUE, 
                    pagination = TRUE,
          					paginationType = "simple",
          					wrap = FALSE,
                    # searchable = TRUE,
                    defaultPageSize = 15,
                    defaultColDef = colDef(headerClass = "header", align = "left"),
                    columns = list(
                        Population = colDef(
                            # name = poll,
                            # defaultSortOrder = "desc",
                            cell = function(value) {
                                width <- paste0(value * 100 / max(shpHIA$Population), "%")
                                value <- format(value, big.mark = ",")
                                value <- format(value, width = 9, justify = "right")
                                bar <- div(
                                    class = "bar-chart",
                                    style = list(marginRight = "6px"),
                                    div(class = "bar", style = list(width = width, backgroundColor = "#3fc1c9"))
                                )
                                div(class = "bar-cell", span(class = "number", value), bar)
                            }
                        ),
                        VAR = colDef(
                            name = paste(var, units),
                            defaultSortOrder = "desc",
                            align = "center",
                            cell = function(value) {
                                width <- paste0(value * 100 / ifelse(max(shpHIA$VAR)==0,100,max(shpHIA$VAR)), "%")
                                # value <- format(value, big.mark = ",")
                                value <- format(value, width = 9, justify = "right")
                                bar <- div(
                                    class = "bar-chart",
                                    style = list(marginRight = "6px"),
                                    div(class = "bar", style = list(width = width, backgroundColor = "#fc5185"))
                                )
                                div(class = "bar-cell", span(class = "number", value), bar)
                            }
                        )
                        
                    )
          )
            
        })
        
        
        #######################################################################
        # SCENARIO CONFIGURATION
        #######################################################################
        
        observe({
          
          scenario_list$timer()
          
          isolate({
            
            if (scenario_list$started) {
              
              progressList <- getScenarioProgressInDB()
              
              if (any(is.na(progressList))) {
                return(NULL)
              }
              
              if (all(progressList == 100)) {
                
                # stoppo il refresh
                scenario_list$timer <- reactiveTimer(Inf)
                scenario_list$started <- FALSE
                
              } else {
                
                updateProgressScenario()
                updateStatusScenario()
                
              }
              
              scenario_list$dataframe <- readScenarioFromDB(username,usergroup)
              scenario_list$list <- as.list(scenarioListfromDB(username,usergroup))
              # updateReactable("scenarioConfigTable", scenario_list$dataframe, page = current_page_Scen())
              
            }
            
          })
          
        })
        
        
        output$scenarioConfigTable <- renderReactable({
      		
        	scenarioDF <- scenario_list$dataframe
        	
        	reactable(scenarioDF,
        						compact = TRUE, 
        						pagination = TRUE,
        						wrap = FALSE,
        						searchable = TRUE,
        						striped = TRUE,
        						highlight = TRUE,
        						bordered = TRUE,
        						defaultPageSize = 15,
        						defaultColDef = colDef(headerClass = "header", align = "left", vAlign = "center", headerVAlign = "center", headerStyle = "min-height: 40px"),
        						class = "scen-table",
        						columns = list(
        							scenarioName = reactable::colDef(maxWidth = 200, name = "Scenario"),
        							year = reactable::colDef(maxWidth = 60, name = "Year"),
        							defaultBAU = reactable::colDef(show = FALSE),
        							dailyComputation = reactable::colDef(show = FALSE),
        							directory = reactable::colDef(show = FALSE),
        							description = reactable::colDef(name = "Description",
        							                                cell = function(value) {
        							                                  div(style = "cursor: help",
        							                                      tippy(value, value))
        							                                }),
        							dateCreation = reactable::colDef(name = "Creation Date", maxWidth = 150),
        							userName = reactable::colDef(name = "User", maxWidth = 150),
        							progress = colDef(
        							  name = "Progress",
        							  # defaultSortOrder = "desc",
        							  cell = JS('function(cellInfo) {
                        // Format as percentage
                        const pct = (cellInfo.value).toFixed(1) + "%"
                        // Pad single-digit numbers
                        let value = pct.padStart(5)
                        let bck = cellInfo.row.status === "Errore" ? "#ff0000" : "#28a745"
                        // Render bar chart
                        return `
                          <div class="bar-cell">
                            <span class="number">${value}</span>
                            <div class="bar-chart" style="background-color: #e1e1e1">
                              <div class="bar" style="width: ${pct}; background-color: ${bck}"></div>
                            </div>
                          </div>
                        `
                      }'),
        							  html = TRUE
        							),
        							status = colDef(
        							  name = "Status",
        							  width = 200,
        							  cell = function(value) {
        							    if (value  == "Ok" )
        							      div(icon("check", class = "fas", style = "color:green;padding-right: 5px"), "Elaborazione terminata")
        							    else if (value  == "Errore" )
        							      div(icon("xmark", class = "fas", style = "color:red;padding-right: 5px"), "Si è verificato un errore")
        							    else
        							      div(icon("rotate", class = "fas", style = "color:orange;padding-right: 5px"), value)
        							  }),
        							isPublic = reactable::colDef(sortable = FALSE, html = TRUE, name = "Public", maxWidth = 80, align = "center", cell = function(value) {
        								if (is.na(value)) {
        									htmltools::tags$div(class = "disabledTableCell")
        								} else if (value) {	
        									htmltools::tags$div(shiny::actionButton("scenList_Public",label = NULL, icon = icon("star", class="fa-solid", style = "color: #28a745")))
        								} else {
        									htmltools::tags$div(shiny::actionButton("scenList_Public",label = NULL, icon = icon("star")))
        								}
        							}) ,
        							UpdateBtn = reactable::colDef(sortable = FALSE, html = TRUE, name = "Edit", maxWidth = 80, align = "center", cell = function(value) {
        								if (is.na(value)) {
        									htmltools::tags$div(class = "disabledTableCell")
        								} else {
        									htmltools::tags$div(shiny::actionButton("scenList_Update",label = NULL, icon = icon("pen-to-square")))	
        								}
        							}) ,
        							DeleteBtn = reactable::colDef(sortable = FALSE, html = TRUE, name = "Remove", maxWidth = 80, align = "center", cell = function(value) {
        								if (is.na(value)) {
        									htmltools::tags$div(class = "disabledTableCell")
        								} else {
        									htmltools::tags$div(shiny::actionButton("scenList_Delete",label = NULL, icon = icon("trash-alt")))
        								}
        							}) 
        						),
        						onClick = JS("function(rowInfo, column) {
															    if (column.id !== 'UpdateBtn' && column.id !== 'DeleteBtn' && column.id !== 'isPublic') {
															      return
															    }
															
															    if (window.Shiny) {
															      Shiny.setInputValue('scenarioConfigAction', { index: rowInfo.index + 1, button: column.id }, { priority: 'event' })
															    }
															  }"),
        						theme = reactableTheme(
        							backgroundColor = "white",
        							borderColor = "#dfe2e5",
        							stripedColor = "#f6f8fa",
        							highlightColor = "#f0f5f9",
        							cellPadding = "4px 6px",
        							style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        							searchInputStyle = list(width = "100%")
        						) 
        							
        					)
        	
        	
        })
        
        selected_row_Scen <- reactive({
        	input$scenarioConfigAction$index
        })
        
        selected_button <- reactive({
        	input$scenarioConfigAction$button
        })
        
        current_page_Scen <- reactive({
        	getReactableState("scenarioConfigTable")$page
        })
        
        observeEvent(input$scenarioConfigAction, {
       		
        	action <- input$scenarioConfigAction$button
        	button <- scenario_list$dataframe[selected_row_Scen(), selected_button()]
        	
        	scenName <- scenario_list$dataframe[selected_row_Scen(), "scenarioName"]
        	scenDesc <- scenario_list$dataframe[selected_row_Scen(), "description"]
        	newStatus <- !scenario_list$dataframe[selected_row_Scen(), "isPublic"]
        	
        	if (action == "isPublic" & !is.na(button)) {
        		
        		showModal(
        			modalDialog(
        				title = "Confirm",
        				span("Are you sure you want to change status for scenario ", tags$b(scenName), "?"),
        				if (newStatus) {
        					div(tags$i("The scenario will be available to all users!", style = "color: grey;"))
        				},
        				easyClose = FALSE,
        				footer = tagList(
        					modalButton("Cancel"),
        					actionButton("changePublicStatus", "OK")
        				)
        			)
        		)
        		
        	} else if (action == "UpdateBtn" & !is.na(button)) {
        		
        		showModal(
        			modalDialog(
        				title = "Edit Scenario",
        				# p(values$dataframe[selected_row_Scen(), ]),
        				textInput(inputId = "newScenarioName",
        									label = "Scenario Name",
        									value = scenName),
        				textInput(inputId = "newDescription",
        									label = "Description",
        									value = scenDesc),
        				easyClose = FALSE,
        				footer = tagList(
        					modalButton("Cancel"),
        					actionButton("saveUpdateScenario", "Save")
        					)
        			)
        		)
        		
        	} else if (action == "DeleteBtn" & !is.na(button)) {
        		
        		showModal(
        			modalDialog(
        				title = "Confirm",
        				span("Are you sure, you want to delete scenario ", tags$b(scenName), "?"),
        				easyClose = FALSE,
        				footer = tagList(
        					modalButton("Cancel"),
        					actionButton("deleteScenario", "OK")
        				)
        			)
        		)
        			
        	}
        	
        })
        
        observeEvent(input$changePublicStatus, {
        	
        	newStatus <- !scenario_list$dataframe[selected_row_Scen(), "isPublic"]
        	updateScenarioPublicInDB(scenario_list$dataframe[selected_row_Scen(), "scenarioName"], newStatus)
        	
        	# scenario_list$dataframe[selected_row_Scen(), "isPublic"] <- newStatus
        	scenario_list$dataframe <- readScenarioFromDB(username,usergroup)
        	scenario_list$list <- as.list(scenarioListfromDB(username,usergroup))
        	scenario_list$fromOtherUser <- getScenarioPublicFromDB(username,usergroup)
	        updateReactable("scenarioConfigTable", scenario_list$dataframe, page = current_page_Scen())
        	
        	removeModal()
        	
        })
        
        observeEvent(input$saveUpdateScenario, {
        	
        	oldDir <- file.path(SCENDIR,scenario_list$dataframe[selected_row_Scen(), "scenarioName"])
        	newDir <- file.path(SCENDIR,input$newScenarioName)
        	file.rename(oldDir,newDir)
        	
        	## aggiorno i grafici della dashboard
        	data <- get(load(file.path(newDir,"emiPlot.rda")))
        	data$name <- input$newScenarioName
        	save(data , file = file.path(newDir,"emiPlot.rda"))
        	data <- get(load(file.path(newDir,"concBoxPlot.rda")))
        	data$name <- input$newScenarioName
        	save(data , file = file.path(newDir,"concBoxPlot.rda"))
        	data <- get(load(file.path(newDir,"hiaPlot.rda")))
        	data$name <- input$newScenarioName
        	save(data , file = file.path(newDir,"hiaPlot.rda"))
        	
        	updateScenarioInfoInDB(scenario_list$dataframe[selected_row_Scen(), "scenarioName"],input$newScenarioName,input$newDescription)
        	
        	# scenario_list$dataframe[selected_row_Scen(), "scenarioName"] <- input$newScenarioName
        	# scenario_list$dataframe[selected_row_Scen(), "description"] <- input$newDescription
        	scenario_list$dataframe <- readScenarioFromDB(username,usergroup)
        	scenario_list$list <- as.list(scenarioListfromDB(username,usergroup))
        	scenario_list$fromOtherUser <- getScenarioPublicFromDB(username,usergroup)
        	
        	updateReactable("scenarioConfigTable", scenario_list$dataframe, page = current_page_Scen())
        	
        	removeModal()
        	
        })
        
        observeEvent(input$deleteScenario, {
        	
          scenarioName <- scenario_list$dataframe[selected_row_Scen(), "scenarioName"]
          
        	dirToRemove <- getScenarioDirectoryInDB(scenarioName)
        	dirToRemove <- file.path(SCENDIR,dirToRemove)
        	print(paste0(Sys.time(), " - Scenario to remove:", dirToRemove))
        	unlink(dirToRemove, recursive = TRUE)
        	
        	deleteScenarioInDB(scenarioName)
        	
        	# scenario_list$dataframe <- scenario_list$dataframe[-selected_row_Scen(),]
        	scenario_list$dataframe <- readScenarioFromDB(username,usergroup)
        	scenario_list$list <- as.list(scenarioListfromDB(username,usergroup))
        	scenario_list$fromOtherUser <- getScenarioPublicFromDB(username,usergroup)
        	
        	updateReactable("scenarioConfigTable", scenario_list$dataframe, page = current_page_Scen())
        	
        	removeModal()
        	
        	
        })

        
        
        #######################################################################
        # USER CONFIGURATION
        #######################################################################
        
        observeEvent(input$newUserButton, {
        
        	showModal(newUserModal(NULL))
        
        })
        
        
        observeEvent(input$saveNewUser, {
        	
        	usersList <- getListUsers(keycloackUrl,realmName)
        	
        	if (input$UserName %in% usersList$username) {

        		showModal(
        			modalDialog(
        				title = "Error",
        				div(tags$b("Error: username already exist!", style = "color: red;")),
        				easyClose = TRUE,
        				footer = tagList(
        					modalButton("Close"),
        				)
        			)
        		)

        		return(NULL)

        	}

        	if (input$Email %in% usersList$email) {

        		showModal(
        			modalDialog(
        				title = "Error",
        				div(tags$b("Error: email already exist!", style = "color: red;")),
        				easyClose = TRUE,
        				footer = tagList(
        					modalButton("Close"),
        				)
        			)
        		)

        		return(NULL)

        	}
        	
        	userData <- list(username = input$UserName,
        									 email = input$Email,
        									 firstName = input$FirstName,
        									 lastName = input$LastName
        	)
        	
        	userPsw <- password(n = 10, numbers = TRUE, case = TRUE, special = c("?", "!", "&", "%", "$"))
        	userGroup <- input$Group
        	
        	
        	# addUserInDB(input$UserName,input$Group)
        	
        	##########
        	# SEND EMAIL!!
        	
        	from <- sprintf(paste0("<",userMailAdmin,">"))
        	to <- sprintf(paste0("<",input$Email,">"))
        	
        	subject <- "SIMBAD - new user"
        	body <- paste0("Dear ", input$FirstName, " ", input$LastName, 
        								 ",\nwelcome in SIMBAD!\n\nHere below you can find your credentials:\n - username: ", input$UserName,
        								 "\n - password: ",userPsw,"\n\nExplore the app at: ",urlSimbad)
        	
        	status <- tryCatch({
									        		status <- sendmail(from,to,subject,body,control=list(smtpServer= smtpServer))
									        		status$code
									        		},
        										 error=function(cond) return(999)
        										 )

        	print(paste0(Sys.time()," - Send mail new user. Status: ", status))
        	
        	if (status=="221") {
 
        		createNewUser(keycloackUrl,realmName,userData,userPsw,userGroup)
        		user_list$dataframe <- getListUsersWithGroups(keycloackUrl,realmName)
        		updateReactable("userConfigTable", user_list$dataframe, page = current_page_User())
        		
        		showModal(newUserModal(failed = FALSE))
        		
        	} else {
        		
        		showModal(newUserModal(failed = TRUE))
        		
        	}
        	
        	##########
        	
        	# removeModal()
        	
        })
        
        
        output$userConfigTable <- renderReactable({
        	
        	userDF <- user_list$dataframe
        	
        	userDF <- userDF[,c("username", "firstName", "lastName", "email", "createdTimestamp", "group")]
        	
        	# userDF$createdTimestamp <- strftime(as.Date(as.POSIXct(userDF$createdTimestamp/1000, origin="1970-01-01")), format = "%d %b %Y")
        	userDF$ResetPsw_User_Btn <- NA
        	userDF$Update_User_Btn <- NA
        	userDF$Delete_User_Btn <- NA
        	
        	reactable(userDF,
        						compact = TRUE, 
        						pagination = TRUE,
        						wrap = FALSE,
        						searchable = TRUE,
        						striped = TRUE,
        						highlight = TRUE,
        						bordered = TRUE,
        						defaultPageSize = 10,
        						defaultColDef = colDef(headerClass = "header", align = "left", vAlign = "center", headerVAlign = "center", headerStyle = "min-height: 40px"),
        						class = "scen-table",
        						columns = list(
        							username = reactable::colDef(maxWidth = 150, name = "Username"),
        							firstName = reactable::colDef(name = "First Name"),
        							lastName = reactable::colDef(name = "Last Name"),
        							email = reactable::colDef(name = "Email"),
        							createdTimestamp = reactable::colDef(name = "Creation Date", cell = function(value) strftime(as.Date(as.POSIXct(value/1000, origin="1970-01-01")), format = "%d %b %Y") ),
        							group = reactable::colDef(name = "Group"),
        							ResetPsw_User_Btn = reactable::colDef(sortable = FALSE, html = TRUE, name = "Reset Psw", maxWidth = 80, align = "center", cell = function() htmltools::tags$div(shiny::actionButton("userList_ResetPsw",label = NULL, icon = icon("key")))),
        							Update_User_Btn = reactable::colDef(sortable = FALSE, html = TRUE, name = "Edit", maxWidth = 60, align = "center", cell = function() htmltools::tags$div(shiny::actionButton("userList_Update",label = NULL, icon = icon("pen-to-square")))),
        							Delete_User_Btn = reactable::colDef(sortable = FALSE, html = TRUE, name = "Remove", maxWidth = 80, align = "center", cell = function() htmltools::tags$div(shiny::actionButton("userList_Delete",label = NULL, icon = icon("trash-alt"))))
        						),
        						onClick = JS("function(rowInfo, column) {
															    if (column.id !== 'Update_User_Btn' && column.id !== 'Delete_User_Btn' && column.id !== 'ResetPsw_User_Btn') {
															      return
															    }
															
															    if (window.Shiny) {
															      Shiny.setInputValue('userConfigAction', { index: rowInfo.index + 1, button: column.id }, { priority: 'event' })
															    }
															  }"),
        						theme = reactableTheme(
        							backgroundColor = "white",
        							borderColor = "#dfe2e5",
        							stripedColor = "#f6f8fa",
        							highlightColor = "#f0f5f9",
        							cellPadding = "4px 6px",
        							style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        							searchInputStyle = list(width = "100%")
        						) 
        						
        	)
        	
        	
        })
        
        
        selected_row_User <- reactive({
        	input$userConfigAction$index
        })
        
        current_page_User <- reactive({
        	getReactableState("userConfigTable")$page
        })
        
        # messaggio per reset password
        resetPswModal <- function(failed, username) {
        	
        	if (is.null(failed)) {
        		
        		modalDialog(
        			title = "Confirm",
        			# paste0("Are you sure, you want to reset password for user ", username, "?\nAn email will be sent to the user!"),
        			span("Are you sure, you want to reset password for user ", tags$b(username), "?"),
        			div(tags$i("An email will be sent to the user!", style = "color: grey;")),
        			easyClose = FALSE,
        			footer = tagList(
        				modalButton("Cancel"),
        				actionButton("resetPswUser", "OK")
        			)
        		)
        		
        	} else if (failed) {
	        	
	        	modalDialog(
	        		title = "Error",
	        		div(tags$b("Error: mail not sent!", style = "color: red;")),
	        		easyClose = TRUE,
	        		footer = tagList(
	        			modalButton("Close"),
	        		)
	        	)
        		
        	} else  {
        		
        		modalDialog(
        			title = "Great",
        			div(tags$b("Mail sent successfully!", style = "color: green;")),
        			easyClose = TRUE,
        			footer = tagList(
        				modalButton("Close"),
        			)
         		)
        		
        	}
        }
        
        observeEvent(input$userConfigAction, {
        	
        	action <- input$userConfigAction$button
        	username <- user_list$dataframe[selected_row_User(), "username"]
        	firstName <- user_list$dataframe[selected_row_User(), "firstName"]
        	lastName <- user_list$dataframe[selected_row_User(), "lastName"]
        	email <- user_list$dataframe[selected_row_User(), "email"]
        	group <- user_list$dataframe[selected_row_User(), "group"]
        	
        	if (action == "ResetPsw_User_Btn") {
        		
        		showModal(resetPswModal(NULL, username))
        		
        	} else if (action == "Update_User_Btn") {
        		
        		showModal(
        			modalDialog(
        				title = "Edit User",
        				textInput(inputId = "newFirstName",label = "First Name",value = firstName),
        				textInput(inputId = "newLastName",label = "Last Name",value = lastName),
        				textInput(inputId = "newEmail",label = "Email",value = email),
        				# selectInput(inputId = "newGroup",label = "Group",choices = c("ADMIN","USER"),selected  = group),
        				easyClose = FALSE,
        				footer = tagList(
        					modalButton("Cancel"),
        					actionButton("saveUpdateUser", "Save")
        				)
        			)
        		)
        		
        	} else if (action == "Delete_User_Btn") {
        		
        		showModal(
        			modalDialog(
        				title = "Confirm",
        				span("Are you sure, you want to delete user ", tags$b(username), "?"),
        				div(tags$i("All scenarios will be deleted!", style = "color: red;")),
        				# paste0("Are you sure, you want to delete user ", username, "?\nAll scenarios will be deleted!"),
        				easyClose = FALSE,
        				footer = tagList(
        					modalButton("Cancel"),
        					actionButton("deleteUser", "OK")
        				)
        			)
        		)
        		
        	}
        	
        })
        
        observeEvent(input$resetPswUser, {
        	
        	username = user_list$dataframe[selected_row_User(), "username"]
        	email = user_list$dataframe[selected_row_User(), "email"]
        	firstName = user_list$dataframe[selected_row_User(), "firstName"]
        	lastName = user_list$dataframe[selected_row_User(), "lastName"]
        	
        	userData <- list(username = username,
        									 email = email,
        									 firstName = firstName,
        									 lastName = lastName
        	)
        	
        	userPsw <- password(n = 10, numbers = TRUE, case = TRUE, special = c("?", "!", "&", "%", "$"))
        	userGroup <- user_list$dataframe[selected_row_User(), "group"]
        	
        	idUser <- user_list$dataframe[selected_row_User(), "id"] 
        	
        	editUser(keycloackUrl,realmName,idUser,userData,userPsw,userGroup)
        	
        	##########
        	# SEND EMAIL!!
        	
        	from <- sprintf(paste0("<",userMailAdmin,">"))
        	to <- sprintf(paste0("<",email,">"))
        	
        	subject <- "SIMBAD - Reset Password"
        	body <- paste0("Dear ", firstName, " ", lastName, 
        								 ",\nyour password has been changed.\n\nHere below you can find your new credentials:\n - username: ", username,
        								 "\n - password: ",userPsw,"\n\nExplore the app at: ",urlSimbad)
        	
        	status <- sendmail(from,to,subject,body,control=list(smtpServer= smtpServer))
        	
        	if (status$code=="221") {
        		showModal(resetPswModal(failed = FALSE, username))
        	} else {
        		showModal(resetPswModal(failed = TRUE, username))
        	}
        	
        	##########
        	
        	# removeModal()
        	
        })
        
        observeEvent(input$saveUpdateUser, {
        	
        	usersList <- getListUsers(keycloackUrl,realmName)
        	
        	if (input$newEmail %in% usersList$email) {
        		
        		showModal(
        			modalDialog(
        				title = "Error",
        				div(tags$b("Error: email already exist!", style = "color: red;")),
        				easyClose = TRUE,
        				footer = tagList(
        					modalButton("Close"),
        				)
        			)
        		)
        		
        		return(NULL)
        		
        	}
        	
        	userData <- list(username = user_list$dataframe[selected_row_User(), "username"],
        									 email = input$newEmail,
        									 firstName = input$newFirstName,
        									 lastName = input$newLastName
        	)
        	
        	userPsw <- password(n = 10, numbers = TRUE, case = TRUE, special = c("?", "!", "&", "%", "$"))
        	userGroup <- user_list$dataframe[selected_row_User(), "group"]
        	
        	idUser <- user_list$dataframe[selected_row_User(), "id"] 
        	
        	editUser(keycloackUrl,realmName,idUser,userData,NA,userGroup)
        	
        	user_list$dataframe <- getListUsersWithGroups(keycloackUrl,realmName)
        	updateReactable("userConfigTable", user_list$dataframe, page = current_page_User())
        	
        	removeModal()
        	
        	
        })
        
        observeEvent(input$deleteUser, {
        	
        	idUser <- user_list$dataframe[selected_row_User(), "id"] 
        	nameUser <- user_list$dataframe[selected_row_User(), "username"]
        	
        	deleteUser(keycloackUrl,realmName,idUser)
        	
        	user_list$dataframe <- getListUsersWithGroups(keycloackUrl,realmName)
        	updateReactable("userConfigTable", user_list$dataframe, page = current_page_User())
        	
        	scenarioDF <- readScenarioFromDB(username,usergroup)
        	scenToRemove <- scenarioDF$scenarioName[scenarioDF$userName == nameUser]
        	if (!identical(scenToRemove, character(0))) {
        		dirToRemove <- file.path(SCENDIR,getScenarioDirectoryInDB(scenToRemove))
        		unlink(dirToRemove, recursive = TRUE)
        	}
        	
        	deleteUserInDB(input$deleteUser)
        	
        	scenario_list$dataframe <- readScenarioFromDB(username,usergroup)
        	scenario_list$list <- as.list(scenarioListfromDB(username,usergroup))
        	scenario_list$fromOtherUser <- getScenarioPublicFromDB(username,usergroup)
        	
        	updateReactable("scenarioConfigTable", scenario_list$dataframe, page = current_page_Scen())
        	
        	removeModal()
        	
        })
        
        
        
        
    }
)
    



