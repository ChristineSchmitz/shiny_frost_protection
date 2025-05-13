#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(datamods)
library(bslib)
library(reactable)

#Scenario_input<-read.csv("frost_scenario_supplementary.csv")
Frost_Input<-read.csv("Frost_protection_input_apple_shiny.csv", fileEncoding = "latin1")


source("frost_prot_function.R")
#Management_values<-data.frame(management_measure,value)

#App####

app_theme <- bslib::bs_theme(
  version = 5, 
  #bootswatch = "sketchy", 
  bg = "#C7E9EA", 
  fg = "#0A655E", 
  primary = "#429323",   # 
  secondary = "#FBBA00",
  base_font = "Arial"
)
# Define UI for application that draws a histogram
#UI####
ui <- fluidPage(
  theme = app_theme,
  # Application title
  #titlePanel(),
  navbarPage(title = (span(img(src="Bild1.png", height =100),"Entscheidungshilfe Forstschutz -Apfel-")),
             theme = app_theme,
             header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }")),
             tabPanel(title = (span(img(src="cold2.png", height =70),"Modell")),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("frost_risk",
                                      "Frostrisiko %",
                                      min = 0,
                                      max = 1,
                                      value = c(0.45,0.9)),
                          sliderInput("mean_price",
                                      "Erzeugerpreis",
                                      min = 0,
                                      max = 3.5,
                                      value = c(0.55,0.6)),
                          sliderInput("plot_area",
                                      "Fläche die geschützt werden soll [ha]:",
                                      min = 1,
                                      max = 20,
                                      value = 5),
                          sliderInput("mean_frost_duration",
                                      "Länge des Zeitraums mit Temperaturen <0°C pro Frostnacht [h]:",
                                      min = 1,
                                      max = 24,
                                      value = c(2,5)),
                          sliderInput("mean_n_nights_frost",
                                      "Anzahl Frostnächte in Frostjahren:",
                                      min = 1,
                                      max = 20,
                                      value = c(2,6)),
                          sliderInput("mean_post_harvest_costs",
                                      "Kosten die nach der Ernte anfallen (Lager, Sortierung) [€/kg]:",
                                      min = 0,
                                      max = 1.2,
                                      value = c(0.09, 0.14)),
                          sliderInput("share_harmful_frost",
                                      "Anteil Frostereigniss die Schaden verursachen [%]",
                                      min = 0,
                                      max = 100,
                                      value = c(9, 11)),
                          sliderInput("water_withdrawl_fee",
                                      "Wasserentnahmegeld [€/m³]:",
                                      min = 0,
                                      max = 1,
                                      value = c(0.05, 0.06)),
                          sliderInput("runs",
                                      "Anzahl Monte-Carlo Durchläufe:",
                                      min = 1,
                                      max = 15000,
                                      value = 100),
                          # sliderInput("Durchmesser",
                          #             "Durchmesser 4 Wochen vor Ernte [cm]:",
                          #             min = 0.5,
                          #             max = 10,
                          #             value = c(5.9,6.5)),
                          # sliderInput("runs",
                          #             "Anzahl Monte-Carlo Durchläufe:",
                          #             min = 1,
                          #             max = 15000,
                          #             value = 1000),
                          tags$style(HTML(".radio-inline {margin-right: 42px;}")),
                          radioButtons("existing_irrigation_pond",
                                       "Bewässerungsteich vorhanden",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 1),
                          radioButtons("groundwater_well",
                                       "Brunnen vorhanden",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 1),
                          checkboxGroupInput( 
                            "Protection_measures", 
                            "Frostschutzmaßnahmen im Vergleich:", 
                            c("Kein Frostschutz" ="no_protection",
                              "Überkonenberegnung" = "ov_irrigation",
                              "Unterkronenbergnung" = "ut_irrigation",
                               "Kerzen" = "candles",
                              "Frostbuster"="frostbuster",
                              "Frostguard"="frostguard",
                              "Pelletöfen"="heaters",
                              "Mobile Windmachine"="mobile_windmachine",
                              "Stationäre Windmachine"="stationary_windmachine"
                              ),
                            selected = "no_protection"
                          ), 
                          radioButtons("plot_type",
                                       "Darstellungsweise",
                                       choices = list("Boxplot"="boxplot", "Verteilung"="smooth_simple_overlay"),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = "smooth_simple_overlay"),
                          # radioButtons("hailnet",
                          #              "Hagelnetz",
                          #              choices = list("Ja "=1, "Nein"=0),
                          #              inline = TRUE,
                          #              #width = "400px",
                          #              selected = 0),
                          # radioButtons("cl_irri",
                          #              "klimatisierende Beregnung",
                          #              choices = list("Ja "=1, "Nein"=0),
                          #              inline = TRUE,
                          #              #width = "400px",
                          #              selected = 0),
                          # radioButtons("foliar_fert",
                          #              "Blattdüngung",
                          #              choices = list("Ja "=1, "Nein"=0),
                          #              inline = TRUE,
                          #              #width = "400px",
                          #              selected = 0),
                          # radioButtons("summer_pruning",
                          #              "Sommerschnitt",
                          #              choices = list("Ja "=1, "Nein"=0),
                          #              inline = TRUE,
                          #              #width = "400px",
                          #              selected = 0),
                          # radioButtons("kaolin",
                          #              "Behandlung mit Kaolin",
                          #              choices = list("Ja "=1, "Nein"=0),
                          #              inline = TRUE,
                          #              #width = "400px",
                          #              selected = 0),
                          # radioButtons("removing_leaves",
                          #              "Entblättern",
                          #              choices = list("Ja "=1, "Nein"=0),
                          #              inline = TRUE,
                          #              #width = "400px",
                          #              selected = 0),
                          # radioButtons("irrigation",
                          #              "Tröpfchenbewässerung",
                          #              choices = list("Ja "=1, "Nein"=0),
                          #              inline = TRUE,
                          #              #width = "400px",
                          #              selected = 0)
                          
                          # awesomeCheckboxGroup(
                          #   inputId = "work",
                          #   label = "Wähle die Maßnahmen aus die in der Anlage durchgeführt werden", 
                          #   choices = c("Behandlung gegen Vorerntefruchtfall"="spray_against_pre_harvest_fruit_drop",
                          #               #"manual_thinning_after_june_drop",
                          #               #"chemical_fruit_thinning",
                          #               #"mechanical_flower_thinning",
                          #               #"chemical_flower_thinning",
                          #               #"frost_protection",
                          #               #"pollinator_support",
                          #               "Hagelnetz"="hailnet",
                          #               "klimatisierende Beregnung"="climatizing_ov_irrigation",
                          #               "Blattdüngung"="leaf_fertilization",
                          #               "Sommerschnitt"="summer_pruning",
                          #               "Entblättern"="removing_leaves",
                          #               "Behandlung mit Kaolin"="use_kaolin",
                          #               "Tröpfchenbewässerung"="irrigation"),
                          #   selected = NULL)
                        ),
                        
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("DA_frost",height = "800px")#,
                          #tableOutput("test_table")
                          #plotOutput("distPlot2")
                        ))),
             tabPanel(title = (span(img(src="data.png", height =80),"Datenübersicht")),
                      
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        #tags$h2(i18n("Edit data"), align = "center"),
                        edit_data_ui(id = "id")#,
                        #verbatimTextOutput("result")
                        
                        #dataTableOutput("input_datasheet")#,
                        #plotOutput("distPlot2")
                      )),
             tabPanel(title = (span(img(src="InfoI.png", height =80),"Infos")),
                      
                      mainPanel(
                        #tags$h2(i18n("Edit data"), align = "center"),
                        uiOutput("text")#,
                        #verbatimTextOutput("result")
                        
                        #dataTableOutput("input_datasheet")#,
                        #plotOutput("distPlot2")
                      ))
             #https://stackoverflow.com/questions/30086881/how-can-i-control-the-size-of-the-numbers-on-my-slider-in-shiny
             
  )
)


# Define server logic required to draw a histogram
#Server####
server <- function(input, output) {
  edited_r <- edit_data_server(
    id = "id",
    data_r = reactive(Frost_Input),
    add = FALSE,
    update = TRUE,
    delete = FALSE,
    download_csv = TRUE,
    download_excel = TRUE,
    file_name_export = "datas",
    # var_edit = c("name", "job", "credit_card_provider", "credit_card_security_code"),
    #var_mandatory = c("name", "job"),
    var_labels = list(
      variable = "Variable",
      distribution = "Verteilung",
      lower = "Untere",
      median="Median (NA)",
      upper = "Obere",
      Unit = "Einheit",
      description = "Beschreibung"
    ),
    var_edit=list("lower", "upper"),
    add_default_values = list(
      name = "Please enter your name here",
      date_obtained = Sys.Date()
    ),
    n_column = 2,
    modal_size = "l",
    modal_easy_close = TRUE,
    reactable_options = list(
      defaultColDef = colDef(filterable = TRUE),
      selection = "single",
      columns = list(
        lower = colDef(name = "Untere", style = list(fontWeight = "bold")),
        upper = colDef(name = "Obere", style = list(fontWeight = "bold"))),
      bordered = TRUE,
      compact = TRUE,
      searchable = TRUE,
      highlight = TRUE
    )
  )
  
  outputOptions(output, "id-table", suspendWhenHidden = FALSE)
  
  output$DA_frost <- renderPlot({
    # Management_values2<-data.frame(management_measure=c("spray_against_pre_harvest_fruit_drop",
    #                                                     "hailnet",
    #                                                     "climatizing_ov_irrigation",
    #                                                     "leaf_fertilization",
    #                                                     "summer_pruning",
    #                                                     "removing_leaves",
    #                                                     "use_kaolin",
    #                                                     "irrigation"),value= as.numeric(c(input$drop,
    #                                                                                       input$hailnet,
    #                                                                                       input$cl_irri,
    #                                                                                       input$foliar_fert,
    #                                                                                       input$summer_pruning,
    #                                                                                       input$removing_leaves,
    #                                                                                       input$kaolin,
    #                                                                                       input$irrigation)))
    # write.csv(Management_values2, "test_management_values.csv")
    library(decisionSupport)
    library(tidyverse)
    
    variable<-c("frost_risk", "mean_frost_duration", "mean_n_nights_frost", "mean_post_harvest_costs", "mean_price", "share_harmful_frost", "water_withdrawl_fee", "plot_area","groundwater_well", "terrain_correction_needed", "existing_irrigation_pond")
    distribution<-c("tnorm_0_1", "posnorm"  , "posnorm" ,  "posnorm"  , "posnorm"   ,"tnorm_0_1" , "posnorm", "const", "const", "const", "const")
    lower<-c(input$frost_risk[1],input$mean_frost_duration[1],input$mean_n_nights_frost[1], input$mean_post_harvest_costs[1], input$mean_price[1], input$share_harmful_frost[1]/100, input$water_withdrawl_fee[1], input$plot_area, input$groundwater_well, 0, input$existing_irrigation_pond)
    median<-rep(NA, 11)
    upper<-c(input$frost_risk[2],input$mean_frost_duration[2],input$mean_n_nights_frost[2], input$mean_post_harvest_costs[2], input$mean_price[2], input$share_harmful_frost[2]/100, input$water_withdrawl_fee[2], input$plot_area, input$groundwater_well, 0, input$existing_irrigation_pond)
    unit<-c("%/100", "h", "nights", "€/kg", "€/kg", "%", "€/m³", "ha","value","value","value")
    Frost_slider_input<-data.frame(variable,distribution,lower,median,upper,unit)
    #orchard_data<-read.csv2("2024_test_apple/test_input.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character"), sep = ";", dec = ".")
    #Apple_prediction_input_raw<-rbind(Apple_prediction_input_csv, Apple_prediction_slider_input)
    Frost_input_all<-rbind(edited_r()[1:6], Frost_slider_input)
    
    #source("management_values.R")
    
    #Quality and Yield tp4####
    
    # plot_area<-input$plot_area[1]
    # terrain_correction_needed<-1
    # existing_irrigation_pond<-input$existing_irrigation_pond
    # groundwater_well==input$groundwater_well
    
    # frost_protection_apple_run<-frost_protection_apple(groundwater_well=input$groundwater_well, 
    #                                                    n_years = n_years)
    
    
    #Monte Carlo####
    # apple_quality_and_yield_mc_simulation_tp4 <- mcSimulation(estimate = as.estimate(Apple_prediction_input_raw),
    #                                                           model_function = tp_4_quality_and_yield_prediction,
    #                                                           numberOfModelRuns = input$runs,
    #                                                           functionSyntax = "plainNames")
    frost_protection_mc_simulation <- mcSimulation(estimate = as.estimate(Frost_input_all),
                                                              model_function = frost_protection_apple,
                                                              numberOfModelRuns = input$runs,
                                                              functionSyntax = "plainNames")
    
    # Plot_a<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp4,
    #                            vars = c("high_quality_yield_diameter"),
    #                            method = 'smooth_simple_overlay',
    #                            base_size = 7)+
    #   theme(axis.text = element_text(colour = "black", size = 10),
    #         axis.title = element_text(colour = "black", size = 10),
    #         legend.position = "none")+
    #   xlab("Qualitätsertrag [t/ha]")+
    #   ylab("Häufigkeit")
    vars_plots<-c(paste0("result_", input$Protection_measures))
    Plot_b<-plot_distributions(mcSimulation_object = frost_protection_mc_simulation,
                               vars = vars_plots,
                               #method = 'smooth_simple_overlay',
                               method = input$plot_type,
                               base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("NPV")+
      ylab("Häufigkeit")
    print(Plot_b)
    # Plot_c<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp4,
    #                            vars = c("fruits_per_tree"),
    #                            method = 'smooth_simple_overlay',
    #                            base_size = 7)+
    #   theme(axis.text = element_text(colour = "black", size = 10),
    #         axis.title = element_text(colour = "black", size = 10),
    #         legend.position = "none")+
    #   xlab("Früchte pro Baum")+
    #   ylab("Häufigkeit")
    
    # library(patchwork)
    # Plots_combined <- list(Plot_a, Plot_b,Plot_c)
    # wrap_plots(Plots_combined, nrow = 3) +
    #   plot_layout(guides = "keep")
    
  })
  
  # output$test_table<- renderTable({
  #   variable<-c("frost_risk", "mean_frost_duration", "mean_n_nights_frost", "mean_post_harvest_costs", "mean_price", "share_harmful_frost", "water_withdrawl_fee", "plot_area","groundwater_well", "terrain_correction_needed", "existing_irrigation_pond")
  #   distribution<-c("tnorm_0_1", "posnorm"  , "posnorm" ,  "posnorm"  , "posnorm"   ,"tnorm_0_1" , "posnorm", "const", "const", "const", "const")
  #   lower<-c(input$frost_risk[1],input$mean_frost_duration[1],input$mean_n_nights_frost[1], input$mean_post_harvest_costs[1], input$mean_price[1], input$share_harmful_frost[1]/100, input$water_withdrawl_fee[1], input$plot_area, input$groundwater_well, 0, input$existing_irrigation_pond)
  #   median<-rep(NA, 11)
  #   upper<-c(input$frost_risk[2],input$mean_frost_duration[2],input$mean_n_nights_frost[2], input$mean_post_harvest_costs[2], input$mean_price[2], input$share_harmful_frost[2]/100, input$water_withdrawl_fee[2], input$plot_area, input$groundwater_well, 0, input$existing_irrigation_pond)
  #   unit<-c("%/100", "h", "nights", "€/kg", "€/kg", "%", "€/m³", "ha","value","value","value")
  #   Frost_slider_input<-data.frame(variable,distribution,lower,median,upper,unit)
  #   #orchard_data<-read.csv2("2024_test_apple/test_input.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character"), sep = ";", dec = ".")
  #   #Apple_prediction_input_raw<-rbind(Apple_prediction_input_csv, Apple_prediction_slider_input)
  #   Frost_input_all<-rbind(edited_r()[1:6], Frost_slider_input)
  #   Frost_input_all[1:5,]})

  output$input_datasheet<-DT::renderDT(rbind(Apple_input[1:7], Apple_estimation[1:7]))
  
  output$text <- renderUI({
    url <- a("Experimentierfelds Suedwest", href="https://ef-sw.de")
    url_b <- a("https://doi.org/10.1016/j.agsy.2024.104255", href="https://doi.org/10.1016/j.agsy.2024.104255")
    url_c <- a("zur Verfügung", href="https://zenodo.org/records/11473204")
    logo<-img(src="BMEL_BLE.png", height =200)
    HTML(paste("<b>Entwicklung der App:</b> Christine Schmitz<sup>1,2</sup><br>",
               "<b>Entwicklung des Modells:</b> Christine Schmitz<sup>1,2</sup>, Lars Zimmermann<sup>1,2</sup>, Katja Schiffers<sup>2</sup>,Cory Whitney<sup>2</sup>, Martin Balmer<sup>1</sup>, Eike Luedeling<sup>2</sup> <br>",
               "<br>",
               "<sup>1</sup>Dienstleistungszentrum Ländlicher Raum Rheinpfalz, Campus Klein-Altendorf 2, 53359 Rheinbach, Germany <br>",
               "<sup>2</sup>INRES – Horticultural Sciences, University of Bonn, Auf dem Hügel 6, 53121 Bonn, Germany.",
               "<br>",
               "Kontakt: christine.schmitz@dlr.rlp.de<br>",
               "<br>",
               "<b>Idee hinter dem Modell:</b><br>",
               "Ziel des Modells ist der wirtschafliche Vergleich verschiedener aktiver Frostschutzmaßnahmen für den Apfelanbau<br>",
               "<br>",
               "<b>Methodik</b><br>",
               "Das Modell basiert auf der Methodik der Entscheidungsanalyse. Dies beinhaltet eine Erarbeitung der Modellgrundlage und Inputwerte mit Experten aus der Obstbranche, die Verwendung vom Wertebereichen als Inputvariablen im Rahmen einer Monte-Carlo Simulation.<br>",
               "<br>",
               "<b>weitere Infos</b><br>",
               "Mit dem Modell berchnete Ergebnisse wurden in Agrigultural Systems veröffentlicht: Christine Schmitz, Lars Zimmermann, Katja Schiffers, Cory Whitney, Martin Balmer, Eike Luedeling, 2025. Model-based decision support for the choice of active spring frost protection measures in apple production. Agricultural Systems 224, 104255. ",
               url_b,
               "<br>",
               "Der Code für das Entscheidungsmodell steht ebenfalls ",
               url_c,
               ".<br>",
               "<br>",
               "<b>Förderung</b><br>",
               "Die Entwicklung des Modelles und der App wurde im Rahmen des<br>",
               url,
               " durchgeführt und wurde vom Bundesministerium für Landwirtschaft und Ernährung gefördert (Förderkennzeichen 28DE111B22)<br>",
               logo,
               sep = ""))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

