# UI for shiny_plot_point_verif_local(monitor-like web page)

# Commands for easy deploying:
# setwd('/perm/sp3c/deode_verif/plot_point_verif_local2')
# library(here)
# library(shiny)
 
font_link <- shiny::tags$link("")
is_online <- shiny::getShinyOption("online")
if (is.null(is_online)) {
  is_online <- TRUE
}
hostname  <- Sys.getenv("HOSTNAME")
if (is_online & (!is.null(hostname) && !grepl("^ecgb", hostname))) {
  font_link <- shiny::tags$link(
    href="https://fonts.googleapis.com/css?family=Comfortaa:400,700",  rel="stylesheet"
  )
}

ui <- pageWithSidebar(
  headerPanel(
    title = a("Monitor-like HARP's visualization tool")
),
  

  sidebarPanel(
    
    selectizeInput(inputId = "var1",
                   label = "Case study:",
                   choices = NULL,
                   options = list(
                     placeholder = 'select',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    
    selectizeInput(inputId = "var2",
                   label = "Period:",
                   choices = NULL,
                   options = list(
                     placeholder = 'select',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    
    selectizeInput(inputId = "var3",
                   label = "Area:",
                   choices = NULL,
                   options = list(
                     placeholder = 'select',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    
    selectizeInput(inputId = "var4",
                   label = "Run type:",
                   choices = NULL,
                   options = list(
                     placeholder = 'select',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    
    
    selectizeInput(inputId = "var5",
                   label = "Runs:",
                   choices = NULL,
                   options = list(placeholder = 'select')
    ),
    
    
    selectizeInput(inputId = "var6",
                   label = "Variable:",
                   choices = NULL,
                   options = list(
                     placeholder = 'select',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    
    selectizeInput(inputId = "var7",
                   label = "Metric:",
                   choices = NULL,
                   options = list(
                     placeholder = 'select',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    
    selectizeInput(inputId = "var8",
                   label = "Submetric:",
                   choices = NULL,
                   options = list(
                     placeholder = 'select',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    
    selectizeInput(inputId = "var9",
                   label = "Forecast Length:",
                   choices = NULL,
                   options = list(
                     placeholder = 'select',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    
    
    h4("Selected Figure:"),
    verbatimTextOutput("subdirText")
        ,width=2),
  
  mainPanel(
    imageOutput(outputId = "png_image",width=50,height=50),
    uiOutput("output")
  )
)
