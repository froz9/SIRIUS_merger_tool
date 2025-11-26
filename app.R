library(shiny)
library(data.table)
library(MetaboCoreUtils)
library(dplyr)
library(plotly)
library(grDevices)
library(DT)

# --- 1. SETUP: LOAD STATIC SUPPORT FILES ---
safe_read_csv <- function(filename) {
  if (file.exists(filename)) {
    read.csv(filename, na.strings=c("", "NA"), stringsAsFactors = FALSE)
  } else {
    warning(paste("Missing support file:", filename))
    NULL
  }
}

class_colors <- safe_read_csv("class_colors.csv")
superclass_colors <- safe_read_csv("superclass_colors.csv")
subclass_colors <- safe_read_csv("subclass_colors.csv")
npc_class_colors <- safe_read_csv("NPC_Class_ColorCode.csv")
npc_superclass_colors <- safe_read_csv("NPC_Superclass_ColorCode.csv")
npc_pathway_colors <- safe_read_csv("NPC_Pathway_ColorCode.csv")
my_adducts <- safe_read_csv("custom_adducts.csv")

if (!is.null(my_adducts)) {
  rownames(my_adducts) <- my_adducts$name
  try({
    unlockBinding(".ADDUCTS", asNamespace("MetaboCoreUtils"))
    unlockBinding(".ADDUCTS_MULT", asNamespace("MetaboCoreUtils"))
    unlockBinding(".ADDUCTS_ADD", asNamespace("MetaboCoreUtils"))
  }, silent = TRUE)
  assign(".ADDUCTS", my_adducts, envir = asNamespace("MetaboCoreUtils"))
  assign(".ADDUCTS_MULT", setNames(my_adducts$mass_multi, rownames(my_adducts)), envir = asNamespace("MetaboCoreUtils"))
  assign(".ADDUCTS_ADD", setNames(my_adducts$mass_add, rownames(my_adducts)), envir = asNamespace("MetaboCoreUtils"))
}

# --- 2. USER INTERFACE ---
ui <- fluidPage(
  # --- START: Page Config & Title/Logo ---
  # Sets the browser tab title and icon (favicon)
  title = "SIRIUS File Processor", 
  tags$head(
    tags$link(rel = "icon", href = "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🧪</text></svg>")
  ),
  
  # Custom title panel with logo
  div(
    style = "display: flex; align-items: center; padding: 10px 0px;",
    # This img() tag looks for 'logo_L125.png' in the 'www' folder
    img(src = "logo_L125.png", height = "200px", style = "margin-right: 20px;"),
    h2("SIRIUS Output Analyzer", style = "font-size: 80px; font-weight: bold; margin: 0;")
  ),
  hr(),
  # --- END: Page Config & Title/Logo ---
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Upload Data"),
      fileInput("canopus_file", "Load 'canopus_structure_summary.tsv'", accept = c(".tsv", "text/plain")),
      fileInput("identifications_file", "Load 'structure_identifications.tsv'", accept = c(".tsv", "text/plain")),
      hr(),
      h4("2. Parameters"),
      sliderInput("ppm_threshold", "Mass Accuracy Threshold (ppm):", min = 1, max = 30, value = 15),
      hr(),
      helpText("Once data is loaded, navigate to the 'Downloads' tab to save results."),
      
      # --- START: About & Contact ---
      hr(style="margin-top: 30px;"),
      h5("About & Contact"),
      p("This app was developed to process SIRIUS output files."),
      a(href = "mailto:f9.alan@gmail.com", "Report a bug")
      # --- END: About & Contact ---
    ),
    
    mainPanel(
      tabsetPanel(
        # --- START: Updated "Data Preview" Tab ---
        tabPanel("Data Preview",
                 h4("Welcome to the SIRIUS Output Analyzer"),
                 p("This app merges, analyzes, and visualizes two of the main outputs from SIRIUS app.",
                   "Upload your 'canopus_structure_summary.tsv' and 'structure_identifications.tsv' files to get started."),
                 hr(),
                 h5("Data Preview (Top 50 rows)"),
                 DTOutput("preview_table")
        ),
        # --- END: Updated "Data Preview" Tab ---
        
        tabPanel("ClassyFire Plots",
                 fluidRow(column(12, plotlyOutput("plot_cf_superclass", 
                                                 height = "600px")), 
                          column(12, plotlyOutput("plot_cf_class", 
                                                  height = "800px"))),
                 fluidRow(column(12, plotlyOutput("plot_cf_subclass", 
                                                  height = "1000px")))),
        tabPanel("NPC Plots",
                 fluidRow(column(12, plotlyOutput("plot_npc_pathway", , 
                                                  height = "600px")), 
                          column(12, plotlyOutput("plot_npc_superclass", 
                                                  height = "800px"))),
                 fluidRow(column(12, plotlyOutput("plot_npc_class", 
                                                  height = "1000px")))),
        tabPanel("Sunburst (ClassyFire)", plotlyOutput("plot_sunburst_classyfire", height = "800px")),
        tabPanel("Sunburst (NPC)", plotlyOutput("plot_sunburst_npc", height = "800px")),
        tabPanel("Downloads",
                 h3("Main Results"),
                 downloadButton("dl_merged", "Download Full Merged Data (.csv)", class = "btn-primary"), br(), br(),
                 downloadButton("dl_filtered", "Download Filtered (PPM) Data (.csv)", class = "btn-primary"),
                 hr(),
                 h3("Summary Tables (for Pie Charts)"),
                 p("These tables include the color codes and 'Others (<3)' grouping used in the plots."),
                 fluidRow(
                   column(6, 
                          h4("ClassyFire"),
                          downloadButton("dl_cf_super", "Superclass Summary"), br(), br(),
                          downloadButton("dl_cf_class", "Class Summary"), br(), br(),
                          downloadButton("dl_cf_sub", "Subclass Summary")
                   ),
                   column(6, 
                          h4("NPC"),
                          downloadButton("dl_npc_path", "Pathway Summary"), br(), br(),
                          downloadButton("dl_npc_super", "Superclass Summary"), br(), br(),
                          downloadButton("dl_npc_class", "Class Summary")
                   )
                 )
        )
      )
    )
  )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100*1024^2)
  
  # --- Helper Functions ---
  get_summary_data <- function(df, col_name, color_ref_df, merge_col_ref) {
    req(df)
    counts <- as.data.frame(table(df[[col_name]]))
    colnames(counts) <- c("Group", "Freq")
    df_main <- counts %>% filter(Freq >= 3)
    df_other <- data.frame(Group = "Others (< 3)", Freq = sum(counts$Freq[counts$Freq < 3]))
    plot_data <- rbind(df_main, df_other)
    plot_data <- plot_data[plot_data$Freq > 0, ]
    
    if (!is.null(color_ref_df)) {
      plot_data <- merge(plot_data, color_ref_df, by.x = "Group", by.y = merge_col_ref, all.x = TRUE)
    } else {
      plot_data$ColorCode <- NA
    }
    return(plot_data)
  }
  
  render_pie <- function(plot_data, title) {
    req(plot_data)
    plot_ly(plot_data, labels = ~Group, values = ~Freq, type = 'pie',
            sort = FALSE, direction = "clockwise",
            marker = list(colors = ~ColorCode), textfont = list(size = 15)) %>%
      layout(font = list(size = 15), legend = list(title = list(text = paste0('<b> ', title, ' </b>')), y = 0.5))
  }
  
  # --- Main Data Processing ---
  merged_data <- reactive({
    req(input$canopus_file, input$identifications_file)
    tryCatch({
      canopus <- as.data.frame(fread(input$canopus_file$datapath))
      idents <- as.data.frame(fread(input$identifications_file$datapath))
      merged <- merge(canopus, idents, by = "mappingFeatureId")
      
      merged$adduct.y <- gsub(" ", "", merged$adduct.y)
      merged$adduct.y[merged$adduct.y == "[M-H2O+H]+"] <- "[M+H-H2O]+"
      merged$adduct.y[merged$adduct.y == "[M+H3N+H]+"] <- "[M+NH4]+"
      merged$adduct.y[merged$adduct.y == "[M-H4O2+H]+"] <- "[M+H-H4O2]+"
      
      merged$Theoretical.mass <- mapply(MetaboCoreUtils::formula2mz, merged$molecularFormula.y, merged$adduct.y)
      merged$Mass.accuracy.ppm <- ((1-(merged$ionMass.y / merged$Theoretical.mass))*1000000)
      merged$TimeInMinutes <- merged$retentionTimeInSeconds.y / 60
      
      merged <- merged %>% rename(superclass = 'ClassyFire#superclass', class = 'ClassyFire#class', 
        subclass = 'ClassyFire#subclass', NPC_patwhay = 'NPC#pathway', NPC_superclass = 'NPC#superclass',
        NPC_class = 'NPC#class')

      merged[c("superclass", "class", "subclass")][merged[c("superclass", "class", "subclass")] == ""] <- "Unassigned"
      merged[c("NPC_patwhay", "NPC_superclass", "NPC_class")][merged[c("NPC_patwhay", "NPC_superclass", "NPC_class")] == ""] <- "Unassigned"
     
      merged$ID_extract <- sub(".*_", "", merged$mappingFeatureId)
      
      merged
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      NULL
    })
  })
  
  filtered_data <- reactive({
    req(merged_data())
    merged_data() %>% filter(Mass.accuracy.ppm > -input$ppm_threshold & Mass.accuracy.ppm < input$ppm_threshold)
  })
  
  # --- Reactive Summary Tables ---
  cf_superclass_data <- reactive({ get_summary_data(merged_data(), "superclass", superclass_colors, "X.canopus.Superclass.") })
  cf_class_data <- reactive({ get_summary_data(merged_data(), "class", class_colors, "X.canopus.Class.") })
  cf_subclass_data <- reactive({ get_summary_data(merged_data(), "subclass", subclass_colors, "subclass") })
  npc_pathway_data <- reactive({ get_summary_data(merged_data(), "NPC_pathway", npc_pathway_colors, "Pathway") })
  npc_superclass_data <- reactive({ get_summary_data(merged_data(), "NPC_superclass", npc_superclass_colors, "Superclass") })
  npc_class_data <- reactive({ get_summary_data(merged_data(), "NPC_class", npc_class_colors, "Class") })
  
  # --- Render Plots ---
  output$plot_cf_superclass <- renderPlotly({ render_pie(cf_superclass_data(), "Superclass") })
  output$plot_cf_class <- renderPlotly({ render_pie(cf_class_data(), "Class") })
  output$plot_cf_subclass <- renderPlotly({ render_pie(cf_subclass_data(), "Subclass") })
  output$plot_npc_pathway <- renderPlotly({ render_pie(npc_pathway_data(), "NPC Pathway") })
  output$plot_npc_superclass <- renderPlotly({ render_pie(npc_superclass_data(), "NPC Superclass") })
  output$plot_npc_class <- renderPlotly({ render_pie(npc_class_data(), "NPC Class") })
  
  output$plot_sunburst_classyfire <- renderPlotly({
    req(merged_data())
    d <- merged_data()
    d$ids <- paste(d$superclass, d$class, d$subclass, sep = " - ")
    d$parents <- paste(d$superclass, d$class, sep = " - ")
    r1 <- unique(data.frame(ids=d$superclass, labels=d$superclass, parents="", stringsAsFactors=F))
    r2 <- unique(data.frame(ids=d$parents, labels=d$class, parents=d$superclass, stringsAsFactors=F))
    r3 <- unique(data.frame(ids=d$ids, labels=d$subclass, parents=d$parents, stringsAsFactors=F))
    plot_ly(rbind(r1, r2, r3), ids=~ids, labels=~labels, parents=~parents, type='sunburst', maxdepth=3)
  })

  output$plot_sunburst_npc <- renderPlotly({
    req(merged_data())
    d <- merged_data()
    d$ids <- paste(d$NPC_patwhay, d$NPC_superclass, d$NPC_class, sep = " - ")
    d$parents <- paste(d$NPC_patwhay, d$NPC_superclass, sep = " - ")
    r1 <- unique(data.frame(ids=d$NPC_patwhay, labels=d$NPC_patwhay, parents="", stringsAsFactors=F))
    r2 <- unique(data.frame(ids=d$parents, labels=d$NPC_superclass, parents=d$NPC_patwhay, stringsAsFactors=F))
    r3 <- unique(data.frame(ids=d$ids, labels=d$NPC_class, parents=d$parents, stringsAsFactors=F))
    plot_ly(rbind(r1, r2, r3), ids=~ids, labels=~labels, parents=~parents, type='sunburst', maxdepth=3)
  })
  
  # --- Outputs & Downloads ---
  output$preview_table <- renderDT({ datatable(head(merged_data(), 50), options = list(scrollX = T)) })
  
  output$dl_merged <- downloadHandler(filename = "SIRIUS_Merged.csv", content = function(f) { write.csv(merged_data(), f, row.names=F) })
  output$dl_filtered <- downloadHandler(filename = function() { paste0("SIRIUS_Filtered_", input$ppm_threshold, "ppm.csv") }, 
                                        content = function(f) { write.csv(filtered_data(), f, row.names=F) })
  
  output$dl_cf_super <- downloadHandler(filename = "ClassyFire_Superclass_Summary.csv", content = function(f) { write.csv(cf_superclass_data(), f, row.names=F) })
  output$dl_cf_class <- downloadHandler(filename = "ClassyFire_Class_Summary.csv", content = function(f) { write.csv(cf_class_data(), f, row.names=F) })
  output$dl_cf_sub <- downloadHandler(filename = "ClassyFire_Subclass_Summary.csv", content = function(f) { write.csv(cf_subclass_data(), f, row.names=F) })
  output$dl_npc_path <- downloadHandler(filename = "NPC_Pathway_Summary.csv", content = function(f) { write.csv(npc_pathway_data(), f, row.names=F) })
  output$dl_npc_super <- downloadHandler(filename = "NPC_Superclass_Summary.csv", content = function(f) { write.csv(npc_superclass_data(), f, row.names=F) })
  output$dl_npc_class <- downloadHandler(filename = "NPC_Class_Summary.csv", content = function(f) { write.csv(npc_class_data(), f, row.names=F) })
}

shinyApp(ui, server)