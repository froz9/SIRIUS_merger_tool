library(shiny)
library(data.table)
library(MetaboCoreUtils)
library(dplyr)
library(plotly)
library(grDevices)
library(DT)
library(colourpicker) # Added for color pickers

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
  title = "SIRIUS File Processor", 
  tags$head(
    tags$link(rel = "icon", href = "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🧪</text></svg>")
  ),
  
  div(
    style = "display: flex; align-items: center; padding: 10px 0px;",
    img(src = "logo_L125.png", height = "200px", style = "margin-right: 20px;"),
    h2("SIRIUS Output Analyzer", style = "font-size: 80px; font-weight: bold; margin: 0;")
  ),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Upload Data"),
      fileInput("canopus_file", "Load 'canopus_structure_summary.tsv'", accept = c(".tsv", "text/plain")),
      helpText("Plots are generated using only the Canopus file. Upload identifications below to merge datasets."),
      fileInput("identifications_file", "Load 'structure_identifications.tsv' (Optional)", accept = c(".tsv", "text/plain")),
      hr(),
      h4("2. Parameters"),
      sliderInput("ppm_threshold", "Mass Accuracy Threshold (ppm, for merged data):", min = 1, max = 30, value = 15),
      numericInput("min_count", "Group features in Pie Charts with counts fewer than:", value = 3, min = 1, step = 1),
      hr(),
      helpText("Once data is loaded, navigate to the 'Downloads' tab to save results."),
      
      hr(style="margin-top: 30px;"),
      h5("About & Contact"),
      p("This app was developed to process SIRIUS output files."),
      a(href = "mailto:f9.alan@gmail.com", "Report a bug")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview",
                 h4("Welcome to the SIRIUS Output Analyzer"),
                 p("This app visualizes the main outputs from SIRIUS app. Plots are generated from the Canopus file.",
                   "Upload 'structure_identifications.tsv' to merge datasets and download the combined tables."),
                 hr(),
                 h5("Data Preview (Top 50 rows)"),
                 DTOutput("preview_table")
        ),
        tabPanel("ClassyFire Plots",
                 fluidRow(
                   column(2, selectInput("cf_format", "Image Format:", choices = c("png", "svg"), selected = "png")),
                   column(3, numericInput("cf_scale", "Image Download Resolution Scale (1-10):", value = 3, min = 1, max = 10, step = 1)),
                   column(7, h5("Manually select colors for pie charts:"), 
                          div(style = "max-height: 150px; overflow-y: auto; display: flex; flex-wrap: wrap; gap: 10px;", 
                              uiOutput("pie_cf_color_pickers")))
                 ),
                 fluidRow(column(12, plotlyOutput("plot_cf_superclass", height = "600px")), 
                          column(12, plotlyOutput("plot_cf_class", height = "800px"))),
                 fluidRow(column(12, plotlyOutput("plot_cf_subclass", height = "1000px")))),
        tabPanel("NPC Plots",
                 fluidRow(
                   column(2, selectInput("npc_format", "Image Format:", choices = c("png", "svg"), selected = "png")),
                   column(3, numericInput("npc_scale", "Image Download Resolution Scale (1-10):", value = 3, min = 1, max = 10, step = 1)),
                   column(7, h5("Manually select colors for pie charts:"), 
                          div(style = "max-height: 150px; overflow-y: auto; display: flex; flex-wrap: wrap; gap: 10px;", 
                              uiOutput("pie_npc_color_pickers")))
                 ),
                 fluidRow(column(12, plotlyOutput("plot_npc_pathway", height = "600px")), 
                          column(12, plotlyOutput("plot_npc_superclass", height = "800px"))),
                 fluidRow(column(12, plotlyOutput("plot_npc_class", height = "1000px")))),
        tabPanel("Sunburst (ClassyFire)", 
                 fluidRow(
                   column(2, selectInput("sun_cf_format", "Image Format:", choices = c("png", "svg"), selected = "png")),
                   column(3, numericInput("sun_cf_scale", "Image Download Resolution Scale:", value = 3, min = 1, max = 10, step = 1)),
                   column(7, h5("Manually select colors for root elements:"), div(style = "max-height: 150px; overflow-y: auto; display: flex; flex-wrap: wrap; gap: 10px;", uiOutput("sun_cf_color_pickers")))
                 ),
                 plotlyOutput("plot_sunburst_classyfire", height = "800px")),
        tabPanel("Sunburst (NPC)", 
                 fluidRow(
                   column(2, selectInput("sun_npc_format", "Image Format:", choices = c("png", "svg"), selected = "png")),
                   column(3, numericInput("sun_npc_scale", "Image Download Resolution Scale:", value = 3, min = 1, max = 10, step = 1)),
                   column(7, h5("Manually select colors for root elements:"), div(style = "max-height: 150px; overflow-y: auto; display: flex; flex-wrap: wrap; gap: 10px;", uiOutput("sun_npc_color_pickers")))
                 ),
                 plotlyOutput("plot_sunburst_npc", height = "800px")),
        tabPanel("Downloads",
                 h3("Main Results (Requires Merged Data)"),
                 downloadButton("dl_merged", "Download Full Merged Data (.csv)", class = "btn-primary"), br(), br(),
                 downloadButton("dl_filtered", "Download Filtered (PPM) Data (.csv)", class = "btn-primary"),
                 hr(),
                 h3("Summary Tables (for Pie Charts)"),
                 p("These tables include the color codes and 'Others' grouping used in the plots based on the Canopus file."),
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
  ),
  # Global Footer
  hr(style="margin-top: 40px;"),
  div(style = "text-align: center; color: gray;",
      p("SIRIUS merger tool"),
      p("This work was supported by Universidad Nacional Autónoma de México Postdoctoral Program"),
      a(href = "mailto:f9.alan@gmail.com", "Report a bug")
  ))

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100*1024^2)
  
  # --- Helper Functions ---
  get_summary_data <- function(df, col_name, color_ref_df, merge_col_ref, min_count) {
    req(df)
    counts <- as.data.frame(table(df[[col_name]]))
    colnames(counts) <- c("Group", "Freq")
    df_main <- counts %>% filter(Freq >= min_count)
    df_other <- data.frame(Group = paste0("Others (< ", min_count, ")"), Freq = sum(counts$Freq[counts$Freq < min_count]))
    plot_data <- rbind(df_main, df_other)
    plot_data <- plot_data[plot_data$Freq > 0, ]
    
    if (!is.null(color_ref_df)) {
      plot_data <- merge(plot_data, color_ref_df, by.x = "Group", by.y = merge_col_ref, all.x = TRUE)
    } else {
      plot_data$ColorCode <- NA
    }
    return(plot_data)
  }
  
  render_pie <- function(plot_data, title, scale_val, input_prefix, format_val) {
    req(plot_data)
    scale_val <- if (!is.null(scale_val)) scale_val else 3
    format_val <- if (!is.null(format_val)) format_val else 'png'
    
    # Apply dynamic colors if selected via UI
    plot_data$DynamicColor <- sapply(plot_data$Group, function(g) {
      safe_g <- gsub("[^[:alnum:]]", "", g)
      val <- input[[paste0(input_prefix, safe_g)]]
      if (!is.null(val)) val else {
        def <- plot_data$ColorCode[plot_data$Group == g]
        if (length(def) > 0 && !is.na(def[1])) def[1] else "#808080"
      }
    })
    
    plot_ly(plot_data, labels = ~Group, values = ~Freq, type = 'pie',
            sort = FALSE, direction = "clockwise",
            marker = list(colors = ~DynamicColor), textfont = list(size = 15)) %>%
      layout(font = list(size = 15), legend = list(title = list(text = paste0('<b> ', title, ' </b>')), y = 0.5)) %>%
      config(toImageButtonOptions = list(format = format_val, filename = paste0(title, '_pie'), scale = scale_val))
  }
  
  # --- Main Data Processing ---
  canopus_data <- reactive({
    req(input$canopus_file)
    tryCatch({
      canopus <- as.data.frame(fread(input$canopus_file$datapath))
      
      canopus <- canopus %>% rename(
        superclass = 'ClassyFire#superclass', 
        class = 'ClassyFire#class', 
        subclass = 'ClassyFire#subclass', 
        NPC_pathway = 'NPC#pathway', 
        NPC_superclass = 'NPC#superclass',
        NPC_class = 'NPC#class'
      )
      
      canopus[c("superclass", "class", "subclass")][canopus[c("superclass", "class", "subclass")] == ""] <- "Unassigned"
      canopus[c("NPC_pathway", "NPC_superclass", "NPC_class")][canopus[c("NPC_pathway", "NPC_superclass", "NPC_class")] == ""] <- "Unassigned"
      
      canopus$ID_extract <- sub(".*_", "", canopus$mappingFeatureId)
      canopus
    }, error = function(e) {
      showNotification(paste("Error loading Canopus file:", e$message), type = "error", duration = 10)
      NULL
    })
  })
  
  merged_data <- reactive({
    req(canopus_data(), input$identifications_file)
    tryCatch({
      idents <- as.data.frame(fread(input$identifications_file$datapath))
      merged <- merge(canopus_data(), idents, by = "mappingFeatureId")
      
      if("adduct.y" %in% names(merged)) {
        merged$adduct.y <- gsub(" ", "", merged$adduct.y)
        merged$adduct.y[merged$adduct.y == "[M-H2O+H]+"] <- "[M+H-H2O]+"
        merged$adduct.y[merged$adduct.y == "[M+H3N+H]+"] <- "[M+NH4]+"
        merged$adduct.y[merged$adduct.y == "[M-H4O2+H]+"] <- "[M+H-H4O2]+"
        
        merged$Theoretical.mass <- mapply(MetaboCoreUtils::formula2mz, merged$molecularFormula.y, merged$adduct.y)
        merged$Mass.accuracy.ppm <- ((1-(merged$ionMass.y / merged$Theoretical.mass))*1000000)
      }
      
      if("retentionTimeInSeconds.y" %in% names(merged)) {
        merged$TimeInMinutes <- merged$retentionTimeInSeconds.y / 60
      }
      
      merged
    }, error = function(e) {
      showNotification(paste("Error merging data:", e$message), type = "error", duration = 10)
      NULL
    })
  })
  
  filtered_data <- reactive({
    req(merged_data())
    merged_data() %>% filter(Mass.accuracy.ppm > -input$ppm_threshold & Mass.accuracy.ppm < input$ppm_threshold)
  })
  
  # --- Reactive Summary Tables (Based ONLY on Canopus file) ---
  cf_superclass_data <- reactive({ get_summary_data(canopus_data(), "superclass", superclass_colors, "X.canopus.Superclass.", input$min_count) })
  cf_class_data <- reactive({ get_summary_data(canopus_data(), "class", class_colors, "X.canopus.Class.", input$min_count) })
  cf_subclass_data <- reactive({ get_summary_data(canopus_data(), "subclass", subclass_colors, "subclass", input$min_count) })
  npc_pathway_data <- reactive({ get_summary_data(canopus_data(), "NPC_pathway", npc_pathway_colors, "Pathway", input$min_count) })
  npc_superclass_data <- reactive({ get_summary_data(canopus_data(), "NPC_superclass", npc_superclass_colors, "Superclass", input$min_count) })
  npc_class_data <- reactive({ get_summary_data(canopus_data(), "NPC_class", npc_class_colors, "Class", input$min_count) })
  
  # --- Render Plots ---
  output$plot_cf_superclass <- renderPlotly({ render_pie(cf_superclass_data(), "Superclass", input$cf_scale, "pie_cf_", input$cf_format) })
  output$plot_cf_class <- renderPlotly({ render_pie(cf_class_data(), "Class", input$cf_scale, "pie_cf_", input$cf_format) })
  output$plot_cf_subclass <- renderPlotly({ render_pie(cf_subclass_data(), "Subclass", input$cf_scale, "pie_cf_", input$cf_format) })
  output$plot_npc_pathway <- renderPlotly({ render_pie(npc_pathway_data(), "NPC Pathway", input$npc_scale, "pie_npc_", input$npc_format) })
  output$plot_npc_superclass <- renderPlotly({ render_pie(npc_superclass_data(), "NPC Superclass", input$npc_scale, "pie_npc_", input$npc_format) })
  output$plot_npc_class <- renderPlotly({ render_pie(npc_class_data(), "NPC Class", input$npc_scale, "pie_npc_", input$npc_format) })
  
  # --- Dynamic Color Pickers for Pie Charts ---
  output$pie_cf_color_pickers <- renderUI({
    req(cf_superclass_data(), cf_class_data(), cf_subclass_data())
    
    df1 <- cf_superclass_data()[, c("Group", "ColorCode")]
    df2 <- cf_class_data()[, c("Group", "ColorCode")]
    df3 <- cf_subclass_data()[, c("Group", "ColorCode")]
    combined <- unique(rbind(df1, df2, df3))
    
    lapply(1:nrow(combined), function(i) {
      g <- combined$Group[i]
      def_col <- combined$ColorCode[i]
      if (is.na(def_col)) def_col <- "#808080"
      safe_g <- gsub("[^[:alnum:]]", "", g)
      div(style = "width: 150px;", colourpicker::colourInput(paste0("pie_cf_", safe_g), g, value = def_col))
    })
  })
  
  output$pie_npc_color_pickers <- renderUI({
    req(npc_pathway_data(), npc_superclass_data(), npc_class_data())
    
    df1 <- npc_pathway_data()[, c("Group", "ColorCode")]
    df2 <- npc_superclass_data()[, c("Group", "ColorCode")]
    df3 <- npc_class_data()[, c("Group", "ColorCode")]
    combined <- unique(rbind(df1, df2, df3))
    
    lapply(1:nrow(combined), function(i) {
      g <- combined$Group[i]
      def_col <- combined$ColorCode[i]
      if (is.na(def_col)) def_col <- "#808080"
      safe_g <- gsub("[^[:alnum:]]", "", g)
      div(style = "width: 150px;", colourpicker::colourInput(paste0("pie_npc_", safe_g), g, value = def_col))
    })
  })
  
  # --- Dynamic Color Pickers for Sunbursts ---
  output$sun_cf_color_pickers <- renderUI({
    req(canopus_data())
    superclasses <- unique(canopus_data()$superclass)
    lapply(superclasses, function(sc) {
      default_col <- superclass_colors$ColorCode[superclass_colors$X.canopus.Superclass. == sc]
      if (length(default_col) == 0 || is.na(default_col)) default_col <- "#808080"
      div(style = "width: 150px;", colourpicker::colourInput(paste0("col_cf_", gsub("[^[:alnum:]]", "", sc)), sc, value = default_col))
    })
  })
  
  output$sun_npc_color_pickers <- renderUI({
    req(canopus_data())
    pathways <- unique(canopus_data()$NPC_pathway)
    lapply(pathways, function(pw) {
      default_col <- npc_pathway_colors$ColorCode[npc_pathway_colors$Pathway == pw]
      if (length(default_col) == 0 || is.na(default_col)) default_col <- "#808080"
      div(style = "width: 150px;", colourpicker::colourInput(paste0("col_npc_", gsub("[^[:alnum:]]", "", pw)), pw, value = default_col))
    })
  })
  
  # --- Sunburst Renderers ---
  output$plot_sunburst_classyfire <- renderPlotly({
    req(canopus_data())
    d <- canopus_data()
    d$ids <- paste(d$superclass, d$class, d$subclass, sep = " - ")
    d$parents <- paste(d$superclass, d$class, sep = " - ")
    r1 <- unique(data.frame(ids=d$superclass, labels=d$superclass, parents="", stringsAsFactors=F))
    r2 <- unique(data.frame(ids=d$parents, labels=d$class, parents=d$superclass, stringsAsFactors=F))
    r3 <- unique(data.frame(ids=d$ids, labels=d$subclass, parents=d$parents, stringsAsFactors=F))
    
    df_sun <- rbind(r1, r2, r3)
    df_sun$root <- sapply(strsplit(df_sun$ids, " - "), `[`, 1)
    
    cols <- sapply(df_sun$root, function(sc) {
      input_id <- paste0("col_cf_", gsub("[^[:alnum:]]", "", sc))
      val <- input[[input_id]]
      if (!is.null(val)) val else {
        def <- superclass_colors$ColorCode[superclass_colors$X.canopus.Superclass. == sc]
        if (length(def) > 0 && !is.na(def[1])) def[1] else "#808080"
      }
    })
    
    scale_val <- if (!is.null(input$sun_cf_scale)) input$sun_cf_scale else 3
    format_val <- if (!is.null(input$sun_cf_format)) input$sun_cf_format else 'png'
    plot_ly(df_sun, ids=~ids, labels=~labels, parents=~parents, type='sunburst', maxdepth=3, marker = list(colors = unname(cols))) %>%
      config(toImageButtonOptions = list(format = format_val, filename = 'sunburst_classyfire', scale = scale_val))
  })
  
  output$plot_sunburst_npc <- renderPlotly({
    req(canopus_data())
    d <- canopus_data()
    d$ids <- paste(d$NPC_pathway, d$NPC_superclass, d$NPC_class, sep = " - ")
    d$parents <- paste(d$NPC_pathway, d$NPC_superclass, sep = " - ")
    r1 <- unique(data.frame(ids=d$NPC_pathway, labels=d$NPC_pathway, parents="", stringsAsFactors=F))
    r2 <- unique(data.frame(ids=d$parents, labels=d$NPC_superclass, parents=d$NPC_pathway, stringsAsFactors=F))
    r3 <- unique(data.frame(ids=d$ids, labels=d$NPC_class, parents=d$parents, stringsAsFactors=F))
    
    df_sun <- rbind(r1, r2, r3)
    df_sun$root <- sapply(strsplit(df_sun$ids, " - "), `[`, 1)
    
    cols <- sapply(df_sun$root, function(pw) {
      input_id <- paste0("col_npc_", gsub("[^[:alnum:]]", "", pw))
      val <- input[[input_id]]
      if (!is.null(val)) val else {
        def <- npc_pathway_colors$ColorCode[npc_pathway_colors$Pathway == pw]
        if (length(def) > 0 && !is.na(def[1])) def[1] else "#808080"
      }
    })
    
    scale_val <- if (!is.null(input$sun_npc_scale)) input$sun_npc_scale else 3
    format_val <- if (!is.null(input$sun_npc_format)) input$sun_npc_format else 'png'
    plot_ly(df_sun, ids=~ids, labels=~labels, parents=~parents, type='sunburst', maxdepth=3, marker = list(colors = unname(cols))) %>%
      config(toImageButtonOptions = list(format = format_val, filename = 'sunburst_npc', scale = scale_val))
  })
  
  # --- Outputs & Downloads ---
  output$preview_table <- renderDT({ 
    if(!is.null(input$identifications_file)) {
      req(merged_data())
      datatable(head(merged_data(), 50), options = list(scrollX = T)) 
    } else {
      req(canopus_data())
      datatable(head(canopus_data(), 50), options = list(scrollX = T)) 
    }
  })
  
  output$dl_merged <- downloadHandler(
    filename = "SIRIUS_Merged.csv", 
    content = function(f) { 
      req(merged_data())
      write.csv(merged_data(), f, row.names=F) 
    }
  )
  
  output$dl_filtered <- downloadHandler(
    filename = function() { paste0("SIRIUS_Filtered_", input$ppm_threshold, "ppm.csv") }, 
    content = function(f) { 
      req(filtered_data())
      write.csv(filtered_data(), f, row.names=F) 
    }
  )
  
  output$dl_cf_super <- downloadHandler(filename = "ClassyFire_Superclass_Summary.csv", content = function(f) { write.csv(cf_superclass_data(), f, row.names=F) })
  output$dl_cf_class <- downloadHandler(filename = "ClassyFire_Class_Summary.csv", content = function(f) { write.csv(cf_class_data(), f, row.names=F) })
  output$dl_cf_sub <- downloadHandler(filename = "ClassyFire_Subclass_Summary.csv", content = function(f) { write.csv(cf_subclass_data(), f, row.names=F) })
  output$dl_npc_path <- downloadHandler(filename = "NPC_Pathway_Summary.csv", content = function(f) { write.csv(npc_pathway_data(), f, row.names=F) })
  output$dl_npc_super <- downloadHandler(filename = "NPC_Superclass_Summary.csv", content = function(f) { write.csv(npc_superclass_data(), f, row.names=F) })
  output$dl_npc_class <- downloadHandler(filename = "NPC_Class_Summary.csv", content = function(f) { write.csv(npc_class_data(), f, row.names=F) })
}

shinyApp(ui, server)
