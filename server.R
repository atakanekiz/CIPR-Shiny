server <- function(input, output){
  
  library(tibble)
  library(dplyr)
  library(ggpubr)
  library(gtools)
  library(shinyWidgets)
  
  
  values <- reactiveValues()
  
  
  # Max allowed size of the for the uploaded csv files
  options(shiny.maxRequestSize=75*1024^2) 
  
  # Setup a brush table function to show more info about data points in top 5 plots
  output$brushtop5 <- renderTable({
    
    validate(
      need(input$brushtop5, "Draw a rectangle around data points for further information")
    )
    
    brushedPoints(top5_df_brush, input$brushtop5)
  }, striped = T, hover = T, spacing = "s")
  
  
  # Disable download button until the execution
  shinyjs::disable("download_res")
  shinyjs::disable("download_top5")
  
  observeEvent(analyzed_df(), {
    shinyjs::enable("download_res")
    shinyjs::enable("download_top5")
  })
  
  
  ################################################################################################################################
  # Define conditional dynamic file upload prompted when user selects "Custom" as reference data
  output$ui_sel_ref <- renderUI ({
    
    if (input$sel_reference == "Custom"){
      fileInput("ref_file", "Upload custom reference file",
                multiple = F, 
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    }
  })
  
  # Select reference cell subsets to analyze
  output$ui_sel_subsets <- renderUI ({
    
    if(input$sel_reference == "ImmGen (mouse)"){
      
      pickerInput("cell_subsets", "Select reference cell subsets to include in analysis",
                  multiple = T,
                  choices = c("B cell", "Basophil", "DC", "Eosinophil", "gd-T cell",
                              "Granulocyte", "ILC-1", "ILC-2", "ILC-3", "Macrophage",
                              "Mast cell", "Monocyte", "NK cell", "NKT cell", "Pre-B cell",
                              "Pre-T cell", "Stem-Progenitor", "Stromal", "T cell", "Treg"),
                  selected = c("B cell", "Basophil", "DC", "Eosinophil", "gd-T cell",
                               "Granulocyte", "ILC-1", "ILC-2", "ILC-3", "Macrophage",
                               "Mast cell", "Monocyte", "NK cell", "NKT cell", "Pre-B cell",
                               "Pre-T cell", "Stem-Progenitor", "Stromal", "T cell", "Treg"),
                  options = list(`actions-box` = TRUE))
      
    } else if(input$sel_reference == "Presorted RNAseq (mouse)"){
      
      pickerInput("cell_subsets", "Select reference cell subsets to analyze",
                  multiple = T,
                  choices = c("Adipocytes", "Astrocytes", "B cells", "Cardiomyocytes", "Dendritic cells",
                              "Endothelial cells", "Epithelial cells",  "Erythrocytes", "Fibroblasts",
                              "Granulocytes", "Hepatocytes", "Macrophages", "Microglia", "Monocytes",
                              "Neurons", "NK cells", "Oligodendrocytes", "T cells"),
                  selected = c("Adipocytes", "Astrocytes", "B cells", "Cardiomyocytes", "Dendritic cells",
                               "Endothelial cells", "Epithelial cells",  "Erythrocytes", "Fibroblasts",
                               "Granulocytes", "Hepatocytes", "Macrophages", "Microglia", "Monocytes",
                               "Neurons", "NK cells", "Oligodendrocytes", "T cells" ),
                  options = list(`actions-box` = TRUE))
      
    } else if(input$sel_reference == "Blueprint-Encode (human)"){
      
      pickerInput("cell_subsets", "Select reference cell subsets to analyze",
                  multiple = T,
                  choices = c("Adipocytes",  "B-cells", "CD4+ T-cells",  "CD8+ T-cells",  "Chondrocytes",
                              "DC", "Endothelial cells",  "Eosinophils", "Epithelial cells", "Erythrocytes",
                              "Fibroblasts", "HSC",  "Keratinocytes", "Macrophages", "Melanocytes" ,
                              "Mesangial cells", "Monocytes", "Myocytes", "Neurons", "Neutrophils" ,
                              "NK cells", "Pericytes", "Skeletal muscle", "Smooth muscle"),
                  selected = c("Adipocytes",  "B-cells", "CD4+ T-cells",  "CD8+ T-cells",  "Chondrocytes",
                               "DC", "Endothelial cells",  "Eosinophils", "Epithelial cells", "Erythrocytes",
                               "Fibroblasts", "HSC",  "Keratinocytes", "Macrophages", "Melanocytes" ,
                               "Mesangial cells", "Monocytes", "Myocytes", "Neurons", "Neutrophils" ,
                               "NK cells", "Pericytes", "Skeletal muscle", "Smooth muscle"),
                  options = list(`actions-box` = TRUE))
      
    } else if(input$sel_reference == "Primary Cell Atlas (human)"){
      
      pickerInput("cell_subsets", "Select reference cell subsets to analyze",
                  multiple = T,
                  choices = c("Astrocyte", "B_cell",  "BM", "BM & Prog.",
                              "Chondrocytes", "CMP", "DC", "Embryonic_stem_cells",
                              "Endothelial_cells", "Epithelial_cells", "Erythroblast", "Fibroblasts",
                              "Gametocytes", "GMP", "Hepatocytes", "HSC_-G-CSF",
                              "HSC_CD34+", "iPS_cells", "Keratinocytes",  "Macrophage",
                              "MEP", "Monocyte",  "MSC", "Myelocyte",
                              "Neuroepithelial_cell",  "Neurons", "Neutrophils", "NK_cell" ,
                              "Osteoblasts", "Platelets", "Pre-B_cell_CD34-", "Pro-B_cell_CD34+",
                              "Pro-Myelocyte",  "Smooth_muscle_cells", "T_cells", "Tissue_stem_cells"),
                  selected = c("Astrocyte", "B_cell",  "BM", "BM & Prog.",
                               "Chondrocytes", "CMP", "DC", "Embryonic_stem_cells",
                               "Endothelial_cells", "Epithelial_cells", "Erythroblast", "Fibroblasts",
                               "Gametocytes", "GMP", "Hepatocytes", "HSC_-G-CSF",
                               "HSC_CD34+", "iPS_cells", "Keratinocytes",  "Macrophage",
                               "MEP", "Monocyte",  "MSC", "Myelocyte",
                               "Neuroepithelial_cell",  "Neurons", "Neutrophils", "NK_cell" ,
                               "Osteoblasts", "Platelets", "Pre-B_cell_CD34-", "Pro-B_cell_CD34+",
                               "Pro-Myelocyte",  "Smooth_muscle_cells", "T_cells", "Tissue_stem_cells"),
                  options = list(`actions-box` = TRUE))
      
    } else if(input$sel_reference == "DICE (human)"){
      
      pickerInput("cell_subsets", "Select reference cell subsets to analyze",
                  multiple = T,
                  choices = c("CD4+ T cell", "CD8+ T cell", "NK cell", "B cell", "Monocyte"),
                  selected = c("CD4+ T cell", "CD8+ T cell", "NK cell", "B cell", "Monocyte"),
                  options = list(`actions-box` = TRUE))
      
      
    } else if(input$sel_reference == "Hematopoietic diff (human)"){
      
      pickerInput("cell_subsets", "Select reference cell subsets to analyze",
                  multiple = T,
                  choices = c("B cells", "Basophils", "CD4+ T cells", "CD8+ T cells", "CMPs",
                              "Dendritic cells",  "Eosinophils", "Erythroid cells",  "GMPs", "Granulocytes",
                              "HSCs", "Megakaryocytes", "MEPs", "Monocytes", "NK cells", "NK T cells"),
                  selected = c("B cells", "Basophils", "CD4+ T cells", "CD8+ T cells", "CMPs",
                               "Dendritic cells",  "Eosinophils", "Erythroid cells",  "GMPs", "Granulocytes",
                               "HSCs", "Megakaryocytes", "MEPs", "Monocytes", "NK cells", "NK T cells"),
                  options = list(`actions-box` = TRUE))
      
    } else if(input$sel_reference == "Presorted RNAseq (human)"){
      
      pickerInput("cell_subsets", "Select reference cell subsets to analyze",
                  multiple = T,
                  choices = c("B cells", "Basophils", "CD4+ T cells", "CD8+ T cells", "Dendritic cells",
                              "Monocytes", "Neutrophils", "NK cells",  "Progenitors", "T cells" ),
                  selected = c("B cells", "Basophils", "CD4+ T cells", "CD8+ T cells", "Dendritic cells",
                               "Monocytes", "Neutrophils", "NK cells",  "Progenitors", "T cells" ),
                  options = list(`actions-box` = TRUE))
      
    } else {}
    
  })
  
  
  
  # Create a dynamic UI object to enable upload dialog upon selection of 'Custom'
  output$ui_sel_ref_annot <- renderUI ({
    
    if (input$sel_reference == "Custom"){
      fileInput("annot_file", "Upload custom annotation file (optional)",
                multiple = F, 
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    }
  })
  
  
  # Show example input file format for logFC dot product method
  output$sample_data_file_logfc <- renderImage({
    
    list(src = "data/cluster_expr_IMG.png",
         alt = "Sample SCseq data",
         width=500)
    
  }, deleteFile = F)
  
  # Show example input file format for reference data set
  output$sample_reference_file <- renderImage({
    
    list(src = "data/ref_data_IMG.png",
         alt = "Sample reference data",
         width=500)
    
  }, deleteFile = F)
  
  # Show example input file format for reference annotation
  output$sample_annotation_file <- renderImage({
    
    list(src = "data/ref_annot_IMG.png",
         alt = "Sample annotation data",
         width=500)
    
  }, deleteFile = F)
  
  ################################################################################################################################
  # Read uploaded differential expression file
  user_data <- reactive({
    
    inFile <- input$data_file
    
    if(grepl("logFC", input$comp_method)){
      
      if(input$example_data == T){
        
        
        dat <- read.csv("data/Trimmed_cluster_signatures.csv",
                        check.names = T,
                        strip.white = T, 
                        stringsAsFactors = F)
        
        
        
      } else {
        
        validate(
          need(input$data_file != "", "Please upload a data set or use example data")
        )
        
        # Make sure the file type is correct
        validate(
          need(tools::file_ext(inFile$name) %in% c(
            'text/csv',
            'text/comma-separated-values',
            'text/plain',
            'csv'
          ), "Wrong File Format try again!"))
        
        
        
        dat <- read.csv(inFile$datapath, check.names=TRUE, strip.white = TRUE, stringsAsFactors = F)
        
        validate(
          need(!anyNA(colnames(dat)), "Unnamed column is found in the uploaded data. Please fix this problem and try again. When manually preparing dataframes, sometimes, empty-looking columns may have 'invisible' data associated with them. Try deleting these columns using a spreadsheet software or re-make a clean csv file.")
        )
        
        # Make sure the column names are proper for correct subsetting
        validate(
          need(
            {if(sum(Reduce("|", lapply(c("logfc", "gene", "cluster"), grepl, colnames(dat), ignore.case=T))) == 3) TRUE else FALSE},
            "Formatting error: Make sure your dataset contains at least three columns named 'logfc', 'gene', and 'cluster' (Capitalization of the column names is not important. Data can have other columns which will be ignored). Did you mean to use correlation methods instead?"
          )
        )
        
      }
      
      # Define column names to allow flexibility in case and close matches in column names
      gene_column <<- grep("gene", colnames(dat), ignore.case = T, value = T)
      logFC_column <<- grep("logfc", colnames(dat), ignore.case = T, value = T)
      cluster_column <<- grep("cluster", colnames(dat), ignore.case = T, value = T)
      
      req(input$run)
      
      # Convert gene symbols to lower case letters to allow mouse-vs-human comparisons
      dat[,gene_column] <- tolower(dat[,gene_column])
      
      dat
      
    } else {
      
      
      
      if(is.null(inFile) & input$example_data == T){
        
        
        
        dat <- readRDS("data/til_scseq_exprs_mean_subset.rds")
        
        req(input$run)
        
        gene_column <<- grep("gene", colnames(dat), ignore.case = T, value = T)
        
        dat
        
        
        
        
        
      } else {
        
        validate(
          need(input$data_file != "", "Please upload a data set or use example data")
        )
        
        # Make sure the file type is correct
        validate(
          need(tools::file_ext(inFile$name) %in% c(
            'text/csv',
            'text/comma-separated-values',
            'text/plain',
            'csv'
          ), "Wrong File Format. File needs to be a .csv file."))
        
        
        
        dat <- read.csv(inFile$datapath, check.names=TRUE, strip.white = TRUE, stringsAsFactors = F)
        
        validate(
          need(!anyNA(colnames(dat)), "Unnamed column is found in the uploaded data. Please fix this problem and try again. When manually preparing dataframes, sometimes, empty-looking columns may have 'invisible' data associated with them. Try deleting these columns using a spreadsheet software or re-make a clean csv file.")
        )
        
        
        
        # Make sure the column names are proper for correct subsetting
        validate(
          need(
            {if(sum(Reduce("|", lapply(c("logfc", "gene"), grepl, colnames(dat), ignore.case=T))) == 1) TRUE else FALSE},
            "Formatting error: Make sure your dataset contains a column named 'gene' (capitalization is not important, duplicate gene column is not allowed). Other columns should contain average gene expression per cluster. Did you mean to use logFC comparison methods?"
          )
        )
        
        
        
        req(input$run)
        
        gene_column <<- grep("gene", colnames(dat), ignore.case = T, value = T)
        
        dat[,gene_column] <- tolower(dat[,gene_column])
        
        dat <- dat[!duplicated(dat[,gene_column]),]
        
        dat
        
      }
    }
    
    
  }) # close user_data reactive object
  
  
  ################################################################################################################################
  
  # Delay slider update
  var_filt <- reactive({input$var_filter})
  
  
  #############################################################################################################################
  
  # Keep selected reference subsets in analysis
  subsets_in_analysis <- reactive({
    
    if(input$sel_reference != "Custom"){
      
      
      select_positions <- which(reference_annotation()[, "reference_cell_type"] %in% input$cell_subsets)
      
      # Bump positions by one (the corresponding column number due to first column being gene name in ref)
      
      select_positions <- select_positions + 1
      
      # Append position 1 to select gene column
      
      select_positions <- c(1, select_positions)  # make this prettier by using name matching (no ordering is needed)
      
      values$subset_num <- length(select_positions)-1
      
      select_positions
      
      
    } else if(input$sel_reference == "Custom"){ 
      # expand on this to enable factor level matching from custom files
      
      select_positions <- dim(ref_data())[2]
      
      values$subset_num <- length(select_positions)-1
      
      select_positions
      
    }
    
    
    
    
  })
  
  
  
  
  ################################################################################################################################
  
  
  
  
  # Read reference dataset
  ref_data <- reactive({
    
    # if(grepl("logFC", input$comp_method)){
    
    
    if(input$sel_reference == "ImmGen (mouse)"){
      
      # Read main expression dataframe
      reference <- as.data.frame(readRDS("data/immgen.rds"))
      
      # Name of the gene column in reference data
      ref_gene_column <<- grep("gene", colnames(reference), ignore.case = T, value = T)
      
      
    } else if(input$sel_reference == "Presorted RNAseq (mouse)"){
      
      
      # Read main expression dataframe
      reference <- as.data.frame(readRDS("data/mmrnaseq_expr.rds"))
      
      # Name of the gene column in reference data
      ref_gene_column <<- grep("gene", colnames(reference), ignore.case = T, value = T)
      
      
    } else if(input$sel_reference == "Blueprint-Encode (human)"){
      
      
      # Read main expression dataframe
      reference <- as.data.frame(readRDS("data/blueprint_expr.rds"))
      
      # Name of the gene column in reference data
      ref_gene_column <<- grep("gene", colnames(reference), ignore.case = T, value = T)
      
      
    }  else if(input$sel_reference == "Primary Cell Atlas (human)"){
      
      
      # Read main expression dataframe
      reference <- as.data.frame(readRDS("data/hpca_expr.rds"))
      
      # Name of the gene column in reference data
      ref_gene_column <<- grep("gene", colnames(reference), ignore.case = T, value = T)
      
      
    } else if(input$sel_reference == "DICE (human)"){
      
      
      # Read main expression dataframe
      reference <- as.data.frame(readRDS("data/dice_expr.rds"))
      
      # Name of the gene column in reference data
      ref_gene_column <<- grep("gene", colnames(reference), ignore.case = T, value = T)
      
      
    } else if(input$sel_reference == "Hematopoietic diff (human)"){
      
      
      # Read main expression dataframe
      reference <- as.data.frame(readRDS("data/hema_expr.rds"))
      
      # Name of the gene column in reference data
      ref_gene_column <<- grep("gene", colnames(reference), ignore.case = T, value = T)
      
      
    } else if(input$sel_reference == "Presorted RNAseq (human)"){
      
      
      # Read main expression dataframe
      reference <- as.data.frame(readRDS("data/hsrnaseq_expr.rds"))
      
      # Name of the gene column in reference data
      ref_gene_column <<- grep("gene", colnames(reference), ignore.case = T, value = T)
      
      
    } else {
      
      validate(
        need(input$ref_file != "", "Please upload reference data set")
      )
      
      
      in_refFile <- input$ref_file
      
      # Make sure the file type is correct
      validate(
        need(tools::file_ext(in_refFile$name) %in% c(
          'text/csv',
          'text/comma-separated-values',
          'text/plain',
          'csv'
        ), "Wrong File Format. File needs to be a .csv file."))
      
      reference <- read.csv(in_refFile$datapath, 
                            check.names=TRUE,
                            strip.white = TRUE, 
                            stringsAsFactors = F)
      
      ref_gene_column <<- grep("gene", colnames(reference), ignore.case = T, value = T)
      
    }
    
    reference[,ref_gene_column] <- tolower(reference[,ref_gene_column])
    
    # Report dims of unfiltered reference dataframe
    
    values$ref_rows <- dim(reference)[1]
    values$ref_cols <- dim(reference)[2]-1
    
    
    # Subset the reference to the relevant subsets in analysis
    reference <- reference[, subsets_in_analysis()]
    
    
    
    # Apply quantile filtering
    if(var_filt() != 100){
      
      var_vec <- apply(reference[, !colnames(reference) %in% ref_gene_column], 1, var, na.rm=T)
      
      keep_var <- quantile(var_vec, probs = 1-var_filt()/100, na.rm = T)
      
      keep_genes <- var_vec >= keep_var
      
      # Return reference data frame
      reference <- as.data.frame(reference[keep_genes, ])
      
    } else {
      
      keep_genes <- rep(T, dim(as.data.frame(reference))[1])
      reference <- as.data.frame(reference)
      
    }
    
    
    
    
    if(grepl("logFC", input$comp_method)){
      
      # Calculate row means for each gene (mean expression across the reference cell types)
      gene_avg <- rowMeans(reference[, !colnames(reference) %in% ref_gene_column])
      
      
      # Calculate the ratio of gene expression in a given cell type compared 
      # to the average of the whole cohort. Calculate log (natural) fold change
      
      # Linear data
      # reference_ratio <- log1p(sweep(reference[,!colnames(reference) %in% ref_gene_column], 1, FUN="/", gene_avg))
      
      # Log scale data
      reference_ratio <- sweep(reference[,!colnames(reference) %in% ref_gene_column], 1, FUN="-", gene_avg)
      
      
      # Combine gene names and the log fold change in one data frame
      reference <- cbind(tolower(reference[,ref_gene_column]), reference_ratio)
      
      colnames(reference)[1] <- ref_gene_column
      
      
    } 
    
    # else {
    #   
    #   if(input$sel_reference == "ImmGen (mouse)"){
    #     
    #     reference <- readRDS("data/immgen.rds")
    #     
    #     ref_gene_column <<- grep("gene", colnames(reference), ignore.case = T, value = T)
    #     
    #   } else if (input$sel_reference == "Custom"){
    #     
    #     in_refFile <- input$ref_file
    #     
    #     reference <- read.csv(in_refFile$datapath, check.names=TRUE, strip.white = TRUE, stringsAsFactors = F)
    #     
    #     ref_gene_column <<- grep("gene", colnames(reference), ignore.case = T, value = T)
    #     
    #     reference[,ref_gene_column] <- tolower(reference[,ref_gene_column])
    #     
    #     
    #   } 
    #   
    # }
    
    
    # # Subset the reference to the relevant subsets in analysis
    # reference <- reference[, subsets_in_analysis()]
    
    # # Report dims of unfiltered reference dataframe
    # 
    # values$ref_rows <- dim(reference)[1]
    # values$ref_cols <- dim(reference)[2]-1
    
    
    
    # # Apply quantile filtering
    # if(var_filt() != 100){
    #   
    #   var_vec <- apply(reference[, !colnames(reference) %in% ref_gene_column], 1, var, na.rm=T)
    #   
    #   keep_var <- quantile(var_vec, probs = 1-var_filt()/100, na.rm = T)
    #   
    #   keep_genes <- var_vec >= keep_var
    #   
    #   # Return reference data frame
    #   refdat <- as.data.frame(reference[keep_genes, ])
    #   
    # } else {
    #   keep_genes <- rep(T, dim(as.data.frame(reference))[1])
    #   refdat <- as.data.frame(reference)
    # }
    
    # Report gene number after filtering 
    values$keep_genes <- sum(keep_genes)
    
    reference
    
  })
  
  ################################################################################################################################
  # Read immgen annotation file for explanations of cell types
  
  
  reference_annotation <- reactive({
    
    if(input$sel_reference == "ImmGen (mouse)"){
      
      # This file was obtained from the ImmGen website source code that runs the analysis modules.
      # It features detailed information about the cellular origins and sorting methods and makes the results
      # understandable by providing long names and descriptions to abbreviated cell types
      ref_annotation <-readRDS("data/immgen_annot.rds")
      ref_annotation
      
    } else if(input$sel_reference == "Presorted RNAseq (mouse)"){
      
      ref_annotation <- readRDS("data/mmrnaseq_samples.rds")
      ref_annotation
      
    } else if(input$sel_reference == "Blueprint-Encode (human)"){
      
      ref_annotation <- readRDS("data/blueprint_samples.rds")
      ref_annotation
      
    } else if(input$sel_reference == "Primary Cell Atlas (human)"){
      
      ref_annotation <- readRDS("data/hpca_samples.rds")
      ref_annotation
      
    } else if(input$sel_reference == "DICE (human)"){
      
      ref_annotation <- readRDS("data/dice_samples.rds")
      ref_annotation
      
    } else if(input$sel_reference == "Hematopoietic diff (human)"){
      
      ref_annotation <- readRDS("data/hema_samples.rds")
      ref_annotation
      
    } else if(input$sel_reference == "Presorted RNAseq (human)"){
      
      ref_annotation <- readRDS("data/hsrnaseq_samples.rds")
      ref_annotation
      
    } else if(input$sel_reference == "Custom"){
      
      annotFile <- input$annot_file
      
      ref_annotation <- read.csv(annotFile$datapath, check.names=TRUE, strip.white = TRUE, stringsAsFactors = F)
      ref_annotation
      
    }
  })
  
  
  
  
  
  
  ################################################################################################################################
  # Define a reactive cluster object that will store cluster information
  clusters <- reactive({
    
    if(grepl("logFC", input$comp_method)){
      
      
      # Get the clusters and sort them in incrementing order from cluster column
      # This is needed to generate results per cluster
      gtools::mixedsort(
        levels(
          as.factor(
            pull(user_data(), grep("cluster", x = colnames(user_data()), ignore.case = T, value = T)
            )
          )
        )
      )
      
    } else {
      
      gtools::mixedsort(
        levels(
          as.factor(
            colnames(user_data())[!grepl("gene", colnames(user_data()), ignore.case = T)]
          )
        )
      )}
    
    
  }) # close clusters reactive object
  
  
  
  ################################################################################################################################
  # Compare user_data against reference file
  analyzed_df <- eventReactive(input$run, {
    
    req(input$run)
    
    if(grepl("logFC", input$comp_method)){
      
      
      if(input$comp_method == "logFC dot product"){
        
        # Initiate a master data frame to store the results
        master_df <- data.frame()
        
        # Indicate progress of the calculations
        withProgress(message = 'Analysis in progress', value = 0, {
          
          values$current_cluster <- character()
          values$genes_in_analysis <- numeric()
          
          # Iterate over clusters to calculate a distinct identity score for each reference cell type
          for (i in clusters()) {
            
            values$current_cluster <- c(values$current_cluster, i)
            
            
            
            # Increment the progress bar, and update the detail text.
            incProgress(1/length(clusters()), detail = paste("Analyzing cluster", i))
            
            # Subset on the cluster in iteration
            sel_clst <- user_data() %>%
              filter(!!rlang::sym(cluster_column) == i) %>%
              select_(.dots = c(gene_column, logFC_column))
            
            genes_in_analysis <- length(intersect(sel_clst[, gene_column], ref_data()[, ref_gene_column]))
            
            values$genes_in_analysis <- c(values$genes_in_analysis, genes_in_analysis)
            
            # Merge SCseq cluster log FC value with immgen log FC for shared genes
            merged <- merge(sel_clst, ref_data(), by.x = gene_column, by.y = ref_gene_column)
            
            
            # Calculate a scoring matrix by multiplying log changes of clusters and immgen cells
            reference_scoring <- data.frame(apply(merged[,3:dim(merged)[2]],2,function(x){x*merged[,2]}), check.names = FALSE)
            
            # Calculate the aggregate score of each immgen cell type by adding
            score_sum <- colSums(reference_scoring)
            
            # Store identity scores in a data frame
            df <- data.frame(identity_score = score_sum)
            
            df <- rownames_to_column(df, var="reference_id")
            
            
            # Merge results with annotation data for informative graphs
            if(input$sel_reference != "Custom"){   #ImmGen (previously)
              
              df <- left_join(df, reference_annotation(), by=c("reference_id" = "short_name"))
              
              
              
              
            } else if (input$sel_reference == "Custom" & !is.null(input$annot_file)){
              
              df <- left_join(df, reference_annotation(), by=c("reference_id" = "short_name"))
              
              
            } else if(input$sel_reference == "Custom" & is.null(input$annot_file)){
              
              # If annotation file is not provided for custom analyses, the table will be populated 
              # with "Upload annotation file" reminder
              df$reference_cell_type <- rep("Upload annotation file", dim(ref_data())[2]-1)
              df$short_name <- colnames(ref_data())[!colnames(ref_data()) %in% ref_gene_column]
              df$long_name <- rep("Upload annotation file", dim(ref_data())[2]-1)
              df$description <- rep("Upload annotation file", dim(ref_data())[2]-1)
              
            }
            
            
            
            # Store cluster information in a column
            df$cluster <- i
            
            # Add confidence-of-prediction calculations here and append to the df
            # Calculate the mean and standard deviation of the aggregate scores per reference cell type
            mean_score_sum <- mean(df$identity_score)
            score_sum_sd <- sd(df$identity_score)
            
            # Calculate the distance of the identity score from population mean (how many std devs apart?)
            df$z_score <- (df$identity_score - mean_score_sum)/score_sum_sd
            
            # Calculate the proportion of the genes changing in the same direction between unknown cluster and reference cell type
            df$percent_pos_correlation <- {
              
              ngenes <- dim(reference_scoring)[1]
              
              pos_corr_vector <- numeric()
              
              for(i in 1:dim(reference_scoring)[2]){
                
                # Calculate number of genes positively correlated (upregulated or downregulated in both unk cluster and reference)
                pos_cor <- ( sum(reference_scoring[, i] > 0) / ngenes ) * 100
                
                pos_corr_vector <- c(pos_corr_vector, pos_cor)
                
              } #close for loop
              
              pos_corr_vector
              
            } # close expression 
            
            
            # Add calculation results under the master data frame to have a composite results file
            master_df <- rbind(master_df,df)
            
            
            
          } # close for loop that iterates over clusters
          
        })
        
        # Return results into reactive object
        master_df
        
        
        # If correlation method is used, algorithm follows the steps below to calculate a
        # correlation coefficient for each cluster and reference cell type pairs.
        
      } else {    ################### Correlation methods ###########################################################
        
        # Initiate master data frame to store results
        master_df <- data.frame()
        
        # Print progress
        withProgress(message = 'Analysis in progress', value = 0, {
          
          # Pass comp_method variable from the user-selected radio buttons
          if(input$comp_method == "logFC Spearman") comp_method = "spearman" else if(input$comp_method == "logFC Pearson") comp_method = "pearson"
          
          
          values$current_cluster <- character()
          values$genes_in_analysis <- numeric()
          
          # Iterate analysis for each cluster. The loop below will calculate a distinct correlation
          # coefficient for each cluster-reference cell pairs 
          for (i in clusters()) {
            
            # Increment the progress bar, and update the detail text.
            incProgress(1/length(clusters()), detail = paste("Analyzing cluster", i))
            
            trim_dat <- user_data() %>%
              filter(!!rlang::sym(cluster_column) == i)
            
            dat_genes <- trim_dat[gene_column] %>% pull() %>% as.character
            ref_genes <- ref_data()[ref_gene_column] %>% pull() %>% as.character
            
            common_genes <- intersect(dat_genes, ref_genes)
            
            values$genes_in_analysis <- c(values$genes_in_analysis, length(common_genes))
            
            values$current_cluster <- c(values$current_cluster, i)
            
            trim_dat <- trim_dat %>%
              filter(!!rlang::sym(gene_column) %in% common_genes) %>%
              arrange(!!rlang::sym(gene_column)) %>%
              select(- !!rlang::sym(gene_column))
            
            
            trim_ref <- ref_data() %>%
              filter(!!rlang::sym(ref_gene_column) %in% common_genes) %>%
              arrange(!!rlang::sym(ref_gene_column)) %>%
              select(- !!rlang::sym(ref_gene_column))
            
            
            # Calculate correlation between the the cluster (single column in trimmed input data) and each of the
            # reference cell subsets (columns of the trimmed reference data)
            cor_df <- cor(trim_dat[logFC_column], trim_ref, method = comp_method)
            
            # Store results in a data frame
            df <- data.frame(identity_score = cor_df[1,])
            
            df <- rownames_to_column(df, var="reference_id")
            
            # Combine results with reference annotations
            if(input$sel_reference != "Custom"){  #ImmGen (previously)
              
              df <- left_join(df, reference_annotation(), by=c("reference_id" = "short_name"))
              
              
            } else if (input$sel_reference == "Custom" & !is.null(input$annot_file)){
              
              df <- left_join(df, reference_annotation(), by=c("reference_id" = "short_name"))
              
              
            } else if(input$sel_reference == "Custom" & is.null(input$annot_file)){
              
              # Fill in with reminder if annotation file is not updated
              df$reference_cell_type <- rep("Upload annotation file", dim(ref_data())[2]-1)
              df$short_name <- colnames(ref_data())[!colnames(ref_data()) %in% ref_gene_column]
              df$long_name <- rep("Upload annotation file", dim(ref_data())[2]-1)
              df$description <- rep("Upload annotation file", dim(ref_data())[2]-1)
              
            }
            
            
            
            # Store cluster information in a column
            df$cluster <- i
            
            # Add confidence-of-prediction calculations here and append to the df
            # Calculate the mean and standard deviation of the aggregate scores per reference cell type
            mean_cor_coeff <- mean(df$identity_score)
            cor_coeff_sd <- sd(df$identity_score)
            
            # Calculate the distance of the identity score from population mean (how many std devs apart?)
            df$z_score <- (df$identity_score - mean_cor_coeff)/cor_coeff_sd
            
            
            # Add all the results to the master data frame
            master_df <- rbind(master_df,df)
            
            
          } # close for loop that iterates over clusters
          
        }) # Close with progress
        
        # Return master data frame to reactive object
        master_df  
      }
      
    } else { 
      
      values$genes_in_analysis <- numeric()
      values$current_cluster <- character()
      
      dat_genes <- user_data()[gene_column] %>% pull() %>% as.character
      ref_genes <- ref_data()[ref_gene_column] %>% pull() %>% as.character
      
      common_genes <- intersect(dat_genes, ref_genes)
      
      values$genes_in_analysis <- c(values$genes_in_analysis, length(common_genes))
      
      
      
      trim_dat <- user_data() %>%
        filter(!!rlang::sym(gene_column) %in% common_genes) %>%
        arrange(!!rlang::sym(gene_column)) %>%
        select_(.dots = paste0("-", gene_column))
      
      trim_ref <- ref_data() %>%
        filter(!!rlang::sym(ref_gene_column) %in% common_genes) %>%
        arrange(!!rlang::sym(ref_gene_column)) %>%
        select_(.dots = paste0("-", ref_gene_column))
      
      
      
      master_df <- data.frame()
      
      withProgress(message = 'Analysis in progress', value = 0, {
        
        if(input$comp_method == "Spearman (all genes)") comp_method = "spearman" else if(input$comp_method == "Pearson (all genes)") comp_method = "pearson"
        
        for (i in clusters()) {
          
          values$current_cluster <- c(values$current_cluster, i)
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/length(clusters()), detail = paste("Analyzing cluster", i))
          
          
          cor_df <- cor(trim_dat[i], trim_ref, method = comp_method)
          
          
          df <- data.frame(identity_score = cor_df[1,])
          
          df <- rownames_to_column(df, var="reference_id")
          
          
          
          if(input$sel_reference != "Custom"){   #ImmGen (previously)
            
            df <- left_join(df, reference_annotation(), by=c("reference_id" = "short_name"))
            
            
            
            
            
          } else if (input$sel_reference == "Custom" & !is.null(input$annot_file)){
            
            df <- left_join(df, reference_annotation(), by=c("reference_id" = "short_name"))
            
            
          } else if(input$sel_reference == "Custom" & is.null(input$annot_file)){
            
            df$reference_cell_type <- rep("Upload annotation file", dim(ref_data())[2]-1)
            df$short_name <- colnames(ref_data())[!colnames(ref_data()) %in% ref_gene_column]
            df$long_name <- rep("Upload annotation file", dim(ref_data())[2]-1)
            df$description <- rep("Upload annotation file", dim(ref_data())[2]-1)
            
          }
          
          
          
          
          df$cluster <- i
          
          # Add confidence-of-prediction calculations here and append to the df
          # Calculate the mean and standard deviation of the aggregate scores per reference cell type
          mean_cor_coeff <- mean(df$identity_score)
          cor_coeff_sd <- sd(df$identity_score)
          
          # Calculate the distance of the identity score from population mean (how many std devs apart?)
          df$z_score <- (df$identity_score - mean_cor_coeff)/cor_coeff_sd
          
          
          master_df <- rbind(master_df,df)
          
          
        } # close for loop that iterates over clusters
        
      }) # Close with progress
      
      
      master_df  
      
    }
    
  }) # close analyzed_df reactive expression
  
  
  
  ################################################################################################################################
  # Generate plotting area dynamically for individual cluster plots
  
  
  # Insert the right number of plot output objects into the web page (https://gist.github.com/wch/5436415/)
  output$plots <- renderUI({
    
    if(input$sel_reference == "ImmGen (mouse)") plot_width <- 2000
    if(input$sel_reference == "Presorted RNAseq (mouse)") plot_width <- 2300
    if(input$sel_reference == "Blueprint-Encode (human)") plot_width <- 2000
    if(input$sel_reference == "Primary Cell Atlas (human)") plot_width <- 4500
    if(input$sel_reference == "DICE (human)") plot_width <- 600
    if(input$sel_reference == "Hematopoietic diff (human)") plot_width <- 1500
    if(input$sel_reference == "Presorted RNAseq (human)") plot_width <- 1250
    if(input$sel_reference == "custom") plot_width <- 2000
    
    
    if(input$sel_reference == "ImmGen (mouse)") plot_height <- 500
    if(input$sel_reference == "Presorted RNAseq (mouse)") plot_height <- 500
    if(input$sel_reference == "Blueprint-Encode (human)") plot_height <- 550
    if(input$sel_reference == "Primary Cell Atlas (human)") plot_height <- 500
    if(input$sel_reference == "DICE (human)") plot_height <- 500
    if(input$sel_reference == "Hematopoietic diff (human)") plot_height <- 500
    if(input$sel_reference == "Presorted RNAseq (human)") plot_height <- 500
    if(input$sel_reference == "custom") plot_height <- 500
  
    
    plot_output_list <- lapply(clusters(), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = plot_height, width = plot_width, brush = "brush") # optimize plotting area
    }
    ) # close lapply 
    
    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, plot_output_list)
    
  }) # close output$plots renderUI
  
  
  
  ################################################################################################################################
  # Prepare individual plots
  
  
  
  # # Call renderPlot for each one. Plots are only actually generated when they
  # # are visible on the web page.
  
  observe({
    
    # if(input$comp_method == "logFC dot product"){
    
    
    
    withProgress(message = 'Graphing', value = 0, {
      
      for (i in clusters()) {
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/length(clusters()), detail = paste("Cluster", i))
        
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          
          # Extract results calculated for individual clusters
          df_plot <- analyzed_df() %>%
            filter(cluster == i)
          
          # Calculate mean and sd deviation for adding confidence bands to graphs
          score_mean <- mean(df_plot$identity_score)
          score_sd <- sd(df_plot$identity_score)
          
          
          # Create plotting area and prepare plots
          my_i <- i
          plotname <- paste("plot", my_i, sep="")
          
          output[[plotname]] <- renderPlot({
            
            df_plot_brushed <<- df_plot
            
            # Plot identity scores per cluster per reference cell type and add confidence bands
            p <- ggdotplot(df_plot, x = "reference_id", y="identity_score", 
                           fill = "reference_cell_type", xlab=F, ylab="Reference identity score",
                           font.y = c(14, "bold", "black"), size=1, x.text.angle=90,
                           title = paste("Cluster:",my_i), font.title = c(15, "bold.italic"),
                           font.legend = c(15, "plain", "black"))+
              theme(axis.text.x = element_text(size=7.5, vjust=0.5, hjust=1))+
              geom_hline(yintercept=score_mean)+
              annotate("rect", xmin = 1, xmax = length(df_plot$reference_id),
                       ymin = score_mean-score_sd, ymax = score_mean+score_sd,
                       fill = "gray50", alpha = .1)+
              annotate("rect", xmin = 1, xmax = length(df_plot$reference_id),
                       ymin = score_mean-2*score_sd, ymax = score_mean+2*score_sd,
                       fill = "gray50", alpha = .1)
            
            
            
            print(p)
            
          }) # close renderPlot
          
        }) # close local
        
      } # close for loop 
      
    })  # close withProgress
    
    
  }) #close observe
  
  
  ################################################################################################################################
  # Prepare top5 summary plots
  # This plot will show the 5 highest scoring reference cell types for each cluster.
  
  top_df <- reactive({
    
    
    # Extract top5 hits from the reuslts
    top5_df <- analyzed_df() %>%
      mutate(cluster = factor(cluster, levels = mixedsort(levels(as.factor(cluster))))) %>%
      arrange(cluster, desc(identity_score)) %>%
      group_by(cluster) %>%
      top_n(5, wt = identity_score)
    
    
    # Index variable helps keeping the results for clusters separate and helps ordered outputs
    top5_df$index <- 1:nrow(top5_df)
    
    # Extract relevant columns 
    top5_df <- select(top5_df, cluster,
                      reference_cell_type,
                      reference_id,
                      long_name,
                      description,
                      identity_score,
                      index, everything())
    
    
    # Assign it to global environment for the brushing to work outside of this reactive context
    top5_df_brush <<- top5_df
    
    top5_df
    
    
    
    
    
  })
  
  # Create plots for top5 results
  output$top5 <- renderPlot({
    
    # This sets up the HTML tag for error message to show properly
    # This was needed here somehow although before validation message was showing properly
    validate(need(user_data(), ""))
    
    
    # if(input$comp_method == "logFC dot product"){
    
    
    ggdotplot(top_df(), x="index", y="identity_score", 
              fill = "cluster", size=1, x.text.angle=90, 
              font.legend = c(15, "plain", "black")) +
      scale_x_discrete(labels=top_df()$reference_id)+
      theme(axis.text.x = element_text(vjust=0.5, hjust=1))
    
    
    
    
    
  })
  
  
  ################################################################################################################################
  # Download results 
  
  # Download all identity scores calculated for each cluster-reference pair
  output$download_res <- downloadHandler(
    filename = "Identity_scores_all.csv",
    content = function(file) {
      write.csv(analyzed_df(), file, row.names = FALSE)
    }
  )
  
  # Download identity scores of the top 5 scoring reference cell subsets for each cluster
  output$download_top5 <- downloadHandler(
    filename = "Identity_scores_top5.csv",
    content = function(file) {
      
      write.csv(top_df(), file, row.names = FALSE)
    }
  )
  
  
  
  
  observeEvent(input$run, {
    
    output$console <- renderText({
      
    })
    
  })
  
  observeEvent(input$run, {
    
    
    a <- paste0("Number of samples in the original reference: ", values$ref_cols)
    
    b <- paste0("Number of samples in reference after subsetting: ", values$subset_num)
    
    c <- paste0("Number of genes in reference without variance filtering: ", values$ref_rows)
    
    d <- paste0("Number of features in reference with variance filtering: ", values$keep_genes);
    
    e <- paste0("Cluster in analysis: ", values$current_cluster);
    
    f <- paste0("Number of genes shared between reference and input: ", values$genes_in_analysis);
    
    
    output$console <- renderText({
      paste(a, b, c, d, paste("\n", e, f, sep ="\n", collapse=""), sep="\n", collapse = "")
      
    })
    
    
    
  })
  
  
  
  
} # close server function


# Testing area (comment out)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# reference_log <- readRDS("data/immgen_recalc_ratio.rds")
# reference <- readRDS("data/immgen.rds")
# ref_annot <- readRDS("data/immgen_annot.rds")
# var_vec_log <- apply(reference_log, 1, var, na.rm=T)
# var_vec <- apply(reference, 1, var, na.rm=T)





