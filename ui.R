library(shinythemes)

ui <- fluidPage(theme=shinytheme("spacelab"),
  
  shinyjs::useShinyjs(), # needed for download button to work
  
  tags$head(includeHTML(("data/google_analytics.html"))), # To track user experience
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: steelblue;
      }
    "))
  ),
  
  titlePanel(title=div(img(src="cipr_logo_small.png"), "Cluster Identity Predictor"), windowTitle = "CIPR"),
  
  sidebarLayout(
    
    sidebarPanel(width = 2,
                 
                 # Cluster markers (Diff exp analysis, columns must have "gene", "avg_logFC", and "cluster" data)
                 fileInput("data_file", "Upload cluster diff. exp. data",
                           multiple = F,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 
                 # horizontal line
                 tags$hr(), 
                 
                 checkboxInput(inputId = "example_data", 
                               label = "Use example data (Ekiz HA and Huffaker TB, JCI Insight, 2019)", 
                               value = F),
                 
                 # horizontal line
                 tags$hr(), 
                 
                 radioButtons("sel_reference", 
                              label = "Select reference data", 
                              choices = c("ImmGen (mouse)",
                                          "Presorted RNAseq (mouse)",
                                          "Blueprint-Encode (human)",
                                          "Primary Cell Atlas (human)",
                                          "DICE (human)",
                                          "Hematopoietic diff (human)",
                                          "Presorted RNAseq (human)",
                                          "Custom"), 
                              selected = "ImmGen (mouse)"), 
                 
                 # horizontal line
                 tags$hr(), 
                 
                 # Setup cell reference cell subset selector
                 conditionalPanel("input.sel_reference != 'Custom'",
                                  uiOutput("ui_sel_subsets")),
                 
                 # horizontal line
                 tags$hr(),
                 
                 
                 conditionalPanel("input.sel_reference == 'Custom'",
                                  uiOutput("ui_sel_ref")),
                 conditionalPanel("input.sel_reference == 'Custom'",
                                  uiOutput("ui_sel_ref_annot")),
                 
                 # horizontal line
                 tags$hr(), 
                 
                 sliderInput("var_filter", "Keep top Nth % of variable genes in reference",
                             min = 0, max = 100,
                             value = 100),
                 
                 # horizontal line
                 tags$hr(), 
                 
                 radioButtons("comp_method", 
                              label = "Select method for comparisons", 
                              choices = c("logFC dot product", "logFC Spearman", "logFC Pearson",
                                          "Spearman (all genes)", "Pearson (all genes)"), 
                              selected = "logFC dot product"), 
                 
                 # horizontal line
                 tags$hr(), 
                 
                 
                 actionButton("run", label="Analyze"),
                 
                 br(),br(), br(),
                 
                 downloadButton("download_res", "Download results"),
                 
                 br(),br(),
                 
                 downloadButton("download_top5", "Download top5 hits")
                 
                 # uiOutput("downloadData_ui") #THIS ALSO WORKS TO SHOW BUTTON AFTER ANALYSIS
                 
                 
                 
                 
                 
    ),
    
    
      
    mainPanel(width=10,
      
      
      
      tabsetPanel(
        
        tabPanel("Top-5 hits",
                 
                 fluidRow(
                   plotOutput("top5", brush = "brushtop5", height="600px"),
                   div(tableOutput("brushtop5"), style = "font-size:85%"),
                   br(),
                   p(strong("\n\nDefinition of table columns")),
                   tags$ul(
                     tags$li(strong("Cluster:"), "The name of the unknown cluster"),
                     tags$li(strong("Reference_cell_type:"), "Broad classification of the reference cell type"),
                     tags$li(strong("Reference_id:"), "Shortened unique identifier for reference cell type"),
                     tags$li(strong("Long_name:"), "Human-readable long name of the reference cell type"),
                     tags$li(strong("Description:"), "Detailed description of the reference cell type"),
                     tags$li(strong("Identity_score:"), "Identity score for the given reference cell type calculated via logFC dot product or correlation methods (see How-to-tab for details"),
                     tags$li(strong("Index:"), "Position in the dataframe (needed for ordered plotting)"),
                     tags$li(strong("Z-score:"), "How many standard deviations apart is the identity score from the average score for that cluster calculated across the reference cell subsets (see How-to tab for details)"),
                     tags$li(strong("Percent_pos_correlation:"), "What percent of the differentially expressed genes in unknown clusters is also differentially expressed in a similar fashion in the reference cell subsets (e.g. both upregulated and downregulated compared to other unknown clusters and the reference cell types")
                     
                   )
                 )
        ),
        
        
        tabPanel("Individual Clusters",
                 
                 # This is the dynamic UI for the plots to be generated for each cluster
                 
                 fluidRow(
                   h4("Please wait while the images are being generated. This can take some time depending on the cluster numbers.\n"),
                   p("In the plots below, horizontal lines indicate the mean identity score for each experimental cell cluster, and the shaded bands indicate +/- 1 and 2 standard deviations from the mean.\n\n"),
                   uiOutput("plots")
                   
                 )       
        ),
        
        tabPanel("Analysis details",
                 
                 verbatimTextOutput(outputId="console", placeholder = T)
                 ),
        
        tabPanel("How to use this program",
                 
                 h3("Summary"),
                 p("Understanding the biological identity of cell clusters in single cell RNA sequencing (scRNAseq) experiments can be challenging due to overlapping gene expression profiles. An accurate assessment of the cluster identity requires analyzing multiple genes simultaneously as opposed to examining a few selected lineage-specific markers. Cluster Identity PRedictor (CIPR) compares user-provided scRNAseq cluster gene signatures with known reference datasets --or with a custom reference--, and calculates an", strong('identity score (IS)'), "for each scRNAseq cluster per reference cell subset. For further information about how to use CIPR, please read the sections below."),
                 
                 br(),br(),
                 
                 h3("Quick Start"),
                 
                 p(strong(span(style="color:green", "To compare your scRNAseq clusters against ImmGen cell types, simply upload the cluster gene expression signatures and click 'Analyze'"))),
                 br(),
                 p(strong(span(style="color:steelblue", "If you would like to compare scRNAseq data with a custom reference, please select the appropriate radio button and upload reference gene expression data"))),
                 br(),
                 p(span(style="color:black", "You can also run the program by using example data that our lab has generated", a('(Ekiz HA and Huffaker TB, JCI Insight, 2019).', href='https://insight.jci.org/articles/view/126543'), "This data was obtained by single cell transcriptomics analysis (10X Genomics) of CD45+ flow-sorted murine tumor-infiltrating immune cells. Example gene expression data on this website is shortened to reduce computing times. In this example dataset, gene signatures of 4 clusters (activated T cells, natural killer cells, Langerhans dendritic cells, and plasmacytoid dendritic cells) are included, whereas the original dataset had 15 distinct cell clusters based on our analysis.")),
                 
                 br(),br(),
                 
                 
                 h3("Input data"),
                 p("CIPR accepts gene expression data derived from scRNAseq experiments in comma separated value (.csv) format. Depending on the calculation method to be used, data need to be formatted as described below. Popular scRNAseq analysis software including Loupe Cell Browser (10X Genomics) and Seurat (Satija Lab, NY) can be used to export suitable data frames for logFC comparison methods"), 
                 strong("For correlation methods using the entire gene set"),
                 tags$ul(
                   tags$li("The input data frame should contain the average gene expression values per cluster for each gene in the experiment."),
                   tags$li("This data frame must have a column named 'gene', and other columns should have the average expression values in individual clusters."),
                   tags$li("If scRNAseq data is being analyzed with Seurat package,", strong("AverageExpression"), "function can be used to generate a CIPR-ready data frame. Please see below the example code snippet to obtain a suitable input data using this method.")
                   
                 ),
                 
                 br(),
                 strong("For logFC comparison methods using differentially expressed genes"),
                 tags$ul(
                   tags$li("This file must have at least three columns named Gene, logFC, and cluster. Other columns can be present in the input file but they will be ignored in the analysis pipeline."),
                   tags$li("This is a long data format where differentially expressed genes in clusters are listed in the rows"),
                   tags$li("For this method, Seurat's", strong("FindAllMarkers"), "function can be used to generate a CIPR-ready dataframe. Extra columns generated by this function will be ignored in CIPR pipeline. Example code snippet to obtain this data frame is provided below."),
                   tags$li("Alternatively, the differential gene expression data can be exported using Loupe Cell Browser (10X Genomics) and manually edited to obtain the correct format for CIPR")
                   
                 ),
                 
                 
                 br(),
                 
                 p(strong("Sample input data. (A) For correlation methods (B) For logFC comparison methods")),
                 imageOutput("sample_data_file_logfc"),
                 
                 
                 h3("Reference data"),
                 p("Users can select one of the pre-loaded reference datasets or provide a custom reference data frame."), 
                 p(strong("The reference datasets available in CIPR include:")),
                 tags$ul(
                   tags$li(a('Immunological Genome Project (ImmGen)' , href= 'https://www.immgen.org'), "microarray data from sorted mouse immune cells. This dataset is prepared by using both V1 and V2 ImmGen releases and it contains 296 samples from 20 different cell types (253 subtypes)."),
                   tags$li("Mouse RNAseq data from sorted cells reported in", a('Benayoun et al. (2019).' , href= 'http://www.genome.org/cgi/doi/10.1101/gr.240093.118'), "This dataset contains 358 sorted immune and nonimmune samples from 18 different lineages (28 subtypes)."),
                   tags$li(a('Blueprint' , href= 'https://doi.org/10.3324/haematol.2013.094243'),"/", a('Encode', href="https://doi.org/10.1038/nature11247"),"RNAseq data that contains 259 sorted human immune and nonimmune samples from 24 different lineages (43 subtypes)."),
                   tags$li(a('Human Primary Cell Atlas', href='https://doi.org/10.1186/1471-2164-14-632'), "that contains microarray data from 713 sorted immune and nonimmune cells (37 main cell types and 157 subtypes)."),
                   tags$li(a('DICE (Database for Immune Cell Expression(/eQTLs/Epigenomics)', href='https://doi.org/10.1016/j.cell.2018.10.022'), "that contains 1561 human immune samples from 5 main cell types (15 subtypes). To reduce object sizes, mean TPM values per cell type is used."),
                   tags$li("Human microarray data from sorted hematopoietic cells reported in", a('Novershtern et al. (2011).', href='https://doi.org/10.1016/j.cell.2011.01.004'), "This dataset contains data from 211 samples and 17 main cell types (38 subtypes)" ),
                   tags$li("Human RNAseq data from sorted cells reported in", a('Monaco et al. (2019).', href='https://doi.org/10.1016/j.celrep.2019.01.041'), "This dataset contains 114 samples originating from 11 main cell types (29 subtypes)."),
                 ),
                 p("We would like to acknowledge", a('SingleR', href='https://doi.org/10.1038/s41590-018-0276-y'), "R package authored by Dvir Aran, Agnieszka P. Looney, Leqian Liu (Bhattacharya Lab, UCSF), which was tremendously helpful for preparing reference datasets. The code for preparing the CIPR-ready reference datasets can be found in the", a('CIPR GitHub Page.', href='https://github.com/atakanekiz/CIPR-Shiny')),
                 br(),
                 p(strong("If one would like to use a custom reference dataset:")),
                 tags$ul(
                   tags$li("Any number of highthroughput data types can be used including microarray, RNAseq, and proteomics data, as long as data are normalized together."),
                   tags$li("Reference data should be normalized and log-transformed"),
                   tags$li("Custom reference datasets can be uploaded as a csv file and should contain gene expression data from known cell types."),
                   tags$li("Reference dataset should have genes in rows and cell types in columns."),
                   tags$li("The first column of the reference data frame must have gene names (e.g. Pdcd1, Tnfa) and must be named as", strong("Gene.")),
                   tags$li("This file can contain biological and technical replicates. In this case, the identity score is calculated and plotted for each replicate separately.")
                 ),
                 p("The user can use the entire reference dataset for comparisons as is, or apply two types of filtering:"),
                 tags$ul(
                   tags$li(strong("Subsetting the reference samples:"), "User can select which samples (i.e. columns of the reference data) are to be included in the analysis. Subsetting the reference dataframe should not impact all-genes-correlation methods, but it will change the logFC comparison methods since the relative changes will be calculated within the data subset."),
                   tags$li(strong("Filtering the genes with low variance:"), "User can limit the analysis to only highly variant genes within the reference dataset. The variance cutoff to exclude the genes can be determined by the using the slider in the input panel."),
                 ),
                 p(strong(span(style="color:steelblue", "Subsetting the reference dataset and filtering genes with low variance can reduce noise, but may also decrease the number of genes that contribute to the identity score calculations. The best parameters need to be empirically determined by the user."))),
        br(),
        p(strong("Sample reference gene expression data")),
        imageOutput("sample_reference_file"),
        
        
        h3("Custom reference annotation data (optional)"),
        tags$ul(
          tags$li("If a custom reference dataset is used, although not necessary, an annotation file (in csv format) can be uploaded to obtain more informative plots."),
          tags$li("Annotation file must contain the columns named as", strong("'short_name', 'long_name', 'description', and 'reference_cell_type'")),
          tags$li("Data under 'short_name' column must", strong("EXACTLY"), "match the column names of the reference gene expression data."),
          tags$li("Annotation file can have other columns as well, which will be ignored.")
        ),
        
        p(strong("Sample reference annotation data")),
        imageOutput("sample_annotation_file"),
        
        
        h3("How is cell identity score calculated?"),
        p("CIPR can either analyze the correlation between unknown clusters and reference cell subsets by using the entire gene set, or can compare the patterns of differential gene expression in unknown clusters and individual reference subsets. When the whole gene set correlation method is used, the used can select either Spearman's or Pearson's correlation. When the logFC comparison method is used, the user can select one of the three calculation approaches: 1) Log fold change (logFC) dot product, 2) Spearman's correlation, 3) Pearson's correlation"),
        
        br(),
        
        
        h4(strong("If the logFC comparison method is used:")),
        
        p("The algorithm works first by calculating gene expression signatures in the reference file, and then comparing the unknown cluster signatures with these reference signatures. Specifically the following processes are performed:"),
        h5(strong("Pre-processing of reference dataset")),
        tags$ul(
          tags$li("The algorithm uses pre-loaded ImmGen signatures or a user-uploaded reference expression file (see below for the details of reference file format)."),
          tags$li("The reference expression file should contain normalized gene expression values in log scale in rows and cell types in columns. Reference data can be derived from various high-throughput experiments such as microarray or RNA sequencing"),
          tags$li("For each gene found in the reference file, algorithm subtracts the average gene expression value of the whole data set (i.e. all columns) from the gene expression value in each cell type (i.e. individual columns) to calculate logFC"),
          tags$li("Therefore, positive logFC values indicate upregulation and negative values indicate downregulation of a given gene. Therefore, pre-processing of the reference file results in a data frame that features logFC values of each gene in each cell type.")
        ),
        h5(strong("Analysis of experimental cluster signatures against reference file")),
        tags$ul(
          tags$li("Uploaded cluster signature file should contain information about genes, logFC in expression levels and the cluster information (see below for the correct file format)."),
          tags$li("Algorithm finds the common genes between experimental data and the reference dataset."),
          tags$li("User can select one of three methods for comparing logFC values: Spearman's correlation, Pearson's correlation, and logFC dot product"),
          tags$li("Correlation methods calculate correlation coefficients between logFC values for each unknown cluster-reference cell subset pairs and reports these as identity scores."),
          tags$li("If logFC dot product method is selected, for each shared gene, logFC values of differentially expressed genes from unknown clusters are multiplied with the logFC values in the reference subsets. This way if a gene is upregulated or downregulated in both the unknown cluster and the reference cell type (i.e. positive correlation), the multiplication will result in a positive number (i.e. multiplication of two positive or two negative numbers). Alternatively, if a gene is differentially regulated in opposite directions in the unknown cluster and reference cell type (i.e. negative correlation), multiplication of logFC values will result in a negative number."),
          tags$li("Multiplied logFC values per each gene are added up, resulting in an aggregate identity score for each cluster in the experimental data. This way, genes that have similar expression patterns in the experimental cell cluster and the reference cell contribute to a higher identity score, whereas genes with opposite expression patterns will result in a lower identity score"),
          tags$li(strong("This way, each cluster in the scRNAseq experiment is analyzed against each known cell type in the reference file and scored for its overall similarity using a dot product approach. A higher identity score indicates the unknown cluster has a similar gene expression profile to a given reference dataset, and suggest shared biological origins.")),
          tags$li("For each cell cluster in the experiment, aggregate identity score of reference cell types are plotted in dot plots which shows reference cell types on the x-axis, and aggregate identity score on the y-axis."),
          tags$li("Reference cell types with the 5 highest identity scores are also plotted for easy visualization.")
        ),
        
        br(),
        
        h4(strong("If correlation method is used:")),
        
        tags$ul(
          tags$li("When using correlation approaches, CIPR algorithm calculates pair-wise correlation coefficients between unknown clusters and each of the reference subsets using either Spearman's or Pearson's methods."),
          tags$li("The correlation coefficients from pairwise comparisons are reported as identity score per reference cell type for each cluster."),
          tags$li("For each cell cluster in the experiment, correlation coefficients of reference cell types are plotted in dot plots which shows reference cell types on the x-axis, and correlation coefficients on the y-axis."),
          tags$li("Reference cell types with the 5 highest identity scores for each unknown cluster is summarized in an interactive dot plot"),
          tags$li("Although, the effects of the outlier genes will be mitigated by the other genes, cell types with only a few strongly distinguishing genes may be misclassified in this method.")
        ),
        
        br(), br(),
        
        p("Although different calculation methods implemented in CIPR generated comparable results while analyzing heterogeneous immune cell populations from tumors (Ekiz HA and Huffaker TB, JCI Insight, 2019), some methods may perform better than others depending on the experimental context. Importantly, since trancript-level correlations may not be sufficient to fully define the cellular pheonotypes, further bench work may be needed to validate the findings from scRNAseq experiments."),
        
        
        
        
        
        h3("About the confidence of the predictions"),
        p("Since our algorithm compares unknown cluster gene signatures with signatures of reference samples, the confidence in the identity prediction can only be as high as the biological overlap between the experimental sample and the reference datasets. In an ideal scenario where gene expression data are available from all possible cell types under various experimental conditions, we envision that this algorithm can describe the identity of the unknown cell clusters in a highly accurate manner. However, in the absence of such data, CIPR is still useful to characterize unknown cluster identities in single cell transcriptomics experiments by using a multiparametric approach. The predictions of this algorithm need to be experimentally tested to ascertain the biological origins of unknown cell clusters. To help assess the confidence of the predictions, we utilize two different metrics:"),
        tags$ul(
          tags$li(strong("Deviation from mean (Z-score):"), "For each unknown cluster, this metric is calculated by", strong(em("i) ")), "Averaging the identity scores from the whole reference dataset", strong(em("ii) ")), "Subtracting the identity scores of individual reference from this average", strong(em("iii) ")), "Dividing the difference by the standard deviation of the identity score across the whole reference dataset. This way, the deviation from mean is reported in the units of ", em("standard deviation."), "The higher deviation indicates a more pronounced distinction from the rest of the reference cell types, hence a higher confidence in prediction."),
          tags$li(strong("Frequency of positively correlated genes:"), "(For logFC dot product method only) Percentage of the genes that show positive correlation between the unknown cluster and reference cell subsets is reported. This value shows what portion of the differentially expressed genes in an unknown cluster is also differentially expressed in the reference cell in a similar fashion (both upregulated or downregulated). The higher percent positive correlation value indicates a higher similarity between the unknown sample and the reference cell")
          
        ), # close tags$ul
        
        p("Although these metrics can be helpful in determining the confidence of prediction, they are not suitable benchmarks for the performance of our algorithm. A 'low-confidence' prediction can be explained by various factors including"),
        tags$em(
          tags$ul(
            tags$li("A lack of comparable cell types in the reference dataset"),
            tags$li("Using suboptimal number of metagene dimensions during clustering step of data analysis (which can result in impure clusters consisting of multiple cell types"),
            tags$li("Presence of outliers in experimental data and clustering artifacts.")
          )),
        
        p("Alternatively, a 'low-confidence' prediction can suggest an interesting new cell type, and identify contaminating cell populations in the experiment. For instance, our experience with tumor-infiltrating lymphocyte single cell RNA-sequencing in murine B16F10 melanoma model suggests that, contaminating melanoma cells are loosely identified as ImmGen blood stem cells due to their proliferative capacity and stem-like gene expression profiles (Ekiz HA, manuscript in preparation)."),
        
        h3("Frequently asked questions"),
        tags$ul(
          tags$li(strong("Why am I getting 'Served Disconnected' error?"), "This can happen if the software is left idle for some time or when it encounters an error during data processing. Common problems include formatting errors of the input and/or the custom reference data. Please see guidelines above to make sure data are formatted correctly. If the problem persists, please contact us."), 
          br(),
          tags$li(strong("Is CIPR limited to analyzing mouse data only?"), "The available reference datasets in CIPR are derived from both mouse and human samples. However, CIPR allows the users to analyze their data using reference datasets from other species. To achieve this, the analytical pipeline converts gene names in the input and reference to lower case letters allowing the matching of orthologous genes across species. Most of the orthologous gene names differ only in capitalization between human and mice (e.g. Pdcd1 vs PDCD1), and lower case conversion is sufficient to compare the majority of genes between the input and the reference data. Therefore, the user can benefit from the CIPR pipelines regardless of the species in the study. However, the user should use caution to derive conclusions while comparing datasets across different species. In the cases where available reference datasets are not suitable for analysis, the user can prepare a custom reference dataset to have a more accurate cellular identity approximation. Any number of high throughput data sets readily accessible from", a('Gene Expression Omnibus', href='https://www.ncbi.nlm.nih.gov/geo/'), "and", a('EBI Expression Atlas', href='https://www.ebi.ac.uk/gxa/home'), "can be used to prepare custom reference datasets."),
          br(),
          tags$li(strong("How are reference datasets prepared for CIPR?"), "Please see the", a('CIPR GitHub page', href='https://github.com/atakanekiz/CIPR'), "for the code to generate reference datasets.")
        ),
        
        h3("Example R code to generate CIPR input"),
        h4("For correlation methods using whole gene set"),
        tags$pre(
          "
# Load Seurat package (v3.0.0 as of this writing)
library(Seurat)

# Generate a Seurat object by following Seurat vignettes 
# (https://satijalab.org/seurat/)



# Calculate average expression per cluster
# For cluster level analysis, combined object should have cluster 
# information in the Identity slot.    
avg_exp <- AverageExpression(seurat_obj)

# Pull gene names from rownames and assign into a separate column
avg_exp$gene <- rownames(avg_exp)

# Data frame contains per cluster average gene expression values
head(avg_exp)

#            clus_1    clus_2    clus_3    clus_4     clus_5    gene
# Sox17   0.0000000 0.0000000 0.0000000 0.0000000 0.00000000   Sox17
# Mrpl15  0.9055106 1.0769703 0.7100053 0.4363261 0.64374313  Mrpl15
# Lypla1  0.2394298 0.2251968 0.3076141 0.2521298 0.05390125  Lypla1
# Tcea1   2.4971414 2.5244691 1.4478863 0.8615930 1.13819434   Tcea1
# Atp6v1h 0.4922616 0.4297001 0.4404676 0.2995814 0.49688672 Atp6v1h
# Rb1cc1  0.5492173 0.4046591 0.5008476 0.5658034 0.59000428  Rb1cc1



# Save csv file ready to be used with CIPR
write.csv(avg_exp, 'per_cluster_avg_expression.csv', row.names = F)

"),
        br(),
        
        h4("For logFC comparison methods"),
        
        tags$pre("library(Seurat)

# Generate a Seurat object by following Seurat vignettes 
# (https://satijalab.org/seurat/)
cluster_markers <- FindAllMarkers(seurat_obj)


head(cluster_markers)
# >  p_val avg_logFC pct.1 pct.2 p_val_adj   cluster  gene
# >1     0  1.794907 0.815 0.151         0   clus_1  Cd8b1
# >2     0  1.760971 0.994 0.253         0   clus_1   Cd3g
# >3     0  1.732805 0.832 0.116         0   clus_1  Cxcr6
# >4     0  1.686574 0.807 0.133         0   clus_1   Cd8a
# >5     0  1.487584 0.994 0.265         0   clus_1   Cd3d
# >6     0  1.429320 0.763 0.119         0   clus_1   Lag3



#  Save the cluster_markers object as .csv file
write.csv(cluster_markers, 'clustermarkers.csv', row.names=F)
"),
        
        
        # h3("CIPR Release Notes"),
        # tags$ul(
        #   tags$li("Release date: 12/29/2019"),
        #   tags$li("5 different computational methods are implemented allowing the comparisons of i) all genes in the experiment, ii) differentially expressed genes in clusters."),
        #   tags$li("ImmGen v1 and v2 data are renormalized from raw data files (.CEL), combined and pre-loaded as the reference. These two datasets contain partially overlapping cell types, and were generated using the same microarray platform (Affymetrix MoGene ST1.0 array). For further information please see ImmGen website and associated publications.")
        # ),
        
        
        h3("Contact us"),
        p("For questions and comments:"),
        p("atakan.ekiz-at-path-utah-edu"),
        p("Ryan O'Connell Lab"),
        p("Department of Pathology"),
        p("University of Utah"),
        p("Salt Lake City, Utah")
        
        
      )
      
      
      
    )
  ) 
)
)


