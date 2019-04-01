<h1> Tutorial </h1>
  This short tutorial uses a test data set provided in the directory 'qPCanalyzer_testdata'. 
  
  <h2> Prerequisites </h2>
  Make sure you have the following R packages installed:<br>
  * dplyr <br>
  * tidyr <br>
  * ggplot2 <br>
  * stringr <br>
  * plotrix
  
  <h2> Input Files </h2>
  Regardless of the chosen subdirectory you are looking at, you will realize that we always
  have to provide three basic input files in <b>comma-delimited format</b>: <br>
  
  <h4> PlateViewResults.csv </h4> 
  Output file created by BioRad machine. Only use the plate view file, not the other formats.
  
  <h4> Genes.csv </h4>
  User-created file of 16x24 cells providing information about genes-of-interest (e.g. 'ACTIN') in the corresponding well.<br>
  Wells that do not correspond to any gene, should be labelled with <b>NA</b> inside the 16x24 frame. 
  
  <h4> Samples.csv </h4> 
  Same as Genes.csv, but with cDNA (e.g. 'WT') instead of genes-of-interest in corresponding well. Based on users choice, input has to be further specified. Take a look at corresponding files for further help: <br> <br>
  * Biological replicates (BR): Specify with mandatory parameter: WT@Rep1, WT@Rep2, ..., WT@RepN <br>
  * Time course (TC): Specifiy with parameter: WT@1, WT@Time1, WT@10min etc. <br>
  * BR + TC: Specify with both: WT@10min@Rep1, WT@Time1@Rep2 etc. <br>
  * neither: Specify by Sample only: WT, mutant1, ... <br> <br>
  <b>Important</b>: Sample labels occurring more than once are considered technical replicates.
  
  <h2> Parameter Adjustment </h2>
  <h4> mandatory parameters </h4>
  In the input area of the program you have to specify name of experiment, working directory, number of 384-well plates used in this experiment ('total_plate_count'), housekeeping gene (required for further normalization), samples you would like to exclude from you analyses (e.g. negative control, multiple selection possible), comparator (e.g. "WT"), 'first_time_point' (timepoint that should be used for comparison). 'output_format' is required to save your plots in correspoding format. 
  
  <h4> optional parameters </h4>
  By specifying 'ignore_genes' you can exclude (multiple) genes from analysis. Increase size of single scatters and lines by changing 'point_size' or 'line_size', respectively.
  
  <h2> Run analysis </h2>
  If you've finished parameter adjustment, mark the complete code and run it. <br>
  You will be prompted to choose your input files now. If you pre-defined 'total_plate_count' as '1', first prompt will ask for your PlateViewResults.csv, second prompt for Samples.csv and third prompt for Genes.csv. If you pre-defined 2 plates, you will be asked twice for PlateViewResults_1.csv and PlateViewResults_2.csv, then for Samples_1.csv, Samples_2.csv and so on.<br> <br>
  Afterwards, statistical analysis and plotting might take some seconds. You will find results in a subdirectory of your pre-defined working directory named after 'experiment_name'. 
  
  <h2> qPCanalyzeR output </h2>
  <h4> plateView_rawCt </h4>
  
