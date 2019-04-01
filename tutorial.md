<h1> Tutorial </h1>
  This short tutorial uses a test data set provided in the directory 'qPCanalyzer_testdata'. 
  
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
  <h4><mandatory parameters</h4>
  In the input area of the program you have to specify name of experiment, working directory, number of 384-well plates used in this experiment ('total_plate_count'), housekeeping gene (required for further normalization), samples you would like to exclude from you analyses (e.g. negative control, multiple selection possible), comparator (e.g. "WT"), first_time_point (e.g. timepoint that should be used for comparison).
  
  <h4><optional parameters</h4>
  By specifying 'ignore_genes' you can exclude (multiple) genes from analysis. Increase size of single scatters and lines by changing 'point_size' or 'line_size', respectively.
  
