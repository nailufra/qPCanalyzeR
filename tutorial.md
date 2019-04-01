<h1> Tutorial </h1>
  This short tutorial uses a test data set provided in the directory 'qPCanalyzer_testdata'. 
  
  <h2> Input Files </h2>
  Regardless of the chosen subdirectory you are looking at, you will realize that we always
  have to provide three basic input files in <b>comma-delimited format</b>: <br>
  
  <h4> PlateViewResults.csv </h4> 
  Output file created by BioRad machine. Only use the plate view file, not the other formats.
  
  <h4> Genes.csv </h4>
  User-created file of 16x24 cells providing information about genes-of-interest (e.g. 'ACTIN') in the corresponding well.<br>
  Wells that do not correspond to any gene, should be labelled with <b>'NA'</b> inside the 16x24 frame. 
  
  <h4> Samples.csv </h4> 
  Same as Genes.csv, but with cDNA (e.g. 'WT') instead of genes-of-interest in corresponding well.<br>
  Based on users choice, input has to be further specified: <br>
  * Multiple Biological Replicated: WT@Rep1, WT@Rep2, ... (see e.g. 'qPCanalyzer_testdata/one_plate/biolrep/tc_Samples.csv')
  
  
