<h1> Tutorial </h1>
  This short tutorial uses a test data set provided in the directory 'qPCanalyzer_testdata'. 
  
  <h2> Input Files </h2>
  Regardless of the chosen subdirectory you are looking at, you will realize that we always
  have to provide three basic input files in <b>comma-delimited format</b>: <br>
  
  <h4> PlateViewResults </h4> 
  Output file created by BioRad machine. Only use the plate view file, not the other formats.
  
  <h4> Genes </h4>
  User-created file of 16x24 cells providing information about genes-of-interest (e.g. 'ACTIN') in the corresponding well.<br>
  Wells that do not correspond to any gene, should be labelled with <b>'NA'</b> inside the 16x24 frame. 
  
  <h4> Samples </h4> 
  User-created file of 16x24 cells providing information about used cDNA (e.g. 'WT') in the corresponding well. <br>
  Wells that do not correspond to any gene, should be labelled with <b>'NA'</b> inside the 16x24 frame.<br>
  
