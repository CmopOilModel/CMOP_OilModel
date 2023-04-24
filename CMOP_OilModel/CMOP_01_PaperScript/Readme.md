What is this
 ------------
   
   This repository contains performance tests for two kind of tools 
   frequently employed in particle tracking algorithms: 
   
   This repository contains benchmarks for two types of tools
   frequently employed in particle tracking algorithms:
   
   > in-cell test : to check if a position is in a cell,
     
   > next-cell test : to find the next cell in a sequence until reaching 
     the target cell, where the particle is contained.
     
   Both tests are adapted and applied in algorithms with partitioned 
   time-step, namely 
   
   > OutEdge : the particle can move behoynd the current cell,
   
   > IntEdge : the particle is intercepted in the edge of the current 
               cell
               
   A detailed description of these algorithms is provided in the manus
   cript "An effcient particle tracking algorithm by partitioning the time 
   interval" by Luciana de Freitas Tessarolo, Valdir Innocentini, 
   Ernesto Caetano, Julio Tomás Aquije Chacaltana, Audálio Rebelo Torres 
   Júnior, Tânia Ocimoto Oda, Júlio César Martins Ribeiro Júnior, 
   Fernando Túlio Camilo
            
 Content of this repository 
 --------------------------
   
   DOCUMENTS
   
     1_Introduction_txt : 
       - how to install, 
       - the basic steps of a script, 
       - how to navigate in this repository. 
       
     2_In-cell_txt
       - tests of performance in triangular, quadrilateral, pentagone 
         cells,
         
     3_Grids_txt
       - basic grid files generated to be used by OutEdge and IntEdge 
         algorithms,
         
     4_OutEdge_txt
       - simulations with the OutEdge algorithm,
       
     5_IntEdge_txt
       - simulations with the IntEdge algorithm,
       
     6_NoSplit_txt  
       - simulations with the NoSplit algorithm,
       
   IN-CELL_TESTS 
   
   GRIDS
    
   OUTEDGE
   
   INTEDGE
   
   NOSPLIT
     
