# CMOP_01_PaperScript
   
## What is this
   
   This repository contains performance tests for tools frequently applied 
   in particle tracking algorithms. 
  
   The tools are
   
   - **in-cell test**   : three options to check if a position is in a 
     cell are analysed,
   - **next-cell test** : to find the next cell in a sequence until reaching 
    the target cell, where the particle is contained.
     
   The next-cell test are adapted and applied in algorithms with partitioned 
   time-step, namely 
   
   - **OutEdge** : the particle can move beyond the current cell,
   - **IntEdge** : the particle is intercepted in the edge of the current 
              cell
               
   A detailed description of these algorithms is provided in the manuscript 

   "An effcient particle tracking algorithm by partitioning the time 
   interval" 
   by Luciana de Freitas Tessarolo, Valdir Innocentini, 
   Ernesto Caetano, Julio Tomás Aquije Chacaltana, Audálio Rebelo Torres 
   Júnior, Tânia Ocimoto Oda, Júlio César Martins Ribeiro Júnior, 
   Fernando Túlio Camilo

               
## Content of this repository 
   
   This repository is organized in the following 6 folders:
   
   **DOCUMENTS**
   
     1_Introduction_txt : how to install,
       
     2_In-cell_txt : tests of performance in triangular, quadrilateral, 
                     and pentagone  cells,
         
     3_Grids_txt : basic grid files generated to be used by OutEdge and 
                   IntEdge algorithms,
         
     4_OutEdge_txt : simulations with the OutEdge algorithm,
       
     5_IntEdge_txt : simulations with the IntEdge algorithm,
       
     6_NoSplit_txt : simulations with the NoSplit algorithm,
        
   **IN-CELL_TESTS** 
   
   **GRIDS**
    
   **OUTEDGE**
   
   **INTEDGE**
   
   **NOSPLIT**
     
   
