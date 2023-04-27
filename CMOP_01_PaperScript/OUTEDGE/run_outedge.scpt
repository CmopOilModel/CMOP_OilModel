#!/bin/ksh



# ---------
# --- informations given by the user

cat <<EOF> outedge_input            
      1.5 = TIME_STOP      ! --- time to stop in HOURS 
      64. = DT             ! --- time step in SECONDS
      0.01 = TIME_PRINT_INC ! --- time between 2 output in HOURS      ! --- hours
EOF


# ---------
# --- clean

      \rm -f *.o
      \rm -f *.mod
      \rm -f *.exe
      \rm -f GRD_*
      \rm -f ENV_*
      \rm -f *_RELEASE
      \rm -f TEV_*
       
# ----------------------------------------- 
# --- link input files  (grid and forcings)

      if [[ ! -e d-input ]]
      then
         mkdir d-input   
         dir_grid="../GRIDS"
         cd d-input
    
         ln -s ../${dir_grid}/GRD_CELL_NODES.DAT  
         ln -s ../${dir_grid}/GRD_NODES_LOCATION.DAT 
         ln -s ../${dir_grid}/GRD_GRID_AROUND.DAT   
      
         ln -s ../${dir_grid}/ENV_CURRENT.DAT       
         ln -s ../${dir_grid}/LOCATION.DAT_RELEASE   
         cd ..
      fi
        
# -----------------------------
# --- generate a clear d-output
 
      [ -e d-output ] && \rm -r d-output
      mkdir d-output
      
      
# --------------------------------
# --- compile f90 codes from d-f90     
    
      dir_f90="d-f90"
      
      gfortran -c ${dir_f90}/module_work_with_outedge.f90
      gfortran -c ${dir_f90}/run_outedge.f90

      gfortran -o run_outedge.exe   \
                  module_work_with_outedge.o \
                  run_outedge.o

      \rm -f *.o
      \rm -f *.mod
      \rm -f fort.1000  
      \rm -f fort.1700 
      
      
# -------
# --- run

    # ------------
      \rm -f GRD_*
      \rm -f ENV_*
      \rm -f *.DAT_RELEASE
      
    # ------------
    
      ln -s d-input/GRD_NODES_LOCATION.DAT
      ln -s d-input/GRD_GRID_AROUND.DAT
      ln -s d-input/GRD_CELL_NODES.DAT    
   
      ln -s d-input/ENV_CURRENT.DAT
      ln -s d-input/LOCATION.DAT_RELEASE  
      
    # ------------
    
      ./run_outedge.exe
      
    # ------------   
      
      \rm -f GRD_*
      \rm -f ENV_*
      \rm -f *.DAT_RELEASE     
      \rm -f  run_outedge.exe 
      
    # ------------       
      
      mv TEV_LOCATION.DAT   d-output/.
      mv outedge_input      d-output/.
      mv fort.1000          d-output/reporter_txt
      mv fort.1700          d-output/history_parcel_01_txt
       
    # ------------       
        
      
#/ ---------------------------------------------------------------------

