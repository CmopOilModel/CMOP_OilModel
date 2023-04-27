#!/bin/ksh

## purpose: 
##   to check and compare the performance of methods to check if a 
##   particle is inside a polygon

#                                                   Valdir Innocentini
#                                                reviewed    2023_0417
#/ ---------------------------------------------------------------------


    \rm -f *.o
    \rm -f *.mod
    \rm -f *.exe
    
    gfortran -c module_in_or_out.f90
    gfortran -c work_with_inside.f90  
    
    gfortran -o run_with_inside.exe module_in_or_out.o work_with_inside.o


    \rm -f *.o
    \rm -f *.mod
    \rm -f fort.50
    
    ./run_with_inside.exe

    mv -f fort.50 log_time

