!/ ------------------------------------------------------------------- /
   PROGRAM RUN_INTEDGE
!   
! 
!/ ------------------------------------------------------------------- /  
   USE MODULE_WORK_WITH_SU,    ONLY: WORK_WITH_SU,                     &
                                     UPGRADE_LOCATION_SU
                                     
   IMPLICIT NONE 
   
!/  
!/ ------------------------------------------------------------------- /  
!/                                                  Local parameters
      INTEGER , PARAMETER :: NBIT = 4
      INTEGER :: NSTOP, NSTEP

      REAL :: DT, TIME, TIME_STOP, TIME_PRINT_INC
      INTEGER :: IIFF ,N_PAR     
      real :: start, finish
!     -----------------------------------------------------------------

      call cpu_time(start)

!     -----------------------------------------------------------------       
    
      OPEN ( 100 , FILE = "intedge_input" , STATUS = 'UNKNOWN'   ,    &
                          FORM = 'FORMATTED'  ) 
               
  
      READ(100,*) TIME_STOP          ! --- HOURS 
      READ(100,*) DT                 ! --- seconds 
      READ(100,*) TIME_PRINT_INC     ! --- hours
      
      CLOSE ( 100 )    
      
!     ----------------------------------------------------------------- 
               
      NSTEP          = 0

 
      TIME_STOP      = TIME_STOP * 3600.
      TIME_PRINT_INC = TIME_PRINT_INC * 3600.

      
    ! --- an initial empty file 
    
      OPEN ( 140 , FILE = "TEV_LOCATION.DAT" , STATUS = 'UNKNOWN'   ,  &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 4 )* NBIT  ) 
               
      CLOSE ( 140 )          
               
  
    ! -------------------------
    ! 
    ! -------------------------    
      
      CALL SYSTEM ( " cat  LOCATION.DAT_RELEASE >> TEV_LOCATION.DAT " )    

      CALL NUMBER_OF_PARCELS (NBIT, N_PAR) 
   
    ! -------------------------
    ! 
    ! -------------------------   

      write(1700,'(6x,"time      xpos      ypos    idcell status" )') 
      
!     -----------------------------------------------------------------
  100 CONTINUE  

      TIME = NSTEP *DT            ! time in sec of this simu

      CALL NUMBER_OF_PARCELS (NBIT, N_PAR)   

    ! -------------------------
    ! 
    ! -------------------------time to print?
            
      IIFF = MOD ( NINT(TIME*100.) , NINT (TIME_PRINT_INC*100.) ) 

      IF ( IIFF .EQ. 0 ) THEN
      
         WRITE(*,*) " :::::::::::::::::::::::::::::::::::::::::: "
         WRITE(*,*) "   Time to print " 
          
         WRITE (1000,'(4X, "step/time/n par :", I10, F10.1, I10)')   &
                       NSTEP, TIME, N_PAR    
         WRITE ( *  ,'(4X, "step/time/n par :", I10, F10.1, I10)')   &
                       NSTEP, TIME, N_PAR                                

      ENDIF 
              
    ! -------------------------
    ! 
    ! ------------------------- time to stop?

      IF ( ABS(TIME*100)  .GE. TIME_STOP*100   ) GO TO 200     
       
    ! -------------------------
    ! 
    ! ------------------------- displacement

      CALL WORK_WITH_SU( NSTEP , DT , N_PAR ) 

    ! -------------------------
    ! 
    ! ------------------------- upgrade positions 

      CALL UPGRADE_LOCATION_SU ( TIME, DT,  NBIT, N_PAR) 

    ! -------------------------
    ! 
    ! ------------------------- 
    
    
      NSTEP = NSTEP + 1 
      

      IF(NSTEP .LT. 1) CALL SYSTEM ( " cat  LOCATION.DAT_RELEASE >> TEV_LOCATION.DAT " )  
 
      GO TO 100 
    ! -------------------------

  200 CONTINUE
 

      
    ! -------------------------
    ! 
    ! ------------------------- 
    
      WRITE(*,*) " ======================================= "
      WRITE(*,*) " --->     FINESHED SIMULATION       <--- "
      WRITE(*,*) " ======================================= "


      call cpu_time(finish)
      print '("Time = ",f16.5," seconds.")',finish - start 


   END PROGRAM RUN_INTEDGE
!/ =================================================================== /
! 
!   
! 
!/ =================================================================== /
   SUBROUTINE NUMBER_OF_PARCELS (NBIT, N_PAR)
!/ 
   IMPLICIT NONE 
   
   INTEGER, INTENT(IN)  :: NBIT
   INTEGER, INTENT(OUT) :: N_PAR
   
   INTEGER :: IIFF, N_JUMP, N_C
   REAL    :: XPOS
 
!/ 
!/ ------------------------------------------------------------------- / 
!/ 
           
      OPEN( 80 , FILE = "TEV_LOCATION.DAT" , STATUS = 'UNKNOWN' ,      &
                        FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,    &
                        RECL   = ( 4 )* NBIT  ) 
                        
      N_PAR = 0  
      
    ! -----------------------------------------                          
      N_JUMP = 10000           ! --- each 10000
      DO N_C = N_PAR + N_JUMP , 100000000*5, N_JUMP
         READ( 80 , REC = N_C , IOSTAT = IIFF ) XPOS
         IF ( IIFF .NE. 0 ) GO TO 500
      ENDDO 
  500 N_PAR =  N_C - N_JUMP 

    ! ----------------------------------------  
      N_JUMP = 1000            ! --- each 1000
      DO N_C = N_PAR + N_JUMP , 100000000*5, N_JUMP
         READ( 80 , REC = N_C , IOSTAT = IIFF ) XPOS
         IF ( IIFF .NE. 0 ) GO TO 501
      ENDDO 
  501 N_PAR =  N_C - N_JUMP 

    ! ---------------------------------------
      N_JUMP = 100             ! --- each 100
      DO N_C = N_PAR + N_JUMP , 100000000*5, N_JUMP
         READ( 80 , REC = N_C , IOSTAT = IIFF ) XPOS
         IF ( IIFF .NE. 0 ) GO TO 502
      ENDDO 
  502 N_PAR = N_C - N_JUMP 

    ! ---------------------------------------
      N_JUMP = 1               ! ---   each 1
      DO N_C = N_PAR + N_JUMP , 100000000*5, N_JUMP
         READ( 80 , REC = N_C , IOSTAT = IIFF ) XPOS
         IF ( IIFF .NE. 0 ) GO TO 503
      ENDDO 
  503 N_PAR = N_C - N_JUMP 

      CLOSE ( 80 ) 
     
   END SUBROUTINE NUMBER_OF_PARCELS
!/ =================================================================== /
!


!/ =================================================================== /
! 
!   
! 









! 
!/ ------------------------------------------------------------------- /

