
  
   PROGRAM FAZ_ARQUIVOS_INICIAIS

!/ ------------------------------------------------------------------- /  
!/   arquivos gerados : 
!/   ----------------
!    
!    ENV_CURRENT.DAT            -> vel_x, vel_y 
!    GRD_CELL_NODES.DAT     -> r1, r2, r3 
!    GRD_CELL_CENTER.DAT    -> x, y       
!    GRD_NODES_LOCATION.DAT     -> x, y, depth
!    GRD_GRID_AROUND.DAT        -> r1, r2, r3, r4, r5, r6, r7, r8, r9
!    
!    LOCATION.DAT_RELEASE -> X, Y, ID_CELL, STAT_PAR  
!/ ------------------------------------------------------------------- /  
!/ 

   IMPLICIT NONE 
   
!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list
      INTEGER, PARAMETER :: NX = 4000,     NY = 100 
      REAL,    PARAMETER :: DX = 1. ,  DY = 1.
      INTEGER, PARAMETER :: NBIT = 4
      REAL,    PARAMETER :: UNDEF = 0.999E+9
      
      REAL ,   DIMENSION (NX,NY)    :: GRID_X
      REAL ,   DIMENSION (NX,NY)    :: GRID_Y
      REAL ,   DIMENSION (NX,NY)    :: DEPTH 
      REAL ,   DIMENSION (NX,NY)    :: MASK
      
      REAL ,   DIMENSION (NX) :: VEL_X, VEL_Y
      REAL    :: R1, R2, R3
      REAL    :: X_POS, Y_POS, STAT_PAR  
      REAL, DIMENSION ( -1:1 , -1:1 ) :: ID_NODE 
      REAL, DIMENSION (4) :: XP, YP
      
      REAL ,   DIMENSION(4) :: A_COE, B_COE, C_COE
      INTEGER :: I, J, IJ, IREC  
      
!/ ------------------------------------------------------------------- /

    ! -------------------        
    !  GRD_NODES_LOCATION.DAT 
    ! -------------------        
    
      DEPTH( : , :  ) = 1. 
      DEPTH( : , 1  ) = 10000.
      DEPTH( : , NY ) = 10000.
      MASK(  : , : ) = 1. 
      
      GRID_X(1,1) = 0. 
      DO I = 2, NX
        GRID_X(I,1) = GRID_X(I-1,1)+  DX
      ENDDO  
      DO J = 2, NY 
      GRID_X(:,J)  = GRID_X(:,1)
      ENDDO 
      
      GRID_Y(1,1) = 0. 
      DO J = 2, NY
        GRID_Y(1,J) = GRID_Y(1,J-1)+  DY
      ENDDO   
      
      DO I = 2, NX   
      GRID_Y(I,:)  = GRID_Y(1,:) 
      ENDDO
      
      OPEN( 100 , FILE = "GRD_NODES_LOCATION.DAT",                     &
                  STATUS = 'UNKNOWN' ,                                 &
                  FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,          &
                  RECL   = ( 4 )* NBIT  )
        IREC = 0
        DO J = 1, NY 
        DO I = 1, NX
        
           IREC = IREC + 1
           WRITE ( 100 , REC = IREC ) GRID_X(I,J), GRID_Y(I,J),        &
                                      DEPTH(I,J) , MASK(I,J) 
           
        ENDDO
        ENDDO
        
      CLOSE ( 100 ) 
      

    ! ------------------- 
    !  ENV_CURRENT.DAT 
    ! -------------------  
     
      VEL_Y(:) = 0. 
      VEL_X(1) = 4.

      DO I = 2, NX
        VEL_X(I) = VEL_X(I-1)-0.0011
        IF ( VEL_X(I) .LT. 0. ) VEL_X(I) = 0.
      ENDDO  
             
      OPEN( 100 , FILE = "ENV_CURRENT.DAT" , STATUS = 'UNKNOWN' ,      &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 2 )* NBIT  )
        IREC = 0
        DO J = 1, NY 
        DO I = 1, NX
           IREC = IREC + 1
           WRITE ( 100 , REC = IREC ) VEL_X(I), VEL_Y(J)
           if(j .eq. 1 ) then 
           write(*,*) irec, VEL_X(I), VEL_Y(J)
        ! if( VEL_X(I) <= 3.0011 .and. VEL_X(I) >= 3.0000 ) read(*,*)
        ! if( VEL_X(I) <= 2.0011 .and. VEL_X(I) >= 2.0000 ) read(*,*)
        ! if( VEL_X(I) <= 1.0011 .and. VEL_X(I) >= 1.0000 ) read(*,*)
        ! if( VEL_X(I) <= 0.0011 .and. VEL_X(I) >= 0.0001 ) read(*,*) 
           endif 
             
        ENDDO
        ENDDO

      CLOSE ( 100 )   
  

    ! -------------------   
    !  GRD_CELL_NODES.DAT 
    ! -------------------     
    
      OPEN( 100 , FILE = "GRD_CELL_NODES.DAT" ,                    &
                  STATUS = 'UNKNOWN' ,                                 &
                  FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,          &
                  RECL   = ( 3 )* NBIT  )    
      R1 = 0.
      R2 = 0.
      R3 = 0.
      
      IREC = 0  
      DO J = 1, NY-1
      
         DO I = 1, NX-1
            R1 = FLOAT(I+1)  +  FLOAT( (J-1)*NX ) 
            R2 = R1 +  FLOAT(NX)
            R3 = R2 - 1      
            IREC = IREC + 1
            WRITE ( 100 , REC = IREC ) R1, R2, R3
         ENDDO 
          
         IREC = IREC + 1  
         R1   = 0.
         R2   = 0.
         R3   = 0.
         WRITE ( 100 , REC = IREC ) R1, R2, R3
      
      ENDDO 
       
      J = NY
      DO I = 1, NX-1    
         R1 = FLOAT(I+1)  +  FLOAT( (J-1)*NX ) 
         R2 = 0.
         R3 = 0.
         IREC = IREC + 1  
         WRITE ( 100 , REC = IREC ) R1, R2, R3 
      ENDDO  
       
      J = NY
      I = NX   
      R1 = 0.
      R2 = 0.
      R3 = 0.
      IREC = IREC + 1  
      WRITE ( 100 , REC = IREC ) R1, R2, R3 
    
      CLOSE ( 100 )  
      
      
    ! ------------------   
    ! GRD_CELL_CENTER.DAT    -> x, y        
    ! ------------------   
    
      OPEN( 100 , FILE = "GRD_VEC_AROUND.DAT" ,                        &
                  STATUS = 'UNKNOWN' ,                                 &
                  FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,          &
                  RECL   = ( 12 )* NBIT  )      
 
      OPEN( 110 , FILE = "GRD_CELL_NODES.DAT" ,                        &
                  STATUS = 'UNKNOWN' ,                                 &
                  FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,          &
                  RECL   = ( 3 )* NBIT  )    
                  
      OPEN( 120 , FILE = "GRD_NODES_LOCATION.DAT",                     &
                  STATUS = 'UNKNOWN' ,                                 &
                  FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,          &
                  RECL   = ( 4 )* NBIT  )

 

      IREC = 0  
      DO J = 1, NY 
      DO I = 1, NX
    
           IREC = IREC + 1
           READ ( 110 , REC = IREC ) R1, R2, R3

           A_COE(:) = UNDEF
           B_COE(:) = UNDEF
           C_COE(:) = UNDEF
           
           IF(  R1* R2* R3  <= 0.9 ) GOTO 200    

           READ ( 120 , REC = IREC     ) XP(1), YP(1)
           READ ( 120 , REC = NINT(R1) ) XP(2), YP(2)       
           READ ( 120 , REC = NINT(R2) ) XP(3), YP(3)           
           READ ( 120 , REC = NINT(R3) ) XP(4), YP(4)


           CALL FIND_A_B_C ( XP(1), YP(1),XP(2), YP(2),     &
                       A_COE(1), B_COE(1), C_COE(1) ) 
                       
           CALL FIND_A_B_C ( XP(2), YP(2),XP(3), YP(3),     &
                       A_COE(2), B_COE(2), C_COE(2) )    
                       
           CALL FIND_A_B_C ( XP(3), YP(3),XP(4), YP(4),     &
                       A_COE(3), B_COE(3), C_COE(3) ) 
                                  
           CALL FIND_A_B_C ( XP(4), YP(4),XP(1), YP(1),     &
                       A_COE(4), B_COE(4), C_COE(4) )    

       200 WRITE ( 100 , REC = IREC )(A_COE(IJ),B_COE(IJ),C_COE(IJ),IJ=1,4)

      ENDDO 
      ENDDO 
      
      CLOSE( 100 )
      CLOSE( 110 )
      CLOSE( 120 )
               
    ! ------------------   
    ! GRD_GRID_AROUND.DAT  .DAT
    ! ------------------        
      OPEN( 100 , FILE = "GRD_GRID_AROUND.DAT " ,                          &
                  STATUS = 'UNKNOWN' ,                                 &
                  FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,          &
                  RECL   = ( 9 )* NBIT  )    
      IREC = 0
      DO J = 1, NY
      DO I = 1, NX
      
        ID_NODE(-1, 0) = FLOAT(I-1)  +  FLOAT( (J-1)*NX )  
        ID_NODE( 0, 0) = FLOAT(I)    +  FLOAT( (J-1)*NX ) 
        ID_NODE( 1, 0) = FLOAT(I+1)  +  FLOAT( (J-1)*NX ) 
        
        ID_NODE(-1, -1) = FLOAT(I-1)  +  FLOAT( (J-2)*NX )  
        ID_NODE( 0, -1) = FLOAT(I)    +  FLOAT( (J-2)*NX ) 
        ID_NODE( 1, -1) = FLOAT(I+1)  +  FLOAT( (J-2)*NX )       
           
        ID_NODE(-1, 1) = FLOAT(I-1)  +  FLOAT( (J)*NX )  
        ID_NODE( 0, 1) = FLOAT(I)    +  FLOAT( (J)*NX ) 
        ID_NODE( 1, 1) = FLOAT(I+1)  +  FLOAT( (J)*NX )         
        
        IF ( I .EQ. 1) ID_NODE(-1, :) = 0
        IF ( I .EQ. NX) ID_NODE(1, :) = 0
         
        IF ( J .EQ. 1) ID_NODE(:, -1) = 0
        IF ( J .EQ. NY) ID_NODE(:, 1) = 0
         
        IREC = IREC + 1 
        WRITE ( 100 , REC = IREC ) ID_NODE(:,:)

        
      ENDDO
      ENDDO 
                    
      CLOSE ( 100 )  
              
    ! -------------------   
    ! LOCATION.DAT_RELEASE
    ! -------------------      
    
          
    
      OPEN( 100 , FILE = "LOCATION.DAT_RELEASE" , STATUS = 'UNKNOWN' , &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 4 )* NBIT  )   
                                    
      OPEN( 200 , FILE = "GRD_NODES_LOCATION.DAT" , STATUS = 'UNKNOWN' ,     &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 4 )* NBIT  )
      IREC = 0   
      STAT_PAR  = -1   ! --- at the surface
      
      ! STAT_PAR     =  1 : in the water
      ! STAT_PAR     =  0 : outside the domain
      ! STAT_PAR     = -1 : surface
      ! STAT_PAR     = -2 : deposited at the bottom 
      ! STAT_PAR     = -3 : beached 
      ! STAT_PAR     = -4 : vc transformed into im   
          
       IREC = 0 
       DO I = NX+2 , NX*(NY-2), NX 
      
         READ( 200 , REC = I ) X_POS, Y_POS  
    
         X_POS = X_POS + 2.*DX/10.
         Y_POS = Y_POS + DY/2.
         
         IREC = IREC + 1        
         WRITE(*, * ) "I  X_POS  Y_POS " , I, X_POS, Y_POS     
         WRITE(100, REC = IREC ) X_POS, Y_POS, FLOAT(I), STAT_PAR      
       
     ENDDO                   
      
      CLOSE ( 200 )       
      CLOSE ( 100 )       
     
     
!   31 32 33
!   21 22 23
!   11 12 13 


            
   END PROGRAM FAZ_ARQUIVOS_INICIAIS
!/ ------------------------------------------------------------------- /  


! 
! 
!/ =================================================================== /
   SUBROUTINE FIND_A_B_C ( X_1, Y_1, X_2, Y_2,  &
                       A, B, C ) 
   IMPLICIT NONE
!     
!  
!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: X_1 ,Y_1 , X_2,Y_2 
      REAL,    INTENT (OUT) :: A, B, C
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter 


!/ ------------------------------------------------------------------- /  

      A = Y_1 - Y_2 
      B = X_2 - X_1 
      C = X_1*Y_2 -  Y_1*X_2

   END SUBROUTINE  FIND_A_B_C
!/ =================================================================== /






