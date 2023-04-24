!/ ------------------------------------------------------------------- /

   MODULE MODULE_WORK_WITH_SU

! 
!/   
      CONTAINS

!/ =================================================================== /
! 
! 
   SUBROUTINE WORK_WITH_SU( NSTEP , DT , N_PAR ) 
   
   IMPLICIT NONE 
   
!/ ------------------------------------------------------------------- /  
!  purpose
!  -------   
!    to compute the displacement of N_PAR parcels during a time step DT
! 
!    the method is the intersection algorithm 
! 
!  feature
!  -------
!  > the only file altered is 
!    TEV_LOCATION.DAT   ( X_POS, Y_POS, RI_GRID, STAT_PAR )
!  
!  > files used
!    ----------
!    GRD_NODES_LOCATION.DAT
!    GRD_CELL_NODES.DAT
!    GRD_VEC_AROUND.DAT
!    GRD_GRID_AROUND.DAT
!      
!    ENV_CURRENT.DAT
!   
!    TEV_LOCATION.DAT
!    AUX_LOCATION_NEW
! 
!  > subroutines used
!    ----------------
!    FOUR_WEIGHTS
!    CHECK_EMPTY 
!    FIND_FOUR_PTOS 
!    CELL_INTERSECTION
!    VEL_INTERPOLATED 
!    INSIDE_AREA 
!    FIND_A_B_C 
!    POINT_INTERSECTION 
! 
! 
!/  
!/ ------------------------------------------------------------------- /  
!/                                                  List of parameters

      INTEGER, INTENT(IN) ::  NSTEP 
      REAL,    INTENT(IN) ::  DT 
      INTEGER, INTENT(IN) ::  N_PAR

!/
!/ ------------------------------------------------------------------- /
!/                                                    Local parameters
      INTEGER :: IP 
      REAL    :: D_X, D_Y 
      REAL    :: DX_AD, DY_AD        
      REAL    :: X_POS, Y_POS, STAT_PAR 
      REAL    :: X_POS_NEW, Y_POS_NEW
      
      INTEGER :: ID_CELL, ID_NODE_NEAR
      INTEGER :: ID_CELL_NEW
      INTEGER :: ID_NODE_CENTER_9,ID_NODE_INSIDE
      
      REAL    :: RID_CELL, RID_CELL_NEW

      INTEGER, DIMENSION (1) ::  ID_AUX
      REAL,    DIMENSION (4) ::  X ,Y, WEI 
      INTEGER, DIMENSION (4) ::  ID_NODE
      REAL,    DIMENSION (4) :: DEP_NODE,MASK_NODE 
      
      REAL    :: TIME_TOT  ! --- total time considered
      REAL    :: TIME_ACU  ! --- time acumulated until TIME_TOT
      REAL    :: DEL_T     ! --- time step  
           
      INTEGER :: NBIT = 4 
      REAL    :: U_PAR, V_PAR

      LOGICAL :: INSIDE
      LOGICAL :: EMPTY_CELL         
      REAL    :: DIST, VEL
      REAL    :: X_INT, Y_INT
          
integer :: n_quebra  
real    :: finish, start                                                                   
!/ ------------------------------------------------------------------- / 
 
      IF ( N_PAR .EQ. 0 ) GO TO 1700
      
    ! ----------------------------- open section
    
      OPEN ( 100 , FILE = "GRD_NODES_LOCATION.DAT" ,                   &
                          STATUS = 'UNKNOWN'   ,                       &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 4 )* NBIT  )
                          ! ###  GRID_X, GRID_Y, DEPTH, MASK
  
      OPEN ( 110 , FILE = "GRD_CELL_NODES.DAT" , STATUS = 'UNKNOWN' ,  &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 3 )* NBIT  )    
                          ! ###  R1, R2, R3
       
      OPEN ( 120 , FILE = "GRD_GRID_AROUND.DAT" , STATUS = 'UNKNOWN' , &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 9 )* NBIT  )     
                          ! ###  R1
           

      OPEN ( 130 , FILE = "ENV_CURRENT.DAT" , STATUS = 'UNKNOWN'    ,  &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 2 )* NBIT  )
                          ! ###  U_PAR, V_PAR

      OPEN ( 140 , FILE = "TEV_LOCATION.DAT" , STATUS = 'UNKNOWN'   ,  &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 4 )* NBIT  ) 
                          ! ###   X_POS, Y_POS, GRID, STAT_PAR    
          
      OPEN ( 150 , FILE = "GRD_VEC_AROUND.DAT" ,                       &
                          STATUS = 'UNKNOWN' ,                         &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 12 )* NBIT  )   
                          ! ##  (A_COE(IJ),B_COE(IJ),C_COE(IJ),IJ=1,4)    
                               
      OPEN ( 81 ,  FILE = "AUX_LOCATION_NEW" , STATUS = 'UNKNOWN'   ,  &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 3 )* NBIT  )                    
                           
    ! --------------------- >>> loop on parcels IP = 1, N_PAR 
      DO 300 IP = 1 , N_PAR 
    ! ---------------------
call cpu_time(start)
          D_X = 0. ; D_Y = 0.  ! --- inicial increment displacement

!write(*,*) " comecando novo time step "
!write(*,*) " ip " , ip
          READ( 140 , REC = IP ) X_POS, Y_POS, RID_CELL, STAT_PAR
!write(*,*) "X_POS Y_POS RID_CELL STAT_PAR " , X_POS, Y_POS, RID_CELL, STAT_PAR   
!read(*,*)
        ! STAT_PAR     =  1 : in the water
        ! STAT_PAR     =  0 : outside the domain
        ! STAT_PAR     = -1 : surface
        ! STAT_PAR     = -2 : deposited at the bottom 
        ! STAT_PAR     = -3 : beached 
        ! STAT_PAR     = -4 : vc transformed into im     
 

          IF( NINT(STAT_PAR) /= -1 ) THEN 
              !--- no displecement
              WRITE ( 81 , REC = IP )  0., 0. , RID_CELL
              GOTO 300
          ENDIF
                 
                         ! --- the process is recursive in time
          TIME_TOT = DT  ! --- total time considered
          TIME_ACU = 0.  ! --- time acumulated until TIME_TOT
          DEL_T    = DT  ! --- time step 
          
n_quebra = 0          
     280  ID_CELL  = NINT(RID_CELL)
     
                         ! --- return 4 nodes / positions  
          CALL FIND_FOUR_PTOS ( ID_CELL,                               &
                     ID_NODE(:),X(:),Y(:),DEP_NODE(:),MASK_NODE(:))   
                     
                         ! --- return weights
          CALL FOUR_WEIGHTS( X_POS, Y_POS, X(:), Y(:),MASK_NODE(:),    &
                        WEI(:) )
        
                         ! --- return velocity interpolated
          CALL VEL_INTERPOLATED (ID_NODE(:), WEI(:),                   &
                        U_PAR, V_PAR ) 
                
          DX_AD =  DEL_T  * U_PAR     ! --- advection
          DY_AD =  DEL_T  * V_PAR

          X_POS_NEW =  X_POS + DX_AD  ! --- new position
          Y_POS_NEW =  Y_POS + DY_AD 
          
          
!write(*,*)"  -------------------- "          
!write(*,*)"  ID_NODE(:)       " , ID_NODE(:)
!write(*,*)"  WEI(:)           " , WEI(:)   
!write(*,*)"  U_PAR, V_PAR     " , U_PAR, V_PAR 
!write(*,*)"  X_POS, X_POS_NEW " , X_POS, X_POS_NEW 
!write(*,*)"  DEL_T            " , DEL_T
!write(*,*)"  DT               " , DT  
!write(*,*)"  -------------------- "          
!read(*,*)


        ! -------------------------- first test 
        ! new position is still in the element?
        !             if yes, go to next parcel 
 
                            ! --- 3 and 4 nodes were read in 
                            !     anticlockwise direction      
!          CALL INSIDE_AREA  ( X(1),Y(1) , X(2),Y(2) ,                  &
!                              X(3),Y(3) , X(4),Y(4) ,                  &
!                              X_POS_NEW, Y_POS_NEW  ,                  &
!                        INSIDE )   

                       
         CALL INSIDE_CROSS_quadrilatere ( X(1),Y(1) , X(2),Y(2) ,      &
                                          X(3),Y(3) , X(4),Y(4) ,      &
                                          X_POS_NEW, Y_POS_NEW  ,      &
                                        INSIDE )                
                        
                                                    

!write(*,*)X(4), X(3)
!write(*,*)X(1), X(2), X_POS_NEW
!write(*,*)Y(4), Y(3)
!write(*,*)Y(1), Y(2), Y_POS_NEW
!write(*,*)'INSIDE ' , INSIDE 
!read(*,*)


          IF( INSIDE ) THEN        ! --- yes, it is inside 
             ID_CELL_NEW = ID_NODE(1) 
                    
             GO TO 290  ! --- in this scheme there is no necessity 
                        !     to check the time 
          ENDIF                 

        ! -------------------------- second test 
        !    compute the intersecction point and the new cell  
        

!          CALL CELL_INTERSECTION ( ID_NODE(:),                         &
!                                   X_POS, Y_POS, X_POS_NEW, Y_POS_NEW, &
!                               X_INT, Y_INT, ID_CELL_NEW ) 

          CALL CELL_INTERSECTION_NEW( ID_NODE(:),                      &
                                      X_POS, Y_POS,                    &
                                      X_POS_NEW, Y_POS_NEW,            &
                                 X_INT, Y_INT, ID_CELL_NEW )                               
                                                  

!write(*,*)"cheque !!!" 
!write(*,*)ID_NODE(:)
!write(*,*)X_POS,  X_INT , X_POS_NEW
!write(*,*)ID_CELL_NEW
!read(*,*)


        ! -------------------------- third test 
        !   check if   ID_CELL_NEW is an empty cell
        
        
          IF ( ID_CELL_NEW  .EQ. 0 ) ID_CELL_NEW  = ID_CELL  
          
          CALL CHECK_EMPTY  ( ID_CELL_NEW,                             &
                          EMPTY_CELL )  
               
          IF ( EMPTY_CELL .EQV. .TRUE. ) THEN
          
               X_POS_NEW = X_INT
               Y_POS_NEW = Y_INT  
               GO TO 290     
               
          ENDIF     
          
        !              how much time was spend?    
        !       if yes, determine the grid cell 
        
!write(*,*) " ////////////////////// " 
!write(*,*) " ////////////////////// " 

!write(*,*)" deltat pra deslocar   ", DEL_T  
!write(*,*)"  X_INT  X_POS " ,   X_INT , X_POS  
          DIST     = ( ( X_INT - X_POS )**2 + ( Y_INT - Y_POS )**2 )**0.5
          VEL      = ( U_PAR**2  +  V_PAR**2 )**0.5 
          DEL_T    = DIST / VEL 
!write(*,*)" deltat ate interseccao  ", DEL_T       
          TIME_ACU = TIME_ACU + DEL_T
          DEL_T    = TIME_TOT - TIME_ACU 
!write(*,*)" deltat sobrando  ", DEL_T          
          RID_CELL  = FLOAT(ID_CELL_NEW) 
          X_POS = X_INT
          Y_POS = Y_INT
          
!write(*,*)"DEL_T, TIME_TOT, TIME_ACU ", DEL_T, TIME_TOT, TIME_ACU   
!write(*,*)"X_POS novo " , X_POS
!write(*,*) " ////////////////////// " 
!write(*,*) " ////////////////////// " 
!read(*,*) 
         
        ! -------------------------- third test 
        !           the total time was applied?    
n_quebra = n_quebra + 1   
          IF( ABS(TIME_TOT) .GT. ABS(TIME_ACU) )  GO TO 280

     290  RID_CELL = FLOAT(ID_CELL_NEW)
              
           
          READ( 140 , REC = IP ) X_POS, Y_POS, STAT_PAR , STAT_PAR   
            
          WRITE ( 81 , REC = IP )                                      & 
                   X_POS_NEW - X_POS ,  Y_POS_NEW - Y_POS, RID_CELL 
! write(1500,*)(NSTEP+1)*DT,X_POS_NEW, DEL_T,n_quebra 
!write(1500,*)(NSTEP+1)*DT,X_POS_NEW
!if(IP .eq. 1) write(1500,'(F10.1,F10.1,F10.1, I10, F7.0 )')            & 
!         (NSTEP+1)*DT, X_POS_NEW, Y_POS_NEW, INT(RID_CELL), STAT_PAR
!call cpu_time(finish)
!if(IP .eq. 1) write(1600,*)(NSTEP+1)*DT,X_POS_NEW, DEL_T,n_quebra,finish - start 
!write(*,*) " X_POS_NEW   " ,  X_POS_NEW
!write(1100,*)X_POS_NEW, nint(RID_CELL)
!write(*,*) " RID_CELL    " ,  RID_CELL  
!read(*,*)

  300 CONTINUE
  
      CLOSE ( 100 )  
      CLOSE ( 110 )               
      CLOSE ( 120 )  
      CLOSE ( 130 )  
      CLOSE ( 140 )  
      
      CLOSE ( 81 )  
      
 1700 CONTINUE

   END SUBROUTINE WORK_WITH_SU
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE FOUR_WEIGHTS( XP, YP, X, Y, MASK,   &
                        WEI)

   IMPLICIT NONE
   
!/ ------------------------------------------------------------------- /  
!  purpose
!  -------
!  to compute the weight of influency of four nodes in 
!  the particle position
! 
!  feature
!    > (xp,yp): the particle position
!    > (x,y)  : the 4 nodes position 
!    > mask   : 0 or 1
!    > wei    : weight using the formula ****
!/
!/ ------------------------------------------------------------------- /
!/                                                  List of parameters

      REAL, INTENT ( IN )               :: XP , YP 
      REAL, DIMENSION(4), INTENT ( IN ) :: X, Y, MASK
      
      REAL, DIMENSION(4), INTENT ( OUT ) :: WEI
      
!/
!/ ------------------------------------------------------------------- /
!/                                                    Local parameters
      INTEGER :: NBIT = 4
      REAL, DIMENSION (4)  ::  RMOD_DIST 
      
!/ ------------------------------------------------------------------- /
      RMOD_DIST (:)  = ( X(:) - XP )**2 + (Y(:)-YP)**2
      
      WEI(:) =  MASK(:) / ( RMOD_DIST (:)**2 + 1.E-10 ) 

      WEI(:) = WEI(:)/SUM(WEI(:))

   END SUBROUTINE  FOUR_WEIGHTS
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE CHECK_EMPTY  ( ID_CELL,                                  &
                          EMPTY_CELL )  


   IMPLICIT NONE
!/ ------------------------------------------------------------------- /  
!  purpose
!  -------
!  to check if the cell has at least one undefined node
! 
!  CHECK_EMPTY = .TRUE.    the cell has at least one node undefined
!              = .FALSE.   every node is defined
!  
!  OBSERVATION: a node is undefined if it is = 0
!    
!/ ------------------------------------------------------------------- /    
 
      INTEGER, INTENT(IN)   :: ID_CELL      
      LOGICAL, INTENT(OUT) ::  EMPTY_CELL
 
!/ ------------------------------------------------------------------- /  
  
      REAL ::  R_1, R_2, R_3, R_4
      
!/ ------------------------------------------------------------------- /          
      R_1 = FLOAT(ID_CELL)

      READ( 110 , REC = ID_CELL ) R_2, R_3, R_4 

      EMPTY_CELL = .FALSE.
      IF(  R_1* R_2* R_3 * R_4 <= 0.9 ) EMPTY_CELL = .TRUE.
 
   END SUBROUTINE CHECK_EMPTY 
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE FIND_FOUR_PTOS ( ID_CELL,                                &
                          ID_NODE , X, Y, DEP_NODE, MASK_NODE )  

   IMPLICIT NONE
   
!/ ------------------------------------------------------------------- /  
!  purpose
!  -------
!  given the id number of the grid cell, returns for each node
!  ID_NODE  :  id number 
!  X        :  position of each node 
!  Y 
!  DEP_NODE : depth  
!  MASK_NODE: mask  
! 
!/ ------------------------------------------------------------------- /    
 
      INTEGER, INTENT(IN)   :: ID_CELL      

      REAL,    INTENT(OUT), DIMENSION(4) ::  X , Y
      INTEGER, INTENT(OUT), DIMENSION(4) ::  ID_NODE 
      REAL,    INTENT(OUT), DIMENSION(4) ::  DEP_NODE    
      REAL,    INTENT(OUT), DIMENSION(4) ::  MASK_NODE    
      
!/ ------------------------------------------------------------------- /    
      INTEGER :: I, IIFF    
      REAL :: R_1, R_2, R_3, R_4
!/ ------------------------------------------------------------------- /          
      ID_NODE(1) = ID_CELL
!write(*,*)"ID_CELL->",ID_CELL
      READ( 110 , REC = ID_NODE(1) ) R_2, R_3, R_4 

      ID_NODE(2) = NINT( R_2 ) 
      ID_NODE(3) = NINT( R_3 ) 
      ID_NODE(4) = NINT( R_4 ) 
      
      DO I = 1,4       
         READ( 100 , REC = ID_NODE(I) , IOSTAT = IIFF )                 &
                     X(I), Y(I),  DEP_NODE(I),  MASK_NODE(I)
      ENDDO


   END SUBROUTINE FIND_FOUR_PTOS 
!/ =================================================================== /
! 
! 
!           
!/ =================================================================== /
   SUBROUTINE  CELL_INTERSECTION ( ID_NODE,                            &
                                   XP_INI, YP_INI, XP_END, YP_END,     & 
                              X_INT, Y_INT, ID_CELL_NEW )               
!                            
   IMPLICIT NONE
   
!/ ------------------------------------------------------------------- /  
!  purpose
!  -------
!  Given : the id number of the nodes of the cell ID_CELL 
!          and two points of a segment 
!  find  : the intersection (X_INT, Y_INT) 
!          and the id number of the cell ID_CELL_NEW
!    
!  features
!  > subroutines used    
!    FIND_A_B_C
!    POINT_INTERSECTION. for each cell verify which one contain the point XP, YP, 
!  > files used 
!    GRD_VEC_AROUND.DAT     : coeficientes a, b, and c of each side
!    GRD_NODES_LOCATION.DAT : location of the nodes 
!    GRD_GRID_AROUND.DAT    : all grid cells around 
! 
!  
!/
!/ ------------------------------------------------------------------- /
!/                                                  List of parameters      

                                             ! --- id four nodes 
      INTEGER, INTENT(IN), DIMENSION (4) :: ID_NODE       
              
      REAL,    INTENT(IN)  :: XP_INI, YP_INI ! --- position initial        
      REAL,    INTENT(IN)  :: XP_END, YP_END ! --- position final         
      INTEGER, INTENT(OUT) :: ID_CELL_NEW    ! --- id cell containing
      REAL,    INTENT(OUT) :: X_INT, Y_INT
!/
!/ ------------------------------------------------------------------- /
!/                                                    Local parameters       
      REAL,    DIMENSION(9) :: RID_CELL_NEIGH   ! --- id cell aroung
      REAL,    DIMENSION(4) :: A, B, C    
      REAL,    DIMENSION(5) :: X, Y
      INTEGER :: I, IIFF
      REAL    :: A_SEG, B_SEG, C_SEG
      LOGICAL :: INTERSECTION 

 
!/ ------------------------------------------------------------------- /   
!     
                              ! --- find coeficients of segment P_i P_f 
      CALL FIND_A_B_C ( XP_INI, YP_INI, XP_END, YP_END,                &
                       A_SEG, B_SEG, C_SEG ) 
                       
!write(*,*)"proximos 2 tem que dar zero" 
!write(*,*)xp_ini*A_seg+yp_ini*B_seg + C_seg
!write(*,*)xp_end*A_seg+yp_end*B_seg + C_seg
                             ! --- read coeficients each side of id_cell

        
      READ ( 150, REC = ID_NODE(1) ) (A(I),B(I),C(I),I=1,4)
      
      DO I = 1,4       
         READ( 100 , REC = ID_NODE(I) , IOSTAT = IIFF )                 &
                     X(I), Y(I)
!write(*,*) i, id_node(i)
!write(*,*) X(I), Y(I)            
      ENDDO

!write(*,*)"proximos 2 tem que dar zero" 
!write(*,*)x(1)*A(1)+y(1)*B(1) + C(1)
!write(*,*)x(2)*A(1)+y(2)*B(1) + C(1)

      X(5) = X(1)
      Y(5) = Y(1)
       
      DO I = 1 , 4 
      
         CALL POINT_INTERSECTION ( A_SEG, B_SEG, C_SEG,                &
                                   A(I) , B(I) , C(I) ,                &
                                   X(I), Y(I), X(I+1), Y(I+1),         &
                              X_INT, Y_INT,  INTERSECTION  )    
!write(*,*)" i " , i                               
!write(*,*)X_INT, Y_INT,  INTERSECTION    
!read(*,*)                    
                                             
         IF (INTERSECTION .EQV. .TRUE. ) GO TO 100                      
      ENDDO
                       ! --- the 9 id cell neighbouring the central cell        
                       
 100  READ ( 120, REC = ID_NODE(1) ) RID_CELL_NEIGH
      
      IF ( I == 1 ) ID_CELL_NEW = RID_CELL_NEIGH(2)
      IF ( I == 2 ) ID_CELL_NEW = RID_CELL_NEIGH(6)
      IF ( I == 3 ) ID_CELL_NEW = RID_CELL_NEIGH(8)
      IF ( I == 4 ) ID_CELL_NEW = RID_CELL_NEIGH(4)
      
!write(*,*)"achou a intersccao " 
!write(*,*) i  
!write(*,*) RID_CELL_NEIGH(:)
!write(*,*)ID_CELL_NEW
!read(*,*)

   END SUBROUTINE CELL_INTERSECTION    
!/ =================================================================== /  
! 
















!           
!/ =================================================================== /
   SUBROUTINE  CELL_INTERSECTION_NEW ( ID_NODE,                        &
                                       XP_INI, YP_INI,                 &
                                       XP_END, YP_END,                 & 
                                 X_INT, Y_INT, ID_CELL_NEW )               
!                            
   IMPLICIT NONE
   
!/ ------------------------------------------------------------------- /  
!  purpose
!  -------
!  Given : the id number of the nodes of the cell ID_CELL 
!          and two points of a segment 
!  find  : the intersection (X_INT, Y_INT) 
!          and the id number of the cell ID_CELL_NEW
!    
!  features
!  > subroutines used    
!    FIND_A_B_C
!    POINT_INTERSECTION. for each cell verify which one contain the point XP, YP, 
!  > files used 
!    GRD_VEC_AROUND.DAT     : coeficientes a, b, and c of each side
!    GRD_NODES_LOCATION.DAT : location of the nodes 
!    GRD_GRID_AROUND.DAT    : all grid cells around 
! 
!  
!/
!/ ------------------------------------------------------------------- /
!/                                                  List of parameters      

                                             ! --- id four nodes 
      INTEGER, INTENT(IN), DIMENSION (4) :: ID_NODE       
              
      REAL,    INTENT(IN)  :: XP_INI, YP_INI ! --- position initial        
      REAL,    INTENT(IN)  :: XP_END, YP_END ! --- position final         
      INTEGER, INTENT(OUT) :: ID_CELL_NEW    ! --- id cell containing
      REAL,    INTENT(OUT) :: X_INT, Y_INT
!/
!/ ------------------------------------------------------------------- /
!/                                                    Local parameters       
      REAL,    DIMENSION(9) :: RID_CELL_NEIGH   ! --- id cell aroung
      REAL,    DIMENSION(4) :: A, B, C    
      REAL,    DIMENSION(5) :: X, Y
      INTEGER :: I, IIFF
      REAL    :: A_SEG, B_SEG, C_SEG
      LOGICAL :: INTERSECTION 

 
!/ ------------------------------------------------------------------- /   
!     
                              ! --- find coeficients of segment P_i P_f 
      CALL FIND_A_B_C ( XP_INI, YP_INI, XP_END, YP_END,                &
                       A_SEG, B_SEG, C_SEG ) 
                       
!write(*,*)"proximos 2 tem que dar zero" 
!write(*,*)xp_ini*A_seg+yp_ini*B_seg + C_seg
!write(*,*)xp_end*A_seg+yp_end*B_seg + C_seg
                             ! --- read coeficients each side of id_cell

        
      READ ( 150, REC = ID_NODE(1) ) (A(I),B(I),C(I),I=1,4)
      
      DO I = 1,4    
         
         READ( 100 , REC = ID_NODE(I) , IOSTAT = IIFF )                 &
                     X(I), Y(I)         
      ENDDO


      X(5) = X(1)
      Y(5) = Y(1)
       
                      ! --- the 9 id cell neighbouring the central cell        
            
      DO I = 1 , 4 
      
         CALL TRAJ_BETWEEN( X(I),   Y(I),                               &
                            X(I+1), Y(I+1),                             &
                            XP_INI, YP_INI,                             &
                            XP_END, YP_END,                             &
                         INTERSECTION ) 
!write(*,*)"aqui", i     
!write(*,*) X(I),   X(I+1)
!write(*,*) Y(I),   Y(I+1) 
!write(*,*) XP_INI, XP_END
!write(*,*) YP_INI, YP_END
!write(*,*) "between" , INTERSECTION
!read(*,*)
         IF (INTERSECTION .EQV. .FALSE. ) CYCLE
      
         CALL TRAJ_2_LEFT( X(I),   Y(I),                               &
                           X(I+1), Y(I+1),                             &
                           XP_INI, YP_INI,                             &
                           XP_END, YP_END,                             &
                         INTERSECTION ) 
!write(*,*)  "to the right" ,INTERSECTION
!read(*,*)
         IF (INTERSECTION .EQV. .FALSE. ) CYCLE

         CALL POINT_INTERSECTION_SURE ( A_SEG, B_SEG, C_SEG,           &
                                        A(I) , B(I) , C(I) ,           &
                                  X_INT, Y_INT  )    

!write(*,*)"X_INT, Y_INT ANTES ", X_INT, Y_INT 

         X_INT = AINT(X_INT*100. + 0.5) / 100. 
         Y_INT = AINT(Y_INT*100. + 0.5) / 100.

!write(*,*)"X_INT, Y_INT DEPOIS", X_INT, Y_INT 
!read(*,*)

         GOTO 100
      ENDDO                     
                                                          
 100  READ ( 120, REC = ID_NODE(1) ) RID_CELL_NEIGH
      
      IF ( I == 1 ) ID_CELL_NEW = RID_CELL_NEIGH(2)
      IF ( I == 2 ) ID_CELL_NEW = RID_CELL_NEIGH(6)
      IF ( I == 3 ) ID_CELL_NEW = RID_CELL_NEIGH(8)
      IF ( I == 4 ) ID_CELL_NEW = RID_CELL_NEIGH(4)
      
      
      
      
!write(*,*)"achou a intersccao " 
!write(*,*) i  
!write(*,*) RID_CELL_NEIGH(:)
!write(*,*)ID_CELL_NEW
!read(*,*)

   END SUBROUTINE CELL_INTERSECTION_NEW
!/ =================================================================== /  
! 


















! 
!/ =================================================================== /
   SUBROUTINE TRAJ_2_LEFT ( X_1,Y_1, X_2,Y_2 ,  &
                          X_I,Y_I, X_F,Y_F,    &
                     WITHIN )

   IMPLICIT NONE

! /// -------------------------------------------------
! ///
! ///    verify if the point X_POS  , Y_POS  is inside the triangle.
! ///    using method of cross vector
! ///
! ///    INSIDE = .TRUE. , it is inside
! /// 
! /// -------------------------------------------------

!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: X_1,Y_1 , X_2,Y_2 
      REAL,    INTENT (IN)  :: X_I,Y_I , X_F,Y_F
      LOGICAL, INTENT (OUT) :: WITHIN
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter
      REAL ::  XFI, YFI
      REAL ::  X1I, Y1I 
      REAL ::  X2I, Y2I
      
      REAL ::  P_1 , P_2                                               

      XFI = X_F - X_I ; YFI =  Y_F - Y_I 
      X1I = X_1 - X_I ; Y1I =  Y_1 - Y_I 
      X2I = X_2 - X_I ; Y2I =  Y_2 - Y_I 
      
      P_1 = XFI*Y1I - YFI*X1I
      P_2 = XFI*Y2I - YFI*X2I
      
      WITHIN = .FALSE. 
      IF ( P_1 .LE. 0. .AND. P_2 .GE. 0.) WITHIN = .TRUE. 

   END SUBROUTINE  TRAJ_2_LEFT
!/ =================================================================== /
! 
! 

!/ =================================================================== /
   SUBROUTINE POINT_INTERSECTION_SURE ( A_1, B_1, C_1,                 &
                                        A_2, B_2, C_2,                 &
                              X_INT, Y_INT  )

   IMPLICIT NONE
   
!/ ------------------------------------------------------------------- /  
!    purpose: 
!    --------   
!    1. find the intersection point ( X_INT, Y_INT) between 
!       the first segment (coeficients A_1, B_1, C_1) and 
!       the second (coeficients A_1, B_1, C_1) 
!    2. returns 
!       INTERSECCTION = .TRUE. if intersection is between 
!                              (X1, Y1) and (X2, Y2)
!                             .FALSE. 
! 
!    3. X_INT, Y_INT  =  0.999E+9  if there is no intersection                         
!   
! 
!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: A_1, B_1, C_1
      REAL,    INTENT (IN)  :: A_2, B_2, C_2
      REAL,    INTENT (OUT) :: X_INT, Y_INT

!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter

      REAL ::  DET_T,  DET_X,   DET_Y  
!/ ------------------------------------------------------------------- /  
  
      X_INT = 0.999E+9
      Y_INT = 0.999E+9

      DET_T =  A_1*B_2 - B_1*A_2 

      IF ( abs(DET_T) <= 0.000001 ) GO TO 300

      DET_X = -C_1*B_2 + B_1*C_2
      DET_Y = -A_1*C_2 + C_1*A_2

      X_INT =  DET_X /   DET_T
      Y_INT =  DET_Y /   DET_T


  300 CONTINUE

   END SUBROUTINE POINT_INTERSECTION_SURE
!/ =================================================================== /
! 
! 







! 
!            
! 
!/ =================================================================== /
   SUBROUTINE VEL_INTERPOLATED ( I_NODE, WEI,                          &
                        U_PAR, V_PAR )            
 
   IMPLICIT NONE
!   
!/ ------------------------------------------------------------------- /  
!    purpose: 
!    --------
!    to compute the velocity (U_PAR, V_PAR) using weights and velocities in each node
!    
!    features 
!    > the weights is input 
!    > the velocities at each grid node is read 
! 
!/ ------------------------------------------------------------------- /
!/                                                  List of parameters   
      INTEGER, INTENT(IN), DIMENSION (4) :: I_NODE
      REAL,    INTENT(IN), DIMENSION (4) :: WEI
      REAL,    INTENT(OUT)               :: U_PAR, V_PAR  
!/
!/ ------------------------------------------------------------------- /
!/                                                    Local parameters     
      REAL, DIMENSION (4) :: VX, VY
         
      READ ( 130 , REC =  I_NODE(1)) VX(1), VY(1)
      READ ( 130 , REC =  I_NODE(2)) VX(2), VY(2)
      READ ( 130 , REC =  I_NODE(3)) VX(3), VY(3)
      READ ( 130 , REC =  I_NODE(4)) VX(4), VY(4)  
           
      U_PAR = SUM( WEI(:)*VX(:) )
      V_PAR = SUM( WEI(:)*VY(:) )

!WRITE(*,*) VX(:)
!WRITE(*,*) VY(:)
!WRITE(*,*)  U_PAR, V_PAR
!READ(*,*)
   END SUBROUTINE VEL_INTERPOLATED
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE INSIDE_AREA (  X_1 ,Y_1 , X_2,Y_2 , X_3,Y_3 , X_4,Y_4,   &
                             X_P, Y_P  ,                           &
                        INSIDE )

   IMPLICIT NONE
   
! *** see IMPORTANT OBSERVATION BELOW
!   
!/ ------------------------------------------------------------------- /  
!    purpose: 
!    --------
!    to check if the point (X_P, Y_P) lies within the quadrilatere 
!   
!    INSIDE = .TRUE. if it is inside
!             .FALSE. otherwise
!   
!    IMPORTANT OBSERVATION: the nodes of the qudrilatere is 
!                           counterclockwise 
! 
! 
!  
!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: X_1 ,Y_1 , X_2,Y_2 , X_3,Y_3 , X_4,Y_4
      REAL,    INTENT (IN)  :: X_P, Y_P 
      LOGICAL, INTENT (OUT) :: INSIDE
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter
      REAL :: A_123, A_134, A_1234 
      REAL :: A_12P, A_23P, A_34P, A_41P, A_1234_P
      REAL :: DIFF  
      
!/ ------------------------------------------------------------------- /   
        
      A_123 = 0.5*ABS((X_2-X_1)*(Y_3-Y_1) - (X_3-X_1)*(Y_2-Y_1))
      A_134 = 0.5*ABS((X_4-X_1)*(Y_3-Y_1) - (X_3-X_1)*(Y_4-Y_1))    
      A_1234 = A_123 + A_134


      A_12P = 0.5*ABS((X_1-X_P)*(Y_2-Y_P) - (X_2-X_P)*(Y_1-Y_P))
      A_23P = 0.5*ABS((X_2-X_P)*(Y_3-Y_P) - (X_3-X_P)*(Y_2-Y_P))
     
      A_34P = 0.5*ABS((X_3-X_P)*(Y_4-Y_P) - (X_4-X_P)*(Y_3-Y_P))
      A_41P = 0.5*ABS((X_4-X_P)*(Y_1-Y_P) - (X_1-X_P)*(Y_4-Y_P))
  
  
      A_1234_P = A_12P + A_23P + A_34P + A_41P 
           
      INSIDE = .TRUE. 
      DIFF = ABS((A_1234 - A_1234_P)/(A_1234+1.E-15))
      IF ( DIFF .GT. 0.0001 ) INSIDE = .FALSE.        
      
   END SUBROUTINE  INSIDE_AREA
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE INSIDE_CROSS_quadrilatere ( X_1,Y_1 , X_2,Y_2 ,          &
                                          X_3,Y_3 , X_4,Y_4 ,          &
                                          X_POS, Y_POS  ,              &
                                        INSIDE )

   IMPLICIT NONE

! /// -------------------------------------------------
! ///
! ///    verify if the point X_POS  , Y_POS  is inside the rectangule.
! ///    using method of cross vector
! ///
! ///    INSIDE = .TRUE. , it is inside
! /// 
! /// -------------------------------------------------

!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: X_1 ,Y_1 , X_2,Y_2 , X_3,Y_3 , X_4,Y_4
      REAL,    INTENT (IN)  :: X_POS, Y_POS  
      LOGICAL, INTENT (OUT) :: INSIDE
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter


      REAL ::  P1 , P2 , P3 , P4                                                           

      REAL ::  X21, X32, X43, X14
      REAL ::  Y21, Y32, Y43, Y14
      REAL, DIMENSION (4)  ::  X_P ,  Y_P


      X_P(1) = X_POS  - X_1
      X_P(2) = X_POS  - X_2
      X_P(3) = X_POS  - X_3
      X_P(4) = X_POS  - X_4

      Y_P(1) = Y_POS  - Y_1
      Y_P(2) = Y_POS  - Y_2
      Y_P(3) = Y_POS  - Y_3
      Y_P(4) = Y_POS  - Y_4


      X21 = X_2 - X_1 ; Y21 =  Y_2 - Y_1 
      X32 = X_3 - X_2 ; Y32 =  Y_3 - Y_2 
      X43 = X_4 - X_3 ; Y43 =  Y_4 - Y_3 
      X14 = X_1 - X_4 ; Y14 =  Y_1 - Y_4 

      INSIDE = .TRUE. 
    
      P1 =  X_P(1)*Y21 - Y_P(1)*X21 
      P2 =  X_P(2)*Y32 - Y_P(2)*X32  ; IF (P1*P2 .LT. 0. ) INSIDE = .FALSE. 
      P3 =  X_P(3)*Y43 - Y_P(3)*X43  ; IF (P2*P3 .LT. 0. ) INSIDE = .FALSE. 
      P4 =  X_P(4)*Y14 - Y_P(4)*X14  ; IF (P3*P4 .LT. 0. ) INSIDE = .FALSE. 
      
      IF (P4*P1 .LT. 0. ) INSIDE = .FALSE. 


   END SUBROUTINE  INSIDE_CROSS_quadrilatere
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE FIND_A_B_C ( X_1, Y_1, X_2, Y_2,  &
                       A, B, C ) 
   IMPLICIT NONE
!   
!/ ------------------------------------------------------------------- /  
!    purpose: 
!    --------     
!    to find the coeficients a, b, and c of the segment passing through
!    the 2 given points 
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
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE POINT_INTERSECTION ( A_1, B_1, C_1,                      &
                                   A_2, B_2, C_2,                      &
                                   X1, Y1, X2, Y2,                     &
                              X_INT, Y_INT, INTERSECTION  )

   IMPLICIT NONE
   
!/ ------------------------------------------------------------------- /  
!    purpose: 
!    --------   
!    1. find the intersection point ( X_INT, Y_INT) between 
!       the first segment (coeficients A_1, B_1, C_1) and 
!       the second (coeficients A_1, B_1, C_1) 
!    2. returns 
!       INTERSECCTION = .TRUE. if intersection is between 
!                              (X1, Y1) and (X2, Y2)
!                             .FALSE. 
! 
!    3. X_INT, Y_INT  =  0.999E+9  if there is no intersection                         
!   
! 
!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: A_1, B_1, C_1
      REAL,    INTENT (IN)  :: A_2, B_2, C_2
      REAL,    INTENT (IN)  :: X1, Y1, X2, Y2
      REAL,    INTENT (OUT) :: X_INT, Y_INT
      LOGICAL, INTENT (OUT) :: INTERSECTION
      REAL,    PARAMETER :: UNDEF = 0.999E+9
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter
      REAL ::  A_PAT,   B_PAT,   C_PAT  
      REAL ::  A_SEG,   B_SEG,   C_SEG
      REAL ::  D_I1,  D_I2,  D_12 
      REAL ::  DET_T,  DET_X,   DET_Y  
!/ ------------------------------------------------------------------- /  
      X_INT = UNDEF
      Y_INT = UNDEF  
      INTERSECTION = .FALSE. 
      
      
      DET_T =  A_1*B_2 - B_1*A_2
      IF (DET_T == 0.0 ) GO TO 300
      DET_X = -C_1*B_2 + B_1*C_2
      DET_Y = -A_1*C_2 + C_1*A_2
 
      X_INT =  DET_X /   DET_T
      Y_INT =  DET_Y /   DET_T
      
      
!write(*,*)"DET_T"
!write(*,*)A_1,B_1
!write(*,*)A_2,B_2
!write(*,*)DET_T  
!write(*,*)DET_X
!write(*,*)DET_Y
!write(*,*)X_INT,Y_INT 
!read(*,*)

    
      D_I1 = ( (X_INT - X1)**2   +   (Y_INT - Y1)**2 )**0.5
      D_I2 = ( (X_INT - X2)**2   +   (Y_INT - Y2)**2 )**0.5
      D_12 = ( (X1 - X2)**2 + (Y1 - Y2)**2 )**0.5
      
      INTERSECTION = .FALSE. 
      IF ( (D_I1+D_I2) <=  D_12*1.000001 ) INTERSECTION = .TRUE. 
  300 CONTINUE      

   END SUBROUTINE POINT_INTERSECTION
!/ =================================================================== /
! 
! 

!/ =================================================================== /
   SUBROUTINE TRAJ_BETWEEN ( X_1,Y_1, X_2,Y_2 ,                        &
                             X_I,Y_I, X_F,Y_F,                         &
                      WITHIN )

   IMPLICIT NONE

! /// -------------------------------------------------
! ///
! ///    verify if the point X_LOC  , Y_LOC  is inside the triangle.
! ///    using method of cross vector
! ///
! ///    INSIDE = .TRUE. , it is inside
! /// 
! /// -------------------------------------------------

!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: X_1,Y_1 , X_2,Y_2 
      REAL,    INTENT (IN)  :: X_I,Y_I , X_F,Y_F
      LOGICAL, INTENT (OUT) :: WITHIN
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter
      REAL ::  XFI, YFI
      REAL ::  X1I, Y1I 
      REAL ::  X2I, Y2I
      
      REAL ::  P_1 , P_2                                               

      XFI = X_F - X_I ; YFI =  Y_F - Y_I 
      X1I = X_1 - X_I ; Y1I =  Y_1 - Y_I 
      X2I = X_2 - X_I ; Y2I =  Y_2 - Y_I  

      P_1 = XFI*Y1I - YFI*X1I
      P_2 = XFI*Y2I - YFI*X2I
      
      WITHIN = .FALSE. 
      IF ( P_1* P_2 .LE. 0.) WITHIN = .TRUE. 


   END SUBROUTINE  TRAJ_BETWEEN
!/ =================================================================== /
! 



! 
!/ =================================================================== /
   SUBROUTINE UPGRADE_LOCATION_SU ( TIME, DT,  NBIT, N_PAR) 
      
   IMPLICIT NONE 
   
!/ ------------------------------------------------------------------- /  
!    purpose:    upgrade the location of su parcels 
!    ------- 
! 
!    used files 
!      TEV_LOCATION.DAT   : file upgraded  
!      AUX_LOCATION_NEW   : incremental displacement and new grid cell
!      GRD_CELL_NODES.DAT : informations about the nodes of each cell 
!      
!    features
!      > work only with parcels on the surface
!      > read increment and add to position 
!      > find the nearest node to the parcel 
!        - if the mask of this node = 1, 
!          then parcel is beached STAT_PAR = -3
!        - if depth of this node > 9999 
!          then parcel is on the boundary STAT_PAR = 0
!      > file AUX_LOCATION_NEW is removed 
!     
!/
!/ ------------------------------------------------------------------- /
!/                                                  List of parameters                         
      INTEGER, INTENT(IN) ::  NBIT, N_PAR
      REAL,    INTENT(IN) ::  TIME, DT
!/
!/ ------------------------------------------------------------------- /
!/                                                    Local parameters
      INTEGER :: IP, ID_CELL
      REAL    :: DX ,  DY, RID_CELL
      REAL    :: X_POS,     Y_POS, GRID, STAT_PAR 
      REAL    :: X_POS_NEW, Y_POS_NEW  
      
      REAL,    DIMENSION(4) ::  X_NODE, Y_NODE
      INTEGER, DIMENSION(4) ::  I_NODE 
      REAL,    DIMENSION(4) ::  DEP_NODE    
      REAL,    DIMENSION(4) ::  MASK_NODE 
      REAL,    DIMENSION(4) ::  WEI
      INTEGER, DIMENSION(1) ::  ID_AUX   
integer :: IIFF
!/          
!/ ------------------------------------------------------------------- /     
            
      OPEN ( 100 , FILE = "GRD_NODES_LOCATION.DAT" ,                   &
                          STATUS = 'UNKNOWN'   ,                       &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 4 )* NBIT  ) 
 
      OPEN ( 140 , FILE = "TEV_LOCATION.DAT" , STATUS = 'UNKNOWN'   ,  &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 4 )* NBIT  ) 
                          ! ###   X_POS, Y_POS, GRID, STAT_PAR    
     
      OPEN ( 81 ,  FILE = "AUX_LOCATION_NEW" , STATUS = 'UNKNOWN'   ,  &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 3 )* NBIT  )  
                          ! ###   DX, DY, RID_CELL   
                           
      OPEN ( 110 , FILE = "GRD_CELL_NODES.DAT" , STATUS = 'UNKNOWN'   ,  &
                          FORM = 'UNFORMATTED' ,  ACCESS = 'DIRECT' ,  &
                          RECL   = ( 3 )* NBIT  )    
                          ! ###  R1, R2, R3

      DO 300 IP = 1,  N_PAR  
          
          READ(140 , REC = IP ) X_POS, Y_POS, GRID, STAT_PAR

if(ip .eq. 1) write(1700,'(F10.1,F10.1,F10.1, I10, F7.0 )')     &
              TIME,X_POS, Y_POS, NINT(GRID), STAT_PAR
         
          IF( NINT(STAT_PAR) .NE. -1 ) GOTO 300

          READ( 81 , REC = IP , IOSTAT = IIFF ) DX ,  DY, RID_CELL

          X_POS_NEW =    X_POS + DX
          Y_POS_NEW =    Y_POS + DY

!write(*,*)"no upgrade" ,  NINT(RID_CELL)
!write(*,*)"STAT_PAR  " , STAT_PAR 
          ID_CELL = NINT(RID_CELL)

          IF( ID_CELL == 0 ) GOTO 300 
          CALL FIND_FOUR_PTOS ( ID_CELL ,                              &
                        I_NODE , X_NODE, Y_NODE, DEP_NODE, MASK_NODE )  
                                     
          CALL  FOUR_WEIGHTS( X_POS_NEW, Y_POS_NEW,                    &
                              X_NODE, Y_NODE, MASK_NODE,               &
                        WEI)   
                                                     
          ID_AUX =  MAXLOC (WEI(:)) 
                                                           
          IF ( MASK_NODE(ID_AUX(1)) .LT. 0.99) STAT_PAR = -3 
                                                ! ---  parcel beached 
         
        ! ------------------------------------------
        ! STATUS OF THE PARCELS
        ! ---------------------
        ! STAT_PAR =  1 : in the water
        ! STAT_PAR =  0 : outside the domain
        ! STAT_PAR = -1 : surface
        ! STAT_PAR = -2 : deposited at the bottom 
        ! STAT_PAR = -3 : beached  ONLY: WORK_WITH_SU

        ! ------------------------------------------   
              
          IF (  DEP_NODE(ID_AUX(1)) .GE. 9999.) STAT_PAR = 0.
                                                ! --- outside the domain

          WRITE(140 , REC = IP ) &
                   X_POS_NEW, Y_POS_NEW, RID_CELL, STAT_PAR

if(ip .eq. 1) write(1700,'(F10.1,F10.1,F10.1, I10, F7.0 )')           &
              TIME+DT, X_POS_NEW, Y_POS_NEW, NINT(RID_CELL), STAT_PAR

  300 CONTINUE 
  
      CLOSE( 100 )   
      CLOSE( 140 )   
      CLOSE( 110 ) 
      CLOSE(  81 )

    ! CALL SYSTEM ("\rm -f AUX_LOCATION_NEW" )

   END SUBROUTINE  UPGRADE_LOCATION_SU 
!/ =================================================================== /
! 
! 
!/ =================================================================== /
! 
END MODULE  MODULE_WORK_WITH_SU
!
!/ =================================================================== /

