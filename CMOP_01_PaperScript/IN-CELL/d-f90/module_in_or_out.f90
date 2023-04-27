!/ ------------------------------------------------------------------- /

   MODULE MODULE_IN_OR_OUT

! 
!/   
      CONTAINS

!/ =================================================================== /
! 

! 
!/ =================================================================== /
   SUBROUTINE INSIDE_BARI_KRAMER ( X_1,Y_1 , X_2,Y_2 , X_3,Y_3 ,       &
                                   X_POS, Y_POS  ,                     &
                                 INSIDE )

   IMPLICIT NONE

!                3
!                x
! 
! 
!                  P
!                             x
!           x                  2
!           1
! 
!     ------------------------------------------------------
!      P = P_1 + v * (P_2 - P_1) + w * (P_3 - P_1 ) 
!           OR
!      P - P_1 = v * (P_2 - P_1) + w * (P_3 - P_1 ) 
!          OR
!      V_p     = v *     V_2     + w * V_3  (vectors are defined)
! 
!     ------------------------------------------------------    
!     solution for v and w 
!    
!     V_p . V_2  = v * V_2 . V_2  + w * V_3  . V_2 (dot product)
!     V_p . V_3  = v * V_2 . V_3  + w * V_3  . V_3 (dot product)
!         OR 
!     using the following notation for dot product
!     d_p2  =  V_p . V_2
!     d_22  =  V_2 . V_2 
!     d_32  =  V_3 . V_2 
!     d_p3  =  V_p . V_3  
!     d_23  =  V_2 . V_3  
!     d_33  =  V_3 . V_3
!   
!     d_p2  = v * d_22  + w * d_32 
!     d_p3  = v * d_23  + w * d_33 
!     
!     multiply and subtrai
!     d_p2 * d_33  = v * d_22 * d_33  + w * d_32 * d_33 
!     d_p3 * d_32  = v * d_23 * d_32  + w * d_33 * d_32      
!     
!     
!     v = ( d_p2 * d_33 - d_p3 * d_32 )/(d_22 * d_33 -  d_23 * d_32 ) 
! 
!     multiply and subtrai
!     d_p2 * d_23  = v * d_22 * d_23  + w * d_32 * d_23 
!     d_p3 * d_22  = v * d_23 * d_22  + w * d_33 * d_22  
!     
!     w =  ( d_p3 * d_22 - d_p2 * d_23 )/(d_33 * d_22 - d_32 * d_23 )
!  
!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: X_1 ,Y_1 , X_2,Y_2 , X_3,Y_3 
      REAL,    INTENT (IN)  :: X_POS, Y_POS  
      LOGICAL, INTENT (OUT) :: INSIDE
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter 

      REAL, DIMENSION (3) :: X_P, Y_P
      REAL  ::  v,  w  
      REAL  :: DET, DET_V,  DET_W

!/ ------------------------------------------------------------------- /  

! --- dot products! 
 
      X_P(1) = X_2   - X_1
      X_P(2) = X_3   - X_1
      X_P(3) = X_POS - X_1

      Y_P(1) = Y_2    - Y_1
      Y_P(2) = Y_3    - Y_1
      Y_P(3) = Y_POS  - Y_1

!      V_3 = v.V_1 + w.V_2
!    
!   |  X_P(1) X_P(2) |    | v |      | X_P(3) |
!   |                |  X |   |  =   |        |
!   |  Y_P(1) Y_P(2) |    | w |      | Y_P(3) |
!   
    DET   = X_P(1)*Y_P(2) - X_P(2)*Y_P(1)
    DET_V = X_P(3)*Y_P(2) - X_P(2)*Y_P(3)
    DET_W = X_P(3)*Y_P(3) - X_P(3)*Y_P(1)
    
      w = DET_W/DET 
      v = DET_V/DET 

     INSIDE = .TRUE. 
     
     
     IF (v .LT. 0. .OR. v .GT. 1. ) INSIDE = .FALSE.    
     IF (w .LT. 0. .OR. w .GT. 1. ) INSIDE = .FALSE.      
     IF ((v + w) .GT. 1.  ) INSIDE = .FALSE.    


   END SUBROUTINE  INSIDE_BARI_KRAMER
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE INSIDE_AREA_triangle (  X_1,Y_1 , X_2,Y_2 , X_3,Y_3 ,    &
                                      X_P,Y_P ,                        &
                                   INSIDE )

   IMPLICIT NONE

! /// -------------------------------------------------
! ///
! /// 
! /// -------------------------------------------------

!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: X_1 ,Y_1 , X_2,Y_2 , X_3,Y_3 
      REAL,    INTENT (IN)  :: X_P, Y_P 
      LOGICAL, INTENT (OUT) :: INSIDE
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter
      REAL :: A_123
      REAL :: A_12P, A_23P, A_31P, A_123_P
      REAL :: DIFF  
      
!/ ------------------------------------------------------------------- /   
        
      A_123 = 0.5*ABS((X_2-X_1)*(Y_3-Y_1) - (X_3-X_1)*(Y_2-Y_1))
      
      A_12P = 0.5*ABS((X_1-X_P)*(Y_2-Y_P) - (X_2-X_P)*(Y_1-Y_P))
      A_23P = 0.5*ABS((X_2-X_P)*(Y_3-Y_P) - (X_3-X_P)*(Y_2-Y_P))
      A_31P = 0.5*ABS((X_3-X_P)*(Y_1-Y_P) - (X_1-X_P)*(Y_3-Y_P))
      
      A_123_P = A_12P + A_23P + A_31P 
            
      INSIDE = .TRUE. 
      DIFF = ABS((A_123 - A_123_P)/(A_123+1.E-15))
      IF ( DIFF .GT. 0.0001 ) INSIDE = .FALSE.        
               
   END SUBROUTINE  INSIDE_AREA_triangle
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE INSIDE_AREA_quadrilatere ( X_1,Y_1 , X_2,Y_2 , X_3,Y_3 , &
                                         X_4,Y_4 ,                     &
                                         X_P,Y_P ,                     &
                                       INSIDE )

   IMPLICIT NONE

! /// -------------------------------------------------
! ///
! /// 
! /// -------------------------------------------------

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
      A_134 = 0.5*ABS((X_3-X_1)*(Y_4-Y_1) - (X_4-X_1)*(Y_3-Y_1))    
      
      A_1234 = A_123 + A_134
      
      A_12P = 0.5*ABS((X_1-X_P)*(Y_2-Y_P) - (X_2-X_P)*(Y_1-Y_P))
      A_23P = 0.5*ABS((X_2-X_P)*(Y_3-Y_P) - (X_3-X_P)*(Y_2-Y_P))
      A_34P = 0.5*ABS((X_3-X_P)*(Y_4-Y_P) - (X_4-X_P)*(Y_3-Y_P))
      A_41P = 0.5*ABS((X_4-X_P)*(Y_1-Y_P) - (X_1-X_P)*(Y_4-Y_P))
      
      A_1234_P = A_12P + A_23P + A_34P + A_41P 
            
      INSIDE = .TRUE. 
      DIFF = ABS((A_1234 - A_1234_P)/(A_1234+1.E-15))
      IF ( DIFF .GT. 0.0001 ) INSIDE = .FALSE.        
               
   END SUBROUTINE  INSIDE_AREA_quadrilatere
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE INSIDE_AREA_pentagone (  X_1,Y_1 , X_2,Y_2 , X_3,Y_3 ,   &
                                       X_4,Y_4 , X_5,Y_5,              &
                                       X_P,Y_P ,                       &
                                    INSIDE )

   IMPLICIT NONE

! /// -------------------------------------------------
! ///
! /// 
! /// -------------------------------------------------

!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: X_1 ,Y_1 , X_2,Y_2 , X_3,Y_3 , X_4,Y_4 , X_5,Y_5
      REAL,    INTENT (IN)  :: X_P, Y_P 
      LOGICAL, INTENT (OUT) :: INSIDE
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter
      REAL :: A_123, A_134, A_145, A_12345 
      REAL :: A_12P, A_23P, A_34P, A_45P, A_51P,  A_12345_P
      REAL :: DIFF  
      
!/ ------------------------------------------------------------------- /   
        
      A_123 = 0.5*ABS((X_2-X_1)*(Y_3-Y_1) - (X_3-X_1)*(Y_2-Y_1))
      A_134 = 0.5*ABS((X_3-X_1)*(Y_4-Y_1) - (X_4-X_1)*(Y_3-Y_1))   
      A_145 = 0.5*ABS((X_4-X_1)*(Y_5-Y_1) - (X_5-X_1)*(Y_4-Y_1))
      
      A_12345 = A_123 + A_134 + A_145
      
      A_12P = 0.5*ABS((X_1-X_P)*(Y_2-Y_P) - (X_2-X_P)*(Y_1-Y_P))
      A_23P = 0.5*ABS((X_2-X_P)*(Y_3-Y_P) - (X_3-X_P)*(Y_2-Y_P))
      A_34P = 0.5*ABS((X_3-X_P)*(Y_4-Y_P) - (X_4-X_P)*(Y_3-Y_P))
      A_45P = 0.5*ABS((X_4-X_P)*(Y_5-Y_P) - (X_5-X_P)*(Y_4-Y_P))
      A_51P = 0.5*ABS((X_5-X_P)*(Y_1-Y_P) - (X_1-X_P)*(Y_5-Y_P))
            
      A_12345_P = A_12P + A_23P + A_34P + A_45P + A_51P 
            
      INSIDE = .TRUE. 
      DIFF = ABS((A_12345 - A_12345_P)/(A_12345+1.E-15))
      IF ( DIFF .GT. 0.0001 ) INSIDE = .FALSE.        
               
   END SUBROUTINE  INSIDE_AREA_pentagone
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE INSIDE_CROSS_triangle ( X_1,Y_1 , X_2,Y_2 , X_3,Y_3,     &
                                      X_POS, Y_POS  ,                  &
                                    INSIDE )

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

      REAL,    INTENT (IN)  :: X_1 ,Y_1 , X_2,Y_2 , X_3,Y_3
      REAL,    INTENT (IN)  :: X_POS, Y_POS  
      LOGICAL, INTENT (OUT) :: INSIDE
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter


      REAL ::  P1 , P2 , P3                                                          

      REAL ::  X21, X32, X13
      REAL ::  Y21, Y32, Y13
      REAL, DIMENSION (3)  ::  X_P ,  Y_P

!/ ------------------------------------------------------------------- /  
      X_P(1) = X_POS  - X_1
      X_P(2) = X_POS  - X_2
      X_P(3) = X_POS  - X_3

      Y_P(1) = Y_POS  - Y_1
      Y_P(2) = Y_POS  - Y_2
      Y_P(3) = Y_POS  - Y_3

      X21 = X_2 - X_1 ; Y21 =  Y_2 - Y_1 
      X32 = X_3 - X_2 ; Y32 =  Y_3 - Y_2 
      X13 = X_1 - X_3 ; Y13 =  Y_1 - Y_3 

      INSIDE = .TRUE. 
    
      P1 =  X_P(1)*Y21 - Y_P(1)*X21 
      P2 =  X_P(2)*Y32 - Y_P(2)*X32  ; IF (P1*P2 .LT. 0. ) INSIDE = .FALSE. 
      P3 =  X_P(3)*Y13 - Y_P(3)*X13  ; IF (P2*P3 .LT. 0. ) INSIDE = .FALSE. 
      
      IF (P3*P1 .LT. 0. ) INSIDE = .FALSE. 


   END SUBROUTINE  INSIDE_CROSS_triangle
!/ =================================================================== /
! 
! 
! 
!/ =================================================================== /
   SUBROUTINE INSIDE_CROSS_quadrilatere ( X_1,Y_1 , X_2,Y_2 , X_3,Y_3 ,&
                                          X_4,Y_4 ,                    &
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
      
!/ ------------------------------------------------------------------- /  
!/

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
   SUBROUTINE INSIDE_CROSS_pentagone ( X_1,Y_1 , X_2,Y_2 , X_3,Y_3 ,   &
                                       X_4,Y_4 , X_5,Y_5 ,             &
                                       X_POS, Y_POS  ,                 &
                                     INSIDE )

   IMPLICIT NONE

! /// -------------------------------------------------
! ///
! ///    verify if the point X_POS  , Y_POS  is inside the pentagone.
! ///    using method of cross vector
! ///
! ///    INSIDE = .TRUE. , it is inside
! /// 
! /// -------------------------------------------------

!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL,    INTENT (IN)  :: X_1 ,Y_1 , X_2,Y_2 , X_3,Y_3 , X_4,Y_4 , X_5,Y_5  
      REAL,    INTENT (IN)  :: X_POS, Y_POS  
      LOGICAL, INTENT (OUT) :: INSIDE
      
!/ ------------------------------------------------------------------- /  
!/                                                     Local parameter


      REAL ::  P1 , P2 , P3 , P4 , P5                                                   

      REAL ::  X21, X32, X43, X54 , X15
      REAL ::  Y21, Y32, Y43, Y54 , Y15
      REAL, DIMENSION (5)  ::  X_P ,  Y_P
      
!/ ------------------------------------------------------------------- /  

      X_P(1) = X_POS  - X_1
      X_P(2) = X_POS  - X_2
      X_P(3) = X_POS  - X_3
      X_P(4) = X_POS  - X_4
      X_P(5) = X_POS  - X_5

      Y_P(1) = Y_POS  - Y_1
      Y_P(2) = Y_POS  - Y_2
      Y_P(3) = Y_POS  - Y_3
      Y_P(4) = Y_POS  - Y_4
      Y_P(5) = Y_POS  - Y_5

      X21 = X_2 - X_1 ; Y21 =  Y_2 - Y_1 
      X32 = X_3 - X_2 ; Y32 =  Y_3 - Y_2 
      X43 = X_4 - X_3 ; Y43 =  Y_4 - Y_3 
      X54 = X_5 - X_4 ; Y54 =  Y_5 - Y_4 
      X15 = X_1 - X_5 ; Y15 =  Y_1 - Y_5 
      
      INSIDE = .TRUE. 
    
      P1 =  X_P(1)*Y21 - Y_P(1)*X21 
      P2 =  X_P(2)*Y32 - Y_P(2)*X32  ; IF (P1*P2 .LT. 0. ) INSIDE = .FALSE. 
      P3 =  X_P(3)*Y43 - Y_P(3)*X43  ; IF (P2*P3 .LT. 0. ) INSIDE = .FALSE. 
      P4 =  X_P(4)*Y54 - Y_P(4)*X54  ; IF (P3*P4 .LT. 0. ) INSIDE = .FALSE. 
      P5 =  X_P(5)*Y15 - Y_P(5)*X15  ; IF (P4*P5 .LT. 0. ) INSIDE = .FALSE. 
            
      IF (P5*P1 .LT. 0. ) INSIDE = .FALSE. 


   END SUBROUTINE  INSIDE_CROSS_pentagone
!/ =================================================================== /
! 
! 

! 


! 
! 
!/ =================================================================== /
! 
END MODULE  MODULE_IN_OR_OUT
!
!/ =================================================================== /

