
  
   PROGRAM WORK_WITH_INSIDE
!/ ------------------------------------------------------------------- /  
!/ 
      USE MODULE_IN_OR_OUT,   ONLY:  INSIDE_BARI_KRAMER  ,           &  
                                     INSIDE_AREA_triangle ,          &  
                                     INSIDE_AREA_quadrilatere ,      &  
                                     INSIDE_AREA_pentagone ,         &  
                                     INSIDE_CROSS_triangle ,         &  
                                     INSIDE_CROSS_quadrilatere ,     &  
                                     INSIDE_CROSS_pentagone

                                    
   IMPLICIT NONE 
   
!/ ------------------------------------------------------------------- /  
!/                                                      Parameter list

      REAL     :: X_1 ,Y_1 , X_2,Y_2 , X_3,Y_3 , X_4,Y_4, X_5,Y_5    
      REAL     :: X_POS, Y_POS  
      LOGICAL  :: INSIDE , INSIDE_1 , INSIDE_2 , INSIDE_3  
      INTEGER  :: I, METODO, N_VEZES
      real :: start, finish

      call cpu_time(start)
        ! put code to test here

          
!/ ------------------------------------------------------------------- /      

!       METODO = 13     !   INSIDE_BARI_KRAMER_triangle
!       METODO = 14     !   INSIDE_BARI_KRAMER_quadrilatere
!       METODO = 15     !   INSIDE_BARI_KRAMER_pentagone 
        
        
!        METODO = 23     !   INSIDE_AREA_triangle 
!        METODO = 24     !   INSIDE_AREA_quadrilatere
!        METODO = 25     !   INSIDE_AREA_pentagone 
        
!        METODO = 33     !   INSIDE_CROSS_triangle 
!        METODO = 34     !   INSIDE_CROSS_quadrilatere 
!        METODO = 35     !   INSIDE_CROSS_pentagone


     WRITE(*,*) " ------------------------------------------------------ " 
     WRITE(*,*) " choose the method " 
     WRITE(*,*) " "
     WRITE(*,*) " type 1, 2, or 3 "  
     WRITE(*,*) " "    
     WRITE(*,*) " 1 = BariCoo method - the barycentric coordinates " 
     WRITE(*,*) " 2 = AreaCom method - the areas comparison        " 
     WRITE(*,*) " 3 = LoopCon method - the looping counterclockwise"
  
     READ(*,*) METODO 
  
     WRITE(*,*) " ------------------------------------------------------ " 
     WRITE(*,*) " choose the polygon " 
     WRITE(*,*) " "
     WRITE(*,*) " type 3, 4, or 5 "  
     WRITE(*,*) " "    
     WRITE(*,*) " 3 = triangle (3 edges)     " 
     WRITE(*,*) " 4 = quadrilatere (4 edges) "
     WRITE(*,*) " 5 = pentagone (5 edges)    "   
     
     READ(*,*)I
     
     METODO = METODO *10 + I 

    
!/ ------------------------------------------------------------------- /           
      X_1  = 0.
      Y_1  = 0.
   
      X_2  = 1.
      Y_2  = 0.
    
      X_3  = 1.
      Y_3  = 1.
      
      X_4  = 0.
      Y_4  = 1.  
      
      X_5  = -0.5
      Y_5  = 0.5      
      
   
      X_POS = 0.5
      Y_POS = 0.4
      
      N_VEZES = 1000000000


!     //////////////////////////////////////////////////////////////////  
!     ==================================================================  
!                                                   BARICENTRIC TRIANGLE    


      IF (  METODO == 13  ) THEN
      WRITE(*,*) " method    BARICENTRIC coordinates" 
      WRITE(*,*) " poligone  TRIANGLE             "    
      WRITE (50, '("method   BARICENTRIC coordinates ")') 
      WRITE (50, '("poligone TRIANGLE   ")') 
      
      DO I = 1, N_VEZES     
      
       CALL  INSIDE_BARI_KRAMER( X_1,Y_1, X_2,Y_2, X_3,Y_3,            &
                                 X_POS,Y_POS,                          &
                           INSIDE )                          
!        print *, INSIDE  
!                                
                                     
      ENDDO 
         
      print *, INSIDE     
      
      GO TO 500 
      ENDIF  
      
!                
!     ================================================================== 
!                                              BARICENTRIC QUADRILATERAL  
!        

      IF (  METODO == 14  ) THEN
      WRITE(*,*) " method    BARICENTRIC coordinates" 
      WRITE(*,*) " poligone  QUADRILATERAL             "    
      WRITE (50, '("method   BARICENTRIC coordinates ")') 
      WRITE (50, '("poligone QUADRILATERAL  ")') 
      DO I = 1, N_VEZES     
      
       CALL  INSIDE_BARI_KRAMER( X_1,Y_1, X_2,Y_2, X_3,Y_3,            &
                                 X_POS,Y_POS,                          &
                           INSIDE_1 )                          
!        print *, INSIDE  
!                                
       CALL  INSIDE_BARI_KRAMER( X_1,Y_1, X_3,Y_3, X_4,Y_4,            &
                                 X_POS, Y_POS,                         &
                          INSIDE_2 )    
        INSIDE = .FALSE.                   
        IF( INSIDE_1 .OR. INSIDE_2 ) INSIDE = .TRUE.                
                             
      ENDDO 
         
      print *, INSIDE     
      
      GO TO 500 
      ENDIF  

!                
!     ================================================================== 
!                                                  BARICENTRIC PENTAGONE  
!                                                  

      IF (  METODO == 15  ) THEN

      WRITE(*,*) " method    BARICENTRIC coordinates" 
      WRITE(*,*) " poligone  PENTAGONE             "    
      WRITE (50, '("method   BARICENTRIC coordinates ")') 
      WRITE (50, '("poligone PENTAGONE  ")') 
      
      DO I = 1, N_VEZES     
      
       CALL  INSIDE_BARI_KRAMER( X_1,Y_1, X_2,Y_2, X_3,Y_3,            &
                                 X_POS,Y_POS,                          &
                           INSIDE_1 )                          
!        print *, INSIDE  
!                                
       CALL  INSIDE_BARI_KRAMER( X_1,Y_1, X_3,Y_3, X_4,Y_4,            &
                                 X_POS, Y_POS,                         &
                          INSIDE_2 )   
                           
         CALL  INSIDE_BARI_KRAMER( X_1,Y_1, X_4,Y_4, X_5,Y_5,          &
                                 X_POS, Y_POS,                         &
                          INSIDE_3 )                            
        INSIDE = .FALSE.                   
        IF( INSIDE_1 .OR. INSIDE_2 .OR. INSIDE_3 ) INSIDE = .TRUE.                
                             
      ENDDO 
         
      print *, INSIDE     
      
      GO TO 500 
      ENDIF  

      
!                
!     ================================================================== 
!                                              AREAS COMPARISON TRIANGLE  
!    
!     
      IF (  METODO == 23  ) THEN 
 
      WRITE(*,*) " method    AREAS comparison" 
      WRITE(*,*) " poligone  TRIANGLE             "    
      WRITE (50, '("method   AREAS comparison ")') 
      WRITE (50, '("poligone TRIANGLE   ")') 
           

      DO I = 1, N_VEZES    

       CALL  INSIDE_AREA_triangle( X_1 ,Y_1 , X_2,Y_2 , X_3,Y_3 ,      &
                                   X_POS, Y_POS  ,                     &
                                INSIDE )
!        print *, INSIDE  
!
  
      ENDDO   
      
      print *, INSIDE
          
      GO TO 500 
      ENDIF
      
!                
!     ================================================================== 
!                                         AREAS COMPARISON QUADRILATERAL
!    
 
      IF (  METODO == 24  ) THEN
      
      WRITE(*,*) " method    AREAS comparison" 
      WRITE(*,*) " poligone  QUADRILATERAL             "    
      WRITE (50, '("method   AREAS comparison ")') 
      WRITE (50, '("poligone QUADRILATERAL  ")') 
      
      DO I = 1, N_VEZES    

       CALL  INSIDE_AREA_quadrilatere( X_1,Y_1 , X_2,Y_2 , X_3,Y_3 ,   &
                                       X_4,Y_4 ,                       &
                                       X_POS,Y_POS  ,                  &
                                     INSIDE )
!        print *, INSIDE  
!
  
      ENDDO   
      
      print *, INSIDE
          
      GO TO 500 
      ENDIF

                                  
!                
!               
!     ================================================================== 
!                                             AREAS COMPARISON PENTAGONE
!   
     
      IF (  METODO == 25  ) THEN
 
 
       
      WRITE(*,*) " method    AREAS comparison" 
      WRITE(*,*) " poligone  PENTAGONE             "    
      WRITE (50, '("method   AREAS comparison ")') 
      WRITE (50, '("poligone PENTAGONE  ")') 
      
      DO I = 1, N_VEZES    

       CALL  INSIDE_AREA_pentagone( X_1,Y_1 , X_2,Y_2 , X_3,Y_3 ,      &
                                    X_4,Y_4 , X_5,Y_5 ,                &
                                    X_POS,Y_POS  ,                     &
                                  INSIDE )
!        print *, INSIDE  
!
  
      ENDDO   
      
      print *, INSIDE
          
      GO TO 500 
      ENDIF

!               
!     ================================================================== 
!                                                       LOOPING TRIANGLE
!   
      IF (  METODO == 33  ) THEN
       
      WRITE(*,*) " method    LOOPING" 
      WRITE(*,*) " poligone  TRIANGLE             "    
      WRITE (50, '("method   LOOPING ")') 
      WRITE (50, '("poligone TRIANGLE  ")') 
      
      DO I = 1, N_VEZES
      CALL INSIDE_CROSS_triangle ( X_1,Y_1 , X_2,Y_2 , X_3,Y_3 ,       &
                                   X_POS,Y_POS  ,                      &
                                 INSIDE )
      ENDDO 
      
      print *, INSIDE   
      
      GO TO 500      
      
      ENDIF 
      
!     
!     ============================================                                         
!                                    cross quadrilatere
!     

!               
!     ================================================================== 
!                                                  LOOPING QUADRILATERAL
!   

      IF (  METODO == 34  ) THEN
       
       
      WRITE(*,*) " method    LOOPING" 
      WRITE(*,*) " poligone  QUADRILATERAL             "    
      WRITE (50, '("method   LOOPING ")') 
      WRITE (50, '("poligone QUADRILATERAL  ")')      
      
      DO I = 1, N_VEZES
      CALL INSIDE_CROSS_quadrilatere ( X_1,Y_1 , X_2,Y_2 , X_3,Y_3 ,   &
                                       X_4,Y_4 ,                       &     
                                       X_POS,Y_POS  ,                  &
                                     INSIDE )
      ENDDO 
      
      print *, INSIDE   
      
      GO TO 500      
      
      ENDIF 
 
!     
!               
!     ================================================================== 
!                                                      LOOPING PENTAGONE
!   

      IF (  METODO == 35  ) THEN
       
      WRITE(*,*) " metodo  INSIDE_CROSS_pentagone " 
         
      WRITE(*,*) " method    LOOPING" 
      WRITE(*,*) " poligone  PENTAGONE             "    
      WRITE (50, '("method   LOOPING ")') 
      WRITE (50, '("poligone PENTAGONE   ")')                
      
      DO I = 1, N_VEZES
      CALL INSIDE_CROSS_pentagone ( X_1,Y_1 , X_2,Y_2 , X_3,Y_3 ,   &
                                    X_4,Y_4 , X_5,Y_5 ,                &     
                                    X_POS,Y_POS  ,                     &
                                  INSIDE )
      ENDDO 
      
      print *, INSIDE   
      
      GO TO 500      
      
      ENDIF 
!     
!     =========================


  500  CONTINUE 
  
      WRITE(*,*) " verify file log_time "
      call cpu_time(finish)
      print '("Time = ",f6.3," seconds.")',finish - start 
      WRITE (50, '("Time = ",f6.3," seconds.")'),finish - start    
      
       
   END PROGRAM WORK_WITH_INSIDE
!/ ------------------------------------------------------------------- /  
