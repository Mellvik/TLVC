C     FORTRAN 77 TEST PROGRAM
      PROGRAM TEST77
      INTEGER A, B, SUM, ARR(30)
      
      A = 10
      B = 20
      SUM = A + B
      ARR(12) = SUM
      
      PRINT *, 'FORTRAN 77 COMPILER TEST: SUCCESS'
      PRINT *, 'THE SUM OF 10 AND 20 IS:', SUM
      CALL SUB1
      
      STOP
      END

      SUBROUTINE SUB1
      PRINT *, 'This is the subroutine SUB1'
      RETURN
      END
