c     Demonstrate 2d arrays
      DIMENSION IAR(3,2)

      DO 200 I=1,3
      DO 100 J=1,2
      READ(*, 10) IAR(I,J)
10    FORMAT(I4)
100   CONTINUE
200   CONTINUE

      DO 300 I=1,3
      WRITE(*,20) IAR(I,1), IAR(I,2)
20    FORMAT(I4, I4)
300   CONTINUE
      END
