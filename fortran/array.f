c     Read in 10 numbers and print them in reverse order
      DIMENSION ILIST(10)

      DO 10 I=1,10

      WRITE(*,20) I
20    FORMAT('Number ', I4, ': ')
      READ(*,30) ILIST(I)
30    FORMAT(I4)
10    CONTINUE

      DO 200 I=10,1,-1
      WRITE(*,30) ILIST(I)
200   CONTINUE
      END
