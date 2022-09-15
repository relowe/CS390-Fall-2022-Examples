C     Force the user to enter 5
1     WRITE(*,100)
2     READ(*,200) I
3     IF (I.EQ.5) GO TO 300
      GO TO 1
100   FORMAT('ENTER 5')
200   FORMAT(I10)
400   FORMAT('THANK YOU')
300   WRITE(*,400)
      END
