C     This Program Calculates Prime Numbers from 11 to 50
      DO 10 I=11, 50, 2
      J=1
4     J=J+2
      A=J
      A=I/A
      L=I/J
      B=A-L
      IF (B) 5, 10, 5
5     IF (J.LT.SQRT(FLOAT(I))) GO TO 4
      WRITE (*,105) I
10    CONTINUE
105   FORMAT (I4, ' IS PRIME.')
      END
