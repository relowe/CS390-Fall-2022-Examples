c     use my function
      PROGRAM FUN
      INTEGER X
      INTEGER user input
      X = user input()
      WRITE(*, 20)X
20    FORMAT(I5)
      END

c     A demonstration of functions and subroutines
      INTEGER FUNCTION user input()
      INTEGER i

      WRITE(*, 10)
10    FORMAT('Enter an integer, maximum 5 digits.')

      READ(*, 20) i
20    FORMAT(I5)

c     return the user input
      user input = i
      RETURN
      END

