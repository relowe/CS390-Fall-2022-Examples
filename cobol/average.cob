      * Compute the average of a list of numbers.
       IDENTIFICATION DIVISION.
           PROGRAM-ID. AVERAGE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
          01 WS-N    PIC 9(2).
          01 WS-SUM  PIC 9(6).
          01 WS-NUM  PIC 9(3).
          01 WS-I    PIC 9(2).
          01 WS-AVG  PIC 9(3)V9(2).

       PROCEDURE DIVISION.
       Average.
           PERFORM READ-N.
           PERFORM ACCUMULATE-SUM.
           PERFORM COMPUTE-AVERAGE.
           STOP RUN.

       READ-N.
           DISPLAY "How many numbers to average? " WITH NO ADVANCING.
           ACCEPT WS-N.

       ACCUMULATE-SUM.
           SET WS-SUM TO 0.
           PERFORM GNUM VARYING WS-I FROM 1 BY 1 UNTIL WS-I GREATER WS-N
      - .

       GNUM.
           ACCEPT WS-NUM.
           ADD WS-NUM TO WS-SUM.


       COMPUTE-AVERAGE.
           DIVIDE WS-N INTO WS-SUM GIVING WS-AVG.
           DISPLAY "Average: " WS-AVG.

