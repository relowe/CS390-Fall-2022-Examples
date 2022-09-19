      * This program counts from 1 to 10
       IDENTIFICATION DIVISION.
          PROGRAM-ID. Count10.
       
       DATA DIVISION.
      * Custom types go here
       WORKING-STORAGE SECTION.
      * Variables go here
      * LVL NAME         PICTURE Width (9 - digit, A - Alpha, X - Alphanum)
        01  WS-NUMBER    PICTURE 99.
        01  WS-NUM       PIC     9(2).

       PROCEDURE DIVISION.
       Count10.
          SET WS-NUMBER TO 1.
          PERFORM COUNT-NUM 10 TIMES.
          STOP RUN.

       COUNT-NUM.
          DISPLAY WS-NUMBER.
          ADD 1 TO WS-NUMBER.
