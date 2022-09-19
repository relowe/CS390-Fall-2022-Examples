      * This program counts according to user desires
       IDENTIFICATION DIVISION.
          PROGRAM-ID. Count-User.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01  WS-NUMBER    PIC 9(5).
        01  WS-START     PIC 9(5).
        01  WS-END       PIC 9(5).
        01  WS-INCREMENT PIC 9(5).

       PROCEDURE DIVISION.
       Count-User.
           DISPLAY "Starting Point? " WITH NO ADVANCING.
           ACCEPT WS-START.

           DISPLAY "Ending Point? " WITH NO ADVANCING.
           ACCEPT WS-END.

           DISPLAY "Increment? " WITH NO ADVANCING.
           ACCEPT WS-INCREMENT.

           MOVE WS-START TO WS-NUMBER.
           PERFORM Count-Up UNTIL WS-NUMBER GREATER THAN WS-END.
           STOP RUN.

       Count-UP.
           DISPLAY WS-NUMBER.
           ADD WS-INCREMENT TO WS-NUMBER.

