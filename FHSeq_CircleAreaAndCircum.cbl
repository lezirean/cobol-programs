      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FHSEQ-CIRCLEAREAANDCIRCUM.
       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         SELECT CIRCLE
         ASSIGN TO "C:\cobol_project\FHSeq_CircleAreaAndCircum.dat"
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.
      **************************************
       DATA DIVISION.
        FILE SECTION.
        FD CIRCLE.
        01 FILE-CIRCLE.
          05 RADIUS PIC 99.99.
          05 CIRCUMFERENCE PIC 999.99.
          05 AREA-CIRCLE PIC 999.99.

       WORKING-STORAGE SECTION.
       01 WS-CIRCLE.
         05 WS-PI-VALUE PIC 9V99999 VALUE 3.14159.
         05 WS-RADIUSINPUT PIC X(5).
         05 WS-RADIUS PIC 99V99.
         05 WS-CIRCUMFERENCE PIC 999V99.
         05 WS-AREA-CIRCLE PIC 999V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER THE RADIUS OF THE CIRCLE: "
            ACCEPT WS-RADIUSINPUT
            MOVE WS-RADIUSINPUT TO WS-RADIUS

            COMPUTE WS-AREA-CIRCLE = WS-PI-VALUE *
                                               (WS-RADIUS * WS-RADIUS).
            COMPUTE WS-CIRCUMFERENCE = 2 * WS-PI-VALUE * WS-RADIUS.

            OPEN EXTEND CIRCLE
               MOVE WS-RADIUS TO RADIUS
               MOVE WS-CIRCUMFERENCE TO CIRCUMFERENCE
               MOVE WS-AREA-CIRCLE TO AREA-CIRCLE
               WRITE FILE-CIRCLE.
            CLOSE CIRCLE.

      *      DISPLAY "The area of the circle is: " AREA-CIRCLE.
      *      DISPLAY "The circumference of the circle is: " CIRCUMFERENCE.
            STOP RUN.
       END PROGRAM FHSEQ-CIRCLEAREAANDCIRCUM.
