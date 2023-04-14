      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FHSEQ_CELTOFAH.
       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         SELECT TEMPERATURE
         ASSIGN TO "C:\cobol_project\FHSeqCelToFah.dat"
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.
      ****************************************
       DATA DIVISION.
        FILE SECTION.
        FD TEMPERATURE.
        01 FILE-TEMPERATURE.
         05 CEL PIC 9(3)V9(3).
         05 FAH PIC 9(3).9(3).
      ***************************************
       WORKING-STORAGE SECTION.
        01 WS-TEMPERATURE.
         05 WS-CELINPUT PIC X(6).
         05 WS-CEL PIC 9(3)V9(3).
         05 WS-FAH PIC 9(3)V9(3).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER THE TEMPERATURE IN CELSIUS: "
            ACCEPT WS-CELINPUT
            MOVE WS-CELINPUT TO WS-CEL

            COMPUTE WS-FAH = (WS-CEL * 01.80 ) + 32.00

            OPEN EXTEND TEMPERATURE
                 MOVE WS-CEL TO CEL.
                 MOVE WS-FAH TO FAH.
            WRITE FILE-TEMPERATURE
            CLOSE TEMPERATURE.

            STOP RUN.
       END PROGRAM FHSEQ_CELTOFAH.
