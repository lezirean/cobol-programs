      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         SELECT PRINT-ONETON
         ASSIGN TO "C:\cobol_project\FHLoop_Print1ToN.dat"
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.
      **************************************
       DATA DIVISION.
       FILE SECTION.
       FD PRINT-ONETON.
       01 FILE-PRINTONETON.
      *   05 LAST1 PIC 9(3).
         05 CTR PIC 9(3).
       WORKING-STORAGE SECTION.
       77 WS-CTR PIC 9(3) VALUE 1.
       77 LAST1 PIC 9(3).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "PRINTING VALUES FROM 1 TO N. ENTER N: ".
            ACCEPT LAST1.

           OPEN OUTPUT PRINT-ONETON
                PERFORM VARYING WS-CTR FROM 1 BY 1 UNTIL WS-CTR > LAST1
                     MOVE WS-CTR TO CTR
                     WRITE FILE-PRINTONETON
                END-PERFORM.
           CLOSE PRINT-ONETON.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
