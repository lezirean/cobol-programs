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
         SELECT FACTORIAL
         ASSIGN TO "C:\cobol_project\FHLoop_Factorial.dat"
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.
      **************************************
       DATA DIVISION.
       FILE SECTION.
       FD FACTORIAL.
       01 FILE-FACTORIAL.
          05 N PIC 9(4).
          05 FACTPRODUCT PIC 9(4) VALUE 1.
       WORKING-STORAGE SECTION.
       01 WS-FACTORIAL.
          05 WS-N PIC 9(4).
          05 CTR PIC 9(4) VALUE 0.
          05 WS-FACTPRODUCT PIC 9(4) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER A NUMBER TO COMPUTE FOR ITS FACTORIAL: ".
            ACCEPT WS-N.

            PERFORM UNTIL CTR = WS-N
              ADD 1 TO CTR
              COMPUTE WS-FACTPRODUCT = WS-FACTPRODUCT * CTR
            END-PERFORM.

            OPEN EXTEND FACTORIAL
               MOVE WS-N TO N
               MOVE WS-FACTPRODUCT TO FACTPRODUCT
            WRITE FILE-FACTORIAL
            CLOSE FACTORIAL.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
