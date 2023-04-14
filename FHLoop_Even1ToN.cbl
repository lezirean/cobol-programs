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
         SELECT EVEN-ONETON
         ASSIGN TO "C:\cobol_project\FHLoop_Even1ToN.dat"
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.
      **************************************
       DATA DIVISION.
       FILE SECTION.
       FD EVEN-ONETON.
       01 FILE-EVENONETON.
          05 CTR PIC 9(3).
       WORKING-STORAGE SECTION.
       01 WS-EVEN1TON.
          05 WS-LASNUM PIC 9(3).
          05 WS-CTR PIC 9(3) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "PRINTING EVEN NUMBERS FROM 1 TO N. ENTER N: ".
            ACCEPT WS-LASNUM.

           OPEN OUTPUT EVEN-ONETON
             PERFORM VARYING WS-CTR FROM 1 BY 1 UNTIL WS-CTR > WS-LASNUM
               IF FUNCTION MOD(WS-CTR, 2) = ZERO
                  MOVE WS-CTR TO CTR
                  WRITE FILE-EVENONETON
               END-IF
             END-PERFORM.
           CLOSE EVEN-ONETON.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
