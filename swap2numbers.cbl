      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SWAP-2-NUMBERS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 NUM-1 PIC 9(2).
       01 NUM-2 PIC 9(2).
       01 TEMPORARY PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "ENTER NUM 1: " ACCEPT NUM-1.
           DISPLAY "ENTER NUM 2: " ACCEPT NUM-2.

           DISPLAY "NUM 1: " NUM-1.
           DISPLAY "NUM 2: " NUM-2.

           MOVE NUM-1 TO TEMPORARY.
           MOVE NUM-2 TO NUM-1.
           MOVE TEMPORARY TO NUM-2.

           DISPLAY "NUM 1 AFTER THE SWAP: " NUM-1.
           DISPLAY "NUM 2 AFTER THE SWAP: " NUM-2.

            STOP RUN.
       END PROGRAM SWAP-2-NUMBERS.
