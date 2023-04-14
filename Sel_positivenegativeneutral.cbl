      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POSITIVE-NEGATIVE-NEUTRAL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 NUM PIC S9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "ENTER A NUMBER: " ACCEPT NUM.

           IF NUM IS EQUAL TO ZERO
               DISPLAY "THE NUMBER IS NEUTRAL"
           ELSE IF NUM IS GREATER THAN ZERO
               DISPLAY "THE NUMBER IS POSITIVE"
           ELSE
               DISPLAY "THE NUMBER IS NEGATIVE"
           END-IF.

            STOP RUN.
       END PROGRAM POSITIVE-NEGATIVE-NEUTRAL.
