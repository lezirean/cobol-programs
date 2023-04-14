      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 INPUT-NUM PIC S9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "ENTER A NUMBER: " ACCEPT INPUT-NUM.

           IF FUNCTION MOD(INPUT-NUM, 2) = 0
               DISPLAY "THE NUMBER IS EVEN"
           ELSE
               DISPLAY "THE NUMBER IS ODD"
           END-IF.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
