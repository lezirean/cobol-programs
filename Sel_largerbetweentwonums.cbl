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
       01 NUM-A PIC 9(2).
       01 NUM-B PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "ENTER THE FIRST NUMBER: " ACCEPT NUM-A.
           DISPLAY "ENTER THE SECOND NUMBER: " ACCEPT NUM-B.

           IF NUM-A >= NUM-B
               DISPLAY NUM-A " IS THE LARGER NUMBER"
           ELSE
               DISPLAY NUM-B " IS THE LARGER NUMBER"
           END-IF.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
