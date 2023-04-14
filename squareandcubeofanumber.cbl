      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQUARE-AND-CUBE-OF-NUMBER.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 NUMBER1 PIC 9(2).
       01 NUM-SQUARE PIC 9(2).
       01 NUM-CUBE PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "ENTER A NUMBER: " ACCEPT NUMBER1.

           COMPUTE NUM-SQUARE = NUMBER1 * NUMBER1.
           COMPUTE NUM-CUBE = NUM-SQUARE * NUMBER1.

           DISPLAY "THE SQUARE OF THE NUMBER IS: " NUM-SQUARE.
           DISPLAY "THE CUBE OF THE NUMBER IS: " NUM-CUBE.

            STOP RUN.
       END PROGRAM SQUARE-AND-CUBE-OF-NUMBER.
