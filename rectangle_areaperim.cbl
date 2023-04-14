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
           01 RECT-LENGTH PIC 9(3).
           01 RECT-WIDTH PIC 9(3).
           01 RECT-AREA PIC 9(3).
           01 RECT-PERIMETER PIC 9(3).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "Enter the length of the rectangle: ".
           ACCEPT RECT-LENGTH.
           DISPLAY "Enter the width of the rectangle:  ".
           ACCEPT RECT-WIDTH.

           COMPUTE RECT-PERIMETER = (2 * RECT-LENGTH) + (2 * RECT-WIDTH).
           COMPUTE RECT-AREA = RECT-LENGTH * RECT-WIDTH.

           DISPLAY "The perimeter of the rectangle is: " RECT-PERIMETER.
           DISPLAY "The area of the rectangle is: " RECT-AREA.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
