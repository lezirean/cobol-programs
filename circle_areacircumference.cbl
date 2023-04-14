      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIRCLE-AREA-CIRCUMFERENCE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 RADIUS PIC 99V99.
           01 CIRCUMFERENCE PIC 99V99.
           01 PI-VALUE PIC 9V99999 VALUE 3.14159.
           01 AREA-CIRCLE PIC 99V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Enter the radius of the circle: ".
            ACCEPT RADIUS.
      *     DISPLAY "Enter the of the circle: ".

            COMPUTE AREA-CIRCLE = PI-VALUE * (RADIUS * RADIUS).
            COMPUTE CIRCUMFERENCE = 2 * PI-VALUE * RADIUS.

            DISPLAY "The area of the circle is: " AREA-CIRCLE.
            DISPLAY "The circumference of the circle is: " CIRCUMFERENCE.
            STOP RUN.
       END PROGRAM CIRCLE-AREA-CIRCUMFERENCE.
