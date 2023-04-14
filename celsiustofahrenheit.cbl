      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CELSIUS-TO-FAHRENHEIT.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 CELSIUS PIC 99V999.
       01 FAHRENHEIT PIC 99V999.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "ENTER THE TEMPERATURE IN CELSIUS: " ACCEPT CELSIUS.

           COMPUTE FAHRENHEIT = (CELSIUS * 01.80 ) + 32.00.

           DISPLAY CELSIUS "C IN FAHRENHEIT IS: " FAHRENHEIT "F".

            STOP RUN.
       END PROGRAM CELSIUS-TO-FAHRENHEIT.
