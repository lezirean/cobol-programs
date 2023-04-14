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
       01 NUM1 PIC 9(3).
       01 NUM2 PIC 9(3).
       01 NUM3 PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Enter the first number: " ACCEPT NUM1.
            DISPLAY "Enter the second number: " ACCEPT NUM2.
            DISPLAY "Enter the third number: " ACCEPT NUM3.

            IF NUM1 >= NUM2 AND NUM1 >= NUM3 THEN
                DISPLAY NUM1 " is the largest number.".
            IF NUM2 >= NUM1 AND NUM2 >= NUM3 THEN
                DISPLAY NUM2 " is the largest number.".
            IF NUM3 >= NUM1 AND NUM3 >= NUM2 THEN
                DISPLAY NUM3 " is the largest number.".

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
