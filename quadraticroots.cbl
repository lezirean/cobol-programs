      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUADRATIC-ROOTS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 A PIC S9(2).
       01 B PIC S9(2).
       01 C PIC S9(2).
       01 DISCRIMINANT PIC S9(3)V9(2).
       01 X1 PIC S9(3)V9(2).
       01 X2 PIC S9(3)V9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Enter the first constant: " ACCEPT A.
            DISPLAY "Enter the second constant: " ACCEPT B.
            DISPLAY "Enter the third constant: " ACCEPT C.

            DISPLAY "Quadratic equation: " A "x^2" B "x" C.

            COMPUTE DISCRIMINANT = (B*B) - (4*A*C).
            COMPUTE X1 = (-B + FUNCTION SQRT(DISCRIMINANT)) / (2*A).
            COMPUTE X2 = (-B - FUNCTION SQRT(DISCRIMINANT)) / (2*A).

            DISPLAY "x1: " X1 ", x2: " X2.


            STOP RUN.
       END PROGRAM QUADRATIC-ROOTS.
