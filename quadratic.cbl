      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUADRATIC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(3) VALUE 0.
       01 B PIC 9(3) VALUE 0.
       01 C PIC 9(3) VALUE 0.
       01 D PIC 9(3) VALUE 0.
       01 E PIC 9(3) VALUE 0.
       01 F PIC 9(3) VALUE 0.
       01 G PIC 9(3) VALUE 0.
       01 H PIC 9(3) VALUE 0.
       01 X1 PIC 9(3) VALUE 0.
       01 X PIC Z(3).Z(2).
       01 Y PIC Z(3).Z(2).
       01 X2 PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *     Display "Quadratic equation solver for three values a, b & c"
           DISPLAY "Enter a number: " ACCEPT A.
           DISPLAY "Enter the second number: " ACCEPT B.
           DISPLAY "Enter the last number: " ACCEPT C.

           COMPUTE D = (B * B)
           COMPUTE E = 4 * A * C
           COMPUTE F = 2 * A
           COMPUTE G = D - E
           COMPUTE H = FUNCTION SQRT(G).
           COMPUTE X1 = (-B) + H
           COMPUTE X = X1 / F

           DISPLAY "x1 = " X

           COMPUTE X2 = (-B) - H
           COMPUTE Y = X2 / F
           DISPLAY "x2 = " Y
      *    Display "Send the accurate program".
            STOP RUN.
       END PROGRAM QUADRATIC.
