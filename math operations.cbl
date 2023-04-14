      ******************************************************************
      * Author: Leila Borromeo
      * Date: Oct. 10, 2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. math-operations.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(2).
       01 NUM2 PIC 9(2).
       01 TOTAL PIC 9(2).
       01 DIFFERENCE PIC 9(2).
       01 QUOTIENT PIC 9(2).
       01 PRODUCT PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter two numbers: "
           ACCEPT NUM1.
           ACCEPT NUM2.

           ADD NUM1 NUM2 GIVING TOTAL.
           MULTIPLY NUM1 BY NUM2 GIVING PRODUCT.
           SUBTRACT NUM1 FROM NUM2 GIVING DIFFERENCE.
           DIVIDE NUM1 BY NUM2 GIVING QUOTIENT.
      *     COMPUTE TOTAL = NUM1 + NUM2.
      *     COMPUTE DIFFERENCE = NUM1 - NUM2.
      *     COMPUTE QUOTIENT = NUM1 / NUM2.
      *     COMPUTE PRODUCT = NUM1 * NUM2.
           DISPLAY "Total: " TOTAL
           DISPLAY "Difference: " DIFFERENCE
           DISPLAY "Quotient: " QUOTIENT
           DISPLAY "Product: " PRODUCT
            STOP RUN.
       END PROGRAM math-operations.
