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
       01 N PIC 9(4).
       01 CTR PIC 9(4) VALUE 0.
       01 FACTORIAL PIC 9(4) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER A NUMBER TO COMPUTE FOR ITS FACTORIAL: ".
            ACCEPT N.

            PERFORM UNTIL CTR = N
              ADD 1 TO CTR
              COMPUTE FACTORIAL = FACTORIAL * CTR
            END-PERFORM.
            DISPLAY "THE FACTORIAL OF " N " IS " FACTORIAL.
            STOP RUN.

       FACTO.
            ADD 1 TO CTR.
            COMPUTE FACTORIAL = FACTORIAL * CTR.
            EXIT.
       END PROGRAM YOUR-PROGRAM-NAME.
