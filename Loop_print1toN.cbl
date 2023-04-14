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
       01 LAST1 PIC 9(3).
       01 CTR PIC 9(3) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "PRINTING VALUES FROM 1 TO N. ENTER N: ".
            ACCEPT LAST1.
           PERFORM VARYING CTR FROM 1 BY 1 UNTIL CTR >LAST1
             DISPLAY CTR
           END-PERFORM.

            STOP RUN.

       DISPLAY-PARA.
            DISPLAY CTR " ".
            EXIT.
       END PROGRAM YOUR-PROGRAM-NAME.
