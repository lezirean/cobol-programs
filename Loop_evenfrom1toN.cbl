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
       01 LASNUM PIC 9(3).
       01 CTR PIC 9(3) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "PRINTING EVEN NUMBERS FROM 1 TO N. ENTER N: ".
            ACCEPT LASNUM.

           PERFORM VARYING CTR FROM 1 BY 1 UNTIL CTR > LASNUM
             IF FUNCTION MOD(CTR, 2) = ZERO
                DISPLAY CTR
             END-IF
           END-PERFORM.
           STOP RUN.

       DISPLAY-PARA.
            IF FUNCTION MOD(CTR, 2) = ZERO
                DISPLAY CTR
            END-IF.

      *      STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
