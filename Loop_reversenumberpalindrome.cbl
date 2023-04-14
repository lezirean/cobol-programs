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
       77 INPUTNUM PIC 9(7).
       77 HOLDNUM PIC 9(7).
       77 TEMP PIC 9(7).
       77 REMAINDERNUM PIC 9.
       77 REVERSENUM PIC 9(7) Value Zeros.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER NUMBER (7 DIGITS MAX) : ".
            ACCEPT INPUTNUM.
            MOVE INPUTNUM TO HOLDNUM.
            MOVE INPUTNUM TO TEMP.
            PERFORM UNTIL INPUTNUM = 0
            DIVIDE INPUTNUM BY 10 GIVING INPUTNUM REMAINDER REMAINDERNUM
            COMPUTE REVERSENUM = REVERSENUM * 10 + REMAINDERNUM
            END-PERFORM.

            DISPLAY "REVERSE OF " TEMP " IS " REVERSENUM.

            IF REVERSENUM IS EQUAL TO HOLDNUM
                DISPLAY "PALINDROME NUMBER"
            ELSE
                DISPLAY "NOT A PALINDROME"
            END-IF.
            STOP RUN.

       PARA-1.
           DIVIDE INPUTNUM BY 10 GIVING INPUTNUM REMAINDER REMAINDERNUM
           COMPUTE REVERSENUM = REVERSENUM * 10 + REMAINDERNUM
           EXIT.

       END PROGRAM YOUR-PROGRAM-NAME.
