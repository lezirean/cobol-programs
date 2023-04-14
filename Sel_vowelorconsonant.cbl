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
       01 CHAR PIC A.
       88 VOWEL VALUE 'A', 'E', 'I', 'O', 'U'.
       88 LOWER-VOWEL VALUE 'a', 'e', 'i', 'o', 'u'.
       88 CONSO VALUE 'B' THRU 'D', 'F' THRU 'H', 'J' THRU 'N',
       'P' THRU 'T', 'V' THRU 'Z'.
       88 LOWER-CONSO VALUE 'b' THRU 'd', 'f' THRU 'h', 'j' THRU 'n',
       'p' THRU 't', 'v' THRU 'z'.
       88 VALID-CHAR VALUE 'A' THRU 'Z', 'a' THRU 'z'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER A LETTER: " ACCEPT CHAR.

            EVALUATE TRUE
             WHEN VOWEL DISPLAY "VOWEL"
             WHEN LOWER-VOWEL DISPLAY "VOWEL"
             WHEN CONSO DISPLAY "CONSONANT"
             WHEN LOWER-CONSO DISPLAY "CONSONANT"
             WHEN OTHER DISPLAY "NOT A LETTER"
            END-EVALUATE.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
