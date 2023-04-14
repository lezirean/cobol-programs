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
       01 ROWS PIC 9(1) VALUE 5.
       01 LINENUM PIC 9(1) VALUE 5.
       01 CUR-NUM PIC 9(1) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM VARYING LINENUM FROM 5 BY -1 UNTIL
             LINENUM = 0
               PERFORM UNTIL CUR-NUM > LINENUM
                   DISPLAY LINENUM WITH NO ADVANCING
                   ADD 1 TO CUR-NUM
               END-PERFORM
                   DISPLAY " "
                   SET CUR-NUM TO 1
             END-PERFORM.

            STOP RUN.
       DISPLAY-NUM.
            PERFORM UNTIL CUR-NUM > LINENUM
             DISPLAY LINENUM WITH NO ADVANCING
             ADD 1 TO CUR-NUM
            END-PERFORM
             DISPLAY " "
             SET CUR-NUM TO 1.

      *      STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
