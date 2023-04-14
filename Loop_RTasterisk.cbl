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
       01 LINESTAR PIC 9(1) VALUE 1.
       01 CUR-STAR PIC 9(1) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM VARYING LINESTAR FROM 1 BY 1 UNTIL
                LINESTAR > ROWS
             PERFORM UNTIL CUR-STAR > LINESTAR
             DISPLAY "*" WITH NO ADVANCING
             ADD 1 TO CUR-STAR
              END-PERFORM
             DISPLAY " "
             SET CUR-STAR TO 1
            END-PERFORM.


            STOP RUN.
       DISPLAY-STAR.
            PERFORM UNTIL CUR-STAR > LINESTAR
             DISPLAY "*" WITH NO ADVANCING
             ADD 1 TO CUR-STAR
            END-PERFORM.
             DISPLAY " ".
             SET CUR-STAR TO 1.

       END PROGRAM YOUR-PROGRAM-NAME.
